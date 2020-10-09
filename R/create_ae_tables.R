#' Create AE tables
#' 
#' DESCRIPTION 
#' 
#' @param ae_data A string containing the path to AE data in csv format, or the
#' AE data as a data frame.
#' 
#' @return A list containing the four AE reporting tables.
#' @export 

# for dev 
#a e_data <- readr::read_csv("~/job/dantrials/ae_analyse/ae_data.csv")

# TODO: Uniform length of entries (extra spaces)

create_ae_tables <- function(ae_data) {

	# initialise list that collects all the finished tables
	tables_ls <- list()

	# check if input is file path or data frame
	if (is.character(ae_data)) {
		ae_raw <- readr::read_csv(ae_data)
	} else if (is.data.frame(ae_data)) {
		ae_raw <- ae_data
	} else {
		stop("`ae_data` is not a string (path to data), or a data frame. Stopping.")
	}

	# Clean up any bad names
	ae_tbl <- ae_raw %>%
		janitor::clean_names(case = "snake")

	# For each subject that have received placebo, change group
	ae_tbl <- 
		ae_tbl %>%
		dplyr::mutate(
			      group_number = ifelse(treatment_assignment == "placebo", "0", group_number),
			      group_label = ifelse(treatment_assignment == "placebo", "Placebo", group_label)
			      )

	# TODO: Create small function that changes the cell values, so that 

	# Prepare table 1 ----------------------------------------------------
	# Overview of subjects reporting at least one AE

	# Find out which subjects has at least one of:
	any_tbl <-
		ae_tbl %>%
		dplyr::group_by(subject_id, group_label) %>%
		dplyr::summarise(
			  `Any AE` = !anyNA(pt),
			  `Any AR` = any(causality_coded == "Related", na.rm = TRUE),
			  `Any AESI` = any(aesi == "Yes", na.rm = TRUE),
			  `Any SAE` = any(sae == "Yes", na.rm = TRUE),
			  `Any AE leading to death` = any(outcome == "Fatal", na.rm = TRUE),
			  `Any AE leading to discontinuation` = any(discontinuation == "yes", na.rm = TRUE),
			  `Number of subjects` = TRUE
			  ) %>%
		dplyr::ungroup()

	# Sum by number of subjects that qualify by cohort
	subjects_w_at_least_one_ae_tbl <-
		any_tbl %>%
		dplyr::group_by(group_label) %>%
		dplyr::summarise_if(is.logical, sum) %>%
		tidyr::gather(-group_label, key = item, val = n) %>%
		tidyr::spread(key = group_label, val = n) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across(-item))) 

	# After adding parenthesis, all vars change to strings.
	# This means that arranging the rows is tricky (9 is then higher than 44)
	# I create an "arrange column" to arrange by before saving the table.
	arrange_col <-
		subjects_w_at_least_one_ae_tbl %>%
		dplyr::arrange(desc(All)) %>%
		dplyr::mutate(arr_col = All) %>%
		dplyr::select(item, arr_col)


	# Add totals in parenthesis
	subjects_w_at_least_one_ae_tbl <- 
		subjects_w_at_least_one_ae_tbl %>%
		tidyr::gather(-item, key = col, val = val) %>%
		tidyr::spread(key = item, val = val) %>%
		dplyr::mutate_if(
			  is.numeric, ~ stringr::str_glue("{.x} ({round(.x/`Number of subjects` * 100, 1)}%)")
			  ) %>%
		tidyr::gather(-col, key = item, val = val) %>%
		tidyr::spread(key = col, val = val) %>%
		# Add arrange column, arrange by it, and remove it again
		dplyr::left_join(arrange_col) %>%
		dplyr::arrange(desc(arr_col)) %>%
		dplyr::select(-arr_col) %>%
		tibble::column_to_rownames("item")

	tables_ls$table_1 <- subjects_w_at_least_one_ae_tbl

	# Prepare table 2 ---------------------------------
	# Subjects reporting AEs that have also been reported by at least one
	# other subject

	# Grab number of subjects by cohort (a bit hacky)
	persons_per_cohort_tbl <-
		ae_tbl %>%
		dplyr::group_by(group_label) %>%
		dplyr::summarise(
			  n = dplyr::n_distinct(subject_id)
			  ) %>%
		tidyr::spread(key = group_label, value = n) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across())) %>%
		tidyr::gather(key = key, val = `Number of subjects`) 

	# Prepare table
	subjects_w_common_ae_tbl <-
		ae_tbl %>%
		# Remove all AE-types that are only observed once
		dplyr::group_by(pt) %>%
		tidyr::drop_na(pt) %>%
		dplyr::mutate(n_subjects = dplyr::n_distinct(subject_id)) %>%
		dplyr::filter(n_subjects >= 2) %>%
		# get number of subjects per cohort with qualifying AE
		dplyr::group_by(pt, group_label) %>%
		dplyr::summarise(
			  n = dplyr::n_distinct(subject_id)
			  ) %>%
		tidyr::spread(key = group_label, val = n, fill = 0) %>%
		dplyr::ungroup() %>%
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across(-c(pt)))) %>%
		# Reshape so persons_per_cohort_tbl fits
		tidyr::gather(-pt, key = key, val = val) %>%
		tidyr::spread(key = pt, val = val) %>%
		dplyr::left_join(persons_per_cohort_tbl) 

	# Create arrange column
	arrange_col <- 
		subjects_w_common_ae_tbl %>%
		tidyr::gather(-key, key = pt, val = val) %>%
		tidyr::spread(key = key, val = val) %>%
		dplyr::arrange(desc(All)) %>%
		dplyr::mutate(arr_col = All) %>%
		dplyr::select(pt, arr_col)

	# Add parenthesis with percentage share of cohort total
	subjects_w_common_ae_tbl <- 
		subjects_w_common_ae_tbl %>%
		# Add parenthesis
		dplyr::mutate_if(is.numeric, ~ stringr::str_glue("{.x} ({round(.x/`Number of subjects` * 100, 1)}%)")) %>%
		# reshape back to output format
		tidyr::gather(-key, key = pt, val = val) %>%
		tidyr::spread(key = key, val = val) %>% 
		# Arrange
		dplyr::left_join(arrange_col) %>%
		dplyr::arrange(desc(arr_col)) %>%
		dplyr::select(-arr_col) %>%
		tibble::column_to_rownames("pt")

	tables_ls$table_2 <- subjects_w_common_ae_tbl

	# Prepare table 3 --------------------------------
	# Number of adverse events which were reported by more than one subject

	# Get table of AEs by cohort
	ae_per_cohort_tbl <- 
		ae_tbl %>%
		dplyr::group_by(group_label) %>%
		tidyr::drop_na(pt) %>%
		dplyr::summarise(
			  n = dplyr::n()
			  ) %>%
		tidyr::spread(key = group_label, value = n) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(
		       All = sum(dplyr::c_across())
		       ) %>%
		tidyr::gather(key = key, val = `Number of adverse events`)

	# Prepare table
	number_of_common_ae_tbl <-
		ae_tbl %>%
		# Remove all AE-types that are only observed by one subject
		dplyr::group_by(pt) %>%
		tidyr::drop_na(pt) %>%
		dplyr::filter(dplyr::n_distinct(subject_id) >= 2) %>%
		# Count the number of AEs reported
		dplyr::group_by(group_label, pt) %>%
		dplyr::summarise(n = dplyr::n()) %>%
		# Format so cohorts are columns
		tidyr::spread(key = group_label, val = n, fill = 0) %>%
		# Create "All" col
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across(-pt))) %>%
		# reshape so ae_per_cohort_tbl fits
		tidyr::gather(-pt, key = key, val = val) %>%
		tidyr::spread(key = pt, val = val) %>%
		dplyr::left_join(ae_per_cohort_tbl)

	# Create arrange column
	arrange_col <- 
		number_of_common_ae_tbl %>%
		tidyr::gather(-key, key = pt, val = val) %>%
		tidyr::spread(key = key, val = val) %>%
		dplyr::arrange(desc(All)) %>%
		dplyr::mutate(arr_col = All) %>%
		dplyr::select(pt, arr_col)

	# Add parenthesis with percentage share of cohort total
	number_of_common_ae_tbl <-
		number_of_common_ae_tbl %>%
		# Add parenthesis
		dplyr::mutate_if(is.numeric, ~ stringr::str_glue("{.x} ({round(.x/`Number of adverse events` * 100, 1)}%)")) %>%
		# reshape back to output format
		tidyr::gather(-key, key = pt, val = val) %>%
		tidyr::spread(key = key, val = val) %>% 
		# Arrange
		dplyr::left_join(arrange_col) %>%
		dplyr::arrange(desc(arr_col)) %>%
		dplyr::select(-arr_col) %>%
		tibble::column_to_rownames("pt")

	tables_ls$table_3 <- number_of_common_ae_tbl

	# Prepare table 4 ---------------------------------
	# List of adverse events by subject

	all_ae_tbl <-
		ae_tbl %>%
		tidyr::drop_na(pt) %>%
		dplyr::arrange(group_label, randomisation_id, start_date) %>%
		dplyr::select(
		       `Group` = group_label,
		       `Rnd. ID` = randomisation_id,
		       `PT` = pt,
		       `Start Date`= start_date,
		       `Start Time`= start_time,
		       `End Date` = end_date,
		       `End Time` = end_time,
		       Outcome = outcome,
		       `Severity` = severity_label,
		       Causality = causality_coded
		)

	tables_ls$table_4 <- all_ae_tbl


	# Return tables ------------------------------------
	return(tables_ls)

}
