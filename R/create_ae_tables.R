#' Create AE tables
#' 
#' DESCRIPTION 
#' 
#' @param ae_data A string containing the path to AE data in csv format, or the
#' AE data as a data frame.
#' 
#' @return A list containing the four AE reporting tables.
#' @export 

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

	# Perform checks ----------------------------------
	# Check if necessary variables are present in file
	vars <- c(
		  "screening_id",
		  "cohort_label",
		  "ae_description_2",
		  "randomization_number", 
		  "ae_description_2",
		  "start_date_and_time",
		  "end_date_and_time",
		  "outcome",
		  "severity_label",
		  "causality_2",
		  "aesi",
		  "sae",
		  "action_taken_with_imp"
	)

	if (!all(vars %in% names(ae_tbl))) {
		missing_vars <- setdiff(vars, names(ae_tbl))
		stop(paste("Some of the necessary input variables are missing from the input data. The missing variables are:", missing_vars))
	}

	# Prepare table 1 ----------------------------------------------------
	# Overview of subjects reporting at least one AE

	# Find out which subjects has at least one of:
	any_tbl <-
		ae_tbl %>%
		dplyr::group_by(screening_id, cohort_label) %>%
		dplyr::summarise(
			  `Any AE` = !anyNA(ae_description_2),
			  `Any AR` = any(causality_2 == "Related", na.rm = TRUE),
			  `Any AESI` = any(aesi == "Yes", na.rm = TRUE),
			  `Any SAE` = any(sae == "Yes", na.rm = TRUE),
			  `Any AE leading to death` = any(outcome == "Fatal", na.rm = TRUE),
			  `Any AE leading to discontinuation` = any(action_taken_with_imp != "None", na.rm = TRUE),
			  `Number of subjects` = TRUE
			  ) %>%
		dplyr::ungroup()

	# Sum by number of subjects that qualify by cohort
	subjects_w_at_least_one_ae_tbl <-
		any_tbl %>%
		dplyr::group_by(cohort_label) %>%
		dplyr::summarise_if(is.logical, sum) %>%
		tidyr::gather(-cohort_label, key = item, val = n) %>%
		tidyr::spread(key = cohort_label, val = n) %>%
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
		dplyr::select(-arr_col)

	tables_ls$table_1 <- subjects_w_at_least_one_ae_tbl

	# Prepare table 2 ---------------------------------
	# Subjects reporting AEs that have also been reported by at least one
	# other subject

	# Grab number of subjects by cohort (a bit hacky)
	persons_per_cohort_tbl <-
		ae_tbl %>%
		dplyr::group_by(cohort_label) %>%
		dplyr::summarise(
			  n = n_distinct(screening_id)
			  ) %>%
		tidyr::spread(key = cohort_label, value = n) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across())) %>%
		tidyr::gather(key = key, val = `Number of subjects`)

	# Prepare table
	subjects_w_common_ae_tbl <-
		ae_tbl %>%
		# Remove all AE-types that are only observed once
		dplyr::group_by(ae_description_2) %>%
		tidyr::drop_na(ae_description_2) %>%
		dplyr::mutate(n_subjects = n_distinct(screening_id)) %>%
		dplyr::filter(n_subjects >= 2) %>%
		# get number of subjects per cohort with qualifying AE
		dplyr::group_by(ae_description_2, cohort_label) %>%
		dplyr::summarise(
			  n = n_distinct(screening_id)
			  ) %>%
		tidyr::spread(key = cohort_label, val = n, fill = 0) %>%
		dplyr::ungroup() %>%
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across(-c(ae_description_2)))) %>%
		# Reshape so persons_per_cohort_tbl fits
		tidyr::gather(-ae_description_2, key = key, val = val) %>%
		tidyr::spread(key = ae_description_2, val = val) %>%
		dplyr::left_join(persons_per_cohort_tbl) 

	# Create arrange column
	arrange_col <- 
		subjects_w_common_ae_tbl %>%
		tidyr::gather(-key, key = ae_description_2, val = val) %>%
		tidyr::spread(key = key, val = val) %>%
		dplyr::arrange(desc(All)) %>%
		dplyr::mutate(arr_col = All) %>%
		dplyr::select(ae_description_2, arr_col)

	# Add parenthesis with percentage share of cohort total
	subjects_w_common_ae_tbl <- 
		subjects_w_common_ae_tbl %>%
		# Add parenthesis
		dplyr::mutate_if(is.numeric, ~ stringr::str_glue("{.x} ({round(.x/`Number of subjects` * 100, 1)}%)")) %>%
		# reshape back to output format
		tidyr::gather(-key, key = ae_description_2, val = val) %>%
		tidyr::spread(key = key, val = val) %>% 
		# Arrange
		dplyr::left_join(arrange_col) %>%
		dplyr::arrange(desc(arr_col)) %>%
		dplyr::select(-arr_col)

	tables_ls$table_2 <- subjects_w_common_ae_tbl

	# Prepare table 3 --------------------------------
	# Number of adverse events which were reported by more than one subject

	# Get table of AEs by cohort
	ae_per_cohort_tbl <- 
		ae_tbl %>%
		dplyr::group_by(cohort_label) %>%
		tidyr::drop_na(ae_description_2) %>%
		dplyr::summarise(
			  n = n()
			  ) %>%
		tidyr::spread(key = cohort_label, value = n) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(
		       All = sum(dplyr::c_across())
		       ) %>%
		tidyr::gather(key = key, val = `Number of adverse events`)

	# Prepare table
	number_of_common_ae_tbl <-
		ae_tbl %>%
		# Remove all AE-types that are only observed by one subject
		dplyr::group_by(ae_description_2) %>%
		tidyr::drop_na(ae_description_2) %>%
		dplyr::filter(n_distinct(screening_id) >= 2) %>%
		# Count the number of AEs reported
		dplyr::group_by(cohort_label, ae_description_2) %>%
		dplyr::summarise(n = n()) %>%
		# Format so cohorts are columns
		tidyr::spread(key = cohort_label, val = n, fill = 0) %>%
		# Create "All" col
		dplyr::rowwise() %>%
		dplyr::mutate(All = sum(dplyr::c_across(-ae_description_2))) %>%
		# reshape so ae_per_cohort_tbl fits
		tidyr::gather(-ae_description_2, key = key, val = val) %>%
		tidyr::spread(key = ae_description_2, val = val) %>%
		dplyr::left_join(ae_per_cohort_tbl)

	# Create arrange column
	arrange_col <- 
		number_of_common_ae_tbl %>%
		tidyr::gather(-key, key = ae_description_2, val = val) %>%
		tidyr::spread(key = key, val = val) %>%
		dplyr::arrange(desc(All)) %>%
		dplyr::mutate(arr_col = All) %>%
		dplyr::select(ae_description_2, arr_col)

	# Add parenthesis with percentage share of cohort total
	number_of_common_ae_tbl <-
		number_of_common_ae_tbl %>%
		# Add parenthesis
		dplyr::mutate_if(is.numeric, ~ stringr::str_glue("{.x} ({round(.x/`Number of adverse events` * 100, 1)}%)")) %>%
		# reshape back to output format
		tidyr::gather(-key, key = ae_description_2, val = val) %>%
		tidyr::spread(key = key, val = val) %>% 
		# Arrange
		dplyr::left_join(arrange_col) %>%
		dplyr::arrange(desc(arr_col)) %>%
		dplyr::select(-arr_col)

	tables_ls$table_3 <- number_of_common_ae_tbl

	# Prepare table 4 ---------------------------------
	# List of adverse events by subject

	all_ae_tbl <-
		ae_tbl %>%
		tidyr::drop_na(ae_description_2) %>%
		dplyr::arrange(cohort_label, randomization_number, start_date_and_time) %>%
		dplyr::select(
		       `Cohort` = cohort_label,
		       `Rnd. Number` = randomization_number,
		       `AE description` = ae_description_2,
		       Start = start_date_and_time,
		       End = end_date_and_time,
		       Outcome = outcome,
		       `Sev.` = severity_label,
		       Causality = causality_2
		)

	tables_ls$table_4 <- all_ae_tbl


	# Return tables ------------------------------------
	return(tables_ls)

}

