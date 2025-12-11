library(tidyverse)

tbl_ga_fn <- function(anaes_group) {
  # Define the groupings
  anaes_groups <- list(
    "GA" = c("GA Only", "GA + Caudal", "GA + Spinal"),
    "Spinal" = c("Spinal Only", "Spinal + Sedation"),
    "Caudal" = c("Caudal Only", "Caudal + Sedation"),
    "all" = unique(retro$anaes_type))
  
  filter_values <- anaes_groups[[anaes_group]]
  
  # Filter the data and drop unused levels
  filtered_data <- retro |>  
    filter(anaes_type %in% filter_values) |>
    mutate(across(where(is.factor), droplevels))
  
  # Create a new variable for reversal agent that's conditional on NMBA use
  # This assumes nmba_type has a level like "None" or NA for no NMBA
  filtered_data <- filtered_data |>
    mutate(
      rev_agent_conditional = case_when(
        is.na(nmba_type) | nmba_type == "None" ~ NA_character_,
        TRUE ~ as.character(rev_agent)),
      rev_agent_conditional = factor(rev_agent_conditional, levels = c("Sugammadex", "Neostigmine", "None")))
  
  # Check which variables have meaningful data (not just "None")
  # Updated order: ga_type, ga_adj, nmba, nmba_type, local_type, sed_type, airway_type
  vars_to_check <- c("ga_type", "ga_adj", "nmba", "nmba_type", "local_type", "sed_type", "airway_type")
  vars_with_data <- c("anaes_type")
  
  for (var in vars_to_check) {
    if (var %in% names(filtered_data)) {
      non_na_values <- filtered_data[[var]][!is.na(filtered_data[[var]])]
      if (length(non_na_values) > 0 && !all(non_na_values == "None")) {
        vars_with_data <- c(vars_with_data, var)}
    }
  }
  
  # Check if rev_agent_conditional has data (only include if NMBA was used)
  if ("nmba_type" %in% vars_with_data) {
    non_na_rev <- filtered_data$rev_agent_conditional[!is.na(filtered_data$rev_agent_conditional)]
    if (length(non_na_rev) > 0) {
      # Insert rev_agent_conditional right after nmba_type
      nmba_position <- which(vars_with_data == "nmba_type")
      vars_with_data <- append(vars_with_data, "rev_agent_conditional", after = nmba_position)}
  }
  
  vars_with_data <- c(vars_with_data, "age_cat")
  
  # Create labels
  labels_list <- list(anaes_type = "Anaesthetic Type")
  if ("ga_type" %in% vars_with_data) labels_list$ga_type <- "Type of GA"
  if ("ga_adj" %in% vars_with_data) labels_list$ga_adj <- "GA Adjunct"
  if ("nmba" %in% vars_with_data) labels_list$nmba <- "NMBA"
  if ("nmba_type" %in% vars_with_data) labels_list$nmba_type <- "Type of NMBA (% of those with NMBA)"
  if ("rev_agent_conditional" %in% vars_with_data) labels_list$rev_agent_conditional <- "NMBA Reversal Agent (% of those with NMBA)"
  if ("local_type" %in% vars_with_data) labels_list$local_type <- "Local Anaesthetic"
  if ("sed_type" %in% vars_with_data) labels_list$sed_type <- "Type of Sedation"
  if ("airway_type" %in% vars_with_data) labels_list$airway_type <- "Type of Airway"
  
  # Value list for binary variables
  value_list <- list()
  if ("nmba" %in% vars_with_data) value_list$nmba <- 1
  
  
  # Create the table
  tbl <- filtered_data |> 
    select(all_of(vars_with_data)) |>
    mutate(across(where(is.factor), droplevels)) |>
    tbl_summary(by = age_cat, missing = "no",
                label = labels_list,
                value = value_list) |> 
    modify_spanning_header(all_stat_cols() ~ "**Age Categories at Birth**") |> 
    add_overall() |> 
    bold_labels() |> 
    italicize_labels() |> 
    modify_table_body(
      ~ .x |>
        dplyr::mutate(
          across(starts_with("stat_"), ~ stringr::str_replace_all(., "^0 \\(.*\\)$", "0")))) |> 
    modify_table_body(
      ~.x |> 
        filter(!(variable %in% c("local_type", "sed_type", "ga_adj") & label == "None")))
  
  return(tbl)
}

# Function to make gtsummary tables for intraop analgesia for each type of anaesthetic
tbl_intraop_analg_fn <- function(strata){
  dat <- retro |> 
    filter(anaes_type == strata)
  
  # Ensure one row per patient before counting
  dat <- if ("count_pt" %in% names(dat)) {
    dat |> filter(count_pt == TRUE)
  } else if ("record_id" %in% names(dat)) {
    dat |> distinct(record_id, .keep_all = TRUE)
  } else {
    dat |> mutate(record_id = row_number()) |> distinct(record_id, .keep_all = TRUE)
  }
  
  # Create a simple patient indicator (TRUE for included rows)
  dat <- dat |> mutate(patientn = TRUE)
  
  tbl <- dat |> 
    select(patientn, intraop_para, intraop_parecoxib, intraop_opioid, age_cat) |>  
    tbl_summary(
      by = age_cat,
      missing = "no",
      label = list(
        patientn = strata,
        intraop_para = "Paracetamol",
        intraop_parecoxib = "Parecoxib",   
        intraop_opioid = "Opioid"),
      value = list(
        patientn ~ "TRUE",
        intraop_para ~ 1,
        intraop_parecoxib ~ 1,
        intraop_opioid ~ 1),
      statistic = list(
        patientn = "{n}" )) |> 
    add_overall() |> 
    modify_header(all_stat_cols() ~ "**{level}**") |> 
    modify_table_styling(
      rows = variable == "patientn",
      columns = everything(),
      text_format = "bold") |> 
    modify_table_styling(
      rows = variable %in% c("intraop_para", "intraop_parecoxib", "intraop_opioid"),
      columns = label,
      text_format = "italic",
      indent = 4)
  
  return(tbl)
}

# Function to make gtsummary tables for postop analgesia for each type of anaesthetic
tbl_postop_analg_fn <- function(strata){
  dat <- retro |> 
    filter(anaes_type == strata)
  
  # Ensure one row per patient before counting
  dat <- if ("count_pt" %in% names(dat)) {
    dat |> filter(count_pt == TRUE)
  } else if ("record_id" %in% names(dat)) {
    dat |> distinct(record_id, .keep_all = TRUE)
  } else {
    dat |> mutate(record_id = row_number()) |> distinct(record_id, .keep_all = TRUE)
  }
  
  # Create a simple patient indicator (TRUE for included rows)
  dat <- dat |> mutate(patientn = TRUE)
  
  tbl <- dat |> 
    select(patientn, postop_para, postop_nsaid, postop_opioid, age_cat) |>  
    tbl_summary(
      by = age_cat,
      missing = "no",
      label = list(
        patientn = strata,
        postop_para = "Paracetamol",
        postop_nsaid = "NSAID",   
        postop_opioid = "Opioid"),
      value = list(
        patientn ~ "TRUE",
        postop_para ~ 1,
        postop_nsaid ~ 1,
        postop_opioid ~ 1),
      statistic = list(
        patientn = "{n}" )) |> 
    add_overall() |> 
    modify_header(all_stat_cols() ~ "**{level}**") |> 
    modify_table_styling(
      rows = variable == "patientn",
      columns = everything(),
      text_format = "bold") |> 
    modify_table_styling(
      rows = variable %in% c("postop_para", "postop_nsaid", "postop_opioid"),
      columns = label,
      text_format = "italic",
      indent = 4)
  
  return(tbl)
}


# Function to rename complications & interventions
make_case_variables <- function(data,
                                events,
                                type,
                                time,
                                event_no = 1:3,
                                mappings,
                                intervention = FALSE,
                                suffix = ".x",
                                var_prefix = NULL,
                                comp_time) {
  # Input validation
  stopifnot(length(events) == length(mappings))
  
  # If var_prefix is provided and intervention = FALSE, it should match length of events
  if (!is.null(var_prefix) && !intervention) {
    stopifnot(length(var_prefix) == length(events))
  }
  
  # Outer loop over event numbers
  purrr::reduce(event_no, function(df_outer, curr_event_no) {
    
    # Inner loop over events and mappings
    purrr::reduce2(seq_along(events), mappings, function(df, idx, map) {
      ev <- events[idx]
      
      # Define variable names
      if (intervention) {
        var_name <- glue::glue("{ev}_int_{type}_{curr_event_no}")
      } else {
        # Use var_prefix if provided, otherwise use ev
        prefix <- if (!is.null(var_prefix)) var_prefix[idx] else ev
        var_name <- glue::glue("{prefix}_{type}_{time}_{curr_event_no}")
      }
      
      # Define source column (always uses ev)
      src_var <- if (intervention) {
        glue::glue("stat_{ev}_int_{curr_event_no}{suffix}")
      } else if (comp_time){
        glue::glue("stat_{ev}_time_{curr_event_no}{suffix}")
      }else {
        glue::glue("stat_{ev}_{curr_event_no}{suffix}")
      }
      
      # Check if source column exists
      if (!src_var %in% names(df)) {
        warning(paste("Column", src_var, "not found in data"))
        return(df)
      }
      
      # Reverse the mapping (value -> label)
      recode_map <- purrr::set_names(names(map), as.character(unlist(map)))
      
      # Apply recoding
      df[[var_name]] <- dplyr::recode(as.character(df[[src_var]]), !!!recode_map, .default = NA_character_)
      
      df
    }, .init = df_outer)
    
  }, .init = data)
}

# Function to make gtsummary tables for each type of anaesthetic and complications
# Function for complication types
tbl_compl_vert_percent_fn <- function(strata = "All",
                         intraop = TRUE) {
  
  # Filter by strata if provided and calculate N
  # Filter data based on strata
  retro_filtered <- if (strata == "All") {
    retro
  } else {
    retro  |>  
      dplyr::filter(anaes_type == strata)
  }
  
  # Filter differently based on intraop vs postop
  if (isTRUE(intraop)) {
    retro_filtered <- retro_filtered |>
      dplyr::mutate(complication = case_when(complication_type_intraop_1 == "None" ~ 0,
                                             TRUE ~ 1),
                    complication = factor(complication, levels = c(0,1)))
  } else {
    # Postop
    retro_filtered <- retro_filtered |>
      dplyr::mutate(complication = case_when(complication_type_postop_1 == "None" ~ 0,
                                             TRUE ~ 1),
                    complication = factor(complication, levels = c(0,1)))
  }
  
  # Create the table
  tbl <- retro_filtered |>
    gtsummary::select(patientn = count_pt, complication, age_cat) |>
    tbl_summary(
      by = age_cat,
      missing = "no",
      label = list(
        patientn = paste0(strata, ", N = "),
        complication = " "),
      statistic = list(
        patientn = "{n}" ),
      value = list(
        patientn ~ "TRUE",
        complication ~ 1)) |>
    add_overall() |>
    modify_footnote(everything() ~ NA) |>
    modify_header(all_stat_cols() ~ "**{level}**") |> 
    modify_table_styling(
      rows = variable == "patientn",
      columns = everything(),
      text_format = "bold") |> 
    modify_table_body(
      ~ .x |>
        dplyr::mutate(
          across(starts_with("stat_"), ~ stringr::str_replace_all(., "^0 \\(.*\\)$", "0"))))
  
  return(tbl)
}

# Function to make gtsummary tables for each type of anaesthetic and complications
tbl_compl_fn <- function(strata,
                         intraop = TRUE){
  
  # Calculate denominators based on the strata
  if(!is.null(strata) && strata != "All") {
    # Get the N for each age category within this specific anaes_type
    strata_age_n <- retro |>
      filter(anaes_type == strata) |>
      count(age_cat) |>
      pivot_wider(names_from = age_cat, values_from = n, values_fill = 0) |>
      mutate(Overall = sum(c_across(everything())))
    
    retro_filtered <- retro |> filter(anaes_type == strata)
    
    # The row total is the total N for this strata
    row_total <- strata_age_n$Overall
  } else {
    # Use overall denominators
    strata_age_n <- overall_age_n
    retro_filtered <- retro
    row_total <- overall_age_n$Overall
  }
  
  # Filter differently based on intraop vs postop
  if (isTRUE(intraop)) {
    dat <- retro_filtered |>
      filter(complication_type_intraop_1 != "None")
  } else {
    # Postop
    dat <- retro_filtered |>
      filter(complication_type_postop_1 != "None")
  }
  
  # Count number of patients that had complication
  dat <- dat |> 
    group_by(record_id) |> 
    mutate(count_pt = row_number() == 1L) |> 
    ungroup()
  
  # If no complications, add a dummy row
  if(nrow(dat) == 0) {
    dat <- tibble(count_pt = FALSE, age_cat = retro$age_cat[1])
  }
  
  # Set the label for the row - add (N = X) for non-"All" strata
  if(is.null(strata) || strata == "All") {
    row_label <- "Overall"
  } else {
    row_label <- paste0(strata, " (N = ", strata_age_n$Overall, ")")
  }
  
  # Get counts by age category - ensure proper ordering
  age_levels <- levels(retro$age_cat)
  complication_counts <- dat |>
    filter(count_pt == TRUE) |>
    count(age_cat) |>
    complete(age_cat = factor(age_levels, levels = age_levels), fill = list(n = 0)) |>
    arrange(age_cat)
  
  # Calculate overall count (total complications for this strata)
  overall_count <- sum(complication_counts$n)
  
  # Calculate ROW percentages - each as a percentage of the TOTAL N for this strata
  n_27 <- complication_counts$n[complication_counts$age_cat == age_levels[1]]
  pct_27 <- round(100 * n_27 / row_total, 1)  # Divide by total strata N
  
  n_28_32 <- complication_counts$n[complication_counts$age_cat == age_levels[2]]
  pct_28_32 <- round(100 * n_28_32 / row_total, 1)  # Divide by total strata N
  
  n_33_36 <- complication_counts$n[complication_counts$age_cat == age_levels[3]]
  pct_33_36 <- round(100 * n_33_36 / row_total, 1)  # Divide by total strata N
  
  n_37 <- complication_counts$n[complication_counts$age_cat == age_levels[4]]
  pct_37 <- round(100 * n_37 / row_total, 1)  # Divide by total strata N
  
  pct_overall <- round(100 * overall_count / row_total, 1)  # Total complications / total N
  
  tbl <- dat |> 
    select(patientn = count_pt, age_cat) |>  
    tbl_summary(by = age_cat, missing = "no",
                label = list(patientn = row_label),
                statistic = list(patientn = "{n}")) |> 
    add_overall() |> 
    modify_header(
      stat_1 ~ paste0("**≤ 27 Weeks**", "<br>", "N = ", strata_age_n[[age_levels[1]]]),
      stat_2 ~ paste0("**28-32 Weeks**", "<br>", "N = ", strata_age_n[[age_levels[2]]]),
      stat_3 ~ paste0("**33-36 Weeks**", "<br>", "N = ", strata_age_n[[age_levels[3]]]),
      stat_4 ~ paste0("**≥37 Weeks**", "<br>", "N = ", strata_age_n[[age_levels[4]]]),
      stat_0 ~ paste0("**Overall**", "<br>", "N = ", strata_age_n$Overall)) |>
    modify_table_body(
      ~ .x |>
        mutate(
          stat_1 = if_else(variable == "patientn", paste0(n_27, " (", pct_27, "%)"), stat_1),
          stat_2 = if_else(variable == "patientn", paste0(n_28_32, " (", pct_28_32, "%)"), stat_2),
          stat_3 = if_else(variable == "patientn", paste0(n_33_36, " (", pct_33_36, "%)"), stat_3),
          stat_4 = if_else(variable == "patientn", paste0(n_37, " (", pct_37, "%)"), stat_4),
          stat_0 = if_else(variable == "patientn", paste0(overall_count, " (", pct_overall, "%)"), stat_0)))
  
  return(tbl)
}

# Function for complication types
tbl_complication_types_fn <- function(complication_cols, 
                                      label = "Complication Type",
                                      strata = NULL,
                                      intervention = FALSE,
                                      add_n_to_label = FALSE,
                                      percent_denominator = c("complications", "column_total")) {
  
  # Match the argument
  percent_denominator <- match.arg(percent_denominator)
  
  # Start with the data
  dat <- retro
  
  # Filter by strata if provided and calculate N
  if(!is.null(strata) && strata != "Overall") {
    dat <- dat |> dplyr::filter(anaes_type == strata)
  }
  
  # Calculate the N for this strata
  strata_n <- nrow(dat)
  
  # If using column_total denominator, we need to calculate totals by age_cat
  if (percent_denominator == "column_total") {
    # Calculate total patients per age category from original data
    age_totals <- dat |>
      dplyr::group_by(age_cat) |>
      dplyr::summarise(total_n = dplyr::n(), .groups = "drop")
    
    overall_total <- nrow(dat)
  }
  
  # Pivot longer with the specified columns
  dat_long <- dat |>
    dplyr::select(record_id, age_cat, count_pt, dplyr::all_of(complication_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(complication_cols),
      names_to = "complication_column",
      values_to = "complication_type")
  
  # Filter differently based on whether this is intervention or complication data
  if (intervention) {
    # For interventions: keep "None" but remove NA
    dat_long <- dat_long |>
      dplyr::filter(!is.na(complication_type)) |> 
      dplyr::mutate(complication_type = forcats::fct_relevel(complication_type, "Nil Documentation", "None", after = Inf))
  } else {
    # For complications: remove both NA and "None"
    dat_long <- dat_long |>
      dplyr::filter(!is.na(complication_type) & complication_type != "None")
  }
  
  if (percent_denominator == "column_total") {
    # Calculate counts for each complication type by age category
    complication_counts <- dat_long |>
      dplyr::group_by(complication_type, age_cat) |>
      dplyr::summarise(n_complications = dplyr::n_distinct(record_id), .groups = "drop")
    
    # Calculate overall counts
    overall_counts <- dat_long |>
      dplyr::group_by(complication_type) |>
      dplyr::summarise(n_overall = dplyr::n_distinct(record_id), .groups = "drop")
    
    # Join with totals and calculate percentages
    complication_stats <- complication_counts |>
      dplyr::left_join(age_totals, by = "age_cat") |>
      dplyr::mutate(
        percent = round(100 * n_complications / total_n, 1),
        stat_display = paste0(n_complications, " (", percent, "%)")
      )
    
    # Add overall stats
    overall_stats <- overall_counts |>
      dplyr::mutate(
        percent = round(100 * n_overall / overall_total, 1),
        stat_0 = paste0(n_overall, " (", percent, "%)")
      )
    
    # Create a custom tbl_summary-like structure
    # First create the base table with standard percentages
    tbl <- dat_long |>
      gtsummary::select(patientn = count_pt, complication_type, age_cat) |>
      tbl_summary(
        by = age_cat,
        missing = "no",
        label = list(
          patientn = label,
          complication_type = strata),
        statistic = list(
          patientn = "{n}",
          complication_type = "{n} ({p}%)")) |>
      add_overall()
    
    # Now manually update the statistics in the table body using purrr
    # Get the table body
    tbl_body <- tbl$table_body
    
    # Find indices of complication_type rows
    comp_rows <- which(tbl_body$variable == "complication_type" & tbl_body$row_type == "level")
    
    # Update the statistics for complication_type rows using purrr::walk
    purrr::walk(comp_rows, function(i) {
      comp_type <- tbl_body$label[i]
      
      # Update each age category column using purrr::iwalk
      age_cats <- unique(dat$age_cat)
      purrr::iwalk(age_cats, function(age, j) {
        stat_col <- paste0("stat_", j)
        
        # Find matching stat
        stat_val <- complication_stats |>
          dplyr::filter(complication_type == comp_type, age_cat == age) |>
          dplyr::pull(stat_display)
        
        tbl_body[[stat_col]][i] <<- if(length(stat_val) > 0) stat_val else "0 (0.0%)"
      })
      
      # Update overall column
      overall_val <- overall_stats |>
        dplyr::filter(complication_type == comp_type) |>
        dplyr::pull(stat_0)
      
      tbl_body$stat_0[i] <<- if(length(overall_val) > 0) overall_val else "0 (0.0%)"
    })
    
    # Put the modified body back
    tbl$table_body <- tbl_body
    
  } else {
    # Use default behavior (percentages of complications)
    tbl <- dat_long |>
      gtsummary::select(patientn = count_pt, complication_type, age_cat) |>
      tbl_summary(
        by = age_cat,
        missing = "no",
        label = list(
          patientn = label,
          complication_type = strata),
        statistic = list(
          patientn = "{n}")) |>
      add_overall()
  }
  
  # Apply the common formatting
  tbl <- tbl |>
    modify_spanning_header(all_stat_cols() ~ "**Age Categories at Birth**") |>
    modify_spanning_header(stat_0 ~ NA) |>
    modify_footnote(everything() ~ NA) |>
    modify_header(all_stat_cols() ~ "**{level}**") |> 
    modify_table_styling(
      rows = variable == "patientn",
      columns = everything(),
      text_format = "bold")
  
  # Now modify the table body
  tbl <- tbl |>
    modify_table_body(
      ~ .x |>
        dplyr::mutate(
          # Add (N = x) to patientn label if requested
          label = dplyr::if_else(
            variable == "patientn" & add_n_to_label & !is.null(strata) & strata != "Overall",
            paste0(label, " (N = ", strata_n, ")"),
            label)) |>
        dplyr::filter(!(row_type == "label" & variable == "complication_type")) |> 
        dplyr::mutate(
          dplyr::across(dplyr::starts_with("stat_"), ~ stringr::str_replace_all(., "^0 \\(.*\\)$", "0"))))
  
  return(tbl)
}

# Function to generate tables with complication timing
tbl_complication_timing <- function(data, strata = NULL) {
  prefixes <- c("brady","hypo","apn","des","bro","lar","othr")
  label_map <- c(
    brady = "Bradycardia",
    hypo  = "Hypotension",
    apn   = "Apnoea",
    des   = "Desaturation",
    bro   = "Bronchospasm",
    lar   = "Laryngospasm",
    othr  = "Other"
  )
  time_map <- c(
    `0` = "≤30m Post-Operative",
    `1` = "31m-12h Post-Operative",
    `2` = "12-48h Post-Operative"
  )
  cols_pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")_time_postop_[1-5]$")
  
  long_dat <- data |>
    tidyr::pivot_longer(
      cols = dplyr::matches(cols_pattern),
      names_to = c("prefix", "event"),
      names_pattern = "^(brady|hypo|apn|des|bro|lar|othr)_time_postop_(\\d)$",
      values_to = "time_code",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      time_label = factor(dplyr::recode(as.character(time_code), !!!time_map),
                          levels = c(time_map[["0"]], time_map[["1"]], time_map[["2"]])),
      complication = dplyr::recode(prefix, !!!label_map)
    ) |>
    dplyr::mutate(
      stat_event_othr_1.y = dplyr::if_else(!is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "hypertension"), "Hypertension", stat_event_othr_1.y),
      stat_event_othr_1.y = dplyr::if_else(!is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "achypnoea|achyponea"), "Tachypnoea", stat_event_othr_1.y),
      stat_event_othr_1.y = dplyr::if_else(!is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "acchycardia"), "Tachycardia", stat_event_othr_1.y),
      stat_event_othr_1.y = dplyr::if_else(!is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "Respiratory acidosis"), "Respiratory Distress", stat_event_othr_1.y),
      stat_event_othr_2.y = dplyr::if_else(!is.na(stat_event_othr_2.y) & stringr::str_detect(stat_event_othr_2.y, "hypertension"), "Hypertension", stat_event_othr_2.y),
      stat_event_othr_3.y = dplyr::if_else(!is.na(stat_event_othr_3.y) & stringr::str_detect(stat_event_othr_3.y, "hypothermic"), "Hypothermia", stat_event_othr_3.y),
      stat_event_othr_4.y = dplyr::if_else(!is.na(stat_event_othr_4.y) & stringr::str_detect(stat_event_othr_4.y, "Tacchycardia"), "Tachycardia", stat_event_othr_4.y)
    ) |>
    dplyr::mutate(
      complication = dplyr::case_when(
        complication == "Other" & !is.na(stat_event_othr_1.y) ~ stat_event_othr_1.y,
        complication == "Other" & !is.na(stat_event_othr_2.y) ~ stat_event_othr_2.y,
        complication == "Other" & !is.na(stat_event_othr_3.y) ~ stat_event_othr_3.y,
        complication == "Other" & !is.na(stat_event_othr_4.y) ~ stat_event_othr_4.y,
        TRUE ~ complication
      )
    ) |>
    dplyr::select(record_id, age_cat, anaes_type, time_label, complication)
  
  # apply strata filter if requested
  dat_use <- if (!is.null(strata) && strata != "Overall") {
    long_dat |> dplyr::filter(anaes_type == strata)
  } else {
    long_dat
  }
  
  # age levels (preserve factor order if present)
  age_levels <- if (is.factor(dat_use$age_cat)) levels(dat_use$age_cat) else sort(unique(dat_use$age_cat))
  # DENOMINATORS: total number of complications (rows), overall and by age_cat
  denom_overall <- nrow(dat_use)
  denom_by_age <- dat_use |> dplyr::count(age_cat)
  denom_by_age <- tibble::tibble(age_cat = age_levels) |>
    dplyr::left_join(denom_by_age, by = "age_cat") |>
    dplyr::mutate(n = tidyr::replace_na(n, 0))
  denom_vec <- c(Overall = denom_overall, setNames(denom_by_age$n, denom_by_age$age_cat))
  
  make_tbl_for <- function(df_time, time_label_text) {
    if (nrow(df_time) == 0) return(NULL)
    
    ordered_levels <- df_time |>
      dplyr::count(complication, name = "n_comp") |>
      dplyr::arrange(dplyr::desc(n_comp)) |>
      dplyr::pull(complication)
    
    tbl <- df_time |>
      dplyr::mutate(complication = forcats::fct_inorder(forcats::fct_relevel(complication, ordered_levels))) |>
      dplyr::select(complication, age_cat) |>
      gtsummary::tbl_summary(
        by = age_cat,
        missing = "no",
        statistic = gtsummary::all_categorical() ~ "{n} ({p}%)"
      ) |>
      gtsummary::add_overall(statistic = gtsummary::all_categorical() ~ "{n} ({p}%)") 
    
    # set top header to include N = denominators (programmatic modify_header)
    stat_count <- length(age_levels) + 1
    stat_names <- paste0("stat_", 0:(stat_count - 1))
    header_labels <- c(Overall = denom_vec["Overall"], setNames(as.character(denom_vec[age_levels]), age_levels))
    header_texts <- paste0("**", names(header_labels), "<br>N = ", header_labels, "**")
    mh_args <- as.list(header_texts)
    names(mh_args) <- stat_names
    tbl <- do.call(gtsummary::modify_header, c(list(tbl), mh_args))
    tbl <- tbl |>
      gtsummary::modify_header(label ~ "**Complication Timing & Type**") |>
      gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "**Age Categories at Birth**") |> 
      modify_spanning_header(stat_0 ~ NA) |>
      modify_table_body(
        ~ .x |>
          dplyr::mutate(
            dplyr::across(dplyr::starts_with("stat_"), ~ stringr::str_replace_all(., "^0 \\(.*\\)$", "0"))))
    
    # NUMERATORS: number of complications in this time window (rows), overall and by age_cat
    numer_overall <- nrow(df_time)
    numer_by_age <- df_time |> dplyr::count(age_cat)
    numer_by_age <- tibble::tibble(age_cat = age_levels) |>
      dplyr::left_join(numer_by_age, by = "age_cat") |>
      dplyr::mutate(n = tidyr::replace_na(n, 0))
    numer_vec <- c(Overall = numer_overall, setNames(numer_by_age$n, numer_by_age$age_cat))
    
    format_cell <- function(numer, denom) {
      if (denom == 0) return("0 (0.0%)")
      pct <- 100 * numer / denom
      sprintf("%d (%.1f%%)", numer, pct)
    }
    formatted_vals <- purrr::map2_chr(numer_vec, denom_vec[names(numer_vec)], format_cell)
    
    # build new "label" row and insert into table body (label row shows counts for time window)
    tb <- tbl$table_body
    stat_cols <- grep("^stat_", names(tb), value = TRUE)
    new_label_row <- tb[1, , drop = FALSE]
    new_label_row[,] <- NA
    new_label_row$label <- time_label_text
    new_label_row$row_type <- "label"
    new_label_row$variable <- NA_character_
    stat_order <- c("Overall", age_levels)
    vals_in_order <- formatted_vals[stat_order]
    new_label_row[stat_cols] <- as.list(vals_in_order)
    
    new_tb <- tb |>
      dplyr::filter(row_type != "label") |>
      dplyr::select(names(tb))
    new_tb <- dplyr::bind_rows(new_label_row, new_tb)
    tbl$table_body <- new_tb
    tbl
  }
  
  tbls <- list(
    make_tbl_for(dat_use |> dplyr::filter(time_label == time_map[["0"]]), time_map[["0"]]),
    make_tbl_for(dat_use |> dplyr::filter(time_label == time_map[["1"]]), time_map[["1"]]),
    make_tbl_for(dat_use |> dplyr::filter(time_label == time_map[["2"]]), time_map[["2"]])
  ) |> purrr::compact()
  
  # stack without group headers (we already insert the time rows)
  tbl_out <- gtsummary::tbl_stack(tbls = tbls)
  
  # convert to gt and style the time_label rows (bold + grey90)
  label_values <- unique(intersect(unname(time_map), tbl_out$table_body$label))
  tbl_gt <- tbl_out |> gtsummary::as_gt()
  if (length(label_values) > 0) {
    tbl_gt <- tbl_gt |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = "grey90")
        ),
        locations = gt::cells_body(
          columns = gt::everything(),
          rows = label %in% label_values
        )
      )
  }
  
  tbl_gt
}

# Change intervention columns from NA to None for percentages of those that had the complication
# Intervention column renaming function
create_intervention_col_intraop <- function(data, i) {
  complication_col <- paste0("complication_type_intraop_", i)
  intervention_col <- paste0("intervention_intraop_", i)
  
  data |>
    mutate(
      !!intervention_col := case_when(
        is.na(.data[[complication_col]]) | .data[[complication_col]] == "None" ~ NA_character_,
        .data[[complication_col]] == "Bradycardia" ~ coalesce(.data[[paste0("brd_int_intraop_", i)]], "None"),
        .data[[complication_col]] == "Hypotension" ~ coalesce(.data[[paste0("hypo_int_intraop_", i)]], "None"),
        .data[[complication_col]] == "Desaturation" ~ coalesce(.data[[paste0("des_int_intraop_", i)]], "None"),
        .data[[complication_col]] == "Apnoea" ~ coalesce(.data[[paste0("apn_int_intraop_", i)]], "None"),
        .data[[complication_col]] == "Laryngospasm" ~ coalesce(.data[[paste0("lar_int_intraop_", i)]], "None"),
        TRUE ~ NA_character_))
}

create_intervention_col_postop <- function(data, i) {
  complication_col <- paste0("complication_type_postop_", i)
  intervention_col <- paste0("intervention_postop_", i)
  
  data |>
    mutate(
      !!intervention_col := case_when(
        is.na(.data[[complication_col]]) | .data[[complication_col]] == "None" ~ NA_character_,
        .data[[complication_col]] == "Bradycardia" ~ coalesce(.data[[paste0("brd_int_postop_", i)]], "None"),
        .data[[complication_col]] == "Hypotension" ~ coalesce(.data[[paste0("hypo_int_postop_", i)]], "None"),
        .data[[complication_col]] == "Desaturation" ~ coalesce(.data[[paste0("des_int_postop_", i)]], "None"),
        .data[[complication_col]] == "Apnoea" ~ coalesce(.data[[paste0("apn_int_postop_", i)]], "None"),
        .data[[complication_col]] == "Laryngospasm" ~ coalesce(.data[[paste0("lar_int_postop_", i)]], "None"),
        TRUE ~ NA_character_))
}

# Function for Respiratory Support-Free Days
rsfd_plot_fn <- function(data,
                         strata_var = NULL,
                         strata_value = NULL,
                         censor_days = 30) { 

  df <- data |>
    dplyr::mutate(
      resp_supp_free_days = dplyr::case_when(
        ae_type == 1 ~ 0,
        po_rsp_oxy == 1 ~ 0,
        resp_supp_type == "None" ~ hosp_los,
        postop_resp_supp == "Yes" ~ (hosp_los - resp_supp_dur_hrs),
        TRUE ~ NA_real_),
      resp_supp_free_days = resp_supp_free_days / 24,
      resp_supp_free_days = round(resp_supp_free_days, 0)
    ) |>
    dplyr::filter(!is.na(resp_supp_free_days))
  
  if(!is.null(strata_var)) {
    if(!strata_var %in% names(df)) stop("strata_var not found in data")
    if(!is.null(strata_value)) {
      df <- df[df[[strata_var]] == strata_value, , drop = FALSE]
    }
  }
  
  if(!is.null(strata_var) && is.null(strata_value)) {
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "resp_supp_free_days", fill = strata_var)) +
      ggplot2::geom_bar(position = "dodge") +
      ggplot2::scale_fill_brewer(palette = "Blues", direction = -1, na.translate = FALSE) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = strata_var))
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = resp_supp_free_days)) +
      ggplot2::geom_bar(fill = "lightblue")
  }
  
  p <- p +
    ggplot2::coord_cartesian(xlim = c(0, censor_days)) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = NULL,
      x = "Post-Operative Days",
      title = "Respiratory Support-Free Days",
      subtitle = paste0("In ", strata_value)
    )
  p
}
