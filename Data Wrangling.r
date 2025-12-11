library(tidyverse)

#==========================================DEMOGRAPHICS==========================================#

# Import data
load("/Users/benjaminmoran/Documents/Research/Projects/Current Projects/RetroNeo/RetroNeo/retro.RData")

# Create Location Prior to Surgery Variable
retro <- retro |> 
  mutate(dem_loc = as.character(dem_loc)) |> 
  mutate(dem_loc = case_when(
    dem_loc == "1" ~ "Home/Residential Care",
    dem_loc == "2" ~ "Hospital Ward",
    dem_loc == "3" ~ "NICU",
    dem_loc == "4" ~ "PICU",
    dem_loc == "5" ~ "Special Care Nursery",
    dem_loc == "6" ~ "Other Health Care Facility"
  )) |> 
  mutate(dem_loc = factor(dem_loc, levels = c("Home/Residential Care", "Hospital Ward", "NICU", "PICU", "Special Care Nursery", "Other Health Care Facility")))

# Identify patients that had more than 1 inguinal hernia surgery (index admission matching)
retro <- retro |>
  dplyr::mutate(
    # parse dates and normalise key components
    dem_dos_date = lubridate::dmy(dem_dos),
    dem_dob_date = lubridate::dmy(dem_dob),
    dem_mrn = as.character(dem_mrn),
    dem_wgt = as.character(dem_wgt),
    age_pma_days = as.character(age_pma_days),
    redcap_data_access_group = as.character(redcap_data_access_group)
  ) |>
  dplyr::mutate(
    # MRN match should not be broken by missing site value -> use MRN alone
    key_mrn = dplyr::if_else(!is.na(dem_mrn) & dem_mrn != "", paste0("MRN__", dem_mrn), NA_character_),
    # weight+PMA and DOB+PMA include site per your spec (require site present)
    key_wt_pma = dplyr::if_else(!is.na(age_pma_days) & age_pma_days != "" & !is.na(dem_wgt) & dem_wgt != "" & !is.na(redcap_data_access_group) & redcap_data_access_group != "",
                                paste0("WTPMA__", redcap_data_access_group, "__", age_pma_days, "__", dem_wgt),
                                NA_character_),
    key_dob_pma = dplyr::if_else(!is.na(dem_dob) & dem_dob != "" & !is.na(age_pma_days) & age_pma_days != "" & !is.na(redcap_data_access_group) & redcap_data_access_group != "",
                                 paste0("DOBPMA__", redcap_data_access_group, "__", dem_dob, "__", age_pma_days),
                                 NA_character_)
  ) |>
  # Choose highest-confidence available key (priority MRN -> WT+PMA -> DOB+PMA -> row fallback)
  dplyr::mutate(
    patient_match_key = dplyr::coalesce(key_mrn, key_wt_pma, key_dob_pma, paste0("ROW__", dplyr::row_number())),
    match_confidence = dplyr::case_when(
      !is.na(key_mrn) ~ "MRN",
      is.na(key_mrn) & !is.na(key_wt_pma) ~ "WT_PMA",
      is.na(key_mrn) & is.na(key_wt_pma) & !is.na(key_dob_pma) ~ "DOB_PMA",
      TRUE ~ "None"
    )
  ) |>
  # Order by surgery date within each matched group and count.
  # place NA dates last explicitly so earliest non-NA is index admission
  dplyr::group_by(patient_match_key) |>
  dplyr::arrange(is.na(dem_dos_date), dem_dos_date, .by_group = TRUE) |>
  dplyr::mutate(
    surg_sequence_index = dplyr::row_number(),       # 1 = first (index admission)
    surgeries_in_group = dplyr::n(),                 # total surgeries for the matched patient
    multiple_surgeries = surgeries_in_group > 1,     # logical
    second_surg_index = dplyr::if_else(surg_sequence_index > 1, 1L, 0L) # binary flag
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-key_mrn, -key_wt_pma, -key_dob_pma) |> 
  dplyr::mutate(dem_wgt = as.numeric(dem_wgt))

#==========================================MEDICAL HISTORY==================================#
# Wrangle mhx_ variables
retro <- retro |> 
  mutate(across(c(mhx_iugr, mhx_nicu, mhx_ivh, mhx_retin, mhx_ccd, mhx_cld, mhx_apn, mhx_intu, mhx_rsp, mhx_caff, mhx_caff_y), as.character))

retro <- retro |> 
  mutate(resp_support_prior = case_when(
    mhx_rsp_y___1 == 1 ~ "Chin Support",
    mhx_rsp_y___2 == 1 ~ "Guedel",
    mhx_rsp_y___3 == 1 ~ "LFNP",
    mhx_rsp_y___4 == 1 ~ "HM",
    mhx_rsp_y___5 == 1 ~ "HFNP",
    mhx_rsp_y___6 == 1 ~ "LMA",
    mhx_rsp_y___7 == 1 ~ "ETT",
    mhx_rsp_y_othr == "BIPAP" ~ "NIV",
    .default = "None"
  )) |> 
  mutate(resp_support_prior = factor(resp_support_prior, levels = c("LFNP", "HFNP", "NIV", "ETT", "None")))

# Relocate Demographic Variables
retro <- retro |> 
  relocate(c(age_pma_wks:age_cat, dem_wgt:dem_loc, dem_sex, mhx_iugr:mhx_rsp, mhx_caff), .after = dem_dos)

# Count row numbers to get patient numbers in table 1
retro <- retro |>  mutate(patient_id = row_number()) |>  
  group_by(patient_id) |> 
  mutate(count_pt = row_number() == 1L) |> 
  ungroup()

#==========================================TABLE 2: SURGERY==========================================#

# Change numeric into binary variable
retro <- retro |> 
  mutate(across(sur_hernia:sur_asspro, ~ as.character(.)))

# Change binary variables into character descriptors
retro <- retro |> 
  mutate(
    sur_hernia = case_when(
      sur_hernia == 0 ~ "Unilateral",
      sur_hernia == 1 ~ "Bilateral"),
    sur_type = case_when(
      sur_type == 0 ~ "Laparoscopic",
      sur_type == 1 ~ "Open"))

# Determine Surgical, Anaesthetic and Total Durations
retro <- retro |> 
  mutate(
    anaes_dur = as.double(sur_time_ana_tot),
    surg_dur = as.double(sur_time_sur_tot),
    total_dur = as.double(sur_time_oproom_tot))

# PACU Details
retro <- retro |> 
  mutate(pacu = case_when(
    po_rec_pacu == 1 ~ "Yes",
    po_rec_pacu == 0 ~ "No")) |>  
  rename("pacu_dur" = po_rec_eot_dsc)

# Relocate Surgical Variables
retro <- retro |> 
  relocate(c(sur_wgt:sur_asspro, total_dur, anaes_dur, surg_dur, pacu, pacu_dur), .after = mhx_caff)

#==========================================INTRAOPERATIVE: ANAESTHETICS==========================================#

# Type of Anaesthetic
retro <- retro |> 
  mutate(
    ga = as.character(sur_an1_ga_yn),
    spinal = as.character(sur_an1_spi_yn),
    caudal = as.character(sur_an1_sed_caud_yn),
    sedation = as.character(sur_an1_sed_yn),
    anaes_type = case_when(
      ga == "1" & spinal == "0" & caudal == "0" & sedation == "0" ~ "GA Only",
      ga == "1" & spinal == "0" & caudal == "0" & sedation == "1" ~ "GA",
      ga == "0" & spinal == "1" & caudal == "0" & sedation == "0" ~  "Spinal Only",
      ga == "0" & spinal == "0" & caudal == "1" & sedation == "0" ~ "Caudal Only",
      ga == "0" & spinal == "0" & caudal == "0" & sedation == "1" ~ "Sedation Only",
      ga == "1" & spinal == "1" & caudal == "0" & sedation == "0" ~ "GA + Spinal",
      ga == "1" & spinal == "0" & caudal == "1" & sedation == "0" ~ "GA + Caudal",
      ga == "0" & spinal == "1" & caudal == "0" & sedation == "1" ~ "Spinal + Sedation",
      ga == "0" & spinal == "0" & caudal == "1" & sedation == "1" ~ "Caudal + Sedation",
      ga == "1" & spinal == "1" & caudal == "1" & sedation == "0" ~ "GA + Spinal + Caudal",
      ga == "1" & spinal == "0" & caudal == "1" & sedation == "1" ~ "GA + Caudal + Sedation"),
    anaes_type = factor(anaes_type, levels = c("GA Only", "GA + Caudal", "GA + Spinal", "Spinal Only", "Spinal + Sedation", "Caudal Only", "Caudal + Sedation")))

# General Anaesthetics
retro <-  retro |> 
  mutate(
    ga_sevo = as.character(sur_an1_ga_drug___1),
    ga_des = as.character(sur_an1_ga_drug___2),
    ga_prop = as.character(sur_an1_ga_drug___3),
    ga_n2o = as.character(sur_an1_ga_drug___4),
    ga_clonidine = as.character(sur_an1_ga_drug___5),
    ga_dexmede = as.character(sur_an1_ga_drug___6),
    ga_type = case_when(
      ga_sevo == "1" & ga_prop == "1"  ~ "Sevoflurane + Propofol",
      ga_sevo == "1"  ~ "Sevoflurane",
      ga_prop == "1"  ~  "Propofol",
      .default = "None"),
    ga_adj = case_when(
      ga_dexmede == "1" ~ "Dexmedetomidine",
      ga_clonidine == "1" ~ "Clonidine",
      ga_n2o == "1" ~ "N2O",
      .default = "None")) |> 
  mutate(ga_type = factor(ga_type, levels = c("Sevoflurane", "Propofol", "Sevoflurane + Propofol", "None")),
         ga_adj = factor(ga_adj, levels = c("Dexmedetomidine", "Clonidine", "N2O", "None")))

# NMBDs
retro <-  retro |> 
  mutate(
    nmba = as.character(sur_an1_neur),
    sux = as.character(sur_an1_neur_agnt___1),
    roc = as.character(sur_an1_neur_agnt___2),
    atrac = as.character(sur_an1_neur_agnt___3),
    vec = as.character(sur_an1_neur_agnt___4),
    cisat = as.character(sur_an1_neur_agnt___5),
    nmba_drug = case_when(
      sux == "1" & roc == "1"  ~ "Suxamethonium + Rocuronium",
      sux == "1" & atrac == "1"  ~ "Suxamethonium + Atracurium",
      sux == "1" & vec == "1"  ~ "Suxamethonium + Vecuronium",
      sux == "1" & cisat == "1"  ~ "Suxamethonium + Cisatracurium",
      sux == "1" ~ "Suxamethonium",
      roc == "1"  ~ "Rocuronium",
      atrac == "1"  ~ "Atracurium",
      vec == "1"  ~ "Vecuronium",
      cisat == "1"  ~ "Cisatracurium"),
    nmba_drug = replace_na(nmba_drug, "None")) |> 
  mutate(nmba_drug = factor(nmba_drug, levels = c("Rocuronium", "Vecuronium", "Atracurium", "Cisatracurium", "Suxamethonium", "Suxamethonium + Rocuronium", "Suxamethonium + Vecuronium", "Suxamethonium + Atracurium", "Suxamethonium + Cisatracurium", "None"))) |> 
  mutate(
    nmba_type = case_when(
      sux == "1" & if_any(c(roc, vec, atrac, cisat), ~ .x == "1")  ~ "Depolarising + Non-Depolarising",
      sux == "1" ~ "Depolarising",
      roc == "1"  ~ "Non-Depolarising",
      atrac == "1"  ~ "Non-Depolarising",
      vec == "1"  ~ "Non-Depolarising",
      cisat == "1"  ~ "Non-Depolarising",
      TRUE ~ NA_character_))

# NMBD Reversal Agents
retro <-  retro |> 
  mutate(
    nmba_rev = as.character(replace_na(sur_an1_neur_blk, 0)),
    rev_agent = case_when(
      sur_an1_neur_blk_agnt == 1 ~ "Sugammadex",
      sur_an1_neur_blk_agnt == 2 ~ "Neostigmine",
      sur_an1_neur_blk_agnt == 0 ~ "Other",
      nmba_rev == 0 ~ "None")) |> 
  mutate(rev_agent = factor(rev_agent, levels = c("Sugammadex", "Neostigmine", "None")))

# Local Anaesthetic
retro <- retro |> 
  mutate(
    local = as.character(sur_an1_locan),
    local_type = case_when(
      sur_an1_locan_drg == 1 ~ "Ilioinguinal Block",
      sur_an1_locan_drg == 2 ~ "Local Infiltration",
      sur_an1_locan_drg == 3 ~ "Inguinal Canal Block",
      .default = "None"),
    local_type = factor(local_type, levels = c("Ilioinguinal Block", "Inguinal Canal Block", "Local Infiltration", "None")),
    local_drug = case_when(
      sur_an1_locan_drg_typ == "1" ~ "Ropivacaine",
      sur_an1_locan_drg_typ == "2" ~ "Bupivacaine",
      sur_an1_locan_drg_typ == "3" ~ "Levobupivacaine",
      sur_an1_locan_drg_typ == "4" ~ "Chirocaine",
      sur_an1_locan_drg_typ == "0" ~ "Not documented"))

# Intraoperative Analgesics
retro <-  retro |> 
  mutate(
    intraop_analgesia = as.character(sur_an1_analdrug),
    intraop_para = as.factor(sur_an1_analdrug_y___1),
    intraop_opioid = as.factor(sur_an1_analdrug_y___2),
    intraop_parecoxib = case_when(
      sur_an1_analdrug_y_othr == "Parecoxib" ~ "1",
      sur_an1_analdrug_y_othr == "parecoxib" ~ "1"),
    intraop_parecoxib = replace_na(intraop_parecoxib, "0"),
    intraop_parecoxib = as.factor(intraop_parecoxib))

# Airway
# Recode Respiratory Support Variable
retro <- retro |> 
  mutate(
    airway = as.character(sur_an1_airway),
    airway_chin_supp = as.character(sur_an1_airway_def___1),
    airway_guedel = as.character(sur_an1_airway_def___2),
    airway_lfnp = as.character(sur_an1_airway_def___3),
    airway_hm = as.character(sur_an1_airway_def___4),
    airway_hfnp = as.character(sur_an1_airway_def___5),
    airway_lma = as.character(sur_an1_airway_def___6),
    airway_ett = as.character(sur_an1_airway_def___7),
    airway_anaes_fm = case_when(
      str_detect(sur_an1_airway_def_othr, "mask") ~ "1"),
    airway_anaes_fm = replace_na(airway_anaes_fm, "0"),
    airway_type = case_when(
      airway_chin_supp == "1" ~ "Chin Support",
      airway_guedel == "1" ~ "Guedel",
      airway_lfnp == "1" ~ "LFNP",
      airway_hm == "1" ~ "Hudson Mask",
      airway_hfnp == "1" ~ "HFNP",
      airway_lma == "1" ~ "LMA",
      airway_ett == "1" ~ "ETT",
      airway_anaes_fm == "1" ~ "Anaesthetic Face Mask"),
    airway_type = replace_na(airway_type, "Unknown"),
    airway_type = factor(airway_type, levels = c("ETT", "LMA", "Anaesthetic Face Mask", "HFNP", "Hudson Mask", "LFNP", "Guedel", "Chin Support", "Unknown")))

# Spinal
retro <- retro |> 
  mutate(
    spinal_drug = as.character(sur_an1_spi_drug),
    spinal_drug = case_when(
      spinal_drug == "1" ~ "Ropivacaine",
      spinal_drug == "2" ~ "Bupivacaine",
      spinal_drug == "3" ~ "Levobupivacaine",
      spinal_drug == "4" ~ "Chirocaine",
      spinal_drug == "0" ~ "Other"),
    spinal_drug_other = sur_an1_spi_drug_othr,
    spinal_drug_conc = case_when(
      sur_an1_spi_drug_conc == 0.7000 ~ "0.7%",
      sur_an1_spi_drug_conc == 0.5000 ~ "0.5%",
      sur_an1_spi_drug_conc == 0.2500 ~ "0.25%",
      sur_an1_spi_drug_conc == 0.2000 ~ "0.2%"),
    spinal_attempts = as.character(sur_an1_spi_atmp),
    spinal_complications = as.character(sur_an1_spi_comp_y),
    spinal_complications = case_when(
      sur_an1_spi_comp_y == "1" ~ "Failed Attempt",
      sur_an1_spi_comp_y == "2" ~ "Inadequate Block",
      sur_an1_spi_comp_y == "3" ~ "Inadequate Block Duration",
      sur_an1_spi_comp_y == "4" ~ "Inadequate Surgical Conditions",
      str_detect(spinal_drug_other, "failed") ~ "Failed Attempt",
      str_detect(spinal_drug_other, "unsuccessful") ~ "Failed Attempt",
      str_detect(sur_an1_spi_comp_y_othr2, "failed") ~ "Failed Attempt",
      str_detect(sur_an1_spi_comp_y_othr2, "Dry tap") ~ "Failed Attempt",
      sur_an1_spi_comp_y_othr == "0" ~ "Bleeding"),
    spinal_ga_conv = as.character(sur_an1_spi_ga))

# Caudal
retro <- retro |> 
  mutate(
    caudal_drug = as.character(sur_an1_sed_drug),
    caudal_drug = case_when(
      caudal_drug == "1" ~ "Ropivacaine",
      caudal_drug == "2" ~ "Bupivacaine",
      caudal_drug == "3" ~ "Levobupivacaine",
      caudal_drug == "4" ~ "Chirocaine",
      caudal_drug == "0" ~ "Other"),
    caudal_drug_conc = case_when(
      sur_an1_sed_drug_conc == "0.5" ~ "0.5%",
      sur_an1_sed_drug_conc == "0.3" ~ "0.3%",
      sur_an1_sed_drug_conc == "0.25" ~ "0.25%",
      sur_an1_sed_drug_conc == "0.2" ~ "0.2%",
      sur_an1_sed_drug_conc == "0.1" ~ "0.1%"),
    caudal_attempts = as.character(sur_an1_sed_atmp),
    caudal_add = case_when(
      sur_an1_sed_adj_drg == 2 ~ "Clonidine",
      sur_an1_sed_adj_drg == 1 ~ "Dexmedetomidine",
      sur_an1_sed_adj_drg == 3 ~ "Adrenaline",
      str_detect(sur_an1_sed_adj_drg_othr, "saline") ~ "Saline"),
    caudal_complications = as.character(sur_an1_sed_comp),
    caudal_complications = case_when(
      caudal_complications == "1" ~ "Failed Attempt",
      caudal_complications == "2" ~ "Inadequate Block",
      caudal_complications == "3" ~ "Inadequate Block Duration",
      caudal_complications == "4" ~ "Inadequate Surgical Conditions",
      sur_an1_sed_comp_y_othr == "1" ~ "Bleeding",
      str_detect(sur_an1_sed_comp_y_othr2, "aborted") ~ "Failed Attempt"),
    caudal_ga_conv = as.character(sur_an1_sed_ga))

# Sedation
retro <- retro |> 
  mutate(
    sed_dexmede = as.character(sur_an1_sed___1),
    sed_prop = as.character(sur_an1_sed___2),
    sed_midaz = as.character(sur_an1_sed___3),
    sed_clonidine = as.character(sur_an1_sed___4),
    sed_remi = as.character(sur_an1_sed___5),
    sed_ket = as.character(sur_an1_sed___6),
    sed_no2 = as.character(sur_an1_sed___7),
    sed_sucrose = as.character(sur_an1_sed___8),
    sed_type = case_when(
      sed_dexmede == "1" & sed_prop == "0" ~ "Dexmedetomidine",
      sed_dexmede == "0" & sed_prop == "1" ~ "Propofol",
      sed_dexmede == "1" & sed_prop == "1" ~ "Dexmedetomidine + Propofol",
      .default = "None"),
    sed_type = factor(sed_type, levels = c("Dexmedetomidine", "Propofol", "Dexmedetomidine + Propofol", "None"))) 

# Secondary Anaesthetic
retro <- retro |> 
  mutate(
    sec_anaes = sur_an2_yn,
    reason_sec_anaes = case_when(
      sec_anaes == 1 & spinal_complications == "Failed Attempt" ~ "Failed Spinal Attempt",
      sec_anaes == 1 & spinal_complications == "Inadequate Block" ~ "Inadequate Spinal Block",
      sec_anaes == 1 & spinal_complications == "Inadequate Block Duration" ~ "Inadequate Spinal Duration",
      sec_anaes == 1 & spinal_complications == "Inadequate Surgical Conditions" ~ "Inadequate Surgical Conditions",
      sec_anaes == 1 & caudal_complications == "Failed Attempt" ~ "Failed Caudal Attempt",
      sec_anaes == 1 & spinal_complications == "Inadequate Block" ~ "Inadequate Caudal Block",
      sec_anaes == 1 & spinal_complications == "Inadequate Block Duration" ~ "Inadequate Caudal Duration",
      sec_anaes == 1 & spinal_complications == "Inadequate Surgical Conditions" ~ "Inadequate Surgical Conditions"),
    sec_ga = as.character(sur_an2_ga_yn),
    sec_spinal = as.character(sur_an2_spi_yn),
    sec_caudal = as.character(sur_an2_sed_caud_yn),
    sec_sed = as.character(sur_an2_sed_yn))

# Relocate Anaesthetic Variables
retro <- retro |> 
  relocate(c(anaes_type, ga_type, ga_adj, nmba:nmba_type, nmba_rev:rev_agent, local, local_type, local_drug, intraop_analgesia:intraop_parecoxib, airway:airway_type, spinal, spinal_drug:spinal_ga_conv, caudal, caudal_drug:caudal_ga_conv, sedation, sed_type, sec_anaes:sec_sed), .after = pacu_dur)

#==========================================INTRAOPERATIVE: ANAESTHETIC COMPLICATIONS==========================================#

# Rename Anaesthetic Complications (in wide format to account for multiple complications in same patient)
# Complication Mapping
complication_mapping <- list(
  "None" = 0,
  "Bradycardia" = 1,
  "Hypotension" = 2,
  "Apnoea" = 3,
  "Desaturation" = 4,
  "Bronchospasm" = 5,
  "Laryngospasm" = 6,
  "Other" = 99)

retro <- retro |>
  make_case_variables(
    events = "event",
    type = "type",
    time = "intraop",
    event_no = 1:3,
    mappings = list(complication_mapping),
    intervention = FALSE,
    suffix = ".x",
    var_prefix = "complication",
    comp_time = FALSE)

# Remove "None" from the second event time
retro <- retro |> 
  dplyr::mutate(complication_type_intraop_2 = case_when(
    complication_type_intraop_2 == "None" ~ NA,
    .default = complication_type_intraop_2))


# Complication Interventions
# Mappings
brady_mapping <- list(
  "None" = 1,
  "Stimulation" = 2,
  "↑FiO2" = 3,
  "Medication" = 4,
  "HFNO" = 5,
  "Intubation" = 6,
  "Other" = 0)

hypotension_mapping <- list(
  "None" = 1,
  "Vasopressor" = 2,
  "Fluid Bolus" = 3,
  "CPR" = 4,
  "Intubation" = 5,
  "Other" = 0)

apnoea_mapping <- list(
  "None" = 1,
  "Stimulation" = 2,
  "↑FiO2" = 3,
  "Positive Pressure Ventilation" = 4,
  "CPR" = 5,
  "Intubation" = 6,
  "HFNO" = 7,
  "Other" = 0)

desat_mapping <- list(
  "None" = 1,
  "Stimulation" = 2,
  "↑FiO2" = 3,
  "Positive Pressure Ventilation" = 4,
  "CPR" = 5,
  "Intubation" = 6,
  "HFNO" = 7,
  "Other" = 0)

bronch_mapping <- list(
  "None" = 1,
  "PEEP" = 2,
  "Bronchodilators" = 3,
  "NMBD" = 4,
  "Intubation" = 5,
  "Other" = 0)

laryngo_mapping <- list(
  "None" = 1,
  "PEEP" = 2,
  "Propofol" = 3,
  "NMBD" = 4,
  "Intubation" = 5,
  "Other" = 0)

# Run on multiple events at once
retro <- retro |>
  make_case_variables(
    events = c("brd", "hypo", "apn", "des", "bro", "lar"),
    type = "intraop",
    time = "intraop",
    event_no = 1:3,
    mappings = list(brady_mapping, hypotension_mapping, apnoea_mapping, desat_mapping, bronch_mapping, laryngo_mapping),
    intervention = TRUE,
    suffix = ".x",
    comp_time = FALSE)

# Rename Other Interventions
retro <- retro |> 
  mutate(
    hypo_int_intraop_1 = case_when(
      !is.na(stat_hypo_int_othr_1.x) & stringr::str_detect(stat_hypo_int_othr_1.x, "reduced") ~ "Reduced Anaesthesia",
      !is.na(stat_hypo_int_othr_1.x) & stringr::str_detect(stat_hypo_int_othr_1.x, "decreased") ~ "Reduced Anaesthesia",
      !is.na(stat_hypo_int_othr_1.x) & stringr::str_detect(stat_hypo_int_othr_1.x, "anaesthetic") ~ "Nil Documentation",
      TRUE ~ hypo_int_intraop_1),
    apn_int_intraop_1 = case_when(
      !is.na(stat_apn_int_othr_1.x) & stringr::str_detect(stat_apn_int_othr_1.x, "xtubat") ~ "Not Extubated",
      !is.na(stat_apn_int_othr_1.x) & stringr::str_detect(stat_apn_int_othr_1.x, "documented") ~ "Nil Documentation",
      TRUE ~ apn_int_intraop_1),
    des_int_intraop_1 = case_when(
      !is.na(stat_des_int_othr_1.x) & stringr::str_detect(stat_des_int_othr_1.x, "document") ~ "Nil Documentation",
      !is.na(stat_des_int_othr_1.x) & stringr::str_detect(stat_des_int_othr_1.x, "FiO2") ~ "↑FiO2",
      TRUE ~ des_int_intraop_1),
    brd_int_intraop_2 = case_when(
      !is.na(stat_brd_int_othr_2.x) & stringr::str_detect(stat_brd_int_othr_2.x, "clear") ~ "Nil Documentation",
      !is.na(stat_brd_int_othr_2.x) & stringr::str_detect(stat_brd_int_othr_2.x, "clear") ~ "Nil Documentation",
      TRUE ~ brd_int_intraop_2),
    des_int_intraop_2 = case_when(
      !is.na(stat_des_int_othr_2.x) & stringr::str_detect(stat_des_int_othr_2.x, "document") ~ "Nil Documentation",
      !is.na(stat_des_int_othr_2.x) & stringr::str_detect(stat_des_int_othr_2.x, "Fi02") ~ "↑FiO2",
      TRUE ~ des_int_intraop_2),
    des_int_intraop_3 = case_when(
      !is.na(stat_des_int_othr_3.x) & stringr::str_detect(stat_des_int_othr_3.x, "unclear") ~ "Nil Documentation",
      TRUE ~ des_int_intraop_3))

# Relocate Anaesthetic Variables
retro <- retro |> 
  relocate(c(complication_type_intraop_1, complication_type_intraop_2, complication_type_intraop_3, brd_int_intraop_1:lar_int_intraop_3), .after = sec_sed)

# Change intervention columns from NA to None for percentages of those that had the complication
# Apply intervention function to intraoperative columns using reduce
retro <- reduce(1:3, create_intervention_col_intraop, .init = retro)

#==========================================TABLE 4: POST-OPERATIVE==========================================#
# Post-Op Analgesia
retro <- retro |> 
  mutate(
    postop_para = as.factor(po_analg_drug___1),
    postop_nsaid = as.factor(po_analg_drug___2),
    postop_opioid = as.factor(po_analg_drug___3),
    postop_sucr = as.factor(po_analg_drug___0))


# Post-Op Disposition
retro <- retro |> 
  mutate(disposition = case_when(
    po_dest == 1 ~ "SCU",
    po_dest == 2 ~ "PICU",
    po_dest == 3 ~ "NICU",
    po_dest == 4 ~ "Ward",
    po_dest == 5 ~ "Home",
    po_dest == 6 ~ "Other Facility",
    po_dest_othr == "Home" ~ "Home",
    str_detect(po_dest_othr, "HDU") ~ "PICU",
    str_detect(po_dest_othr, "Wards") ~ "Ward High Obs",
    str_detect(po_dest_othr, "High Care") ~ "Ward High Obs",
    str_detect(po_dest_othr, "Protection") ~ "Home"),
    disposition = factor(disposition, levels = c("Home", "Ward", "Ward High Obs", "SCU", "NICU", "PICU", "Other Facility")))

# Create Variable Whether Post-Op Disposition is Different from Pre-Op
retro <- retro |> 
  mutate(disposition_numerical = case_when(
    disposition == "Home" ~ 0,
    disposition == "Ward" ~ 1,
    disposition == "Ward High Obs" ~ 2,
    disposition == "SCU" ~ 3,
    disposition == "NICU" ~ 4,
    disposition == "PICU" ~ 4,
    disposition == "Other Facility" ~ 4)) |> 
  mutate(disposition_prior_numerical = case_when(
    dem_loc == "Home/Residential Care" ~ 0,
    dem_loc == "Hospital Ward" ~ 1,
    dem_loc == "NICU" ~ 4,
    dem_loc == "PICU" ~ 4,
    dem_loc == "Special Care Nursery" ~ 3,
    dem_loc == "Other Health Care Facility" ~ 4)) |> 
  rowwise() |> 
  mutate(disposition_diff = disposition_numerical - disposition_prior_numerical) |> 
  mutate(disposition_change = case_when(
    disposition_diff < 0 ~ "Decreased",
    disposition_diff == 0 ~ "Same",
    disposition_diff > 0 ~ "Increased"))

# ICU Details
# Readmission
retro <- retro |> 
  mutate(icu_readm = case_when(
    po_dest_read == 1 ~ "Yes",
    po_dest_read == 0 ~ "No"))

# No NICU/PICU Readmissions #

# Unplanned ICU Admission
retro <- retro |> 
  mutate(unplanned_icu = case_when(
    po_icu_unplnd == 1 ~ "Yes",
    po_icu_unplnd == 0 ~ "No"),
    unplanned_icu_reason = case_when(
      po_icu_unplnd_y == 1 ~ "Respiratory",
      po_icu_unplnd_y == 2 ~ "Surgical",
      str_detect(po_icu_unplnd_y_othr, "sepsis") ~ "Metabolic/Sepsis",
      str_detect(po_icu_unplnd_y_othr, "metabolic") ~ "Metabolic/Sepsis",
      str_detect(po_icu_unplnd_y_othr, "time") ~ "Time of Day")) 

# Hospital LOS
retro <- retro |> 
  mutate(hosp_los = if_else(is.na(po_hos_adm_dura), po_hos_adm_dura_rch, po_hos_adm_dura)) |> 
  mutate(hosp_los_days = hosp_los/24)

# Hospital & Surgical Readmission
retro <- retro |> 
  mutate(
    hosp_readm = case_when(
      po_read == 1 ~ "Yes",
      po_read == 0 ~ "No"),
    surg_return = case_when(
      po_read_sur == 1 ~ "Yes",
      po_read_sur == 0 ~ "No"),
    hosp_readm_reason = case_when(
      str_detect(po_read_y, "pnoea") ~ "Respiratory",
      str_detect(po_read_y, "COVID") ~ "Respiratory",
      str_detect(po_read_y, "Bronchiolitis") ~ "Respiratory",
      str_detect(po_read_y, "Unsettled") ~ "Unsettled",
      str_detect(po_read_y, "ernia") ~ "Surgical",
      str_detect(po_read_y, "Irritable") ~ "Unsettled",
      str_detect(po_read_y, "Urinary") ~ "UTI",
      str_detect(po_read_y, "eeding") ~ "Feeding",
      str_detect(po_read_y, "respiratory") ~ "Respiratory",
      str_detect(po_read_y, "nasogastric") ~ "Feeding",
      str_detect(po_read_y, "vomiting") ~ "Feeding"),
    surg_return_reason = case_when(
      str_detect(po_read_sur_y, "adhesions") ~ "Adhesiolysis",
      str_detect(po_read_sur_y, "hernia") ~ "Re-Do Hernia Repair"))

# Rename Type of Respiratory Support
retro <- retro |> 
  mutate(
    postop_resp_supp = case_when(
      po_rsp == 1 ~ "Yes",
      po_rsp == 0 ~ "No"),
    resp_supp_type = case_when(
      po_rsp_y___4 == 1 ~ "IPPV",
      po_rsp_y___3 == 1 ~ "CPAP",
      po_rsp_y___2 == 1 ~ "HFNO",
      po_rsp_y___5 == 1 ~ "HM/Facemask O2",
      po_rsp_y___1 == 1 ~ "LFNO",
      postop_resp_supp == 0 ~ "None"),
    resp_supp_type = replace_na(resp_supp_type, "None"),
    resp_supp_type = factor(resp_supp_type, levels = c("IPPV", "CPAP", "HNFO", "LFNO", "HM/Facemask O2", "None")))

# Duration of Respiratory Support
retro <- retro |> 
  mutate(
    resp_supp_dur_mins = po_rsp_diff,
    resp_supp_dur_hrs = po_rsp_diff/60,
    resp_supp_dur_days = po_rsp_diff/1440,
    resp_supp_dur_mins = replace_na(resp_supp_dur_mins, 0),
    resp_supp_dur_hrs = replace_na(resp_supp_dur_hrs, 0),
    resp_supp_dur_days = replace_na(resp_supp_dur_days, 0)) 

# Rename Change in Respiratory Support
retro <- retro |> 
  mutate(resp_supp_change = case_when(
    po_rsp_inc == 0 ~ "Decreased",
    po_rsp_inc == 1 ~ "Increased",
    po_rsp_inc == 2 ~ "Same"))

# Respiratory Support-Free Days
retro <- retro |> 
  mutate(
    resp_supp_free_days = case_when(
      ae_type == 1 ~ 0,
      po_rsp_oxy== 1 ~ 0,
      resp_supp_type == "None" ~ hosp_los,
      postop_resp_supp == "Yes" ~ (hosp_los - resp_supp_dur_hrs)),
    resp_supp_free_days = resp_supp_free_days/24,
    resp_supp_free_days = round(resp_supp_free_days, 0))

# Post-Op Intubation or Retintubation
retro <- retro |> 
  mutate(
    postop_intub = case_when(
      po_intu == 1 ~ "Yes",
      po_intu == 0 ~ "No"),
    postop_intub_from_ot = case_when(
      po_intu_sur == 1 ~ "Yes",
      po_intu_sur == 0 ~ "No")) 

# Relocate Post-Op Variables
retro <- retro |> 
  relocate(c(postop_resp_supp, resp_supp_type, resp_supp_dur_mins, resp_supp_dur_hrs, resp_supp_dur_days, resp_supp_change, postop_intub, postop_intub_from_ot, postop_para:postop_sucr, disposition, disposition_change , icu_readm, unplanned_icu, unplanned_icu_reason, hosp_los, hosp_los_days, hosp_readm, hosp_readm_reason, surg_return, surg_return_reason), .after = lar_int_intraop_3)

#==========================================TABLE 5: POST-OPERATIVE COMPLICATIONS==========================================#
# Rename Post-Operative Complication Number
retro <- retro |> 
  make_case_variables(
    events = "event",
    type = "type",
    time = "postop",
    event_no = 1:5,
    mappings = list(complication_mapping),
    intervention = FALSE,
    suffix = ".y",
    var_prefix = "complication",
    comp_time = FALSE)

# Rename Post-Operative Complications
# Run on multiple events at once
retro <- retro |>
  make_case_variables(
    events = c("brd", "hypo", "apn", "des", "bro", "lar"),
    type = "postop",
    time = "postop",
    event_no = 1:5,
    mappings = list(brady_mapping, hypotension_mapping, apnoea_mapping, desat_mapping, bronch_mapping, laryngo_mapping),
    intervention = TRUE,
    suffix = ".y",
    comp_time = FALSE)

# Rename Other Complication Event
retro <- retro |> 
  mutate(
    complication_type_postop_1 = case_when(
      !is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "hypertension") ~ "Hypertension",
      !is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "achypnoea") ~ "Tachypnoea",
      !is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "achyponea") ~ "Tachypnoea",
      !is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "acchycardia") ~ "Tachycardia",
      !is.na(stat_event_othr_1.y) & stringr::str_detect(stat_event_othr_1.y, "Respiratory acidosis") ~ "Respiratory Distress",
      TRUE ~ complication_type_postop_1),
    complication_type_postop_2 = case_when(
      !is.na(stat_event_othr_2.y) & stringr::str_detect(stat_event_othr_2.y, "hypertension") ~ "Hypertension",
      TRUE ~ complication_type_postop_2),
    complication_type_postop_3 = case_when(
      !is.na(stat_event_othr_3.y) & stringr::str_detect(stat_event_othr_3.y, "hypothermic") ~ "Hypothermia",
      TRUE ~ complication_type_postop_3),
    complication_type_postop_4 = case_when(
      !is.na(stat_event_othr_4.y) & stringr::str_detect(stat_event_othr_4.y, "Tacchycardia") ~ "Tachycardia",
      TRUE ~ complication_type_postop_4),
    othr_int_postop_1 = case_when(
      !is.na(stat_othr_int_1.y) & stringr::str_detect(stat_othr_int_1.y, "nalgesia") ~ "Analgesia",
      !is.na(stat_othr_int_1.y) & stringr::str_detect(stat_othr_int_1.y, "il") ~ "Monitored/Nil Intervention",
      !is.na(stat_othr_int_1.y) & stringr::str_detect(stat_othr_int_1.y, "Monitored") ~ "Monitored/Nil Intervention",
      TRUE ~ stat_othr_int_1.y),
    othr_int_postop_2 = case_when(
      !is.na(stat_othr_int_2.y) & stringr::str_detect(stat_othr_int_2.y, "il") ~ "Monitored/Nil Intervention",
      TRUE ~ stat_othr_int_2.y),
    othr_int_postop_3 = case_when(
      !is.na(stat_othr_int_3.y) & stringr::str_detect(stat_othr_int_3.y, "warming") ~ "Convection Warming",
      TRUE ~ stat_othr_int_3.y),
    othr_int_postop_4 = case_when(
      !is.na(stat_othr_int_4.y) & stringr::str_detect(stat_othr_int_4.y, "nalgesia") ~ "Analgesia",
      TRUE ~ stat_othr_int_4.y),
    apn_int_postop_1 = case_when(
      !is.na(stat_apn_int_othr_1.y) & stringr::str_detect(stat_apn_int_othr_1.y, "Caffeine") ~ "Caffeine",
      TRUE ~ apn_int_postop_1),
    des_int_postop_1 = case_when(
      !is.na(stat_des_int_othr_1.y) & stringr::str_detect(stat_des_int_othr_1.y, "Observation") ~ "Monitored/Nil Intervention",
      !is.na(stat_des_int_othr_1.y) & stringr::str_detect(stat_des_int_othr_1.y, "baseline") ~ "↑FiO2",
      !is.na(stat_des_int_othr_1.y) & stringr::str_detect(stat_des_int_othr_1.y, "PBF") ~ "PBF",
      !is.na(stat_des_int_othr_1.y) & stringr::str_detect(stat_des_int_othr_1.y, "HFOV") ~ "High-Frequency Oscillatory Ventilation",
      TRUE ~ des_int_postop_1),
    des_int_postop_2 = case_when(
      !is.na(stat_des_int_othr_2.y) & stringr::str_detect(stat_des_int_othr_2.y, "iNO") ~ "Inhaled NO",
      !is.na(stat_des_int_othr_2.y) & stringr::str_detect(stat_des_int_othr_2.y, "PICU") ~ "Monitored in PICU",
      !is.na(stat_des_int_othr_2.y) & stringr::str_detect(stat_des_int_othr_2.y, "High frequency") ~ "High-Frequency Oscillatory Ventilation",
      !is.na(stat_des_int_othr_2.y) & stringr::str_detect(stat_des_int_othr_2.y, "Extubated") ~ "Extubation + Reintubation",
      TRUE ~ des_int_postop_2),
    des_int_postop_3 = case_when(
      !is.na(stat_des_int_othr_3.y) & stringr::str_detect(stat_des_int_othr_3.y, "choked") ~ "Stimulation + Backblow",
      TRUE ~ des_int_postop_3),
    des_int_postop_4 = case_when(
      !is.na(stat_des_int_othr_4.y) & stringr::str_detect(stat_des_int_othr_4.y, "High frequency") ~ "High-Frequency Oscillatory Ventilation",
      TRUE ~ des_int_postop_4))

# Rename Post-Op Complication Time
time_mapping <- list(
  "≤30m Post-Operative" = 0,
  "31m-12h Post-Operative" = 1,
  "12-48h Post-Operative" = 2)

retro <- retro |> 
  make_case_variables(
    events = c("brady", "hypo", "apn", "des", "bro", "lar", "othr"),
    type = "time",
    time = "postop",
    event_no = 1:5,
    mappings = list(time_mapping, time_mapping, time_mapping, time_mapping, time_mapping, time_mapping, time_mapping),
    intervention = F,
    suffix = ".y",
    comp_time = TRUE)

# Relocate Variables
retro <- retro |> 
  relocate(c(complication_type_postop_1:complication_type_postop_5, brd_int_postop_1:lar_int_postop_5, othr_int_postop_1:othr_int_postop_4, brady_time_postop_1:othr_time_postop_5), .after = surg_return_reason)

# Apply to all columns using reduce
retro <- reduce(1:5, create_intervention_col_postop, .init = retro)

#==========================================TABLE 6: POST-OPERATIVE ADVERSE EVENTS==========================================#
retro <- retro |> 
  mutate(
    adverse = case_when(
      ae_yn == 1 ~ "Yes",
      ae_yn == 0 ~ "No"),
    adverse_timepoint = case_when(
      ae_visit == 1 ~ "Pre-Operative",
      ae_visit == 2 ~ "Intra-Operative",
      ae_visit == 3 ~ "Post-Operative"),
    adverse_type = case_when(
      ae_type == 1 ~ "Death",
      str_detect(ae_type_othr, "intubation") ~ "Difficult Intubation",
      str_detect(ae_type_othr, "infusion") ~ "Transfusion",
      str_detect(ae_type_othr, "infection") ~ "Wound Infection",
      str_detect(ae_type_othr, "Multiple") ~ "Feeding Issues",
      str_detect(ae_type_othr, "Febrile") ~ "Viral Respiratory Infection")) |> 
  relocate(c(adverse, adverse_timepoint, adverse_type), .after = othr_time_postop_5)


# Save dataframe
save(retro, file = "retro_wrangled.RData")
