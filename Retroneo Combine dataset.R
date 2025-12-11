library(tidyverse)

#==========================================DEMOGRAPHICS==========================================#
# Import data
retro_dem <- readr::read_csv("~/Documents/Research/Projects/Current Projects/RetroNeo/Data/RetroNeo_Pre_Op.csv") |> 
  dplyr::select(record_id:mhx_caff_y)

# Remove unneccessary columns
retro_dem <- retro_dem |> 
  dplyr::select(-c(redcap_event_name:redcap_repeat_instance))

# Remove Duplicates
retro_dem <- retro_dem |> 
  filter(!str_detect(record_id, "dup"))

# Change missing values from 9999 to NA
retro_dem <- retro_dem |> 
  mutate(dem_wgt = na_if(dem_wgt,9999.00))

# Create Post-Menstrual Age Variable in Weeks
retro_dem <- retro_dem |> 
  mutate(age_pma_wks = age_pma_days/7)

# Create Post-Menstrual Age at Surgery Variable in Weeks
retro_dem <- retro_dem |> 
  mutate(age_pma_surg_wks = age_pma_surg_days/7)

# Create Grouping Variable Based on Age
retro_dem <- retro_dem |> 
  mutate(age_cat = case_when(
    age_pma_wks <= 27 ~ "≤ 27 Weeks",
    age_pma_wks >27 & age_pma_wks <33 ~ "28-32 Weeks",
    age_pma_wks >=33 & age_pma_wks <37 ~ "33-36 Weeks",
    age_pma_wks >=37 ~ "≥37 Weeks")) |> 
  mutate(age_cat = factor(age_cat, 
                          levels = c("≤ 27 Weeks", "28-32 Weeks", "33-36 Weeks", "≥37 Weeks")))


#==========================================SURGERY==========================================#
# Import Surgical Data
retro_surg <- readr::read_csv("~/Documents/Research/Projects/Current Projects/RetroNeo/Data/RetroNeo_Operative.csv") |> 
  select(record_id:surgery_complete) 

# Select Operative Data Only & remove columns
retro_surg <- retro_surg|> 
  filter(is.na(redcap_repeat_instrument)) |> 
  select(-c(redcap_event_name:medical_history_complete)) |> 
  select(c(record_id:sure_asspro_y, sur_time_ana_star:sur_time_oproom_tot))


#==========================================ANAESTHETICS==========================================#
# Import Anaesthetic Data & Filter
retro_anaes_tot <- readr::read_csv("~/Documents/Research/Projects/Current Projects/RetroNeo/Data/RetroNeo_Operative.csv")

# Filter Operative Arm (Without Complications) Only
retro_anaes <- retro_anaes_tot |> 
  select(record_id, redcap_repeat_instrument, sur_an1_ga_yn:sur_an2_sed_ga) |> 
  filter(is.na(redcap_repeat_instrument)) |> 
  select(-c(redcap_repeat_instrument))

#==========================================ANAESTHETIC COMPLICATIONS==========================================#
retro_anaes_compl <- retro_anaes_tot |> 
  filter(redcap_repeat_instrument == "complications") |> 
  #select(-c(redcap_repeat_instrument)) |> 
  select(record_id, redcap_repeat_instance, stat_event:complications_complete) |> 
  pivot_wider(
    id_cols = record_id,
    names_from = redcap_repeat_instance,
    values_from = -c(record_id, redcap_repeat_instance),
    names_glue = "{.value}_{redcap_repeat_instance}")

#==========================================POST-OPERATIVE==========================================#
# Import Post-Operative Data & Filter
retro_postop <- readr::read_csv("~/Documents/Research/Projects/Current Projects/RetroNeo/Data/RetroNeo_Post_Op.csv") |> 
  select(record_id, redcap_repeat_instrument, po_rsp:postoperative_complete) 

# Filter Operative Arm (Without Complications) Only
retro_postop <- retro_postop |> 
  filter(is.na(redcap_repeat_instrument)) |> 
  select(-c(redcap_repeat_instrument))

#==========================================POST-OPERATIVE COMPLICATIONS==========================================#
# Import Post-Operative Data & Filter
retro_postop_compl <- readr::read_csv("~/Documents/Research/Projects/Current Projects/RetroNeo/Data/RetroNeo_Post_Op.csv") |> 
  select(record_id, redcap_repeat_instrument, redcap_repeat_instance, stat_event:complications_complete) 

# Filter Operative Arm (Without Complications) Only
retro_postop_compl <- retro_postop_compl |> 
  filter(redcap_repeat_instrument == "complications") |> 
  select(-c(redcap_repeat_instrument)) |> 
  pivot_wider(
    id_cols = record_id,
    names_from = redcap_repeat_instance,
    values_from = -c(record_id, redcap_repeat_instance),
    names_glue = "{.value}_{redcap_repeat_instance}")

#==========================================POST-OPERATIVE ADVERSE EVENTS==========================================#
# Import Adverse Events Data & Filter
retro_adverse <- readr::read_csv("~/Documents/Research/Projects/Current Projects/RetroNeo/Data/RetroNeo_Adverse.csv") |> 
  select(record_id, ae_yn:adverse_events_complete) 

# Find records that do not have complete data across dataframes
#retro_mismatch <- anti_join(retro_dem, retro_surg, by = "record_id")
#retro_mismatch <- anti_join(retro_dem, retro_anaes, by = "record_id") 
#retro_mismatch <- anti_join(retro_dem, retro_anaes_compl, by = "record_id") 
#retro_mismatch <- anti_join(retro_dem, retro_postop, by = "record_id") 
#retro_mismatch <- anti_join(retro_dem, retro_postop_compl, by = "record_id") 
#retro_mismatch <- anti_join(retro_dem, retro_adverse, by = "record_id")


# Merge Data
retro <- inner_join(retro_dem, retro_surg, by = "record_id", relationship = "many-to-many")
retro <- inner_join(retro, retro_anaes, by = "record_id", relationship = "many-to-many") 
retro <- inner_join(retro, retro_anaes_compl, by = "record_id", relationship = "many-to-many") 
retro <- inner_join(retro, retro_postop, by = "record_id", relationship = "many-to-many") 
retro <- inner_join(retro, retro_postop_compl, by = "record_id", relationship = "many-to-many") |>  
  dplyr::rename_with(
    .fn = ~ paste0(.x, ".y"),
    .cols = dplyr::matches("^stat_") & !dplyr::matches("\\.(x|y)$"))
retro <- inner_join(retro, retro_adverse, by = "record_id", relationship = "many-to-many")

# Save dataframe
save(retro, file = "retro.RData")