###############################################################
################# Create CAPTND DT for Dashboard ##############
###############################################################

#Author: Luke Taylor
#Date: 05-05-2025

#Script to create datatables for Shiny dashboard

month_end <- "2025-07-01"

# source("./07_publication/script/chapters/2_load_functions.R")
# source("./07_publication/script/chapters/3_set_constants.R")

##### Referral demographics #####
ref_sex_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_sex.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by sex',
         waiting_time = 'Total referrals') |>
  rename(measure_breakdown = sex_reported) |>
  #append_quarter_ending(date_col = 'referral_month') |>
  pivot_longer(cols = c('count', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_age_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_age.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by age',
         waiting_time = 'Total referrals') |>
  rename(measure_breakdown = agg_age_groups) |>
  #append_quarter_ending(date_col = 'referral_month') |>
  pivot_longer(cols = c('count', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_simd_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_simd.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by SIMD',
         waiting_time = 'Total referrals') |>
  rename(measure_breakdown = simd2020_quintile) |>
  #append_quarter_ending(date_col = 'referral_month') |>
  pivot_longer(cols = c('count', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_master_df <- rbind(ref_sex_df, ref_age_df, ref_simd_df) |> ungroup() |>
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |>
  change_nhsscotland_label() |>
  arrange(hb_name) |>
  filter(measure_breakdown != 'Not specified' & measure_breakdown != 'Not known' & measure_breakdown != 'Data missing',
         hb_name != 'NHS 24') |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of referrals',
                                   measure_type == 'prop' ~ 'Proportion of referrals (%)',
                                   measure_type == 'pop_rate_1000' ~ 'Rate per 1,000 pop',
                                   TRUE ~ measure_type),
         measure_breakdown = fct_relevel(measure_breakdown, "Male", "Female", "Under 6", "6-11", 
                                         "12-15", "Over 15", "Under 25", "25-39", 
                                         "40-64", "65 plus", "1 - Most Deprived", "2", 
                                         "3", "4", "5 - Least Deprived"))

demo_palette <- c("Female" = "#9B4393","Male" =  "#3F3685", "Under 6" = "#3F3685", "6-11" = "#9B4393", 
                 "12-15" = "#0078D4", "Over 15" = "#83BB26", "Under 25" = "#3F3685", "25-39" = "#9B4393", 
                 "40-64" = "#0078D4", "65 plus" = "#83BB26", "1 - Most Deprived" = "#3F3685", "2" = "#9B4393", 
                 "3" = "#0078D4", "4" = "#83BB26", "5 - Least Deprived" = "#1E7F84")

quarters_in_data <- sort(unique(ref_master_df$quarter_ending))

##### Referral acceptance #####
ref_accept_df <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance/non_acceptance_summary_quarter_hb.parquet")) |>
  mutate(measure_name = 'Referrals acceptance status') |>
  rename(measure_breakdown = ref_acc_desc) |>
  pivot_longer(cols = c('count', 'prop'),
               names_to = 'measure_type',
               values_to = 'count')

ref_accept_df <- ref_accept_df |> ungroup() |>
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |>
  change_nhsscotland_label() |>
  arrange(hb_name) |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of referrals',
                                  measure_type == 'prop' ~ 'Proportion of referrals (%)',
                                  TRUE ~ measure_type),
         measure_breakdown = fct_relevel(measure_breakdown, "Referral accepted", "Referral not accepted",
                                          "Pending", "No information"))

ref_acc_palette <- c("Referral accepted" = "#3F3685", "Referral not accepted" = "#9B4393", 
                  "Pending" = "#0078D4", "No information" = "#83BB26")


##### Appointment attendance #####
appt_att_df <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |>
  mutate(measure_name = 'Appointment attendance status') |>
  rename(measure_breakdown = Attendance,
         quarter_ending = app_quarter_ending,
         Count = apps_att,
         Total = total_apps,
         Percentage = prop_apps_att) |>
  pivot_longer(cols = c('Count', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

appt_att_df <- appt_att_df |> ungroup() |>
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |>
  change_nhsscotland_label() |>
  arrange(hb_name) |>
  mutate(measure_type = case_when(measure_type == 'Count' ~ 'Number of appointments',
                                  measure_type == 'Percentage' ~ 'Proportion of appointments (%)',
                                  TRUE ~ measure_type),
         measure_breakdown = fct_relevel(measure_breakdown, "Attended", "Clinic cancelled", "Patient DNA", 
                                         "Patient cancelled", "Not recorded", "Patient CNW", "Not known" ),
         measure_name = 'Total appointment attendances')

appt_att_palette <- c("Attended" = "#3F3685", "Clinic cancelled" = "#9B4393", 
                      "Patient DNA" = "#0078D4", "Patient cancelled" = "#83BB26", "Not recorded" = "#6B5C85",
                      "Patient CNW" = "#C73918", "Not known" = "#948DA3")

##### First contact attendance #####
first_con_df <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb.parquet")) |>
  select(-total_apps) |>
  mutate(measure_name = 'First contact attendance status') |>
  rename(measure_breakdown = Attendance,
         quarter_ending = app_quarter_ending,
         Count = firstcon_att,
         Total = first_contact,
         Percentage = prop_firstcon_att) |>
  pivot_longer(cols = c('Count', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

first_con_df <- first_con_df |> ungroup() |>
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |>
  change_nhsscotland_label() |>
  arrange(hb_name) |>
  mutate(measure_type = case_when(measure_type == 'Count' ~ 'Number of appointments',
                                  measure_type == 'Percentage' ~ 'Proportion of appointments (%)',
                                  TRUE ~ measure_type),
         measure_breakdown = fct_relevel(measure_breakdown, "Attended", "Clinic cancelled", "Patient DNA", 
                                         "Patient cancelled", "Not recorded", "Patient CNW", "Not known"),
         measure_name = 'First contact attendances')

master_appts_df <- rbind(appt_att_df, first_con_df)

##### Referral source #####
df_ref_source <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/ref_source_month_hb.parquet")) |>
  mutate(measure_name = 'Referral source') |>
  rename(measure_breakdown = ref_source_name,
         Count = count,
         Total = total,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### Appointment care locations #####
df_care_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_mth_hb.parquet")) |>
  mutate(measure_name = 'Appointment care location') |>
  rename(measure_breakdown = loc_label,
         Count = count,
         Total = total_apps,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### Care professionals #####
df_prof_group <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_mth_hb.parquet")) |>
  mutate(measure_name = 'Care professional') |>
  rename(measure_breakdown = prof_label,
         Count = count,
         Total = total_apps,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### First contact #####

##### Waiting list #####

##### Patients seen #####

##### Open cases #####
