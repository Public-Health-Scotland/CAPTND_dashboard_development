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


##### Referral non-acceptance/non-acceptance reason #####
df_non_acc_reason <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance_reason/non_acceptance_reason_quarter_hb.parquet")) |> 
  select(-total, -prop) |>
  right_join(df_qt_ds_hb, by = c("dataset_type", "hb_name", "quarter_ending")) |>
  mutate(hb_name := factor(hb_name, levels = hb_vector),
         ref_rej_reason_desc = case_when(is.na(ref_rej_reason_desc) ~ 'No data',
                                         TRUE ~ ref_rej_reason_desc),
         count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |> 
  group_by(quarter_ending, dataset_type, hb_name) |>
  arrange(desc(count), .by_group = TRUE) |>
  mutate(rank = row_number(),
         top5 = case_when(rank >5 ~ "All other non acceptance reasons",
                          TRUE ~ ref_rej_reason_desc)) |>
  ungroup() |>
  group_by(quarter_ending, dataset_type, hb_name, top5) |> 
  mutate(count = sum(count)) |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) %>%
  
  bind_rows(summarise(.,
                      across(count, sum),
                      across(top5, ~"Total"),
                      across(rank, ~ 7),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  select(quarter_ending, dataset_type, hb_name, measure_breakdown = top5, 
         count, rank, total, prop) |>
  mutate(measure_breakdown = case_when(is.na(measure_breakdown) ~ 'Missing data',
                                TRUE ~ measure_breakdown))

df_non_acc_reason <- df_non_acc_reason |>
  pivot_longer(cols = c('count', 'prop'),
               names_to = 'measure_type',
               values_to = 'count') |>
  change_nhsscotland_label() |>
  arrange(hb_name) |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of rejected referrals',
                                  measure_type == 'prop' ~ 'Proportion of rejected referrals (%)',
                                  TRUE ~ measure_type))

#non acceptance action
df_non_acc_actions <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance_action/non_acceptance_action_quarter_hb.parquet")) |> 
  select(-total, -prop) |>
  right_join(df_qt_ds_hb, by = c("dataset_type", "hb_name", "quarter_ending")) |>
  mutate(hb_name := factor(hb_name, levels = hb_vector),
         ref_rej_act_desc = case_when(is.na(ref_rej_act_desc) ~ 'No data',
                                      TRUE ~ ref_rej_act_desc),
         count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |> 
  group_by(quarter_ending, dataset_type, hb_name) |>
  arrange(desc(count), .by_group = TRUE) |>
  mutate(rank = row_number(),
         top5 = case_when(rank >5 ~ "All other non acceptance actions",
                          TRUE ~ ref_rej_act_desc)) |>
  ungroup() |>
  group_by(quarter_ending, dataset_type, hb_name, top5) |> 
  mutate(count = sum(count)) |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) %>%
  
  bind_rows(summarise(.,
                      across(count, sum),
                      across(top5, ~"Total"),
                      across(rank, ~ 7),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  select(quarter_ending, dataset_type, hb_name, rej_action = top5, 
         count, rank, total, prop) |>
  mutate(rej_action = case_when(is.na(rej_action) ~ 'Missing data',
                                TRUE ~ rej_action))

df_non_acc_actions <- df_non_acc_actions |>
  pivot_longer(cols = c('count', 'prop'),
               names_to = 'measure_type',
               values_to = 'count') |>
  change_nhsscotland_label() |>
  arrange(hb_name) |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of rejected referrals',
                                  measure_type == 'prop' ~ 'Proportion of rejected referrals (%)',
                                  TRUE ~ measure_type))


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
                      "Patient CNW" = "#C73918", "Not known" = "#1E7F84")

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
df_ref_source <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/ref_source_quarter_hb.parquet")) |>
  select(-total, -prop) |>
  right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |>
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  arrange(desc(count), .by_group = TRUE) |>
  mutate(rank = row_number(),
         top5 = case_when(rank >5 ~ "All other referral sources",
                          TRUE ~ ref_source_name)) |>
  ungroup() |>
  group_by(quarter_ending, dataset_type, hb_name, top5) |>
  mutate(count = sum(count)) |>
  group_by(quarter_ending, dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) %>%

  bind_rows(summarise(.,
                      across(count, sum),
                      across(top5, ~"Total"),
                      across(rank, ~ 7),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  select(quarter_ending, dataset_type, hb_name, ref_source_name = top5,
         count, rank, total, prop) |>
  change_nhsscotland_label()


df_ref_source <- df_ref_source |>
  mutate(measure_name = 'Referral source') |>
  rename(measure_breakdown = ref_source_name) |>
  pivot_longer(cols = c('count','prop'),
               names_to = 'measure_type',
               values_to = 'count') |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of referrals',
                                  measure_type == 'prop' ~ 'Proportion of referrals (%)',
                                  TRUE ~ measure_type))

##### Appointment care locations #####
df_care_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_qt_hb.parquet")) |> 
  select(-total_apps, -prop) |>
  right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
  group_by(app_quarter_ending, dataset_type, hb_name) |>
  arrange(desc(count), .by_group = TRUE) |>
  mutate(rank = row_number(),
         top5 = case_when(rank >5 ~ "All other care locations",
                          TRUE ~ loc_label)) |>
  ungroup() |>
  group_by(app_quarter_ending, dataset_type, hb_name, top5) |> 
  mutate(count = sum(count)) |>
  group_by(app_quarter_ending, dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6,
         hb_name != 'NHS 24') |>
  add_proportion_ds_hb(vec_group = c("app_quarter_ending", "dataset_type", "hb_name")) %>%
  select(app_quarter_ending, dataset_type, hb_name, measure_breakdown = top5, 
         count, rank, total, prop) |>
  change_nhsscotland_label() |>
  mutate(measure_breakdown = case_when(is.na(measure_breakdown) ~ 'Missing data',
                               TRUE ~ measure_breakdown))

df_care_loc <- df_care_loc |>
  mutate(measure_name = 'Appointment care location') |>
  pivot_longer(cols = c('count', 'prop'),
               names_to = 'measure_type',
               values_to = 'count') |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of appointments',
                                  measure_type == 'prop' ~ 'Proportion of appointments (%)',
                                  TRUE ~ measure_type))

##### Care professionals #####
df_prof_group <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_qt_hb.parquet")) |> 
  select(-total_apps, -prop) |>
  right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
  group_by(app_quarter_ending, dataset_type, hb_name) |>
  arrange(desc(count), .by_group = TRUE) |>
  mutate(rank = row_number(),
         top5 = case_when(rank >5 ~ "All other professional groups",
                          TRUE ~ prof_label)) |>
  ungroup() |>
  group_by(app_quarter_ending, dataset_type, hb_name, top5) |> 
  mutate(count = sum(count)) |>
  group_by(app_quarter_ending, dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6,
         hb_name != 'NHS 24') |>
  add_proportion_ds_hb(vec_group = c("app_quarter_ending", "dataset_type", "hb_name")) %>%
  select(app_quarter_ending, dataset_type, hb_name, measure_breakdown = top5, 
         count, rank, total, prop) |>
  change_nhsscotland_label() |>
  mutate(measure_breakdown = case_when(is.na(measure_breakdown) ~ 'Missing data',
                                TRUE ~ measure_breakdown))


df_prof_group <- df_prof_group |>
  mutate(measure_name = 'Care professional') |>
  pivot_longer(cols = c('count', 'prop'),
               names_to = 'measure_type',
               values_to = 'count') |>
  mutate(measure_type = case_when(measure_type == 'count' ~ 'Number of appointments',
                                  measure_type == 'prop' ~ 'Proportion of appointments (%)',
                                  TRUE ~ measure_type))

master_loc_prof_df <- rbind(df_prof_group, df_care_loc)

##### Total appt DNAs #####
tot_dnas_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |>
  filter(Attendance == 'Patient DNA') |>
  select(dataset_type, hb_name, app_quarter_ending, dna_count = apps_att, total_apps, 
         dna_rate = prop_apps_att) |>
  right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(hb_name := factor(hb_name, levels = hb_vector)) |> 
  group_by(app_quarter_ending, dataset_type, hb_name) |>
  ungroup() |>
  arrange(dataset_type, hb_name) |>
  change_nhsscotland_label()

tot_dnas_latest <- tot_dnas_latest |>
  mutate(measure_name = 'Total appt DNAs') |>
  pivot_longer(cols = c('dna_count', 'dna_rate'),
               names_to = 'measure_type',
               values_to = 'count') |>
  mutate(measure_type = case_when(measure_type == 'dna_count' ~ 'Number of DNAs',
                                  measure_type == 'dna_rate' ~ 'DNA rate (%)',
                                  TRUE ~ measure_type))


