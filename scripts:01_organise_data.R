# Load libraries
library(readxl)
library(dplyr)
library(lubridate)
library(purrr)
library(janitor)
library(stringr)
library(ggplot2)

# Set broad working directory
setwd("~/Downloads/FYP/DATA")

# Define where your Excel data files are stored and list all .xls files
folder_path <- "~/Downloads/FYP/DATA"
file_list <- list.files(path = folder_path, pattern = "\\.xls$", full.names = TRUE)

# Define a list where each target column name (e.g., AE_att_T1) is mapped to one or more possible original names from the Excel files. This standardizes variable names across all files.
column_mapping <- list(
  code = c("code", "x"),
  region = "region",
  name = "name",
  
  AE_att_T1 = "a_e_attendances_type_1_departments_major_a_e",
  AE_att_T2 = "a_e_attendances_type_2_departments_single_specialty",
  AE_att_T3 = "a_e_attendances_type_3_departments_other_a_e_minor_injury_unit",
  AE_att_tot = "a_e_attendances_total_attendances",
  
  AE_more4_T1 = c(
    "a_e_attendances_4_hours_from_arrival_to_admission_transfer_or_discharge_type_1_departments_major_a_e",
    "a_e_attendances_greater_than_4_hours_from_arrival_to_admission_transfer_or_discharge_type_1_departments_major_a_e"
  ),
  AE_more4_T2 = c(
    "a_e_attendances_4_hours_from_arrival_to_admission_transfer_or_discharge_type_2_departments_single_specialty",
    "a_e_attendances_greater_than_4_hours_from_arrival_to_admission_transfer_or_discharge_type_2_departments_single_specialty"
  ),
  AE_more4_T3 = c(
    "a_e_attendances_4_hours_from_arrival_to_admission_transfer_or_discharge_type_3_departments_other_a_e_minor_injury_unit",
    "a_e_attendances_greater_than_4_hours_from_arrival_to_admission_transfer_or_discharge_type_3_departments_other_a_e_minor_injury_unit"
  ),
  AE_more4_tot = c(
    "a_e_attendances_4_hours_from_arrival_to_admission_transfer_or_discharge_total_attendances_4_hours",
    "a_e_attendances_greater_than_4_hours_from_arrival_to_admission_transfer_or_discharge_total_attendances_4_hours"
  ),
  
  AE_per4_T1 = c(
    "a_e_attendances_4_hours_from_arrival_to_admission_transfer_or_discharge_percentage_in_4_hours_or_less_type_1",
    "percentage_of_attendances_within_4_hours_percentage_in_4_hours_or_less_type_1"
  ),
  AE_per4_all = c(
    "a_e_attendances_4_hours_from_arrival_to_admission_transfer_or_discharge_percentage_in_4_hours_or_less_all",
    "percentage_of_attendances_within_4_hours_percentage_in_4_hours_or_less_all"
  ),
  
  Em_adm_AE_T1 = "emergency_admissions_emergency_admissions_via_type_1_a_e",
  Em_adm_AE_T2 = "emergency_admissions_emergency_admissions_via_type_2_a_e",
  Em_adm_AE_T34 = "emergency_admissions_emergency_admissions_via_type_3_and_4_a_e",
  Em_adm_AE_tot = "emergency_admissions_total_emergency_admissions_via_a_e",
  Em_adm_other = "emergency_admissions_other_emergency_admissions_i_e_not_via_a_e",
  Em_adm_tot = "emergency_admissions_total_emergency_admissions",
  Em_pat_adm_more4 = "emergency_admissions_number_of_patients_spending_4_hours_from_decision_to_admit_to_admission",
  Em_pat_adm_more12 = "emergency_admissions_number_of_patients_spending_12_hours_from_decision_to_admit_to_admission",
  
  time_period = "time_period",
  file_name = "file_name"
)

# This function standardizes column names using the alias mapping, cleaning them and renaming to the expected format used in analysis.
normalize_columns <- function(df) {
  all_aliases <- unlist(column_mapping)
  alias_lookup <- setNames(
    rep(names(column_mapping), lengths(column_mapping)),
    make_clean_names(all_aliases)
  )
  
  # Clean and match column names
  current_names <- names(df)
  cleaned_names <- make_clean_names(current_names)
  renamed <- setNames(cleaned_names, cleaned_names)
  
  # Apply mapping
  for (col in names(renamed)) {
    if (col %in% names(alias_lookup)) {
      renamed[col] <- alias_lookup[[col]]
    } else {
      renamed[col] <- NA  # Mark for removal
    }
  }
  
  renamed <- renamed[!is.na(renamed)]
  df <- df %>% select(all_of(names(renamed))) %>% rename_with(~ renamed[.x])
  return(df)
}

# Read each Excel file, extract headers (main + sub), clean them, assign as column names, and attach time_period and file metadata
read_my_excel <- function(file) {
  message("📄 Reading: ", basename(file))
  
  # Select the correct sheet
  sheet_names <- excel_sheets(file)
  target_sheet <- if ("Provider Level Data" %in% sheet_names) {
    "Provider Level Data"
  } else if (length(sheet_names) == 1) {
    sheet_names[1]
  } else {
    stop("No 'Provider Level Data' found in ", file)
  }
  
  # Get time period from C6
  time_period <- tryCatch({
    val <- read_excel(file, sheet = target_sheet, range = "C6", col_names = FALSE)[[1]]
    if (is.null(val) || is.na(val) || trimws(val) == "") "Unknown" else trimws(as.character(val))
  }, error = function(e) "Unknown")
  
  # Read from B15 and B16 to skip column A (which is empty)
  main_headers <- read_excel(file, sheet = target_sheet, range = "B15:ZZ15", col_names = FALSE) %>%
    unlist(use.names = FALSE) %>% as.character()
  
  sub_headers <- read_excel(file, sheet = target_sheet, range = "B16:ZZ16", col_names = FALSE) %>%
    unlist(use.names = FALSE) %>% as.character()
  
  # 🧩 Combine and propagate main + subheaders correctly
  # Assumes: main_headers and sub_headers have been read already (as character vectors)
  
  # 1. Standardize lengths
  max_length <- max(length(main_headers), length(sub_headers))
  main_headers <- c(main_headers, rep("", max_length - length(main_headers)))
  sub_headers  <- c(sub_headers,  rep("", max_length - length(sub_headers)))
  
  # 2. Fix main headers only from column 4 onward (avoid overwriting "Code", "Region", "Name")
  last_main <- ""
  for (i in 4:max_length) {
    if (!is.na(main_headers[i]) && trimws(main_headers[i]) != "") {
      last_main <- trimws(main_headers[i])
    } else {
      main_headers[i] <- last_main
    }
  }
  
  # 3. Combine intelligently: 
  # - Columns 1–3 use subheaders alone
  # - From column 4 onward: if both main and sub are present, join with " - "
  #                        if only one exists, use it
  #                        if both are missing, fallback to "unnamed_{i}"
  final_headers <- character(max_length)
  for (i in 1:max_length) {
    main <- trimws(ifelse(is.na(main_headers[i]), "", main_headers[i]))
    sub  <- trimws(ifelse(is.na(sub_headers[i]), "", sub_headers[i]))
    
    if (nzchar(main) && nzchar(sub)) {
      final_headers[i] <- paste(main, sub, sep = " - ")
    } else if (nzchar(main)) {
      final_headers[i] <- main
    } else if (nzchar(sub)) {
      final_headers[i] <- sub
    } else {
      final_headers[i] <- paste0("unnamed_", i)
    }
  }
  
  # Clean for R (e.g., underscores, unique names)
  final_headers <- make_clean_names(final_headers, unique_sep = "_")
  
  # Read the actual data
  df <- read_excel(file, sheet = target_sheet, skip = 16, col_names = FALSE)
  
  # Match header count
  expected_cols <- length(final_headers)
  actual_cols <- ncol(df)
  
  if (actual_cols < expected_cols) {
    message("⚠ WARNING: Data has fewer columns than headers. Trimming headers.")
    final_headers <- final_headers[1:actual_cols]
  } else if (actual_cols > expected_cols) {
    message("⚠ WARNING: Data has more columns than headers. Padding headers.")
    final_headers <- c(final_headers, paste0("Extra_Column_", seq_len(actual_cols - expected_cols)))
  }
  
  # Assign to dataframe
  colnames(df) <- final_headers
  
  df <- df %>%
    filter(if_any(everything(), ~ !is.na(.) & . != ""))
  
  df <- df %>%
    mutate(across(everything(), as.character))
  
  df <- normalize_columns(df)
  
  # Add this block right after `normalize_columns(df)` inside your `read_my_excel()` function,
  # or just after the final call to `normalize_columns(df)` before returning the `df`.
  
  # Standardized NHS Region Mapping
  region_mapping <- c(
    # Midlands
    "Midlands And East Of England Commissioning Region" = "NHS England Midlands",
    "NHS England Midlands And East" = "NHS England Midlands",
    
    # East of England
    "East Of England Commissioning Region" = "NHS England East Of England",
    
    # North East and Yorkshire
    "North Of England Commissioning Region" = "NHS England North East And Yorkshire",
    "NHS England North" = "NHS England North East And Yorkshire",
    
    # North West
    "North West Commissioning Region" = "NHS England North West",
    
    # London
    "London Commissioning Region" = "NHS England London",
    
    # South East
    "South East Commissioning Region" = "NHS England South East",
    
    # South West
    "South Of England Commissioning Region" = "NHS England South West"
  )
  
  # Standardize region names in-place
  if ("region" %in% names(df)) {
    df <- df %>%
      mutate(
        region = str_trim(region),
        region = recode(region, !!!region_mapping),
        region = ifelse(
          str_detect(region, "Organisation|org"), NA_character_, region
        )
      )
  }
  
  # 🔁 Apply the column mapping
  standard_names <- names(column_mapping)
  column_lookup <- setNames(rep(standard_names, lengths(column_mapping)), unlist(column_mapping))
  
  # 🔁 Rename known columns to standard names using lookup
  matched_cols <- intersect(names(df), names(column_lookup))
  df <- df %>%
    rename_with(~ column_lookup[.x], .cols = all_of(matched_cols))
  
  # 🧹 Drop any columns not in the final desired schema
  df <- df %>% select(any_of(names(column_mapping)))
  
  # Drop any columns not in the standard list
  df <- df %>% select(any_of(standard_names))
  
  df <- df %>%
    mutate(
      time_period = time_period,
      file_name = basename(file)
    )
  
  return(df)
  
}

combined_data <- map(file_list, read_my_excel) %>% bind_rows()

# Identify the position of the 'time_period' column
file_name_index <- which(names(combined_data) == "file_name")

# Keep all columns from the first up to the 'file_name' column
combined_data <- combined_data[, 1:file_name_index]

# Remove rows where "Code" is exactly "-"
combined_data <- combined_data %>%
  filter(code != "-")

# Order by date
ordered_data <- combined_data %>%
  mutate(date = my(time_period)) %>%  # Convert "June 2022" into a Date (e.g., 2022-06-01)
  arrange(desc(date), region) %>%       # Order by date (most recent first) then by Region
  relocate(date, .before = code)  # Move the date column before Time_Period

# Standardize region names to the 7 modern NHS England regions
ordered_data <- ordered_data %>%
  mutate(
    region = case_when(
      region %in% c("NHS England Midlands", "NHS England Midlands And East", "Midlands And East Of England Commissioning Region") ~ "NHS England Midlands",
      region %in% c("NHS England North West") ~ "NHS England North West",
      region %in% c("NHS England North East And Yorkshire", "NHS England North", "North Of England Commissioning Region") ~ "NHS England North East And Yorkshire",
      region %in% c("NHS England London", "London Commissioning Region") ~ "NHS England London",
      region %in% c("NHS England South East") ~ "NHS England South East",
      region %in% c("NHS England South West", "South Of England Commissioning Region") ~ "NHS England South West",
      region %in% c("NHS England East Of England") ~ "NHS England East Of England",
      str_detect(region, "Organisation") | str_detect(region, "org") ~ NA_character_,  # Drop rows with irrelevant org labels
      TRUE ~ region  # Keep as-is if already correct or unknown
    )
  )

ordered_data <- ordered_data %>%
  filter(!is.na(region))

# Convert Variables to number
ordered_data <- ordered_data %>%
  mutate(across(
    c(AE_att_T1, AE_att_T2, AE_att_T3, AE_att_tot, 
      AE_more4_T1, AE_more4_T2, AE_more4_T3, AE_more4_tot, 
      AE_per4_T1, AE_per4_all, Em_adm_AE_T1, Em_adm_AE_T2, Em_adm_AE_T34, Em_adm_AE_tot, 
      Em_adm_other, Em_adm_tot, Em_pat_adm_more4, Em_pat_adm_more12),
    ~ as.numeric(na_if(na_if(as.character(.x), "-"), "NA"))
  ))

# Convert Date to numeric (days since the first date)
# Ensure date is a Date object
ordered_data <- ordered_data %>%
  mutate(date = as.Date(date))

# Create day_num starting from 0
min_date <- min(ordered_data$date, na.rm = TRUE)

ordered_data <- ordered_data %>%
  mutate(day_num = as.integer(difftime(date, min_date, units = "days")))

model_data <- ordered_data %>%
  filter(!is.na(AE_att_tot) & !is.na(code) & !is.na(region) & !is.na(day_num))

# Step 1: Create lookup table with latest region name per code
latest_region_lookup <- ordered_data %>%
  filter(!is.na(code), !is.na(region)) %>%
  group_by(code) %>%
  slice_max(order_by = day_num, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(code, latest_region = region)

# Step 2: Join and update the region field
ordered_data <- ordered_data %>%
  left_join(latest_region_lookup, by = "code") %>%
  mutate(region = latest_region) %>%
  select(-latest_region)

# This plot shows how total A&E attendances (AE_att_tot) vary over time across all NHS organizations.
# Each point represents a monthly total for a specific provider.
# This helps visualize general attendance trends and seasonal patterns.
ggplot(ordered_data) +
  geom_point(aes(x = date, y = as.numeric(AE_att_tot))) +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# This scatter plot breaks down A&E attendance totals by region.
# It uses facets to show each region separately and color codes each provider.
# Useful for comparing trends and outliers across different NHS regions.
ggplot(ordered_data) +
  geom_point(aes(x = date, y = as.numeric(AE_att_tot), colour = name)) +
  theme_bw() +
  facet_wrap(vars(region)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(legend.position = "none")

# Aggregated Attendance Totals per Hospital, Region, and Date
df1 <- ordered_data %>%
  group_by(name, region, date) %>%
  summarise(total = sum(as.numeric(AE_att_tot), na.rm = TRUE)) %>%
  as.data.frame()

# This line plot aggregates attendance data at the hospital level within each region.
# It tracks how each provider's attendance changes over time.
# Helpful for detecting consistent performers or anomalies within regions.
ggplot(df1, aes(x = date, y = total, colour = name)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(vars(region)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(legend.position = "none")

model <- lm(as.numeric(AE_att_tot) ~ code + region + day_num, data = model_data)

ordered_data$predict <- NA  # Initialize column with NAs
ordered_data$predict[which(!is.na(ordered_data$AE_att_tot) & 
                             !is.na(ordered_data$code) & 
                             !is.na(ordered_data$region) & 
                             !is.na(ordered_data$day_num))] <- predict(model)

# This chart compares actual A&E attendance to predicted values using a linear model.
# Solid lines = actual data; dashed lines = model predictions.
# Helps assess how well the model captures real attendance behavior.
ggplot(ordered_data, aes(x = date, colour = name)) +
  geom_line(aes(y = as.numeric(AE_att_tot)), linetype = "solid") +
  geom_line(aes(y = predict), linetype = "dashed") +
  theme_bw() +
  facet_grid(vars(region)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

# Aggregate Attendance Total Data by Region and Date
df_region_avg <- ordered_data %>%
  group_by(region, date) %>%
  summarise(avg_total = mean(as.numeric(AE_att_tot), na.rm = TRUE), .groups = "drop")

# This chart shows the average A&E attendance per region over time.
# It smooths out provider-level noise and highlights macro trends.
# Useful for policy insights or comparing how regions perform relative to each other.
ggplot(df_region_avg, aes(x = date, y = avg_total, colour = region)) +
  geom_point(alpha = 0.7) +  # Add transparency for clarity
  geom_line() +  # Connect points with a line
  theme_bw() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +  # Adjust date labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Aggregate More 4 Total Data by Region and Date
df_region_avg <- ordered_data %>%
  group_by(region, date) %>%
  summarise(avg_total_more4 = mean(as.numeric(AE_more4_tot), na.rm = TRUE), .groups = "drop")

# This chart tracks the average number of attendances that took more than 4 hours from arrival to resolution.
# Important for monitoring compliance with the NHS 4-hour A&E target across regions.
# Highlights problem areas and seasonal spikes in delayed service.
ggplot(df_region_avg, aes(x = date, y = avg_total_more4, colour = region)) +
  geom_point(alpha = 0.7) +  # Add transparency for clarity
  geom_line() +  # Connect points with a line
  theme_bw() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +  # Adjust date labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Step 1: Count total number of months in your data
total_months <- ordered_data %>% 
  distinct(date) %>% 
  nrow()

# Step 2: Calculate long-term % waiting > 4h per trust, with filtering
trust_waits <- ordered_data %>%
  mutate(over4_pct = (1 - as.numeric(AE_per4_T1)) * 100) %>%
  group_by(code, name) %>%
  summarise(
    mean_over4h = mean(over4_pct, na.rm = TRUE),
    valid_points = sum(!is.na(over4_pct)),
    .groups = "drop"
  ) %>%
  filter(valid_points >= total_months * 0.9) %>%  # Trusts with at least 90% data coverage over 10 years
  arrange(desc(mean_over4h))

# Step 3: Extract top and bottom performers
worst_trust <- trust_waits %>% slice(1)
best_trust <- trust_waits %>% slice(n())

# Output
worst_trust
best_trust

# Filter by Codes
df_new <- ordered_data %>% 
  filter(code=="RC9")

library(ggrepel)  # For clean direct labels

# Filter for two trusts
df_new <- ordered_data %>% 
  filter(code %in% c("RAS", "RCU"))

# Fit linear model with interaction
model <- lm(as.numeric(AE_per4_T1) ~ code * day_num, data = df_new)

# Predict values
df_new$predict <- predict(model, newdata = df_new)

# Define line colors manually
line_colors <- c("RAS" = "#914D76", "RCU" = "#C25B82")  # Replace with any hex codes you'd like

# Get label positions (latest point for each trust)
label_data <- df_new %>%
  group_by(code) %>%
  filter(date == max(date)) %>%
  ungroup()

ggplot(df_new, aes(x = date, colour = code)) +
  geom_line(aes(y = as.numeric(AE_per4_T1)), linetype = "solid", size = 0.9) +
  geom_line(aes(y = predict), linetype = "dashed", size = 0.9) +
  geom_text_repel(
    data = label_data,
    aes(y = as.numeric(AE_per4_T1), label = code),
    show.legend = FALSE,
    hjust = 0,
    nudge_x = 20,
    direction = "y",
    segment.color = NA,
    size = 4.5
  ) +
  scale_colour_manual(values = line_colors) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0, 0.15))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Date",
    y = "T1 attendances < 4 hours (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#FFF7FA", color = NA),
    plot.background = element_rect(fill = "#FFF7FA", color = NA),
    axis.line = element_line(color = "black", size = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# SOCIOECONOMIC DATA

# Install once
install.packages("devtools")
devtools::install_github("epiforecasts/covid19.nhs.data")

# Load it
library(covid19.nhs.data)
library(dplyr)

mapping_raw <- load_mapping(scale = "utla", source = "link")

mapping_named <- mapping_raw %>%
  get_names(geo_names = utla_names)

file_path1 <- "/Users/gabrielavargas/Downloads/FYP/SOCIOECON/IMD_DATA.xlsx"
imd_data <- read_excel(file_path1, sheet = 2)

imd_data_clean <- imd_data %>%
  rename(UTLA = `Upper Tier Local Authority District name (2019)`) %>%
  mutate(UTLA = trimws(tolower(UTLA)))  # lowercase for consistent join

# Step 1: Load mapping with trust + UTLA names
mapping <- load_mapping(scale = "utla", source = "link") %>%
  get_names(geo_names = utla_names)

# Step 2: Standardize names for join
imd_data_clean <- imd_data_clean %>%
  rename(imd_score = `IMD - Average score`,  # Replace with your actual column name if different
         geo_name = UTLA) %>%
  mutate(geo_name = tolower(geo_name))

mapping <- mapping %>%
  mutate(geo_name = tolower(geo_name))

# Step 3: Join IMD scores into mapping
mapping_with_imd <- mapping %>%
  left_join(imd_data_clean, by = "geo_name")

# Step 4: Create new dataset with Trust-level weighted IMD score
trust_data_with_imd <- mapping_with_imd %>%
  group_by(trust_name) %>%
  summarise(
    weighted_imd = sum(imd_score * p_trust, na.rm = TRUE)
  ) %>%
  ungroup()

# Final check
head(trust_data_with_imd)

# Create a new merged dataset
final_data <- combined_data %>%
  left_join(trust_data_with_imd, by = c("name" = "trust_name"))

# Preview the merged data
head(final_data)

# Check if there are any Trusts with missing IMD data
final_data %>% filter(is.na(weighted_imd)) %>% select(name)

# Create a clean dataset with no missing values in the needed columns
model_data <- final_data %>%
  select(AE_att_tot, weighted_imd) %>%
  filter(!is.na(AE_att_tot) & !is.na(weighted_imd))

model_data <- model_data %>%
  mutate(
    AE_att_tot = as.numeric(AE_att_tot),
    weighted_imd = as.numeric(weighted_imd)
  )

clean_data <- final_data %>%
  filter(!is.na(weighted_imd) & !is.na(AE_att_tot))

# SEASON VARIABLE

library(dplyr)
library(lubridate)

# Convert "April 2016" to a Date object (default to first of the month)
IMD_seasons_data_nhs <- clean_data %>%
  mutate(
    # Parse the text into a proper date (first day of the month)
    time_date = dmy(paste("01", time_period)),  
    month = month(time_date, label = TRUE, abbr = FALSE),
    season = case_when(
      month %in% c("December", "January", "February") ~ "Winter",
      month %in% c("March", "April", "May") ~ "Spring",
      month %in% c("June", "July", "August") ~ "Summer",
      month %in% c("September", "October", "November") ~ "Autumn",
      TRUE ~ NA_character_
    )
  )

#Practice models!
model_lm <- lm(AE_att_tot ~ weighted_imd, data = model_data)
summary(model_lm)

install.packages("randomForest")
library(randomForest)

model_rf <- randomForest(
  AE_att_tot ~ weighted_imd,
  data = model_data,
  ntree = 500,
  importance = TRUE
)

# Variable importance
importance(model_rf)

#PLOTS
model_data$pred_lm <- predict(model_lm, newdata = model_data)
model_data$pred_rf <- predict(model_rf, newdata = model_data)

plot(model_data$AE_att_tot, model_data$pred_rf,
     main = "Random Forest Predictions",
     xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")




