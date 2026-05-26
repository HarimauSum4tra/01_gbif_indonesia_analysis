
# GBIF API Data Download and Analysis For Pongo pygmaeus
# Script to Download and Analyze Bornean Orangutan Occurrence Data


# 1. Clean Environment ---------------------------------------
rm(list = ls())
cat("\f")
Sys.Date()
Sys.timezone()

# 2. Install and Load Packages -------------------------------
packages_needed <- c(
  # Data wrangling
  "dplyr", "tidyr", "tidyverse", "readr", "data.table",
  "lubridate", "readxl", "purrr", "stringr", "hms",
  # Spatial
  "sp", "sf", "mapview", "rnaturalearth", "rnaturalearthdata",
  # Visualization
  "ggplot2", "ggrepel", "patchwork", "tidyquant",
  # Utilities
  "here", "fs", "rstudioapi", "rgbif", "usethis",
  "scales", "RColorBrewer"
)

pk_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if (length(pk_to_install) > 0) {
  install.packages(pk_to_install, repos = "http://cran.r-project.org")
}

invisible(lapply(packages_needed, library, character.only = TRUE))

# 3. Setup GBIF Credentials ----------------------------------
# Important: Replace with your credentials or use .Renviron file
# usethis::edit_r_environ() # Add: GBIF_USER, GBIF_PWD, GBIF_EMAIL
Sys.setenv(GBIF_USER = "xxxxxxxxx", # use your username
           GBIF_PWD = "Kehutanan_13",  # use your password
           GBIF_EMAIL = "xxxxxxxxx@email.com") # use tour email

# 4. Get Taxon Key -------------------------------------------
cat("\n=== Searching Taxon Key For Pongo pygmaeus ===\n")

taxon_info <- name_backbone(name = "Pongo pygmaeus", rank = "species")
taxon_key <- taxon_info$usageKey

cat(paste("Taxon Key:", taxon_key, "\n"))
cat(paste("Scientific name:", taxon_info$scientificName, "\n"))

# 5. Check Data Availability ---------------------------------
cat("\n=== Checking Data Availability ===\n")

total_records <- occ_count(taxonKey = taxon_key, 
                           hasCoordinate = TRUE,
                           hasGeospatialIssue = FALSE)

cat(paste("Total records (with coordinates & no geospatial issues):", 
          format(total_records, big.mark = ","), "\n"))

# 6. Download Data -------------------------------------------
cat("\n=== Starting Data Download Process ===\n")

download_key <- occ_download(
  pred("taxonKey", taxon_key),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred_gte("year", 2000),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV"
)

cat(paste("Download key:", download_key[1], "\n"))

# 7. Wait For Download Completion ----------------------------
source("scripts/source/fn_wait_for_download.r")
# Run wait function
if (wait_for_download(download_key, sleep_seconds = 10, max_wait_minutes = 30)) {
  
  # 8. Download and Import Data ------------------------------
  cat("\n=== Downloading Data File ===\n")
  
  # Create directories
  if (!dir.exists(here::here("data"))) {
    dir.create(here::here("data"), recursive = TRUE)
  }
  
  if (!dir.exists(here::here("outputs"))) {
    dir.create(here::here("outputs"), recursive = TRUE)
  }
  
  if (!dir.exists(here::here("outputs", "figures"))) {
    dir.create(here::here("outputs", "figures"), recursive = TRUE)
  }
  
  if (!dir.exists(here::here("outputs", "tables"))) {
    dir.create(here::here("outputs", "tables"), recursive = TRUE)
  }
  
  data_path <- occ_download_get(download_key, 
                                path = here::here("data"), 
                                overwrite = TRUE)
  
  cat("Importing data...\n")
  pongo_data <- occ_download_import(data_path)
  
  # 9. Filter Data (Only Indonesia and Malaysia [Kalimantan Island]) -------------
  cat("\n=== Filtering Data ===\n")
  cat(paste("Data before filter:", nrow(pongo_data), "\n"))
  
  # Filter countryCode ID and MY
  pongo_data <- pongo_data %>%
    filter(countryCode %in% c("ID", "MY"))
  
  cat(paste("Data after filter (ID & MY):", nrow(pongo_data), "\n"))
  
  # 10. Standardize State/Province Names ----------------------
  cat("\n=== Standardizing Province Names ===\n")
  
  pongo_data <- pongo_data %>%
    mutate(
      stateProvince = case_when(
        # Handle NA values first
        is.na(stateProvince) ~ "Unidentified Province",
        # Kalimantan provinces standardization
        grepl("Kalimantan Tengah|Central Kalimantan", stateProvince, ignore.case = TRUE) ~ "Central Kalimantan",
        grepl("Kalimantan Barat|West Kalimantan", stateProvince, ignore.case = TRUE) ~ "West Kalimantan",
        grepl("Kalimantan Selatan|South Kalimantan", stateProvince, ignore.case = TRUE) ~ "South Kalimantan",
        grepl("Kalimantan Timur|East Kalimantan", stateProvince, ignore.case = TRUE) ~ "East Kalimantan",
        grepl("Kalimantan Utara|North Kalimantan", stateProvince, ignore.case = TRUE) ~ "North Kalimantan",
        grepl("Kalimantan", stateProvince, ignore.case = TRUE) ~ "Kalimantan",
        # Sabah and Sarawak
        grepl("Sabah", stateProvince, ignore.case = TRUE) ~ "Sabah",
        grepl("Sarawak", stateProvince, ignore.case = TRUE) ~ "Sarawak",
        # Default (keep original)
        TRUE ~ stateProvince
      ),
      # Clean subspecies names
      subspecies = case_when(
        is.na(infraspecificEpithet) ~ "Unidentified",
        tolower(infraspecificEpithet) %in% c("morio", "pygmaeus", "wurmbii") ~ tolower(infraspecificEpithet),
        TRUE ~ "Unidentified"
      )
    )
  
  # 11. Initial Data Exploration -----------------------------
  cat("\n=== Downloaded Data Structure ===\n")
  cat(paste("Number of records:", nrow(pongo_data), "\n"))
  cat(paste("Number of columns:", ncol(pongo_data), "\n"))
  
  # Subspecies distribution
  cat("\n=== Subspecies Distribution ===\n")
  subspecies_counts <- pongo_data %>%
    group_by(subspecies) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  print(subspecies_counts)
  
  # 12. Data Preparation For Visualization ------------------------
  cat("\n=== Preparing Data For Visualization ===\n")
  
  pongo_clean <- pongo_data %>%
    mutate(
      year_group = case_when(
        year < 2005 ~ "2000-2004",
        year < 2010 ~ "2005-2009",
        year < 2015 ~ "2010-2014",
        year < 2020 ~ "2015-2019",
        TRUE ~ "2020-2024"
      ),
      year_group = factor(year_group, 
                          levels = c("2000-2004", "2005-2009", 
                                     "2010-2014", "2015-2019", "2020-2024")),
      country_name = case_when(
        countryCode == "ID" ~ "Indonesia",
        countryCode == "MY" ~ "Malaysia",
        TRUE ~ countryCode
      )
    )
  
  
  # 13. Visualization 1: Borneo Map With Distribution ------
  
  cat("\n=== Creating Borneo Focus Map ===\n")
  
  # Download world map and crop to Borneo
  world <- ne_countries(scale = "medium", returnclass = "sf")
  borneo_map <- st_crop(world, xmin = 108, xmax = 120, ymin = -4, ymax = 8)
  
  # Sample data for map (max 5000 points)
  set.seed(123)
  pongo_sample <- pongo_clean %>%
    filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))
  
  # Only sample if more than 5000 rows
  if(nrow(pongo_sample) > 5000) {
    pongo_sample <- pongo_sample %>% sample_n(5000)
  }
  
  # Map with subspecies differentiation
  p1_borneo_map <- ggplot() +
    geom_sf(data = borneo_map, fill = "lightgray", color = "white", linewidth = 0.2) +
    geom_point(data = pongo_sample,
               aes(x = decimalLongitude, y = decimalLatitude, 
                   color = subspecies, shape = subspecies),
               alpha = 0.6, size = 1.5) +
    coord_sf(xlim = c(108, 120), ylim = c(-4, 8)) +
    theme_minimal() +
    scale_color_manual(name = "Subspecies",
                       values = c("morio" = "#E41A1C", 
                                  "pygmaeus" = "#377EB8", 
                                  "wurmbii" = "#4DAF4A",
                                  "Unidentified" = "gray50")) +
    scale_shape_manual(name = "Subspecies",
                       values = c("morio" = 16, 
                                  "pygmaeus" = 17, 
                                  "wurmbii" = 15,
                                  "Unidentified" = 4)) +
    labs(title = "Pongo pygmaeus Distribution in Borneo",
         subtitle = "Occurrence Records by Subspecies (2000-2024)",
         x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "right")
  
  print(p1_borneo_map)
  ggsave(here::here("outputs", "figures", "01_borneo_distribution_map.png"), 
         plot = p1_borneo_map, width = 12, height = 10, dpi = 300)
  
  
  # 14. Visualization 2: Annual Occurrence Trend -------
  
  p2_yearly <- ggplot(pongo_clean, aes(x = year)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal() +
    labs(title = "Annual Occurrence of Pongo pygmaeus",
         subtitle = "GBIF Data 2000-2024 (Indonesia & Malaysia)",
         x = "Year", y = "Number of Occurrences") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 2))
  
  print(p2_yearly)
  ggsave(here::here("outputs", "figures", "02_annual_occurrence.png"), 
         plot = p2_yearly, width = 12, height = 6, dpi = 300)
  
  
  # 15. Visualization 3: 5-Year Occurrence Trend --------
  
  p3_5year <- ggplot(pongo_clean, aes(x = year_group, fill = year_group)) +
    geom_bar() +
    theme_minimal() +
    scale_fill_brewer(palette = "Blues") +
    labs(title = "Pongo pygmaeus Occurrence by 5-Year Period",
         subtitle = "GBIF Data 2000-2024 (Indonesia & Malaysia)",
         x = "5-Year Period", y = "Number of Occurrences",
         fill = "Period") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(stat = 'count', aes(label = after_stat(count)), 
              vjust = -0.5, size = 3.5)
  
  print(p3_5year)
  ggsave(here::here("outputs", "figures", "03_5year_occurrence.png"), 
         plot = p3_5year, width = 10, height = 6, dpi = 300)
  
  
  # 16. Visualization 4: Top Locations (State/Province) -------
  
  top_states <- pongo_clean %>%
    filter(stateProvince != "Unidentified Province") %>%
    group_by(stateProvince, country_name) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count)) %>%
    head(10)
  
  p4_locations <- ggplot(top_states, aes(x = reorder(stateProvince, count), 
                                         y = count, fill = country_name)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = c("Indonesia" = "#2E8B57", "Malaysia" = "#FFD700")) +
    labs(title = "Top 10 Locations With Highest Occurrence",
         subtitle = "Pongo pygmaeus in Indonesia and Malaysia",
         x = "State/Province", y = "Number of Occurrences",
         fill = "Country") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_text(aes(label = count), hjust = -0.2, size = 3)
  
  print(p4_locations)
  ggsave(here::here("outputs", "figures", "04_top_locations.png"), 
         plot = p4_locations, width = 10, height = 6, dpi = 300)
  
  
  # 17. Visualization 5: Subspecies Distribution by Province -------
  
  subspecies_by_location <- pongo_clean %>%
    filter(subspecies != "Unidentified") %>%
    group_by(stateProvince, subspecies) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(stateProvince) %>%
    mutate(total = sum(count)) %>%
    filter(total > 10) %>%
    ungroup()
  
  if(nrow(subspecies_by_location) > 0) {
    p5_subspecies_loc <- ggplot(subspecies_by_location, 
                                aes(x = reorder(stateProvince, -total), 
                                    y = count, fill = subspecies)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      scale_fill_manual(values = c("morio" = "#E41A1C", 
                                   "pygmaeus" = "#377EB8", 
                                   "wurmbii" = "#4DAF4A")) +
      labs(title = "Subspecies Distribution by Location",
           subtitle = "Pongo pygmaeus Subspecies Per Province/State",
           x = "State/Province", y = "Number of Occurrences",
           fill = "Subspecies") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5), 
                size = 2.5)
    
    print(p5_subspecies_loc)
    ggsave(here::here("outputs", "figures", "05_subspecies_by_location.png"), 
           plot = p5_subspecies_loc, width = 12, height = 6, dpi = 300)
  }
  
  
  # 18. Visualization 6: Subspecies Temporal Trend ---------
  
  subspecies_trend <- pongo_clean %>%
    filter(subspecies != "Unidentified") %>%
    group_by(year_group, subspecies) %>%
    summarise(count = n(), .groups = 'drop')
  
  if(nrow(subspecies_trend) > 0) {
    p6_subspecies_trend <- ggplot(subspecies_trend, 
                                  aes(x = year_group, y = count, 
                                      color = subspecies, group = subspecies)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      scale_color_manual(values = c("morio" = "#E41A1C", 
                                    "pygmaeus" = "#377EB8", 
                                    "wurmbii" = "#4DAF4A")) +
      labs(title = "Temporal Trend of Pongo pygmaeus Subspecies",
           subtitle = "Occurrence Records by 5-Year Period",
           x = "Period", y = "Number of Occurrences",
           color = "Subspecies") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(aes(label = count), vjust = -0.5, size = 3)
    
    print(p6_subspecies_trend)
    ggsave(here::here("outputs", "figures", "06_subspecies_temporal_trend.png"), 
           plot = p6_subspecies_trend, width = 10, height = 6, dpi = 300)
  }
  
  
  # 19. Visualization 7: Country Comparison ----------
  
  p7_country <- pongo_clean %>%
    group_by(year_group, country_name) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ggplot(aes(x = year_group, y = count, fill = country_name)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    scale_fill_manual(values = c("Indonesia" = "#2E8B57", "Malaysia" = "#FFD700")) +
    labs(title = "Country Comparison: Indonesia vs Malaysia",
         subtitle = "Pongo pygmaeus Occurrence by 5-Year Period",
         x = "5-Year Period", y = "Number of Occurrences",
         fill = "Country") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3)
  
  print(p7_country)
  ggsave(here::here("outputs", "figures", "07_country_comparison.png"), 
         plot = p7_country, width = 10, height = 6, dpi = 300)
  
  
  # 20. Visualization 8: Heatmap by Year and Month --------
  
  p8_heatmap <- pongo_clean %>%
    filter(!is.na(month) & !is.na(year)) %>%
    group_by(year, month) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ggplot(aes(x = year, y = month, fill = count)) +
    geom_tile() +
    theme_minimal() +
    scale_fill_gradient(low = "yellow", high = "red", 
                        trans = "log10", 
                        name = "Number of\nOccurrences") +
    scale_y_continuous(breaks = 1:12, 
                       labels = month.abb) +
    labs(title = "Occurrence Heatmap: Year vs Month",
         subtitle = "Pongo pygmaeus (2000-2024)",
         x = "Year", y = "Month") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p8_heatmap)
  ggsave(here::here("outputs", "figures", "08_heatmap_year_month.png"), 
         plot = p8_heatmap, width = 14, height = 8, dpi = 300)
  
  
  # 21. Statistical Summary -------
  
  cat("\n=== Statistical Summary ===\n")
  
  # Overall statistics
  cat("\n--- Overall Statistics ---\n")
  cat(paste("Total records:", nrow(pongo_clean), "\n"))
  cat(paste("Year range:", min(pongo_clean$year, na.rm = TRUE), "-", 
            max(pongo_clean$year, na.rm = TRUE), "\n"))
  cat(paste("Number of institutions:", n_distinct(pongo_clean$institutionCode, na.rm = TRUE), "\n"))
  cat(paste("Number of locations:", n_distinct(pongo_clean$stateProvince, na.rm = TRUE), "\n"))
  
  # Statistics by country
  stats_country <- pongo_clean %>%
    group_by(country_name) %>%
    summarise(
      Total_Records = n(),
      Earliest_Year = min(year, na.rm = TRUE),
      Latest_Year = max(year, na.rm = TRUE),
      Unique_Institutions = n_distinct(institutionCode, na.rm = TRUE),
      Unique_Locations = n_distinct(stateProvince, na.rm = TRUE)
    )
  
  cat("\n--- Statistics by Country ---\n")
  print(stats_country)
  
  # Statistics by subspecies
  stats_subspecies <- pongo_clean %>%
    filter(subspecies != "Unidentified") %>%
    group_by(subspecies) %>%
    summarise(
      Total_Records = n(),
      Earliest_Year = min(year, na.rm = TRUE),
      Latest_Year = max(year, na.rm = TRUE),
      Unique_Locations = n_distinct(stateProvince, na.rm = TRUE)
    )
  
  cat("\n--- Statistics by Subspecies ---\n")
  print(stats_subspecies)
  
  # Save statistics as tables
  write.csv(stats_country, here::here("outputs", "tables", "statistics_by_country.csv"), 
            row.names = FALSE)
  write.csv(stats_subspecies, here::here("outputs", "tables", "statistics_by_subspecies.csv"), 
            row.names = FALSE)
  
  # Save subspecies counts table
  write.csv(subspecies_counts, here::here("outputs", "tables", "subspecies_counts.csv"), 
            row.names = FALSE)
  
  # Save top locations table
  write.csv(top_states, here::here("outputs", "tables", "top_locations.csv"), 
            row.names = FALSE)
  
  
  # 22. Save Filtered Data --------
  
  cat("\n=== Saving Filtered Data ===\n")
  
  saveRDS(pongo_clean, here::here("data", "pongo_pygmaeus_filtered.rds"))
  
  write.csv(pongo_clean, 
            here::here("data", "pongo_pygmaeus_filtered.csv"), 
            row.names = FALSE)
  
  
  # 23. Final Summary -------
  
  cat("\n✅ All processes completed successfully!\n")
  cat("\n📁 Files saved in following directories:\n")
  cat("\n--- Data Files ---\n")
  cat("  • data/pongo_pygmaeus_filtered.rds\n")
  cat("  • data/pongo_pygmaeus_filtered.csv\n")
  cat("\n--- Figures (outputs/figures/) ---\n")
  cat("  • 01_borneo_distribution_map.png\n")
  cat("  • 02_annual_occurrence.png\n")
  cat("  • 03_5year_occurrence.png\n")
  cat("  • 04_top_locations.png\n")
  if(exists("p5_subspecies_loc")) cat("  • 05_subspecies_by_location.png\n")
  if(exists("p6_subspecies_trend")) cat("  • 06_subspecies_temporal_trend.png\n")
  cat("  • 07_country_comparison.png\n")
  cat("  • 08_heatmap_year_month.png\n")
  cat("\n--- Tables (outputs/tables/) ---\n")
  cat("  • statistics_by_country.csv\n")
  cat("  • statistics_by_subspecies.csv\n")
  cat("  • subspecies_counts.csv\n")
  cat("  • top_locations.csv\n")
  
} else {
  cat("\n❌ Failed to download data. Please try again later.\n")
  cat("Download key for reference:", download_key, "\n")
}

  # 24 Save Environment Data --------
save.image("D:/OneDrive - Fauna & Flora/3. West Kalimantan Analysis/G00-Project for Portfolio/01_gbif_indonesia_analysis/data/envData.RData")
