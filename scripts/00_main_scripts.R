# ============================================================
# SETUP DAN KONEKSI KE GBIF API
# Script untuk mengunduh data Pongo pygmaeus dari GBIF
# ============================================================

# 1. CLEAN ENVIRONMENT ---------------------------------------
rm(list = ls())
cat("\f")
Sys.Date()
Sys.timezone()

# 2. INSTALL DAN LOAD PACKAGES -------------------------------
packages_needed <- c(
  "dplyr", "tidyr", "tidyverse", "readr", "data.table",
  "lubridate", "readxl", "purrr", "stringr", "hms",
  "sp", "sf", "mapview", "ggplot2", "ggrepel", "patchwork", 
  "tidyquant", "here", "fs", "rstudioapi", "rgbif", "usethis"
)

pk_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if (length(pk_to_install) > 0) {
  install.packages(pk_to_install, repos = "http://cran.r-project.org")
}

invisible(lapply(packages_needed, library, character.only = TRUE))

# 3. SETUP GBIF CREDENTIALS ----------------------------------
# Ganti password Anda setelah ini!
Sys.setenv(GBIF_USER = "xxxxxxxxx", # use your username
           GBIF_PWD = "Kehutanan_13",  # use your password
           GBIF_EMAIL = "xxxxxxxxx@email.com") # use tour email

# 4. DAPATKAN TAXON KEY --------------------------------------
cat("\n=== Mencari taxon key untuk Pongo pygmaeus ===\n")

taxon_info <- name_backbone(name = "Pongo pygmaeus", rank = "species")
taxon_key <- taxon_info$usageKey

cat(paste("Taxon Key:", taxon_key, "\n"))
cat(paste("Scientific name:", taxon_info$scientificName, "\n"))

# 5. CEK KETERSEDIAAN DATA -----------------------------------
cat("\n=== Cek ketersediaan data ===\n")

total_records <- occ_count(taxonKey = taxon_key, 
                           hasCoordinate = TRUE,
                           hasGeospatialIssue = FALSE)

cat(paste("Total records (dengan koordinat & tanpa issue):", 
          format(total_records, big.mark = ","), "\n"))

# 6. UNDUH DATA ----------------------------------------------
cat("\n=== Memulai proses download data ===\n")

download_key <- occ_download(
  pred("taxonKey", taxon_key),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred_gte("year", 2000),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV"
)

cat(paste("Download key:", download_key[1], "\n"))

# 7. TUNGGU DOWNLOAD SELESAI (Manual Loop) -------------------
wait_for_download <- function(key, sleep_seconds = 10, max_wait_minutes = 30) {
  cat("\nMenunggu download selesai...\n")
  start_time <- Sys.time()
  
  repeat {
    # Cek status
    meta <- occ_download_meta(key)
    status <- meta$status
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    
    cat(paste("Status:", status, "- Waktu:", 
              round(elapsed, 1), "dari", max_wait_minutes, "menit\n"))
    
    if (status == "SUCCEEDED") {
      cat("\n✅ Download berhasil!\n")
      cat(paste("Ukuran file:", meta$size, "bytes\n"))
      cat(paste("Jumlah record:", meta$totalRecords, "\n"))
      return(TRUE)
    } else if (status == "FAILED") {
      cat("\n❌ Download gagal!\n")
      cat("Error:", meta$error, "\n")
      return(FALSE)
    } else if (elapsed > max_wait_minutes) {
      cat("\n⏰ Waktu tunggu habis setelah", max_wait_minutes, "menit\n")
      cat("Download masih dalam antrian. Coba lagi nanti dengan key:", key, "\n")
      return(FALSE)
    }
    
    Sys.sleep(sleep_seconds)
  }
}

# Jalankan fungsi tunggu
if (wait_for_download(download_key, sleep_seconds = 10, max_wait_minutes = 30)) {
  
  # 8. DOWNLOAD DAN IMPORT DATA --------------------------------
  cat("\n=== Mengunduh file data ===\n")
  
  # Buat folder jika belum ada
  if (!dir.exists(here::here("data_raw"))) {
    dir.create(here::here("data_raw"), recursive = TRUE)
  }
  
  data_path <- occ_download_get(download_key, 
                                path = here::here("data_raw"), 
                                overwrite = TRUE)
  
  cat("Mengimpor data...\n")
  pongo_data <- occ_download_import(data_path)
  
  # 9. EKSPLORASI AWAL DATA ------------------------------------
  cat("\n=== Struktur data yang diunduh ===\n")
  cat(paste("Jumlah record:", nrow(pongo_data), "\n"))
  cat(paste("Jumlah kolom:", ncol(pongo_data), "\n"))
  
  # Kolom penting
  important_cols <- c("gbifID", "scientificName", "decimalLatitude", 
                      "decimalLongitude", "year", "countryCode", 
                      "institutionCode", "basisOfRecord")
  
  cols_available <- important_cols[important_cols %in% names(pongo_data)]
  cat("\nKolom penting yang tersedia:\n")
  print(cols_available)
  
  # 10. SIMPAN DATA --------------------------------------------
  cat("\n=== Menyimpan data ===\n")
  
  # Simpan sebagai RDS
  saveRDS(pongo_data, here::here("data_raw", "pongo_pygmaeus_gbif.rds"))
  
  # Simpan sebagai CSV (opsional, jika tidak terlalu besar)
  if(nrow(pongo_data) < 100000) {
    write.csv(pongo_data, 
              here::here("data_raw", "pongo_pygmaeus_gbif.csv"), 
              row.names = FALSE)
    cat("Data disimpan sebagai CSV\n")
  }
  
  cat("\n✅ Semua proses selesai!\n")
  cat("File tersimpan di folder 'data_raw'\n")
  
  # 11. VISUALISASI CEPAT --------------------------------------
  if(nrow(pongo_data) > 0 & all(c("decimalLongitude", "decimalLatitude") %in% names(pongo_data))) {
    
    # Buat folder output jika belum ada
    if (!dir.exists(here::here("output"))) {
      dir.create(here::here("output"), recursive = TRUE)
    }
    
    # Plot sebaran
    p <- ggplot(pongo_data, aes(x = decimalLongitude, y = decimalLatitude)) +
      geom_point(alpha = 0.3, size = 1, color = "darkgreen") +
      theme_minimal() +
      labs(title = "Sebaran Pongo pygmaeus (Orangutan Kalimantan)",
           subtitle = paste("Data dari GBIF |", format(nrow(pongo_data), big.mark = ","), "record"),
           x = "Longitude", y = "Latitude") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    print(p)
    
    # Simpan plot
    ggsave(here::here("output", "pongo_distribution.png"), 
           plot = p, width = 10, height = 8, dpi = 300)
    cat("Plot disimpan di folder 'output'\n")
  }
  
} else {
  cat("\n❌ Gagal mendownload data. Silakan coba lagi nanti.\n")
  cat("Download key untuk referensi:", download_key, "\n")
}
