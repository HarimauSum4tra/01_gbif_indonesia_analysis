wait_for_download <- function(key, sleep_seconds = 10, max_wait_minutes = 30) {
  cat("\nWaiting for download to complete...\n")
  start_time <- Sys.time()
  
  repeat {
    meta <- occ_download_meta(key)
    status <- meta$status
    elapsed <- difftime(Sys.time(), start_time, units = "mins")
    
    cat(paste("Status:", status, "- Time:", 
              round(elapsed, 1), "of", max_wait_minutes, "minutes\n"))
    
    if (status == "SUCCEEDED") {
      cat("\n✅ Download successful!\n")
      cat(paste("File size:", meta$size, "bytes\n"))
      cat(paste("Number of records:", meta$totalRecords, "\n"))
      return(TRUE)
    } else if (status == "FAILED") {
      cat("\n❌ Download failed!\n")
      cat("Error:", meta$error, "\n")
      return(FALSE)
    } else if (elapsed > max_wait_minutes) {
      cat("\n⏰ Timeout after", max_wait_minutes, "minutes\n")
      return(FALSE)
    }
    
    Sys.sleep(sleep_seconds)
  }
}
