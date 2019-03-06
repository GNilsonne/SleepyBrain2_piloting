# Script to rename files to the proper BIDS format
rm(list=ls())
EEG_raw <- list.files(pattern = ".acq")
EEG_f <- list.files(pattern = "filtered")
diary <- read.csv2("Sleepy_brain_diary_checked.csv")
diary <- diary[c("id", "session", "day", "month", "year")]
diary$date <- paste(diary$year, diary$month, diary$day, sep ="/")

# Rename raw files
for (i in 1:length(EEG_raw)){
  #thisfile <- read.table(EEG_raw[i], header = T)
  thissubject <- substring(EEG_raw[i], 1, 4) # Extract subject
  year <- substring(EEG_raw[i], 6, 9) # Extract year
  month <- substring(EEG_raw[i], 11, 12) # Extract month
  day <- substring(EEG_raw[i], 14, 15) # Extract day
  if(substr(month, 1, 1) == "0"){ # This fixes issues when the file begins with a "0"
    month <- substr(month, 2, 2)
  }
  if(substr(day, 1, 1) == "0"){
    day <- substr(day, 2, 2)
  }
  thisdate <- paste(year, month, day, sep ="/") # Paste together day/month/year
  thissession <- diary[diary$id == as.integer(thissubject) & diary$date == thisdate, "session"] + 1 # Session nr, "+ 1" because session 1 corresponds to session 2
  thistask <- "rest"
  output_filename <- paste("sub-", thissubject, "_ses-", thissession, "_task-", thistask, "_eeg.acq", sep = "")
  #write.table(thisfile, file = output_filename, sep = "\t", row.names = FALSE)
  file.rename(EEG_raw[i], output_filename)
}

# Rename filtered files
for (i in 1:length(EEG_f)){
  #thisfile <- read.table(EEG_f[i], header = T)
  thissubject <- substring(EEG_f[i], 1, 4) # Extract subject
  year <- substring(EEG_f[i], 6, 9) # Extract year
  month <- substring(EEG_f[i], 11, 12) # Extract month
  day <- substring(EEG_f[i], 14, 15) # Extract day
  if(substr(month, 1, 1) == "0"){ # This fixes issues when the file begins with a "0"
    month <- substr(month, 2, 2)
  }
  if(substr(day, 1, 1) == "0"){
    day <- substr(day, 2, 2)
  }
  thisdate <- paste(year, month, day, sep ="/") # Paste together day/month/year
  thissession <- diary[diary$id == as.integer(thissubject) & diary$date == thisdate, "session"] + 1 # Session nr, "+ 1" because session 1 corresponds to session 2
  thistask <- "rest"
  output_filename <- paste("sub-", thissubject, "_ses-", thissession, "_task-", thistask, "_eeg.edf", sep = "")
  #write.table(EEG_f, file = output_filename, sep = "\t", row.names = FALSE)
  file.rename(EEG_f[i], output_filename)
}
