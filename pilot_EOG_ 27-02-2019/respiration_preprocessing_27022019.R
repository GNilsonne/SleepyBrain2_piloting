# Preprocessing of respiration data o receive graphs and write datafiles to tables
# This is ONLY for the pilot testing on date 2019-02-27!

rm(list=ls())

#read in respiratory data
files_respiration <- list.files(pattern = "RESP", recursive = T)

#comment: I renamed the heart rate files for each participant in for example: RESPTrig_8041_s2_2019-01-11
#so that I can extract session und subject id

#loop over all files again
for(i in 1:length(files_respiration)){
  thisfile <- read.delim(files_respiration[i])
  if(!exists("respiration")){
    respiration <- data.frame(file = files_respiration[i], peak = thisfile[, 1])
  } else {
    thesedata <- data.frame(file = files_respiration[i], peak = thisfile[, 1])
    respiration <- rbind(respiration, thesedata)
  }
}
#cut off subject and session from filename
respiration$file <- as.character(respiration$file)
respiration$subject <- as.integer("1001")
respiration$session <- as.integer("1001")

#set condition for each participant "manually"
respiration$condition <- "fullsleep"

# Quality control based on visual inspection
respiration$included <- TRUE


#loop over each file to correct the data
for(i in 1:length(files_respiration)){ 
  
  breathing_recording <- respiration[respiration$file==files_respiration[i],]
  
  
  #creates new column containing the time between two pulses
  breathing_recording$deltaTime <- breathing_recording$peak
  breathing_recording$deltaTime <- (c(breathing_recording$peak, 0)-c(0, breathing_recording$peak))[1:length(breathing_recording$deltaTime)]
  
  
  #creates a column for the breathing rate in breathing per minute (records data every 40ms)
  breathing_recording$RR<- 1500/breathing_recording$deltaTime 
  
  #creates column for recording in s
  breathing_recording$timeins <- breathing_recording$peak/25
  
  #saves the latest breathing time and creates a matrix with this length (rows from 1 to latest time)
  latest_pulse <- max(breathing_recording$peak)
  
  PgDataHandsTime <- matrix(1:latest_pulse)
  
  #creates another matrix for actual heart rate
  CurrentSessionColumn <- matrix(1:latest_pulse)
  
  CurrentSessionPulses <- breathing_recording$RR
  CurrentSessionPulseRegistrations <- breathing_recording$peak
  
  # Clean up pulse matrix -> set NA for times before first registration
  for (k in 1:CurrentSessionPulseRegistrations[1]) {
    CurrentSessionColumn[k] = NA
  }
  
  # Clean up pulse after last registration -> set NA after last one (backwards)
  # Question: why don't we take just LatestPulsTime for TimeAtLastRegistration?
  # I don't understand why there is a loop for k in a:a
  TimeAtLastRegistration <- CurrentSessionPulseRegistrations[length(CurrentSessionPulseRegistrations)]
  
  for (k in TimeAtLastRegistration:length(CurrentSessionColumn)) {
    CurrentSessionColumn[k] = NA
  }
  
  
  #For loop for setting a certain breathing rate during a time intervall
  for (k in 1:(length(CurrentSessionPulseRegistrations)-1)) {
    
    PulseRegA <- CurrentSessionPulseRegistrations[k]      #time Pulse A occurs
    PulseRegB <- CurrentSessionPulseRegistrations[k + 1]  #time Pulse B occurs
    PulseBetweenAAndB = CurrentSessionPulses[k]           #resulting heart rate
    
    # Set the pulse to the resulting breathing rate for all times between A and B
    CurrentSessionColumn[PulseRegA:PulseRegB] <- PulseBetweenAAndB
  }
  
  
  #CurrentSessionColumn[CurrentSessionColumn > 40] <- NA
  
  #Plot of breathing rate
  plot(CurrentSessionColumn, type="l", main = files_respiration[i], ylab = "breathing rate (bpm)", xlab = "time (decisecond)", frame.plot = F)
  #abline(h = , lty = 2)
  #abline(h = , lty = 2)
  
  
  #creates new dataframe for the corrected breathing rate for each participant
  if(!exists("rpm_data")){
    rpm_data <- data.frame(resp_rate=CurrentSessionColumn,subject=breathing_recording$subject[1], session=breathing_recording$session[1], condition=breathing_recording$condition[1])
  } else {
    thesedata <- data.frame(resp_rate=CurrentSessionColumn,subject=breathing_recording$subject[1], session=breathing_recording$session[1], condition=breathing_recording$condition[1])
    rpm_data <- rbind(rpm_data, thesedata)
  }
  
  
  #calculation of mean and standard deviation of the breathing rate within 30s intervals
  #start with declaring vectors and count variables
  MeanBreath <- vector()
  SdBreath <- vector()
  a <- 1
  count <- 1
  
  #calculate mean and standard deviation of breathing every 30s (records with 25Hz, so 25*30s equals 750 entries)
  
  while(a < length(CurrentSessionColumn)-750) {
    
    MeanBreath[count] <- mean(CurrentSessionColumn[a:(a+749)],na.rm=T)
    SdBreath[count] <- sd(CurrentSessionColumn[a:(a+749)], na.rm=T)
    a <- a+750
    count <- count+1
  }
}