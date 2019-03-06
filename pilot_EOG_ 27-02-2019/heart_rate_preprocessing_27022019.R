# Preprocessing of heartrate data to receive graphs and write datafiles to tables
# This is ONLY for the pilot testing on date 2019-02-27!

rm(list=ls())

#read in heart rate files
files_heartrate <- list.files(pattern = "PPG", recursive = T)

#comment: I renamed the heart rate files for each participant in for example: PPGTrig_8041_s2_2019-01-11
#so that I can extract session und subject id

#create dataframe containing file names and heart rate date
for(i in 1:length(files_heartrate)){
  thisfile <- read.delim(files_heartrate[i])
  
  if(!exists("heartrate")){
    heartrate <- data.frame(file = files_heartrate[i], peak = thisfile[, 1])
  } else {
    thesedata <- data.frame(file = files_heartrate[i], peak = thisfile[, 1])
    heartrate <- rbind(heartrate, thesedata)
  }
}

#cut off subject and session from filename
heartrate$file <- as.character(heartrate$file)
heartrate$subject <- as.integer("1001")
heartrate$session <- as.integer("1001")

#set condition for each participant "manually"
heartrate$condition <- "fullsleep"

# Quality control based on visual inspection
heartrate$included <- TRUE

#loops over every file to correct the data from each participant
for(i in 1:length(files_heartrate)){ 
  
  pulse_recording <- heartrate[heartrate$file==files_heartrate[i],]
  
  if(pulse_recording$included[1] == TRUE){
    
    #creates new column containing the time between two pulses
    pulse_recording$deltaTime <- pulse_recording$peak
    pulse_recording$deltaTime <- (c(pulse_recording$peak, 0)-c(0, pulse_recording$peak))[1:length(pulse_recording$deltaTime)]
    
    
    #creates a column for the heart rate in beat per minute (from centiseconds)
    pulse_recording$HR<- 6000/pulse_recording$deltaTime 
    
    #creates column for time in s
    pulse_recording$timeins <- pulse_recording$peak/100
    
    #saves the latest pulse time and creates matrices with this length (rows from 1 to latest time)
    latest_pulse <- max(pulse_recording$peak)
    
    PgDataHandsTime <- matrix(1:latest_pulse)
    CurrentSessionColumn <- matrix(1:latest_pulse)
    
    #contains recordings from the file that is currently in the loop
    CurrentSessionPulses <- pulse_recording$HR
    CurrentSessionPulseRegistrations <- pulse_recording$peak
    
    # Clean up pulse matrix -> set NA for times before first registration
    for (k in 1:CurrentSessionPulseRegistrations[1]) {
      CurrentSessionColumn[k] = NA
    }
    
    # Clean up pulse after last registration -> set NA after last one (backwards)
    # Question: why don't we take just LatestPulseTime for TimeAtLastRegistration?
    # I don't understand why there is a loop for k in a:a (same value)
    TimeAtLastRegistration <- CurrentSessionPulseRegistrations[length(CurrentSessionPulseRegistrations)]
    
    for (k in TimeAtLastRegistration:length(CurrentSessionColumn)) {
      CurrentSessionColumn[k] = NA
    }
    
    
    #loop for setting a certain heart rate during a time intervall
    for (k in 1:(length(CurrentSessionPulseRegistrations)-1)) {
      
      PulseRegA <- CurrentSessionPulseRegistrations[k]      #time Pulse A occurs
      PulseRegB <- CurrentSessionPulseRegistrations[k + 1]  #time Pulse B occurs
      PulseBetweenAAndB = CurrentSessionPulses[k]           #resulting heart rate
      
      #set the pulse to the resulting heart rate for all times between A and B
      CurrentSessionColumn[PulseRegA:PulseRegB] <- PulseBetweenAAndB
    }
    
    plot(CurrentSessionColumn, type="l", main = files_heartrate[i], ylab = "heart rate (bpm)", xlab = "time (centisecond)", frame.plot = F, col = "red")
    abline(h = 100, lty = 2)
    abline(h = 30, lty = 2)
    abline(v = 30000, col = "blue")
    
    CurrentSessionColumn[CurrentSessionColumn > 100] <- NA
    CurrentSessionColumn[CurrentSessionColumn < 30] <- NA
    
    lines(CurrentSessionColumn)
    
    #creates dataframe with corrected data from every participant
    if(!exists("bpm_data")){
      bpm_data <- data.frame(heartrate=CurrentSessionColumn,subject=pulse_recording$subject[1], session=pulse_recording$session[1], condition=pulse_recording$condition[1], index=1:length(CurrentSessionColumn))
    } else {
      thesedata <- data.frame(heartrate=CurrentSessionColumn,subject=pulse_recording$subject[1], session=pulse_recording$session[1], condition=pulse_recording$condition[1], index=1:length(CurrentSessionColumn))
      bpm_data <- rbind(bpm_data, thesedata)
    }
    
    
    #calculation of mean and standard deviation of the heart rate within 30s intervals
    #start with declaring vectors and count variables
    
    MeanHeart <- vector()
    SdHeart <- vector()
    a <- 1
    count <- 1
    
    #30s means 3000 entries for the HeartRate (every 10 milliseconds one value)
    while(a < length(CurrentSessionColumn)-3000) {
      
      MeanHeart[count] <- mean(CurrentSessionColumn[a:(a+2999)],na.rm=T)
      SdHeart[count] <- sd(CurrentSessionColumn[a:(a+2999)], na.rm=T)
      a <- a+3000
      count <- count+1
    }
    
    #plot(MeanHeart, type="l", main= "Mean of heart rate")
    
    #create another dataframe containing the 30s interval data for the heart rate from each participant
    if(!exists("bpm_mean_data")){
      bpm_mean_data <- data.frame(heartrate_mean=MeanHeart,heartrate_sd=SdHeart,subject=pulse_recording$subject[1], session=pulse_recording$session[1], condition=pulse_recording$condition[1], index=1:length(MeanHeart))
    } else {
      thesedata <- data.frame(heartrate_mean=MeanHeart,heartrate_sd=SdHeart,subject=pulse_recording$subject[1], session=pulse_recording$session[1], condition=pulse_recording$condition[1], index=1:length(MeanHeart))
      bpm_mean_data <- rbind(bpm_mean_data, thesedata)
    }
    
  }
}
