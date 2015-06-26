#script to pull data from the phadia100's .csv run files.

rm(list=ls())

#your directory goes here
filenum <- 0
list <- list.files("C:\\Users\\cew27\\Dropbox (Personal)\\R\\Phadia Test")
list<- paste("C:\\Users\\cew27\\Dropbox (Personal)\\R\\Phadia Test\\", list, sep="")

for (step in 1:length(list)) {
  step=2

phadia <- read.csv(list[step], header=T, skip = 7, na.strings = "")

#seperate out the data we need and clean it up
#Note that if you're looking at the file in excel, the concentration appears under the column "(quot)", but when you read it into r, it's under "X.Conc.". I can't figure out why. Seems to be the case for other columns as well. Double-check that you're getting the data you want.
phadia <- data.frame(phadia$X.Request., phadia$X.Test., phadia$X.Conc., phadia$X.InsDil.)
colnames(phadia) <- c("sampleID", "test", "conc", "dil")

#Removes the junk leftover from the data summary at the bottom of the .csv and QC/CC data
phadia <- phadia[complete.cases(phadia),]

#make columns for study, subject #, and visit # (used to pick out which redcap doc to enter the values into)
phadia$study <- phadia$sampleID
phadia$study <- gsub("-.*", "", phadia$study)

phadia$study_id <- phadia$sampleID
phadia$study_id <- gsub("_.*", "", phadia$study_id)

phadia$visitNum <- phadia$sampleID
phadia$visitNum <- gsub(".*_", "", phadia$visitNum)

phadia$file <- rep(list[step])

#convert the concentration readings to numerics rather than factors.

phadia$conc <- as.numeric(as.character(phadia$conc))

#The raw data doesn't indicate if a concentration is off the scale, so we need to figure out which are extrapolated (>100 or < 0.1)

#change under-the-limit concentrations to "0"
v <- vector(mode="logical", length=0)
for (i in 1:length(phadia$conc)){
  v[i] <- (phadia$conc[i] < 0.1)
}   
phadia$conc[v] <- 0

#change over-the-limit concentrations to ">"
phadia$max <- phadia$dil*100

for (i in 1:length(phadia$max)){
  if (phadia$test[i] == "a-IgE") {phadia$max[i] = phadia$dil[i]*5000}
}

v <- vector(mode="logical", length=0)
for (i in 1:length(phadia$conc)){
  v[i] <- (phadia$conc[i] > phadia$max[i])
}   
phadia$conc[v] <- paste(">",phadia$max[v], sep="")

if(filenum == 0){
  all <- phadia
}
  else {
    all <- rbind(all, phadia)
  }
filenum <- filenum + 1
}

#Note that, because concentration is a string rather than a numeric, this doesn't order the concentrations very well. (98.6 will come before 980, for example), but the purpose is to put the 0's and >100's on the bottom of the pile.
all <- all[order(all$conc, decreasing=T),]

#typo fix
all$study <- gsub("956", "965", all$study)
all$sampleID <- gsub("956", "965", all$sampleID)
all$study_id <- gsub("956", "965", all$study_id)

#Then we need to split the data by project and read the data into the appropriate redcap.
phad <- split.data.frame(all, all$study)

list2env(phad, .GlobalEnv)

#965 - PNOIT1 Study data
#First step is to create columns with names to match the RedCAP database. Note that the columns must be in the same order as the redCAP datasheet places them.

`965`$redcap_event_name <- `965`$visitNum

#get the event name right so RedCAP can find the form

`965`$redcap_event_name <- gsub("(?<![0-9])0+", "", `965`$redcap_event_name, perl=T)
`965`$redcap_event_name <- gsub("BU", "Build Up #", `965`$redcap_event_name)
`965`$redcap_event_name <- gsub("MN", "Maintenance #", `965`$redcap_event_name)
`965`$redcap_event_name <- gsub("FU", "Follow up #", `965`$redcap_event_name)

con <- grep("965-04|965-09|965-10|965-11|965-15", `965`$study_id)

`965`$redcap_event_name[con] <- paste(`965`$redcap_event_name[con], "(Arm 4: Active Phase for Controls)", sep=" ")

`965`$redcap_event_name[-con] <- paste(`965`$redcap_event_name[-con], "(Arm 2: Active Group Arm)", sep=" ")

#con3 <- grep("Follow up #. .Arm 4",`965`$redcap_event_name) 
#`965`$redcap_event_name[con3] <- gsub("*\\(.*?\\) *", "\\(Arm 3: Control Group Arm\\)",`965`$redcap_event_name[con3]) 

`965`$redcap_event_name <- gsub("BL \\(.*?\\)", "Baseline (Arm 1: Enrollment/Baseline)", `965`$redcap_event_name)

#Get the tests into wide format
`965`<- reshape(`965`, v.names="conc", idvar="sampleID", timevar="test", direction="wide", drop = c("max", "dil"))

#More column names
`965`$subject_enrollment_number <- `965`$study_id
colnames(`965`)[names(`965`) == "conc.f13"] <- "peanut_f13"
colnames(`965`)[names(`965`) == "conc.f352"] <- "rara_h_8_peanut_f352"
colnames(`965`)[names(`965`) == "conc.f422"] <- "rara_h_1_peanut_f422"
colnames(`965`)[names(`965`) == "conc.f423"] <- "rara_h_2_peanut_f423"
colnames(`965`)[names(`965`) == "conc.f424"] <- "rara_h_3_peanut_f424"
colnames(`965`)[names(`965`) == "conc.f427"] <- "rara_h_9_peanut_f427"
colnames(`965`)[names(`965`) == "conc.a-IgE"] <- "total_ige_peanut"


#Remove the extra columns
keep <- c("study_id", "redcap_event_name", "subject_enrollment_number", "peanut_f13", "rara_h_1_peanut_f422", "rara_h_2_peanut_f423", "rara_h_3_peanut_f424", "rara_h_8_peanut_f352", "rara_h_9_peanut_f427", "total_ige_peanut")
"965" <- `965`[keep]

rcorder <- c("study_id", "redcap_event_name", "subject_enrollment_number", "peanut_f13", "rara_h_1_peanut_f422","rara_h_2_peanut_f423", "rara_h_3_peanut_f424", "rara_h_8_peanut_f352", "rara_h_9_peanut_f427", "total_ige_peanut")

`965` <- `965`[c("study_id", "redcap_event_name", "subject_enrollment_number", "peanut_f13", "rara_h_1_peanut_f422","rara_h_2_peanut_f423", "rara_h_3_peanut_f424", "rara_h_8_peanut_f352", "rara_h_9_peanut_f427", "total_ige_peanut")]


#Add similar clean ups for other projects as needed here
