# Phadia for AAAAI abstract 8/28/15
# Starts with Cecilia's script: script to pull data from the phadia100's .csv run files.
# Merges in the data with the RedCAP Phadia data, exported as PhadiaLabs report

rm(list=ls())
require(ggplot2)
require(plyr)
require(reshape2)
require(stringr)
require(hash)
require(gsubfn)
require(gdata)
require(tidyr)
require(RColorBrewer)

#your directory goes here

# check for windows/unix platform and make little compatibility adjustments
switch(Sys.info()[['sysname']],
       Windows = {print("I'm a Windows PC and the call to windows() should work as is.")},
       Linux   = {print("I'm a penguin and windows() will break but I'm not sure what graphics device you want to use.")},
       Darwin  = {print("I'm a Mac and I'm redirecting windows() to quartz()."); windows <- function(width,height) quartz(title="untitled",width, height)})
# as an exmaple, the above snippet redefines the function windows() so that calls to that from a Mac will still work.
# perhaps someone would care to add code to line 9 to redefine quartz() for PC users?
# meanwhile, Cecilia or another PC user might fully assess whether there are any remaining issues with file paths since we agreed that PC users would move 'Dropbox (ShreffLabDrop)' to the Documents folder. That will allow the use of the '~' character to specify the appropriate 'home' folder on either platform

# I put Cecilia's test files up on Dropbox

filenum <- 0
#list <- list.files("~/Dropbox (ShreffLabDrop)/Projects/PNOIT/Sample Phadia Output") # alternative here on unix would be to pipe a ls to get the entire path in one go
#list<- paste("~/Dropbox (ShreffLabDrop)/Projects/PNOIT/Sample Phadia Output", list, sep="/")

# For IgE
setwd("/Volumes/MGH-CIID/ShreffLabRemote/Phadia Results/IgE")
list <- list.files("/Volumes/MGH-CIID/ShreffLabRemote/Phadia Results/IgE") # alternative here on unix would be to pipe a ls to get the entire path in one go
list<- paste("/Volumes/MGH-CIID/ShreffLabRemote/Phadia Results/IgE", list, sep="/")


for (step in 1:length(list)) {
  
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
  
  #puts the original file name in. Comment back in for troubleshooting.
  #phadia$file <- rep(list[step])
  
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

# Write this file 
setwd("~/Dropbox (Personal)/Basophil Lab Work/Projects/PNOIT1_BAT/AugustinAnalysis/DatabasesProvided/August2015")
write.csv(`965`, "Phadia_IgE_08282015.csv")

rm(`1037`, `1057`, MIT, `965`, all)
################################################################################
# Merging all of the Databases: Cecilia's script of our Phadia runs + previous
################################################################################
# Merge the above with data from Redcap
setwd("~/Dropbox (Personal)/Basophil Lab Work/Meetings/AAAAI_2016/RedCAP_Phadia")
redphad <- read.csv("PNOIT1Phadia_08282015.csv", as.is=TRUE)

# Reload the: IgE, IgG, AH1 IgG4, and AH2 IgG4
setwd("~/Dropbox (Personal)/Basophil Lab Work/Meetings/AAAAI_2016/IC1000Data")
ige <- read.csv("Phadia_IgE_08282015.csv", as.is=TRUE)
igg <- read.csv("Phadia_IgG_08282015.csv", as.is=TRUE)
ah1igg4 <- read.csv("Phadia_AH1IgG4_08282015.csv", as.is=TRUE)
ah2igg4 <- read.csv("Phadia_AH2IgG4_08282015.csv", as.is=TRUE)

# Extracting data from previous phadia data
names(redphad)

# Rename the tests, so that they match
names(redphad)[7] <- "f422"
names(redphad)[8] <- "f423"
names(redphad)[9] <- "f424"
names(redphad)[10] <- "f352"
names(redphad)[11] <- "f427"
names(redphad)[13] <- "a-IgE"
names(redphad)[15] <- "f422g"
names(redphad)[16] <- "f423g"  

# Transfer the dilah2ige to the f423 that are over 100
redphad$f423[redphad$f423==">100"] <- redphad$dilah2ige[redphad$f423==">100"]
  
# Make a visit label  
# First change all of the arm3 FU visits to AFU
redphad$visit[redphad$redcap_event_name=="follow_up_visit_1_arm_3"] <- "AFU01"
redphad$visit[redphad$redcap_event_name=="follow_up_visit_2_arm_3"] <- "AFU02"

redphad$visitTYPE <- redphad$redcap_event_name
v <- grep("baseline", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "BL"
v <- grep("cbc", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "cbc"
v <- grep("modified_rush", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "MR"
v <- grep("build_up", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "BU"
v <- grep("maintenance", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "MN"
v <- grep("ofc", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "OFC"
v <- grep("dbfc", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "DBFC"
v <- grep("extra_visit", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "XV"
v <- grep("follow_up", redphad$visitTYPE, TRUE)
redphad$visitTYPE[v] <- "FU"
v <- grep("AFU", redphad$visit, TRUE)
redphad$visitTYPE[v] <- "AFU"

# VisitNo
redphad$visitNo <- redphad$redcap_event_name
redphad$visitNo <- gsub("_arm_[[:digit:]]", "",redphad$visitNo)
redphad$visitNo <- gsub("_if_nec", "",redphad$visitNo)
redphad$visitNo <- gsub("[[:alpha:]]+", "",redphad$visitNo)
redphad$visitNo <- gsub("_", "",redphad$visitNo)

for(i in 1:length(redphad$visitNo)) {
  if(nchar(redphad$visitNo[i])==1)
  {redphad$visitNo[i] <- paste("0", redphad$visitNo[i], sep="")}
}

redphad$visitID <- paste(redphad$visitTYPE, redphad$visitNo, sep="")

# Handcorrect
redphad$study_id[redphad$study_id=="15"] <- "965-15"

# Create a correlating "sampleID"
redphad$sampleID <- paste(redphad$study_id, redphad$visitID, sep="_")

# Create a subset of data that you actually need
redphad.small <- data.frame(redphad$sampleID, redphad$peanut_f13, redphad$f422, 
                            redphad$f423, redphad$f424, redphad$f352, 
                            redphad$f427, redphad[13], redphad$f422g, redphad$f423g)
names(redphad.small) <- c("sampleID", "peanut_f13", "f422", "f423",
                          "f424", "f352", "f427","aIgE", "f422g", "f423g")

# Extracting data from our Phadia runs datasets
ige$check <- as.numeric(ige$max) - as.numeric(ige$conc)
plot(ige$check)

# Keep only those that are appropriately diluted (not over the max)
# remove if the conc has a ">"
ige <- ige[grep(">", ige$conc, invert=TRUE), ]

# change the test name of 'a-IgE' to aIgE because R otherwise struggles with hyphens
ige$test[ige$test=="a-IgE"] <- "aIgE"

# remove if the test has 965.  There is an error in compiling
ige <- ige[grep("965-", ige$test, invert=TRUE), ]

# Correct the inconsistencies in the naming of the visits
ige$subject <- str_sub(ige$sampleID, 1, 6)
ige$visit <- ige$sampleID
ige$visit <- gsub("965-[[:digit:]]+", "", ige$visit)
ige$visit <- gsub("-", "", ige$visit)
ige$visit <- gsub(" ", "", ige$visit)
ige$visit[ige$visit=="BU2"] <- "BU02"
ige$visit[ige$visit=="BU4"] <- "BU04"
ige$visit[ige$visit=="FU1"] <- "FU01"
ige$visit[ige$visit=="FU2"] <- "FU02"
ige$visit[ige$visit=="FU3"] <- "FU03"
ige$visit[ige$visit=="FU4"] <- "FU04"
ige$visit[ige$visit=="MN1"] <- "MN01"
ige$visit[ige$visit=="MN2"] <- "MN02"
ige$sampleID <- paste(ige$subject, ige$visit, sep="_")

# then make a smaller dataset for merging
ige.small <- data.frame(ige$sampleID, ige$test, ige$conc, stringsAsFactors=F)
names(ige.small) <- c("sampleID", "test", "conc")

#For the IgG4 test datasets
#Correct the test variable
ah1igg4$test <- "f422g"
ah2igg4 <- ah2igg4[grep("965-", ah2igg4$test, invert=TRUE), ]
ah2igg4$test <- "f423g"

# Remove the 1037 samples
ah2igg4 <- ah2igg4[grep("1037", ah2igg4$sampleID, invert=TRUE), ]

# Keep only those that are appropriately diluted (not over the max)
ah1igg4$check <- as.numeric(ah1igg4$max) - as.numeric(ah1igg4$conc)
plot(ah1igg4$check)

ah2igg4$check <- as.numeric(ah2igg4$max) - as.numeric(ah2igg4$conc)
plot(ah2igg4$check)

# Correct sample ID as above
ah1igg4$subject <- str_sub(ah1igg4$sampleID, 1, 6)
ah1igg4$visit <- ah1igg4$sampleID
ah1igg4$visit <- gsub("965-[[:digit:]]+", "", ah1igg4$visit)
ah1igg4$visit <- gsub("_", "", ah1igg4$visit)
ah1igg4$visit <- gsub("-", "", ah1igg4$visit)
ah1igg4$visit <- gsub(" ", "", ah1igg4$visit)
ah1igg4$visit[ah1igg4$visit=="BU2"] <- "BU02"
ah1igg4$visit[ah1igg4$visit=="BU4"] <- "BU04"
ah1igg4$visit[ah1igg4$visit=="FU1"] <- "FU01"
ah1igg4$visit[ah1igg4$visit=="FU2"] <- "FU02"
ah1igg4$visit[ah1igg4$visit=="FU3"] <- "FU03"
ah1igg4$visit[ah1igg4$visit=="FU4"] <- "FU04"
ah1igg4$visit[ah1igg4$visit=="MN1"] <- "MN01"
ah1igg4$visit[ah1igg4$visit=="MN2"] <- "MN02"
ah1igg4$sampleID <- paste(ah1igg4$subject, ah1igg4$visit, sep="_")

ah2igg4$subject <- str_sub(ah2igg4$sampleID, 1, 6)
ah2igg4$visit <- ah2igg4$sampleID
ah2igg4$visit <- gsub("965-[[:digit:]]+", "", ah2igg4$visit)
ah2igg4$visit <- gsub("_", "", ah2igg4$visit)
ah2igg4$visit <- gsub("-", "", ah2igg4$visit)
ah2igg4$visit <- gsub(" ", "", ah2igg4$visit)
ah2igg4$visit[ah2igg4$visit=="BU2"] <- "BU02"
ah2igg4$visit[ah2igg4$visit=="BU4"] <- "BU04"
ah2igg4$visit[ah2igg4$visit=="FU1"] <- "FU01"
ah2igg4$visit[ah2igg4$visit=="FU2"] <- "FU02"
ah2igg4$visit[ah2igg4$visit=="FU3"] <- "FU03"
ah2igg4$visit[ah2igg4$visit=="FU4"] <- "FU04"
ah2igg4$visit[ah2igg4$visit=="FU5"] <- "FU05"
ah2igg4$visit[ah2igg4$visit=="MN1"] <- "MN01"
ah2igg4$visit[ah2igg4$visit=="MN2"] <- "MN02"
ah2igg4$sampleID <- paste(ah2igg4$subject, ah2igg4$visit, sep="_")

# Merge all of the smaller datasets, on the sampleID
ah1igg4.small
ah2igg4.small

ah1igg4.small <- data.frame(ah1igg4$sampleID, ah1igg4$test, ah1igg4$conc, stringsAsFactors=F)
names(ah1igg4.small) <- c("sampleID", "test", "conc")

ah2igg4.small <- data.frame(ah2igg4$sampleID, ah2igg4$test, ah2igg4$conc, stringsAsFactors=F)
names(ah2igg4.small) <- c("sampleID", "test", "conc")

# I had to stringsAsFactors=F when making the dataframes, so rbind would work
shreffphad <- rbind(ige.small, ah1igg4.small, ah2igg4.small)

# Write this final phadia csv (it's in the long format, and so will have to change to short)
write.csv(shreffphad, "ShrefflerPhadia_long_08282015.csv")

# make into wide format, and save
shreffphad <- shreffphad[!duplicated(shreffphad), ]
# remove all of the 0 values
shreffphad <- subset(shreffphad, shreffphad$conc!=0)
shreffphad$conc <- as.numeric(shreffphad$conc)

# Making it wide is problematic, as there are some repeat values which are not 0
# this is going to have to be troubleshooted later. For now, I'm going to use the average
# Most of these look very similar, so that should be ok
shreffphad$row <- 1:nrow(shreffphad)
shreffphad.wide <- spread(shreffphad, test, conc)
names(shreffphad.wide)
# TO figure out, why there were duplicates, use ddply
shreffphad.wide$aIgE <- as.numeric(shreffphad.wide$aIgE)
shreffphad.wide$f13 <- as.numeric(shreffphad.wide$f13)
shreffphad.wide$f352 <- as.numeric(shreffphad.wide$f352)
shreffphad.wide$f422 <- as.numeric(shreffphad.wide$f422)
shreffphad.wide$f423 <- as.numeric(shreffphad.wide$f423)
shreffphad.wide$f424 <- as.numeric(shreffphad.wide$f424)
shreffphad.wide$f427 <- as.numeric(shreffphad.wide$f427)
shreffphad.wide$f422g <- as.numeric(shreffphad.wide$f422g)
shreffphad.wide$f423g <- as.numeric(shreffphad.wide$f423g)

shreffphadcheck <- ddply(shreffphad.wide, .(sampleID), summarise,
      aIgE = mean(aIgE, na.rm=TRUE),
      f13 = mean(f13, na.rm=TRUE),
      f352 = mean(f352, na.rm=TRUE),
      f422 = mean(f422, na.rm=TRUE),
      f423= mean(f423, na.rm=TRUE),
      f424=mean(f424, na.rm=TRUE),
      f427=mean(f427, na.rm=TRUE),
      f422g=mean(f422g, na.rm=TRUE),
      f423g=mean(f423g, na.rm=TRUE))

write.csv(shreffphadcheck, "ShrefflerPhadia_08282015.csv")

################################################################################
# Merge with redcap phadia data
################################################################################

all <- merge(redphad.small, shreffphadcheck, by="sampleID", all=TRUE)
write.csv(all, "Prelim_merged_phadia_08282015.csv")

# For this abstract's purpose, focus on aIgE, AH1 f422 and AH2 sIgE f423 and sIgG4
[1] "sampleID"   "peanut_f13" "f422.x"     "f423.x"     "f424.x"     "f352.x"    
[7] "f427.x"     "aIgE.x"     "f422g.x"    "f423g.x"    "aIgE.y"     "f13"       
[13] "f352.y"     "f422.y"     "f423.y"     "f424.y"     "f427.y"     "f422g.y"   
[19] "f423g.y"

unique(all$f422.x)
unique(all$f422.y)

check <- ddply(all, .(sampleID), summarise, f423=paste(f422.x, f422.y, collpase="_"))

# These don't work. Gave up and merged all results quickly by hand.
all$f422<-all$f422.x
for (i in 1:length(all$sampleID)) {
  if (all$f422.x[i]==">100" & all$f422.y[i]!="NA" & all$f422.y[i]!="NaN") {
    all$f422[i] = all$f422.y[i]}
  if (all$f422.x[i]=="NA") {all$f422[i] = all$f422.y[i]}
  }

for (i in 1:length(all$sampleID)) {
  if (all$f422.x[i]==">100") {print(all$sampleID[i])}
}



