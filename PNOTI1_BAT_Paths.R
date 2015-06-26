#Script to generate .csv of file paths for BAT data that can be uploaded to the PNOIT1 RedCap. Also generates a .csv of abnormal BAT file paths.

rm(list=ls())

#Here are the directories that you need to change to run on your computer
remote <-"Z:/ShreffLabRemote/0965_zipped"
#path and name for ready-to-go output
ready <- "C:/Users/cew27/Dropbox (Personal)/R/965_bat_paths_for_redcap.csv"
#path and file name for output that needs manual attention
manual<-"C:/Users/cew27/Dropbox (Personal)/R/965_bat_paths_manual_check_required.csv"
#wd path may need modified slightly for your computer
setwd(remote)

#list file paths and get rid of extraneous files
buhl_filename <- list.files()
buhl_filename<- paste("Z:/ShreffLabRemote/0965_zipped/", buhl_filename, sep="")
bad.list <- list.files("Z:/ShreffLabRemote/0965_zipped/bad")
bad.list <- paste("Z:/ShreffLabRemote/0965_zipped/bad/", bad.list, sep="")
buhl_filename<- c(buhl_filename, bad.list)

exclude <- grepl(".csv", buhl_filename)
buhl_filename <- buhl_filename[!exclude]
exclude <- grepl(".rtf", buhl_filename)
buhl_filename <- buhl_filename[!exclude]
exclude <- grepl(".png", buhl_filename)
buhl_filename <- buhl_filename[!exclude]
exclude <- grepl("Bad", buhl_filename)
buhl_filename <- buhl_filename[!exclude]
exclude <- grepl("db", buhl_filename)
buhl_filename <- buhl_filename[!exclude]
bat <- as.data.frame(buhl_filename)

#get the subject number
bat$study_id <- bat$buhl_filename
bat$study_id <- sub(".*(965-..).*", "\\1", bat$study_id)

#get the visit number
bat$redcap_event_name <- bat$buhl_filename
bat$redcap_event_name <- sub(".*(BL|OFC|DBFC).*", "\\1", bat$redcap_event_name)
bat$redcap_event_name <- sub(".*((BU|FU|MA|MN)[0-9][0-9]?).*", "\\1", bat$redcap_event_name)
bat$redcap_event_name <- sub("MN", "MA", bat$redcap_event_name)
bat$redcap_event_name <- sub("(BU|FU|MA)([0-9])", "\\1 \\2", bat$redcap_event_name)
bat$redcap_event_name <- sub(" ([0-9]$)", " 0\\1", bat$redcap_event_name)

#put things in the order redcap likes
bat <- bat[,c(2,3,1)]

#We want two .csv files. One with ready-to-upload file paths, and one with the file paths that require special human attention.
#First the ones without study ids or visit numbers
nameless <- grep("Z:", bat$study_id)
weirdbat <- bat[nameless,]
bat <- bat[-nameless,]
visitless <- grep("Z:", bat$redcap_event_name)
weirdbat<- rbind(weirdbat, bat[visitless,])
bat<- bat[-visitless,]

#Next up are the followups for the control arm
bat$sample <- paste(bat$study_id, bat$redcap_event_name, sep="_")
fu <- grep("(965-04|965-09|965-10|965-11|965-15)_FU", bat$sample)
bat$sample <- NULL
weirdbat<- rbind(weirdbat, bat[fu,])
bat <- bat[-fu,]

#now to check for duplicates
bat <-bat[order(-grepl("zip$", bat$buhl_filename)),]
repeated <- duplicated(bat[c("study_id", "redcap_event_name")])|duplicated(bat[c("study_id", "redcap_event_name")], fromLast=T)
dups <- bat[repeated,]
dups<- dups[order(dups$study_id,dups$redcap_event_name),]

#take out files that are just unzipped folders, test for files that are still duplicated, and put file those under 'weird'
##BEFORE DOING THIS, take a quick peek at the dups data frame to make sure that there are no duplicates that are both unzipped
dups<- dups[grepl("zip$", dups$buhl_filename),]
bat<- bat[!repeated,]

weirdzip <- duplicated(dups[c("study_id", "redcap_event_name")])|duplicated(dups[c("study_id", "redcap_event_name")], fromLast=T)
weirdbat<- rbind(weirdbat, dups[weirdzip,])
dups<- dups[!weirdzip,]
bat<- rbind(bat, dups)

#Now it's time to print out our two data frames!
#change filename as needed to print to your computer
write.csv(bat, file=ready)
write.csv(weirdbat, file=manual)


