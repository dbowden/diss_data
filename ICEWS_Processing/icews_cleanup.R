# This file takes the output of text_to_cameo_mod.py, merges them, extracts specific actor names using regex, and merges in group names from the ICEWS dictionary file. It outputs a csv file with the complete ICEWS data ready for country-specific data cleaning.

# Load data and perform initial cleaning --------------------------------

#load packages
library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)

#collect the .txt files produced by text_to_cameo_mod.py (1 for every year), and merge into a single dataframe
files <- list.files(path = "post_python/",pattern='reduced.ICEWS.events.*.txt', full.names = T)
icews <- do.call('rbind', lapply(files, function(x) read_delim(x, delim="\t", quote="\"", col_names = F, escape_double = F)))
rm(files)

#add column names
colnames(icews) = c('date','location','country.src','country.tgt','name.src','name.tgt','iso.src','cow.src','agent.src','iso.tgt','cow.tgt','agent.tgt','cameo','goldstein','quad')

#reorder columns to make them a bit more readable
icews <- icews[,c(1:5,7:9,6,10:15)]

#create an identifier for each month
icews$ym <- floor_date(icews$date, "month")

#Subset to months that occur during or within WINDOW prior to conflict months using UCDP 25 death threshold -------------------------------------

#import ucdp armed conflict data v.4-2015. this is a set of all conflict-years.
ucdp <- read.csv("post_python/ucdp_acd.csv")

#

#Extract most specific actor name available (sometimes it is in parentheses) -----------------------------------------------------------------------

#create a regex to extract the text between two parentheses
re <- "\\(([^()]+)\\)"

#use the regex to to perform the extraction on the name variables
icews$alt.src <- gsub(re, "\\1", str_extract_all(icews$name.src, re))
icews$alt.tgt <- gsub(re, "\\1", str_extract_all(icews$name.tgt, re))

#if there are no parentheses, use the raw name rather than regex output
icews$alt.src <- ifelse(icews$alt.src=="character0", paste(icews$name.src), paste(icews$alt.src))
icews$alt.tgt <- ifelse(icews$alt.tgt=="character0", paste(icews$name.tgt), paste(icews$alt.tgt))

#if it is a generic name (i.e. Rebels (Afghanistan)), code as unattributed
icews$alt.src[icews$alt.src==icews$country.src] <- "Unattributed"
icews$alt.tgt[icews$alt.tgt==icews$country.tgt] <- "Unattributed"

#some events turn out to be duplicates once the names are fixed. I want to keep the unattribu
icews <- subset(icews, duplicated(subset(icews,select=c(date,alt.src,alt.tgt,cameo)))==F | alt.src=="Unattributed" | alt.tgt=="Unattributed")

rm(re)

#Add group names for individuals -----------------------------------------

#import the ICEWS actor dictionary, which has names/dates of groups with which individuals are associated
actors <- read_csv("post_python/icews.actors.20140112.csv")

#remove unneeded column and make column names match ICEWS
actors <- subset(actors,select=-Aliases)

colnames(actors) <- c("location","alt.src","actor.type","affiliation","stdate","edate")

#replace dataset beg/end placeholders w/ actual dates
actors$stdate[actors$stdate=="beginning of time"] <- "1995-01-01"
actors$edate[actors$edate=="end of time"] <- "2014-12-31"

#convert to date format
actors$stdate <- ymd(actors$stdate)
actors$edate <- ymd(actors$edate)

#only need info on individuals - groups already have all needed info
actors <- subset(actors,actor.type=="Individual")
actors <- subset(actors,select=-actor.type)

#create number for 1s, 2nd, nth group affiliation for each individual (some individuals have multiple groups, which are split across multiple rows)
actors <- actors %>%
  group_by(alt.src) %>%
  mutate(num=1:length(alt.src))

##convert to wide format - 1 obs per individual, with groups & dates spread across multiple columns. Easiest to do names and dates separately.

#subset to components
names <- subset(actors, select=c(alt.src,affiliation,num))
st <- subset(actors, select=c(alt.src,stdate,num))
end <- subset(actors, select=c(alt.src,edate,num))

#use tidyr to spread to wide format
names <- spread(names, num, affiliation)
st <- spread(st, num, stdate)
end <- spread(end, num, edate)

#make descriptive column names
colnames(names)[2:21] <- paste("group",colnames(names)[2:21],sep="")
colnames(st)[2:21] <- paste("stdate",colnames(st)[2:21],sep="")
colnames(end)[2:21] <- paste("edate",colnames(end)[2:21],sep="")

#merge
actors <- merge(names,st,by="alt.src")
actors <- merge(actors,end,by="alt.src")

rm(names,st,end)

#Merge dictionary data into ICEWS ----------------------------------------

#create unique id for each event
icews$id <- seq(1:length(icews$date))

#referencing against group dates is computationally intensive, so easiest to use a pared-down version of icews
slim <- subset(icews, select=c(id, date, alt.src, alt.tgt))

#merge dictionary info into slimmed-down icews
slim <- merge(slim,actors,all.x=T,all.y=F)

#Remove groups that were inactive during events & cleanup ----------------

##source actors

#remove names that weren't active on event date
slim$group1 <- ifelse(slim$date>=slim$stdate1 & slim$date<=slim$edate1, slim$group1, NA)
slim$group2 <- ifelse(slim$date>=slim$stdate2 & slim$date<=slim$edate2, slim$group2, NA)
slim$group3 <- ifelse(slim$date>=slim$stdate3 & slim$date<=slim$edate3, slim$group1, NA)
slim$group4 <- ifelse(slim$date>=slim$stdate4 & slim$date<=slim$edate4, slim$group4, NA)
slim$group5 <- ifelse(slim$date>=slim$stdate5 & slim$date<=slim$edate5, slim$group5, NA)
slim$group6 <- ifelse(slim$date>=slim$stdate6 & slim$date<=slim$edate6, slim$group6, NA)
slim$group7 <- ifelse(slim$date>=slim$stdate7 & slim$date<=slim$edate7, slim$group7, NA)
slim$group8 <- ifelse(slim$date>=slim$stdate8 & slim$date<=slim$edate8, slim$group8, NA)
slim$group9 <- ifelse(slim$date>=slim$stdate9 & slim$date<=slim$edate9, slim$group9, NA)
slim$group10 <- ifelse(slim$date>=slim$stdate10 & slim$date<=slim$edate10, slim$group10, NA)
slim$group11 <- ifelse(slim$date>=slim$stdate11 & slim$date<=slim$edate11, slim$group11, NA)
slim$group12 <- ifelse(slim$date>=slim$stdate12 & slim$date<=slim$edate12, slim$group12, NA)
slim$group13 <- ifelse(slim$date>=slim$stdate13 & slim$date<=slim$edate13, slim$group13, NA)
slim$group14 <- ifelse(slim$date>=slim$stdate14 & slim$date<=slim$edate14, slim$group14, NA)
slim$group15 <- ifelse(slim$date>=slim$stdate15 & slim$date<=slim$edate15, slim$group15, NA)
slim$group16 <- ifelse(slim$date>=slim$stdate16 & slim$date<=slim$edate16, slim$group16, NA)
slim$group17 <- ifelse(slim$date>=slim$stdate17 & slim$date<=slim$edate17, slim$group17, NA)
slim$group18 <- ifelse(slim$date>=slim$stdate18 & slim$date<=slim$edate18, slim$group18, NA)
slim$group19 <- ifelse(slim$date>=slim$stdate19 & slim$date<=slim$edate19, slim$group19, NA)
slim$group20 <- ifelse(slim$date>=slim$stdate20 & slim$date<=slim$edate20, slim$group20, NA)

#remove date columns
slim <- slim[,-grep("^stdate*",colnames(slim))]
slim <- slim[,-grep("^edate*",colnames(slim))]

#merge all group names into a single column
slim <- unite(slim, src.groups, c(group1,group2,group3,group4,group5,group6,group7,group8,group9,group10,group11,group12,group13,group14,group15,group16,group17,group18,group19,group20), remove=T, sep="; ")

#remove NAs
slim$src.groups <- gsub("; NA", "", slim$src.groups)
slim$src.groups <- gsub("NA; ", "", slim$src.groups)

##repeat for targets

colnames(actors)[1] <- "alt.tgt"

slim <- merge(slim,actors,all.x=T,all.y=F)

rm(actors)

#remove names that weren't active on event date
slim$group1 <- ifelse(slim$date>=slim$stdate1 & slim$date<=slim$edate1, slim$group1, NA)
slim$group2 <- ifelse(slim$date>=slim$stdate2 & slim$date<=slim$edate2, slim$group2, NA)
slim$group3 <- ifelse(slim$date>=slim$stdate3 & slim$date<=slim$edate3, slim$group1, NA)
slim$group4 <- ifelse(slim$date>=slim$stdate4 & slim$date<=slim$edate4, slim$group4, NA)
slim$group5 <- ifelse(slim$date>=slim$stdate5 & slim$date<=slim$edate5, slim$group5, NA)
slim$group6 <- ifelse(slim$date>=slim$stdate6 & slim$date<=slim$edate6, slim$group6, NA)
slim$group7 <- ifelse(slim$date>=slim$stdate7 & slim$date<=slim$edate7, slim$group7, NA)
slim$group8 <- ifelse(slim$date>=slim$stdate8 & slim$date<=slim$edate8, slim$group8, NA)
slim$group9 <- ifelse(slim$date>=slim$stdate9 & slim$date<=slim$edate9, slim$group9, NA)
slim$group10 <- ifelse(slim$date>=slim$stdate10 & slim$date<=slim$edate10, slim$group10, NA)
slim$group11 <- ifelse(slim$date>=slim$stdate11 & slim$date<=slim$edate11, slim$group11, NA)
slim$group12 <- ifelse(slim$date>=slim$stdate12 & slim$date<=slim$edate12, slim$group12, NA)
slim$group13 <- ifelse(slim$date>=slim$stdate13 & slim$date<=slim$edate13, slim$group13, NA)
slim$group14 <- ifelse(slim$date>=slim$stdate14 & slim$date<=slim$edate14, slim$group14, NA)
slim$group15 <- ifelse(slim$date>=slim$stdate15 & slim$date<=slim$edate15, slim$group15, NA)
slim$group16 <- ifelse(slim$date>=slim$stdate16 & slim$date<=slim$edate16, slim$group16, NA)
slim$group17 <- ifelse(slim$date>=slim$stdate17 & slim$date<=slim$edate17, slim$group17, NA)
slim$group18 <- ifelse(slim$date>=slim$stdate18 & slim$date<=slim$edate18, slim$group18, NA)
slim$group19 <- ifelse(slim$date>=slim$stdate19 & slim$date<=slim$edate19, slim$group19, NA)
slim$group20 <- ifelse(slim$date>=slim$stdate20 & slim$date<=slim$edate20, slim$group20, NA)

#remove date columns
slim <- slim[,-grep("^stdate*",colnames(slim))]
slim <- slim[,-grep("^edate*",colnames(slim))]

#merge all remaining group names into a single column
slim <- unite(slim, tgt.groups, c(group1,group2,group3,group4,group5,group6,group7,group8,group9,group10,group11,group12,group13,group14,group15,group16,group17,group18,group19,group20), remove=T, sep="; ")

#remove NAs
slim$tgt.groups <- gsub("; NA", "", slim$tgt.groups)
slim$tgt.groups <- gsub("NA; ","",slim$tgt.groups)

#merge slimmed version back into full ICEWS
icews <- merge(icews,slim,all=T)

rm(slim,actors)

#write csv for use later on ----------------------------------------------

write_csv(icews,"icews_with_groups.csv")