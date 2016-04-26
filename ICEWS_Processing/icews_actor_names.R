library(lubridate)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)

setwd("ICEWS_Processing/processed_with_names/")
files <- list.files(pattern='reduced.ICEWS.events.*.txt')
icews <- do.call('rbind', lapply(files, function(x) read_delim(x, delim="\t", quote="\"", col_names = F, escape_double = F)))
rm(files)
setwd("~/Google Drive/Dissertation Data/networkcreation/")

colnames(icews) = c('date','location','country.src','country.tgt','name.src','name.tgt','iso.src','cow.src','agent.src','iso.tgt','cow.tgt','agent.tgt','cameo','goldstein','quad')

#reorder
icews <- icews[,c(1:5,7:9,6,10:15)]

#create monthly var
icews$ym <- floor_date(icews$date, "month")

#fix actor names
re <- "\\(([^()]+)\\)"
icews$alt.src <- gsub(re, "\\1", str_extract_all(icews$name.src, re))
icews$alt.src <- ifelse(icews$alt.src=="character0", paste(icews$name.src), paste(icews$alt.src))
icews$alt.src[icews$alt.src==icews$country.src] <- "Unattributed"
icews$alt.tgt <- gsub(re, "\\1", str_extract_all(icews$name.tgt, re))
icews$alt.tgt <- ifelse(icews$alt.tgt=="character0", paste(icews$name.tgt), paste(icews$alt.tgt))
icews$alt.tgt[icews$alt.tgt==icews$country.tgt] <- "Unattributed"

icews <- subset(icews, duplicated(subset(icews,select=c(date,alt.src,alt.tgt,cameo)))==F)
rm(re)

#### Code Individuals ####
actors <- read_csv("icews.actors.20140112.csv")

actors <- subset(actors,select=-Aliases)

colnames(actors) <- c("location","alt.src","actor.type","affiliation","stdate","edate")

actors$stdate[actors$stdate=="beginning of time"] <- "1995-01-01"
actors$edate[actors$edate=="end of time"] <- "2014-12-31"

actors$stdate <- ymd(actors$stdate)
actors$edate <- ymd(actors$edate)

actors <- subset(actors,actor.type=="Individual")
actors <- subset(actors,select=-actor.type)

#create generic var names to convert from long to wide
actors <- actors %>% 
  group_by(alt.src) %>% 
  mutate(num=1:length(alt.src))

#convert from long to wide format
names <- subset(actors, select=c(alt.src,affiliation,num))
names <- spread(names, num, affiliation)
colnames(names)[2:21] <- paste("group",colnames(names)[2:21],sep="")

st <- subset(actors, select=c(alt.src,stdate,num))
st <- spread(st, num, stdate)
colnames(st)[2:21] <- paste("stdate",colnames(st)[2:21],sep="")

end <- subset(actors, select=c(alt.src,edate,num))
end <- spread(end, num, edate)
colnames(end)[2:21] <- paste("edate",colnames(end)[2:21],sep="")

actors <- merge(names,st,by="alt.src")
actors <- merge(actors,end,by="alt.src")
rm(names,st,end)

##### Merge w/ ICEWs ####
#icews <- merge(icews,actors,all.x=T,all.y=F)

icews$id <- seq(1:length(icews$date))

slim <- subset(icews, select=c(id, date, alt.src, alt.tgt))

slim <- merge(slim,actors,all.x=T,all.y=F)

#### Code group names  for sources ####

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

#paste all remaining group names into a column
slim <- unite(slim, src.groups, c(group1,group2,group3,group4,group5,group6,group7,group8,group9,group10,group11,group12,group13,group14,group15,group16,group17,group18,group19,group20), remove=T, sep="; ")

slim$src.groups <- gsub("; NA", "", slim$src.groups)
slim$src.groups <- gsub("NA; ", "", slim$src.groups)

#### Code group names for targets ####
colnames(actors)[1] <- "alt.tgt"

slim <- merge(slim,actors,all.x=T,all.y=F)

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

#paste all remaining group names into a column
slim <- unite(slim, tgt.groups, c(group1,group2,group3,group4,group5,group6,group7,group8,group9,group10,group11,group12,group13,group14,group15,group16,group17,group18,group19,group20), remove=T, sep="; ")

slim$tgt.groups <- gsub("; NA", "", slim$tgt.groups)
slim$tgt.groups <- gsub("NA; ","",slim$tgt.groups)

#merge back into ICEWS
icews <- merge(icews,slim,all=T)
rm(slim,actors)

#write
write_csv(icews,"icews_groups.csv")