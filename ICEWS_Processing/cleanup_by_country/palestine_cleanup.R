# This file cleans the ICEWS data to isolate events between Palestinian dissidents during months that occur during a conflict (or within the chosen window prior to the conflict). It outputs two files to the network_creation directory: one with lists of dates where the nodes are active, and one with the events used to compute edges.

#Import packages and data ----------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(zoo)
library(ggplot2)
theme_set(theme_bw())

#import icews w/ group names added for individuals
icews <- read.csv("../ICEWS_Processing/icews_with_groups.csv", stringsAsFactors = FALSE)

#fix dates
icews$date <- ymd(icews$date)
icews$ym <- floor_date(icews$date,"month")

#subset to actors likely to be palestinian dissidents
pal.diss <- subset(icews, ((iso.src=="PSE" & (location=="Israel" | location=="Occupied Palestinian Territories")) | name.src=="Islamic Movement in Israel"))

#Attach Src Individuals to Groups ---------------------------------------

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
pal.diss <- pal.diss %>%
  mutate(src.groups=strsplit(src.groups,"; ")) %>%
  unnest(src.groups)

#use group name instead of individual name when available
pal.diss$alt.src <- ifelse(is.na(pal.diss$src.groups)==T, pal.diss$alt.src, pal.diss$src.groups)

#remove invalid and non-dissident groups
pal.diss <- subset(pal.diss, alt.src!="c(\"Govt\", \"Palestinian Territory, Occupied\")")
pal.diss <- subset(pal.diss, alt.src!="Muhammad Abu Khdeir") #child who was kidnapped and murdered

#in some cases the group names from dictionary aren't helpful, so go back to individual name
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Human Rights NGOs (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Government (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Cabinet (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Elite (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Executive Office (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Information / Communication / Transparency Ministry (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Women / Children / Social / Welfare / Development / Religion Ministry (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Military (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Foreign Ministry (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)

#standardize group names
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Territory, Occupied"] <- "Unattributed"
pal.diss$alt.src[pal.diss$alt.src=="Government (Hamas)"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Popular Front for the Liberation of Palestine – General Command"] <- "Popular Front for the Liberation of Palestine"

#code individuals that the dictionary didn't get
pal.diss$alt.src[pal.diss$alt.src=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Faisal Husseini"] <- "Palestine Liberation Organization"
pal.diss$alt.src[pal.diss$name.src=="Nabil Abu-Rudaynah"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Nabil Shaath"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Ahmed Qureia"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Mahmoud Abbas"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.src[pal.diss$name.src=="Tayyib Abd-al-Rahim"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Hanan Ashrawi" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.src[pal.diss$name.src=="Hanan Ashrawi" & pal.diss$date < "2005-12-02"] <- "Palestinian Legislative Council"
pal.diss$alt.src[pal.diss$name.src=="Yasser Abed Rabbo"] <- "Palestine Liberation Organization; Palestine Democratic Union"
pal.diss$alt.src[pal.diss$name.src=="Yasir Abd-Rabbuh"] <- "Palestine Liberation Organization; Palestine Democratic Union"
pal.diss$alt.src[pal.diss$name.src=="Ghazi Al-Jabali"] <- "Palestinian Preventive Security Agency"
pal.diss$alt.src[pal.diss$name.src=="Salam Fayyad" & pal.diss$date < "2005-12-01"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Salam Fayyad" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.src[pal.diss$alt.src=="Abdel Razak Al-Yahya"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.src[pal.diss$name.src=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.src[pal.diss$name.src=="Riyad al-Malki"] <- "Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Mushir al-Masri"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Izzat al-Rishq"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Fawzi Barhum"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Khalil al-Hayyah"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Rauhi Fattouh"] <- "Fatah"

#split again
pal.diss <- pal.diss %>%
  mutate(alt.src=strsplit(alt.src,"; ")) %>%
  unnest(alt.src)

#ID which groups are dissidents, save list of node dates -----------------

group.months <- pal.diss

#id anti-gov events (all Israeli targets will be used - often it is civilians)
group.months$gov.gold <- ifelse((group.months$cow.tgt == 666) , group.months$goldstein, NA)

#create monthly summaries of mean gold vs. Israel
group.months <- group.months %>%
  group_by(alt.src,ym) %>%
  summarize(gov.gold = mean(gov.gold,na.rm = T))

#calculate time since last month w/ events - scores are kept for the duration of the WINDOW variable
group.months <- group.months %>%
  group_by(alt.src) %>%
  mutate(last=lag(ym))

#for the first event for a group there is no lagged value
group.months$last <- ifelse(is.na(group.months$last)==T, paste(group.months$ym), paste(group.months$last))

group.months$last <- ymd(group.months$last)

#create end date + WINDOW
group.months <- group.months %>%
  group_by(alt.src) %>%
  mutate(end=max(ym))

group.months$end <- group.months$end + WINDOW

#fix end dates that fall beyond end of sample
group.months$end <- ifelse(group.months$end > ymd("2014-09-01"), paste("2014-09-01"), paste(group.months$end))

group.months$end <- ymd(group.months$end)

#create empty frame of all months while a group is active
frame <- group.months %>%
  group_by(alt.src) %>%
  summarize(start=min(ym),end=max(end)) %>%
  rowwise() %>%
  do(data.frame(alt.src=.$alt.src, ym=seq(.$start, .$end, by="1 month")))

#merge
group.months <- merge(frame,group.months,all=T)
rm(frame)

#roll last obs forward
group.months <- group.months %>%
  group_by(alt.src) %>%
  do(na.locf(.))

group.months$gov.gold <- as.numeric(group.months$gov.gold)

#fix dates
group.months$ym <- ymd(group.months$ym)
group.months$last <- ymd(group.months$last)

#drop observations if there hasn't been a new antigov event in longer than the duration of WINDOW
group.months$gov.gold <- ifelse(as.period(group.months$ym - group.months$last) > WINDOW, NA, group.months$gov.gold)

#remove non-diss obs (cooperative)
group.months <- subset(group.months,gov.gold<0)

## Use frame as list of nodes ##
write.csv(group.months, "../Network_Creation/nodes/palestine_nodes.csv")

## Use frame to remove events that occur while a group is out of dissident network

#create group-month id
group.months$conmo <- paste(group.months$alt.src, group.months$ym, sep="")

#do the same for pal.diss, and subset
pal.diss$conmo <- paste(pal.diss$alt.src, pal.diss$ym, sep="")

pal.diss <- subset(pal.diss, conmo %in% group.months$conmo)


#Attach TGT invididuals to Groups ---------------------------------------

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
pal.diss <- pal.diss %>%
  mutate(tgt.groups = strsplit(tgt.groups,"; ")) %>%
  unnest(tgt.groups)

#use group instead of individual name when available
pal.diss$alt.tgt <- ifelse(is.na(pal.diss$tgt.groups) == T, pal.diss$alt.tgt, pal.diss$tgt.groups)

#switch back to individual names when the dictionary groups aren't useful
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Executive Office (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Cabinet (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Information / Communication / Transparency Ministry (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Government (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Elite (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Foreign Ministry (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Human Rights NGOs (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)

#standardize group names
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Territory, Occupied"] <- "Unattributed"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Popular Front for the Liberation of Palestine – General Command"] <- "Popular Front for the Liberation of Palestine"

#code a few that the dictionary doesn't take care of
pal.diss$alt.tgt[pal.diss$alt.tgt=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Mahmoud Abbas"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ahmed Qureia"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$name.tgt=="Salam Fayyad" & pal.diss$date < "2005-12-01"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Salam Fayyad" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.tgt[pal.diss$name.tgt=="Tayyib Abd-al-Rahim"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Majid Faraj"] <- "Palestinian Preventive Security Agency"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Mohammed Deif"] <- "Izz ad-Din al-Qassam Brigades"
pal.diss$alt.tgt[pal.diss$name.tgt=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Rami Hamdallah"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Nabil Abu-Rudaynah"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Riyad al-Malki"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Abdel Razak Al-Yahya"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Yasser Abed Rabbo"] <- "Palestine Liberation Organization; Palestine Democratic Union"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Faisal Husseini"] <- "Palestine Liberation Organization"
pal.diss$alt.tgt[pal.diss$name.tgt=="Nabil Shaath"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Hanan Ashrawi" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.tgt[pal.diss$name.tgt=="Hanan Ashrawi" & pal.diss$date < "2005-12-02"] <- "Palestinian Legislative Council"
pal.diss$alt.tgt[pal.diss$name.tgt=="Riyad al-Malki"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Nabil Abu-Rudaynah"] <- "Fatah"

#unnest again
pal.diss <- pal.diss %>%
  mutate(tgt.groups=strsplit(tgt.groups,"; ")) %>%
  unnest(tgt.groups)

## remove events that don't occur during dissident period
pal.diss$conmo <- paste(pal.diss$alt.tgt, pal.diss$ym, sep="")

pal.diss <- subset(pal.diss, conmo %in% group.months$conmo)

#remove events w/ self
pal.diss <- subset(pal.diss, alt.src!=alt.tgt)

#remove events w/ unattributed
pal.diss <- subset(pal.diss, alt.src!="Unattributed" & alt.tgt!="Unattributed")

#remove duplicate events
pal.diss <- subset(pal.diss, duplicated(subset(pal.diss,select=c(alt.tgt,alt.src,date,cameo)))==F)

#write
write_csv(pal.diss,"../../Network_Creation/edges/palestine_edge_events.csv")

rm(group.months, pal.diss)
