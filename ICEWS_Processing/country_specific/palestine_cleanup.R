##############################################
#### Create Palestinian Dissident Network ####
##############################################
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(zoo)
library(ggplot2)
theme_set(theme_bw())

#import icews w/ group names added for individuals
icews <- read.csv("~/Google Drive/Dissertation Data/networkcreation/icews_groups.csv", stringsAsFactors = FALSE)

#fix dates
icews$date <- ymd(icews$date)
icews$ym <- floor_date(icews$date,"month")

#subset to actors likely to be palestinian dissidents
pal.diss <- subset(icews, (cow.src==0 & iso.src=="PSE" & agent.src!="BUS" & (location=="Israel" | location=="Occupied Palestinian Territories")))

#### 1. Attach Src Individuals to Groups ####

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
pal.diss <- pal.diss %>% 
  mutate(src.groups=strsplit(src.groups,"; ")) %>% 
  unnest(src.groups)

#use group name instead of individual name when available
pal.diss$alt.src <- ifelse(is.na(pal.diss$src.groups)==T, pal.diss$alt.src, pal.diss$src.groups)

#remove extraneous/invalid groups
pal.diss <- subset(pal.diss, alt.src!="Palestinian Territory, Occupied" & alt.src!="c(\"Govt\", \"Palestinian Territory, Occupied\")" & alt.src!="Riyad Mansour" & alt.src!="Muhammad Abu Khdeir" & alt.src!="Finance / Economy / Commerce / Trade Ministry (Palestinian Territory, Occupied)" & alt.src!="Intelligence Ministry (Palestinian Territory, Occupied)" & alt.src!="Human Rights NGOs (Palestinian Territory, Occupied)")

#code a few individuals that weren't in the dictionary
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Yahya Ayyash"] <- "Izz ad-Din al-Qassam Brigades"
pal.diss$alt.src[pal.diss$alt.src=="Faisal Husseini"] <- "Palestine Liberation Organization"
pal.diss$alt.src[pal.diss$alt.src=="Popular Front for the Liberation of Palestine – General Command"] <- "Popular Front for the Liberation of Palestine"
pal.diss$alt.src[pal.diss$alt.src=="Majid Faraj"] <- "Palestinian Preventive Security Agency"
pal.diss$alt.src[pal.diss$alt.src=="Ziyad Abu-Ayn"] <- "Fatah; Palestine Liberation Organization"
pal.diss$alt.src[pal.diss$alt.src=="Ramazan Abdullah"] <- "Palestinian Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Ziyad al-Nakhalah"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Mushir al-Masri"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Izzat al-Rishq"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Fawzi Barhum"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Khalil al-Hayyah"] <- "Hamas"
pal.diss$alt.src[pal.diss$name.src=="Nabil Abu-Rudaynah"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Yasser Abed Rabbo"] <- "Palestine Liberation Organization; Palestine Democratic Union"
pal.diss$alt.src[pal.diss$name.src=="Yasir Abd-Rabbuh"] <- "Palestine Liberation Organization; Palestine Democratic Union"
pal.diss$alt.src[pal.diss$name.src=="Faisal Husseini"] <- "Palestine Liberation Organization"
pal.diss$alt.src[pal.diss$name.src=="Issa Qaraqe"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Nabil Shaath"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Ahmed Qureia"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Mahmoud Abbas"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.src[pal.diss$name.src=="Salam Fayyad" & pal.diss$date < "2005-12-01"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Salam Fayyad" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.src[pal.diss$name.src=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.src[pal.diss$name.src=="Rauhi Fattouh"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.src[pal.diss$name.src=="Rami Hamdallah"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Tayyib Abd-al-Rahim"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Riyad al-Malki"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Hanan Ashrawi" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.src[pal.diss$name.src=="Ghazi Al-Jabali"] <- "Palestinian Preventive Security Agency"
pal.diss$alt.src[pal.diss$alt.src=="Media (Hamas)"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Government (Hamas)"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Abdel Razak Al-Yahya"] <- "Fatah"

#split again
pal.diss <- pal.diss %>% 
  mutate(alt.src=strsplit(alt.src,"; ")) %>% 
  unnest(alt.src)

#### 2. ID which groups are dissidents - Nodes for Networks ####

group.months <- pal.diss

#id anti-gov events
group.months$gov.gold <- ifelse((group.months$cow.tgt == 666) , group.months$goldstein, NA)

#create monthly summaries of mean gold vs. gov
group.months <- group.months %>% 
  group_by(alt.src,ym) %>% 
  summarize(gov.gold = mean(gov.gold,na.rm = T))

#calculate time since last month w/ events
group.months <- group.months %>% 
  group_by(alt.src) %>% 
  mutate(last=lag(ym))

group.months$last <- ifelse(is.na(group.months$last)==T, paste(group.months$ym), paste(group.months$last))

group.months$last <- ymd(group.months$last)

#create end date + 18mos
group.months <- group.months %>% 
  group_by(alt.src) %>% 
  mutate(end=max(ym))

group.months$end <- group.months$end + month(18)

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

#get size of gap from last obs, drop if more than 18
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

group.months$gov.gold <- ifelse(elapsed_months(group.months$ym,group.months$last)>24, NA, group.months$gov.gold)
rm(elapsed_months)

#remove non-diss obs
group.months <- subset(group.months,gov.gold<0)


## Use frame as list of nodes ##
write.csv(group.months, "~/Google Drive/Dissertation Data/networkcreation/Network_Creation/nodes/palestine_nodes.csv")

## Use frame to remove events that occur while a group is out of dissident network ##

#create group-month id
group.months$conmo <- paste(group.months$alt.src, group.months$ym, sep="")

#subset
pal.diss$conmo <- paste(pal.diss$alt.src, pal.diss$ym, sep="")

pal.diss <- subset(pal.diss, conmo %in% group.months$conmo)


#### 3. Attach TGT invididuals to Groups ####

#give individuals with multiple groups an obs for each group
pal.diss <- pal.diss %>% 
  mutate(tgt.groups = strsplit(tgt.groups,"; ")) %>% 
  unnest(tgt.groups)

#use group instead of individual name when available
pal.diss$alt.tgt <- ifelse(is.na(pal.diss$tgt.groups) == T, pal.diss$alt.tgt, pal.diss$tgt.groups)


#code a few that the dictionary doesn't take care of
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Majid Faraj"] <- "Palestinian Preventive Security Agency"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Mohammed Deif"] <- "Izz ad-Din al-Qassam Brigades"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ahmed Qureia"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Mahmoud Abbas"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Salam Fayyad" & pal.diss$date < "2005-12-01"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Salam Fayyad" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$name.tgt=="Yasir Arafat"] <- "Palestine Liberation Organization; Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Rami Hamdallah"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Tayyib Abd-al-Rahim"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Nabil Abu-Rudaynah"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Riyad al-Malki"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Abdel Razak Al-Yahya"] <- "Fatah"

#unnest again
pal.diss <- pal.diss %>% 
  mutate(tgt.groups=strsplit(tgt.groups,"; ")) %>% 
  unnest(tgt.groups)

## remove events that don't occur during dissident period
pal.diss$conmo <- paste(pal.diss$alt.tgt, pal.diss$ym, sep="")

pal.diss <- subset(pal.diss, conmo %in% group.months$conmo)

#remove events w/ self
pal.diss <- subset(pal.diss, alt.src!=alt.tgt)

#write
write_csv(pal.diss,"~/Google Drive/Dissertation Data/networkcreation/Network_Creation/edges/palestine_edge_events.csv")

rm(group.months, pal.diss)
