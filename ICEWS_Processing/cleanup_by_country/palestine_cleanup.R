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
icews <- read.csv("ICEWS_Processing/icews_with_groups.csv", stringsAsFactors = FALSE)

#fix dates
icews$date <- ymd(icews$date)
icews$ym <- floor_date(icews$date,"month")

#subset to actors likely to be palestinian dissidents
pal.diss <- subset(icews, ((iso.src=="PSE" & (location=="Israel" | location=="Occupied Palestinian Territories")) | name.src=="Islamic Movement in Israel"))

# Define episodes -----------------------------
pal.diss$wave <- vector(mode = "numeric", length = nrow(pal.diss))

pal.diss$wave[pal.diss$date < "2000-09-28"] <- 0
pal.diss$wave[pal.diss$date > "2000-09-27" & pal.diss$date < "2004-11-11"] <- 1
pal.diss$wave[pal.diss$date > "2004-11-10" & pal.diss$date < "2006-01-25"] <- 2
pal.diss$wave[pal.diss$date > "2006-01-24" & pal.diss$date < "2007-06-01"] <- 3
pal.diss$wave[pal.diss$date > "2007-06-01"] <- 4

#Attach Src Individuals to Groups -------------------------------------

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
pal.diss <- separate_rows(pal.diss, src.groups, sep = "; ")

#use group name instead of individual name when available
pal.diss$alt.src <- ifelse(is.na(pal.diss$src.groups)==T, pal.diss$alt.src, pal.diss$src.groups)

#remove invalid and non-dissident groups
pal.diss <- subset(pal.diss, alt.src!="Muhammad Abu Khdeir") #child who was kidnapped and murdered
pal.diss <- subset(pal.diss, alt.src!="Riyad Mansour") #observer to UN

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
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Finance / Economy / Commerce / Trade Ministry (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)
pal.diss$alt.src <- ifelse(pal.diss$alt.src=="Intelligence Ministry (Palestinian Territory, Occupied)", pal.diss$name.src, pal.diss$alt.src)


#standardize group names
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Territory, Occupied"] <- "Unattributed"
pal.diss$alt.src[pal.diss$alt.src=="Government (Hamas)"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Popular Front for the Liberation of Palestine – General Command"] <- "Popular Front for the Liberation of Palestine"

#these are de facto connected, so I'll treat them as same node
pal.diss$alt.src[pal.diss$alt.src=="Palestine Liberation Organization"] <- "Fatah" #PLO has other factions, but most of those are identified as such
pal.diss$alt.src[pal.diss$alt.src=="Voice of Palestine"] <- "Fatah" #radio station controlled by Palestinian Authority
pal.diss$alt.src[pal.diss$alt.src=="Izz ad-Din al-Qassam Brigades"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Preventive Security Agency"] <- "Fatah" #controlled by PA
pal.diss$alt.src[pal.diss$alt.src=="WAFA"] <- "Fatah" #news agency of PA
pal.diss$alt.src[pal.diss$alt.src=="Tanzim"] <- "Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Legislative Council" & pal.diss$date < "2006-01-27"] <- "Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Palestinian Legislative Council" & pal.diss$date > "2006-01-26"] <- "Hamas"


#code individuals that the dictionary didn't get
pal.diss$alt.src[pal.diss$alt.src=="c(\"Govt\", \"Palestinian Territory, Occupied\")"] <- "Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Yasir Arafat"] <- "Fatah"
pal.diss$alt.src[pal.diss$alt.src=="Faisal Husseini"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Nabil Abu-Rudaynah"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Nabil Shaath"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Ahmed Qureia"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Mahmoud Abbas"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Tayyib Abd-al-Rahim"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Hanan Ashrawi" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.src[pal.diss$name.src=="Hanan Ashrawi" & pal.diss$date < "2005-12-02"] <- "Fatah"
pal.diss$alt.src[pal.diss$name.src=="Yasser Abed Rabbo"] <- "Palestine Democratic Union"
pal.diss$alt.src[pal.diss$name.src=="Yasir Abd-Rabbuh"] <- "Palestine Democratic Union"
pal.diss$alt.src[pal.diss$name.src=="Ghazi Al-Jabali"] <- "Fatah"
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
pal.diss$alt.src[pal.diss$alt.src=="Shukri Bishara"] <- "Fatah" #finance min
pal.diss$alt.src[pal.diss$alt.src=="Tawfik Tirawi"] <- "Fatah" #PA intelligence
pal.diss$alt.src[pal.diss$alt.src=="Issa Qaraqe"] <- "Fatah" #Min of Prisoner Affairs
pal.diss$alt.src[pal.diss$alt.src=="Majid Faraj"] <- "Fatah" #PA intelligence
pal.diss$alt.src[pal.diss$alt.src=="Ziyad Abu-Ayn"] <- "Fatah" #minister
pal.diss$alt.src[pal.diss$alt.src=="Media (Hamas)"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Rami Hamdallah"] <- "Fatah" #PM
pal.diss$alt.src[pal.diss$alt.src=="Ramazan Abdullah"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Ziyad al-Nakhalah"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Moataz Hejazi"] <- "Islamic Jihad"
pal.diss$alt.src[pal.diss$alt.src=="Mahmud al-Zahhar"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Saeb Erekat"] <- "Fatah" #Oslo negotiator
pal.diss$alt.src[pal.diss$alt.src=="Sami Abu-Zuhri"] <- "Hamas" #spokesman
pal.diss$alt.src[pal.diss$alt.src=="Musa Abu-Marzuq"] <- "Hamas"
pal.diss$alt.src[pal.diss$alt.src=="Jibril Rajoub"] <- "Fatah"


#remove unidentified groups
pal.diss <- subset(pal.diss, alt.src!="Unattributed")


# get min and max goldstein scores to exclude foreign actors with no material events ------------------------------------------------------
# pal.diss <- pal.diss %>% 
#   group_by(alt.src) %>% 
#   mutate(min.gold = min(goldstein), max.gold = max(goldstein))
# 
# pal.diss <- subset(pal.diss, iso.src=="PSE" | min.gold < -5 | max.gold > 5)

# get anti-gov events to use as DV, remove pro-gov foreign actors ------------------------------

pal.diss$gov.gold <- ifelse(pal.diss$cow.tgt==666 & pal.diss$agent.tgt=="GOV", pal.diss$goldstein, NA)
pal.diss$isr.gold <- ifelse(pal.diss$cow.tgt == 666, pal.diss$goldstein, NA)

pal.diss <- pal.diss %>% 
  group_by(alt.src) %>% 
  mutate(median.gov.gold = median(gov.gold,na.rm=T), median.isr.gold = median(isr.gold, na.rm = T))

#pal.diss <- subset(pal.diss, iso.src=="PSE" | median.gov.gold < 0)

# create group numbers
nums <- data.frame(alt.src=unique(pal.diss$alt.src))
nums$srcnum <- seq(1:length(nums$alt.src))

pal.diss <- merge(pal.diss, nums)

#keep anti-gov events
dv <- subset(pal.diss, isr.gold < 0)

write_csv(dv,"Network_Creation/dv/palestine_dv.csv")
rm(dv)

# Get network composition by year ---------------------

nodes <- pal.diss %>% 
  group_by(srcnum) %>% 
  summarize(start=min(wave),end=max(wave))

write.csv(nodes, "Network_Creation/nodes/palestine_nodes.csv", row.names = F)
rm(nodes)

#Attach TGT invididuals to Groups --------------------------------------

#split obs w/ multiple groups into multiple lines (i.e. if an individual is in multiple groups an event is created for each of them)
pal.diss <- separate_rows(pal.diss, tgt.groups, sep = "; ")

#use group instead of individual name when available
pal.diss$alt.tgt <- ifelse(is.na(pal.diss$tgt.groups) == T, pal.diss$alt.tgt, pal.diss$tgt.groups)

#in some cases the group names from dictionary aren't helpful, so go back to individual name
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Human Rights NGOs (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Government (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Cabinet (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Elite (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Executive Office (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Information / Communication / Transparency Ministry (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Women / Children / Social / Welfare / Development / Religion Ministry (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Military (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)
pal.diss$alt.tgt <- ifelse(pal.diss$alt.tgt=="Foreign Ministry (Palestinian Territory, Occupied)", pal.diss$name.tgt, pal.diss$alt.tgt)


#standardize group names
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Territory, Occupied"] <- "Unattributed"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Government (Hamas)"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Islamic Jihad"] <- "Islamic Jihad"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Popular Front for the Liberation of Palestine – General Command"] <- "Popular Front for the Liberation of Palestine"

#these are de facto connected, so I'll treat them as same node
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestine Liberation Organization"] <- "Fatah" #PLO has other factions, but most of those are identified as such
pal.diss$alt.tgt[pal.diss$alt.tgt=="Voice of Palestine"] <- "Fatah" #radio station controlled by Palestinian Authority
pal.diss$alt.tgt[pal.diss$alt.tgt=="Izz ad-Din al-Qassam Brigades"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Preventive Security Agency"] <- "Fatah" #controlled by PA
pal.diss$alt.tgt[pal.diss$alt.tgt=="WAFA"] <- "Fatah" #news agency of PA
pal.diss$alt.tgt[pal.diss$alt.tgt=="Tanzim"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Legislative Council" & pal.diss$date < "2006-01-27"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Palestinian Legislative Council" & pal.diss$date > "2006-01-26"] <- "Hamas"


#code individuals that the dictionary didn't get
pal.diss$alt.tgt[pal.diss$alt.tgt=="c(\"Govt\", \"Palestinian Territory, Occupied\")"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Yasir Arafat"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Faisal Husseini"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Nabil Abu-Rudaynah"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Nabil Shaath"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ahmed Qureia"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Mahmoud Abbas"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Tayyib Abd-al-Rahim"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Hanan Ashrawi" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.tgt[pal.diss$name.tgt=="Hanan Ashrawi" & pal.diss$date < "2005-12-02"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Yasser Abed Rabbo"] <- "Palestine Democratic Union"
pal.diss$alt.tgt[pal.diss$name.tgt=="Yasir Abd-Rabbuh"] <- "Palestine Democratic Union"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ghazi Al-Jabali"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Salam Fayyad" & pal.diss$date < "2005-12-01"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Salam Fayyad" & pal.diss$date > "2005-12-01"] <- "Third Way"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Abdel Razak Al-Yahya"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$name.tgt=="Ismail Haniya"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$name.tgt=="Riyad al-Malki"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Mushir al-Masri"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Izzat al-Rishq"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Fawzi Barhum"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Khalil al-Hayyah"] <- "Hamas"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Rauhi Fattouh"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Rami Hamdallah"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Majid Faraj"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Issa Qaraqe"] <- "Fatah"
pal.diss$alt.tgt[pal.diss$alt.tgt=="Mohammed Deif"] <- "Hamas" #izz ad din commander
pal.diss$alt.tgt[pal.diss$alt.tgt=="Al-Manar"] <- "Hamas" # tv station



## remove events that where target isn't dissident
pal.diss <- subset(pal.diss, alt.tgt %in% alt.src)

#remove events w/ self
pal.diss <- subset(pal.diss, alt.src!=alt.tgt)

#remove events w/ unattributed
pal.diss <- subset(pal.diss, alt.src!="Unattributed" & alt.tgt!="Unattributed")

#remove duplicate events
pal.diss <- subset(pal.diss, duplicated(subset(pal.diss,select=c(alt.tgt,alt.src,date,cameo)))==F)

# Aggregate events by wave ----------------

# merge numbers
colnames(nums) <- c("alt.tgt","tgtnum")

pal.diss <- merge(pal.diss, nums, all.x=T, all.y=F, by="alt.tgt")


#dyadid
pal.diss$dirdyad <- paste(pal.diss$srcnum, pal.diss$tgtnum, sep="-")

# summarize
pal.diss <- pal.diss %>% 
  group_by(dirdyad, wave) %>% 
  summarize(srcnum = first(srcnum), tgtnum = first(tgtnum), mean.gold = mean(goldstein))

pal.diss <- subset(pal.diss, mean.gold > 0)


#write
write_csv(pal.diss,"Network_Creation/edges/palestine_edge_events.csv")
rm(pal.diss)

# save key num for ids -----------------------------------------

nums$abbrev <- c("IJ", "Fatah", "Hamas", "PFLP", "IMiI", "Al-Quds", "DFLP", "PDU", "al-Aqsa", "3rd Way", "MSCEJ", "AoI")

nums <- write_csv(nums, "Network_Creation/ids/palestine_ids.csv")
rm(nums)
