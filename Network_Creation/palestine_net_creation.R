#### Convert Palestine Event Data to Node & Edge Spells ####
library(dplyr)
library(lubridate)
library(zoo)

#function to count months between dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#### 1. Create node spells ####

#import node data
nodes <- read_csv("~/Dissertation Data/networkcreation/Network_Creation/nodes/palestine_nodes.csv")

#aggregate to monthly summaries
nodes <- nodes %>% 
  group_by(alt.src,ym) %>% 
  summarize(goldstein=mean(gov.gold))

#create lag month var
nodes <- nodes %>% 
  group_by(alt.src) %>% 
  mutate(last=lag(ym),rev.lag=lead(ym))

#code start of new spells
nodes$new <- ifelse(elapsed_months(nodes$ym, nodes$last)>18 | is.na(nodes$last)==T, 1, 0)

#code end of spells
nodes$end <- ifelse(elapsed_months(nodes$rev.lag,nodes$ym)>18 | is.na(nodes$rev.lag)==T, 1, 0)

#subset to months that were start or end of a spell
nodes <- subset(nodes, new==1 | end==1)
nodes <- subset(nodes, select=-c(last,rev.lag))

#get start dates and roll forward
nodes$start <- ifelse(nodes$new==1, paste(nodes$ym), NA)

nodes <- nodes %>% 
  group_by(alt.src) %>% 
  do(na.locf(.))

nodes$end <- ifelse(nodes$end==1, paste(nodes$ym), NA)

#subset to complete spell
nodes <- subset(nodes, !is.na(end)==T)

#create id nums for groups
nums <- data.frame(alt.src=unique(nodes$alt.src), id1=seq(1,length(unique(nodes$alt.src))))

nodes <- merge(nodes,nums)

#subset to needed vars
nodes <- subset(nodes, select=c(id1,start,end))

#### 2. Create Edge Spells ####