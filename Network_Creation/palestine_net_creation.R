#### Convert Palestine Event Data to Node & Edge Spells ####
library(dplyr)
library(lubridate)
library(zoo)
library(readr)
library(stringr)
library(network)
library(networkDynamic)
library(ggplot2)
theme_set(theme_bw())

#function to count months between dates
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#### 1. Create node spells ####

#import node data
nodes <- read_csv("~/Google Drive/Dissertation Data/networkcreation/Network_Creation/nodes/palestine_nodes.csv")


#create id nums for groups
nums <- data.frame(alt.src=unique(nodes$alt.src), id1=seq(1,length(unique(nodes$alt.src))))

nodes <- merge(nodes,nums)

#subset to needed vars
nodes <- subset(nodes, select=c(id1,start,end))

#### 2. Get Months w/ Active Dyads ####

edges <- read_csv("~/Google Drive/Dissertation Data/networkcreation/Network_Creation/edges/palestine_edge_events.csv")

#add actor numbers for source
edges <- merge(edges,nums)

#add actor numbers for target
colnames(nums) <- c("alt.tgt","id2")
edges <- merge(edges,nums)

#create non-directed dyad number
edges$dyad <- ifelse(edges$id1 < edges$id2, paste(edges$id1,edges$id2,sep="-"), paste(edges$id2,edges$id1,sep="-"))

#aggregate to monthly summaries
edges <- edges %>%
  group_by(dyad,ym) %>% 
  summarize(gold=mean(goldstein))

## keep events fo 18mos
edges <- edges %>% 
  group_by(dyad) %>% 
  mutate(end=max(ym),last=lag(ym))

#fix lagged month
edges$last <- ifelse(is.na(edges$last)==T, paste(edges$ym), paste(edges$last))
edges$last <- ymd(edges$last)

#add 18 mos to last date
edges$end <- edges$end + months(18)

#reset to max date in data if it's exceeded
edges$end <- ifelse(edges$end > ymd("2014-09-01"), paste("2014-09-01"), paste(edges$end))
edges$end <- ymd(edges$end)

#generate frame of dates when dyad is active
frame <- edges %>% 
  group_by(dyad) %>% 
  summarize(start=min(ym),end=max(end)) %>% 
  rowwise() %>% 
  do(data.frame(dyad=.$dyad, ym=seq(.$start, .$end, by="1 month")))

edges <- merge(frame,edges,all=T)
rm(frame)
edges <- subset(edges,select=-end)

#roll previous goldstein score forward
edges <- edges %>% 
  group_by(dyad) %>% 
  do(na.locf(.))

#remove goldstein if it has been > 18 mos since last event
edges$gold <- ifelse(elapsed_months(edges$ym, edges$last)>18, NA, edges$gold)
edges$gold <- as.numeric(edges$gold)

#for now we won't do signed edges - just keep cooperation
edges <- subset(edges, gold>0)
edges <- subset(edges, select=-last)

#### 3. Convert to Edge Spells ####

edges$ym <- ymd(edges$ym)

#create lag and lead vars
edges <- edges %>%
  group_by(dyad) %>%
  mutate(lag=lag(ym),rev.lag=lead(ym))

#code start of new spells
edges$new <- ifelse(elapsed_months(edges$ym, edges$lag)>18 | is.na(edges$lag)==T, 1, 0)

#code end of spells
edges$end <- ifelse(elapsed_months(edges$rev.lag, edges$ym)>18 | is.na(edges$rev.lag)==T, 1, 0)

#subset to obs that are at start or end of spell
edges <- subset(edges, new==1 | end==1)
edges <- subset(edges, select=-c(lag,rev.lag))

#get start dates & roll forward
edges$start <- ifelse(edges$new==1, paste(edges$ym), NA)

edges <- edges %>%
  group_by(dyad) %>%
  do(na.locf(.))

#get endings
edges$end <- ifelse(edges$end==1, paste(edges$ym), NA)

#subset to complete spells
edges <- subset(edges, !is.na(end)==T)

#split dyad num into separate ids
edges <- edges %>% mutate(id1=str_split(dyad,"-"))
edges$id1 <- as.numeric(lapply(edges$id1, "[", 1))
edges <- edges %>% mutate(id2=str_split(dyad,"-"))
edges$id2 <- as.numeric(lapply(edges$id2, "[", 2))

#subset to needed vars
edges <- subset(edges,select=c(id1,id2,start,end,gold))

#### 4. Build Dynamic Network ####

#need numeric time columns
nodes$start <- ymd(nodes$start)
nodes$end <- ymd(nodes$end)
edges$start <- ymd(edges$start)
edges$end <- ymd(edges$end)

#make a dataframe with all dates in range, and identifier for each
dates <- data.frame(ym=seq.Date(ymd("1995-01-01"),max(edges$end),"months"))
dates$num <- seq(1:length(dates$ym))

#merge identifiers into node & edge data
colnames(dates) <- c("start","from")
nodes <- merge(nodes,dates,all.x=T,all.y=F)
edges <- merge(edges,dates,all.x=T,all.y=F)
colnames(dates) <- c("end","to")
nodes <- merge(nodes,dates,all.x=T,all.y=F)
edges <- merge(edges,dates,all.x=T,all.y=F)

#subset
nodes <- subset(nodes,select=c(from,to,id1))
edges <- subset(edges,select=c(from,to,id1,id2))

#create network object
pal <- networkDynamic(node.spells=nodes, edge.spells=edges, directed=F)

#teach it that time is in months
att <- get.network.attribute(pal, 'net.obs.period')
att$time.unit <- "months"
set.network.attribute(pal, 'net.obs.period', att)

#plot density over time
series <- get.networks(pal, start=0, end=237, time.increment = 1, rule='latest')
series.density <- sapply(series, network.density)
plot(series.density,type='l',xlab="Month",ylab="Network Density")

#plot networks
plot(network.extract(pal,onset=100,length=40,rule="any"),main="Palestinian Dissident Network")
