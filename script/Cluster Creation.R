rm(list=ls())
setwd("~/Documents/Laurea Magistrale/Tesi/datasets")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/number.RData")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/cluster.RData")
library(dbscan)


#Change format of y so that it makes more sense 
data_location_adjusted$y <- 5000 - data_location_adjusted$y
data_location$y <- 800 - data_location$y




par(mfrow=c(1,1))
foto <- 98
index <- which(data_location_adjusted$ID==foto)

plot1 <- ggplot(data_location[index,], aes(x=x-300, y=y)) + geom_point(size=1.5,fill="mediumpurple2", colour="black", pch=23) + ylim(0, 800) + xlim(0, 1300) +theme(axis.title.x = element_text(size=10, face="bold"), axis.title.y = element_text(size=10, face="bold"), plot.title=element_text(size=15, face="bold")) + xlab("X") + ylab("Y") +ggtitle("Original Coordinates")
plot2 <- ggplot(data_location_adjusted[index,], aes(x=x, y=y)) + geom_point(size=1.5,, fill="mediumpurple2", colour="black", pch=23) +  ylim(0, 5000) + xlim(0, 1300)  +theme(axis.title.x = element_text(size=10, face="bold"), axis.title.y = element_text(size=10, face="bold"),plot.title=element_text(size=15, face="bold")) + xlab("X") + ylab("Y") + ggtitle("Projected Coordinates")
grid.arrange(plot1, plot2, ncol=2)

plot(data_location[index,4:5], xlim=c(0,2100), ylim=c(0,850), cex=1)
plot(data_location_adjusted[index,4:5], xlim=c(0,1350), ylim=c(0,5000), cex=1)


im <- load.image("/Users/ian/Desktop/test.png")

ggplot(data_location_adjusted[index,], aes(x=x, y=y)) + background_image(im) + geom_point(size=2.5,, fill="green", colour="black", pch=23) +  ylim(200, 5000) + xlim(-100, 1300)  +theme(axis.title.x = element_text(size=10, face="bold"), axis.title.y = element_text(size=10, face="bold"),plot.title=element_text(size=15, face="bold")) + xlab("X") + ylab("Y")


#Create clusters using dbscan, distance 1.40 metres of radius
number_clusters <- rep(0,1421)
clusters <- list()

for (i in 1:1421){
  index <- which(data_location_adjusted$ID==i)
  clusters[[i]] <- dbscan(data_location_adjusted[index,4:5], eps=140, minPts = 1)
  number_clusters[i] <- length(unique(clusters[[i]]$cluster))
}


par(mfrow=c(1,1))
foto <- 175
index <- which(data_location_adjusted$ID==foto)
par(mfrow=c(1,3))
plot(data_location[index,4:5], xlim=c(0,2100), ylim=c(0,850), cex=1)
plot(data_location_adjusted[index,4:5], xlim=c(0,1350), ylim=c(0,5000), cex=1)
plot(data_location_adjusted[index,4:5], xlim=c(0,1350), ylim=c(0,5000), col=clusters[[foto]]$cluster, lwd=1, cex=1, pch=clusters[[foto]]$cluster)



ggplot(data_location_adjusted[index,], aes(x=x, y=y)) + geom_point(shape=clusters[[foto]]$cluster, color=clusters[[foto]]$cluster, fill=clusters[[foto]]$cluster,size=4.5) +  ylim(0, 5000) + xlim(0, 1300)  +theme(axis.title.x = element_text(size=10, face="bold"), axis.title.y = element_text(size=10, face="bold"),plot.title=element_text(size=15, face="bold")) + xlab("X") + ylab("Y") + ggtitle("Clusters of people") + geom_point(colour = clusters[[foto]]$cluster , size = 2)



par(mfrow=c(3,2))
plot.ts(morning$number)
plot.ts(morning_cluster$number)
plot.ts(noon$number)
plot.ts(noon_cluster$number)
plot.ts(evening$number)
plot.ts(evening_cluster$number)






#Create dataset using same code used for number dataset
data_cluster <- data_number
data_cluster[,"number"] <- number_clusters


#Divide in Morning, Noon and Evening
morning <- data_cluster[data_cluster$time=="Morning",]
noon <- data_cluster[data_cluster$time=="Noon",]
evening <- data_cluster[data_cluster$time=="Evening",]

#There are a couple of observations per dataset containing two observations for the same
#time frame, since they are just two or three I decide to remove them
which(duplicated(morning$date))
morning <- morning[-c(3,429,430),]
which(duplicated(morning$date))

which(duplicated(noon$date))
noon <- noon[-c(4,21,482),]
which(duplicated(morning$date))

which(duplicated(evening$date))
evening <- evening[-c(151,440,452),]
which(duplicated(evening$date))


#Now we add the missing values to the different dataframes

allDates <- seq.Date(from=min(morning$date),to=max(morning$date),"day")
morning <- merge(x=data.frame(date=allDates),y=morning,all.x=TRUE)

allDates <- seq.Date(from=min(noon$date),to=max(noon$date),"day")
noon <- merge(x=data.frame(date=allDates),y=noon,all.x=TRUE)

allDates <- seq.Date(from=min(evening$date),to=max(evening$date),"day")
evening <- merge(x=data.frame(date=allDates),y=evening,all.x=TRUE)


#Now we go on and add all the covariates for the missing data
#Colore Zona
zona <- rep(0, 933)
c <- 1
for (i in morning$date){
  if (i <= as.Date('3/05/2020',format='%d/%m/%Y')){
    zona[c] <- "lockdown"
  }
  if (i <= as.Date('14/06/2020',format='%d/%m/%Y') & i>= as.Date('04/05/2020',format='%d/%m/%Y')){
    zona[c] <- "phase2"
  }
  if (i <= as.Date('7/10/2020',format='%d/%m/%Y') & i>= as.Date('15/06/2020',format='%d/%m/%Y')){
    zona[c] <- "phase3"
  }
  if (i <= as.Date('5/11/2020',format='%d/%m/%Y') & i>= as.Date('8/10/2020',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('23/12/2020',format='%d/%m/%Y') & i>= as.Date('6/11/2020',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('10/01/2021',format='%d/%m/%Y') & i>= as.Date('24/12/2020',format='%d/%m/%Y')){
    zona[c] <- "red"
  }
  if (i <= as.Date('31/01/2021',format='%d/%m/%Y') & i>= as.Date('11/01/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('07/03/2021',format='%d/%m/%Y') & i>= as.Date('01/02/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('14/03/2021',format='%d/%m/%Y') & i>= as.Date('08/03/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('05/04/2021',format='%d/%m/%Y') & i>= as.Date('15/03/2021',format='%d/%m/%Y')){
    zona[c] <- "red"
  }
  if (i <= as.Date('25/04/2021',format='%d/%m/%Y') & i>= as.Date('06/04/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('06/06/2021',format='%d/%m/%Y') & i>= as.Date('26/04/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('19/12/2021',format='%d/%m/%Y') & i>= as.Date('07/06/2021',format='%d/%m/%Y')){
    zona[c] <- "white"
  }
  if (i <= as.Date('27/02/2022',format='%d/%m/%Y') & i>= as.Date('20/12/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('31/03/2022',format='%d/%m/%Y') & i>= as.Date('28/02/2022',format='%d/%m/%Y')){
    zona[c] <- "white"
  }
  if (i>= as.Date('01/04/2022',format='%d/%m/%Y')){
    zona[c] <- "no-emergency"
  }
  c <- c+1
}
morning["zona"] <- zona


zona <- rep(0, 932)
c <- 1
for (i in noon$date){
  if (i <= as.Date('3/05/2020',format='%d/%m/%Y')){
    zona[c] <- "lockdown"
  }
  if (i <= as.Date('14/06/2020',format='%d/%m/%Y') & i>= as.Date('04/05/2020',format='%d/%m/%Y')){
    zona[c] <- "phase2"
  }
  if (i <= as.Date('7/10/2020',format='%d/%m/%Y') & i>= as.Date('15/06/2020',format='%d/%m/%Y')){
    zona[c] <- "phase3"
  }
  if (i <= as.Date('5/11/2020',format='%d/%m/%Y') & i>= as.Date('8/10/2020',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('23/12/2020',format='%d/%m/%Y') & i>= as.Date('6/11/2020',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('10/01/2021',format='%d/%m/%Y') & i>= as.Date('24/12/2020',format='%d/%m/%Y')){
    zona[c] <- "red"
  }
  if (i <= as.Date('31/01/2021',format='%d/%m/%Y') & i>= as.Date('11/01/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('07/03/2021',format='%d/%m/%Y') & i>= as.Date('01/02/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('14/03/2021',format='%d/%m/%Y') & i>= as.Date('08/03/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('05/04/2021',format='%d/%m/%Y') & i>= as.Date('15/03/2021',format='%d/%m/%Y')){
    zona[c] <- "red"
  }
  if (i <= as.Date('25/04/2021',format='%d/%m/%Y') & i>= as.Date('06/04/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('06/06/2021',format='%d/%m/%Y') & i>= as.Date('26/04/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('19/12/2021',format='%d/%m/%Y') & i>= as.Date('07/06/2021',format='%d/%m/%Y')){
    zona[c] <- "white"
  }
  if (i <= as.Date('27/02/2022',format='%d/%m/%Y') & i>= as.Date('20/12/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('31/03/2022',format='%d/%m/%Y') & i>= as.Date('28/02/2022',format='%d/%m/%Y')){
    zona[c] <- "white"
  }
  if (i>= as.Date('01/04/2022',format='%d/%m/%Y')){
    zona[c] <- "no-emergency"
  }
  c <- c+1
}
noon["zona"] <- zona


zona <- rep(0, 931)
c <- 1
for (i in evening$date){
  if (i <= as.Date('3/05/2020',format='%d/%m/%Y')){
    zona[c] <- "lockdown"
  }
  if (i <= as.Date('14/06/2020',format='%d/%m/%Y') & i>= as.Date('04/05/2020',format='%d/%m/%Y')){
    zona[c] <- "phase2"
  }
  if (i <= as.Date('7/10/2020',format='%d/%m/%Y') & i>= as.Date('15/06/2020',format='%d/%m/%Y')){
    zona[c] <- "phase3"
  }
  if (i <= as.Date('5/11/2020',format='%d/%m/%Y') & i>= as.Date('8/10/2020',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('23/12/2020',format='%d/%m/%Y') & i>= as.Date('6/11/2020',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('10/01/2021',format='%d/%m/%Y') & i>= as.Date('24/12/2020',format='%d/%m/%Y')){
    zona[c] <- "red"
  }
  if (i <= as.Date('31/01/2021',format='%d/%m/%Y') & i>= as.Date('11/01/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('07/03/2021',format='%d/%m/%Y') & i>= as.Date('01/02/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('14/03/2021',format='%d/%m/%Y') & i>= as.Date('08/03/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('05/04/2021',format='%d/%m/%Y') & i>= as.Date('15/03/2021',format='%d/%m/%Y')){
    zona[c] <- "red"
  }
  if (i <= as.Date('25/04/2021',format='%d/%m/%Y') & i>= as.Date('06/04/2021',format='%d/%m/%Y')){
    zona[c] <- "orange"
  }
  if (i <= as.Date('06/06/2021',format='%d/%m/%Y') & i>= as.Date('26/04/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('19/12/2021',format='%d/%m/%Y') & i>= as.Date('07/06/2021',format='%d/%m/%Y')){
    zona[c] <- "white"
  }
  if (i <= as.Date('27/02/2022',format='%d/%m/%Y') & i>= as.Date('20/12/2021',format='%d/%m/%Y')){
    zona[c] <- "yellow"
  }
  if (i <= as.Date('31/03/2022',format='%d/%m/%Y') & i>= as.Date('28/02/2022',format='%d/%m/%Y')){
    zona[c] <- "white"
  }
  if (i>= as.Date('01/04/2022',format='%d/%m/%Y')){
    zona[c] <- "no-emergency"
  }
  c <- c+1
}
evening["zona"] <- zona



#Temperatures
temp <- read.csv("temperatures.csv", header=T, sep=";")[1:366,1:3]

temp_2022 <- temp[,1]
temp_2021 <- temp[,2]
temp_2020 <- temp[,3]

dates_2022 <- c(seq( as.Date("2022-01-01"), as.Date("2022-12-31"), by="+1 day"),NA)
dates_2021 <- c(seq( as.Date("2021-01-01"), as.Date("2021-12-31"), by="+1 day"),NA)
dates_2020 <- seq( as.Date("2020-01-01"), as.Date("2020-12-31"), by="+1 day")

temp_2022 <- as.data.frame(cbind(dates_2022, temp_2022))
temp_2021 <- as.data.frame(cbind(dates_2021, temp_2021))
temp_2020 <- as.data.frame(cbind(dates_2020, temp_2020))

temperatures_for_dataset <- rep(0, 933)
c1 <- 1
for (i in morning$date){
  c2 <- 1
  for (j in temp_2020$dates_2020){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2020$temp_2020[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in morning$date){
  c2 <- 1
  for (j in temp_2021$dates_2021[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2021$temp_2021[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in morning$date){
  c2 <- 1
  for (j in temp_2022$dates_2022[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2022$temp_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
morning["mean_temperature"] <- temperatures_for_dataset


temperatures_for_dataset <- rep(0, 932)
c1 <- 1
for (i in noon$date){
  c2 <- 1
  for (j in temp_2020$dates_2020){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2020$temp_2020[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in noon$date){
  c2 <- 1
  for (j in temp_2021$dates_2021[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2021$temp_2021[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in noon$date){
  c2 <- 1
  for (j in temp_2022$dates_2022[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2022$temp_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
noon["mean_temperature"] <- temperatures_for_dataset


temperatures_for_dataset <- rep(0, 931)
c1 <- 1
for (i in evening$date){
  c2 <- 1
  for (j in temp_2020$dates_2020){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2020$temp_2020[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in evening$date){
  c2 <- 1
  for (j in temp_2021$dates_2021[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2021$temp_2021[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in evening$date){
  c2 <- 1
  for (j in temp_2022$dates_2022[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2022$temp_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
evening["mean_temperature"] <- temperatures_for_dataset


#Weekend or festive
weekend <- rep(0, 933)
c <- 1
for (i in weekdays(morning$date)){
  if ((i=="Sunday") | (i=="Saturday")){
    weekend[c] <- 1
  }
  c <- c+1
}
morning["weekend_festive"] <- weekend

weekend <- rep(0, 932)
c <- 1
for (i in weekdays(noon$date)){
  if ((i=="Sunday") | (i=="Saturday")){
    weekend[c] <- 1
  }
  c <- c+1
}
noon["weekend_festive"] <- weekend

weekend <- rep(0, 931)
c <- 1
for (i in weekdays(evening$date)){
  if ((i=="Sunday") | (i=="Saturday")){
    weekend[c] <- 1
  }
  c <- c+1
}
evening["weekend_festive"] <- weekend

index_morning <- c(33,37,49,55,87,132,161,183,239,259,276,293,294,300,305,390,394,414,420,452,
                   496,526,547,604,624,641,658,659,665,670,768,772,779,785,817,860,891,911)
index_noon <- index_morning-1
index_evening <- index_morning-6

morning[index_morning,"weekend_festive"] <- 1
noon[index_noon,"weekend_festive"] <- 1
evening[index_evening,"weekend_festive"] <- 1




#Precipitazioni
mm <- read.csv("data_mm.csv", header=T, sep=";")[1:366,1:3]

mm_2022 <- mm[,3]
mm_2021 <- mm[,2]
mm_2020 <- mm[,1]

dates_2022 <- c(seq( as.Date("2022-01-01"), as.Date("2022-12-31"), by="+1 day"),NA)
dates_2021 <- c(seq( as.Date("2021-01-01"), as.Date("2021-12-31"), by="+1 day"),NA)
dates_2020 <- seq( as.Date("2020-01-01"), as.Date("2020-12-31"), by="+1 day")

mm_2022 <- as.data.frame(cbind(dates_2022, mm_2022))
mm_2021 <- as.data.frame(cbind(dates_2021, mm_2021))
mm_2020 <- as.data.frame(cbind(dates_2020, mm_2020))

mm_for_dataset <- rep(0, 933)
c1 <- 1
for (i in morning$date){
  c2 <- 1
  for (j in mm_2020$dates_2020){
    if (i==j){
      mm_for_dataset[c1] <- mm_2020$mm_2020[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in morning$date){
  c2 <- 1
  for (j in mm_2021$dates_2021[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2021$mm_2021[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in morning$date){
  c2 <- 1
  for (j in mm_2022$dates_2022[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2022$mm_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
morning["rain"] <- mm_for_dataset


mm_for_dataset <- rep(0, 932)
c1 <- 1
for (i in noon$date){
  c2 <- 1
  for (j in mm_2020$dates_2020){
    if (i==j){
      mm_for_dataset[c1] <- mm_2020$mm_2020[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in noon$date){
  c2 <- 1
  for (j in mm_2021$dates_2021[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2021$mm_2021[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in noon$date){
  c2 <- 1
  for (j in mm_2022$dates_2022[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2022$mm_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
noon["rain"] <- mm_for_dataset


mm_for_dataset <- rep(0, 931)
c1 <- 1
for (i in evening$date){
  c2 <- 1
  for (j in mm_2020$dates_2020){
    if (i==j){
      mm_for_dataset[c1] <- mm_2020$mm_2020[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in evening$date){
  c2 <- 1
  for (j in mm_2021$dates_2021[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2021$mm_2021[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
c1 <- 1
for (i in evening$date){
  c2 <- 1
  for (j in mm_2022$dates_2022[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2022$mm_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}
evening["rain"] <- mm_for_dataset


#Seasons
get_season <- function(i){
  if (i <= as.Date('20/03/2020',format='%d/%m/%Y') ){
    return("winter")
  }
  if (i >= as.Date('21/03/2020',format='%d/%m/%Y') & i <= as.Date('20/06/2020',format='%d/%m/%Y') ){
    return("spring")
  }
  if (i >= as.Date('21/06/2020',format='%d/%m/%Y') & i <= as.Date('22/09/2020',format='%d/%m/%Y') ){
    return("summer")
  }
  if (i >= as.Date('23/09/2020',format='%d/%m/%Y') & i <= as.Date('21/12/2020',format='%d/%m/%Y') ){
    return("autumn")
  }
  if (i >= as.Date('22/12/2020',format='%d/%m/%Y') & i <= as.Date('20/03/2021',format='%d/%m/%Y') ){
    return("winter")
  }
  if (i >= as.Date('21/03/2021',format='%d/%m/%Y') & i <= as.Date('20/06/2021',format='%d/%m/%Y') ){
    return("spring")
  }
  if (i >= as.Date('21/06/2021',format='%d/%m/%Y') & i <= as.Date('22/09/2021',format='%d/%m/%Y') ){
    return("summer")
  }
  if (i >= as.Date('23/09/2021',format='%d/%m/%Y') & i <= as.Date('21/12/2021',format='%d/%m/%Y') ){
    return("autumn")
  }
  if (i >= as.Date('22/12/2021',format='%d/%m/%Y') & i <= as.Date('20/03/2022',format='%d/%m/%Y') ){
    return("winter")
  }
  if (i >= as.Date('21/03/2022',format='%d/%m/%Y') & i <= as.Date('20/06/2022',format='%d/%m/%Y') ){
    return("spring")
  }
  if (i >= as.Date('21/06/2022',format='%d/%m/%Y') & i <= as.Date('22/09/2022',format='%d/%m/%Y') ){
    return("summer")
  }
  if (i >= as.Date('23/09/2022',format='%d/%m/%Y') & i <= as.Date('21/12/2022',format='%d/%m/%Y') ){
    return("autumn")
  }
}


seasons <- rep("NA", 933)
for (i in 1:length(morning$date)){
  seasons[i] <- get_season(morning$date[i])
}
morning["season"] <- seasons

seasons <- rep("NA", 932)
for (i in 1:length(noon$date)){
  seasons[i] <- get_season(noon$date[i])
}
noon["season"] <- seasons

seasons <- rep("NA", 931)
for (i in 1:length(evening$date)){
  seasons[i] <- get_season(evening$date[i])
}
evening["season"] <- seasons

morning_cluster <- morning[, c("date", "number", "zona", "mean_temperature", "weekend_festive", "rain","season")]
evening_cluster <- evening[, c("date", "number", "zona", "mean_temperature", "weekend_festive", "rain","season")]
noon_cluster <- noon[, c("date", "number", "zona", "mean_temperature", "weekend_festive", "rain","season")]

save(data_cluster, morning_cluster, noon_cluster, evening_cluster, file="cluster.RData")
