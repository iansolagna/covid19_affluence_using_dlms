# NUMBER OF PEOPLE DATASET CREATION
rm(list=ls())

setwd("~/Documents/Laurea Magistrale/Tesi/people_position_txt")

data_number <- matrix(nrow=1421, ncol=4)
colnames(data_number) <- c("ID","date", "time", "number")
data_number <- as.data.frame(data_number)

extract_number <- function(text_file){
  table <- read.table(text_file)
  n = dim(table)[1]
  if (substr(text_file,1,3)=="IMG"){
    year <- substr(text_file,5,8)
    month <- substr(text_file,9,10)
    day <- substr(text_file,11,12)
    date = paste(day,"-",month,"-",year)
    hour <- as.numeric(substr(text_file,14,15))
  }  
  else {
    year <- substr(text_file,1,4)
    month <- substr(text_file,5,6)
    day <- substr(text_file,7,8)
    date = paste(day,"-",month,"-",year)
    hour <- as.numeric(substr(text_file,10,11))
  }
  
  if (hour>=0 & hour<=10){
      time_of_day <- "Morning"
  }
  else if (hour>=11 & hour<=15){
      time_of_day <- "Noon"
  }
  else if (hour>=16){
      time_of_day <- "Evening"
  }
  return(list(date, time_of_day, n))
}

c=1
for (i in list.files()){
  data_number[c,2:4] <- extract_number(i)
  c=c+1
}

sum(data_number$Number)

# PEOPLE POSITION DATASET

setwd("~/Documents/Laurea Magistrale/Tesi/people_position_txt")

data_location <- matrix(nrow=32434, ncol=5)
colnames(data_location) <- c("ID", "Date", "Time", "x", "y")
data_location <- as.data.frame(data_location)

id_vec <- NULL
date_vec <- NULL
time_vec <- NULL

for (i in 1:dim(data_number)[1]){
  if (i<265){
    id_vec <- c(id_vec, rep(i+1157, data_number$Number[i]))
  }
  else{
    id_vec <- c(id_vec, rep(i-264, data_number$Number[i]))
  }
  date_vec <- c(date_vec, rep(data_number$Date[i], data_number$Number[i]))
  time_vec <- c(time_vec, rep(data_number$Time[i], data_number$Number[i]))
}

data_location[,1:3] <- cbind(id_vec,date_vec,time_vec)

c=1
for (i in list.files()){
  table <- as.data.frame(read.table(i)[,2:3])
  jump <- dim(table)[1]
  data_location[c:(c+jump-1), 4:5] <- table
  c=c+jump
}

#Change dates to actual date format
data_location$Date <- as.Date(data_location$Date, format="%d - %m - %Y")
data_number$Date <- as.Date(data_number$Date, format="%d - %m - %Y")

#Sort the vector per date 
order_number <- order(data_number$Date)
order_location <- order(data_location$Date)

data_number = data_number[order_number,]
data_location = data_location[order_location,]

#Add ID to number dataset
data_number[,1] <- 1:1421


#Scale to image dimension
data_location$x <- data_location$x*2048
data_location$y <- data_location$y*800


#Dataset with corner position
data_corner <- matrix(nrow=1421, ncol=3)
colnames(data_corner) <- c("ID", "x", "y")
data_corner <- as.data.frame(data_corner)

setwd("~/Documents/Laurea Magistrale/Tesi/corner_position_txt")

c=1
for (i in list.files()){
  table <- as.data.frame(read.table(i)[,2:3])
  data_corner[c, 2:3] <- table
  c=c+1
}

data_corner <- data_corner[order_number,]
data_corner[,"ID"] <- 1:1421

#Scale to image dimension
data_corner$x <- data_corner$x*2048
data_corner$y <- data_corner$y*800



#Translate all points based on the corner 
data_corner$y <- 800-data_corner$y
data_location$y <- 2048-data_location$y
plot(data_location[which(data_location$ID==5),4:5])

c=0
for (i in 1:dim(data_corner)[1]){
  n <- data_number$Number[i]
  new_x <- data_corner$x[i]
  new_y <- data_corner$y[i]
  c <- c+1
  for (j in c:(c+(n-1))){
    data_location$x[j] <- data_location$x[j]-new_x
    data_location$y[j] <- data_location$y[j]-new_y
  }
  c <- c+n-1
}

plot(data_location[which(data_location$ID==5),4:5])

data_corner$y <- 800-data_corner$y
data_location$y <- 2048-data_location$y

rm(table, c, date_vec, i, id_vec, jump, order_location, order_number, time_vec, extract_number, j,n,new_x,new_y)
save.image("~/Documents/Laurea Magistrale/Tesi/Datasets.RData")



#Dataset completion


#Colore Zona
zona <- rep(0, 1421)

c <- 1
for (i in data_number$Date){
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

data_number["zona"] <- zona


#Luce
vec <- c(194,200,203,208,211,214,217,220,223,226,233,235,240,243,248,251,257,260,263,
         266,271,276,279,282,285,290,297,304,306,309,311,314,331,336, 344, 353, 382,
         916,919,925,933,936,940,943,946,949,950,953,956,959,961,964,967, 974, 977,
         980,983,985,989,992,995,997,1000,1002,1006,1009,1012,1017,1019,1021,1024,
         1026,1030,1033,1036,1039,1046, 1051,1064,1070)

data_number[data_number$ID %in% vec,]["luce"] = 0

#Temperatures
setwd("~/Documents/Laurea Magistrale/Tesi")

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

temperatures_for_dataset <- rep(0, 1421)

c1 <- 1
for (i in data_number$Date){
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
for (i in data_number$Date){
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
for (i in data_number$Date){
  c2 <- 1
  for (j in temp_2022$dates_2022[1:365]){
    if (i==j){
      temperatures_for_dataset[c1] <- temp_2022$temp_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}

data_number["mean_temperature"] <- temperatures_for_dataset


#Weekend/Festive
weekend <- rep(0, 1421)
c <- 1
for (i in weekdays(data_number$Date)){
  if ((i=="Sunday") | (i=="Saturday")){
    weekend[c] <- 1
  }
  c <- c+1
}



data_number["weekend_festive"] <- weekend

vec_festive <- c(289,290,1018,1019,300,301,34,35,510,511,40,41,518,519,520,521,63,
                 64,568,569,1188,1189,1190,72,73,576,577,578,1198,1199,1200,123,
                 125,652,653,654,1242,1243,1244,747,748,749,750,751,752,753,754,
                 755,1313,1314,1315,1316,1317,795,796,915,916,941,942,943,215,216,
                 217,257,274,1001,1002,275,276,1003)

data_number[data_number$ID %in% vec_festive,]["weekend_festive"] = 1


#Precipitazioni
setwd("~/Documents/Laurea Magistrale/Tesi")

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

mm_for_dataset <- rep(0, 1421)

c1 <- 1
for (i in data_number$Date){
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
for (i in data_number$Date){
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
for (i in data_number$Date){
  c2 <- 1
  for (j in mm_2022$dates_2022[1:365]){
    if (i==j){
      mm_for_dataset[c1] <- mm_2022$mm_2022[c2]
    }
    c2 <- c2+1
  }
  c1 <- c1+1
}

data_number["rain"] <- mm_for_dataset


#Season

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


seasons <- rep("NA", 1421)
for (i in 1:length(data_number$Date)){
  seasons[i] <- get_season(data_number$Date[i])
}

data_number["season"] <- seasons

colnames(data_number) <- c("ID","date","time","number","zona","luce","mean_temperature","weekend_festive","rain","season")          




#Now we create the datasets for every single period of the day (Morning, Noon and Evening)

#Convert factors to factors

data_number$time <- as.factor(data_number$time)
data_number$time <- relevel(data_number$time, ref="Morning")

data_number$zona <- as.factor(data_number$zona)
data_number$zona <- relevel(data_number$zona, ref="no-emergency")

data_number$luce <- as.factor(data_number$luce)

data_number$weekend_festive <- as.factor(data_number$weekend_festive)

data_number$season <- as.factor(data_number$season)
data_number$season <- relevel(data_number$season, ref="winter")

#Divide in Morning, Noon and Evening
morning <- data_number[data_number$time=="Morning",]
noon <- data_number[data_number$time=="Noon",]
evening <- data_number[data_number$time=="Evening",]

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

morning <- morning[, c("date", "number", "zona", "mean_temperature", "weekend_festive", "rain","season")]
evening <- evening[, c("date", "number", "zona", "mean_temperature", "weekend_festive", "rain","season")]
noon <- noon[, c("date", "number", "zona", "mean_temperature", "weekend_festive", "rain","season")]

save.image("~/Documents/Laurea Magistrale/Tesi/datasets/Datasets.RData")




