rm(list=ls())
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/number.RData")
library(ggplot2)
require(gridExtra)

# Number of People --------------------------------------------------------

#Date 


data_number$time <- factor(data_number$time, levels=c("Morning","Noon","Evening"))
data_number$season <- factor(data_number$season, levels=c("spring",  "summer", "autumn", "winter"))
levels(data_number$season) <- c('Spring', 'Summer', 'Autumn', "Winter")


ggplot(data_number, aes(x=data_number$season, y=data_number$number, fill=data_number$time)) + geom_boxplot() +
  scale_fill_manual(name = "Time of the Day",values=c("powderblue", "gold", "lightsalmon1")) + labs(x="Season",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=20, face="bold"))+
  theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="bold"))




#Time of the day and date
plot1 <- ggplot() + geom_point(data=morning, aes(y=number, x=date), size=0.8, color="deepskyblue3")+
  ggtitle("Morning") + labs(x="Date",y="Number of People") +
  theme(plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30) + theme(axis.text.x=element_text(size=12, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot() + geom_point(data=noon, aes(y=number, x=date), size=0.8, color="gold3")+
  ggtitle("Noon") + labs(x="Date",y="Number of People") +
  theme(plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) + theme(axis.text.x=element_text(size=12, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=12, face="bold"))

plot3 <- ggplot() + geom_point(data=evening, aes(y=number, x=date), size=0.8, color="lightsalmon3")+
  ggtitle("Evening") + labs(x="Date",y="Number of People") +
  theme(plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) + theme(axis.text.x=element_text(size=12, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=12, face="bold"))

grid.arrange(plot1, plot2, plot3, ncol=1)        

#Zona
data_number$zona <- factor(data_number$zona, levels=c('lockdown', 'phase2', 'phase3', "white","yellow","orange","red", "no-emergency"))
levels(data_number$zona) <- c('Lockdown', 'Phase2', 'Phase3', "White","Yellow","Orange","Red", "No-Emergency")

ggplot(data_number, aes(x=data_number$zona, y=data_number$number, fill=data_number$zona)) + geom_boxplot() +
  scale_fill_manual(name = "Zone",values=c("grey50", "indianred", "pink","white","yellow","orange","red","forestgreen")) + labs(x="Season",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=20, face="bold"))+
  theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="bold"))


#Festive

levels(data_number$weekend_festive) <- c("Working Day", "Weekend/Festive")

ggplot(data_number, aes(x=data_number$weekend_festive, y=data_number$number, fill=data_number$weekend_festive)) + geom_boxplot() +
  scale_fill_manual(name = "Zone",values=c("grey70","coral2")) + labs(x="Season",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=20, face="bold"))+
  theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="bold"))






#Temperature

rain_fac <- factor(ifelse(data_number$rain<=5, 0, 1))
levels(rain_fac) <- c("No-Rain", "Rain")

plot1 <- ggplot(data_number, aes(x=rain_fac, y=data_number$number, fill=rain_fac)) + geom_boxplot() +
  scale_fill_manual(name = "Rain",values=c("lightcyan","deepskyblue3")) + labs(x="Season",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=20, face="bold"))+
  theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="bold"))

temp <- rep(0,1421)
for (i in 1:1421){
  if (data_number$mean_temperature[i] <=15){
    temp[i] <- "Cold"
  }
  if (data_number$mean_temperature[i]>15 & data_number$mean_temperature[i]<=25){
    temp[i] <- "Hot"
  }
  if (data_number$mean_temperature[i]>25){
    temp[i] <- "Very-Hot"
  }
}


plot2 <- ggplot(data_number, aes(x=temp, y=data_number$number, fill=temp)) + geom_boxplot() +
  scale_fill_manual(name = "Temperature",values=c("lightcyan","gold","indianred3")) + labs(x="Temperature",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=20, face="bold"))+
  theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_blank(), axis.title.y = element_text(size=15, face="bold"))

grid.arrange(plot1, plot2, ncol=2)



#Number of people

plot1 <- ggplot(data_number, aes(y=data_number$number)) + geom_boxplot(color="black", fill="plum2")  + labs(y="Number of People") +
  theme(axis.text.x=element_text(size=15, face="bold", color="white")) +theme(axis.title.x =element_text(size=15, face="bold") , axis.title.y = element_text(size=15, face="bold")) +
  xlab("Boxplot")
  

plot2 <- ggplot(data_number, aes(x=data_number$number)) +  geom_histogram(binwidth=2, color="black", fill="powderblue") +
   geom_vline(aes(xintercept=mean(data_number$number)),color="black", linetype="dashed", size=1.5) +geom_density(alpha=.2, fill="powderblue") +
  xlab("Number of People") + ylab('Frequency') + theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_text(size=15, face="bold"), axis.title.y = element_text(size=15, face="bold"))+

grid.arrange(plot1,plot2, ncol=2)

