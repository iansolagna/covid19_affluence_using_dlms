####SEASONALITY####
library(grid)
require(gridExtra)
plot1 <- ggplot() + geom_line(data=data.frame(cbind(morning$date, gammas_morning)), aes(x=morning$date[1:length(morning$date)], y=gammas_morning),size=0.3) +
        ggtitle("Seasonality of the Morning Time Series") + labs(x="Date",y="Number of People") +
        theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=15, face="bold")) +
      coord_cartesian(ylim=c(-1000,1000))

plot2 <- ggplot() + geom_line(data=data.frame(cbind(noon$date, gammas_noon)), aes(x=noon$date[1:length(noon$date)], y=gammas_noon),size=0.3) +
        ggtitle("Seasonality of the Noon Time Series") + labs(x="Date",y="Number of People") +
        theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=15, face="bold")) +
        coord_cartesian(ylim=c(-1200,1200))

plot3 <- ggplot() + geom_line(data=data.frame(cbind(evening$date, gammas_evening)), aes(x=evening$date[1:length(evening$date)], y=gammas_evening),size=0.3) + 
        ggtitle("Seasonality of the Evening Time Series") + labs(x="Date",y="Number of People") +
        theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=15, face="bold")) +
        coord_cartesian(ylim=c(-1000,1000))

grid.arrange(plot1,plot2,plot3)




####REGRESSORS####

beta_morning <- filtered_morning$m[257:length(morning$date), 8:17]
beta_noon <- filtered_noon$m[256:length(noon$date), 9:18]
beta_evening <- filtered_evening$m[257:length(evening$date), 8:17]


#Temperature
plot1 <- ggplot() + geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,1])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,1], colour="Morning"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,1])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,1], colour="Noon"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,1])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,1], colour="Evening"),size=0.8) +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=15, face="bold")) + 
  scale_color_manual(name = "Legend:", values = c("Morning" = "lightsteelblue3", "Noon" = "orange2", "Evening"="dodgerblue4")) +
  ggtitle("Mean Temperature Coefficients") + labs(x="Date",y="Value") +
  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(-2000,2000))



#Rain
plot2<- ggplot() + geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,9])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,9], colour="Morning"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,9])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,9], colour="Noon"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,9])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,9], colour="Evening"),size=0.8) +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=15, face="bold")) + 
  scale_color_manual(name = "Legend:", values = c("Morning" = "lightsteelblue", "Noon" = "orange2", "Evening"="dodgerblue4")) +
  ggtitle("Rain Coefficients") + labs(x="Date",y="Value") +
  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(-1000,1000))
grid.arrange(plot1,plot2, ncol=2)

#Weekend_festive
ggplot() + geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,10])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,10], colour="Morning"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,10])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,10], colour="Noon"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,10])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,10], colour="Evening"),size=0.8) +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
  legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=5 , face="bold")) + 
  scale_color_manual(name = "Legend:", values = c("Morning" = "lightblue4", "Noon" = "tomato3", "Evening"="navy")) +
  ggtitle("Festive Day Coefficients") + labs(x="Date",y="Value") +
  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(-10,10))


#Zona

plot1 <- ggplot() + geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,2])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,2], colour="No-Emergency"),size=0.8) +
  geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,3])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,3], colour="Orange Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,4])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,4], colour="Phase2"),size=0.8) +
  geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,5])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,5], colour="Phase3"),size=0.8) +
  geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,6])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,6], colour="Red Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,7])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,7], colour="White Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(morning$date[257:length(morning$date)], beta_morning[,8])), aes(x=morning$date[257:length(morning$date)], y=beta_morning[,8], colour="Yellow Zone"),size=0.8) +
  theme(legend.position = "none", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic")) + 
  scale_color_manual(name = "Legend:", values = c("No-Emergency" = "forestgreen", "Orange Zone"="Orange", "Phase2"="pink","Phase3"="indianred",
                                                  "Red Zone"="Red", "White Zone"="grey50", "Yellow Zone"="gold")) +
  ggtitle("Morning") + labs(x="Date",y="Value") +
  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=10, face="bold")) +
  coord_cartesian(ylim=c(-2500,1500))


plot2 <- ggplot() + geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,2])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,2], colour="No-Emergency"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,3])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,3], colour="Orange Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,4])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,4], colour="Phase2"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,5])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,5], colour="Phase3"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,6])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,6], colour="Red Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,7])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,7], colour="White Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(noon$date[256:length(noon$date)], beta_noon[,8])), aes(x=noon$date[256:length(noon$date)], y=beta_noon[,8], colour="Yellow Zone"),size=0.8) +
  theme(legend.position = "none", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic")) + 
  scale_color_manual(name = "Legend:", values = c("No-Emergency" = "forestgreen", "Orange Zone"="Orange", "Phase2"="pink","Phase3"="indianred",
                                                  "Red Zone"="Red", "White Zone"="grey50", "Yellow Zone"="gold")) +
  ggtitle("Noon") + labs(x="Date",y="Value") +
  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=10, face="bold")) +
  coord_cartesian(ylim=c(-2500,1500))


plot3 <- ggplot() + geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,2])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,2], colour="No-Emergency"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,3])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,3], colour="Orange Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,4])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,4], colour="Phase2"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,5])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,5], colour="Phase3"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,6])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,6], colour="Red Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,7])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,7], colour="White Zone"),size=0.8) +
  geom_line(data=data.frame(cbind(evening$date[257:length(evening$date)], beta_evening[,8])), aes(x=evening$date[257:length(evening$date)], y=beta_evening[,8], colour="Yellow Zone"),size=0.8) +
  theme(legend.position = "none", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic")) + 
  scale_color_manual(name = "Legend:", values = c("No-Emergency" = "forestgreen", "Orange Zone"="Orange", "Phase2"="pink","Phase3"="indianred",
                                                  "Red Zone"="Red", "White Zone"="grey50", "Yellow Zone"="gold")) +
  ggtitle("Evening") + labs(x="Date",y="Value") +
  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), plot.title=element_text(size=10, face="bold")) +
  coord_cartesian(ylim=c(-2500,1500))

plot4 <- ggplot() + scale_color_manual(name = "Legend:", values = c("No-Emergency" = "forestgreen", "Orange Zone"="Orange", "Phase2"="pink","Phase3"="indianred",
                                                                    "Red Zone"="Red", "White Zone"="grey50", "Yellow Zone"="gold"))

grid.arrange(plot1, plot2, plot3, plot4,ncol=4, top = textGrob("Government Restrictions",gp=gpar(fontsize=20,font=2)))



#MARGINAL EFFECT ZONA

lm_morning <- lm(number~ zona, data=morning)
lm_noon <- lm(number~ zona, data=noon)
lm_evening <- lm(number~ zona, data=evening)

summary(lm_morning)
summary(lm_noon)
summary(lm_evening)




sum(morning_cluster$variance_within[500:933]==0, na.rm=T)




