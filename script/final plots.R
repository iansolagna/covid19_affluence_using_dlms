library(ggpubr)
library(jpeg)
library(png)
library(plotly)
require(gridExtra)
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/number.RData")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/cluster.RData")

tutti <- readPNG("/Users/ian/Desktop/colori_partenza/colori_tutti.png")
mattina <- readPNG("/Users/ian/Desktop/colori_partenza/colori_mattina.png")
pomeriggio <- readPNG("/Users/ian/Desktop/colori_partenza/colori_mattina.png")
sera <- readPNG("/Users/ian/Desktop/colori_partenza/colori_sera.png")

  


plot1 <- ggplot() +
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=morning, aes(y=number, x=date), size=0.6)+ 
  ggtitle("Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30) +theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=noon, aes(y=number, x=date), size=0.6)+
  ggtitle("Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine3", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) +theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot3 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=evening, aes(y=number, x=date), size=0.6)+
  ggtitle("Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine4", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) +theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

grid.arrange(plot1, plot2, plot3, ncol=1)






plot1 <- ggplot() +
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=morning_cluster, aes(y=variance_within, x=date), size=0.7)+
  ggtitle("Morning Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 15000) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=noon_cluster, aes(y=variance_within, x=date), size=0.7)+
  ggtitle("Noon Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine3", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 20000)  + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot3 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=evening_cluster, aes(y=variance_within, x=date), size=0.7)+
  ggtitle("Evening Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine4", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 20000)  + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

grid.arrange(plot1, plot2, plot3, ncol=1)





plot1 <- ggplot()+ 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=morning_cluster, aes(y=number, x=date), size=0.7)+
  ggtitle("Morning Clusters Time Series") + labs(x="Date",y="Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 20)  + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot()+ 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=noon_cluster, aes(y=number, x=date), size=0.7)+
  ggtitle("Noon Clusters Time Series") + labs(x="Date",y="Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine3", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30)  + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot3 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=evening_cluster, aes(y=number, x=date), size=0.7)+
  ggtitle("Evening Clusters Time Series") + labs(x="Date",y="Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine4", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30)  + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

grid.arrange(plot1, plot2, plot3, ncol=1)




#DLM

plot1 <- ggplot() + geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=data.frame(cbind(morning$date, mu_morning)), aes(x=morning$date, y=mu_morning, colour="Smoothed Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Smoothed Trend" = "seagreen3")) +
  ggtitle("Smoothed Trend for the Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot() + geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning$date, alpha_morning)), aes(x=morning$date, y=alpha_morning, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Smoothed Series" = "mediumpurple2")) +
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  ggtitle("Smoother for Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(0,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))
grid.arrange(plot1, plot2, ncol=1)









plot1 <- ggplot() + geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=data.frame(cbind(noon$date, mu_noon)), aes(x=noon$date, y=mu_noon, colour="Smoothed Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Smoothed Trend" = "seagreen3")) +
  ggtitle("Smoothed Trend for the Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot() + geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=data.frame(cbind(noon$date, alpha_noon)), aes(x=noon$date, y=alpha_noon, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Smoothed Series" = "mediumpurple2")) +
  ggtitle("Smoother for Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))
grid.arrange(plot1, plot2, ncol=1)








plot1 <- ggplot() + geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=data.frame(cbind(evening$date, mu_evening)), aes(x=evening$date, y=mu_evening, colour="Smoothed Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Smoothed Trend" = "seagreen3")) +
  ggtitle("Smoothed Trend for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot2 <- ggplot() + geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=data.frame(cbind(evening$date, alpha_evening)), aes(x=evening$date, y=alpha_evening, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Smoothed Series" = "mediumpurple2")) +
  ggtitle("Smoother for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))
grid.arrange(plot1, plot2, ncol=1)





#PROPHET 

plot1 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning$date, forecast_morning$trend)), aes(x=morning$date, y=forecast_morning$trend, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))



plot2 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon$date, forecast_noon$trend)), aes(x=noon$date, y=forecast_noon$trend, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))




plot3 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening$date, forecast_evening$trend)), aes(x=evening$date, y=forecast_evening$trend, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

grid.arrange(plot1, plot2, plot3, ncol=1)





#VARIANCE WITHIN DLM

plot1 <- ggplot() +
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=morning_cluster, aes(y=variance_within, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning_cluster$date, mu_morning)), aes(x=morning_cluster$date, y=mu_morning, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Morning Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,15000)) +  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"),plot.title=element_text(size=15, face="bold"))
plot2 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=morning_cluster, aes(y=variance_within, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning_cluster$date, alpha_morning)), aes(x=morning_cluster$date, y=alpha_morning, colour="Smoothed Series"),size=0.8) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Smoothed Series" = "mediumpurple2")) +
  ggtitle("Smoother for Morning Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(0,15000)) +  theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"),plot.title=element_text(size=15, face="bold"))
grid.arrange(plot1, plot2, ncol=1)




plot1 <- ggplot() +
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=noon_cluster, aes(y=variance_within, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon_cluster$date, mu_noon)), aes(x=noon_cluster$date, y=mu_noon, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Noon Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 20000) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"),plot.title=element_text(size=15, face="bold"))
plot2 <- ggplot() +
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=noon_cluster, aes(y=variance_within, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon_cluster$date, alpha_noon)), aes(x=noon_cluster$date, y=alpha_noon, colour="Smoothed Series"),size=0.7) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Smoothed Series" = "mediumpurple2")) +
  ggtitle("Smoother for Noon Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 20000)+ theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"),plot.title=element_text(size=15, face="bold"))
grid.arrange(plot1, plot2, ncol=1)




plot1 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=evening_cluster, aes(y=variance_within, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening_cluster$date, mu_evening)), aes(x=evening_cluster$date, y=mu_evening, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Evening Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 20000)+ theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"),plot.title=element_text(size=15, face="bold"))
plot2 <- ggplot() + 
  geom_rect(aes(xmin = as.Date("2020-10-08") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="gold", alpha=0.22)+
  geom_line(data=evening_cluster, aes(y=variance_within, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening_cluster$date, alpha_evening)), aes(x=evening_cluster$date, y=alpha_evening, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Smoothed Series" = "mediumpurple2")) +
  ggtitle("Smoother for Evening Time Series") + labs(x="Date",y="Variance Within") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 20000)+ theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"),plot.title=element_text(size=15, face="bold"))
grid.arrange(plot1, plot2, ncol=1)
