rm(list=ls())
setwd("~/Documents/Laurea Magistrale/Tesi/datasets")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/number.RData")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/cluster.RData")
library(ggplot2)
library(prophet)
require(gridExtra)

ggacf <- function(series, col="darkcyan", title="ACF") {
  significance_level <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(series)))  
  a<-acf(series, plot=F, na.action=na.pass)
  a.2<-with(a, data.frame(lag, acf))
  g<- ggplot(a.2[-1,], aes(x=lag,y=acf)) + 
    geom_bar(stat = "identity", position = "identity", fill = col, color="black", width=0.6) +
    xlab('Lag') + ylab('ACF') + geom_hline(yintercept=c(significance_level,-significance_level), lty=3, size=1) +
    ggtitle(title)  + coord_cartesian(ylim=c(-0.2,0.65))
  theme( plot.title=element_text(size=15, face="bold"));
  
  # fix scale for integer lags
  if (all(a.2$lag%%1 == 0)) {
    g<- g + scale_x_discrete(limits = seq(1, max(a.2$lag)));
  }
  return(g);
}

#Festive Days in Venice
index_morning <- c(33,37,49,55,87,132,161,183,239,259,276,293,294,300,305,390,394,414,420,452,
                   496,526,547,604,624,641,658,659,665,670,768,772,779,785,817,860,891,911)
index_noon <- index_morning-1
index_evening <- index_morning-6


holidays_morning <- data.frame(rep("Festive",38),morning[index_morning,1])
colnames(holidays_morning) <- c("holiday","ds")


holidays_noon <- data.frame(rep("Festive",38),noon$date[index_noon])
colnames(holidays_noon) <- c("holiday","ds")


holidays_evening <- data.frame(rep("Festive",38),evening$date[index_evening])
colnames(holidays_evening) <- c("holiday","ds")



##########################MORNING##########################
df_morning <- morning[,c("date","number","mean_temperature","zona","rain")]
colnames(df_morning) <- c("ds","y","mean_temperature","zona","rain")
prophet_morning <- prophet()
prophet_morning <- add_regressor(prophet_morning, "mean_temperature")
prophet_morning <- add_regressor(prophet_morning, "rain")
prophet_morning <- add_regressor(prophet_morning,"zona")
prophet_morning <- prophet(df_morning, weekly.seasonality = T, holidays=holidays_morning)
future_morning <- make_future_dataframe(prophet_morning, period= 1)
forecast_morning <- predict(prophet_morning, future_morning)[1:933,]



ggplot() + geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning$date, forecast_morning$trend)), aes(x=morning$date, y=forecast_morning$trend, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

ggplot(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning$date, forecast_morning$yhat)), aes(x=morning$date, y=forecast_morning$yhat, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=forecast_morning$yhat_lower, ymax=forecast_morning$yhat_upper), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("Forecast for Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(-10,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


resids_morning <- morning$number - forecast_morning$yhat
plot1 <- ggplot(data.frame(resids_morning), aes(x=resids_morning)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Morning Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-12,12))
plot2 <- ggacf(resids_morning, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_morning, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_morning)


##########################NOON##########################
df_noon <- noon[,c("date","number","mean_temperature","zona","rain")]
colnames(df_noon) <- c("ds","y","mean_temperature","zona","rain")
prophet_noon <- prophet()
prophet_noon <- add_regressor(prophet_noon, "mean_temperature")
prophet_noon <- add_regressor(prophet_noon,"zona")
prophet_noon <- add_regressor(prophet_noon, "rain")
prophet_noon <- prophet(df_noon, weekly.seasonality = T, holidays = holidays_noon)
future_noon <- make_future_dataframe(prophet_noon, period= 1)
forecast_noon <- predict(prophet_noon, future_noon)[1:932,]

ggplot() + geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon$date, forecast_noon$trend)), aes(x=noon$date, y=forecast_noon$trend, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

ggplot(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon$date, forecast_noon$yhat)), aes(x=noon$date, y=forecast_noon$yhat, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=forecast_noon$yhat_lower, ymax=forecast_noon$yhat_upper), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("Forecast for Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


resids_noon <- noon$number - forecast_noon$yhat
plot1 <- ggplot(data.frame(resids_noon), aes(x=resids_noon)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 30)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Noon Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-19,19))
plot2 <- ggacf(resids_noon, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_noon, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_noon)


prophet_plot_components(prophet_noon, forecast_noon)

##########################EVENING##########################
df_evening <- evening[,c("date","number","mean_temperature","zona","rain")]
colnames(df_evening) <- c("ds","y","mean_temperature","zona","rain")
prophet_evening <- prophet()
prophet_evening <- add_regressor(prophet_evening, "mean_temperature")
prophet_evening <- add_regressor(prophet_evening,"zona")
prophet_evening <- add_regressor(prophet_evening, "rain")
prophet_evening <- prophet(df_evening, weekly.seasonality = T, holidays=holidays_evening)
future_evening <- make_future_dataframe(prophet_evening, period= 1)
forecast_evening <- predict(prophet_evening, future_evening)[1:931,]

ggplot() + geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening$date, forecast_evening$trend)), aes(x=evening$date, y=forecast_evening$trend, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

ggplot(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening$date, forecast_evening$yhat)), aes(x=evening$date, y=forecast_evening$yhat, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=forecast_evening$yhat_lower, ymax=forecast_evening$yhat_upper), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("Forecast for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 11, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


resids_evening <- evening$number - forecast_evening$yhat
plot1 <- ggplot(data.frame(resids_evening), aes(x=resids_evening)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 30)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Evening Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-17,17))
plot2 <- ggacf(resids_evening, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_evening, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_evening)



#Out of sample MSE
cross_morning <- prophet::cross_validation(prophet_morning, initial=900, period = 1, horizon = 3, units = 'days')
prophet::performance_metrics(cross_morning)


cross_noon <- prophet::cross_validation(prophet_noon, initial = 900, period = 1, horizon = 3, units = 'days')
prophet::performance_metrics(cross_noon)

cross_evening <- prophet::cross_validation(prophet_evening, initial = 900, period = 1, horizon = 3, units = 'days')
prophet::performance_metrics(cross_evening)



#In sample MSE
mean((morning$number[-which(is.na(morning$number))] - forecast_morning$yhat[-which(is.na(morning$number))])^2)
mean((noon$number[-which(is.na(noon$number))] - forecast_noon$yhat[-which(is.na(noon$number))])^2)
mean((evening$number[-which(is.na(evening$number))] - forecast_evening$yhat[-which(is.na(evening$number))])^2)

