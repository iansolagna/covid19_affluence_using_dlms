rm(list=ls())
setwd("~/Documents/Laurea Magistrale/Tesi/datasets")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/number.RData")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/cluster.RData")
library(ggplot2)
library(dlm)
require(gridExtra)

#Function to create a ggplot object acf
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


plot1 <- ggplot() + geom_line(data=morning_cluster, aes(y=number, x=date), size=0.7)+
  ggtitle("Morning Clusters Time Series") + labs(x="Date",y="Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 20)

plot2 <- ggplot() + geom_line(data=noon_cluster, aes(y=number, x=date), size=0.7)+
  ggtitle("Noon Clusters Time Series") + labs(x="Date",y="Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine3", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30)

plot3 <- ggplot() + geom_line(data=evening_cluster, aes(y=number, x=date), size=0.7)+
  ggtitle("Evening Clusters Time Series") + labs(x="Date",y="Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine4", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30)

grid.arrange(plot1, plot2, plot3, ncol=1)


#Check for correlation between time series since they are very similar to the number of people 
par(mfrow=c(1,1))
plot1 <- ggacf(morning_cluster$number, title="Morning")
plot2 <- ggacf(noon_cluster$number, title="Noon")
plot3 <- ggacf(evening_cluster$number, title="Evening")
grid.arrange(plot1,plot2,plot3,ncol=1)


###############################MORNING###############################

X_morning <- model.matrix(~., data=morning_cluster[,c("mean_temperature", "zona", "rain", "weekend_festive")])[,-1]

fn <- function(parm) {
  mod <- dlmModPoly(order = 1) +  dlmModSeas(frequency = 7) +dlmModReg(X_morning, addInt=F)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit_morning <- dlmMLE(morning_cluster$number, rep(0, 3), build = fn, hessian = TRUE)
fit_morning$convergence


mod_morning <- fn(fit_morning$par)

filtered_morning <- dlmFilter(morning_cluster$number, mod = mod_morning)
smoothed_morning <- dlmSmooth(filtered_morning)
resids_morning <- residuals(filtered_morning, sd = FALSE)
mu_morning <- dropFirst(smoothed_morning$s[, 1])
gammas_morning <- dropFirst(smoothed_morning$s[, 2])
betas_morning <- dropFirst(smoothed_morning$s[, 8:17])

#Plot trend component
require(gridExtra)
plot1 <- ggplot() + geom_line(data=morning_cluster, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning_cluster$date, mu_morning)), aes(x=morning_cluster$date, y=mu_morning, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Morning Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,20))


#Add seasonal and regression components
reg_morning <- apply(betas_morning*X_morning, 1, sum)
alpha_morning <- mu_morning + gammas_morning + reg_morning


plot2 <- ggplot() + geom_line(data=morning_cluster, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning_cluster$date, alpha_morning)), aes(x=morning_cluster$date, y=alpha_morning, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Smoothed Series" = "plum3")) +
  ggtitle("Smoother for Morning Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(0,20))


grid.arrange(plot1, plot2, ncol=1)


#One step ahead prediction for whole time-series
conf.low_morning <- filtered_morning$f - residuals(filtered_morning)$sd * qnorm(0.975) 
conf.up_morning <-  filtered_morning$f + residuals(filtered_morning)$sd * qnorm(0.975) 


ggplot(data=morning_cluster, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=morning_cluster, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning_cluster$date, filtered_morning$f)), aes(x=morning_cluster$date, y=filtered_morning$f, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=conf.low_morning, ymax=conf.up_morning), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("One Step Forecast for Morning Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 18, face="bold"), 
        legend.text = element_text(color = "black", size = 14, face="italic"), plot.title=element_text(size=20, face="bold"))+
  coord_cartesian(ylim=c(0,20))


#Check residuals
plot1 <- ggplot(data.frame(resids_morning), aes(x=resids_morning)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Morning Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-2.5,2.5))
plot2 <- ggacf(resids_morning, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_morning, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_morning)




###############################NOON###############################

X_noon <- model.matrix(~., data=noon_cluster[,c("mean_temperature", "zona", "rain", "weekend_festive")])[,-1]

fn <- function(parm) {
  mod <- dlmModPoly(order = 1) +  dlmModSeas(frequency = 7) +dlmModReg(X_noon, addInt=F)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit_noon <- dlmMLE(noon_cluster$number, rep(0, 3), build = fn, hessian = TRUE)
fit_noon$convergence



mod_noon <- fn(fit_noon$par)

filtered_noon <- dlmFilter(noon_cluster$number, mod = mod_noon)
smoothed_noon <- dlmSmooth(filtered_noon)
resids_noon <- residuals(filtered_noon, sd = FALSE)
mu_noon <- dropFirst(smoothed_noon$s[, 1])
gammas_noon <- dropFirst(smoothed_noon$s[, 2])
betas_noon <- dropFirst(smoothed_noon$s[, 8:17])


#Plot trend component
require(gridExtra)
plot1 <- ggplot() + geom_line(data=noon_cluster, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon_cluster$date, mu_noon)), aes(x=noon_cluster$date, y=mu_noon, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Noon Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30)


#Add seasonal and regression components
reg_noon <- apply(betas_noon*X_noon, 1, sum)
alpha_noon <- mu_noon + gammas_noon + reg_noon


plot2 <- ggplot() + geom_line(data=noon_cluster, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon_cluster$date, alpha_noon)), aes(x=noon_cluster$date, y=alpha_noon, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Smoothed Series" = "plum3")) +
  ggtitle("Smoother for Noon Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 30)


grid.arrange(plot1, plot2, ncol=1)


#One step ahead prediction for whole time-series
conf.low_noon <- filtered_noon$f - residuals(filtered_noon)$sd * qnorm(0.975) 
conf.up_noon <-  filtered_noon$f + residuals(filtered_noon)$sd * qnorm(0.975) 

ggplot(data=noon_cluster, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) +
  geom_line(data=noon_cluster, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon_cluster$date, filtered_noon$f)), aes(x=noon_cluster$date, y=filtered_noon$f, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=conf.low_noon, ymax=conf.up_noon), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("One Step Forecast for Noon Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 18, face="bold"), 
        legend.text = element_text(color = "black", size = 14, face="italic"), plot.title=element_text(size=20, face="bold")) +
  coord_cartesian(ylim=c(0,30))




#Check residuals
plot1 <- ggplot(data.frame(resids_noon), aes(x=resids_noon)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Noon Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-2.5,2.5))
plot2 <- ggacf(resids_noon, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_noon, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_noon)







######################### EVENING #########################

X_evening <- model.matrix(~., data=evening_cluster[,c("mean_temperature", "zona", "rain","weekend_festive")])[,-1]

fn <- function(parm) {
  mod <- dlmModPoly(order = 1) +  dlmModSeas(frequency = 7) +dlmModReg(X_evening, addInt=F)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit_evening <- dlmMLE(evening_cluster$number, rep(0, 3), build = fn, hessian = TRUE)
fit_evening$convergence



mod_evening <- fn(fit_evening$par)

filtered_evening <- dlmFilter(evening_cluster$number, mod = mod_evening)
smoothed_evening <- dlmSmooth(filtered_evening)
resids_evening <- residuals(filtered_evening, sd = FALSE)
mu_evening <- dropFirst(smoothed_evening$s[, 1])
gammas_evening <- dropFirst(smoothed_evening$s[, 2])
betas_evening <- dropFirst(smoothed_evening$s[, 8:17])


#Plot trend component
require(gridExtra)
plot1 <- ggplot() + geom_line(data=evening_cluster, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening_cluster$date, mu_evening)), aes(x=evening_cluster$date, y=mu_evening, colour="Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Trend" = "seagreen3")) +
  ggtitle("Trend for Evening Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 30)



#Add seasonal and regression components
reg_evening <- apply(betas_evening*X_evening, 1, sum)
alpha_evening <- mu_evening + gammas_evening + reg_evening


plot2 <- ggplot() + geom_line(data=evening_cluster, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening_cluster$date, alpha_evening)), aes(x=evening_cluster$date, y=alpha_evening, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Smoothed Series" = "plum3")) +
  ggtitle("Smoother for Evening Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 30)


grid.arrange(plot1, plot2, ncol=1)


#One step ahead prediction for whole time-series
conf.low_evening <- filtered_evening$f - residuals(filtered_evening)$sd * qnorm(0.975) 
conf.up_evening <-  filtered_evening$f + residuals(filtered_evening)$sd * qnorm(0.975) 

ggplot(data=evening_cluster, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) +
  geom_line(data=evening_cluster, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening_cluster$date, filtered_evening$f)), aes(x=evening_cluster$date, y=filtered_evening$f, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=conf.low_evening, ymax=conf.up_evening), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("One Step Forecast for Evening Time Series") + labs(x="Date",y="Number of Clusters") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 18, face="bold"), 
        legend.text = element_text(color = "black", size = 14, face="italic"), plot.title=element_text(size=20, face="bold"))+
  coord_cartesian(ylim=c(0,30))



#Check residuals
plot1 <- ggplot(data.frame(resids_evening), aes(x=resids_evening)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Evening Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-2.5,2.5))
plot2 <- ggacf(resids_evening, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_evening, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_evening)
