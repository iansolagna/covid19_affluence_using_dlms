




ggplot() + geom_segment(aes(x=1, xend=76, y=88, yend=88), color="grey40", size=6)+
  geom_segment(aes(x=77, xend=146, y=88, yend=88), color="pink", size=6)+
  geom_segment(aes(x=147, xend=176, y=88, yend=88), color="indianred", size=6)+
  geom_segment(aes(x=177, xend=271, y=88, yend=88), color="yellow", size=6)+
  geom_segment(aes(x=272, xend=311, y=88, yend=88), color="red", size=6)+
  geom_segment(aes(x=312, xend=358, y=88, yend=88), color="orange", size=6)+
  geom_segment(aes(x=359, xend=441, y=88, yend=88), color="yellow", size=6)+
  geom_segment(aes(x=442, xend=460, y=88, yend=88), color="orange", size=6)+
  geom_segment(aes(x=461, xend=521, y=88, yend=88), color="red", size=6)+
  geom_segment(aes(x=522, xend=569, y=88, yend=88), color="orange", size=6)+
  geom_segment(aes(x=570, xend=662, y=88, yend=88), color="yellow", size=6)+
  geom_segment(aes(x=663, xend=989, y=88, yend=88), color="white", size=6)+
  geom_segment(aes(x=990, xend=1129, y=88, yend=88), color="yellow", size=6)+
  geom_segment(aes(x=1130, xend=1160, y=88, yend=88), color="white", size=6)+
  geom_segment(aes(x=1161, xend=1421, y=88, yend=88), color="forestgreen", size=6)+
  geom_line(data=data_number, aes(y=number, x=ID), size=0.5, color="black")+
   labs(x="Observation",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 90) +  theme(axis.text.x=element_text(size=15, face="bold")) +theme(axis.title.x = element_text(size=15, face="bold"), axis.title.y = element_text(size=15, face="bold"))



plot1 <- ggplot() + geom_line(data=morning, aes(y=number, x=date), size=0.6)+
        ggtitle("Morning Time Series") + labs(x="Date",y="Number of People") +
        theme(legend.position = "right", legend.title = element_text(color = "aquamarine", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
        ylim(0, 30)  +theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))
plot2 <- ggplot() + geom_line(data=noon, aes(y=number, x=date), size=0.6)+
  ggtitle("Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine3", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) +theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

plot3 <- ggplot() + geom_line(data=evening, aes(y=number, x=date), size=0.6)+
  ggtitle("Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "aquamarine4", size = 10, face="bold"), 
        legend.text = element_text(color = "black", size = 12, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) +theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))

grid.arrange(plot1, plot2, plot3, ncol=1)


plot1 <- ggacf(morning$number, col="aquamarine", title="Morning")
plot2 <- ggacf(noon$number, col="aquamarine3", title="Noon")
plot3 <- ggacf(evening$number, col="aquamarine4", title="Evening")
grid.arrange(plot1, plot2, plot3, ncol=1)


######################### MORNING #########################
#########################SEASON + COVARIATES#########################

X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain", "zona","weekend_festive")])[,-1]

fn <- function(parm) {
  mod <- dlmModPoly(order = 1) +  dlmModSeas(frequency = 7) + dlmModReg(X_morning, addInt=F)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit_morning <- dlmMLE(morning$number, rep(0, 3), build = fn, hessian = TRUE)
fit_morning$convergence


mod_morning <- fn(fit_morning$par)

filtered_morning <- dlmFilter(morning$number, mod = mod_morning)
smoothed_morning <- dlmSmooth(filtered_morning)
resids_morning <- residuals(filtered_morning, sd = FALSE)
mu_morning <- dropFirst(smoothed_morning$s[, 1])
gammas_morning <- dropFirst(smoothed_morning$s[, 2])
betas_morning <- dropFirst(smoothed_morning$s[, 8:17])


#Plot trend component
require(gridExtra)
plot1 <- ggplot() + geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-12-19") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="red", alpha=0.12)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="red", alpha=0.12)+
  geom_line(data=data.frame(cbind(morning$date, mu_morning)), aes(x=morning$date, y=mu_morning, colour="Smoothed Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Smoothed Trend" = "seagreen3")) +
  ggtitle("Smoothed Trend for the Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(0,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


#Add seasonal and regression components
reg_morning <- apply(betas_morning*X_morning, 1, sum)
alpha_morning <- mu_morning + gammas_morning + reg_morning


plot2 <- ggplot() + geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(morning$date, alpha_morning)), aes(x=morning$date, y=alpha_morning, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Smoothed Series" = "plum")) +
  geom_rect(aes(xmin = as.Date("2020-12-19") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="red", alpha=0.12)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="red", alpha=0.12)+
  ggtitle("Smoother for Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(0,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


grid.arrange(plot1, plot2, ncol=1)


#One step ahead prediction for whole time-series
conf.low_morning <- filtered_morning$f - residuals(filtered_morning)$sd * qnorm(0.975) 
conf.up_morning <-  filtered_morning$f + residuals(filtered_morning)$sd * qnorm(0.975) 


ggplot(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_line(data=morning, aes(y=number, x=date, colour="Morning Time Series"), size=0.7) + 
  geom_rect(aes(xmin = as.Date("2020-12-19") , xmax = as.Date("2021-06-06"), ymin=-Inf, ymax=Inf), fill="red", alpha=0.12)+
  geom_rect(aes(xmin = as.Date("2021-12-19") , xmax = as.Date("2022-02-27"), ymin=-Inf, ymax=Inf), fill="red", alpha=0.12) +
  geom_line(data=data.frame(cbind(morning$date, filtered_morning$f)), aes(x=morning$date, y=filtered_morning$f, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=conf.low_morning, ymax=conf.up_morning), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Morning Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("One Step Forecast for Morning Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 10, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(-10,30)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold")) 




#Check residuals
plot1 <- ggplot(data.frame(resids_morning), aes(x=resids_morning)) + 
         geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
         geom_density(alpha=.2, fill="yellow4") + xlab('Morning Residuals') + ylab('Frequency')+
         coord_cartesian(xlim=c(-2,2))
plot2 <- ggacf(resids_morning, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_morning, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_morning)



######################### NOON #########################
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "zona", "rain", "weekend_festive")])[,-1]

fn <- function(parm) {
  mod <- dlmModPoly(order = 2) +  dlmModSeas(frequency = 7) +dlmModReg(X_noon, addInt=F)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit_noon <- dlmMLE(noon$number, rep(0, 3), build = fn, hessian = TRUE)
fit_noon$convergence

mod_noon <- fn(fit_noon$par)

filtered_noon <- dlmFilter(noon$number, mod = mod_noon)
smoothed_noon <- dlmSmooth(filtered_noon)
resids_noon <- residuals(filtered_noon, sd = FALSE)
mu_noon <- dropFirst(smoothed_noon$s[, 1])
gammas_noon <- dropFirst(smoothed_noon$s[, 3])
betas_noon <- dropFirst(smoothed_noon$s[, 9:18])


#Plot trend component
require(gridExtra)
plot1 <- ggplot() + geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon$date, mu_noon)), aes(x=noon$date, y=mu_noon, colour="Smoothed Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Smoothed Trend" = "seagreen3")) +
  ggtitle("Smoothed Trend for the Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


#Add seasonal and regression components
reg_noon <- apply(betas_noon*X_noon, 1, sum)
alpha_noon <- mu_noon + gammas_noon + reg_noon


plot2 <- ggplot() + geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon$date, alpha_noon)), aes(x=noon$date, y=alpha_noon, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Smoothed Series" = "plum3")) +
  ggtitle("Smoother for Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


grid.arrange(plot1, plot2, ncol=1)


#One step ahead prediction for whole time-series
conf.low_noon <- filtered_noon$f - residuals(filtered_noon)$sd * qnorm(0.975) 
conf.up_noon <-  filtered_noon$f + residuals(filtered_noon)$sd * qnorm(0.975) 

ggplot(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) +
  geom_line(data=noon, aes(y=number, x=date, colour="Noon Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(noon$date, filtered_noon$f)), aes(x=noon$date, y=filtered_noon$f, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=conf.low_noon, ymax=conf.up_noon), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Noon Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("One Step Forecast for Noon Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 10, face="italic"), plot.title=element_text(size=15, face="bold")) +
  coord_cartesian(ylim=c(0,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))
  



#Check residuals
plot1 <- ggplot(data.frame(resids_noon), aes(x=resids_noon)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Noon Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-2,2))
plot2 <- ggacf(resids_noon, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_noon, lag = 12, type = "Ljung", fitdf = 2)
shapiro.test(resids_noon)





######################### EVENING #########################
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "zona", "rain", "weekend_festive")])[,-1]

fn <- function(parm) {
  mod <- dlmModPoly(order = 1) +  dlmModSeas(frequency = 7) + dlmModReg(X_evening, addInt=F)
  V(mod) <- exp(parm[1])
  diag(W(mod))[1:2] <- exp(parm[2:3])
  return(mod)
}

fit_evening <- dlmMLE(evening$number, rep(0, 3), build = fn, hessian = TRUE)
fit_evening$convergence




mod_evening <- fn(fit_evening$par)

filtered_evening <- dlmFilter(evening$number, mod = mod_evening)
smoothed_evening <- dlmSmooth(filtered_evening)
resids_evening <- residuals(filtered_evening, sd = FALSE)
mu_evening <- dropFirst(smoothed_evening$s[, 1])
gammas_evening <- dropFirst(smoothed_evening$s[, 2])
betas_evening <- dropFirst(smoothed_evening$s[, 8:17])


#Plot trend component
require(gridExtra)
plot1 <- ggplot() + geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening$date, mu_evening)), aes(x=evening$date, y=mu_evening, colour="Smoothed Trend"),size=1.5) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Smoothed Trend" = "seagreen3")) +
  ggtitle("Smoothed Trend for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold"))+
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))



#Add seasonal and regression components
reg_evening <- apply(betas_evening*X_evening, 1, sum)
alpha_evening <- mu_evening + gammas_evening + reg_evening


plot2 <- ggplot() + geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening$date, alpha_evening)), aes(x=evening$date, y=alpha_evening, colour="Smoothed Series"),size=0.4) +
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Smoothed Series" = "plum3")) +
  ggtitle("Smoother for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "right", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 9, face="italic"), plot.title=element_text(size=15, face="bold")) +
  ylim(0, 70) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))


grid.arrange(plot1, plot2, ncol=1)


#One step ahead prediction for whole time-series
conf.low_evening <- filtered_evening$f - residuals(filtered_evening)$sd * qnorm(0.975) 
conf.up_evening <-  filtered_evening$f + residuals(filtered_evening)$sd * qnorm(0.975) 

ggplot(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) +
  geom_line(data=evening, aes(y=number, x=date, colour="Evening Time Series"), size=0.7) + 
  geom_line(data=data.frame(cbind(evening$date, filtered_evening$f)), aes(x=evening$date, y=filtered_evening$f, colour="Predictions"),size=0.5) +
  geom_ribbon(aes(ymin=conf.low_evening, ymax=conf.up_evening), alpha=0.2, fill = "steelblue", color = NA, linetype = "dotted")+
  scale_color_manual(name = "Legend:", values = c("Evening Time Series" = "black", "Predictions" = "steelblue4")) +
  ggtitle("One Step Forecast for Evening Time Series") + labs(x="Date",y="Number of People") +
  theme(legend.position = "bottom", legend.title = element_text(color = "black", size = 7, face="bold"), 
        legend.text = element_text(color = "black", size = 10, face="italic"), plot.title=element_text(size=15, face="bold"))+
  coord_cartesian(ylim=c(-10,70)) + theme(axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"))



#Check residuals
plot1 <- ggplot(data.frame(resids_evening), aes(x=resids_evening)) + 
  geom_histogram(color="black", aes(y=..density..),fill="yellow",bins = 20)+
  geom_density(alpha=.2, fill="yellow4") + xlab('Evening Residuals') + ylab('Frequency')+
  coord_cartesian(xlim=c(-2,2))
plot2 <- ggacf(resids_evening, title="")
grid.arrange(plot1, plot2, ncol=1)

Box.test(resids_evening, lag = 12, type = "Ljung", fitdf = 1)
shapiro.test(resids_evening)

mean((morning$number[-which(is.na(morning$number))] - alpha_morning[-which(is.na(morning$number))])^2)
mean((noon$number[-which(is.na(noon$number))] - alpha_noon[-which(is.na(noon$number))])^2)
mean((evening$number[-which(is.na(evening$number))] - alpha_evening[-which(is.na(evening$number))])^2)

mean((morning$number[900:933][-which(is.na(morning$number[900:933]))] - filtered_morning$f[900:933][-which(is.na(morning$number[900:933]))])^2)
mean((noon$number[900:932][-which(is.na(noon$number[900:932]))] - filtered_noon$f[900:932][-which(is.na(noon$number[900:932]))])^2)
mean((evening$number[900:931][-which(is.na(evening$number[900:931]))] - filtered_evening$f[900:931][-which(is.na(evening$number[900:931]))])^2)


