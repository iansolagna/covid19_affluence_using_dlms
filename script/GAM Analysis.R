rm(list=ls())
setwd("~/Documents/Laurea Magistrale/Tesi/datasets")
load("/Users/ian/Documents/Laurea Magistrale/Tesi/datasets/Datasets.Rdata")

#Convert factors to factors

data_number$time <- as.factor(data_number$time)
data_number$time <- relevel(data_number$time, ref="Morning")

data_number$zona <- as.factor(data_number$zona)
data_number$zona <- relevel(data_number$zona, ref="no-emergency")

data_number$luce <- as.factor(data_number$luce)

data_number$weekend_festive <- as.factor(data_number$weekend_festive)

data_number$season <- as.factor(data_number$season)
data_number$season <- relevel(data_number$season, ref="winter")


indexes <- 1300:1421

train <- data_number[-indexes,c("number","time", "zona","luce","mean_temperature","weekend_festive","rain","season")]

test <- data_number[indexes, c("number","time", "zona","luce","mean_temperature","weekend_festive","rain","season")]


#Use GAMs 

library(gam)
par(mfrow=c(2,1))

test_gam <- gam::gam(number~ time+ zona + luce + s(mean_temperature)+
                  weekend_festive+ s(rain)+ season, data=train)

summary(test_gam)

par(mfrow = c(1,1))
plot(test_gam, se = T, terms="zona")


df_list = paste0("df=", 2:20)
df_list

gam_list = gam::gam.scope(data_number[,c("number","time", "zona","luce","mean_temperature","weekend_festive","rain","season")],
                     response = 1,
                     arg = df_list,
                     smoother = "s")

gam_null = gam::gam(number ~ 1, data = train)
gam_gaussian = gam::step.Gam(gam_null, scope = gam_list, trace = T, direction="both")

summary(gam_gaussian)

par(mfrow = c(3,3))
plot(gam_gaussian, se=T)
par(mfrow = c(1,1))


#Let's try normal GAM but with poisson family

train_log <- train
test_log <- test

log_number_train <- log(train$number)
log_number_test <- log(test$number)

train_log["number"] <- log_number_train
test_log["number"] <- log_number_test

gam_null = gam::gam(number ~ 1, data = train_log)
gam_poisson = gam::step.Gam(gam_null, scope = gam_list, trace = T)

summary(gam_poisson)


#Predictions
pred_gaussian <- predict(gam_gaussian, test)
pred_poisson <- exp(predict(gam_poisson, test_log))

mean((pred_gaussian-test$number)^2)
mean((pred_poisson-test$number)^2)


#Use whole dataset and check on autocorrelation

df_list = paste0("df=", 2:20)
df_list

gam_list = gam::gam.scope(data_number[,c("number","time", "zona","luce","mean_temperature","weekend_festive","rain","season")],
                          response = 1,
                          arg = df_list,
                          smoother = "s")

gam_null = gam::gam(number ~ 1, data = data_number)
gam_ts = gam::step.Gam(gam_null, scope = gam_list, trace = T)

summary(gam_ts)


#SPAM
library(sparseGAM)

y <- train$number
X<- model.matrix(~.-1, train[, c("time", "zona","luce","mean_temperature","weekend_festive","rain","season")])
X_test <- model.matrix(~.-1, test[, c("time", "zona","luce","mean_temperature","weekend_festive","rain","season")])

spam_spline <- SFGAM(y, X, df=4, family="gaussian")
matplot(spam_spline$lambda, t(spam_spline$beta), type = "l", xlab = "Regularization Parameters", 
        ylab = "Functional Norms", cex.lab = 2, log = "x", lwd = 2, col=rep(1:33, each=4), lty=rep(1:33, each=4))

lambdas <- seq(0.001, 1, length=200)
error <- rep(0,100)
c<-1
for (i in lambdas){
  model <- SFGAM(y, X, df=4, X.test=X_test, family="gaussian", lambda=i)
  error[c] <- mean((model$mu.pred-test$number)^2)
  c <- c+1
}

#Check autocorrelations
library(stats)

par(mfrow=c(1,2))
pacf(data_number$number, lag.max = 100)
acf(gam_ts$residuals, lag.max=100)


par(mfrow=c(1,1))
plot(data_number$number, type="l")
lines(predict(gam_ts,data_number), col="red")












