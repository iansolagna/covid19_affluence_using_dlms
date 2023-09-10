#####ZONA#####


#Without zona
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 3
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  


#With zona
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain","zona", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 10
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  




#Without zona 
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "rain", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 3
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  

#With zona
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 10
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  



#Without zona
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "rain", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number,  dlmModReg(X_evening, addInt=F))
n.coef_evening <- 3
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  

#With zona
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number, dlmModReg(X_evening, addInt=F))
n.coef_evening <- 10
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  



#####TEMPERATURE#####


#Without temperature
X_morning <- model.matrix(~., data=morning[,c( "zona", "rain", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 9
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  


#With temperature
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain","zona", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 10
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  




#Without tempearture
X_noon <- model.matrix(~., data=noon[,c("zona", "rain", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 9
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  

#With tempertaure
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 10
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  



#Without temperature
X_evening <- model.matrix(~., data=evening[,c("zona", "rain", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number,  dlmModReg(X_evening, addInt=F))
n.coef_evening <- 9
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  

#With temperature
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number, dlmModReg(X_evening, addInt=F))
n.coef_evening <- 10
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  



#####FESTIVE#####


#Without festive
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain", "zona")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 9
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  


#With festive
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain","zona", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 10
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  




#Without festive 
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "rain", "zona")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 9
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  

#With festive
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 10
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  



#Without festive
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "rain", "zona")])[,-1]
loglik_evening <- dlmLL(evening$number,  dlmModReg(X_evening, addInt=F))
n.coef_evening <- 9
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  

#With festive
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number, dlmModReg(X_evening, addInt=F))
n.coef_evening <- 10
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  


#####RAIN#####


#Without rain
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "zona", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 9
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  


#With rain
X_morning <- model.matrix(~., data=morning[,c("mean_temperature", "rain","zona", "weekend_festive")])[,-1]
loglik_morning <- dlmLL(morning$number,dlmModReg(X_morning, addInt=F))
n.coef_morning <- 10
(2 * (loglik_morning)) + 2 * (sum(n.coef_morning))  




#Without rain
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "zona", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 9
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  

#With rain
X_noon <- model.matrix(~., data=noon[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_noon <- dlmLL(noon$number,dlmModReg(X_noon, addInt=F))
n.coef_noon <- 10
(2 * (loglik_noon)) + 2 * (sum(n.coef_noon))  



#Without rain
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "zona", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number,  dlmModReg(X_evening, addInt=F))
n.coef_evening <- 9
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  

#With rain
X_evening <- model.matrix(~., data=evening[,c("mean_temperature", "rain", "zona", "weekend_festive")])[,-1]
loglik_evening <- dlmLL(evening$number, dlmModReg(X_evening, addInt=F))
n.coef_evening <- 10
(2 * (loglik_evening)) + 2 * (sum(n.coef_evening))  








