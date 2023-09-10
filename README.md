# covid19_affluence_using_dlms
Modeling the number of people in Venice during COVID-19: a State Space approach

If you want the photos to recreate the work message me!!!

This was my master thesis

Inside script you will find:

-AIC.R computes the AIC for different configurations of the Dynamic Linear Model
-Cluster Creation.R creates the clusters of people trough DBSCAN using their distances 
-Datasets Creation.R creates datasets in rdata format to make it easier
-Dynamic Linear Models Cluster.R fits dynamic linear models on the number of clusters at Campo San Felice in Venice
-Dynamic Linear Models People.R fits dynamic linear models on the number of people at Campo San Felice in Venice
-Dynamic Linear Models Variance Within.R fits dynamic linear models on the average variance within between clusters at Campo San Felice in Venice
-Prophet Cluster.R creates fits prophet model on the number of clusters at Campo San Felice in Venice
-Prophet People.R fits prophet model on the number of people at Campo San Felice in Venice
-Prophet Variance Within.R fits prophet model on the average variance within between clusters at Campo San Felice in Venice
-EDA.R perform exploratory data analysis
-final plots.R creates plots using ggplot
-interpretation.R using dynamic linear models coefficients interprets them 
