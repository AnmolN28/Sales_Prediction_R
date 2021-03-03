#IMPORT DATASET
salesdata <- read.csv(file.choose())

#Finding missing data
sum(is.na(salesdata))

#Finding Improper data
sum(salesdata$item_cnt_day<0)
sum(salesdata$item_price<0)

#Convert to NA
salesdata$item_cnt_day <- ifelse(salesdata$item_cnt_day < 0, salesdata$item_cnt_day == NA, salesdata$item_cnt_day)
salesdata$item_price <- ifelse(salesdata$item_price < 0, salesdata$item_price == NA, salesdata$item_price)

#Replacing NA values with mean
mean1 <- round(mean(salesdata$item_cnt_day, na.rm = TRUE))
salesdata[c("item_cnt_day")][is.na(salesdata[c("item_cnt_day")])] <- mean1
mean2 <- round(mean(salesdata$item_price, na.rm = TRUE))
salesdata[c("item_price")][is.na(salesdata[c("item_price")])] <- mean2
                      
#Split the data
testdata <- subset(salesdata, salesdata$date_block_num==33)
traindata <- subset(salesdata, salesdata$date_block_num!=33)


#Checking NA values
sum(is.na(traindata))

#Outlier Detection
plot(salesdata$item_cnt_day~salesdata$date_block_num,xlab="Month",ylab="Total Sales",main="Scatter Plot")


#LINEAR MODEL
tic("Linear Model")
Linearmodel <- lm(item_cnt_day ~ shop_id+item_id+item_price+date_block_num, data = traindata)
toc()
#IT TAKES A LOT OF TIME TO PLOT THE GRAPH
plot(Linearmodel)

#SUMMARY OF LINEAR MODEL
summary(Linearmodel)

#RESULT
result = predict(Linearmodel, testdata[,c("shop_id","item_id","item_price","date_block_num")])

#MSE AND RMSE
install.packages("Metrics")
library(Metrics)
mse(testdata$item_cnt_day, predict(Linearmodel,testdata))
rmse(testdata$item_cnt_day, predict(Linearmodel, testdata))

#VIEW RESULT
appnd <- subset(testdata, select=c("shop_id", "item_id"))
result <- round(result)
result <- cbind(result,appnd)

#COMPARING ACTUAL AND PREDICTED DATA
predicted = result$result
actual = traindata$item_cnt_day
actual_pred = data.frame(cbind(actuals = actual,predicted =predicted))
correlation_acc =cor(actual_pred)
correlation_acc
View(actual_pred)

#CONF INTERVAL FOR LINEAR MODEL
install.packages("Rmisc")
library(Rmisc)
mean1<- mean(result$result)
sd1<- sd(result$result)
n <- nrow(result)
#80%
z1<- qnorm(0.90)
margine1<- z1*(sd1/sqrt(n))
CI_upper1 <- mean1 + margine1
CI_lower1 <- mean1- margine1
CI_upper1
CI_lower1

#90%
z2<- qnorm(0.95)
margine2<- z2*(sd1/sqrt(n))
CI_upper2 <- mean1 +margine2
CI_lower2 <- mean1 - margine2
CI_upper2 
CI_lower2

#95%
z3 <- qnorm(0.975)
margine3<- z3*(sd1/sqrt(n))
CI_upper3 <- mean1 +margine3
CI_lower3 <- mean1 - margine3
CI_upper3 
CI_lower3

#GBM MODEL
install.packages("gbm")
library(gbm)
install.packages("tictoc")
library(tictoc)

tic("GBM Model ")
gbm_model  =  gbm(item_cnt_day ~ shop_id + item_id + date_block_num + item_price,
                  data = traindata,
                  shrinkage = 0.01,
                  distribution = "gaussian",
                  n.trees = 5,
                  interaction.depth = 5, 
                  bag.fraction = 0.5,
                  train.fraction = 0.8,
                  # cv.folds = 5,
                  n.cores = -1,
                  verbose = T)
toc()

#PREDICTION FOR GBM
result2 = predict(gbm_model,data = testdata[,c("shop_id","item_id","item_price","date_block_num")], n.trees = 5)
View(round(result2))

#MSE AND RMSE
mse(testdata$item_cnt_day, predict(gbm_model,testdata))
rmse(testdata$item_cnt_day, predict(gbm_model, testdata))

#CONF Interval for GBM
mean1<- mean(result$result)
sd1<- sd(result$result)
n1 <- nrow(result)
#80%
z4<- qnorm(0.90)
margine1<- z4*(sd1/sqrt(n1))
CI_upper4 <- mean1 + margine1
CI_lower4 <- mean1- margine1
CI_upper4
CI_lower4

#90%
z5<- qnorm(0.95)
margine2<- z5*(sd1/sqrt(n1))
CI_upper5 <- mean1 +margine2
CI_lower5 <- mean1 - margine2
CI_upper5 
CI_lower5

#95%
z3 <- qnorm(0.975)
margine3<- z3*(sd1/sqrt(n1))
CI_upper6 <- mean1 +margine3
CI_lower6 <- mean1 - margine3
CI_upper6 
CI_lower6

