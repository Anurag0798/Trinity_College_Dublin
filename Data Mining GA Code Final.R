#Read dataset
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

View(telco)
dim(telco)

#EDA
#check for NA values
sum(is.na(telco))
#Remove observations with NA values
na.indices <- rowSums(is.na(telco))>0
telco <- telco[!na.indices,]
dim(telco)

#Check class of each variable
lapply(telco,class)

library(skimr)
skim(telco)

#Check class imbalance. 5163 "No" and 1869 "Yes", requires oversampling for the class "Yes"
table(telco$Churn)

library(tidyverse)
#Check distribution of continuous variables - Tenure, MonthlyCharges, TotalCharges
plot(density(telco$tenure)) #not normally distributed
hist(telco$tenure)

plot(density(telco$MonthlyCharges)) #not normally distributed
hist(telco$MonthlyCharges)

plot(density(telco$TotalCharges)) #not normally distributed
hist(telco$TotalCharges)

#Check outliers for continuous variables - Tenure, MonthlyCharges, TotalCharges
ggplot(data=telco, mapping=aes(x=tenure))+geom_boxplot() #No outliers
ggplot(data=telco, mapping=aes(x=MonthlyCharges))+geom_boxplot() #No outliers
ggplot(data=telco, mapping=aes(x=TotalCharges))+geom_boxplot() #No outliers

#Check distribution of categorical variables
ggplot(data=telco)+geom_bar(mapping=aes(x=gender))
ggplot(data=telco)+geom_bar(mapping=aes(x=SeniorCitizen))
ggplot(data=telco)+geom_bar(mapping=aes(x=Partner))
ggplot(data=telco)+geom_bar(mapping=aes(x=Dependents))
ggplot(data=telco)+geom_bar(mapping=aes(x=PhoneService))
ggplot(data=telco)+geom_bar(mapping=aes(x=MultipleLines))
ggplot(data=telco)+geom_bar(mapping=aes(x=InternetService))
ggplot(data=telco)+geom_bar(mapping=aes(x=OnlineSecurity))
ggplot(data=telco)+geom_bar(mapping=aes(x=OnlineBackup))
ggplot(data=telco)+geom_bar(mapping=aes(x=DeviceProtection))
ggplot(data=telco)+geom_bar(mapping=aes(x=TechSupport))
ggplot(data=telco)+geom_bar(mapping=aes(x=StreamingMovies))
ggplot(data=telco)+geom_bar(mapping=aes(x=Contract))
ggplot(data=telco)+geom_bar(mapping=aes(x=PaperlessBilling))
ggplot(data=telco)+geom_bar(mapping=aes(x=PaymentMethod))
ggplot(data=telco)+geom_bar(mapping=aes(x=Churn))

#Converting binary variables to 1 and 0 and then converting them to factors
#gender
telco$gender <- ifelse(telco$gender=="Male",1,telco$gender)
telco$gender <- ifelse(telco$gender=="Female",0,telco$gender)
telco$gender <- as.factor(telco$gender)
class(telco$gender)
levels(telco$gender)
#SeniorCitizen
telco$SeniorCitizen <- as.factor(telco$SeniorCitizen)
class(telco$gender)
levels(telco$gender)
#Partner
telco$Partner <- ifelse(telco$Partner=="Yes",1,telco$Partner)
telco$Partner <- ifelse(telco$Partner=="No",0,telco$Partner)
telco$Partner <- as.factor(telco$Partner)
class(telco$Partner)
levels(telco$Partner)
#Dependents
telco$Dependents <- ifelse(telco$Dependents =="Yes",1,telco$Dependents )
telco$Dependents  <- ifelse(telco$Dependents =="No",0,telco$Dependents )
telco$Dependents  <- as.factor(telco$Dependents )
class(telco$Dependents)
levels(telco$Dependents)
#PhoneService
telco$PhoneService <- ifelse(telco$PhoneService =="Yes",1,telco$PhoneService )
telco$PhoneService  <- ifelse(telco$PhoneService =="No",0,telco$PhoneService )
telco$PhoneService  <- as.factor(telco$PhoneService)
class(telco$PhoneService)
levels(telco$PhoneService)
#MultipleLines
telco$MultipleLines <- ifelse(telco$MultipleLines =="Yes",1,telco$MultipleLines)
telco$MultipleLines <- ifelse(telco$MultipleLines =="No",0,telco$MultipleLines)
telco$MultipleLines <- ifelse(telco$MultipleLines =="No phone service",0,telco$MultipleLines)
telco$MultipleLines  <- as.factor(telco$MultipleLines)
class(telco$MultipleLines)
levels(telco$MultipleLines)
#InternetService
unique(telco$InternetService)
telco$InternetService_DSL <- 1*(telco$InternetService=="DSL")
telco$InternetService_FiberOptic <- 1*(telco$InternetService=="Fiber optic")
telco$InternetService_DSL  <- as.factor(telco$InternetService_DSL)
telco$InternetService_FiberOptic  <- as.factor(telco$InternetService_FiberOptic)
class(telco$InternetService_DSL)
levels(telco$InternetService_DSL)
class(telco$InternetService_FiberOptic )
levels(telco$InternetService_FiberOptic )
#OnlineSecurity
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity =="Yes",1,telco$OnlineSecurity)
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity =="No",0,telco$OnlineSecurity)
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity =="No internet service",0,telco$OnlineSecurity)
telco$OnlineSecurity  <- as.factor(telco$OnlineSecurity)
class(telco$OnlineSecurity)
levels(telco$OnlineSecurity)
#OnlineBackup
telco$OnlineBackup <- ifelse(telco$OnlineBackup =="Yes",1,telco$OnlineBackup)
telco$OnlineBackup <- ifelse(telco$OnlineBackup =="No",0,telco$OnlineBackup)
telco$OnlineBackup <- ifelse(telco$OnlineBackup =="No internet service",0,telco$OnlineBackup)
telco$OnlineBackup  <- as.factor(telco$OnlineBackup)
class(telco$OnlineBackup)
levels(telco$OnlineBackup)
#DeviceProtection
telco$DeviceProtection <- ifelse(telco$DeviceProtection =="Yes",1,telco$DeviceProtection)
telco$DeviceProtection <- ifelse(telco$DeviceProtection =="No",0,telco$DeviceProtection)
telco$DeviceProtection <- ifelse(telco$DeviceProtection =="No internet service",0,telco$DeviceProtection)
telco$DeviceProtection  <- as.factor(telco$DeviceProtection)
class(telco$DeviceProtection)
levels(telco$DeviceProtection)
#TechSupport
telco$TechSupport <- ifelse(telco$TechSupport =="Yes",1,telco$TechSupport)
telco$TechSupport <- ifelse(telco$TechSupport =="No",0,telco$TechSupport)
telco$TechSupport <- ifelse(telco$TechSupport =="No internet service",0,telco$TechSupport)
telco$TechSupport  <- as.factor(telco$TechSupport)
class(telco$TechSupport)
levels(telco$TechSupport)
#StreamingTV
telco$StreamingTV <- ifelse(telco$StreamingTV =="Yes",1,telco$StreamingTV)
telco$StreamingTV <- ifelse(telco$StreamingTV =="No",0,telco$StreamingTV)
telco$StreamingTV <- ifelse(telco$StreamingTV =="No internet service",0,telco$StreamingTV)
telco$StreamingTV  <- as.factor(telco$StreamingTV)
class(telco$StreamingTV)
levels(telco$StreamingTV)
#StreamingMovies
telco$StreamingMovies <- ifelse(telco$StreamingMovies =="Yes",1,telco$StreamingMovies)
telco$StreamingMovies <- ifelse(telco$StreamingMovies =="No",0,telco$StreamingMovies)
telco$StreamingMovies <- ifelse(telco$StreamingMovies =="No internet service",0,telco$StreamingMovies)
telco$StreamingMovies  <- as.factor(telco$StreamingMovies)
class(telco$StreamingMovies)
levels(telco$StreamingMovies)
#Contract
telco$Contract_Month <- 1*(telco$Contract=="Month-to-month")
telco$Contract_1Year <- 1*(telco$Contract=="One year")
telco$Contract_Month <- as.factor(telco$Contract_Month)
telco$Contract_1Year <- as.factor(telco$Contract_1Year)
class(telco$Contract_Month)
class(telco$Contract_1Year)
#PaperlessBilling
telco$PaperlessBilling <- ifelse(telco$PaperlessBilling =="Yes",1,telco$PaperlessBilling)
telco$PaperlessBilling <- ifelse(telco$PaperlessBilling =="No",0,telco$PaperlessBilling)
telco$PaperlessBilling <- as.factor(telco$PaperlessBilling)
class(telco$PaperlessBilling)
levels(telco$PaperlessBilling)
#PaymentMethod
table(telco$PaymentMethod)
telco$PaymentMethod_BankTransfer <- 1*(telco$PaymentMethod=="Bank transfer (automatic)")
telco$PaymentMethod_CreditCard <- 1*(telco$PaymentMethod=="Credit card (automatic)")
telco$PaymentMethod_ECheck <- 1*(telco$PaymentMethod=="Electronic check")
telco$PaymentMethod_BankTransfer <- as.factor(telco$PaymentMethod_BankTransfer)
telco$PaymentMethod_CreditCard <- as.factor(telco$PaymentMethod_CreditCard)
telco$PaymentMethod_ECheck <- as.factor(telco$PaymentMethod_ECheck)
#Churn
telco$Churn <- ifelse(telco$Churn =="Yes",1,telco$Churn)
telco$Churn <- ifelse(telco$Churn =="No",0,telco$Churn)
telco$Churn <- as.factor(telco$Churn)
levels(telco$Churn)

#Rearranging the columns, keeping dummy variables, discarding original variables
churn <- telco %>% 
  as_tibble() %>% 
  select(gender,SeniorCitizen,Partner,Dependents,tenure,PhoneService,MultipleLines,InternetService_DSL,
         InternetService_FiberOptic,OnlineSecurity,OnlineBackup,DeviceProtection,
         TechSupport,StreamingTV,StreamingMovies,Contract_Month,Contract_1Year,PaperlessBilling,
         PaymentMethod_BankTransfer,PaymentMethod_CreditCard,PaymentMethod_ECheck,MonthlyCharges,TotalCharges,Churn)
View(churn)

#Rebalancing - using package "ROSE" (Random Over Sampling Examples)
install.packages("ROSE")
library(ROSE)

#Re-balancing and saving into new dataframe churnBS i.e. Churn Balanced Sample
churnBS <- ovun.sample(Churn~., data = churn, method = "both", p=0.5, seed= 123, N=7032)$data
table(churnBS$Churn)
ggplot(data=churnBS)+geom_bar(mapping=aes(x=Churn))
dim(churnBS)
View(churnBS)

#Re-balancing and saving into new dataframe churnOS i.e. Churn Over Sampled
churnOS <- ovun.sample(Churn~., data = churn, method = "over", p=0.5, seed= 123)$data
table(churnOS$Churn)
ggplot(data=churnOS)+geom_bar(mapping=aes(x=Churn))
dim(churnOS)
View(churnOS)

#train-test split
set.seed(1234)

ind <- sample(2, nrow(churnOS), replace = T, prob = c(0.7,0.3))
training <- churnOS[ind==1,]
test <- churnOS[ind==2,]

#Scaling
# Identify numeric and factor columns
numeric_columns <- sapply(churnOS, is.numeric)
factor_columns <- sapply(churnOS, is.factor)

# Extract numeric and factor columns
numeric_data <- churnOS[, numeric_columns, drop = FALSE]
factor_data <- churnOS[, factor_columns, drop = FALSE]

# Scale the numeric columns
scaled_numeric_data <- as.data.frame(scale(numeric_data))

# Combine the scaled numeric columns with the original factor columns
scaled_df <- cbind(scaled_numeric_data, factor_data)

set.seed(987)
training1 <- scaled_df[ind==1,]
test1 <- scaled_df[ind==2,]





#KNN Model using caret
library(caret)
#using cross validation. Accuracy is a parameter to select knn
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
#scaling and fitting simultaneously
set.seed(321)
fit <- train(Churn ~ .,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center","scale"))
fit
plot(fit)
varImp(fit)
plot(varImp(fit))

#predicting 
pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$Churn, positive = '1')
class(pred)

library(pROC)
predknn <- predict(fit, newdata = test, type = 'prob')
predknn1 <- predknn[,2]
roc_curve_knn <- roc(test$Churn, predknn1)
plot(roc_curve_knn, main = "ROC Curve for KNN", col = "blue", lwd = 2)
auc_value_knn <- auc(roc_curve_knn)
text(0.8, 0.2, paste("AUC =", round(auc_value_knn, 2)), col = "blue", cex = 1.2)





#logistic regression
m <- glm(Churn ~ ., data = training1, family = "binomial")
summary(m)

#predictions for training data
p1 <- predict(m, training1, type = 'response')

#confusion matrix for training data
pred1 <- ifelse(p1>0.5, 1, 0)
table(Predicted = pred1, Actual =training1$Churn)
pred1 <- as.factor(pred1)
confusionMatrix(pred1, training1$Churn, positive='1')

#predictions for testing data
p2 <- predict(m, test1, type = 'response')

#confusion matrix for training data
pred2 <- ifelse(p2>0.5, 1, 0)
pred2 <- as.factor(pred2)
table(Predicted = pred2, Actual = test1$Churn)
confusionMatrix(pred2, test1$Churn, positive='1')

#plot ROC curve
library(pROC)
roc_curve <- roc(test1$Churn, p2)
plot(roc_curve, main = "ROC Curve for Testing Data", col = "blue", lwd = 2)
auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue", cex = 1.2)

#Now, only taking significant columns
summary_model <- summary(m)
insignificant_columns <- names(which(summary_model$coefficients[, "Pr(>|z|)"] > 0.05))
print(insignificant_columns)

m1 <- glm(Churn ~ ., data = training1[,-c(1,3,4,6,10,11,12,13,15,22)], family = "binomial")
summary(m1)

#taking predictions
p3 <- predict(m1, training1, type = "response")

#confusion matrix for training data
pred3 <- ifelse(p3>0.5, 1,0)
pred3 <- as.factor(pred3)
confusionMatrix(pred3 ,training1$Churn, positive='1')

#taking predictions
p4 <- predict(m1, test1, type = "response")
class(p4)
#confusion matrix for test data
pred4 <- ifelse(p4>0.5, 1,0)
pred4 <- as.factor(pred4)
confusionMatrix(pred4 ,test1$Churn, positive='1')

#plotting ROC curve
roc_curve_log <- roc(test1$Churn, p4)
plot(roc_curve_log, main = "ROC Curve for Logistic Regresion", col = "blue", lwd = 2)
auc_value_log <- auc(roc_curve_log)
text(0.8, 0.2, paste("AUC =", round(auc_value_log, 2)), col = "blue", cex = 1.2)





#Random Forest
library(randomForest)
set.seed(1234)
#simple mode;
rf <- randomForest(Churn ~., data = training1)
print(rf)

#confusion matrix
rf$confusion
plot(rf)
#predict training model
#p4 <- predict(rf, training1)
#confusionMatrix(p4, training1$Churn)

#predict testing model
p5 <- predict(rf, test1)
confusionMatrix(p5, test1$Churn, positive='1')

#tune mtry (reducing OOb error by 0.05 percent)
t<- tuneRF(training1[,-24],training1[,24],
           stepFactor = 0.5,
           plot = TRUE,
           ntreeTry = 300,
           trace = TRUE,
           improve = 0.05)
#here, we can see the out of the box error is minimum at mtry 16, so now we will change our code a bit
rf1 <- randomForest(Churn ~., data = training1,
                    ntree = 300,
                    mtry = 16,
                    importance = TRUE,
                    proximity = TRUE)

plot(rf1)
varImpPlot(rf1)
p8 <- predict(rf1, test1, type = "response")
p8 <- as.numeric(p8)
#plotting roc curve
roc_curve_rf <- roc(test1$Churn, p8)
plot(roc_curve_rf, main = "ROC Curve for Random Forest", col = "blue", lwd = 2)
auc_value_rf <- auc(roc_curve_rf)
text(0.8, 0.2, paste("AUC =", round(auc_value_rf, 2)), col = "blue", cex = 1.2)

p7 <- predict(rf1, test1)
confusionMatrix(p7, test1$Churn, positive='1')





#Decision tree
library(rpart)
library(rpart.plot)
train.dt <- rpart(Churn ~ .,
              data = training1,
              method = 'class',
              cp = 0.001,
              maxdepth = 5,
              model = TRUE)
prp(train.dt,
    type = 1,
    extra = 1,
    under = TRUE,
    split.font = 1,
    varlen = -10)

train.dt$cptable

prunefit <- prune(train.dt, cp=train.dt$cptable[which.min(train.dt$cptable[,'xerror']),'CP'])
prp(prunefit)

summary(prunefit)

#load the caret package
library(caret)

# Make predictions on the validation set
predictions <- predict(prunefit, newdata = test1, type = "class")

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, test1$Churn, positive='1')
conf_matrix

# Install and load the pROC package
install.packages("pROC")
library(pROC)

# Create a ROC curve
roc_curve_dt <- roc(as.numeric(predictions == "1"), as.numeric(test$Churn == "1"))
# Plot the ROC curve
plot(roc_curve_dt, main = "ROC Curve", col = "blue", lwd = 2)
auc_value_dt <- auc(roc_curve_dt)
text(0.8, 0.2, paste("AUC =", round(auc_value_dt, 2)), col = "blue", cex = 1.2)





#Neural Network
library(caret)
library(neuralnet)

churnOS1 <- data.frame(churnOS)

m <- model.matrix( 
  ~ ., 
  data = churnOS1 
)

class(m)

churnOSnew <- as.data.frame(m)

class(churnOSnew)

#train-test split
set.seed(1234)
ind1 <- sample(2, nrow(churnOSnew), replace = T, prob = c(0.7,0.3))
training2 <- churnOSnew[ind1==1,]
test2 <- churnOSnew[ind1==2,]

norm.values1 <- preProcess(training2, method="range")
train.norm.df1 <- predict(norm.values1, training2)
test.norm.df1 <- predict(norm.values1, test2)

str(churnOSnew)

nn <- neuralnet(
  Churn1 ~ gender1 + SeniorCitizen1 + Partner1 + Dependents1 + tenure + PhoneService1 + MultipleLines1 + 
    InternetService_DSL1 + InternetService_FiberOptic1 + OnlineSecurity1 + OnlineBackup1 + 
    DeviceProtection1 + TechSupport1 + StreamingTV1 + StreamingMovies1 + Contract_Month1 + 
    Contract_1Year1 + PaperlessBilling1 + PaymentMethod_BankTransfer1 + PaymentMethod_CreditCard1 + 
    PaymentMethod_ECheck1 + MonthlyCharges + TotalCharges,
  data = train.norm.df1, 
  linear.output = FALSE,  # Use nonlinear activation for binary classification
  hidden = 8  # Number of neurons in each hidden layer
)

# Plot the neural network
plot(nn)

# Make predictions on the test set
predictions <- predict(nn, newdata = test.norm.df1)

# Convert predictions to binary (1 or 0)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

#confusionMatrix(as.factor(test2$Churn > 0.5), as.factor(predicted_labels > 0.5), positive='TRUE')
confusionMatrix(as.factor(predicted_labels > 0.5), as.factor(test2$Churn > 0.5), positive='TRUE')


# Optionally, plot the ROC curve
library(pROC)
roc_curve_nn <- roc(test2$Churn, as.numeric(predicted_labels))
plot(roc_curve_nn, main = "ROC Curve", col = "blue", lwd = 2)
auc_value_nn <- auc(roc_curve_nn)
text(0.8, 0.2, paste("AUC =", round(auc_value_nn, 2)), col = "blue", cex = 1.2)





#plotting ROC curves for all
# Plot the first ROC curve
plot(roc_curve_knn, col = "blue", lwd = 2, main = "ROC Curves Comparison", col.main = "black", cex.main = 1.2)

# Add the second ROC curve to the plot
lines(roc_curve_log, col = "red", lwd = 2)

# Add the third ROC curve to the plot
lines(roc_curve_rf, col = "darkgreen", lwd = 2)

# Add the fourth ROC curve to the plot
lines(roc_curve_dt, col = "purple", lwd = 2)

# Add the fifth ROC curve to the plot
lines(roc_curve_nn, col = "orange", lwd = 2)

# Add legends
legend("bottomright", legend = c("KNN", "Logistic Regression", "Random Forest", "Decision Tree", "Neural Network"), col = c("blue", "red", "darkgreen", "purple", "orange"), lwd = 2)
text(1.1, 1.0, paste("AUC =", round(auc(roc_curve_knn), 2)), col = "blue", cex = 0.8)
text(1.1, 0.95, paste("AUC =", round(auc(roc_curve_log), 2)), col = "red", cex = 0.8)
text(1.1, 0.9, paste("AUC =", round(auc(roc_curve_rf), 2)), col = "darkgreen", cex = 0.8)
text(1.1, 0.85, paste("AUC =", round(auc(roc_curve_dt), 2)), col = "purple", cex = 0.8)
text(1.1, 0.80, paste("AUC =", round(auc(roc_curve_nn), 2)), col = "orange", cex = 0.8)





library(adabag)
library(rpart)
library(caret)

#Bagging
bag <- bagging(Churn ~ ., data = training1)
pred <- predict(bag, test1, type = "class")
confusionMatrix(as.factor(pred$class), test1$Churn)

#Boosting
boost <- boosting(Churn ~ ., data = training)
pred <- predict(boost, test, type = "class")
confusionMatrix(as.factor(pred$class), test$Churn)
