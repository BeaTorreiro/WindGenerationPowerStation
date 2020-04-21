# PRÁCTICA FINAL - MODELOS
# Beatriz Torreiro Mosquera

library(dplyr)
library(tidyr)
library(MLTools)
library(ggplot2)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(MLTools)
library(caret)
library(Metrics)
library(tseries)
library(fpp2)
library(lmtest) 
library(rpart)
library(rpart.plot)
library(partykit)
library(pROC)
library(cluster)
library(cclust)
library(dendextend)
library(stats)
library(factoextra)
library(NbClust)
#Lectura del dataset
data <- read.csv("TTOTdataEEM17v3.csv")

#Create an hourly dataset containing average values of all variables for each hour

data_hourly <- data %>%
  select(-Min) %>%
  group_by(Date,Hour) %>%
  summarise_all(mean)

###############################################################################################
########################################## PROBLEM 4 ########################################## 
###############################################################################################

# 1) Creation of a new variable 'MinimumGeneration', indicates wheter the minimum levels of generation have been reached
data_hourly$MinimumGeneration <- as.factor(ifelse(data_hourly$WG>10,1,0))


# 2) Exploratory Analysis
prop_MinimumGeneration <- with(data_hourly,prop.table(table(MinimumGeneration)))

data_hourly$Date <- as.Date(data_hourly$Date,"%Y / %m / %d")
data_hourly$Month <- strftime(data_hourly$Date,"%m")


data_classification <- data_hourly
data_classification <- data_classification %>%
  select(contains("H100"))
data_classification[,c(2:4)] <- NULL
data_classification[,c(3:12)] <- NULL
data_classification[,c(4:8)] <- NULL
data_classification$Date <- NULL
data_classification$Month <- as.numeric(data_hourly$Month)
data_classification$Hour <- data_hourly$Hour
data_classification$MinimumGeneration <- data_hourly$MinimumGeneration

ggpairs(data_classification[,c(1,2,23,24,25)],aes(color=MinimumGeneration, alpha=0.1))
ggpairs(data_classification[,c(3:12,25)],aes(color=MinimumGeneration, alpha=0.1))
ggpairs(data_classification[,c(13:22,25)],aes(color=MinimumGeneration, alpha=0.1))

# 3) Divide de data into training and validation sets
seed = 150
set.seed(seed) 
ratioTR = 0.8 
trainIndex <- createDataPartition(data_classification$MinimumGeneration,p = ratioTR,list = FALSE,times = 1)
data_train <- data_classification[trainIndex,]
levels(data_train$MinimumGeneration) <- make.names(levels(factor(data_train$MinimumGeneration)))
data_val <- data_classification[-trainIndex,]
levels(data_val$MinimumGeneration) <- make.names(levels(factor(data_val$MinimumGeneration)))

# 4) Upsampling data_train
data_train_up <- upSample(data_train,data_train$MinimumGeneration)
data_train_up$Class <- NULL
summary(data_train_up)

## Initialize trainControl
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 10,                          #Number of folds
                     summaryFunction = twoClassSummary,     #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples

#training
train_eval <- data_train_up
#validation
val_eval <- data_val

###############################################################################################
##################################### LOGISTIC REGRESSION ##################################### 
###############################################################################################
# 1) Todas las variables
set.seed(seed) #For replication
#Train model using training data
LogReg.fit <- train(form = MinimumGeneration ~ ., #formula for specifying inputs and outputs.
                    data = data_train_up,               #Training dataset 
                    method = "glm",                   #Train logistic regression
                    preProcess = c("center","scale"), #Center an scale inputs
                    trControl = ctrl,                 #trainControl Object
                    metric = "Accuracy")              #summary metric used for selecting hyperparameters
LogReg.fit          #information about the resampling
summary(LogReg.fit) #detailed information about the fit of the final model

#Evaluate the model with training and validation sets
#training
train_eval$LRprob_log_1 <- predict(LogReg.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_log_1 <- predict(LogReg.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_log_1 <- predict(LogReg.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_log_1 <- predict(LogReg.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_log_1, #Predicted classes
                reference = train_eval$MinimumGeneration)
# Validation
confusionMatrix(data = val_eval$LRpred_log_1, 
                reference = val_eval$MinimumGeneration)
#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_log_1,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_log_1,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)

# 2) MinimumGeneration ~ TL4H100 + GSRL5H100 + WSL1H100 + WSL3H100 + WSL4H100 + WSL5H100 + WSL7H100 + WDL3H100 + WDL7H100 + Month + Hour
set.seed(seed) #For replication
#Train model using training data
LogReg2.fit <- train(form = MinimumGeneration ~ TL4H100 + GSRL5H100 + WSL1H100 + WSL3H100 + WSL4H100 + WSL5H100 + WSL7H100 + WDL3H100 + WDL7H100 + Month + Hour, #formula for specifying inputs and outputs.
                    data = data_train_up,               #Training dataset 
                    method = "glm",                   #Train logistic regression
                    preProcess = c("center","scale"), #Center an scale inputs
                    trControl = ctrl,                 #trainControl Object
                    metric = "Accuracy")              #summary metric used for selecting hyperparameters
LogReg2.fit          #information about the resampling
summary(LogReg2.fit) #detailed information about the fit of the final model

#Evaluate the model with training and validation sets
#training
train_eval$LRprob_log_2 <- predict(LogReg2.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_log_2 <- predict(LogReg2.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_log_2 <- predict(LogReg2.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_log_2 <- predict(LogReg2.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_log_2, #Predicted classes
                reference = train_eval$MinimumGeneration)
# Validation
confusionMatrix(data = val_eval$LRpred_log_2, 
                reference = val_eval$MinimumGeneration)
#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_log_2,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_log_2,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)

# 3) MinimumGeneration ~ WSL1H100 + WSL3H100 + WSL4H100 + WSL5H100 + Month
set.seed(seed) #For replication
#Train model using training data
LogReg3.fit <- train(form = MinimumGeneration ~ WSL1H100 + WSL3H100 + WSL4H100 + WSL5H100 + Month, #formula for specifying inputs and outputs.
                     data = data_train_up,               #Training dataset 
                     method = "glm",                   #Train logistic regression
                     preProcess = c("center","scale"), #Center an scale inputs
                     trControl = ctrl,                 #trainControl Object
                     metric = "Accuracy")              #summary metric used for selecting hyperparameters
LogReg3.fit          #information about the resampling
summary(LogReg3.fit) #detailed information about the fit of the final model

#Evaluate the model with training and validation sets
#training
train_eval$LRprob_log_3 <- predict(LogReg3.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_log_3 <- predict(LogReg3.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_log_3 <- predict(LogReg3.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_log_3 <- predict(LogReg3.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_log_3, #Predicted classes
                reference = train_eval$MinimumGeneration)
# Validation
  confusionMatrix(data = val_eval$LRpred_log_3, 
                  reference = val_eval$MinimumGeneration)
#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_log_3,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_log_3,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)
###############################################################################################
#ROC curve
reducedRoc <- roc(response = train_eval$MinimumGeneration, train_eval$LRprob_log_2$X0)
plot(reducedRoc, add=TRUE, col="green")
auc(reducedRoc)
reducedRoc <- roc(response = train_eval$MinimumGeneration, train_eval$LRprob_log_3)
plot(reducedRoc, add=TRUE, col="red")
auc(reducedRoc)
legend("bottomright", legend=c("lda", "qda", "knn"), col=c("black", "red", "green"), lwd=2)

###############################################################################################
##################################### K-NEAREST NEIGHBORS ##################################### 
###############################################################################################

set.seed(seed) #For replication
#Train knn model model.
#Knn contains 1 tuning parameter k (number of neigbors). Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame(k = 5),
#  - Try with a range of values specified in tuneGrid: tuneGrid = data.frame(k = seq(2,120,4)),
#  - Caret chooses 10 values: tuneLength = 10,
knn.fit = train(form = MinimumGeneration ~ ., #formula for specifying inputs and outputs.
                data = data_train_up,   #Training dataset 
                method = "knn",
                preProcess = c("center","scale"),#muy importante
                tuneGrid = data.frame(k = 1),
                #tuneLength = 10,
                #tuneGrid = data.frame(k = seq(1,50,1)),
                trControl = ctrl, 
                metric = "AUC")
# Ante valores de error parecidos, mejor nos quedamos ocn la K más alta
knn.fit #information about the settings
ggplot(knn.fit) #plot the summary metric as a function of the tuning parameter
knn.fit$finalModel

#Evaluate the model with training and validation sets
#training
train_eval$LRprob_knn_1 <- predict(knn.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_knn_1 <- predict(knn.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_knn_1 <- predict(knn.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_knn_1 <- predict(knn.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_knn_1, #Predicted classes
                reference = train_eval$MinimumGeneration)
# Validation
confusionMatrix(data = val_eval$LRpred_knn_1, 
                reference = val_eval$MinimumGeneration)
#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_knn_1,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_knn_1,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)

###############################################################################################
######################################## DECISION TREE ######################################## 
###############################################################################################
# 1) Todas las variables
set.seed(seed) #For replication. 
# Decision trees can work with categorical variables as theey are. Then, x and y arguments can be used
tree.fit <- train(form =  MinimumGeneration ~ ., #formula for specifying inputs and outputs.
                   data = data_train_up,   #Training dataset 
                   method = "rpart",   #Decision tree with cp as tuning parameter
                   control = rpart.control(minsplit = 50,  # Minimum number of obs in node to keep cutting
                                           minbucket = 80), # Minimum number of obs in a terminal node
                   parms = list(split = "gini"),          # impuriry measure
                   #tuneGrid = data.frame(cp = 0.1), # TRY this: tuneGrid = data.frame(cp = 0.25),
                   #tuneLength = 10,
                   tuneGrid = data.frame(cp = seq(0,0.1,0.005)),
                   trControl = ctrl, 
                   metric = "ROC")
tree.fit #information about the resampling settings
ggplot(tree.fit) #plot the summary metric as a function of the tuning parameter
summary(tree.fit)  #information about the model trained
tree.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.
#Basic plot of the tree:
plot(tree.fit$finalModel, uniform = TRUE, margin = 0.1)
text(tree.fit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
#Advanced plots
rpart.plot(tree.fit$finalModel, type = 2, fallen.leaves = FALSE, box.palette = "Blues")
tree.fit.party <- as.party(tree.fit$finalModel)
plot(tree.fit.party)

#Measure for variable importance
varImp(tree.fit,scale = FALSE)
plot(varImp(tree.fit,scale = FALSE))


#Evaluate the model with training and validation sets
#training
train_eval$LRprob_tree_1 <- predict(tree.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_tree_1 <- predict(tree.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_tree_1 <- predict(tree.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_tree_1<- predict(tree.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_tree_1, #Predicted classes
                reference = train_eval$MinimumGeneration) #Class labeled as Positive
# No information rate -> modelo tonto de referencia sin entrenar
# Validation
confusionMatrix(data = val_eval$LRpred_tree_1, 
                reference = val_eval$MinimumGeneration)

#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_tree_1,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_tree_1,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)
# 2) WSL4H100 + WSL1H100 + WSL10H100 + WSL2H100 + WSL5H100
set.seed(seed) #For replication. 
# Decision trees can work with categorical variables as theey are. Then, x and y arguments can be used
tree2.fit <- train(form =  MinimumGeneration ~ WSL4H100 + WSL1H100 + WSL10H100 + WSL2H100 + WSL5H100, #formula for specifying inputs and outputs.
                  data = data_train_up,   #Training dataset 
                  method = "rpart",   #Decision tree with cp as tuning parameter
                  control = rpart.control(minsplit = 50,  # Minimum number of obs in node to keep cutting
                                          minbucket = 80), # Minimum number of obs in a terminal node
                  parms = list(split = "gini"),          # impuriry measure
                  #tuneGrid = data.frame(cp = 0.1), # TRY this: tuneGrid = data.frame(cp = 0.25),
                  #tuneLength = 10,
                  tuneGrid = data.frame(cp = seq(0,0.1,0.005)),
                  trControl = ctrl, 
                  metric = "ROC")
tree2.fit #information about the resampling settings
ggplot(tree2.fit) #plot the summary metric as a function of the tuning parameter
summary(tree2.fit)  #information about the model trained
tree2.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.
#Basic plot of the tree:
plot(tree2.fit$finalModel, uniform = TRUE, margin = 0.1)
text(tree2.fit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
#Advanced plots
rpart.plot(tree2.fit$finalModel, type = 2, fallen.leaves = FALSE, box.palette = "Blues")
tree2.fit.party <- as.party(tree2.fit$finalModel)
plot(tree2.fit.party)

#Measure for variable importance
varImp(tree2.fit,scale = FALSE)
plot(varImp(tree2.fit,scale = FALSE))


#Evaluate the model with training and validation sets
#training
train_eval$LRprob_tree_2 <- predict(tree2.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_tree_2 <- predict(tree2.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_tree_2 <- predict(tree2.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_tree_2<- predict(tree2.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_tree_2, #Predicted classes
                reference = train_eval$MinimumGeneration) #Class labeled as Positive
# No information rate -> modelo tonto de referencia sin entrenar
# Validation
confusionMatrix(data = val_eval$LRpred_tree_2, 
                reference = val_eval$MinimumGeneration)

#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_tree_2,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_tree_2,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)
# 3) WSL4H100 + WSL1H100 + WSL10H100 
set.seed(seed) #For replication. 
# Decision trees can work with categorical variables as theey are. Then, x and y arguments can be used
tree3.fit <- train(form =  MinimumGeneration ~ WSL4H100 + WSL1H100 + WSL10H100, #formula for specifying inputs and outputs.
                   data = data_train_up,   #Training dataset 
                   method = "rpart",   #Decision tree with cp as tuning parameter
                   control = rpart.control(minsplit = 50,  # Minimum number of obs in node to keep cutting
                                           minbucket = 80), # Minimum number of obs in a terminal node
                   parms = list(split = "gini"),          # impuriry measure
                   #tuneGrid = data.frame(cp = 0.1), # TRY this: tuneGrid = data.frame(cp = 0.25),
                   #tuneLength = 10,
                   tuneGrid = data.frame(cp = seq(0,0.1,0.005)),
                   trControl = ctrl, 
                   metric = "ROC")
tree3.fit #information about the resampling settings
ggplot(tree3.fit) #plot the summary metric as a function of the tuning parameter
summary(tree3.fit)  #information about the model trained
tree3.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.
#Basic plot of the tree:
plot(tree3.fit$finalModel, uniform = TRUE, margin = 0.1)
text(tree3.fit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
#Advanced plots
rpart.plot(tree3.fit$finalModel, type = 2, fallen.leaves = FALSE, box.palette = "Blues",)
tree3.fit.party <- as.party(tree3.fit$finalModel)
plot(tree3.fit.party)

#Measure for variable importance
varImp(tree3.fit,scale = FALSE)
plot(varImp(tree3.fit,scale = FALSE))


#Evaluate the model with training and validation sets
#training
train_eval$LRprob_tree_3 <- predict(tree3.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_tree_3 <- predict(tree3.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_tree_3 <- predict(tree3.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_tree_3<- predict(tree3.fit, type="raw", newdata = data_val) # predict classes 

## Performance measures 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_tree_3, #Predicted classes
                reference = train_eval$MinimumGeneration) #Class labeled as Positive
# No information rate -> modelo tonto de referencia sin entrenar
# Validation
confusionMatrix(data = val_eval$LRpred_tree_3, 
                reference = val_eval$MinimumGeneration)

#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_tree_3,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_tree_3,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)

###############################################################################################
############################################# SVM ############################################# 
###############################################################################################

library(kernlab)
set.seed(seed)
svm.fit <- train(form = MinimumGeneration ~ ., #formula for specifying inputs and outputs.
                 data = data_train_up,   #Training dataset 
                 method = "svmLinear",
                 preProcess = c("center","scale"),
                 # 1) try C=0.1
                 #tuneGrid = data.frame(C = 0.1),
                 # 2) try C=10 and compare with C=0.1
                 #tuneGrid = data.frame(C = 10),
                 # 3) find the optimal value of C
                 tuneGrid = expand.grid(C = c(0.00001,0.0001,0.001,0.01,0.1,1,10,100,1000)),
                 #tuneGrid = data.frame(C = seq(0.1,10,1)),
                 #tuneLength = 10,
                 trControl = ctrl, 
                 metric = "ROC")
svm.fit #information about the resampling settings
ggplot(svm.fit) + scale_x_log10()
svm.fit$finalModel
isupvect <- alphaindex(svm.fit$finalModel)[[1]]

###############################################################################################
############################################# MLP ############################################# 
###############################################################################################
#1)Todas las variables
set.seed(seed) #For replication
mlp.fit = train(form = MinimumGeneration ~ .,
                data = data_train_up, 
                method = "nnet",
                #linout = TRUE,
                maxit = 200,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "Accuracy")
mlp.fit #information about the resampling settings
#ggplot(mlp.fit) + scale_x_log10()

#Evaluate the model with training and validation sets
#training
train_eval$LRprob_mlp_1 <- predict(mlp.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_mlp_1 <- predict(mlp.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_mlp_1 <- predict(mlp.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_mlp_1<- predict(mlp.fit, type="raw", newdata = data_val) # predict classes 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_mlp_1, #Predicted classes
                reference = train_eval$MinimumGeneration) #Class labeled as Positive
# No information rate -> modelo tonto de referencia sin entrenar
# Validation
confusionMatrix(data = val_eval$LRpred_mlp_1, 
                reference = val_eval$MinimumGeneration)

#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_mlp_1,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_mlp_1,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)

data_sel_mlp<- data_train_up %>%
  select(Hour,TL3H100,GSRL5H100,WSL4H100,WDL7H100,WG)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_train_up, data_train_up$MinimumGeneration,train_eval$LRpred_mlp_1, together = TRUE)

#2) WSL4H100 + WSL1H100 + TL4H100 + WSL6H100
set.seed(seed) #For replication
mlp2.fit = train(form = MinimumGeneration ~ WSL4H100 + WSL1H100 + TL4H100 + WSL6H100,
                data = data_train_up, 
                method = "nnet",
                #linout = TRUE,
                maxit = 200,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "ROC")
mlp2.fit #information about the resampling settings
#ggplot(mlp.fit) + scale_x_log10()

#Evaluate the model with training and validation sets
#training
train_eval$LRprob_mlp_2 <- predict(mlp2.fit, type="prob", newdata = data_train_up) # predict probabilities
train_eval$LRpred_mlp_2 <- predict(mlp2.fit, type="raw", newdata = data_train_up) # predict classes 
#validation
val_eval$LRprob_mlp_2 <- predict(mlp2.fit, type="prob", newdata = data_val) # predict probabilities
val_eval$LRpred_mlp_2<- predict(mlp2.fit, type="raw", newdata = data_val) # predict classes 

## Confusion matrices
# Training
confusionMatrix(data = train_eval$LRpred_mlp_2, #Predicted classes
                reference = train_eval$MinimumGeneration) #Class labeled as Positive
# No information rate -> modelo tonto de referencia sin entrenar
# Validation
confusionMatrix(data = val_eval$LRpred_mlp_2, 
                reference = val_eval$MinimumGeneration)

#######Classification performance plots 
# Training
PlotClassPerformance(train_eval$MinimumGeneration,       #Real observations
                     train_eval$LRprob_mlp_2,#,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed
# Validation
PlotClassPerformance(val_eval$MinimumGeneration,       #Real observations
                     val_eval$LRprob_mlp_2,  #predicted probabilities
                     selClass = "X1") #Class to be analyzed)

data_sel_mlp<- data_train_up %>%
  select(Hour,TL3H100,GSRL5H100,WSL4H100,WDL7H100,WG)

SensAnalysisMLP(mlp2.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_train_up, data_train_up$MinimumGeneration,train_eval$LRpred_mlp_2, together = TRUE)
