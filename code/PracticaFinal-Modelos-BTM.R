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

#Lectura del dataset
data <- read.csv("TTOTdataEEM17v3.csv")

#Create an hourly dataset containing average values of all variables for each hour

data_hourly <- data %>%
               select(-Min) %>%
               group_by(Date,Hour) %>%
               summarise_all(mean)

###############################################################################################
########################################## PROBLEM 3 ########################################## 
###############################################################################################

## Análisis exploratorio de las variables
# - Temperatura
temperaturas <- data_hourly %>%
  select(starts_with("TL"))
temperaturas$Date<-NULL
temp_vector <- gather(temperaturas,key="sensores",value="temperaturas")
temp_DF <- data.frame(temp_vector)
b <- ggplot(temp_DF, aes(temperaturas)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the temperature") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Temperature") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
b <- ggplot(temp_DF, aes(x=1,temperaturas)) +  
  ggtitle("Boxplot of temperatures") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(fill = 1), show.legend = FALSE) +
  ylab('Temperature') +
  xlab(" ") +
  geom_text(data=data.frame(q=as.numeric(quantile(temp_DF$temperaturas))[2:4]), aes(x=rep(1.4,3),y=q,label=format(q,digits=2)))
b
outliers_temp_high <- temp_DF %>%
                filter(temperaturas>=30) %>%
                distinct(sensores)
outliers_temp_low <- temp_DF %>%
                filter(temperaturas<=-10) %>%
                distinct(sensores)
# - Radiación
radiacion <- data_hourly %>%
  select(starts_with("GSRL"))
radiacion$Date <- NULL
radiacion_vector <- gather(radiacion,key="sensores",value="radiacion")
radiacion_DF <- data.frame(radiacion_vector)
b <- ggplot(radiacion_DF, aes(radiacion)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the radiation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Radiation") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
b <- ggplot(radiacion_DF, aes(x=1,radiacion)) +  
  ggtitle("Boxplot of radiation") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(fill = 1), show.legend = FALSE) +
  ylab('Radiation') +
  xlab(" ") +
  geom_text(data=data.frame(q=as.numeric(quantile(radiacion_DF$radiacion))[2:4]), aes(x=rep(1.41,3),y=q,label=format(q,digits=2)))
b

outliers_radiation <- radiacion_DF %>%
  filter(radiacion>=600) %>%
  distinct(sensores)

# - Velocidad del viento
wind_speed <- data_hourly %>%
  select(starts_with("WSL"))
wind_speed$Date <- NULL
wind_speed_vector <- gather(wind_speed,key="sensores",value="wind_speed")
wind_speed_DF <- data.frame(wind_speed_vector)
b <- ggplot(wind_speed_DF, aes(wind_speed)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the wind speed") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Wind speed") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
b <- ggplot(wind_speed_DF, aes(x=1,wind_speed)) +  
  ggtitle("Boxplot of the wind speed") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(fill = 1), show.legend = FALSE) +
  ylab('Wind speed') +
  xlab(" ") +
  geom_text(data=data.frame(q=as.numeric(quantile(wind_speed_DF$wind_speed))[2:4]), aes(x=rep(1.41,3),y=q,label=format(q,digits=2)))
b

outliers_wind_speed <- wind_speed_DF %>%
  filter(wind_speed>=12.43481) %>%
  distinct(sensores)

# - Dirección del viento
wind_dir <- data_hourly %>%
  select(starts_with("WDL"))
wind_dir$Date <- NULL
wind_dir_vector <- gather(wind_dir,key="sensores",value="wind_dir")
wind_dir_DF <- data.frame(wind_dir_vector)
b <- ggplot(wind_dir_DF, aes(wind_dir)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the wind direction") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Wind direction") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
b <- ggplot(wind_dir_DF, aes(x=1,wind_dir)) +  
  ggtitle("Boxplot of the wind direction") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(fill = 1), show.legend = FALSE) +
  ylab('Wind direction') +
  xlab(" ") +
  geom_text(data=data.frame(q=as.numeric(quantile(wind_dir_DF$wind_dir))[2:4]), aes(x=rep(1.41,3),y=q,label=format(q,digits=2)))
b
outliers_wind_direction <- wind_dir_DF %>%
  filter(wind_dir<=-180)

# - Potencia generada
wg <- data_hourly %>%
  select("WG")
wg$Date<-NULL
wg_vector <- gather(wg,key="sensores",value="wg")
wg_DF <- data.frame(wg_vector)
b <- ggplot(wg_DF, aes(wg)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the power generation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Power generated [MWh]") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
b <- ggplot(wg_DF, aes(x=1,wg)) +  
  ggtitle("Boxplot of the power generation") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(fill = 1), show.legend = FALSE) +
  ylab('Power generated [MWh]') +
  xlab(" ") +
  geom_text(data=data.frame(q=as.numeric(quantile(wg_DF$wg))[2:4]), aes(x=rep(1.41,3),y=q,label=format(q,digits=2)))
b
outliers_wg <- wg_DF %>%
              filter(wg>=145.2584)
prop_ouliers_wg <- nrow(outliers_wg)/nrow(wg_DF)*100

# Correlación entre variables
data_hourly_no_date <- data_hourly
data_hourly_no_date$Date <- NULL
correlacion <- cor(data_hourly_no_date)
correlacion_DF <- data.frame(correlacion)
correlacion_DF$names <- rownames(correlacion)
correlacion_gather <- gather(correlacion_DF,key="variables",value="correlation",-names)
corrplot(correlacion, type="upper",col=brewer.pal(n=10, name="RdYlBu"),tl.cex=0.5)

tmp_rad <- data_hourly %>%
            select(starts_with("TL"),starts_with("GSR"))
tmp_rad$Date <- NULL
corr_temp_rad <- cor(tmp_rad)
corrplot(corr_temp_rad, type="upper",col=brewer.pal(n=8, name="RdYlBu"))


# Extract significant correlations
corr_higher_075 <- correlacion_gather %>%
                  filter(correlation>=0.75)
corr_lower_075 <- correlacion_gather %>%
                filter(correlation<=-0.75)

# Corr varibables
corr_temperaturas <- cor(temperaturas)
corrplot(corr_temperaturas,type="upper",col=brewer.pal(n=8, name="RdYlBu"))
corr_radiation <- cor(radiacion)
corrplot(corr_radiation,type="upper",col=brewer.pal(n=8, name="RdYlBu"))
corr_ws <- cor(wind_speed)
corrplot(corr_ws,type="upper",col=brewer.pal(n=8, name="RdYlBu"))
corr_wd <- cor(wind_dir)
corrplot(corr_wd,type="upper",col=brewer.pal(n=8, name="RdYlBu"))

# LIMPIEZA DE DATOS
data_hourly_clean <- data_hourly %>%
                    filter(WDL2H2>=(-180) & WDL2H80>=(-180) & WDL2H100>=(-180))
data_hourly_clean$Date <- NULL

# NORMALIZACIÓN DE LOS DATOS
normalize <- function(toNormalize,normalizer){ # normalize data in toNormalize regarding normalizer
  y <- (toNormalize-min(normalizer))/(max(normalizer)-min(normalizer))
  return(y)
}
data_norm <- mapply(normalize, data_hourly_clean, data_hourly_clean)
data_norm <- data.frame(data_norm)

##TRAIN, TEST AND VALIDATION
seed = 150
set.seed(seed) 
ratioTR = 0.7 #Percentage for training

trainIndex <- createDataPartition(data_norm$WG,p = ratioTR,list = FALSE,times = 1) #only one partition

data_train = data_norm[trainIndex,]
data_val_aux = data_norm[-trainIndex,]

valTR = 0.50 #Percentage for validation and test

trainIndex2 <- createDataPartition(data_val_aux$WG,p = valTR,list = FALSE,times = 1) #only one partition
data_test = data_val_aux[trainIndex2,]
data_val = data_val_aux[-trainIndex2,]

# - Selección recursiva de variables temperatura
ctrl_rfe <- rfeControl(method="cv",number=10,verbose=TRUE,functions = caretFuncs)
set.seed(seed)
subsets <- 1:10
lm.RFE <- rfe(form = WG~TL1H100+TL2H100+TL3H100+TL4H100+TL5H100+TL6H100+TL7H100+TL8H100+TL9H100+TL10H100,
              data=data_train,
              method="lm",
              preProcess = c("center","scale"),
              sizes = subsets,
              metric = "RMSE",
              rfeControl = ctrl_rfe)
lm.RFE
ggplot(lm.RFE,metric="RMSE")
lm.RFE$fit
lm.RFE$fit$finalModel

# - Selección recursiva de variables radiación solar
ctrl_rfe <- rfeControl(method="cv",number=10,verbose=TRUE,functions = caretFuncs)
set.seed(seed)
subsets <- 1:10
lm.RFE <- rfe(form = WG~GSRL1H100+GSRL2H100+GSRL3H100+GSRL4H100+GSRL5H100+GSRL6H100+GSRL7H100+GSRL8H100+GSRL9H100+GSRL10H100,
              data=data_train,
              method="lm",
              preProcess = c("center","scale"),
              sizes = subsets,
              metric = "RMSE",
              rfeControl = ctrl_rfe)
lm.RFE
ggplot(lm.RFE,metric="RMSE")
lm.RFE$fit
lm.RFE$fit$finalModel

# - Selección recursiva de variables dirección del vieno
ctrl_rfe <- rfeControl(method="cv",number=10,verbose=TRUE,functions = caretFuncs)
set.seed(seed)
subsets <- 1:10
lm.RFE <- rfe(form = WG~WDL1H100+WDL2H100+WDL3H100+WDL4H100+WDL5H100+WDL6H100+WDL7H100+WDL8H100+WDL9H100+WDL10H100,
              data=data_train,
              method="lm",
              preProcess = c("center","scale"),
              sizes = subsets,
              metric = "RMSE",
              rfeControl = ctrl_rfe)
lm.RFE
ggplot(lm.RFE,metric="RMSE")
lm.RFE$fit
lm.RFE$fit$finalModel

# - Initialize de train control
ctrl <- trainControl(method = "cv",                        #k-fold cross-validation
                     number = 10,                          #Number of folds
                     summaryFunction = defaultSummary,     #Performance summary for comparing models in hold-out samples.
                     classProbs = TRUE)                    #Compute class probs in Hold-out samples

#-------------------------------------------------------------------------------------------------
#---------------------------- REGRESSION MODELS  --------------------------------------------------
#-------------------------------------------------------------------------------------------------

# - Regresión Lineal
set.seed(seed) #For replication
lm.fit = train(form = WG ~ Hour + TL3H100 + GSRL5H100 + WSL4H100 + WDL7H100,
               data = data_train, 
               method = "lm", #Linear model
               preProcess = c("center","scale"),
               trControl = ctrl, 
               metric = "RMSE")
lm.fit #information about the resampling settings
summary(lm.fit)

# -  Evaluate model
#training
data_train_eval <- data_train
data_train_eval$lm_pred <- predict(lm.fit, type="raw", newdata = data_train)
#test
data_test_eval <- data_test
data_test_eval$lm_pred <- predict(lm.fit, type="raw", newdata = data_test) 
#validacion
data_val_eval <- data_val
data_val_eval$lm_pred <- predict(lm.fit, type="raw", newdata = data_val)

# RMSE
rmse_train_linear1 = rmse(data_train$WG,data_train_eval$lm_pred)
rmse_test_linear1 = rmse(data_test$WG,data_test_eval$lm_pred)
rmse_val_linear1 = rmse(data_val$WG,data_val_eval$lm_pred)

data_sel_linear <- data_train %>%
                  select(Hour,TL3H100,GSRL5H100,WSL4H100,WDL7H100,WG)

PlotModelDiagnosis(data_sel_linear, data_sel_linear$WG,data_train_eval$lm_pred, together = TRUE)

# ------------------------------SMOOTHING SPLINES------------------------------------ 
set.seed(seed)
gam.fit <- train(form = WG ~ Hour + TL3H100 + GSRL5H100 + WSL4H100 + WDL7H100,
                 data = data_train,
                 method = "gamSpline",
                 preProcess = c("center","scale"),
                 tuneGrid = data.frame(df = seq(2,10,2)),
                 trControl = ctrl, 
                 metric = "RMSE")
gam.fit
summary(gam.fit)
ggplot(gam.fit)

data_train_eval$gam_pred = predict(gam.fit,newdata = data_train)  
data_test_eval$gam_pred = predict(gam.fit,newdata = data_test)
data_val_eval$gam_pred = predict(gam.fit,newdata = data_val)

# RMSE
rmse_train_gam = rmse(data_train$WG,data_train_eval$gam_pred)
rmse_test_gam = rmse(data_test$WG,data_test_eval$gam_pred)
rmse_val_gam = rmse(data_val$WG,data_val_eval$gam_pred)

data_sel_gam <- data_train %>%
  select(Hour,TL3H100,GSRL5H100,WSL4H100,WDL7H100,WG)

PlotModelDiagnosis(data_sel_gam, data_sel_gam$WG, data_train_eval$gam_pred, together = TRUE)

#-------------------------RED NEURONAL---------------------------
#1)WG ~ Hour + TL3H100 + GSRL5H100 + WSL4H100 + WDL7H100
set.seed(seed) #For replication
mlp.fit = train(form = WG ~ Hour + TL3H100 + GSRL5H100 + WSL4H100 + WDL7H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred1 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred1 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred1 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp1 = rmse(data_train$WG,data_train_eval$mlp_pred1)
rmse_test_mlp1 = rmse(data_test$WG,data_test_eval$mlp_pred1)
rmse_val_mlp1 = rmse(data_val$WG,data_val_eval$mlp_pred1)

data_sel_mlp<- data_train %>%
  select(Hour,TL3H100,GSRL5H100,WSL4H100,WDL7H100,WG)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred1, together = TRUE)

#2)WG ~ WSL4H100 + WSL1H80 + GSRL5H100
set.seed(seed) #For replication
mlp.fit = train(form = WG ~ WSL4H100 + WSL1H80 + GSRL5H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred2 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred2 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred2 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp2 = rmse(data_train$WG,data_train_eval$mlp_pred2)
rmse_test_mlp2 = rmse(data_test$WG,data_test_eval$mlp_pred2)
rmse_val_mlp2 = rmse(data_val$WG,data_val_eval$mlp_pred2)

data_sel_mlp<- data_train %>%
  select(WG,WSL4H100,WSL1H80,GSRL5H100)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred2, together = TRUE)

#3)WG ~ WSL4H100 + WSL9H100 + GSRL5H100 + WDL7H100
set.seed(seed) #For replication
mlp.fit = train(form =WG ~ WSL4H100 + WSL9H100 + GSRL5H100 + WDL7H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred3 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred3 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred3 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp3 = rmse(data_train$WG,data_train_eval$mlp_pred3)
rmse_test_mlp3 = rmse(data_test$WG,data_test_eval$mlp_pred3)
rmse_val_mlp3 = rmse(data_val$WG,data_val_eval$mlp_pred3)

data_sel_mlp<- data_train %>%
  select(WG,WSL4H100,WSL9H100,GSRL5H100,WDL7H100)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred3, together = TRUE)

#4)WG ~ WSL4H100 + WSL1H80 + GSRL5H100 + WDL7H100
set.seed(seed) #For replication
mlp.fit = train(form =WG ~ WSL4H100 + WSL1H80 + GSRL5H100 + WDL7H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred4 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred4 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred4 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp4 = rmse(data_train$WG,data_train_eval$mlp_pred4)
rmse_test_mlp4 = rmse(data_test$WG,data_test_eval$mlp_pred4)
rmse_val_mlp4 = rmse(data_val$WG,data_val_eval$mlp_pred4)

data_sel_mlp<- data_train %>%
  select(WG,WSL4H100,WSL1H80,GSRL5H100,WDL7H100)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred4, together = TRUE)

#5)WG ~ WSL4H100 + WSL1H100 + WSL2H100 + WSL3H100 + WSL5H100 + WSL6H100 + WSL7H100 + WSL8H100 + WSL9H100 + WSL10H100 + GSRL5H100 + WDL1H100 + WDL2H100 + WDL3H100 + WDL4H100 + WDL5H100 + WDL6H100 + WDL7H100 + WDL8H100 + WDL9H100 + WDL10H100
set.seed(seed) #For replication
mlp.fit = train(form =WG ~ WSL4H100 + WSL1H100 + WSL2H100 + WSL3H100 + WSL5H100 + WSL6H100 + WSL7H100 + WSL8H100 + WSL9H100 + WSL10H100 + GSRL5H100 + WDL1H100 + WDL2H100 + WDL3H100 + WDL4H100 + WDL5H100 + WDL6H100 + WDL7H100 + WDL8H100 + WDL9H100 + WDL10H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 200,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred5 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred5 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred5 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp5 = rmse(data_train$WG,data_train_eval$mlp_pred5)
rmse_test_mlp5 = rmse(data_test$WG,data_test_eval$mlp_pred5)
rmse_val_mlp5 = rmse(data_val$WG,data_val_eval$mlp_pred5)

data_sel_mlp<- data_train %>%
  select(WG,WSL4H100,WSL1H100,WSL2H100,WSL3H100,WSL5H100,WSL6H100,WSL7H100,WSL8H100,WSL9H100,WSL10H100,GSRL5H100,WDL1H100,WDL2H100,WDL3H100,WDL4H100,WDL5H100,WDL6H100,WDL7H100,WDL8H100,WDL9H100,WDL10H100)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred5, together = TRUE)

#6)WG ~ WSL4H100 + WSL1H100 + WSL3H100 + WSL8H100 + GSRL5H100 + WDL2H100
set.seed(seed) #For replication
mlp.fit = train(form =WG ~ WSL4H100 + WSL1H100 + WSL3H100 + WSL8H100 + GSRL5H100 + WDL2H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 200,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred6 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred6 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred6 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp6 = rmse(data_train$WG,data_train_eval$mlp_pred6)
rmse_test_mlp6 = rmse(data_test$WG,data_test_eval$mlp_pred6)
rmse_val_mlp6 = rmse(data_val$WG,data_val_eval$mlp_pred6)

data_sel_mlp<- data_train %>%
  select(WG,WSL4H100,WSL1H100,WSL3H100,WSL8H100,GSRL5H100,WDL2H100)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred6, together = TRUE)

#7)WG ~ WSL4H100 + WSL1H100 + WDL2H100
set.seed(seed) #For replication
mlp.fit = train(form =WG ~ WSL4H100 + WSL1H100 + WDL2H100,
                data = data_train, 
                method = "nnet",
                linout = TRUE,
                maxit = 200,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                       decay=10^seq(-7,-2, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
ggplot(mlp.fit) + scale_x_log10()

data_train_eval$mlp_pred7 = predict(mlp.fit,  newdata = data_train)  
data_test_eval$mlp_pred7 = predict(mlp.fit,  newdata = data_test)
data_val_eval$mlp_pred7 = predict(mlp.fit,  newdata = data_val)

# RMSE
rmse_train_mlp7 = rmse(data_train$WG,data_train_eval$mlp_pred7)
rmse_test_mlp7 = rmse(data_test$WG,data_test_eval$mlp_pred7)
rmse_val_mlp7 = rmse(data_val$WG,data_val_eval$mlp_pred7)

data_sel_mlp<- data_train %>%
  select(WG,WSL4H100,WSL1H100,WDL2H100)

SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(data_sel_mlp, data_sel_mlp$WG,data_train_eval$mlp_pred6, together = TRUE)

#-------------------------SVM---------------------------
library(kernlab)
set.seed(seed) #For replication
#Train model using training data
#Train radial  svm
#svm contains 2 tuning parameter C (Cost) and sigma. Three options:
#  - Train with a fixed parameter: tuneGrid = data.frame( sigma=100, C=1),
#  - Try with a range of values specified in tuneGrid: tuneGrid = expand.grid(C = seq(0.1,100,length.out = 8), sigma=seq(0.01,50,length.out = 4)),
#  - Caret chooses 10 values: tuneLength = 10,
svm.fit = train(form = WG ~ Hour + TL3H100 + GSRL5H100 + WSL4H100 + WDL7H100, #formula for specifying inputs and outputs.
                data = data_train,   #Training dataset 
                method = "svmRadial",
                preProcess = c("center","scale"),
                #tuneGrid = expand.grid(C = c(0.001,0.01,0.1,1,10,100,1000), sigma=c(0.0001,0.001,0.01,0.1,1,10)),
                #tuneGrid =  data.frame(sigma = 0.01, C = 0.1),  
                tuneGrid = expand.grid(C = seq(0.1,1000,length.out = 8), sigma=seq(0.01,50,length.out = 4)),
                #tuneLength = 10,
                trControl = ctrl, 
                metric = "RMSE")
svm.fit #information about the resampling settings
ggplot(svm.fit) + scale_x_log10() # el sigmoa = 1e-04 es la pero opción
svm.fit$finalModel #information about the model trained
#Plot the svm support vectors:
isupvect <- alphaindex(svm.fit$finalModel)[[1]] #indexes for support vectors
#plot support vectors
ggplot() + geom_point(data = fTR[isupvect,], aes(x = X1, y = X2), color = "red") +
  geom_point(data = fTR[-isupvect,], aes(x = X1, y = X2))
plot(svm.fit$finalModel, data = as.matrix(predict(svm.fit$preProcess,fTR[,1:2])))

data_train_eval$svm_pred1 = predict(svm.fit,  newdata = data_train)  
data_test_eval$svm_pred1 = predict(svm.fit,  newdata = data_test)
data_val_eval$svm_pred1 = predict(svm.fit,  newdata = data_val)

# RMSE
rmse_train_svm1 = rmse(data_train$WG,data_train_eval$svm_pred1)
rmse_test_svm1 = rmse(data_test$WG,data_test_eval$svm_pred1)
rmse_val_svm1 = rmse(data_val$WG,data_val_eval$svm_pred1)

data_sel_svm<- data_train %>%
  select(Hour,TL3H100,GSRL5H100,WSL4H100,WDL7H100,WG)

PlotModelDiagnosis(data_sel_svm, data_sel_svm$WG,data_train_eval$svm_pred1, together = TRUE)
