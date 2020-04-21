# PRÁCTICA FINAL
# Beatriz Torreiro Mosquera

library(dplyr)
library(tidyr)
library(MLTools)
library(ggplot2)
library(GGally)
library(BEST)

#Lectura del dataset
data <- read.csv("TTOTdataEEM17v3.csv")

#Obtención información general de las variables
# - Temperatura
temperaturas <- data %>%
                select(starts_with("TL"))
temp_vector <- gather(temperaturas,key="sensores",value="temperaturas") %>%
                select("temperaturas")
temp_DF <- data.frame(temp_vector)
b <- ggplot(temp_DF, aes(temperaturas)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the temperature") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Temperature") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
# - Radiación
radiacion <- data %>%
  select(starts_with("GSRL"))
radiacion_vector <- gather(radiacion,key="sensores",value="radiacion") %>%
  select("radiacion")
radiacion_DF <- data.frame(radiacion_vector)
b <- ggplot(radiacion_DF, aes(radiacion)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the radiation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Radiation") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
# - Velocidad del viento
wind_speed <- data %>%
  select(starts_with("WSL"))
wind_speed_vector <- gather(wind_speed,key="sensores",value="wind_speed") %>%
  select("wind_speed")
wind_speed_DF <- data.frame(wind_speed_vector)
b <- ggplot(wind_speed_DF, aes(wind_speed)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the wind speed") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Wind speed") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
# - Dirección del viento
wind_dir <- data %>%
  select(starts_with("WDL"))
wind_dir_vector <- gather(wind_dir,key="sensores",value="wind_dir") %>%
  select("wind_dir")
wind_dir_DF <- data.frame(wind_dir_vector)
b <- ggplot(wind_dir_DF, aes(wind_dir)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the wind direction") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Wind direction") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
# - Potencia generada
wg <- data %>%
  select("WG")
wg_vector <- gather(wg,key="sensores",value="wg") %>%
  select("wg")
wg_DF <- data.frame(wg_vector)
b <- ggplot(wg_DF, aes(wg)) + 
  geom_histogram(binwidth = 0.5,aes(fill=..count..)) +
  ggtitle("Histogram of the power generation") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab(" ") +
  xlab("Power generated [MWh]") +
  scale_fill_gradient("Count", low="light blue", high="blue")
b
# - Potencia generada a lo largo de un día
wg_hourly <- data %>%
              select("WG","Hour") %>%
              group_by(Hour) %>%
              summarize(mean_WG = mean(WG))
b <- ggplot(wg_hourly, aes(x=Hour,y=mean_WG)) +
      ggtitle("Average Power Generated Throughout One Day") + theme(plot.title = element_text(hjust = 0.5)) +
      geom_line(color="blue") +
      xlab("Hour") + ylab("Average Power Generated [MWh]")
b
              
###############################################################################################
########################################## PROBLEM 1 ########################################## 
###############################################################################################
# Find how generation changes during one hour of operation
wind_power <- data %>%
              select("WG","Hour","Date","Min")

wind_power_DF <- data.frame(wind_power)
medians <- aggregate(WG ~  Hour, wind_power_DF, median)
b <- ggplot(wind_power_DF,aes(x=Hour,WG)) + 
  ggtitle("Wind power throughout the day") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(group=Hour,fill=Hour), show.legend = FALSE) +
  geom_text(data = medians, aes(label = format(WG,digits=2), y = WG + 8))+
  ylab('Power Generation') +
  xlab("Hour") +
  scale_fill_gradient("Hour", low="light blue", high="blue") 
b

wind_preparation <- wind_power %>%
        filter(Min==0 | Min==45) %>%
        select("Date","Hour","Min","WG")

wind_preparation <- spread(wind_preparation,key="Min",value="WG")
wind_preparation_DF <- data.frame(wind_preparation)

variation_per_hour <- wind_preparation_DF %>%
                      select("Hour")
variation_per_hour$variation <- wind_preparation_DF$X45 - wind_preparation_DF$X0

average_variation_range_inside_hour <- mean(abs(variation_per_hour$variation))
maximum_variation_range_inside_hour <- max(abs(variation_per_hour$variation))
minimum_variation_range_inside_hour <- min(abs(variation_per_hour$variation))

variation_per_hour_abs <- variation_per_hour
variation_per_hour_abs$variation <- abs(variation_per_hour$variation)

b <- ggplot(variation_per_hour_abs,aes(x=1,variation)) + 
  geom_boxplot(aes(fill = 1), show.legend = FALSE) +
  ggtitle("Wind power variation in one hour") + theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Power variation") + 
  xlab(" ")
b

variation_per_hour_summarised <- variation_per_hour_abs %>% 
                                  group_by(Hour) %>% 
                                  summarize_at(funs(mean,max),.vars=c("variation"))

medians <- aggregate(variation ~  Hour, variation_per_hour_abs, median)
b <- ggplot(variation_per_hour_abs,aes(x=Hour,variation)) + 
  ggtitle("Wind power variation throughout the day") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(group=Hour,fill=Hour), show.legend = FALSE) +
  geom_text(data = medians, aes(label = format(variation,digits=2), y = variation + 8))+
  ylab('Power Generation') +
  xlab("Hour") +
  scale_fill_gradient("Hour", low="light blue", high="blue") 
b

medians <- aggregate(variation ~  Hour, variation_per_hour_abs, median)
b <- ggplot(variation_per_hour_abs,aes(x=Hour,variation)) + 
  ggtitle("Wind power variation throughout the day") + theme(plot.title = element_text(hjust = 0.5)) +
  geom_boxplot(aes(group=Hour,fill=Hour), show.legend = FALSE,outlier.shape = NA) +
  ylab('Power Generation') +
  xlab("Hour") +
  scale_fill_gradient("Hour", low="light blue", high="blue") +
  scale_y_continuous(limits = quantile(variation_per_hour_abs$variation, c(0.1, 0.9)))
b

###############################################################################################
########################################## PROBLEM 2 ########################################## 
###############################################################################################

# A priori probability -> prior = abs(GSRL9H10 - GSRL10H10)/GSRL9H10

data_bayes <- data

n <- nrow(data_bayes)
ns <- seq(0,n,by=1)
p <- seq(0,1,by=0.001)

data_bayes$diferencia <- ifelse(data$GSRL9H100==0,ifelse(data$GSRL10H100==0,0,1.1),abs(data$GSRL9H100 - data$GSRL10H100)/data$GSRL9H100)
data_bayes$defective <- ifelse(data_bayes$diferencia > 1,1,0)

num_defective <- nrow(filter(data_bayes,defective==1))
prop_defectuoso <- num_defective/nrow(data_bayes)*100

pars <- expand.grid(prop=p,susc=ns)
pars$prior <- dbeta(pars$prop,prop_defectuoso,100-prop_defectuoso)
pars$likelihood <-dbinom(pars$susc,n,pars$prop)
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability/sum(pars$probability)

probDefective <- pars %>%
  filter(susc>=ceiling(0.02*nrow(data_bayes)) & susc <=ceiling(0.05*nrow(data_bayes)))%>%
  select(prop,probability,susc,likelihood,prior)
list_susc = unique(probDefective$susc)
for(i in list_susc){
  probDefective$probability[probDefective$susc==i] <- probDefective$probability[probDefective$susc==i] / sum(probDefective$probability[probDefective$susc==i])
}
#probDefective$probability <- probDefective$probability / sum(probDefective$probability)

b <- ggplot() +
  ggtitle("Bayes Inference")+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(data=probDefective, aes(x=prop, y=probability,color="Defective Rate Of Dataset")) +
  #geom_line(data=probDefective,aes(x=prop, y=prior,color="Prior Knowledge")) +
  #geom_line(data=probDefective,aes(x=prop,y=likelihood,color="Likelihood Knowledge"))+
  scale_color_manual(name = " ",values = c('Defective Rate Of Dataset' = "#F7756D",'Prior Knowledge' = "#01BEC3","Likelihood Knowledge"="chartreuse2")) +
  xlab("Defective Rate") + 
  ylab("Probability of that defective rate") +
  xlim(0.00,0.15)+
  theme(legend.position = "none")
b


