
#------------------------------------------------------------------
# Sales Prediction and Forecasting - Wine Sales
# Singh, Gurjeet
#------------------------------------------------------------------

library(readr)
library(car)
library(fBasics)
library(ggplot2)
library(corrplot)
library(plyr)
library(gmodels)
library(MASS)
library(gridExtra)

library(rpart)
library(rpart.plot)
library(RGtk2)
library(rattle)
library(RColorBrewer)
library(pscl)

options(scipen = 999)
#-------------------------------------------------------------------------------
## 1 - DATA EXPLORATION
#-------------------------------------------------------------------------------

wine <- read_csv("wine.csv")


wine.Data <- wine
colnames(wine.Data)[1] <- "INDEX"

#Understand the stats and summary
str(wine.Data)
summary(wine.Data)

#reviewing the stats
View(t(basicStats(wine.Data[sapply(wine.Data,is.numeric)])))
      # View(char_stats)

WineTreeSTARS <- rpart(STARS ~
                         AcidIndex
                       +Alcohol
                       +Chlorides
                       +CitricAcid
                       +Density
                       +FixedAcidity
                       +FreeSulfurDioxide
                       +LabelAppeal
                       +ResidualSugar
                       +Sulphates
                       +TotalSulfurDioxide
                       +VolatileAcidity
                       +pH, data = wine.Data, method = 'class')

fancyRpartPlot(WineTreeSTARS)

#check the mean and variance of TARGET
mean(wine.Data[which(wine.Data$TARGET >0),"TARGET"])
var(wine.Data[which(wine.Data$TARGET >0),"TARGET"])

#----------------------------------------
#clean missing values with median values
#----------------------------------------

summary(wine.Data)

##clean missing values with median values
wine.Data$IMP_ResidualSugar <- ifelse(is.na(wine.Data$ResidualSugar),
                                 3.9,
                                 wine.Data$ResidualSugar)
wine.Data$M_ResidualSugar <- ifelse(is.na(wine.Data$ResidualSugar),
                                1, 0)

wine.Data$IMP_Chlorides <-ifelse(is.na(wine.Data$Chlorides),
                                 0.05,
                                 wine.Data$Chlorides)
wine.Data$M_Chlorides <- ifelse(is.na(wine.Data$Chlorides),
                                1, 0)


wine.Data$IMP_FreeSulfurDioxide <-ifelse(is.na(wine.Data$FreeSulfurDioxide),
                                      30,
                                      wine.Data$FreeSulfurDioxide)
wine.Data$M_FreeSulfurDioxide <- ifelse(is.na(wine.Data$FreeSulfurDioxide),
                                     1, 0)



wine.Data$IMP_TotalSulfurDioxide <-ifelse(is.na(wine.Data$TotalSulfurDioxide), 
                                          123,
                                          wine.Data$TotalSulfurDioxide)

wine.Data$M_TotalSulfurDioxide <- ifelse(is.na(wine.Data$TotalSulfurDioxide),
                                        1, 0)


wine.Data$IMP_pH <-ifelse(is.na(wine.Data$pH), 
                                          3.2,
                                          wine.Data$pH)

wine.Data$M_pH <- ifelse(is.na(wine.Data$pH),
                                         1, 0)

wine.Data$IMP_Sulphates <-ifelse(is.na(wine.Data$Sulphates), 
                                          0.50,
                                          wine.Data$Sulphates)

wine.Data$M_Sulphates <- ifelse(is.na(wine.Data$Sulphates),
                                         1, 0)


wine.Data$IMP_Alcohol <-ifelse(is.na(wine.Data$Alcohol), 
                                          10.40,
                                          wine.Data$Alcohol)

wine.Data$M_Alcohol <- ifelse(is.na(wine.Data$Alcohol),
                                         1, 0)



wine.Data$IMP_STARS <-ifelse(is.na(wine.Data$STARS) & (wine.Data$LabelAppeal < -0.5), 1,
                        ifelse(is.na(wine.Data$STARS) & (wine.Data$LabelAppeal >= -0.5),2,
                               wine.Data$STARS))

wine.Data$M_STARS <- ifelse(is.na(wine.Data$STARS),
                                         1, 0)


#options(scipen = 999)
View(t(basicStats(wine.Data[sapply(wine.Data,is.numeric)])))

#----------------------------------------
##Histograms - Exploration
#----------------------------------------

#---------------
# TARGET
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=TARGET))+
          geom_histogram(fill = "deepskyblue3",
                          colour="black", binwidth = 0.5) + 
          stat_bin(binwidth=1, geom="text", 
                      aes(label=..count..), vjust=-1) +
          labs(title = "Histogram of TARGET", 
                      x = "TARGET", y = "Frequency" ) +
          scale_x_continuous(breaks = seq(0, 10,by = 1))+
          theme_bw()

plot2 <- ggplot(wine.Data,
                mapping = aes(x=TARGET,
                          fill=as.factor(STARS), 
                          group = as.factor(STARS)))+
          geom_bar()+
          theme_bw() +
          labs(title = "Histogram of TARGET by STARS",
               x = "TARGET", y = "Frequency") +
          scale_fill_discrete(name="STARS") +
        scale_x_continuous(breaks = seq(0, 10,by = 1))

grid.arrange(plot1, plot2, nrow = 1)

#---------------
# FixedAcidity
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=FixedAcidity))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of FixedAcidity", 
       x = "FixedAcidity", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-100, 100,by = 10))+
  theme_bw()

#---------------
# VolatileAcidity
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=VolatileAcidity))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of VolatileAcidity", 
       x = "VolatileAcidity", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-5, 5,by = 1))+
  theme_bw()

grid.arrange(plot1, plot2, nrow = 1)

#---------------
# CitricAcid
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=CitricAcid))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of CitricAcid", 
       x = "CitricAcid", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-6, 6,by = 1))+
  theme_bw()


#---------------
# IMP_ResidualSugar
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=IMP_ResidualSugar))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_ResidualSugar", 
       x = "IMP_ResidualSugar", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-200, 200,by = 20))+
  theme_bw()


grid.arrange(plot1, plot2, nrow = 1)
#---------------
# IMP_Chlorides
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=IMP_Chlorides))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_Chlorides", 
       x = "IMP_Chlorides", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-5, 5,by = 0.5))+
  theme_bw()

#---------------
# IMP_FreeSulfurDioxide
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=IMP_FreeSulfurDioxide))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_FreeSulfurDioxide", 
       x = "IMP_FreeSulfurDioxide", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-1000, 1000,by = 100))+
  theme_bw()

grid.arrange(plot1, plot2, nrow = 1)
#---------------
# IMP_TotalSulfurDioxide
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=IMP_TotalSulfurDioxide))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_TotalSulfurDioxide", 
       x = "IMP_TotalSulfurDioxide", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-1100, 1100,by = 300))+
  theme_bw()

#---------------
# Density
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=Density))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of Density", 
       x = "Density", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-3, 3,by = 0.02))+
  theme_bw()

grid.arrange(plot1, plot2, nrow = 1)
#---------------
# IMP_pH
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=IMP_pH))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_pH", 
       x = "IMP_pH", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-10, 10,by = 0.5))+
  theme_bw()

#---------------
# IMP_Sulphates
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=IMP_Sulphates))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_Sulphates", 
       x = "IMP_Sulphates", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-10, 10,by = 0.5))+
  theme_bw()

grid.arrange(plot1, plot2, nrow = 1)
#---------------
# IMP_Alcohol
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=IMP_Alcohol))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_Alcohol", 
       x = "IMP_Alcohol", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-50, 50,by = 2))+
  theme_bw()


#---------------
# LabelAppeal
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=LabelAppeal))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black", binwidth = 0.5) + 
  labs(title = "Histogram of LabelAppeal", 
       x = "LabelAppeal", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-5, 5,by = 1))+
  theme_bw()

grid.arrange(plot1, plot2, nrow = 1)
#---------------
# AcidIndex
#---------------
plot1 <- ggplot(wine.Data, mapping = aes(x=AcidIndex))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black", binwidth = 0.5) + 
  labs(title = "Histogram of AcidIndex", 
       x = "AcidIndex", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-50, 50,by = 1))+
  theme_bw()

#---------------
# IMP_STARS
#---------------
plot2 <- ggplot(wine.Data, mapping = aes(x=IMP_STARS))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black", binwidth = 0.5) + 
  labs(title = "Histogram of IMP_STARS", 
       x = "IMP_STARS", 
       y = "Frequency" ) +
  scale_x_continuous(breaks = seq(-5, 5,by = 1))+
  theme_bw()

grid.arrange(plot1, plot2, nrow = 1)


#---------------------------------------------
# Q-Q Plots below
#---------------------------------------------

par(mfrow=c(2,2))
#---------------
# FixedAcidity
#---------------

with(wine.Data, 
     qqPlot(FixedAcidity,
            main="QQ-Plot FixedAcidity", 
            col = "gray0"));

#---------------
# VolatileAcidity
#---------------

with(wine.Data, 
     qqPlot(VolatileAcidity,
            main="QQ-Plot VolatileAcidity", 
            col = "gray0"));


#---------------
# CitricAcid
#---------------

with(wine.Data, 
     qqPlot(CitricAcid,
            main="QQ-Plot CitricAcid", 
            col = "gray0"));


#---------------
# IMP_ResidualSugar
#---------------


with(wine.Data, 
     qqPlot(IMP_ResidualSugar,
            main="QQ-Plot IMP_ResidualSugar", 
            col = "gray0"));


#---------------
# IMP_Chlorides
#---------------

with(wine.Data, 
     qqPlot(IMP_Chlorides,
            main="QQ-Plot IMP_Chlorides", 
            col = "gray0"));

#---------------
# IMP_FreeSulfurDioxide
#---------------

with(wine.Data, 
     qqPlot(IMP_FreeSulfurDioxide,
            main="QQ-Plot IMP_FreeSulfurDioxide", 
            col = "gray0"));

#---------------
# IMP_TotalSulfurDioxide
#---------------
with(wine.Data, 
     qqPlot(IMP_TotalSulfurDioxide,
            main="QQ-Plot IMP_TotalSulfurDioxide", 
            col = "gray0"));


#---------------
# Density
#---------------

with(wine.Data, 
     qqPlot(Density,
            main="QQ-Plot Density", 
            col = "gray0"));


#---------------
# IMP_pH
#---------------

with(wine.Data, 
     qqPlot(IMP_pH,
            main="QQ-Plot IMP_pH", 
            col = "gray0"));

#---------------
# IMP_Sulphates
#---------------

with(wine.Data, 
     qqPlot(IMP_Sulphates,
            main="QQ-Plot IMP_Sulphates", 
            col = "gray0"));

#---------------
# IMP_Alcohol
#---------------

with(wine.Data, 
     qqPlot(IMP_Alcohol,
            main="QQ-Plot IMP_Alcohol", 
            col = "gray0"));

#---------------
# LabelAppeal
#---------------

with(wine.Data, 
     qqPlot(LabelAppeal,
            main="QQ-Plot LabelAppeal", 
            col = "gray0"));

#---------------
# AcidIndex
#---------------

with(wine.Data, 
     qqPlot(AcidIndex,
            main="QQ-Plot AcidIndex", 
            col = "gray0"));

#---------------
# IMP_STARS
#---------------

with(wine.Data, 
     qqPlot(IMP_STARS,
            main="QQ-Plot IMP_STARS", 
            col = "gray0"));

par(mfrow=c(1,1))


#------------------------------------------------------------------------------
## 2 - DATA PREPARATION 
#------------------------------------------------------------------------------

#-------------------------------
## Fix extreme values for:
#     - IMP_TotalSulfurDioxide
#     - IMP_FreeSulfurDioxide
#     - IMP_ResidualSugar

# These have mean/median very low but min and max very high
#-------------------------------


wine.Data$IMP_TotalSulfurDioxide = ifelse(wine.Data$IMP_TotalSulfurDioxide < -350, -350,
                                            ifelse(wine.Data$IMP_TotalSulfurDioxide > 620, 620,
                                                   wine.Data$IMP_TotalSulfurDioxide))

wine.Data$IMP_ResidualSugar = ifelse(wine.Data$IMP_ResidualSugar < -50, -50,
                                           ifelse(wine.Data$IMP_ResidualSugar > 75, 75,
                                                  wine.Data$IMP_ResidualSugar))

wine.Data$IMP_FreeSulfurDioxide = ifelse(wine.Data$IMP_FreeSulfurDioxide < -300, -300,
                                           ifelse(wine.Data$IMP_FreeSulfurDioxide > 350, 350,
                                                  wine.Data$IMP_FreeSulfurDioxide))


#----------------------------------------
# Histograms to show new distribution
#----------------------------------------
plot1 <- ggplot(wine.Data, mapping = aes(x=IMP_TotalSulfurDioxide))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
    labs(title = "Histogram of IMP_TotalSulfurDioxide", 
       x = "IMP_TotalSulfurDioxide", 
       y = "Frequency" ) +
  theme_bw()

plot2 <- ggplot(wine.Data, mapping = aes(x=IMP_ResidualSugar))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
    labs(title = "Histogram of IMP_ResidualSugar", 
       x = "IMP_ResidualSugar", 
       y = "Frequency" ) +
  theme_bw()

plot3 <- ggplot(wine.Data, mapping = aes(x=IMP_FreeSulfurDioxide))+
  geom_histogram(fill = "deepskyblue3",
                 colour="black") + 
  labs(title = "Histogram of IMP_FreeSulfurDioxide", 
       x = "IMP_FreeSulfurDioxide", 
       y = "Frequency" ) +
  theme_bw()

grid.arrange(plot1, plot2, plot3, nrow = 2)
#--------------------------
##Make a copy of the data ---START FROM HERE
#--------------------------

View(t(basicStats(wine.Data[sapply(wine.Data,is.numeric)])))

# Save the R data frame as an .RData object
saveRDS(wine.Data,file="wine.Data_Fixed.RData" );


# Read (or reload) the .RData object as an R data frame
wine.Data_fixed <- readRDS(file= "wine.Data_Fixed.RData");

#--------------------------
##creating a drop list
#-------------------------------------------------------------------------------
names(wine.Data)


#creating a drop list to remove not required variables.
drop.list <- c('ResidualSugar','Chlorides', 'FreeSulfurDioxide',
               'TotalSulfurDioxide','pH', 'Sulphates','Alcohol','STARS'
               )

#droping the variables
wine.Data_fixed <- wine.Data_fixed[,!(names(wine.Data_fixed) %in% drop.list )]

names(wine.Data_fixed)
summary(wine.Data_fixed)
View(wine.Data_fixed)

View(t(basicStats(wine.Data_fixed[sapply(wine.Data_fixed,is.numeric)])))
#------------------------------------------------------------------
#Requirement # Add a train/test flag to split the sample  
#------------------------------------------------------------------


wine.Data_fixed$u <- runif(n=dim(wine.Data_fixed)[1],min=0,max=1);
wine.Data_fixed$train <- ifelse(wine.Data_fixed$u<0.70,1,0);

#Save the R data frame as an .RData object
saveRDS(wine.Data_fixed,file="wine.Data_fixed_train.RData" );

# Check the counts on the train/test split
table(wine.Data_fixed$train)

# Check the train/test split as a percentage of whole
table(wine.Data_fixed$train)/dim(wine.Data_fixed)[1]


#------------------------------------------------------------------------------
## 3- Build Models
#-------------------------------------------------------------------------------

options(scipen = 999)

# Read (or reload) the .RData object as an R data frame
wine.Data_fixed_Final <- readRDS(file= "wine.Data_fixed_train.RData");

names(wine.Data_fixed_Final)
View(wine.Data_fixed_Final)

# Create train/test split;
train.df <- subset(wine.Data_fixed_Final, train==1);
test.df <- subset(wine.Data_fixed_Final, train==0);

View(t(basicStats(wine.Data_fixed_Final[sapply(wine.Data_fixed_Final,is.numeric)])))

##Checking Mean and Variance - SHowing Variance over Mean - OverDispersion
mean(wine.Data_fixed_Final[which(wine.Data_fixed_Final$TARGET >0),"TARGET"])
var(wine.Data_fixed_Final[which(wine.Data_fixed_Final$TARGET >0),"TARGET"])

mean(train.df[which(train.df$TARGET >0),"TARGET"])
var(train.df[which(train.df$TARGET >0),"TARGET"])

mean(test.df[which(test.df$TARGET >0),"TARGET"])
var(test.df[which(test.df$TARGET >0),"TARGET"])

# #------------------------------
# ### ##Model_1_POIS - FINAL
# #------------------------------

names(train.df)

# Define the upper model as the FULL model
Model_1_POIS <- glm(TARGET ~ IMP_STARS + M_STARS + 
                      LabelAppeal + AcidIndex + 
                      VolatileAcidity + IMP_Alcohol +
                      IMP_Chlorides + IMP_TotalSulfurDioxide
                    ,family = poisson
                    ,data = train.df )
summary(Model_1_POIS)

# #------------------------------
# ### ##Model_2_NB - Negative Binomial - FINAL
# #------------------------------

names(train.df)

# Define the upper model as the FULL model
Model_2_NB <- glm.nb(TARGET ~ IMP_STARS + M_STARS + LabelAppeal +
                      AcidIndex + VolatileAcidity 
                    ,data = train.df )
summary(Model_2_NB)

# #------------------------------
# ### ##Model_3_ZIP - Zero Inflated POisson - FINAL
# #------------------------------

names(train.df)
# Define the upper model as the FULL model
Model_3_ZIP <- zeroinfl(TARGET ~  LabelAppeal + AcidIndex +
                           IMP_Alcohol + 
                           IMP_STARS + M_STARS
                         |  VolatileAcidity +
                           LabelAppeal + AcidIndex +
                           IMP_TotalSulfurDioxide  + IMP_pH + 
                           IMP_Sulphates +  IMP_Alcohol +
                           IMP_STARS + M_STARS 
                         ,data = train.df )
summary(Model_3_ZIP)

# #------------------------------
# ### ##Model_4_ZIP_NB - Zero Inflated Negative Binomial Distribution _ FINAL
# #------------------------------

names(train.df)

# Define the upper model as the FULL model
Model_4_ZIP_NB <- zeroinfl(TARGET ~  LabelAppeal + AcidIndex +
                           IMP_Alcohol + VolatileAcidity +
                           IMP_STARS + M_STARS |  VolatileAcidity + LabelAppeal + AcidIndex +
                           IMP_pH + 
                           IMP_STARS + M_STARS
                         ,data = train.df, dist="negbin") ##, EM=TRUE)
summary(Model_4_ZIP_NB)

# #------------------------------
# ### ##Model_5_LM
# #------------------------------


# Define the upper model as the FULL model
Model_5_LM <- lm(TARGET ~ IMP_STARS   +  M_STARS +
                     LabelAppeal +  AcidIndex +
                     VolatileAcidity + IMP_Chlorides +
                     IMP_TotalSulfurDioxide + IMP_Alcohol + M_Alcohol
                   ,data = train.df )
summary(Model_5_LM)


#------------------------------
##Model_5_lm - Assessing the Goodness-Of-Fit in OLS Regression
#------------------------------ 

# Validating the normality assumption:
par(mfrow = c(1,2))
#Creating 2 Q-Q plots to evaluate the distribution of 
# SalePrice and L_SalePrice
qqnorm(Model_5_LM$residuals, main = "Q-Q plot, Rediduals", 
       xlab="Theoretical Quantiles", col = "black",
       ylab="Standardized residuals",datax=FALSE)

qqline(Model_5_LM$residuals, datax=FALSE, distribution=qnorm,
       probs=c(0.25,0.75),qtype=7, col = "red")

hist(Model_5_LM$residuals, breaks = "FD", col = "violet"); box();
par(mfrow = c(1,1))

#Validating the homoscedasticity assumption (equal variance):
residualPlots(Model_5_LM)
# par(mfrow = c(1,1))
# residualPlot(Model_1_lm)

# #--------------------------------------------------------------------
# ## Predictive Adequcy
# #---------------------------------------------------------------------

#------------------------------------
# CHECK FOR Summary Statistic
#------------------------------------
summary(Model_1_POIS)
summary(Model_2_NB)
summary(Model_3_ZIP)
summary(Model_4_ZIP_NB)
summary(Model_5_LM)

#------------------------------------
# Convert Coefficients into count and %
#------------------------------------

#--Model 1
expCoeff_Model_1_POIS<- as.data.frame(cbind(Model_1_POIS$coefficients,
                                      exp(Model_1_POIS$coefficients), 
                                      100*(exp(Model_1_POIS$coefficients)-1)))
names(expCoeff_Model_1_POIS) <- c("Coefficients","Count", "Percent")
View(expCoeff_Model_1_POIS)

#-- Model 2

expCoeff_Model_2_NB <- as.data.frame(cbind(Model_2_NB$coefficients,
                                     exp(Model_2_NB$coefficients), 
                                     100*(exp(Model_2_NB$coefficients)-1)))
names(expCoeff_Model_2_NB) <- c("Coefficients","Count", "Percent")
View(expCoeff_Model_2_NB)

#Model 3

  names(Model_3_ZIP)
  #--Count
  Model_3_ZIP$coefficients$count
expCoeff_Model_3_ZIP_Count<- as.data.frame(cbind(Model_3_ZIP$coefficients$count,
                                      exp(Model_3_ZIP$coefficients$count), 
                                      100*(exp(Model_3_ZIP$coefficients$count)-1)))
  names(expCoeff_Model_3_ZIP_Count) <- c("Coefficients","Count", "Percent")
  View(expCoeff_Model_3_ZIP_Count)
  
  #--Zero
  Model_3_ZIP$coefficients$zero
  
Odds_Ratio_Model_3_ZIP_Zero <-  as.data.frame(cbind(Model_3_ZIP$coefficients$zero,
                                      exp(Model_3_ZIP$coefficients$zero), 
                                      100*(exp(Model_3_ZIP$coefficients$zero)-1)))

    
    #as.data.frame(exp(Model_3_ZIP$coefficients$zero))
  names(Odds_Ratio_Model_3_ZIP_Zero) <- c("Coefficients","Odds Ratio - Zero", "Percent")
  View(Odds_Ratio_Model_3_ZIP_Zero)


#Model 4

  summary(Model_4_ZIP_NB)  
  names(Model_4_ZIP_NB)
  
  #--Count
  Model_4_ZIP_NB$coefficients$count
expCoeff_Model_4_ZIP_NB_Count <- as.data.frame(cbind(Model_4_ZIP_NB$coefficients$count,
                                           exp(Model_4_ZIP_NB$coefficients$count), 
                                           100*(exp(Model_4_ZIP_NB$coefficients$count)-1)))
  names(expCoeff_Model_4_ZIP_NB_Count) <- c("coefficients","Count", "Percent")
  View(expCoeff_Model_4_ZIP_NB_Count)
  
  #--Zero
  Model_4_ZIP_NB$coefficients$zero
  
  Odds_Ratio_Model_4_ZIP_NB_Zero <- as.data.frame(cbind(Model_4_ZIP_NB$coefficients$zero,
                                          exp(Model_4_ZIP_NB$coefficients$zero), 
                                          100*(exp(Model_4_ZIP_NB$coefficients$zero)-1)))
    
    #as.data.frame(exp(Model_4_ZIP_NB$coefficients$zero))
  names(Odds_Ratio_Model_4_ZIP_NB_Zero) <- c("Coefficients","Odds Ratio - Zero", "Percent")
  View(Odds_Ratio_Model_4_ZIP_NB_Zero)
  
#Model 5

  expCoeff_Model_5_LM      <- as.data.frame(cbind(Model_5_LM$coefficients))
  names(expCoeff_Model_5_LM) <- c("Coefficients")
  View(expCoeff_Model_5_LM)


#------------------------------------
# CHECK FOR AIC Values for Each Model
#------------------------------------
AIC(Model_1_POIS)
AIC(Model_2_NB)
AIC(Model_3_ZIP)
AIC(Model_4_ZIP_NB)
AIC(Model_5_LM)

#------------------------------------
# Calculate MSE and MAE Values for Each Model
# for Train and Test
#------------------------------------
#-------------------
#Model_1_POIS
#-------------------

#---TRAIN
#MSE
mse.Model_1_POIS <- mean(Model_1_POIS$residuals^2)

##MAE
mae.Model_1_POIS <- mean(abs(Model_1_POIS$residuals))

#---TEST
##-get the predicted/fitted values
Pred.Model_Pois <- predict(Model_1_POIS, 
                           newdata = test.df, type = "response")

#MSE
mse.Model_POIS_TEST <- mean((test.df$TARGET - Pred.Model_Pois)^2)
#MAE
mae.Model_POIS_TEST <- mean(abs(test.df$TARGET - Pred.Model_Pois))


#-------------------
#Model_2_NB
#-------------------

#--TRAIN
##MSE
mse.Model_2_NB <- mean(Model_2_NB$residuals^2)
##MAE
mae.Model_2_NB <- mean(abs(Model_2_NB$residuals))

#--TEST

##-get the predicted/fitted values
Pred.Model_NB <- predict(Model_2_NB, newdata = test.df, type = "response")

#MSE
mse.Model_NB_TEST <- mean((test.df$TARGET - Pred.Model_NB)^2)
#MAE
mae.Model_NB_TEST <- mean(abs(test.df$TARGET - Pred.Model_NB))


#-------------------
#Model_3_ZIP
#-------------------

summary(Model_3_ZIP)


#train.df
TEMP_ZIP <- with(train.df ,
                                        (1.160645)      +
                     LabelAppeal     *  (0.226027)      + 
                     AcidIndex       *  (-0.022432)     +
                     IMP_Alcohol     *  (0.006575)      +
                     IMP_STARS       *  (0.119220)     +
                     M_STARS         *  (-0.124404))

P_SCORE_Zip_ALL <- exp(TEMP_ZIP)

TEMP_Zip_Zero <- with(train.df,
                                                  (-5.1897502)	  +
                        VolatileAcidity	        *	(	0.1885154	)	    +
                        LabelAppeal	            *	(	1.2245524	)	    +
                        AcidIndex	              *	(	0.4426177	)	    +
                        IMP_TotalSulfurDioxide	*	(-0.0011424	)	    +
                        IMP_pH	                *	(	0.2197314	)	    +
                        IMP_Sulphates	          *	(	0.0993321	)	    +
                        IMP_Alcohol	            *	(	0.0237497)	    +
                        IMP_STARS	              *	(-1.5964480	)	    +
                        M_STARS	                *	(	3.8688698	))

P_SCORE_Zip_ZERO <- exp(TEMP_Zip_Zero)/(1+exp(TEMP_Zip_Zero))

P_SCORE_Zip <- P_SCORE_Zip_ALL * (1 - P_SCORE_Zip_ZERO)

mse.Model_ZIP <- mean((train.df$TARGET - P_SCORE_Zip)^2)
mae.Model_ZIP <- mean(abs(train.df$TARGET - P_SCORE_Zip))

  
#test.df

TEMP_ZIP_test <- with(test.df ,
                                           (1.160645)      +
                        LabelAppeal     *  (0.226027)      + 
                        AcidIndex       *  (-0.022432)     +
                        IMP_Alcohol     *  (0.006575)      +
                        IMP_STARS       *  (0.119220)      +
                        M_STARS         *  (-0.124404))

P_SCORE_Zip_ALL_test <- exp(TEMP_ZIP_test)

TEMP_Zip_Zero_test <- with(test.df,
                                                        (-5.1897502)	  +
                             VolatileAcidity	        *	(	0.1885154	)	    +
                             LabelAppeal	            *	(	1.2245524	)	    +
                             AcidIndex	              *	(	0.4426177	)	    +
                             IMP_TotalSulfurDioxide	  *	(-0.0011424	)	    +
                             IMP_pH	                  *	(	0.2197314	)	    +
                             IMP_Sulphates	          *	(	0.0993321	)	    +
                             IMP_Alcohol	            *	(	0.0237497)	    +
                             IMP_STARS	              *	(-1.5964480	)	    +
                             M_STARS	                *	(	3.8688698	))

P_SCORE_Zip_ZERO_test <- exp(TEMP_Zip_Zero_test)/(1+exp(TEMP_Zip_Zero_test))

P_SCORE_Zip_test <- P_SCORE_Zip_ALL_test * (1 - P_SCORE_Zip_ZERO_test)

mse.Model_ZIP_test <- mean((test.df$TARGET - P_SCORE_Zip_test)^2)
mae.Model_ZIP_test <- mean(abs(test.df$TARGET - P_SCORE_Zip_test))


#-------------------
#Model_4_ZIP_NB
#-------------------

#train.df
summary(Model_4_ZIP_NB)
TEMP_ZIP_NB <- with(train.df ,
                                      (1.168309)      +
                   LabelAppeal     *  (0.226210)      + 
                   AcidIndex       *  (-0.022410)     +
                   IMP_Alcohol     *  (0.006262)      +
                   VolatileAcidity *  (-0.015760)     +
                   IMP_STARS       *  (0.119048)      +
                   M_STARS         *  (-0.122895))     

P_SCORE_Zip_ALL_NB <- exp(TEMP_ZIP_NB)

TEMP_Zip_Zero_NB <- with(train.df,
                                                  ( -5.10487)	  +
                        VolatileAcidity	        *	(	0.19299	)	    +
                        LabelAppeal	            *	(	1.22646	)	    +
                        AcidIndex	              *	(	0.45084	)	    +
                        IMP_pH	                *	(	0.22918	)	    +
                        IMP_STARS	              *	(-1.59889	)	    +
                        M_STARS	                *	(	3.86298	))

P_SCORE_Zip_ZERO_NB <- exp(TEMP_Zip_Zero_NB)/(1+exp(TEMP_Zip_Zero_NB))

P_SCORE_Zip_NB <- P_SCORE_Zip_ALL_NB * (1 - P_SCORE_Zip_ZERO_NB)

mse.Model_ZIP_NB <- mean((train.df$TARGET - P_SCORE_Zip_NB)^2)
mae.Model_ZIP_NB <- mean(abs(train.df$TARGET - P_SCORE_Zip_NB))

#test.df

TEMP_ZIP_NB_test <- with(test.df ,
                                         (1.168309)      +
                      LabelAppeal     *  (0.226210)      + 
                      AcidIndex       *  (-0.022410)     +
                      IMP_Alcohol     *  (0.006262)      +
                      VolatileAcidity *  (-0.015760)     +
                      IMP_STARS       *  (0.119048)      +
                      M_STARS         *  (-0.122895))     

P_SCORE_Zip_ALL_NB_test <- exp(TEMP_ZIP_NB_test)

TEMP_Zip_Zero_NB_test <- with(test.df,
                                                      ( -5.10487)	    +
                           VolatileAcidity	        *	(	0.19299	)	    +
                           LabelAppeal	            *	(	1.22646	)	    +
                           AcidIndex	              *	(	0.45084	)	    +
                           IMP_pH	                  *	(	0.22918	)	    +
                           IMP_STARS	              *	(-1.59889	)	    +
                           M_STARS	                *	(	3.86298	))

P_SCORE_Zip_ZERO_NB_test <- exp(TEMP_Zip_Zero_NB_test)/(1+exp(TEMP_Zip_Zero_NB_test))

P_SCORE_Zip_NB_test <- P_SCORE_Zip_ALL_NB_test * (1 - P_SCORE_Zip_ZERO_NB_test)

mse.Model_ZIP_NB_test <- mean((test.df$TARGET - P_SCORE_Zip_NB_test)^2)
mae.Model_ZIP_NB_test <- mean(abs(test.df$TARGET - P_SCORE_Zip_NB_test))



# 
# #-------------------
# ### ##Model_5_LM
# #-------------------

##MSE
mse.Model_5_LM <- mean(Model_5_LM$residuals^2)

##MAE
mae.Model_5_LM <- mean(abs(Model_5_LM$residuals))




##-get the predicted/fitted values
Pred.Model_LM <- predict(Model_5_LM, newdata = test.df)

#MSE
mse.Model_LM_TEST <- mean((test.df$TARGET - Pred.Model_LM)^2)
#MAE
mae.Model_LM_TEST <- mean(abs(test.df$TARGET - Pred.Model_LM))






#------------------------------------
# CHECK FOR MSE Values for Each Model
#------------------------------------

#Model_1
  mse.Model_1_POIS
  mse.Model_POIS_TEST
  
  mae.Model_1_POIS
  mae.Model_POIS_TEST 

#Model_2
  mse.Model_2_NB
  mse.Model_NB_TEST
  
  mae.Model_2_NB
  mae.Model_NB_TEST 

#Model_3
  mse.Model_ZIP
  mse.Model_ZIP_test
  
  mae.Model_ZIP
  mae.Model_ZIP_test

#Model_4
  mse.Model_ZIP_NB
  mse.Model_ZIP_NB_test
  
  mae.Model_ZIP_NB
  mae.Model_ZIP_NB_test

#Model_5
  mse.Model_5_LM
  mse.Model_LM_TEST
  
  mae.Model_5_LM
  mae.Model_LM_TEST 

  #--------------------------------
  # THE END
  #-------------------------------