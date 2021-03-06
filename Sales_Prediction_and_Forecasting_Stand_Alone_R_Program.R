#------------------------------------------------------------------
# Sales Prediction and Forecasting - Wine Sales
# Singh, Gurjeet
# Stand-Alone program 
#------------------------------------------------------------------

library(readr)
library(fBasics)

options(scipen = 999)
#----------------------------------------------------------------------------
## 1 - Importing a Test File and check import
#----------------------------------------------------------------------------

View(wine_test)
summary(wine_test)
str(wine_test)
colnames(wine_test)[1] <- "INDEX"

#----------------------------------------------------------------------------
## 2 - DATA PREPARATION 
#----------------------------------------------------------------------------

#----------------------------------------
#clean missing values with median values
#----------------------------------------

summary(wine_test)
##clean missing values with median values

wine_test$IMP_ResidualSugar <- ifelse(is.na(wine_test$ResidualSugar),
                                      3.9,
                                      wine_test$ResidualSugar)
wine_test$M_ResidualSugar <- ifelse(is.na(wine_test$ResidualSugar),
                                    1, 0)

wine_test$IMP_Chlorides <-ifelse(is.na(wine_test$Chlorides),
                                 0.05,
                                 wine_test$Chlorides)
wine_test$M_Chlorides <- ifelse(is.na(wine_test$Chlorides),
                                1, 0)


wine_test$IMP_FreeSulfurDioxide <-ifelse(is.na(wine_test$FreeSulfurDioxide),
                                         30,
                                         wine_test$FreeSulfurDioxide)
wine_test$M_FreeSulfurDioxide <- ifelse(is.na(wine_test$FreeSulfurDioxide),
                                        1, 0)



wine_test$IMP_TotalSulfurDioxide <-ifelse(is.na(wine_test$TotalSulfurDioxide), 
                                          123,
                                          wine_test$TotalSulfurDioxide)

wine_test$M_TotalSulfurDioxide <- ifelse(is.na(wine_test$TotalSulfurDioxide),
                                         1, 0)


wine_test$IMP_pH <-ifelse(is.na(wine_test$pH), 
                          3.2,
                          wine_test$pH)

wine_test$M_pH <- ifelse(is.na(wine_test$pH),
                         1, 0)

wine_test$IMP_Sulphates <-ifelse(is.na(wine_test$Sulphates), 
                                 0.50,
                                 wine_test$Sulphates)

wine_test$M_Sulphates <- ifelse(is.na(wine_test$Sulphates),
                                1, 0)


wine_test$IMP_Alcohol <-ifelse(is.na(wine_test$Alcohol), 
                               10.40,
                               wine_test$Alcohol)

wine_test$M_Alcohol <- ifelse(is.na(wine_test$Alcohol),
                              1, 0)



wine_test$IMP_STARS <-ifelse(is.na(wine_test$STARS) & (wine_test$LabelAppeal < -0.5), 1,
                             ifelse(is.na(wine_test$STARS) & (wine_test$LabelAppeal >= -0.5),2,
                                    wine_test$STARS))

wine_test$M_STARS <- ifelse(is.na(wine_test$STARS),
                            1, 0)


# These have mean/median very low but min and max very high
#-------------------------------


wine_test$IMP_TotalSulfurDioxide = ifelse(wine_test$IMP_TotalSulfurDioxide < -350, -350,
                                          ifelse(wine_test$IMP_TotalSulfurDioxide > 620, 620,
                                                 wine_test$IMP_TotalSulfurDioxide))

wine_test$IMP_ResidualSugar = ifelse(wine_test$IMP_ResidualSugar < -50, -50,
                                     ifelse(wine_test$IMP_ResidualSugar > 75, 75,
                                            wine_test$IMP_ResidualSugar))

wine_test$IMP_FreeSulfurDioxide = ifelse(wine_test$IMP_FreeSulfurDioxide < -300, -300,
                                         ifelse(wine_test$IMP_FreeSulfurDioxide > 350, 350,
                                                wine_test$IMP_FreeSulfurDioxide))

summary(wine_test)

#------------------------------------------------------------------------------
## 3- MODEL Deployment
#-------------------------------------------------------------------------------
#-------------------------
## Exporting Model - Poisson and Logistic Model
#-------------------------

#wine_test
TEMP_ZIP_Wine_test <- with(wine_test ,
                      (1.160645)      +
                        LabelAppeal     *  (0.226027)      + 
                        AcidIndex       *  (-0.022432)     +
                        IMP_Alcohol     *  (0.006575)      +
                        IMP_STARS       *  (0.119220)      +
                        M_STARS         *  (-0.124404))

P_SCORE_Zip_ALL_Wine_test <- exp(TEMP_ZIP_Wine_test)

TEMP_Zip_Zero_Wine_test <- with(wine_test,
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

P_SCORE_Zip_ZERO_Wine_test <- exp(TEMP_Zip_Zero_Wine_test)/(1+exp(TEMP_Zip_Zero_Wine_test))

P_TARGET <- P_SCORE_Zip_ALL_Wine_test * (1 - P_SCORE_Zip_ZERO_Wine_test)


hist(P_TARGET)

summary(P_TARGET)


#--------------------------------
## Exporting Model - Linear Model
#--------------------------------

P_TARGET

View(t(basicStats(P_TARGET)))
View(t(basicStats(round(P_TARGET,0))))

#--------------------------------
## Creating Scoring Ouput file
#--------------------------------

#Integer values
FINAL_Submission <- with(wine_test, 
                         cbind.data.frame(INDEX, 
                                          round(P_TARGET,0)))

colnames(FINAL_Submission) <- c("INDEX", "P_TARGET")
write.csv(FINAL_Submission, 
          "Singh_Gurjeet_Wine_Sales_Test_Score_IntegerValues.csv")

#Decimal values
FINAL_Submission_dec <- with(wine_test, 
                         cbind.data.frame(INDEX, P_TARGET))

colnames(FINAL_Submission_dec) <- c("INDEX", "P_TARGET")
write.csv(FINAL_Submission_dec, 
          "Singh_Gurjeet_Wine_Sales_Test_Score_DecimalValues.csv")











