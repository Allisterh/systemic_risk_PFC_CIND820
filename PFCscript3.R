# Four data sets are used
# 1) IMF World Economic Outlook Database by Country
# 2) Global Crises Data from Harvard Business School
# 3) World Housing Index
# 4) S&P 500 

# Import IMF World Economic Outlook Database by Country

library(readxl)
WEO_Data_large <- read_excel("~/Data Analytics/CIND 820/WMF/WEO_Data large.xlsx", 
                             col_types = c("text", "text", "skip", 
                                           "skip", "skip", "skip", "skip", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "skip"))
View(WEO_Data_large)

# Review Data and structure of Data
summary(WEO_Data_large)
str(WEO_Data_large)

## DATA CLEANING (IMF World Economic Outlook Database)

# Move Year columns to rows to align with Global Crisis Data set

library(reshape2)
WEO1 <-  melt(WEO_Data_large, id.vars=c("Country", "Measure"))
WEData <- dcast(WEO1, Country + variable~Measure, value.var = "value")

# Rename the "Variable" Column to "Year"
names(WEData)[2] <- "Year"

# Convert "Year" Column to numeric
WEData$Year <- as.character(WEData$Year)
WEData$Year <- as.numeric(WEData$Year)


# Review Data and structure of Data
summary(WEData)
str(WEData)

# Import Global Crises Data from Harvard Business School (excel)
# Import only required column for the predictor variable

library(readxl)
global_crisis_data <- read_excel("~/Data Analytics/CIND 820/WMF/global_crisis_data.xlsx", 
                                 col_types = c("skip", "skip", "text", 
                                               "numeric", "numeric", "skip", "numeric", 
                                               "skip", "skip", "skip", "skip", "skip", 
                                               "skip", "skip", "skip", "skip", "skip", 
                                               "skip", "skip", "skip", "skip", "skip", 
                                               "skip", "skip", "skip", "numeric", 
                                               "numeric"))
View(global_crisis_data)

## DATA CLEANING (Global Crises Data)

# Extract on data from 1980 to 2016 from global crises data set to match available data in IMF debt database

Gcrisisdata <- subset(global_crisis_data,global_crisis_data$Year >= 1980)

# Review Data and structure of Data
summary(Gcrisisdata)
str(Gcrisisdata)


# Import Global House Price Index

library(readr)
GlobalHousePriceIndex_IMF <- read_csv("~/Data Analytics/CIND 820/WMF/GlobalHousePriceIndex IMF.csv", 
                                      col_types = cols(`equally weighted` = col_number()))
View(GlobalHousePriceIndex_IMF)

GHPI  <-  GlobalHousePriceIndex_IMF

# Extract Annual data for q1 only

GHPI_ann <- GHPI[grepl(glob2rx('*q1'), GHPI$dateq),]

# Add the Year column and remove the q1 identifier

GHPI_ann$Year <- c(2000:2020)

GHPI_ann$dateq <- NULL
colnames(GHPI_ann) <- c("GHouseIndex", "Year")

# Data not available for the years 1980 to 1999, add NA's and use MICE to impute

GH_NAdata <- data.frame(GHouseIndex = c(NA), Year = c(1980:1999))

#Add to existing Housing Index Data

GHPI_ann<- rbind(GHPI_ann,GH_NAdata)


library(mice)

# Use Mice "norm predict" method to impute missing values
GHPI_ann.imp <- mice(GHPI_ann, m=50, method = 'norm.predict', seed = 101)
Imp_GHPI <- complete(GHPI_ann.imp)

#Merge Global Crises data set with Globing Housing Index

MGCGHI <- merge(Gcrisisdata,Imp_GHPI, by= c ("Year"))


# Import S & P 500 Annual data

library(readxl)
SP500 <- read_excel("~/Data Analytics/CIND 820/WMF/SP500 Annual data.xlsx", 
                                col_types = c("date", "numeric"))
View(SP500)

# Change date to year only

SP500$year <- format(as.Date(SP500$Date, format="%d/%m/%Y"),"%Y")
SP500$Date <- NULL
colnames(SP500) <- c("SP500", "Year")

#Merge Global Crises and Global Housing with S & P 500 data

MGCGHISP500 <- merge(MGCGHI,SP500, by= c ("Year"))



#Merge Global Crises data set with IMF World Economic Outlook Database

MergedPFC <- merge(WEData, MGCGHISP500, by= c("Country", "Year"))
str(MergedPFC)
summary(MergedPFC)

# Correct space in Column "Total Inv"
colnames(MergedPFC)[colnames(MergedPFC) == "Total Inv"] <- "TotalInv" 



## Explore and visualize data
library(DataExplorer)
library(htmltools)
library(jquerylib)
library(corrplot)
library(Hmisc)

# Exclude Predictor variables 
colnames(MergedPFC)
MPFC <- MergedPFC[, -c(41,42,43,44)]
colnames(MPFC)

predictors <- MergedPFC[,c(41:44)]
table(predictors$`Banking Crisis`)
table(predictors$`Systemic Crisis`)
table(predictors$`Currency Crises`)
table(predictors$`Inflation Crises`)

# For "Currency Crisis" replace "2" with "1" for binary predictor similar to other predictors.

predictors$`Currency Crises`[predictors$`Currency Crises` == "2"] <- 1

# Convert 0 to "NO" and 1 to "YES' for predictor variables

predictors$`Currency Crises`[predictors$`Currency Crises` == "0"] <- "NO"
predictors$`Currency Crises`[predictors$`Currency Crises` == "1"] <- "YES"

predictors$`Banking Crisis`[predictors$`Banking Crisis` == "0"] <- "NO"
predictors$`Banking Crisis`[predictors$`Banking Crisis` == "1"] <- "YES"

predictors$`Systemic Crisis`[predictors$`Systemic Crisis` == "0"] <- "NO"
predictors$`Systemic Crisis`[predictors$`Systemic Crisis` == "1"] <- "YES"

predictors$`Inflation Crises`[predictors$`Inflation Crises` == "0"] <- "NO"
predictors$`Inflation Crises`[predictors$`Inflation Crises` == "1"] <- "YES"

table(predictors$`Banking Crisis`)
table(predictors$`Systemic Crisis`)
table(predictors$`Currency Crises`)
table(predictors$`Inflation Crises`)




# Missing Data
plot_missing(MPFC)

# Visualize Data with Histogram
plot_histogram(MPFC)

# Visualize Data with Density plot
plot_density(MPFC)

# Visualize Data with Boxplot
BP <- plot_boxplot(MPFC, "Year")
BP$page_1
BP$page_2
BP$page_3
BP$page_4


# Visualize Data with Plot of each variable (Column)  *******
MPFCtest <- MPFC


ggplot(MPFCtest, aes(x=Year, y = CurrAccPerGDP)) +
  geom_line(color="grey") +
  geom_point(color="blue") +
  facet_wrap(~Country) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  labs(title = "Changes in CurrAccPerGDP",
       x = "Year",
       y = "CurrAccPerGDP") 

ggplot(data = MPFCtest, aes(x = Year, y = CurrAccPerGDP)) +
  geom_line() +
  facet_wrap(facets = vars(Country))+ labs(title = "Changes in CurrAccPerGDP",
                                          x = "Year",
                                          y = "CurrAccPerGDP") + geom_line(color="red") +
  geom_point(color="blue")

str(MPFCtest)

summary(MPFCtest)


# Visualize Data Correlation

# See both complete cases and pairwise complete cases
# Remove Country name

colnames(MPFC)
MPFC <- MPFC[-1]
corpwMPFC <- cor(MPFC, use = "pairwise.complete.obs")
corMPFC <- cor(MPFC, use = "complete.obs")

# Review correlation
round(corpwMPFC,2)
round(corMPFC,2)

# Review correlation and p-values

# Pearson Correlation
corrpearMPFC <- rcorr(as.matrix(MPFC), type = c("pearson"))
corrpearMPFC

# Spearman Correlation
corrspearMPFC <- rcorr(as.matrix(MPFC), type = c("spearman"))
corrspearMPFC


# Extract the correlation coefficients and p-values
MPFC_corr <- corrpearMPFC$r
# Extract p-values
MPFC_Pvals <- corrpearMPFC$P




# ++++++++++++++++++++++++++++
# flatten Corr Matrix
# from: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Get matrix of correlation coefficients and p-values
# PEARSON
corrpeardata <- flattenCorrMatrix(corrpearMPFC$r, corrpearMPFC$P)
View(corrpeardata)

# SPEARMAN
corrspeardata <- flattenCorrMatrix(corrspearMPFC$r, corrspearMPFC$P)
View(corrspeardata)


# Remove columns with >.90 or < -.90 correlation use Pearson excluding SP500 and Global Housing Index

Rem_Cols_MPFC <- subset(corrpeardata,corrpeardata$cor > 0.9 |corrpeardata$cor < -0.90)
View(Rem_Cols_MPFC)
print(Rem_Cols_MPFC$column)
colnames(MPFC)

MPFC <- subset(MPFC, select = -c(GDPcurpPCPPPidUnits,  GDPcurpPCUSDUnits, 
                                GDPcurpUSDBil, GDPfycurpNCBil,GGExpNCBil, GGGrossdebtNCBil,   
                                GGRevNCBil, GGRevPerGDP, INFcpiend, INFcpiPerch))


# Create full Data Explorer Report
create_report(MPFC[-1])



# Assess missing data
library(rio)
library(DEoptimR)
library(VIM)
library(mice)
library(VIM)
install_formats("fst")   # Includes‘hexView’, ‘pzfx’, ‘readODS’, ‘rmatio’)
pMiss <- function(x){sum(is.na(x))/length(x)*100}

# Percentage of Missing data by columns
colsmiss <- data.frame(apply(MPFC,2,pMiss))

# Percentage of Missing data by rows
rowsmiss <- data.frame(apply(MPFC,1,pMiss))


# Remove all rows with > 5% missing data
MPFC_nomissdarow <- MPFC[c(rowsmiss <= 5),] # Missing data rows removed
MPFC_nomissdarow_not <- MPFC[c(rowsmiss > 5),] # These are the missing data rows

# Remove all columns with > 5% missing data
MPFC_nomissdarow_col <- MPFC_nomissdarow[,c(colsmiss <= 5)] # Missing data columns removed
str(MPFC_nomissdarow_col)

# Rename column to remove spaces

colnames(MPFC_nomissdarow_col)[colnames(MPFC_nomissdarow_col) == "Gross Nat Savs"] <- "GrossNatSavs"  



# Impute missing values in data set with no missing rows or columns and predictor variables removed

# Use Mice "cart"(Classification and Regression Trees) method to impute missing values
PFC.imp <- mice(MPFC_nomissdarow_col, m=5, method = 'cart', seed = 101)
Imp_PFC <- complete(PFC.imp)
str(Imp_PFC)
summary(Imp_PFC)


# Review changes made

# Create full Data Explorer Report on cleaned and imputed data set "Imp_PFC"
create_report(Imp_PFC[-1])

# Add columns with predictor variables

# Adjust predictor rows to match Imputed file "Imp_PFC" number of rows, i.e. remove missing rows in predictor set
Pred <- predictors[c(rowsmiss <= 5),]

# Combine Imputed missing values with Predictors

Pred_PFC <- cbind(Imp_PFC, Pred)
str(Pred_PFC)
summary(Pred_PFC)
colnames(Pred_PFC)

# Rename column to remove space in name of predictor variable

colnames(Pred_PFC)[colnames(Pred_PFC) == "Currency Crises"] <- "CurrencyCrises"  
colnames(Pred_PFC)[colnames(Pred_PFC) == "Banking Crisis"] <- "BankingCrises"  
colnames(Pred_PFC)[colnames(Pred_PFC) == "Systemic Crisis"] <- "SystemicCrises"  
colnames(Pred_PFC)[colnames(Pred_PFC) == "Inflation Crises"] <- "InflationCrises"



# Convert predictor variables to factors

Pred_PFC$`CurrencyCrises` <- as.factor(Pred_PFC$`CurrencyCrises`)
Pred_PFC$`BankingCrises` <- as.factor(Pred_PFC$`BankingCrises`)
Pred_PFC$`SystemicCrises` <- as.factor(Pred_PFC$`SystemicCrises`)
Pred_PFC$`InflationCrises` <- as.factor(Pred_PFC$`InflationCrises`)
str(Pred_PFC)

library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(ggpubr)

par(mfrow=c(2, 2))
freq(Pred_PFC)
plot_num(Pred_PFC)







### DATA MODELLING  ###


library(caret)
library(doSNOW)
library(caTools)
library(stats)
library(randomForest)
library(lattice)
library(caret)
library(ggplot2)
library(ROCR)
library(regclass)

# Ensure only complete cases for running model
Pred_PFC <- Pred_PFC[complete.cases(Pred_PFC),]


# BANKING CRISIS PREDICTION
# Use only Banking crises as predictor, remove other predictors
colnames(Pred_PFC)
Pred_PFC_BC <-Pred_PFC[,-c(20,21,22)]

str(Pred_PFC_BC)

# Create train and test Data sets
set.seed(12345)
index <- sample(2, nrow(Pred_PFC_BC), replace = TRUE,prob = c(0.7,0.3))

Training <- Pred_PFC_BC[index ==1,]
Testing <- Pred_PFC_BC[index ==2,]

# training the model by assigning Currency Crisis column
# as target variable and rest other column
# as independent variable
set.seed(12345)
RFM <- randomForest(BankingCrises ~., data = Training)
RFM
# Run model on test set
set.seed(5678)
BankCrisis_Pred <- predict(RFM,Testing)
Testing$BankCrisis_Pred <- BankCrisis_Pred


# Create Confusion Matrix to asses model

#Accuracy = TP+TN/(TP+TN+FP+FN)

#Precision = TP/(TP+FP)

#Recall = TP/(TP + FN) (Same as Sensitivity)

# Sensitivity is the percentage of "NO" that are are correctly identified. (Same as Recall)
# Specificity is the percentage of "YES" that are are correctly identified. 

Conf_BankCrisis <- table(Testing$BankCrisis_Pred, Testing$BankingCrises,  dnn=c("Predicted", "Actual"))
Conf_BankCrisis

Accuracy_BankCrisis <- sum(diag(Conf_BankCrisis)/sum(Conf_BankCrisis))
Accuracy_BankCrisis


Model <- "DiagRF1_BC"
Crises <- "Banking"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_BankCrisis
Precision <- precision(Conf_BankCrisis)
Recall <- recall(Conf_BankCrisis)
Sensitivity <- sensitivity(Conf_BankCrisis)
Specificity <- specificity(Conf_BankCrisis)
F1 <- F_meas(Conf_BankCrisis)

DiagRF1_BC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)




# CURRENCY CRISIS PREDICTION
# Use only currency crises as predictor, remove other predictors
colnames(Pred_PFC)
Pred_PFC_CC <-Pred_PFC[,-c(19,20,22)]

str(Pred_PFC_CC)

# Create train and test Data sets

index <- sample(2, nrow(Pred_PFC_CC), replace = TRUE,prob = c(0.7,0.3))

Training <- Pred_PFC_CC[index ==1,]
Testing <- Pred_PFC_CC[index ==2,]

# Training the model by assigning Currency Crisis column
# as target variable and rest other column
# as independent variable
set.seed(12345)
RFM <- randomForest(CurrencyCrises ~., data = Training)
RFM
# Run model on test set
set.seed(5678)
CurrCrisis_Pred <- predict(RFM,Testing)
Testing$CurrCrisis_Pred <- CurrCrisis_Pred

# Create Confusion Matrix to asses model
Conf_CurrCrisis <- table(Testing$CurrCrisis_Pred, Testing$CurrencyCrises, dnn=c("Predicted", "Actual"))
Conf_CurrCrisis


Accuracy_CurrCrisis <- sum(diag(Conf_CurrCrisis)/sum(Conf_CurrCrisis))
Accuracy_CurrCrisis

Model <- "DiagRF1_cC"
Crises <- "Currency"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_CurrCrisis
Precision <- precision(Conf_CurrCrisis)
Recall <- recall(Conf_CurrCrisis)
Sensitivity <- sensitivity(Conf_CurrCrisis)
Specificity <- specificity(Conf_CurrCrisis)
F1 <- F_meas(Conf_CurrCrisis)

DiagRF1_CC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)




# SYSTEMIC CRISIS PREDICTION
# Use only Systemic crises as predictor, remove other predictors
colnames(Pred_PFC)
Pred_PFC_SC <-Pred_PFC[,-c(19,21,22)]

str(Pred_PFC_SC)

# Create train and test Data sets

index <- sample(2, nrow(Pred_PFC_SC), replace = TRUE,prob = c(0.7,0.3))

Training <- Pred_PFC_SC[index ==1,]
Testing <- Pred_PFC_SC[index ==2,]

# training the model by assigning Currency Crisis column
# as target variable and rest other column
# as independent variable
set.seed(12345)
RFM <- randomForest(SystemicCrises ~., data = Training)
RFM
# Run model on test set
set.seed(5678)
SystCrisis_Pred <- predict(RFM,Testing)
Testing$SystCrisis_Pred <- SystCrisis_Pred


# Create Confusion Matrix to asses model
Conf_SystCrisis <- table(Testing$SystCrisis_Pred,Testing$SystemicCrises,  dnn=c("Predicted", "Actual"))
Conf_SystCrisis

Accuracy_SystCrisis <- sum(diag(Conf_SystCrisis)/sum(Conf_SystCrisis))
Accuracy_SystCrisis

Model <- "DiagRF1_SC"
Crises <- "Systemic"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_SystCrisis
Precision <- precision(Conf_SystCrisis)
Recall <- recall(Conf_SystCrisis)
Sensitivity <- sensitivity(Conf_SystCrisis)
Specificity <- specificity(Conf_SystCrisis)
F1 <- F_meas(Conf_SystCrisis)

DiagRF1_SC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)




# INFLATION CRISIS PREDICTION
# Use only Inflation crises as predictor, remove other predictors
colnames(Pred_PFC)
Pred_PFC_IC <-Pred_PFC[,-c(19,20,21)]

str(Pred_PFC_IC)

# Create train and test Data sets

index <- sample(2, nrow(Pred_PFC_IC), replace = TRUE,prob = c(0.7,0.3))

Training <- Pred_PFC_IC[index ==1,]
Testing <- Pred_PFC_IC[index ==2,]

# training the model by assigning Currency Crisis column
# as target variable and rest other column
# as independent variable
set.seed(12345)
RFM1_IC <- randomForest(InflationCrises ~., data = Training)
RFM1_IC
# Run model on test set
set.seed(5678)
InflCrisis_Pred <- predict(RFM1_IC,Testing)
Testing$InflCrisis_Pred <- InflCrisis_Pred


# Create Confusion Matrix to asses model
Conf_InflCrisis <- table(Testing$InflCrisis_Pred, Testing$InflationCrises,  dnn=c("Predicted", "Actual"))
Conf_InflCrisis

Accuracy_InflCrisis <- sum(diag(Conf_InflCrisis)/sum(Conf_InflCrisis))
Accuracy_InflCrisis

Model <- "DiagRF1_IC"
Crises <- "Inflation"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_InflCrisis
Precision <- precision(Conf_InflCrisis)
Recall <- recall(Conf_InflCrisis)
Sensitivity <- sensitivity(Conf_InflCrisis)
Specificity <- specificity(Conf_InflCrisis)
F1 <- F_meas(Conf_InflCrisis)

DiagRF1_IC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)

All_DiagRF1 <- rbind(DiagRF1_BC,DiagRF1_CC, DiagRF1_SC,DiagRF1_IC)


#####
# Simplify Random Forest Modelling and use predictors as independent variables

# Create train and test Data sets

index <- sample(2, nrow(Pred_PFC), replace = TRUE,prob = c(0.7,0.3))

Training <- Pred_PFC[index ==1,]
Testing <- Pred_PFC[index ==2,]




## BANKING CRISES ##

# training the model by assigning Banking Crisis column
# as target variable and other columns
# as independent variable

set.seed(12345)
RFMBC <- randomForest(BankingCrises ~., data = Training)
RFMBC

# Run model on test set
set.seed(5678)
BC_Pred <- predict(RFMBC,Testing)
Testing$BC_Pred <- BC_Pred


# Create Confusion Matrix to asses model
Conf_BC <- table(Testing$BC_Pred, Testing$BankingCrises,  dnn=c("Predicted", "Actual"))
Conf_BC

Accuracy_BC <- sum(diag(Conf_BC)/sum(Conf_BC))
Accuracy_BC

Model <- "DiagRF2_BC"
Crises <- "Banking"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_BC
Precision <- precision(Conf_BC)
Recall <- recall(Conf_BC)
Sensitivity <- sensitivity(Conf_BC)
Specificity <- specificity(Conf_BC)
F1 <- F_meas(Conf_BC)

DiagRF2_BC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)




## CURRENCY CRISES ##

# training the model by assigning Currency Crisis column
# as target variable and other columns
# as independent variable

set.seed(12345)
RFMCC <- randomForest(CurrencyCrises ~., data = Training)
RFMCC

# Run model on test set
set.seed(5678)
CC_Pred <- predict(RFMCC,Testing)
Testing$CC_Pred <- CC_Pred


# Create Confusion Matrix to asses model
Conf_CC <- table(Testing$CC_Pred, Testing$CurrencyCrises,  dnn=c("Predicted","Actual"))
Conf_CC

Accuracy_CC <- sum(diag(Conf_CC)/sum(Conf_CC))

Model <- "DiagRF2_CC"
Crises <- "Currency"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_CC
Precision <- precision(Conf_CC)
Recall <- recall(Conf_CC)
Sensitivity <- sensitivity(Conf_CC)
Specificity <- specificity(Conf_CC)
F1 <- F_meas(Conf_CC)

DiagRF2_CC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1,Model)



## SYSTEMIC CRISES ##

# training the model by assigning Systemic Crisis column
# as target variable and other columns
# as independent variable

set.seed(12345)
RFMSC <- randomForest(SystemicCrises ~., data = Training)
RFMSC

# Run model on test set
set.seed(5678)
SC_Pred <- predict(RFMSC,Testing)
Testing$SC_Pred <- SC_Pred


# Create Confusion Matrix to asses model
Conf_SC <- table(Testing$SC_Pred, Testing$SystemicCrises,  dnn=c("Predicted","Actual"))
Conf_SC

Accuracy_SC <- sum(diag(Conf_SC)/sum(Conf_SC))
Accuracy_SC

Model <- "DiagRF2_SC"
Crises <- "Systemic"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_SC
Precision <- precision(Conf_SC)
Recall <- recall(Conf_SC)
Sensitivity <- sensitivity(Conf_SC)
Specificity <- specificity(Conf_SC)
F1 <- F_meas(Conf_SC)

DiagRF2_SC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



## INFLATION CRISES ##

# training the model by assigning Inflation Crisis column
# as target variable and other columns
# as independent variable

set.seed(12345)
RFMIC <- randomForest(InflationCrises ~., data = Training)
RFMIC

# Run model on test set
set.seed(5678)
IC_Pred <- predict(RFMIC,Testing)
Testing$IC_Pred <- IC_Pred


# Create Confusion Matrix to asses model
Conf_IC <- table(Testing$IC_Pred, Testing$InflationCrises,dnn=c("Predicted","Actual") )
Conf_IC

Accuracy_IC <- sum(diag(Conf_IC)/sum(Conf_IC))
Accuracy_IC

Model <- "DiagRF2_IC"
Crises <- "Inflation"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_IC
Precision <- precision(Conf_IC)
Recall <- recall(Conf_IC)
Sensitivity <- sensitivity(Conf_IC)
Specificity <- specificity(Conf_IC)
F1 <- F_meas(Conf_IC)

DiagRF2_IC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)

# Random Forest Diagnostics #


DiagRF_ALL <- rbind(DiagRF1_BC,DiagRF1_CC, DiagRF1_SC, DiagRF1_IC, DiagRF2_BC,DiagRF2_CC, DiagRF2_SC, DiagRF2_IC)







#####

# Using the above model with predictors included as Independent variables, now only the top 5 and top 7 most important
# independent variables will be used to train the model.



## BANKING CRISES Top 5 and 7 variables ##

# training the model by assigning Currency Crisis column
# as target variable and other columns
# as independent variable



varImp_RFMBC <- print(varImp(RFMBC))

### These are the top 7 important variables  ###
# SystemicCrises
# 89.697040
# GDPconpPerch
# 30.860586
# GDPconpPCPPPidUnits
# 19.191831
# TotalInv
# 13.823279
# INFcpi
# 13.582253
# GrossNatSavs
# 13.571722
# GDPdefIndex
# 13.330070

colnames(Training)
colnames(Testing)


# Use only top 5 Important variables

set.seed(12345)
RFMBCFS5 <- randomForest(BankingCrises ~., data = Training[,c(20,7,6,16,14,19)])
RFMBCFS5


# Run model on test set
set.seed(5678)
BC_PredFS5 <- predict(RFMBCFS5,Testing[,c(20,7,6,16,14,19)])
Testing$BC_PredFS5 <- BC_PredFS5


# Create Confusion Matrix to asses model
Conf_BCFS5 <- table(Testing$BC_PredFS5, Testing$BankingCrises,  dnn=c("Predicted", "Actual"))
Conf_BCFS5

Accuracy_BCFS5 <- sum(diag(Conf_BCFS5)/sum(Conf_BCFS5))
Accuracy_BCFS5

Model <- "DiagRF2_BCFS5"
Crises <- "Banking"
Preds_used <- "YES"
Feat_Sel <- "Yes  5"
Accuracy <- Accuracy_BCFS5
Precision <- precision(Conf_BCFS5)
Recall <- recall(Conf_BCFS5)
Sensitivity <- sensitivity(Conf_BCFS5)
Specificity <- specificity(Conf_BCFS5)
F1 <- F_meas(Conf_BCFS5)

DiagRF2_BCFS5 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



## Use only top 7 Important variables

set.seed(12345)
RFMBCFS7 <- randomForest(BankingCrises ~., data = Training[,c(20,7,6,16,14,11,13,19)])
RFMBCFS7


# Run model on test set
set.seed(5678)
BC_PredFS7 <- predict(RFMBCFS7,Testing[,c(20,7,6,16,14,11,13,19)])
Testing$BC_PredFS7 <- BC_PredFS7


# Create Confusion Matrix to asses model
Conf_BCFS7 <- table(Testing$BC_PredFS7, Testing$BankingCrises,  dnn=c("Predicted", "Actual"))
Conf_BCFS7

Accuracy_BCFS7 <- sum(diag(Conf_BCFS7)/sum(Conf_BCFS7))
Accuracy_BCFS7

Model <- "DiagRF2_BCFS7"
Crises <- "Banking"
Preds_used <- "YES"
Feat_Sel <- "Yes  7"
Accuracy <- Accuracy_BCFS7
Precision <- precision(Conf_BCFS7)
Recall <- recall(Conf_BCFS7)
Sensitivity <- sensitivity(Conf_BCFS7)
Specificity <- specificity(Conf_BCFS7)
F1 <- F_meas(Conf_BCFS7)

DiagRF2_BCFS7 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


DiagRF2_BCFS57 <- rbind(DiagRF1_BC, DiagRF2_BC, DiagRF2_BCFS5,DiagRF2_BCFS7)



## CURRENCY CRISES Top 5 and 7 variable*##


# training the model by assigning Currency Crisis column
# as target variable and other columns
# as independent variable



varImp_RFMCC <- print(varImp(RFMCC))

### These are the top 7 important variables  ###
# INFcpi
# 24.191093
# GDPdefIndex
# 21.474901
# InflationCrises
# 15.520173
# CurrAccUSDBil
# 14.294438
# GDPcurpPCNCUnits
# 14.227269
# GDPconpPerch
# 13.814647
# GDPconpPCNCUnits
# 13.261983

colnames(Training)
colnames(Testing)


# Use only top 5 Important variables

set.seed(12345)
RFMCCFS5 <- randomForest(CurrencyCrises ~., data = Training[,c(21,14,11,22,3,9)])
RFMCCFS5


# Run model on test set
set.seed(5678)
CC_PredFS5 <- predict(RFMCCFS5,Testing[,c(21,14,11,22,3,9)])
Testing$CC_PredFS5 <- CC_PredFS5


# Create Confusion Matrix to asses model
Conf_CCFS5 <- table(Testing$CC_PredFS5, Testing$CurrencyCrises,  dnn=c("Predicted", "Actual"))
Conf_CCFS5

Accuracy_CCFS5 <- sum(diag(Conf_CCFS5)/sum(Conf_CCFS5))
Accuracy_CCFS5

Model <- "DiagRF2_CCFS5"
Crises <- "Currency"
Preds_used <- "YES"
Feat_Sel <- "Yes  5"
Accuracy <- Accuracy_CCFS5
Precision <- precision(Conf_CCFS5)
Recall <- recall(Conf_CCFS5)
Sensitivity <- sensitivity(Conf_CCFS5)
Specificity <- specificity(Conf_CCFS5)
F1 <- F_meas(Conf_CCFS5)

DiagRF2_CCFS5 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



## Use only top 7 Important variables

set.seed(12345)
RFMCCFS7 <- randomForest(CurrencyCrises ~., data = Training[,c(21,14,11,22,3,9,7,5)])
RFMCCFS7


# Run model on test set
set.seed(5678)
CC_PredFS7 <- predict(RFMCCFS7,Testing[,c(21,14,11,22,3,9,7,5)])
Testing$CC_PredFS7 <- CC_PredFS7


# Create Confusion Matrix to asses model
Conf_CCFS7 <- table(Testing$CC_PredFS7, Testing$CurrencyCrises,  dnn=c("Predicted", "Actual"))
Conf_CCFS7

Accuracy_CCFS7 <- sum(diag(Conf_CCFS7)/sum(Conf_CCFS7))


Model <- "DiagRF2_CCFS7"
Crises <- "Currency"
Preds_used <- "YES"
Feat_Sel <- "Yes  7"
Accuracy <- Accuracy_CCFS7
Precision <- precision(Conf_CCFS7)
Recall <- recall(Conf_CCFS7)
Sensitivity <- sensitivity(Conf_CCFS7)
Specificity <- specificity(Conf_CCFS7)
F1 <- F_meas(Conf_CCFS7)

DiagRF2_CCFS7 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


DiagRF2_CCFS57 <- rbind(DiagRF1_CC, DiagRF2_CC, DiagRF2_CCFS5,DiagRF2_CCFS7)




## SYSTEMIC CRISES ##
## Top 5 and 7 variable ##

varImp_RFMSC <- print(varImp(RFMSC))

### These are the top 7 important variables  ###
# BankingCrises
# 60.633857
# GDPconpPerch
# 15.744477
# GDPconpPCPPPidUnits
# 11.098262
# INFcpi
# 10.869910
# GDPdefIndex
# 10.552741
# GDPcurpPCNCUnits
# 9.871795
# GDPconpPCNCUnits
# 9.700354

colnames(Training)
colnames(Testing)


# Use only top 5 Important variables

set.seed(12345)
RFMSCFS5 <- randomForest(SystemicCrises ~., data = Training[,c(20,19,7,6,14,11)])
RFMSCFS5


# Run model on test set
set.seed(5678)
SC_PredFS5 <- predict(RFMSCFS5,Testing[,c(20,19,7,6,14,11)])
Testing$SC_PredFS5 <- SC_PredFS5


# Create Confusion Matrix to asses model
Conf_SCFS5 <- table(Testing$SC_PredFS5, Testing$SystemicCrises,  dnn=c("Predicted", "Actual"))
Conf_SCFS5

Accuracy_SCFS5 <- sum(diag(Conf_SCFS5)/sum(Conf_SCFS5))


Model <- "DiagRF2_SCFS5"
Crises <- "Systemic"
Preds_used <- "YES"
Feat_Sel <- "Yes  5"
Accuracy <- Accuracy_SCFS5
Precision <- precision(Conf_SCFS5)
Recall <- recall(Conf_SCFS5)
Sensitivity <- sensitivity(Conf_SCFS5)
Specificity <- specificity(Conf_SCFS5)
F1 <- F_meas(Conf_SCFS5)

DiagRF2_SCFS5 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



## Use only top 7 Important variables

set.seed(12345)
RFMSCFS7 <- randomForest(SystemicCrises ~., data = Training[,c(20,19,7,6,14,11,9,5)])
RFMSCFS7


# Run model on test set
set.seed(5678)
SC_PredFS7 <- predict(RFMSCFS7,Testing[,c(20,19,7,6,14,11,9,5)])
Testing$SC_PredFS7 <- SC_PredFS7


# Create Confusion Matrix to asses model
Conf_SCFS7 <- table(Testing$SC_PredFS7, Testing$SystemicCrises,  dnn=c("Predicted", "Actual"))
Conf_SCFS7

Accuracy_SCFS7 <- sum(diag(Conf_SCFS7)/sum(Conf_SCFS7))


Model <- "DiagRF2_SCFS7"
Crises <- "Systemic"
Preds_used <- "YES"
Feat_Sel <- "Yes  7"
Accuracy <- Accuracy_SCFS7
Precision <- precision(Conf_SCFS7)
Recall <- recall(Conf_CCFS7)
Sensitivity <- sensitivity(Conf_SCFS7)
Specificity <- specificity(Conf_SCFS7)
F1 <- F_meas(Conf_SCFS7)

DiagRF2_SCFS7 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


DiagRF2_SCFS57 <- rbind(DiagRF1_SC, DiagRF2_SC, DiagRF2_SCFS5,DiagRF2_SCFS7)







## INFLATION CRISES ##
## Top 5 and 7 variable ##

varImp_RFMIC <- print(varImp(RFMIC))

### These are the top 7 important variables  ###
# INFcpi
# 27.0939789
# GDPdefIndex
# 23.1670611
# GDPcurpPCNCUnits
# 10.8942274
# GDPcurpNCBil
# 9.3525912
# GDPconpPCPPPidUnits
# 8.2583025
# CurrencyCrises
# 8.0917822
# GDPconpPCNCUnits
# 7.2063882

colnames(Training)
colnames(Testing)


# Use only top 5 Important variables

set.seed(12345)
RFMICFS5 <- randomForest(InflationCrises ~., data = Training[,c(22,14,11,9,8,6)])
RFMICFS5


# Run model on test set
set.seed(5678)
IC_PredFS5 <- predict(RFMICFS5,Testing[,c(22,14,11,9,8,6)])
Testing$IC_PredFS5 <- IC_PredFS5


# Create Confusion Matrix to asses model
Conf_ICFS5 <- table(Testing$IC_PredFS5, Testing$InflationCrises,  dnn=c("Predicted", "Actual"))
Conf_ICFS5

Accuracy_ICFS5 <- sum(diag(Conf_ICFS5)/sum(Conf_ICFS5))


Model <- "DiagRF2_ICFS5"
Crises <- "Inflation"
Preds_used <- "YES"
Feat_Sel <- "Yes  5"
Accuracy <- Accuracy_ICFS5
Precision <- precision(Conf_ICFS5)
Recall <- recall(Conf_ICFS5)
Sensitivity <- sensitivity(Conf_ICFS5)
Specificity <- specificity(Conf_ICFS5)
F1 <- F_meas(Conf_ICFS5)

DiagRF2_ICFS5 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



## Use only top 7 Important variables

set.seed(12345)
RFMICFS7 <- randomForest(InflationCrises ~., data = Training[,c(22,14,11,9,8,6,21,5)])
RFMICFS7


# Run model on test set
set.seed(5678)
IC_PredFS7 <- predict(RFMICFS7,Testing[,c(22,14,11,9,8,6,21,5)])
Testing$IC_PredFS7 <- IC_PredFS7


# Create Confusion Matrix to asses model
Conf_ICFS7 <- table(Testing$IC_PredFS7, Testing$InflationCrises,  dnn=c("Predicted", "Actual"))
Conf_ICFS7

Accuracy_ICFS7 <- sum(diag(Conf_ICFS7)/sum(Conf_ICFS7))


Model <- "DiagRF2_ICFS7"
Crises <- "Inflation"
Preds_used <- "YES"
Feat_Sel <- "Yes  7"
Accuracy <- Accuracy_ICFS7
Precision <- precision(Conf_ICFS7)
Recall <- recall(Conf_ICFS7)
Sensitivity <- sensitivity(Conf_ICFS7)
Specificity <- specificity(Conf_ICFS7)
F1 <- F_meas(Conf_ICFS7)

DiagRF2_ICFS7 <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


DiagRF2_ICFS57 <- rbind(DiagRF1_IC, DiagRF2_IC, DiagRF2_ICFS5,DiagRF2_ICFS7)


Diag_ALLRF <- rbind(DiagRF2_BCFS57, DiagRF2_CCFS57, DiagRF2_SCFS57, DiagRF2_ICFS57)










##############################################################################





# Logistic Regression Modelling


## Banking Crises ##

## Use only Independent Variables no predictor variables

###### 1 #########

colnames(Training)

Logmod_BC <- glm(BankingCrises ~., family="binomial", data=Training[,c(1:18,19)])

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod_BC)


# The interpretation of McFadden's pseudo $R^2$ between 0.2-0.4 comes from a book chapter 
# he contributed to: Behavioral Travel Modelling. Edited by David Hensher and Peter Stopher. 1979. 
# McFadden contributed Ch. 15 "Quantitative Methods for Analyzing Travel Behaviour on Individuals: 
# Some Recent Developments". Discussion of model evaluation (in the context of multinomial logit models) 
# begins on page 306 where he introduces $\rho^2$ (McFadden's pseudo $R^2$). McFadden states 
# "while the $R^2$ index is a more familiar concept to planner who are experienced in OLS, 
# it is not as well behaved as the $\rho^2$ measure, for ML estimation. Those unfamiliar with 
# $\rho^2$ should be forewarned that its values tend to be considerably lower than those of the 
# $R^2$ index...For example, values of 0.2 to 0.4 for $\rho^2$ represent EXCELLENT fit."
# From:http://stats.stackexchange.com/questions/82105/ddg#99615


# Calculate McFadden pseudo R2

MFRlogmod1BC <- pscl::pR2(Logmod_BC)["McFadden"]
print(MFRlogmod1BC)


# Use model on Testing set
colnames(Testing)

Pred_Logmod_BC <- predict(Logmod_BC, Testing[,c(1:18,19)])

Testing$BC_Logmod1Pred <- Pred_Logmod_BC

summary(Testing$BC_Logmod1Pred)

hist(Testing$BC_Logmod1Pred)
# use Zero as the cut-off, less than or equal to Zero then "NO", else "YES"

Testing$BC_Log1pred <- ifelse(Testing$BC_Logmod1Pred <= 0, "NO", "YES")
Testing$BC_Log1pred <- as.factor(Testing$BC_Log1pred)




# Create Confusion Matrix to asses model
Conf_BC <- table(Testing$BankingCrises, Testing$BC_Log1pred)
Conf_BC

# Logistic Model Diagnostics #

Accuracy_BC <- sum(diag(Conf_BC)/sum(Conf_BC))
Accuracy_BC


Model <- "LogReg1_BC"
Crises <- "Banking"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_BC
Precision <- precision(Conf_BC)
Recall <- recall(Conf_BC)
Sensitivity <- sensitivity(Conf_BC)
Specificity <- specificity(Conf_BC)
F1 <- F_meas(Conf_BC)

LogReg1_BC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)

###### 2 #########
## Use Predictor Variables in Model
# training the model by assigning Banking Crisis column
# as target variable and other columns
# as independent variable

colnames(Training)

Logmod_BC <- glm(BankingCrises ~., family="binomial", data=Training)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod_BC)


# Calculate McFadden pseudo R2

MFRlogmod2BC <- pscl::pR2(Logmod_BC)["McFadden"]
print(MFRlogmod2BC)



# Use model on Testing set

Pred_Logmod_BC <- predict(Logmod_BC, Testing)

Testing$BC_Logmod2Pred <- Pred_Logmod_BC

summary(Testing$BC_Logmod2Pred)
hist(Testing$BC_Logmod2Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$BC_Log2pred <- ifelse(Testing$BC_Logmod2Pred <= 0, "NO", "YES")
Testing$BC_Log2pred <- as.factor(Testing$BC_Log2pred)




# Create Confusion Matrix to asses model
Conf_BC <- table(Testing$BankingCrises, Testing$BC_Log2pred)
Conf_BC

# Logistic Model Diagnostics #

Accuracy_BC <- sum(diag(Conf_BC)/sum(Conf_BC))
Accuracy_BC


Model <- "LogReg2_BC"
Crises <- "Banking"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_BC
Precision <- precision(Conf_BC)
Recall <- recall(Conf_BC)
Sensitivity <- sensitivity(Conf_BC)
Specificity <- specificity(Conf_BC)
F1 <- F_meas(Conf_BC)

LogReg2_BC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



###### 3 #########

## Use Predictor Variables in Model and choose only VARIABLES THAT ARE 95% OR MORE STASTICALLY SIGNIFICANT
# training the model by assigning Banking Crisis column
# as target variable and columns 95% OR MORE STASTICALLY SIGNIFICANT
# as independent variable

colnames(Training)

#view model summary
Selvar_BC <- summary(Logmod_BC)$coefficients
Selvar_BC

# Estimate
# Std. Error
# z value
# Pr(>|z|)
# SystemicCrisesYES
# 5.4303267869158373671
# 0.4727788089197754728
# 11.4859776
# 0.000000000000000000000000000001551703
# Population
# 0.0040599769600551680
# 0.0007650601037680386
# 5.3067425
# 0.000000111601663980597871060097825424
# GDPconpPCPPPidUnits
# 0.0000467658907818867
# 0.0000104529835671443
# 4.4739275
# 0.000007679578503408849858048734748905
# GDPconpPerch
# -0.1504998775125298838
# 0.0435681906736562694
# -3.4543523
# 0.000551616374404432802808218028189913
# SP500
# -0.0016746375150235484
# 0.0005952409615265090
# -2.8133775
# 0.004902406454918010157706387275311499


## Variable importance from caret package identifies the same as using p-values

VarimpLogmod_BC <- caret::varImp(Logmod_BC)


# SystemicCrisesYES
# 11.4859776
# Population
# 5.3067425
# GDPconpPCPPPidUnits
# 4.4739275
# GDPconpPerch
# 3.4543523
# SP500
# 2.8133775


## Therefore train model with only 95% OR MORE STASTICALLY SIGNIFICANT as identified above

colnames(Training)

Logmod_BC <- glm(BankingCrises ~., family="binomial", data=Training[,c(20,15,6,7,18,19)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod_BC)


# Calculate McFadden pseudo R2

MFRlogmod3BC <- pscl::pR2(Logmod_BC)["McFadden"]
print(MFRlogmod3BC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_BC <- predict(Logmod_BC, Testing[,c(20,15,6,7,18,19)])

Testing$BC_Logmod3Pred <- Pred_Logmod_BC

summary(Testing$BC_Logmod3Pred)
hist(Testing$BC_Logmod3Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$BC_Log3pred <- ifelse(Testing$BC_Logmod3Pred <= 0, "NO", "YES")
Testing$BC_Log3pred <- as.factor(Testing$BC_Log3pred)




# Create Confusion Matrix to asses model
Conf_BC <- table(Testing$BankingCrises, Testing$BC_Log3pred)
Conf_BC

# Logistic Model Diagnostics #

Accuracy_BC <- sum(diag(Conf_BC)/sum(Conf_BC))
Accuracy_BC


Model <- "LogReg3_BC"
Crises <- "Banking"
Preds_used <- "YES"
Feat_Sel <- "Yes SS"
Accuracy <- Accuracy_BC
Precision <- precision(Conf_BC)
Recall <- recall(Conf_BC)
Sensitivity <- sensitivity(Conf_BC)
Specificity <- specificity(Conf_BC)
F1 <- F_meas(Conf_BC)

LogReg3_BC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


######## 4 #########


## Use Predictor Variables in Model and choose only TOP 7 VARIABLES
# training the model by assigning Banking Crisis column
# as target variable

Logmod_BC <- glm(BankingCrises ~., family="binomial", data=Training)


colnames(Training)

## Variable importance from caret package identifies TOP 7 variables

VarimpLogmod_BC <- caret::varImp(Logmod_BC)


# Overall
# SystemicCrisesYES
# 11.4859776
# Population
# 5.3067425
# GDPconpPCPPPidUnits
# 4.4739275
# GDPconpPerch
# 3.4543523
# SP500
# 2.8133775
# GHouseIndex
# 1.6820161
# GDPdefIndex
# 0.9559160



## Therefore train model with only TOP 7 variables as identified above

colnames(Training)

Logmod4_BC <- glm(BankingCrises ~., family="binomial", data=Training[,c(20,15,6,7,18,19,17,11)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod4_BC)


# Calculate McFadden pseudo R2

MFRlogmod4BC <- pscl::pR2(Logmod4_BC)["McFadden"]
print(MFRlogmod4BC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_BC <- predict(Logmod4_BC, Testing[,c(20,15,6,7,18,19,17,11)])

Testing$BC_Logmod4Pred <- Pred_Logmod_BC

summary(Testing$BC_Logmod4Pred)
hist(Testing$BC_Logmod4Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$BC_Log4pred <- ifelse(Testing$BC_Logmod4Pred <= 0, "NO", "YES")
Testing$BC_Log4pred <- as.factor(Testing$BC_Log4pred)


# Create Confusion Matrix to asses model
Conf_BC <- table(Testing$BankingCrises, Testing$BC_Log4pred)
Conf_BC

# Logistic Model Diagnostics #

Accuracy_BC <- sum(diag(Conf_BC)/sum(Conf_BC))
Accuracy_BC


Model <- "LogReg4_BC"
Crises <- "Banking"
Preds_used <- "YES"
Feat_Sel <- "Yes 7"
Accuracy <- Accuracy_BC
Precision <- precision(Conf_BC)
Recall <- recall(Conf_BC)
Sensitivity <- sensitivity(Conf_BC)
Specificity <- specificity(Conf_BC)
F1 <- F_meas(Conf_BC)

LogReg4_BC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


##### SUMMARY DISGNOSTIC BANKING CRISES #############

LogRegALL_BC <- rbind(LogReg1_BC,LogReg2_BC,LogReg3_BC,LogReg4_BC)





###### CURRENCY CRISES  ######

## Use only Independent Variables no predictor variables

###### 1 #########

colnames(Training)

Logmod1_CC <- glm(CurrencyCrises ~., family="binomial", data=Training[,c(1:18,21)])

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod1_CC)


# Calculate McFadden pseudo R2

MFRlogmod1CC <- pscl::pR2(Logmod1_CC)["McFadden"]
print(MFRlogmod1CC)


# Use model on Testing set
colnames(Testing)

Pred_Logmod_CC <- predict(Logmod1_CC, Testing[,c(1:18,21)])

Testing$CC_Logmod1Pred <- Pred_Logmod_CC

summary(Testing$CC_Logmod1Pred)

hist(Testing$CC_Logmod1Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$CC_Log1pred <- ifelse(Testing$CC_Logmod1Pred <= 0, "NO", "YES")
Testing$CC_Log1pred <- as.factor(Testing$CC_Log1pred)




# Create Confusion Matrix to asses model
Conf_CC <- table(Testing$CurrencyCrises, Testing$CC_Log1pred)
Conf_CC

# Logistic Model Diagnostics #

Accuracy_CC <- sum(diag(Conf_CC)/sum(Conf_CC))
Accuracy_CC


Model <- "LogReg1_CC"
Crises <- "Currency"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_CC
Precision <- precision(Conf_CC)
Recall <- recall(Conf_CC)
Sensitivity <- sensitivity(Conf_CC)
Specificity <- specificity(Conf_CC)
F1 <- F_meas(Conf_CC)

LogReg1_CC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)

###### 2 #########
## Use Predictor Variables in Model
# training the model by assigning Currency Crisis column
# as target variable and other columns
# as independent variable

colnames(Training)

Logmod_CC <- glm(CurrencyCrises ~., family="binomial", data=Training)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod_CC)


# Calculate McFadden pseudo R2

MFRlogmod2CC <- pscl::pR2(Logmod_CC)["McFadden"]
print(MFRlogmod2CC)



# Use model on Testing set

Pred_Logmod_CC <- predict(Logmod_CC, Testing)

Testing$CC_Logmod2Pred <- Pred_Logmod_CC

summary(Testing$CC_Logmod2Pred)
hist(Testing$CC_Logmod2Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$CC_Log2pred <- ifelse(Testing$CC_Logmod2Pred <= 0, "NO", "YES")
Testing$CC_Log2pred <- as.factor(Testing$CC_Log2pred)




# Create Confusion Matrix to asses model
Conf_CC <- table(Testing$BankingCrises, Testing$CC_Log2pred)
Conf_CC

# Logistic Model Diagnostics #

Accuracy_CC <- sum(diag(Conf_CC)/sum(Conf_CC))
Accuracy_CC


Model <- "LogReg2_CC"
Crises <- "Currency"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_CC
Precision <- precision(Conf_CC)
Recall <- recall(Conf_CC)
Sensitivity <- sensitivity(Conf_CC)
Specificity <- specificity(Conf_CC)
F1 <- F_meas(Conf_CC)

LogReg2_CC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



###### 3 #########

## Use Predictor Variables in Model and choose only VARIABLES THAT ARE 95% OR MORE STASTICALLY SIGNIFICANT
# training the model by assigning Currency Crisis column
# as target variable and columns 95% OR MORE STASTICALLY SIGNIFICANT
# as independent variable

colnames(Training)

#view model summary
Selvar_CC <- summary(Logmod_CC)$coefficients
Selvar_CC


# Estimate
# Std. Error
# z value
# Pr(>|z|)
# InflationCrisesYES
# 2.31498675335868009384
# 0.30421130183835615179
# 7.6097986
# 0.00000000000002745233
# SP500
# 0.00197727590457558557
# 0.00048317004058551010
# 4.0922982
# 0.00004271188320142916
# Year
# -0.17094818875673886693
# 0.05171846989885240525
# -3.3053605
# 0.00094854301410003455
# (Intercept)
# 335.17709143354363732215
# 102.13136457991949157531
# 3.2818233
# 0.00103138216337173342
# GDPconpPerch
# -0.08858295085380769873
# 0.02904106387448988219
# -3.0502653
# 0.00228639264179760632

## Variable importance from caret package identifies the same as using p-values

VarimpLogmod_CC <- caret::varImp(Logmod_CC)

## 7 most Important Variables
# Overall
# InflationCrisesYES
# 7.6097986
# SP500
# 4.0922982
# Year
# 3.3053605
# GDPconpPerch
# 3.0502653
# GDPconpPCNCUnits
# 1.8968505
# INFcpi
# 1.7791202
# GHouseIndex
# 1.7351099


## Therefore train model with only 95% OR MORE STASTICALLY SIGNIFICANT as identified above

colnames(Training)

Logmod_CC <- glm(CurrencyCrises ~., family="binomial", data=Training[,c(22,18,1,7,21)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod_CC)


# Calculate McFadden pseudo R2

MFRlogmod3CC <- pscl::pR2(Logmod_CC)["McFadden"]
print(MFRlogmod3CC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_CC <- predict(Logmod_CC, Testing[,c(22,18,1,7,21)])

Testing$CC_Logmod3Pred <- Pred_Logmod_CC

summary(Testing$CC_Logmod3Pred)
hist(Testing$CC_Logmod3Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$CC_Log3pred <- ifelse(Testing$CC_Logmod3Pred <= 0, "NO", "YES")
Testing$CC_Log3pred <- as.factor(Testing$CC_Log3pred)




# Create Confusion Matrix to asses model
Conf_CC <- table(Testing$CurrencyCrises, Testing$CC_Log3pred)
Conf_CC

# Logistic Model Diagnostics #

Accuracy_CC <- sum(diag(Conf_CC)/sum(Conf_CC))
Accuracy_CC


Model <- "LogReg3_CC"
Crises <- "Currency"
Preds_used <- "YES"
Feat_Sel <- "Yes SS"
Accuracy <- Accuracy_CC
Precision <- precision(Conf_CC)
Recall <- recall(Conf_CC)
Sensitivity <- sensitivity(Conf_CC)
Specificity <- specificity(Conf_CC)
F1 <- F_meas(Conf_CC)

LogReg3_CC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


######## 4 #########


## Use Predictor Variables in Model and choose only TOP 7 VARIABLES
# training the model by assigning Banking Crisis column
# as target variable

Logmod_CC <- glm(CurrencyCrises ~., family="binomial", data=Training)


colnames(Training)

## Variable importance from caret package identifies TOP 7 variables

VarimpLogmod_CC <- caret::varImp(Logmod_CC)


# Overall
# InflationCrisesYES
# 7.6097986
# SP500
# 4.0922982
# Year
# 3.3053605
# GDPconpPerch
# 3.0502653
# GDPconpPCNCUnits
# 1.8968505
# INFcpi
# 1.7791202
# GHouseIndex
# 1.7351099


## Therefore train model with only TOP 7 variables as identified above

Logmod4_CC <- glm(CurrencyCrises ~., family="binomial", data=Training[,c(22,18,1,7,5,14,17,21)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod4_CC)


# Calculate McFadden pseudo R2

MFRlogmod4CC <- pscl::pR2(Logmod4_CC)["McFadden"]
print(MFRlogmod4CC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_CC <- predict(Logmod4_CC, Testing[,c(22,18,1,7,5,14,17,21)])

Testing$CC_Logmod4Pred <- Pred_Logmod_CC

summary(Testing$CC_Logmod4Pred)
hist(Testing$CC_Logmod4Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$CC_Log4pred <- ifelse(Testing$CC_Logmod4Pred <= 0, "NO", "YES")
Testing$CC_Log4pred <- as.factor(Testing$CC_Log4pred)


# Create Confusion Matrix to asses model
Conf_CC <- table(Testing$CurrencyCrises, Testing$CC_Log4pred)
Conf_CC

# Logistic Model Diagnostics #

Accuracy_CC <- sum(diag(Conf_CC)/sum(Conf_CC))
Accuracy_CC


Model <- "LogReg4_CC"
Crises <- "Currency"
Preds_used <- "YES"
Feat_Sel <- "Yes 7"
Accuracy <- Accuracy_CC
Precision <- precision(Conf_CC)
Recall <- recall(Conf_CC)
Sensitivity <- sensitivity(Conf_CC)
Specificity <- specificity(Conf_CC)
F1 <- F_meas(Conf_CC)

LogReg4_CC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


##### SUMMARY DISGNOSTIC CURRENCY CRISES #############

LogRegALL_CC <- rbind(LogReg1_CC,LogReg2_CC,LogReg3_CC,LogReg4_CC)



########### SYSTEMIC CRISES   ################

## Use only Independent Variables no predictor variables

###### 1 #########

colnames(Training)

Logmod1_SC <- glm(SystemicCrises ~., family="binomial", data=Training[,c(1:18,20)])

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod1_SC)


# Calculate McFadden pseudo R2

MFRlogmod1SC <- pscl::pR2(Logmod1_SC)["McFadden"]
print(MFRlogmod1SC)


# Use model on Testing set
colnames(Testing)

Pred_Logmod_SC <- predict(Logmod1_SC, Testing[,c(1:18,20)])

Testing$SC_Logmod1Pred <- Pred_Logmod_SC

summary(Testing$SC_Logmod1Pred)

hist(Testing$SC_Logmod1Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$SC_Log1pred <- ifelse(Testing$SC_Logmod1Pred <= 0, "NO", "YES")
Testing$SC_Log1pred <- as.factor(Testing$SC_Log1pred)




# Create Confusion Matrix to asses model
Conf_SC <- table(Testing$SystemicCrises, Testing$SC_Log1pred)
Conf_SC

# Logistic Model Diagnostics #

Accuracy_SC <- sum(diag(Conf_SC)/sum(Conf_SC))
Accuracy_SC


Model <- "LogReg1_SC"
Crises <- "Systemic"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_SC
Precision <- precision(Conf_SC)
Recall <- recall(Conf_SC)
Sensitivity <- sensitivity(Conf_SC)
Specificity <- specificity(Conf_SC)
F1 <- F_meas(Conf_SC)

LogReg1_SC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)

###### 2 #########
## Use Predictor Variables in Model
# training the model by assigning Systemic Crisis column
# as target variable and other columns
# as independent variable

colnames(Training)

Logmod_SC <- glm(SystemicCrises ~., family="binomial", data=Training)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod_SC)


# Calculate McFadden pseudo R2

MFRlogmod2SC <- pscl::pR2(Logmod_SC)["McFadden"]
print(MFRlogmod2SC)



# Use model on Testing set

Pred_Logmod_SC <- predict(Logmod_SC, Testing)

Testing$SC_Logmod2Pred <- Pred_Logmod_SC

summary(Testing$SC_Logmod2Pred)
hist(Testing$SC_Logmod2Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$SC_Log2pred <- ifelse(Testing$SC_Logmod2Pred <= 0, "NO", "YES")
Testing$SC_Log2pred <- as.factor(Testing$SC_Log2pred)




# Create Confusion Matrix to asses model
Conf_SC <- table(Testing$SystemicCrises, Testing$SC_Log2pred)
Conf_SC

# Logistic Model Diagnostics #

Accuracy_SC <- sum(diag(Conf_SC)/sum(Conf_SC))
Accuracy_SC


Model <- "LogReg2_SC"
Crises <- "Systemic"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_SC
Precision <- precision(Conf_SC)
Recall <- recall(Conf_SC)
Sensitivity <- sensitivity(Conf_SC)
Specificity <- specificity(Conf_SC)
F1 <- F_meas(Conf_SC)

LogReg2_SC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



###### 3 #########

## Use Predictor Variables in Model and choose only VARIABLES THAT ARE 95% OR MORE STASTICALLY SIGNIFICANT
# training the model by assigning Systemic Crisis column
# as target variable and columns 95% OR MORE STASTICALLY SIGNIFICANT
# as independent variable

colnames(Training)

#view model summary
Selvar_SC <- summary(Logmod_SC)$coefficients
Selvar_SC


# Estimate
# Std. Error
# z value
# Pr(>|z|)
# BankingCrisesYES
# 5.2618297609612181276
# 0.44370212024046551402
# 11.8589241
# 0.00000000000000000000000000000001934448
# InflationCrisesYES
# 2.3605579911548431049
# 0.59769155184528655056
# 3.9494585
# 0.00007832818289173310260901927915000442
# CurrAccUSDBil
# 0.0111662082916179715
# 0.00467793372056451995
# 2.3869958
# 0.01698668387581799346652822180203656899


## Variable importance from caret package identifies the same as using p-values

VarimpLogmod_SC <- caret::varImp(Logmod_SC)


# Overall
# BankingCrisesYES
# 11.8589241
# InflationCrisesYES
# 3.9494585
# CurrAccUSDBil
# 2.3869958
# GDPconpPerch
# 1.8014991
# INFcpi
# 1.7476330
# GDPconpPCPPPidUnits
# 1.6771645
# GDPcurpPPPidBil
# 1.4137836


## Therefore train model with only 95% OR MORE STASTICALLY SIGNIFICANT as identified above

colnames(Training)

Logmod_SC <- glm(SystemicCrises ~., family="binomial", data=Training[,c(22,19,3,20)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod_SC)


# Calculate McFadden pseudo R2

MFRlogmod3SC <- pscl::pR2(Logmod_SC)["McFadden"]
print(MFRlogmod3SC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_SC <- predict(Logmod_SC, Testing[,c(22,19,3,20)])

Testing$SC_Logmod3Pred <- Pred_Logmod_SC

summary(Testing$SC_Logmod3Pred)
hist(Testing$SC_Logmod3Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$SC_Log3pred <- ifelse(Testing$SC_Logmod3Pred <= 0, "NO", "YES")
Testing$SC_Log3pred <- as.factor(Testing$SC_Log3pred)




# Create Confusion Matrix to asses model
Conf_SC <- table(Testing$SystemicCrises, Testing$SC_Log3pred)
Conf_SC

# Logistic Model Diagnostics #

Accuracy_SC <- sum(diag(Conf_SC)/sum(Conf_SC))
Accuracy_SC


Model <- "LogReg3_SC"
Crises <- "Systemic"
Preds_used <- "YES"
Feat_Sel <- "Yes SS"
Accuracy <- Accuracy_SC
Precision <- precision(Conf_SC)
Recall <- recall(Conf_SC)
Sensitivity <- sensitivity(Conf_SC)
Specificity <- specificity(Conf_SC)
F1 <- F_meas(Conf_SC)

LogReg3_SC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


######## 4 #########


## Use Predictor Variables in Model and choose only TOP 7 VARIABLES
# training the model by assigning Banking Crisis column
# as target variable

Logmod_SC <- glm(SystemicCrises ~., family="binomial", data=Training)


colnames(Training)

## Variable importance from caret package identifies TOP 7 variables

VarimpLogmod_SC <- caret::varImp(Logmod_SC)


# BankingCrisesYES
# 11.8589241
# InflationCrisesYES
# 3.9494585
# CurrAccUSDBil
# 2.3869958
# GDPconpPerch
# 1.8014991
# INFcpi
# 1.7476330
# GDPconpPCPPPidUnits
# 1.6771645
# GDPcurpPPPidBil
# 1.4137836


## Therefore train model with only TOP 7 variables as identified above

Logmod_SC <- glm(SystemicCrises ~., family="binomial", data=Training[,c(19,22,3,7,14,6,10,20)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod_SC)


# Calculate McFadden pseudo R2

MFRlogmod4SC <- pscl::pR2(Logmod_SC)["McFadden"]
print(MFRlogmod4SC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_SC <- predict(Logmod_SC, Testing[,c(19,22,3,7,14,6,10,20)])

Testing$SC_Logmod4Pred <- Pred_Logmod_SC

summary(Testing$SC_Logmod4Pred)
hist(Testing$SC_Logmod4Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$SC_Log4pred <- ifelse(Testing$SC_Logmod4Pred <= 0, "NO", "YES")
Testing$SC_Log4pred <- as.factor(Testing$SC_Log4pred)


# Create Confusion Matrix to asses model
Conf_SC <- table(Testing$SystemicCrises, Testing$SC_Log4pred)
Conf_SC

# Logistic Model Diagnostics #

Accuracy_SC <- sum(diag(Conf_SC)/sum(Conf_SC))
Accuracy_SC


Model <- "LogReg4_SC"
Crises <- "Systemic"
Preds_used <- "YES"
Feat_Sel <- "Yes 7"
Accuracy <- Accuracy_SC
Precision <- precision(Conf_SC)
Recall <- recall(Conf_SC)
Sensitivity <- sensitivity(Conf_SC)
Specificity <- specificity(Conf_SC)
F1 <- F_meas(Conf_SC)

LogReg4_SC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


##### SUMMARY DISGNOSTIC SYSTEMIC CRISES #############

LogRegALL_SC <- rbind(LogReg1_SC,LogReg2_SC,LogReg3_SC,LogReg4_SC)





####### INFLATION CRISES   ####################

## Use only Independent Variables no predictor variables

###### 1 #########

colnames(Training)

Logmod_IC <- glm(InflationCrises ~., family="binomial", data=Training[,c(1:18,22)])

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod_IC)


# Calculate McFadden pseudo R2

MFRlogmod1IC <- pscl::pR2(Logmod_IC)["McFadden"]
print(MFRlogmod1IC)


# Use model on Testing set
colnames(Testing)

Pred_Logmod_IC <- predict(Logmod_IC, Testing[,c(1:18,22)])

Testing$IC_Logmod1Pred <- Pred_Logmod_IC

summary(Testing$IC_Logmod1Pred)

hist(Testing$IC_Logmod1Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$IC_Log1pred <- ifelse(Testing$IC_Logmod1Pred <= 0, "NO", "YES")
Testing$IC_Log1pred <- as.factor(Testing$IC_Log1pred)




# Create Confusion Matrix to asses model
Conf_IC <- table(Testing$InflationCrises, Testing$IC_Log1pred)
Conf_IC

# Logistic Model Diagnostics #

Accuracy_IC <- sum(diag(Conf_IC)/sum(Conf_IC))
Accuracy_IC


Model <- "LogReg1_IC"
Crises <- "Inflation"
Preds_used <- "NO"
Feat_Sel <- "NO"
Accuracy <- Accuracy_IC
Precision <- precision(Conf_IC)
Recall <- recall(Conf_IC)
Sensitivity <- sensitivity(Conf_IC)
Specificity <- specificity(Conf_IC)
F1 <- F_meas(Conf_IC)

LogReg1_IC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)

###### 2 #########
## Use Predictor Variables in Model
# training the model by assigning Inflation Crisis column
# as target variable and other columns
# as independent variable

colnames(Training)

Logmod_IC <- glm(InflationCrises ~., family="binomial", data=Training)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(Logmod_IC)


# Calculate McFadden pseudo R2

MFRlogmod2IC <- pscl::pR2(Logmod_IC)["McFadden"]
print(MFRlogmod2IC)



# Use model on Testing set

Pred_Logmod_IC <- predict(Logmod_IC, Testing)

Testing$IC_Logmod2Pred <- Pred_Logmod_IC

summary(Testing$IC_Logmod2Pred)
hist(Testing$IC_Logmod2Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$IC_Log2pred <- ifelse(Testing$IC_Logmod2Pred <= 0, "NO", "YES")
Testing$IC_Log2pred <- as.factor(Testing$IC_Log2pred)




# Create Confusion Matrix to asses model
Conf_IC <- table(Testing$InflationCrises, Testing$IC_Log2pred)
Conf_IC

# Logistic Model Diagnostics #

Accuracy_IC <- sum(diag(Conf_IC)/sum(Conf_IC))
Accuracy_IC


Model <- "LogReg2_IC"
Crises <- "Inflation"
Preds_used <- "YES"
Feat_Sel <- "NO"
Accuracy <- Accuracy_IC
Precision <- precision(Conf_IC)
Recall <- recall(Conf_IC)
Sensitivity <- sensitivity(Conf_IC)
Specificity <- specificity(Conf_IC)
F1 <- F_meas(Conf_IC)

LogReg2_IC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)



###### 3 #########

## Use Predictor Variables in Model and choose only VARIABLES THAT ARE 95% OR MORE STASTICALLY SIGNIFICANT
# training the model by assigning Inflation Crisis column
# as target variable and columns 95% OR MORE STASTICALLY SIGNIFICANT
# as independent variable

colnames(Training)

#view model summary
Selvar_IC <- summary(Logmod_IC)$coefficients
Selvar_IC


# Estimate
# Std. Error
# z value
# Pr(>|z|)
# CurrencyCrisesYES
# 2.395667994560110703
# 0.3347874960415955
# 7.15578695
# 0.0000000000008319427
# GDPconpPCPPPidUnits
# -0.000113032151695253
# 0.0000228921235823
# -4.93760010
# 0.0000007908981313261
# SystemicCrisesYES
# 1.840129359430134004
# 0.5658365386369639
# 3.25205114
# 0.0011457540574225935
# GDPdefIndex
# 0.002762918705159146
# 0.0008636088016224
# 3.19927113
# 0.0013777553205919231
# GDPcurpPCNCUnits
# -0.000001194415367842
# 0.0000004067756742
# -2.93629989
# 0.0033215304318705676
# GHouseIndex
# -0.097509228333955311
# 0.0342065999681547
# -2.85059691
# 0.0043637249736028054
# GDPcurpNCBil
# 0.000004829868780437
# 0.0000017966551368
# 2.68825590
# 0.0071826327854390432
# GDPconpPCNCUnits
# 0.000000189608021632
# 0.0000000731848025
# 2.59081142
# 0.0095749946010446584





## Variable importance from caret package identifies the same as using p-values

VarimpLogmod_IC <- caret::varImp(Logmod_IC)

# Overall
# CurrencyCrisesYES
# 7.15578695
# GDPconpPCPPPidUnits
# 4.93760010
# SystemicCrisesYES
# 3.25205114
# GDPdefIndex
# 3.19927113
# GDPcurpPCNCUnits
# 2.93629989
# GHouseIndex
# 2.85059691
# GDPcurpNCBil
# 2.68825590


## Therefore train model with only 95% OR MORE STASTICALLY SIGNIFICANT as identified above

colnames(Training)

Logmod3_IC <- glm(InflationCrises ~., family="binomial", data=Training[,c(21,6,20,11,9,17,8,5,22)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod3_IC)


# Calculate McFadden pseudo R2

MFRlogmod3IC <- pscl::pR2(Logmod3_IC)["McFadden"]
print(MFRlogmod3IC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_IC <- predict(Logmod3_IC, Testing[,c(21,6,20,11,9,17,8,5,22)])

Testing$IC_Logmod3Pred <- Pred_Logmod_IC

summary(Testing$IC_Logmod3Pred)
hist(Testing$IC_Logmod3Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$IC_Log3pred <- ifelse(Testing$IC_Logmod3Pred <= 0, "NO", "YES")
Testing$IC_Log3pred <- as.factor(Testing$IC_Log3pred)




# Create Confusion Matrix to asses model
Conf_IC <- table(Testing$InflationCrises, Testing$IC_Log3pred)
Conf_IC

# Logistic Model Diagnostics #

Accuracy_IC <- sum(diag(Conf_IC)/sum(Conf_IC))
Accuracy_IC


Model <- "LogReg3_IC"
Crises <- "Inflation"
Preds_used <- "YES"
Feat_Sel <- "Yes SS"
Accuracy <- Accuracy_IC
Precision <- precision(Conf_IC)
Recall <- recall(Conf_IC)
Sensitivity <- sensitivity(Conf_IC)
Specificity <- specificity(Conf_IC)
F1 <- F_meas(Conf_IC)

LogReg3_IC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


######## 4 #########


## Use Predictor Variables in Model and choose only TOP 7 VARIABLES
# training the model by assigning Inflation Crisis column
# as target variable

Logmod_IC <- glm(InflationCrises ~., family="binomial", data=Training)


colnames(Training)

## Variable importance from caret package identifies TOP 7 variables

VarimpLogmod_IC <- caret::varImp(Logmod_IC)

# Overall
# CurrencyCrisesYES
# 7.15578695
# GDPconpPCPPPidUnits
# 4.93760010
# SystemicCrisesYES
# 3.25205114
# GDPdefIndex
# 3.19927113
# GDPcurpPCNCUnits
# 2.93629989
# GHouseIndex
# 2.85059691
# GDPcurpNCBil
# 2.68825590



## Therefore train model with only TOP 7 variables as identified above

Logmod_IC <- glm(InflationCrises ~., family="binomial", data=Training[,c(21,6,20,11,9,17,8,22)])

# disable scientific notation for model summary
options(scipen=999)

summary(Logmod_IC)


# Calculate McFadden pseudo R2

MFRlogmod4IC <- pscl::pR2(Logmod_IC)["McFadden"]
print(MFRlogmod4IC)



# Use model on Testing set

colnames(Testing)

Pred_Logmod_IC <- predict(Logmod_IC, Testing[,c(21,6,20,11,9,17,8,22)])

Testing$IC_Logmod4Pred <- Pred_Logmod_IC

summary(Testing$IC_Logmod4Pred)
hist(Testing$IC_Logmod4Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

Testing$IC_Log4pred <- ifelse(Testing$IC_Logmod4Pred <= 0, "NO", "YES")
Testing$IC_Log4pred <- as.factor(Testing$IC_Log4pred)


# Create Confusion Matrix to asses model
Conf_IC <- table(Testing$InflationCrises, Testing$IC_Log4pred)
Conf_IC

# Logistic Model Diagnostics #

Accuracy_IC <- sum(diag(Conf_IC)/sum(Conf_IC))
Accuracy_IC


Model <- "LogReg4_IC"
Crises <- "Inflation"
Preds_used <- "YES"
Feat_Sel <- "Yes 7"
Accuracy <- Accuracy_IC
Precision <- precision(Conf_IC)
Recall <- recall(Conf_IC)
Sensitivity <- sensitivity(Conf_IC)
Specificity <- specificity(Conf_IC)
F1 <- F_meas(Conf_IC)

LogReg4_IC <- data.frame(Crises, Preds_used, Feat_Sel, Accuracy, Precision, Recall, Sensitivity,Specificity, F1, Model)


##### SUMMARY DISGNOSTIC INFLATION CRISES #############

LogRegALL_IC <- rbind(LogReg1_IC,LogReg2_IC,LogReg3_IC,LogReg4_IC)
LogRegALL  <- rbind(LogRegALL_BC,LogRegALL_CC,LogRegALL_SC,LogRegALL_IC)

################# SUMMARY ALL DISGNOSTICS   ##########################

diagALL_RFLR <- rbind(Diag_ALLRF,LogRegALL)

### End Modelling####
################################################################################


####Export results to EXCEL#####

install.packages('writexl')
library(writexl)

write_xlsx(diagALL_RFLR, 'C:\\Users\\premr\\Documents\\Data Analytics\\CIND 820\\R Data\\Predictfc\\GDD\\diagall.xlsx')




#### Extract Actual Data 2015 to 2026 run Model for G7 Countries

# Import IMF World Economic Outlook Database For G7 Countries 2015 to 2026


library(readxl)
Actual_WEO_Data_2015_2026 <- read_excel("Actual WEO Data 2015 2026.xlsx", 
                                        sheet = "Actual WEO Data 2015 2026  R", 
                                        col_types = c("text", "text", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric"))
View(Actual_WEO_Data_2015_2026)
WEO_to2026 <- Actual_WEO_Data_2015_2026

# Review Data and structure of Data
summary(WEO_to2026)
str(WEO_to2026)

## DATA CLEANING (IMF World Economic Outlook Database)

# Move Year columns to rows to align with Global Crisis Data set

library(reshape2)
WEO2026 <-  melt(WEO_to2026, id.vars=c("Country", "Variable"))
WEData2026 <- dcast(WEO2026, Country + variable~Variable, value.var = "value")

# Rename the "Variable" Column to "Year"
names(WEData2026)[2] <- "Year"

# Convert "Year" Column to numeric
WEData2026$Year <- as.character(WEData2026$Year)
WEData2026$Year <- as.numeric(WEData2026$Year)


# Review Data and structure of Data
summary(WEData2026)
str(WEData2026)


# Import S & P 500 Annual data to Jan 1, 2021

library(readxl)
SP500 <- read_excel("~/Data Analytics/CIND 820/WMF/SP500 Annual data.xlsx", 
                    col_types = c("date", "numeric"))
View(SP500)

# Change date to year only

SP500$year <- format(as.Date(SP500$Date, format="%d/%m/%Y"),"%Y")
SP500$Date <- NULL
colnames(SP500) <- c("SP500", "Year")

# Impute Data to 2026
# Rows 2022 to 2026

SP500new_rows <- data.frame(SP500=c(NA,NA,NA,NA,NA), Year=c(2026,2025,2024,2023,2022))

SP500_2026 <- rbind(SP500new_rows, SP500)
str(SP500_2026)


# Convert year to number for imputation
SP500_2026$Year <- as.numeric(SP500_2026$Year)

library(mice)
methods(mice)

# Use Mice "quadratic" method to impute missing values
SP2026.imp <- mice(SP500_2026, m=50, method = 'quadratic', seed = 101)
SP500_2026_imp <- complete(SP2026.imp)


#  Global Housing Index Data is only avaiable to 2020, use mice to impute to 2026

GH_NAdatanew <- data.frame(GHouseIndex = c(NA), Year = c(2026:2021))

#Add to existing Housing Index Data

GHPI_annnew<- rbind(GH_NAdatanew,Imp_GHPI)



# Use Mice "norm predict" method to impute missing values
GHPI_annnew.imp <- mice(GHPI_annnew, m=50, method = 'norm.predict', seed = 101)
Imp_GHPI_2026 <- complete(GHPI_annnew.imp)

# Add Housing and SP500 data to actual and project WEO data to have all column to 2026

WEData2026_HP <- merge(WEData2026, Imp_GHPI_2026, by= c ("Year"))

WEData2026_all <- merge(WEData2026_HP, SP500_2026_imp, by= c ("Year"))



#################  Run best models on actual data   ############################

### Select best model for each crises based on Specificity (% of Crises correctly predicted), F1, and Accuracy

#### However, to run on actual data we cannot use models that use predictor variables as those will not be available
### therefore, use best model without predictors 
###Best Models: 
# Crises	Preds_used	Feat_Sel	Accuracy	Precision	   Recall	    Sensitivity	Specificity	F1	         Model
# Banking	YES	        Yes 7	 0.910941476	0.981012658	0.914454277	0.914454277	0.888888889	0.946564885	LogReg4_BC = Logmod4_BC
# Banking	NO	        NO	   0.901709402	0.905339806	0.981578947	0.981578947	0.556818182	0.941919192	DiagRF1_BC = RFM


# Crises	 Preds_used	Feat_Sel	Accuracy	  Precision	 Recall	     Sensitivity	Specificity	F1	        Model
# Currency	YES	      Yes 7	    0.875318066	0.973214286	0.891008174	0.891008174	0.653846154	0.93029872	LogReg4_CC = Logmod4_CC
# Currency	NO	      NO	      0.857506361	0.991071429	0.862694301	0.862694301	0.571428571	0.922437673	LogReg1_CC = Logmod1_CC


# Crises	  Preds_used	Feat_Sel	Accuracy	  Precision	  Recall	    Sensitivity	Specificity	F1	        Model
# Inflation	YES	        Yes SS	  0.93129771	0.977900552	0.949061662	0.949061662	0.6	        0.963265306	LogReg3_IC = Logmod3_IC
# Inflation	NO	        NO	      0.956743	  0.9649596	  0.9889503	  0.9889503	  0.5806452	  0.9768076	  DiagRF1_IC = RFM1_IC


# Crises	  Preds_used	Feat_Sel	 Accuracy	   Precision	  Recall	    Sensitivity	Specificity	F1	        Model
#Systemic	  YES	        NO	       0.96692112	 0.971264368	0.991202346	0.991202346	0.807692308	0.981132075	DiagRF2_SC = RFMSC
#Systemic	  NO	        NO	       0.870229008 0.994134897	0.87371134	0.87371134	0.6	        0.930041152	LogReg1_SC = Logmod1_SC


###Run BEST BANKING CRISES model WITH NO PREDICTORS INCLUDED IN MODEL on actual data

set.seed(5678)
ABankCrisis_Pred <- predict(RFM,WEData2026_all)
WEData2026_all$BankCrisis_Pred <- ABankCrisis_Pred


###Run BEST CURRENCY CRISES model WITH NO PREDICTORS INCLUDED IN MODEL on actual data

set.seed(5678)
ACurrencyCrisis_Pred <- predict(Logmod1_CC,WEData2026_all)
WEData2026_all$CurrencyCrisis_Pred <- ACurrencyCrisis_Pred

summary(WEData2026_all$CurrencyCrisis_Pred)

hist(WEData2026_all$CurrencyCrisis_Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

WEData2026_all$CurrencyCrisis_Pred <- ifelse(WEData2026_all$CurrencyCrisis_Pred <= 0, "NO", "YES")
WEData2026_all$CurrencyCrisis_Pred <- as.factor(WEData2026_all$CurrencyCrisis_Pred)

###Run BEST INFLATION CRISES model WITH NO PREDICTORS INCLUDED IN MODEL on actual data

set.seed(5678)
AInflationCrisis_Pred <- predict(RFM1_IC,WEData2026_all)
WEData2026_all$InflationCrisis_Pred <- AInflationCrisis_Pred


###Run BEST SYSTEMIC CRISES model WITH NO PREDICTORS INCLUDED IN MODEL on actual data

set.seed(5678)
ASystemicCrisis_Pred <- predict(Logmod1_SC,WEData2026_all)
WEData2026_all$SystemicCrisis_Pred <- ASystemicCrisis_Pred

summary(WEData2026_all$SystemicCrisis_Pred)

hist(WEData2026_all$SystemicCrisis_Pred)
# use use Zero as the cutoff, less than or equal to Zero then "NO", else "YES"

WEData2026_all$SystemicCrisis_Pred <- ifelse(WEData2026_all$SystemicCrisis_Pred <= 0, "NO", "YES")
WEData2026_all$SystemicCrisis_Pred <- as.factor(WEData2026_all$SystemicCrisis_Pred)

WEData2026_summ <- WEData2026_all[,c(1,2,20,21,22,23)]

str(WEData2026_all)
summary(WEData2026_all)


write_xlsx(WEData2026_all, 'C:\\Users\\premr\\Documents\\Data Analytics\\CIND 820\\R Data\\Predictfc\\GDD\\WEData2026all.xlsx')

########################################################################
library(dplyr)
library(ggplot2)

filter(WEData2026_summ, Year%in%c(2021:2026)) %>%
  ggplot(aes(Country,CurrencyCrisis_Pred, col = Country)) +
  geom_point(size = 2) +
  facet_wrap(. ~ Year, scales = "free")

filter(WEData2026_summ, Year%in%c(2025:2026)) %>%
  ggplot(aes(Country,SystemicCrisis_Pred, col = Country)) +
  geom_point(size = 2) +
  facet_wrap(. ~ Year, scales = "free")

##########################################END######################################################
