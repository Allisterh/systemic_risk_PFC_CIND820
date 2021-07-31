# Import and clean each data set individually
# Two data sets are used
# 1) IMF World Economic Outlook Database by Country
# 2) Global Crises Data from Harvard Business School




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

# Import Global Crises Data from Harvard Business School (excel)

library(readxl)
global_crisis_data <- read_excel("~/Data Analytics/CIND 820/WMF/global_crisis_data.xlsx", 
                                 sheet = "revised for R", col_types = c("text", 
                                                                        "text", "text", "text", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "text", "text", "text", "numeric", 
                                                                        "numeric", "text", "text", "text", 
                                                                        "text", "text"))
View(global_crisis_data)

## DATA CLEANING (Global Crises Data)

# Extract on data from 1980 to 2016 from global crises data set to match available data in IMF debt database

Gcrisisdata <- subset(global_crisis_data,global_crisis_data$Year >= 1980)

# Review Data and structure of Data
summary(Gcrisisdata)
str(Gcrisisdata)
# Shorten column names
names(Gcrisisdata) <- c("Case#", "CC3", "Ctry", "Yr", "USDxch","USDxch1","USDxch2","USDxch3", "DDDef", "SED1", "SED2","GDPWDef", "INf", "Indep", "CC", "IC","BC","SC")
summary(Gcrisisdata)
str(Gcrisisdata)
# Convert "DDDef", "SED1", "SED2" to numerical to pick up NA's
Gcrisisdata$DDDef <- as.numeric(Gcrisisdata$DDDef)
Gcrisisdata$SED1 <- as.numeric(Gcrisisdata$SED1)
Gcrisisdata$SED2 <- as.numeric(Gcrisisdata$SED2)
summary(Gcrisisdata)
str(Gcrisisdata)
# Review Missing Values in Crises data set
# Using method from Jens Laufer, https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
library(tidyverse)
library(dplyr)
library(tidyr)
missing.valuesCrises <- Gcrisisdata %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

view(missing.valuesCrises)


missing.valuesCrises %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Assess missing data
library(rio)
library(DEoptimR)
library(VIM)
library(mice)
library(VIM)
install_formats("fst")   # Includes‘hexView’, ‘pzfx’, ‘readODS’, ‘rmatio’)
aggr_plot <- aggr(Gcrisisdata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Gcrisisdata), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Gcrisisdata,2,pMiss)
apply(Gcrisisdata,1,pMiss)
MissGC <- md.pattern(Gcrisisdata)
MissGC

# Remove columns with greater than 5% (130) of missing values:"USDxch1","USDxch2","USDxch3", "DDDef" and "GDPWDef"

table(Gcrisisdata$Indep)
# Also remove "Indep" as it the same for all rows
colnames(Gcrisisdata)
Gcrisisdata <- Gcrisisdata[,c(-6,-7,-8,-9,-12,-14)]
str(Gcrisisdata)

summary(Gcrisisdata)
str(Gcrisisdata)

# Impute missing values in Crises data set

# Reduce columns by Case# and CC3 to support imputation
Gcrisisdatals <- Gcrisisdata[,c(-1,-2)]
summary(Gcrisisdatals)
# Use Mice "cart"(Classification and Regression Trees) method to impute missing values
Gcrisisdata.imp <- mice(Gcrisisdatals, m=5, method = 'cart', seed = 101)
Imp_Crises <- complete(Gcrisisdata.imp)
str(Imp_Crises)
summary(Imp_Crises)
# Review changes made
summary(Gcrisisdata.imp)
Gcrisisdata.imp$imp$USDxch
Gcrisisdata.imp$imp$SED1
Gcrisisdata.imp$imp$SED2
Gcrisisdata.imp$imp$INf
Imp_Crises[c(259,2588,111,1974, 2589, 1050,2186),]

# Examine file
summary(Imp_Crises)
str(Imp_Crises)
pairs(Imp_Crises[,3:6])
#There is no apparent correlation for these variables "USDxch", "SED1", "SED2", and "INf".
plot(Imp_Crises$Yr,Imp_Crises$USDxch)

# Review Predictor variable for NA's

table(Imp_Crises$CC)
table(Imp_Crises$IC)
table(Imp_Crises$BC)
table(Imp_Crises$SC)

# For "CC" replace "2" with "1" for binary predictor similar to other predictors.
# For other predictors replace lower case "n/a" with "0" to simplify (0= not a crises)
# Imp_Crises$CC <- as.numeric(Imp_Crises$CC)
Imp_Crises$CC[Imp_Crises$CC == "2"] <- 1
Imp_Crises$IC[Imp_Crises$IC == "n/a"] <- 0
Imp_Crises$BC[Imp_Crises$BC == "n/a"] <- 0
Imp_Crises$SC[Imp_Crises$SC == "n/a"] <- 0
# Convert 0 to "NO" and 1 to "YES' for predictor variables

Imp_Crises$CC[Imp_Crises$CC == "0"] <- "NO"
Imp_Crises$CC[Imp_Crises$CC == "1"] <- "YES"

Imp_Crises$IC[Imp_Crises$IC == "0"] <- "NO"
Imp_Crises$IC[Imp_Crises$IC == "1"] <- "YES"

Imp_Crises$BC[Imp_Crises$BC == "0"] <- "NO"
Imp_Crises$BC[Imp_Crises$BC == "1"] <- "YES"

Imp_Crises$SC[Imp_Crises$SC == "0"] <- "NO"
Imp_Crises$SC[Imp_Crises$SC == "1"] <- "YES"

table(Imp_Crises$CC)
table(Imp_Crises$IC)
table(Imp_Crises$BC)
table(Imp_Crises$SC)

table(Imp_Crises$USDxch)

## DATA CLEANING (IMF World Economic Outlook Database)

# Move Year columns to rows to align with Global Crisis Data set

library(reshape2)
WEO1 <-  melt(WEO_Data_large, id.vars=c("Country", "Measure"))
WEData <- dcast(WEO1, Country + variable~Measure, value.var = "value")

# Review Data and structure of Data
summary(WEData)
str(WEData)

# Review Missing Values in Crises data set
# Using method from Jens Laufer, https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
library(tidyverse)
library(dplyr)
library(tidyr)
missing.valuesWEData <- WEData %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

view(missing.valuesWEData)


missing.valuesWEData %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing, fill = key), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = "none") + labs(x = "Measure")

# Assess missing data
library(rio)
library(DEoptimR)
library(VIM)
install_formats("fst")   # Includes‘hexView’, ‘pzfx’, ‘readODS’, ‘rmatio’)
aggr_plot <- aggr(WEData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(WEData), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(WEData,2,pMiss)
apply(WEData,1,pMiss)
md.pattern(WEData)








# Remove columns with greater than 5% (130) of missing values:"USDxch1","USDxch2","USDxch3", "DDDef" and "GDPWDef"

table(Gcrisisdata$Indep)
# Also remove "Indep" as it the same for all rows
colnames(Gcrisisdata)
Gcrisisdata <- Gcrisisdata[,c(-6,-7,-8,-9,-12,-14)]
str(Gcrisisdata)

summary(Gcrisisdata)
str(Gcrisisdata)


## DATA VISUALIZATION

# create histograms for each attribute
plot(Imp_Crises$USDxch)
plot(Imp_Crises$INf,breaks = 100, col = "orange", main = "Histogram of INf", xlab = "Percent")

summary(Imp_Crises)

ggplot(Imp_Crises, aes(Yr,USDxch)) + geom_point() 
ggplot(Imp_Crises, aes(Yr,Imp_Crises$INf)) + geom_point()

ggplot(Imp_Crises, aes(Yr,USDxch)) + geom_boxplot() 
ggplot(Imp_Crises, aes(Yr,Imp_Crises$INf)) + geom_boxplot()




# Determine if any Multicollinearity issues exist if  = r > .90

# Review revised correlation and p-values

# Pearson Correlation
corrpearImp <- rcorr(as.matrix(Imp_PFC), type = c("pearson"))
corrpearImp

# Spearman Correlation
corrspearImp <- rcorr(as.matrix(Imp_PFC), type = c("spearman"))
corrspearImp


# Extract the correlation coefficients
corrpearImp$r
# Extract p-values
corrpearImp$P


# Get matrix of revised correlation coefficients and p-values
# PEARSON
corrpeardata <- flattenCorrMatrix(corrpearImp$r, corrpearImp$P)
view(corrpeardata)

# SPEARMAN
corrspeardata <- flattenCorrMatrix(corrspearImp$r, corrspearImp$P)
view(corrspeardata)
str(corrpeardata)
corrpeardata$cor <- round(corrpeardata$cor,3)
table(corrpeardata$row, corrpeardata$column)

Imp_nocoLin <- Imp_PFC[corrpeardata$cor<= .90 & corrpeardata$cor >= -.90]

