#merge Global Crises data set with Global Debt database

MergedD <- merge(GDB, Gcrisis, by= c("Country", by= "Year"))
str(MergedD)
summary(MergedD)
# Data frame with columns to be used
Model1 <- MergedD[, c(-3,-4,-5,-12,-13,-17,-18)]

# Move dependent variables to end columns
Model1$BC <- Model1$`Banking Crisis`
Model1$SC <- Model1$`Systemic Crisis`
Model1a <- Model1[, c(-12,-13)]

# Impute missing values
library(mice)
library(VIM)
# Simplify model column names
names(Model1a) <- c("Cty", "Yr", "TPDall", "TPDlds", "HDall", "HDlds", "NFCDall", "NFCDlds", "Ggd", "Cgd", "GDPbils", "DDDef", "GDPwDef", "In", "CC", "IC", "BC","SC")

# Assess missing data
aggr_plot <- aggr(Model1a, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(Model1a), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Model1a,2,pMiss)
apply(Model1a,1,pMiss)
md.pattern(Model1a)

# Use Mice "cart"(Classification and Regression Trees) method to impute missing values
Model1a.imp <- mice(Model1a, m=5, method = 'cart', seed = 101)

Imp.Model <- complete(Model1a.imp)
# Replace lower case "n/a" with Zero *****
Imp.Model$DDDef[Imp.Model$DDDef == "n/a"] <- 0


library(caret)
library(doSNOW)
library(caTools)




# CURRENCY CRISIS PREDICTION
# Remove Country and other crisis predictors
Imp.Model.cc <- Imp.Model[c(-1,-16,-17,-18)]
Imp.Model.cc$CC[Imp.Model.cc$CC == "2"] <- 1
Imp.Model.cc$CC <- as.factor(Imp.Model.cc$CC)
Imp.Model.cc$DDDef <- as.numeric(Imp.Model.cc$DDDef)

# Set response variable to Yes/No
levels(Imp.Model.cc$CC) <- c("No", "Yes")

# Create train and test Data sets
ind <- createDataPartition(Imp.Model.cc$CC, p = .80, list = FALSE)
train.cc <- Imp.Model.cc[ind,]
test.cc <- Imp.Model.cc[-ind,]

# setting seed to generate a
# reproducible random sampling
set.seed(12345)
parameterGrid <- expand.grid(mtry = c(2,3,4))
# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10,savePredictions = TRUE, classProbs = TRUE)

# training the model by assigning Currency Crisis(cc) column
# as target variable and rest other column
# as independent variable

model.cc <- train(CC ~., data = train.cc, method = "rf",
               trControl = train_control, tuneGrid = parameterGrid)

print(model.cc)


# Run model on test set
predictions.model.cc <- predict(model.cc, test.cc)
# Create Confusion matrix
tpredictions.cc <- table(predictions.model.cc, actual = test.cc$CC)
tpredictions.cc




# INFLATION CRISIS PREDICTION
# Remove Country and other crisis predictors
Imp.Model.ic <- Imp.Model[c(-1,-15,-17,-18)]
table(Imp.Model.ic$IC)

#Convert 3 n/a and 5 NA's to 0
Imp.Model.ic$IC[Imp.Model.ic$IC == "n/a"] <- 0
Imp.Model.ic$IC[is.na(Imp.Model.ic$IC)] <- 0
Imp.Model.ic$IC <- as.factor(Imp.Model.ic$IC)
Imp.Model.ic$DDDef <- as.numeric(Imp.Model.ic$DDDef)

# Set response variable to Yes/No
levels(Imp.Model.ic$IC) <- c("No", "Yes")
str(Imp.Model.ic)
# Create train and test Data sets
ind <- createDataPartition(Imp.Model.ic$IC, p = .80, list = FALSE)
train.ic <- Imp.Model.ic[ind,]
test.ic <- Imp.Model.ic[-ind,]
table(train.ic$IC)
summary(train.ic$IC)
# setting seed to generate a
# reproducible random sampling
set.seed(12345)
parameterGrid <- expand.grid(mtry = c(2,3,4))
# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10,savePredictions = TRUE, classProbs = TRUE)

# training the model by assigning Currency Crisis(cc) column
# as target variable and rest other column
# as independent variable

model.ic <- train(IC ~., data = train.ic, method = "rf",
                  trControl = train_control, tuneGrid = parameterGrid)

print(model.ic)


# Run model on test set
predictions.model.ic <- predict(model.ic, test.ic)
# Create Confusion matrix
tpredictions.ic <- table(predictions.model.ic, actual = test.ic$IC)
tpredictions.ic


