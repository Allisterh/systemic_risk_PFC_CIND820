###  SVM MODELLING ###

library(e1071)

model_reg = svm(medv~., data=train)
print(model_reg)

SVMmod_BC <- svm(BankingCrises ~., data=Training)
print(SVMmod_BC)

SVMpred_BC = predict(SVMmod_BC, Testing)
SVMpred_BC
summary(SVMpred_BC)

Testing$BankCrisis_SVMPred <- SVMpred_BC


# Create Confusion Matrix to asses model
Conf_BC <- table(Testing$BankingCrises, Testing$BankCrisis_SVMPred)
Conf_BC

Accuracy_BC <- sum(diag(Conf_BC)/sum(Conf_BC))
Accuracy_BC
library(caret)
Crises <- " SVM Banking"
Accuracy <- Accuracy_BC
Precision <- precision(Conf_BC)
Recall <- recall(Conf_BC)
Sensitivity <- sensitivity(Conf_BC)
Specificity <- specificity(Conf_BC)
F1 <- F_meas(Conf_BC)

SVM_Diag_BC <- data.frame(Crises, Accuracy, Precision, Recall, Sensitivity,Specificity, F1)

Overall_Diag <- rbind(DiagRF, SVM_Diag_BC)
