rm(list=ls())
setwd("C:/Users/abc/Downloads/DataSets/Loan_prediction")
train_data2 <- read.csv('train_u6lujuX_CVtuZ9i.csv',  na.strings = c("",",","."))
test_data <- read.csv('test_Y3wMUE5_7gLdaTN.csv',  na.strings = c("",",","."))
train_data2 <- train_data
test_data2 <- test_data
# ----------------------------Data Visualization/Univariate analysis---------------------
head(train_data)
str(train_data)
train_cont <- subset(train_data, select = c(LoanAmount, Loan_Amount_Term, Credit_History, ApplicantIncome, CoapplicantIncome))
train_categ <- subset(train_data, select = -c(LoanAmount, Loan_Amount_Term, Credit_History, ApplicantIncome, CoapplicantIncome))
summary(train_cont)
View(as.data.frame(train_cont$Credit_History))
# Converting credit history as factors
train_data$Credit_History <- as.factor(train_data$Credit_History)
test_data$Credit_History <- as.factor(test_data$Credit_History)
train_cont$Credit_History <- as.factor(train_cont$Credit_History)
train_categ$Credit_History <- train_cont$Credit_History
train_cont$Credit_History <- NULL
hist(train_cont$LoanAmount) # there lies few outliers
hist(train_cont$ApplicantIncome)# few outliers
unique(train_cont$Loan_Amount_Term)

as.matrix(prop.table(table(train_categ$Gender)))
as.matrix(prop.table(table(train_categ$Married)))
as.matrix(prop.table(table(train_categ$Dependents)))
sum(is.na(train_categ$Dependents))/614 #Only 2%na's
as.matrix(prop.table(table(train_categ$Education)))
as.matrix(prop.table(table(train_categ$Self_Employed)))
as.matrix(prop.table(table(train_categ$Property_Area)))
unique(train_categ$Dependents)
str(train_categ$CoapplicantIncome)
as.matrix(prop.table(table(train_categ$Loan_Status)))
as.matrix(prop.table(table(train_categ$Credit_History)))

# ---------------------BiVariate Analysis-----------------------------
cor(train_cont$LoanAmount, train_cont$Loan_Amount_Term, method = "spearman") #NA's are there so couldn't find
cor(train_cont$ApplicantIncome, train_cont$CoapplicantIncome, method = "spearman") # not a very significant relation exist
chisq.test(train_data$Gender, train_data$Loan_Status) #not a significant relation exist
chisq.test(train_data$Married, train_data$Loan_Status) # Relation is significant
chisq.test(train_data$Loan_Status, train_data$Dependents) #Not very significant relation but can be considere
chisq.test(train_data$Loan_Status, train_data$Education) # Significant relation
chisq.test(train_data$Loan_Status, train_data$Self_Employed) #Not at all significant
train_data2$Self_Employed <- NULL
test_data2$Self_Employed <- NULL
train_data2$Loan_ID <- NULL
test_data2$Loan_ID <- NULL
chisq.test(train_data$Loan_Status, train_data$Credit_History) #Significant very much
chisq.test(train_data$Loan_Status, train_data$Property_Area) #yes significant
summary(aov(ApplicantIncome~Loan_Status, data = train_data)) #Not a significant relation
summary(aov(CoapplicantIncome~Loan_Status, data = train_data)) #Worth Consideration
summary(aov(LoanAmount~Loan_Status, data = train_data))
summary(aov(Loan_Amount_Term~Loan_Status, data = train_data))
unique(train_data$Loan_Amount_Term)
train_data$Loan_Amount_Term <- as.factor(train_data$Loan_Amount_Term) 
test_data$Loan_Amount_Term <- as.factor(test_data$Loan_Amount_Term)
train_data2$Loan_Amount_Term <- as.factor(train_data2$Loan_Amount_Term)
test_data2$Loan_Amount_Term <- as.factor(test_data2$Loan_Amount_Term)
chisq.test(train_data$Loan_Status, train_data$Loan_Amount_Term) #Yes, relation exist
train_data2$Gender <- NULL
test_data2$Gender <- NULL

#--------------Missing value treatment-----------------------------

colSums(is.na(train_data))
install.packages("DMwR")
library(DMwR)
test_data2 <- knnImputation(test_data2, k=10)
train_data2 <- knnImputation(train_data2, k=10)
sum(is.na(train_data2))
#-------------Outlier treatment---------------------
me_value <- mean(train_data2$ApplicantIncome)
train_data2$ApplicantIncome[train_data2$ApplicantIncome>20000] <- me_value
men_value <- mean(test_data2$ApplicantIncome)
hist(test_data2$ApplicantIncome)
test_data2$ApplicantIncome[test_data2$ApplicantIncome>20000] <- men_value
hist(train_data2$CoapplicantIncome)
me_value <- mean(train_data2$CoapplicantIncome)
train_data2$CoapplicantIncome[train_data2$CoapplicantIncome>10000] <- me_value
hist(test_data2$CoapplicantIncome)
me_value <- mean(test_data2$CoapplicantIncome)
test_data2$CoapplicantIncome[test_data2$CoapplicantIncome>5000] <- me_value
hist(train_data2$LoanAmount)
me_value <- mean(train_data2$LoanAmount)
train_data2$LoanAmount[train_data2$LoanAmount>350] <- me_value
hist(test_data2$LoanAmount)
me_value <- mean(test_data2$LoanAmount)
test_data2$LoanAmount[test_data2$LoanAmount>300] <- me_value
test_data2$Loan_Amount_Term <- as.integer(test_data2$Loan_Amount_Term)
unique(test_data2$Loan_Amount_Term)
test_data2$Loan_Amount_Term[test_data2$Loan_Amount_Term==350] <- 360
train_data2$Loan_Amount_Term <- NULL
test_data2$Loan_Amount_Term <- NULL
#--------------------Prediction----------------------------
install.packages("rpart")
library(rpart)
model <- rpart(Loan_Status~., data = train_data2, method = "class")
predict_test <- predict(model, newdata = test_data2, type = "class")
model2 <-rpart(Loan_Status~., data = train_data2, method =  "class", control = rpart.control(minsplit = 20, minbucket = 100, maxdepth = 10), xval = 5)
predict_test2 <- predict(model2, newdata = test_data2, type = "class")
solution_frame <- data.frame(Loan_ID=test_data$Loan_ID, Loan_Status= predict_test2)
write.csv(solution_frame, file="final_soln2.csv")
