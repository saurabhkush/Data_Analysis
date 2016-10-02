setwd("C:/Users/abc/Downloads/DataSets/House_Pricing")
train_data <- read.csv("train.csv", stringsAsFactors=FALSE)
test_data <- read.csv("test.csv", stringsAsFactors=FALSE)

#------------------------Data Exploration and preparation------------------------

str(train_data)

length(unique(train_data$MSSubClass)) # only 15 different values for 1400 obs hence can be traansformed as factor
train_data$MSSubClass <-as.factor(train_data$MSSubClass)
test_data$MSSubClass <- as.factor(test_data$MSSubClass)

install.packages("dplyr")
library(dplyr)

length(unique(train_data$LotFrontage)) # sufficient to be of int type

length(unique(train_data$LotArea)) # Int values

unique(train_data$Street) # 2 levels
train_data$Street<- as.factor(train_data$Street)
test_data$Street <- as.factor(test_data$Street)

unique(train_data$Alley) # 2 levels but NA here has a significance so will change from NA to NoAc
train_data$Alley[is.na(train_data$Alley)] <-'NoAC' 
test_data$Alley[is.na(test_data$Alley)] <- 'NoAC'
train_data$Alley<- as.factor(train_data$Alley)
test_data$Alley <- as.factor(test_data$Alley)

unique(train_data$LotShape) # 4 levels
train_data$LotShape <- as.factor(train_data$LotShape)
test_data$LotShape <- as.factor(test_data$LotShape)

unique(train_data$LandContour) # 4 levels
train_data$LandContour <- as.factor(train_data$LandContour)
test_data$LandContour <- as.factor(test_data$LandContour)

unique(train_data$Utilities) # 3 levels
train_data$Utilities <- as.factor(train_data$Utilities)
test_data$Utilities <- as.factor(test_data$Utilities)

unique(train_data$LotConfig) # 5 levels
train_data$LotConfig <- as.factor(train_data$LotConfig)
test_data$LotConfig <- as.factor(test_data$LotConfig)

unique(train_data$LandSlope) # 3 levels
train_data$LandSlope <- as.factor(train_data$LandSlope)
test_data$LandSlope <- as.factor(test_data$LandSlope)

unique(train_data$Neighborhood) #25 levels
train_data$Neighborhood <- as.factor(train_data$Neighborhood)
test_data$Neighborhood <- as.factor(test_data$Neighborhood)

length(unique(train_data$OverallQual)) # only 10 different values and have meaning as can be seen from the description hence it is required to be converted into facotors
# Changes to be done in both train as well as test data
train_data$OverallQual <- as.factor(train_data$OverallQual)
test_data$OverallQual <- as.factor(test_data$OverallQual)

length(unique(train_data$OverallCond)) # need to be converted into factors
train_data$OverallCond <- as.factor(train_data$OverallCond)
test_data$OverallCond <- as.factor(test_data$OverallCond)

length(unique(train_data$YearBuilt)) # let it be integer

length(unique(train_data$YearRemodAdd)) # let it be integer as it is similar to above

length(unique(train_data$Condition1))
train_data$Condition1 <- as.factor(train_data$Condition1)
test_data$Condition1 <- as.factor(test_data$Condition1)

train_data$Condition2 <- as.factor(train_data$Condition2)
test_data$Condition2 <- as.factor(test_data$Condition2)

train_data$BldgType <- as.factor(train_data$BldgType)
test_data$BldgType <- as.factor(test_data$BldgType)

length(unique(train_data$HouseStyle)) #8, need to be as factors
train_data$HouseStyle <- as.factor(train_data$HouseStyle)
test_data$HouseStyle <- as.factor(test_data$HouseStyle)

length(unique(train_data$RoofStyle))
train_data$RoofStyle <- as.factor(train_data$RoofStyle)
test_data$RoofStyle <- as.factor(test_data$RoofStyle)

length(unique(train_data$RoofMatl))
train_data$RoofMatl <- as.factor(train_data$RoofMatl)
test_data$RoofMatl <- as.factor(test_data$RoofMatl)

for(i in 24:26){
  train_data[,i] <- as.factor(train_data[,i])
  test_data[,i] <- as.factor(test_data[,i])
}

for(i in 28:30){
  train_data[,i] <- as.factor(train_data[,i])
  test_data[,i] <- as.factor(test_data[,i])
}

unique(train_data$BsmtQual) # 5 levels but NA here has a significance so will change from NA to NoAc
train_data$BsmtQual[is.na(train_data$BsmtQual)] <-'NoB' 
test_data$BsmtQual[is.na(test_data$BsmtQual)] <- 'NoB'
train_data$BsmtQual <- as.factor(train_data$BsmtQual)
test_data$BsmtQual <- as.factor(test_data$BsmtQual)

train_data$BsmtCond[is.na(train_data$BsmtCond)] <-'NoB' 
test_data$BsmtCond[is.na(test_data$BsmtCond)] <- 'NoB'
train_data$BsmtCond <- as.factor(train_data$BsmtCond)
test_data$BsmtCond <- as.factor(test_data$BsmtCond)

train_data$BsmtExposure[is.na(train_data$BsmtExposure)] <-'NoB' 
test_data$BsmtExposure[is.na(test_data$BsmtExposure)] <- 'NoB'
train_data$BsmtExposure <- as.factor(train_data$BsmtExposure)
test_data$BsmtExposure <- as.factor(test_data$BsmtExposure)

train_data$BsmtFinType1[is.na(train_data$BsmtFinType1)] <-'NoB' 
test_data$BsmtFinType1[is.na(test_data$BsmtFinType1)] <- 'NoB'
train_data$BsmtFinType1 <- as.factor(train_data$BsmtFinType1)
test_data$BsmtFinType1 <- as.factor(test_data$BsmtFinType1)

length(unique(train_data$BsmtFinSF2))

train_data$BsmtFinType2[is.na(train_data$BsmtFinType2)] <-'NoB' 
test_data$BsmtFinType2[is.na(test_data$BsmtFinType2)] <- 'NoB'
train_data$BsmtFinType2 <- as.factor(train_data$BsmtFinType2)
test_data$BsmtFinType2 <- as.factor(test_data$BsmtFinType2)

for(i in 40:43){
  train_data[,i] <- as.factor(train_data[,i])
  test_data[,i] <- as.factor(test_data[,i])
}

length(unique(train_data$LowQualFinSF))# int as per description

length(unique(train_data$BsmtFullBath))# int as per description
length(unique(train_data$BsmtHalfBath))# int as per description
length(unique(train_data$FullBath))# int as per description
length(unique(train_data$HalfBath))# int as per description
length(unique(train_data$LowQualFinSF))# int as per description

train_data$KitchenQual <- as.factor(train_data$KitchenQual)
test_data$KitchenQual <- as.factor(test_data$KitchenQual)

train_data$Functional <- as.factor(train_data$Functional)
test_data$Functional <- as.factor(test_data$Functional)

train_data$GarageFinish[is.na(train_data$GarageFinish)] <-'NoG' 
test_data$GarageFinish[is.na(test_data$GarageFinish)] <- 'NoG'
train_data$GarageFinish <- as.factor(train_data$GarageFinish)
test_data$GarageFinish <- as.factor(test_data$GarageFinish)

train_data$GarageQual[is.na(train_data$GarageQual)] <-'NoG' 
test_data$GarageQual[is.na(test_data$GarageQual)] <- 'NoG'
train_data$GarageQual <- as.factor(train_data$GarageQual)
test_data$GarageQual <- as.factor(test_data$GarageQual)

train_data$GarageCond[is.na(train_data$GarageCond)] <-'NoG' 
test_data$GarageCond[is.na(test_data$GarageCond)] <- 'NoG'
train_data$GarageCond <- as.factor(train_data$GarageCond)
test_data$GarageCond <- as.factor(test_data$GarageCond)

train_data$GarageType[is.na(train_data$GarageType)] <-'NoG' 
test_data$GarageType[is.na(test_data$GarageType)] <- 'NoG'
train_data$GarageType <- as.factor(train_data$GarageType)
test_data$GarageType <- as.factor(test_data$GarageType)

train_data$FireplaceQu[is.na(train_data$FireplaceQu)] <-'NoF' 
test_data$FireplaceQu[is.na(test_data$FireplaceQu)] <- 'NoF'
train_data$FireplaceQu <- as.factor(train_data$FireplaceQu)
test_data$FireplaceQu <- as.factor(test_data$FireplaceQu)

train_data$PavedDrive <- as.factor(train_data$PavedDrive)
test_data$PavedDrive <- as.factor(test_data$PavedDrive)

unique(train_data$PoolQC) # 4 levels but NA here has a significance so will change from NA to NoAc
train_data$PoolQC[is.na(train_data$PoolQC)] <-'NoPool' 
test_data$PoolQC[is.na(test_data$PoolQC)] <- 'NoPool'
train_data$PoolQC<- as.factor(train_data$PoolQC)
test_data$PoolQC <- as.factor(test_data$PoolQC)

unique(train_data$Fence) # 5 levels but NA here has a significance so will change from NA to NoAc
train_data$Fence[is.na(train_data$Fence)] <-'NoF' 
test_data$Fence[is.na(test_data$Fence)] <- 'NoF'
train_data$Fence<- as.factor(train_data$Fence)
test_data$Fence <- as.factor(test_data$Fence)

unique(train_data$MiscFeature) # 5 levels but NA here has a significance so will change from NA to NoAc
train_data$MiscFeature[is.na(train_data$MiscFeature)] <-'None' 
test_data$MiscFeature[is.na(test_data$MiscFeature)] <- 'None'
train_data$MiscFeature <- as.factor(train_data$MiscFeature)
test_data$MiscFeature <- as.factor(test_data$MiscFeature)

for(i in 79:80){
  train_data[,i] <- as.factor(train_data[,i])
  test_data[,i] <- as.factor(test_data[,i])
}


#--------------------Feature Selection------------------------

install.packages("Boruta")
library(Boruta)
colSums(is.na(train_data)) #LotFrontAge(259), MasVnrType(8), GarageYrBlt(81) missing values are there
 # Misssing value treatment
cor.test(train_data$LotFrontage, train_data$SalePrice, method = "spearman")
train_data2$LotFrontage[is.na(train_data2$LotFrontage)] <- mean(train_data2$LotFrontage, na.rm = TRUE)
test_data$LotFrontage[is.na(test_data$LotFrontage)] <- mean(test_data$LotFrontage, na.rm = TRUE)
train_data$GarageYrBlt[is.na(train_data$GarageYrBlt)] <- mean(train_data$GarageYrBlt, na.rm = TRUE)
test_data$GarageYrBlt[is.na(test_data$GarageYrBlt)] <- mean(test_data$GarageYrBlt, na.rm = TRUE)
train_data$MasVnrType[is.na(train_data$MasVnrType)] <- Mode(train_data$MasVnrType)
test_data$MasVnrType[is.na(test_data$MasVnrType)] <- Mode(test_data$MasVnrType)
train_data$Electrical[is.na(train_data$Electrical)] <- Mode(train_data$Electrical)
test_data$Electrical[is.na(test_data$Electrical)] <- Mode(test_data$Electrical)
train_data$MasVnrArea[is.na(train_data$MasVnrArea)] <- mean(train_data$MasVnrArea, na.rm = TRUE)
test_data$MasVnrArea[is.na(test_data$MasVnrArea)] <- mean(test_data$MasVnrArea, na.rm = TRUE)

# Treating missing data from test data

test_data2$MSZoning <- as.factor(test_data2$MSZoning)
test_data2$MSZoning[is.na(test_data2$MSZoning)] <- Mode(test_data2$MSZoning)
test_data2$Exterior1st[is.na(test_data2$Exterior1st)] <- Mode(test_data2$Exterior1st)
test_data2$Exterior2nd[is.na(test_data2$Exterior2nd)] <- Mode(test_data2$Exterior2nd)
test_data2$BsmtFinSF1[is.na(test_data$BsmtFinSF1)] <- mean(test_data$BsmtFinSF1, na.rm = TRUE)
test_data2$BsmtUnfSF[is.na(test_data2$BsmtUnfSF)] <- mean(test_data2$BsmtUnfSF, na.rm = TRUE)
test_data2$TotalBsmtSF[is.na(test_data2$TotalBsmtSF)] <- mean(test_data2$TotalBsmtSF, na.rm = TRUE)
test_data2$KitchenQual[is.na(test_data2$KitchenQual)] <- Mode(test_data2$KitchenQual)
test_data2$BsmtFullBath[is.na(test_data2$BsmtFullBath)] <- Mode(test_data2$BsmtFullBath)
test_data2$Functional[is.na(test_data2$Functional)] <- Mode(test_data2$Functional)
test_data2$GarageCars[is.na(test_data2$GarageCars)] <- Mode(test_data2$GarageCars)
test_data2$GarageArea[is.na(test_data2$GarageArea)] <- mean(test_data2$GarageArea, na.rm = TRUE)

#Mode function
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#Feature selection can be done using Boruta now
boruta.train <- Boruta(SalePrice~.-Id, data = train_data, doTrace=2)
print(boruta.train)
plot(boruta.train)
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = FALSE)
boruta.df <- attStats(final.boruta)
View(boruta.df)
selecteAtrr <- getSelectedAttributes(final.boruta, withTentative = FALSE)

# Only important features are selected
train_data2 <- subset(train_data, select= selecteAtrr) # this train set will be used for further exploration
test_data2 <- subset(test_data, select= selecteAtrr)
View(test_data2)
colSums(is.na(test_data2))

#-----------------------Univariate Analysis----------------------------
str(train_data2)
train_data2$MSZoning <- as.factor(train_data2$MSZoning)
train_data2$HeatingQC <- as.factor(train_data2$HeatingQC)
train_data2$CentralAir <- as.factor(train_data2$CentralAir)
train_data2$Electrical <- as.factor(train_data2$Electrical)
train_data2$MasVnrArea <- as.integer(train_data2$MasVnrArea)
train_data2$LotFrontage <- as.integer(train_data2$LotFrontage)
train_data2$GarageYrBlt <- as.integer(train_data2$GarageYrBlt)

#-----------------------------------------------------------------------

hist(train_data$LotFrontage) # ~normal distribn with few outliers
boxplot(train_data$LotFrontage)

hist(train_data2$LotArea) #right skewed, taking log helps
train_data2$LotArea <- log(train_data2$LotArea)
test_data2$LotArea <- log(test_data2$LotArea)

# Area <- train_data2$MasVnrArea
# Area <- log(Area)
# hist(Area)
boxplot(train_data2$LotArea) #  Few Outlier

hist(train_data2$YearBuilt)

hist(train_data2$YearRemodAdd)

hist(train_data2$MasVnrArea)#right skewed, log transformation helps
train_data2$MasVnrArea <- log(train_data2$MasVnrArea)# now normal with few outliers
test_data2$MasVnrArea <- log(test_data2$LotArea)

hist(train_data2$BsmtUnfSF) #right skewed
train_data2$BsmtUnfSF <- log(train_data2$BsmtUnfSF)#got left skewed
train_data2$BsmtUnfSF <- train_data$BsmtUnfSF
train_data2$BsmtUnfSF <- (train_data2$BsmtUnfSF)^(1/3)#normal with some outlier
hist(train_data2$BsmtUnfSF) 
test_data2$BsmtUnfSF <- (test_data2$BsmtUnfSF)^(1/3)

hist(train_data2$BsmtFinSF1) #right skewed
train_data2$BsmtFinSF1 <- log(train_data2$BsmtFinSF1)
test_data2$BsmtFinSF1 <- log(test_data2$BsmtFinSF1)
hist(train_data2$BsmtFinSF1) #solved

hist(train_data2$TotalBsmtSF) #normal with few outliers

hist(train_data2$X1stFlrSF)
hist(train_data2$X2ndFlrSF) #right skew
train_data2$X2ndFlrSF <- log(train_data2$X2ndFlrSF) #normal now with few outliers
test_data2$X2ndFlrSF <- log(test_data2$X2ndFlrSF)

hist(train_data2$GrLivArea) #normal distributed

hist(train_data2$GarageArea) #close to normal

hist(train_data2$WoodDeckSF) #right skew
train_data2$WoodDeckSF <- log(train_data2$WoodDeckSF)
test_data2$WoodDeckSF <- log(test_data2$WoodDeckSF)

hist(train_data2$OpenPorchSF)
train_data2$OpenPorchSF <- log(train_data2$OpenPorchSF)
test_data2$OpenPorchSF <- log(test_data2$OpenPorchSF)

#--------------------Outlier Treatment--------------------------
train_data2$LotFrontage[train_data2$LotFrontage>130] <- mean(train_data2$LotFrontage[-train_data2$LotFrontage>130])
test_data2$LotFrontage[test_data2$LotFrontage>130] <- mean(test_data2$LotFrontage[-test_data2$LotFrontage>130])

train_data2$LotArea[train_data2$LotArea>10.5] <- mean(train_data2$LotArea[-train_data2$LotArea > 10.5])
test_data2$LotArea[test_data2$LotArea>10.5] <- mean(test_data2$LotArea[-test_data2$LotArea > 10.5])

train_data2$MasVnrArea[train_data2$MasVnrArea<2] <- mean(train_data2$MasVnrArea[-train_data2$MasVnrArea < 2])

train_data2$BsmtUnfSF[train_data2$BsmtUnfSF<2] <- mean(train_data2$BsmtUnfSF[-train_data2$BsmtUnfSF < 2])
train_data2$BsmtUnfSF[train_data2$BsmtUnfSF>13] <- mean(train_data2$BsmtUnfSF[-train_data2$BsmtUnfSF >13])

train_data2$TotalBsmtSF[train_data2$TotalBsmtSF<2] <- mean(train_data2$TotalBsmtSF[-train_data2$TotalBsmtSF > 2500])

train_data2$X1stFlrSF[train_data2$X1stFlrSF>2500] <- mean(train_data2$X1stFlrSF[-train_data2$X1stFlrSF > 2500])

train_data2$X2ndFlrSF[train_data2$X2ndFlrSF < 5] <- mean(train_data2$X2ndFlrSF[-train_data2$X2ndFlrSF <5])

train_data2$X2ndFlrSF[train_data2$X2ndFlrSF > 8] <- mean(train_data2$X2ndFlrSF[-train_data2$X2ndFlrSF >8])

train_data2$GrLivArea[train_data2$GrLivArea > 3500] <- mean(train_data2$GrLivArea[-train_data2$GrLivArea >3500])

train_data2$WoodDeckSF[train_data2$WoodDeckSF < 3] <- mean(train_data2$WoodDeckSF[-train_data2$WoodDeckSF <3])

train_data2$OpenPorchSF[train_data2$OpenPorchSF < 2] <- mean(train_data2$OpenPorchSF[-train_data2$OpenPorchSF <2])

train_data2$BsmtFinSF1 <- NULL
test_data2$BsmtFinSF1 <- NULL
#-------------Model Building------------------------------
fit <- lm(Sale_Price~., data = train_data2)
predict(fit, newdata = test_data2)
View(test_data2)
str(test_data2)
