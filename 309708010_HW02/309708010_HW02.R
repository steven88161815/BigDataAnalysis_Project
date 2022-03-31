# 導入相關套件
library(tree)
library(rpart)
library(randomForest)
library(e1071)

# 讀入資料
setwd('C:/Users/Steven/Desktop/陽交109下/巨量資料分析/課程/單元3：預測模式（二）/範例程式與資料')
data <- read.csv("MaaS_Data.csv",header=T)
head(data) # 看一下前幾筆資料

# 資料清洗和選取
data[data == ""] <- NA # 將空值以NA取代
head(data) # 確認一下空值是否都用NA補

# 刪除具有NA的資料並加以確認
data <- data[,-1] # ID對判斷是否購買沒幫助，予以刪除
num_na <- function(x){sum(is.na(x))}
sapply(data, num_na) # 對Data Frame的每一行(column)進行num_na函數運算，用來計算NA的數量，發現只有Buy那欄有NA
data <- data[!is.na(data$Buy),] # 將有NA值得資料刪除
sapply(data, num_na) # 確認是否還有空值
head(data) # 確認一下資料現在的樣子

# label轉成factor，用於分類
data$Buy <- as.factor(data$Buy)

# 分割資料(train:80%, test:20%)(這個block要跑10次)
n <- 0.2*nrow(data)
index <- sample(1:nrow(data), n) # 用隨機取的方式
maas_train <- data[-index,]
maas_test <- data[index,]

# 利用tree套件建模
maas.tree <- tree(Buy ~ ., data=maas_train)
tree.predict <- predict(maas.tree, maas_test, type="class")
compare.tree <- ifelse(tree.predict == maas_test$Buy, 1, 0)
accuracy.tree <- sum(compare.tree)/ length(compare.tree); accuracy.tree

# 利用rpart套件建模
maas.rpart <- rpart(Buy ~ ., data=maas_train)
rpart.predict <- predict(maas.rpart, maas_test, type="class")
compare.rpart <- ifelse(rpart.predict == maas_test$Buy, 1, 0)
accuracy.rpart <- sum(compare.rpart)/ length(compare.rpart); accuracy.rpart

# 利用rf套件建模
maas.rf <- randomForest(Buy ~ ., data=maas_train)
rf.predict <- predict(maas.rf, maas_test, type="class")
compare.rf <- ifelse(rf.predict == maas_test$Buy, 1, 0)
accuracy.rf <- sum(compare.rf)/ length(compare.rf); accuracy.rf

# 利用svm套件建模
maas.svm <- svm(Buy ~ ., data=maas_train)
svm.predict <- predict(maas.svm, maas_test)
compare.svm <- ifelse(svm.predict == maas_test$Buy, 1, 0)
accuracy.svm <- sum(compare.svm)/ length(compare.svm); accuracy.svm

# 利用Logistics Regression套件建模
maas.logit <- glm(Buy ~ ., family=binomial(link='logit'), data=maas_train)
logit.predict <- predict(maas.logit, maas_test, type="response")
logit.results <- ifelse(logit.predict > 0.5, 2, 1)
compare.logit <- ifelse(logit.results == as.numeric(maas_test$Buy), 1, 0)
accuracy.logit <- sum(compare.logit)/ length(compare.logit); accuracy.logit
