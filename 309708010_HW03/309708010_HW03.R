setwd("C:/Users/Steven/Desktop/陽交109下/巨量資料分析/課程/單元5：相似度、鄰點、與聚類/範例程式與資料")
accs <- read.csv(file = "Accidents.csv", header=T, encoding='ANSI')
View(accs) # 開另一個視窗觀察csv檔的長相
str(accs) # 觀察此資料的結構以及各屬性的資料型態以及內容
class(accs) # 確認一下accs是不是dataframe

# 將不重要的屬性、重複的屬性刪掉(像名稱跟代碼意義一樣就選擇把代碼去掉)
accs = accs[, c(6, 8, 10, 12, 13, 15, 17, 19, 20)] 
# 觀察新資料的結構以及各屬性的資料型態以及內容 
str(accs) 
# 看一下剩餘特徵的資料分布，觀察有無空值、異常值、以及需要處理成數字的值
summary(accs) 

# 把age的異常值直接改成NA
accs$age <- ifelse(accs$age>=0 , accs$age, NA)

# 從類別型屬性(共8個)下手，從中找到其空值的長相，有兩種分別是" "和""
unique(accs$Weather.name)
unique(accs$Light.name)
unique(accs$Road.condition.name)
unique(accs$Gender.name)
unique(accs$Vehicle.type.name)
unique(accs$Protection.equipment.name)
unique(accs$Drinking.situation.name)
unique(accs$Accident.category)

# 將" "和""帶換成空值
accs[accs == ""] <- NA
accs[accs == " "] <- NA
# 確認一下""和" "是否都轉成NA了
unique(accs$Weather.name)
unique(accs$Light.name)
unique(accs$Road.condition.name)
unique(accs$Gender.name)
unique(accs$Vehicle.type.name)
unique(accs$Protection.equipment.name)
unique(accs$Drinking.situation.name)
unique(accs$Accident.category)

# 算一下各個屬性共有幾筆NA
num_na <- function(x){sum(is.na(x))}
sapply(accs, num_na)

# 一共9643筆資料
# 但Drinking.situation.name、Protection.equipment.name、Light.name這三個特徵NA都過高，所以選擇刪除
accs = accs[, c(1, 3, 4, 5, 6, 9)]
# Vehicle.type.name、Gender.name、age因為空值很少但不好插補，選擇有NA的直接整筆刪除
accs <- accs[!is.na(accs$Vehicle.type.name),]
accs <- accs[!is.na(accs$Gender.name),]
accs <- accs[!is.na(accs$age),]
sapply(accs, num_na) # 只剩Weather.name、Road.condition.name有NA

# 接下來處理天氣狀況去插補路面的NA、或是一些明顯錯誤的值(比如暴雨地面不會乾燥)
accs$Road.condition.name <- ifelse(is.na(accs$Road.condition.name) & 
                                     accs$Weather.name=='晴', '乾燥', accs$Road.condition.name)
accs$Road.condition.name <- ifelse(is.na(accs$Road.condition.name) & 
                                     accs$Weather.name=='雨', '濕潤', accs$Road.condition.name)
accs$Road.condition.name <- ifelse(is.na(accs$Road.condition.name) & 
                                     accs$Weather.name=='暴雨', '濕潤', accs$Road.condition.name)
accs$Road.condition.name <- ifelse(is.na(accs$Road.condition.name) & 
                                     accs$Weather.name=='陰', '乾燥', accs$Road.condition.name)
accs$Road.condition.name <- ifelse(accs$Road.condition.name=='乾燥' & 
                                     accs$Weather.name=='暴雨', '濕潤', accs$Road.condition.name)
accs$Road.condition.name <- ifelse(accs$Road.condition.name=='濕潤' & 
                                     accs$Weather.name=='晴', '乾燥', accs$Road.condition.name)
# 剩下的NA直接整筆刪除
accs <- accs[!is.na(accs$Road.condition.name),]
accs <- accs[!is.na(accs$Weather.name),]
sapply(accs, num_na) # 發現都已補完

# 再次確認一下剩餘類別屬性的值有哪些
unique(accs$Weather.name)
unique(accs$Road.condition.name)
unique(accs$Gender.name)
unique(accs$Vehicle.type.name)
unique(accs$Accident.category)

# 發現Vehicle.type.name值有點多
# 因此打算依據速度分成三種，人、慢車一組；機車一組;剩餘的一組
accs$Vehicle.type.name <- ifelse(accs$Vehicle.type.name=='人', '慢', accs$Vehicle.type.name)
accs$Vehicle.type.name <- ifelse(accs$Vehicle.type.name=='慢車', '慢', accs$Vehicle.type.name)
accs$Vehicle.type.name <- ifelse(accs$Vehicle.type.name=='機車', '中', accs$Vehicle.type.name)
accs$Vehicle.type.name <- ifelse(accs$Vehicle.type.name=='慢' | 
                                   accs$Vehicle.type.name=='中', accs$Vehicle.type.name, '快')
unique(accs$Vehicle.type.name)

# 將類別型屬性轉成one-hot variable前要先將屬性轉成factor的型態(性別因為是二元所以不用)
accs$Weather.name <- as.factor(accs$Weather.name)
accs$Road.condition.name <- as.factor(accs$Road.condition.name)
accs$Vehicle.type.name <- as.factor(accs$Vehicle.type.name)
accs$Accident.category <- as.factor(accs$Accident.category)
# 確認是否轉成功
str(accs)
# 導入套件並做one-hot encoding
library(mltools)
library(data.table)
accs <- one_hot(as.data.table(accs))
View(accs)

# 最後處理gender這欄，男生:1 女生:0
accs$Gender.name <- ifelse(accs$Gender.name=='男', 1, 0)

# 資料正規化
accs_z <- sapply(accs, scale)
View(accs_z)

# k-mean聚類分析，令k=2
kc <- kmeans(accs_z, iter.max=30, centers=2, nstart=10) 
# 主成分分析
library(factoextra)
fviz_cluster(kc, geom="point", data=accs_z)
# 分群結果放入原本的資料集
accs$cluster <- kc$cluster

# 看一下accs的feature有哪些
names(accs)
# 查看各群的屬性值是否有差異
aggregate(data=accs, cbind(age, Gender.name, Weather.name_雨, 
                           Weather.name_強風, Weather.name_陰, 
                           Weather.name_暴雨, Vehicle.type.name_中, 
                           Vehicle.type.name_快, Vehicle.type.name_慢, 
                           Accident.category_A1, Accident.category_A2, 
                           Accident.category_A3) ~ cluster, mean, na.rm=TRUE)



