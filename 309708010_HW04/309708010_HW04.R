
setwd("C:\\Users\\Steven\\Desktop\\陽交109下\\巨量資料分析\\課程\\單元10：文字探勘\\範例程式與資料")
getwd()


# 載入ptt爬蟲與斷詞相關套件
library(XML)
library(RCurl)
library(tm)
library(jiebaR)


# 寫個迴圈，可以一次抓多頁所有文章的連結（每次抓兩頁修改，共十頁）
link <- NULL
for( i in 4998:5007){
  url <- paste0("https://www.ptt.cc/bbs/Kaohsiung/index", i, ".html")
  html <- htmlParse(getURL(url)) 
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  link <- c(link, paste('https://www.ptt.cc', url.list, sep=''))
}
length(link)


# 寫個迴圈，抓出所有儲存在link中的文章，處理並儲存
article <- NULL
for(i in 1:length(link)){
  Sys.sleep(runif(1,1,3)) # 休息一下，以免被誤會成攻擊程式
  html <- htmlParse(getURL(link[i]))
  doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
  doc <- removePunctuation(doc)
  doc <- gsub("[A-Za-z0-9]", "", doc)
  doc <- gsub(" ", "", doc)
  doc <- gsub("\n", "", doc)
  doc <- gsub("[.]|[?]|[!]|[:]|[：]|[。]|[，]|[→]|[※]|[♡]", "", doc) 
  doc <- gsub("[～]|[「]|[」]|[！]|[、]|[（]|[）]|[︰]|[？]|[…]", "", doc)
  doc <- gsub("[〔]|[〕]|[／]|[《]|[》]", "", doc)
  article[i] <- doc
}
length(article)


# 刪掉一些Stop Words後斷句，然後將斷句後的結果予以儲存
seg <- worker(stop_word="Stop_Tw.txt")
corpus <- NULL
for(i in 1:length(article)){
  corpus[[i]] <- segment(article[i], seg)
}
length(corpus)
corpus[[170]]
edit_dict() #別忘了要重新載入新詞庫


# 產生詞袋（Bag of Words）
union <- NULL
for(i in 1: length(article)){
  union <- c(union, corpus[[i]])
}


# 去掉重複的
bag <- unique(union)
length(union)
length(bag)


# 文件集中有多份文件，寫個迴圈來執行
tf <- matrix(0, nrow=length(article), ncol=length(bag), dimnames=list(NULL,bag))
new.tf1 <- tf
new.tf2 <- tf
for(i in 1:length(corpus)){
  matchID <- match(corpus[[i]], bag)
  len <- length(matchID)
  for(j in 1:len){
    new.tf1[i, matchID[j]] <- 1
    new.tf2[i, matchID[j]] <- (new.tf2[i, matchID[j]] + 1)
  }
}


# 計算每個token在文件集中出現的次數
new.tf1[1:10, 1:10]
tf_count <- colSums(new.tf1)
tf_count <- sort(tf_count, decreasing = TRUE)
tf_count[1:5]
# idf
idf <- 1 + log(nrow(new.tf1)/colSums(new.tf1))

# 完成TFIDF表格
tfidf <- new.tf2
for(word in names(idf)){
  tfidf[,word] <- tfidf[,word] * idf[word]
}
tfidf[1:10, 1:10]

# 算相關性
cor_term <- cor(new.tf1)
cor_guardian <- cor_term["高雄",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian[1:5]
cor_guardian <- cor_term["口罩",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian[1:5]
cor_guardian <- cor_term["防疫",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian[1:5]
cor_guardian <- cor_term["疫情",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian[1:5]
cor_guardian <- cor_term["戴",]
cor_guardian <- sort(cor_guardian, decreasing = TRUE)
cor_guardian[1:5]

