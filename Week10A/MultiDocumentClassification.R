#init

library("tm")
library("plyr")
library("class")
library("SnowballC")
library("stringr")
library("wordcloud")

library("RTextTools")
library("e1071")
library("tidyr")
library("DT")

options(stringsAsFactors = FALSE)

datapath <- "F:/Data/Reuters21578_90Cat/training"
category <- c("grain","wheat")

trade.directory <- "F:/Data/Reuters21578_90Cat/training/trade"
moneyfx.directory <- "F:/Data/Reuters21578_90Cat/training/moneyfx"    
crude.directory <- "F:/Data/Reuters21578_90Cat/training/crude"    


tradetest.dir <- "F:/Data/Reuters21578_90Cat/test/trade"
moneyfxtest.dir <- "F:/Data/Reuters21578_90Cat/test/moneyfx"
crudetest.dir <- "F:/Data/Reuters21578_90Cat/test/crude"



# clean text
cleanupDataCorpus <- function(dataCorpus){
     
     cleanDataCorpus <- tm_map(dataCorpus, removePunctuation)
     cleanDataCorpus <- tm_map(cleanDataCorpus, removeNumbers)
     
     cleanDataCorpus <- tm_map(cleanDataCorpus, str_replace_all, pattern
                              = "[[:punct:]]", replacement = " ")     
     cleanDataCorpus <- tm_map(cleanDataCorpus, tolower)
     cleanDataCorpus <- tm_map(cleanDataCorpus, removeWords, c('said','u.s',stopwords("english")))
     
     
     cleanDataCorpus <- tm_map(cleanDataCorpus, stemDocument)
     cleanDataCorpus<- tm_map(cleanDataCorpus, PlainTextDocument)
     return(cleanDataCorpus)
     
}

# build Term Document Matrix for training data

trade.corpus <- Corpus(DirSource(directory=trade.directory, encoding = "ASCII"))
moneyfx.corpus <- Corpus(DirSource(directory=moneyfx.directory, encoding = "ASCII"))
crude.corpus <- Corpus(DirSource(directory=crude.directory, encoding = "ASCII"))


trade.cleancorpus <- cleanupDataCorpus(trade.corpus)
moneyfx.cleancorpus <- cleanupDataCorpus(moneyfx.corpus)
crude.cleancorpus <- cleanupDataCorpus(crude.corpus)

trade.tdm <- TermDocumentMatrix(trade.cleancorpus)
moneyfx.tdm <- TermDocumentMatrix(moneyfx.cleancorpus)
crude.tdm <- TermDocumentMatrix(crude.cleancorpus)


trade.tdm <- removeSparseTerms(trade.tdm, 0.7)
moneyfx.tdm<- removeSparseTerms(moneyfx.tdm, 0.7)
crude.tdm<- removeSparseTerms(crude.tdm, 0.7)

trade.datamatx <- t(data.matrix(trade.tdm))
moneyfx.datamatx <- t(data.matrix(moneyfx.tdm))
crude.datamatx <- t(data.matrix(crude.tdm))


head(trade.datamatx)
head(moneyfx.datamatx)
head(crude.datamatx)



trade.df <- as.data.frame(trade.datamatx)
moneyfx.df <- as.data.frame(moneyfx.datamatx)
crude.df <- as.data.frame(crude.datamatx)


# build Term Document Matrix for test data

tradetest.corpus <- Corpus(DirSource(directory=tradetest.dir, encoding = "ASCII"))
moneyfxtest.corpus <- Corpus(DirSource(directory=moneyfxtest.dir, encoding = "ASCII"))
crudetest.corpus <- Corpus(DirSource(directory=crudetest.dir, encoding = "ASCII"))


tradetest.cleancorpus <- cleanupDataCorpus(tradetest.corpus)
moneyfxtest.cleancorpus <- cleanupDataCorpus(moneyfxtest.corpus)
crudetest.cleancorpus <- cleanupDataCorpus(crudetest.corpus)


tradetest.tdm <- TermDocumentMatrix(tradetest.cleancorpus)
moneyfxtest.tdm <- TermDocumentMatrix(moneyfxtest.cleancorpus)
crudetest.tdm <- TermDocumentMatrix(crudetest.cleancorpus)

tradetest.tdm <- removeSparseTerms(tradetest.tdm, 0.7)
moneyfxtest.tdm<- removeSparseTerms(moneyfxtest.tdm, 0.7)
crudetest.tdm<- removeSparseTerms(crudetest.tdm, 0.7)

tradetest.datamatx <- t(data.matrix(tradetest.tdm))
moneyfxtest.datamatx <- t(data.matrix(moneyfxtest.tdm))
crudetest.datamatx <- t(data.matrix(crudetest.tdm))

head(tradetest.datamatx)
head(moneyfxtest.datamatx)
head(crudetest.datamatx)

tradetest.df <- as.data.frame(tradetest.datamatx)
moneyfxtest.df <- as.data.frame(moneyfxtest.datamatx)
crudetest.df <- as.data.frame(crudetest.datamatx)

# filtering for columns present in test and training and then adding the columns/terms present only in training
# attempt to keep the same terms in test and training 


# subset trade testing data by colnames (ie. terms) from training trade data

trade.df.mat <- as.matrix(trade.df)
tradetest.df.mat <-  as.matrix(tradetest.df)


tradetestfiltered1 <- data.frame(tradetest.df.mat[,intersect(colnames(tradetest.df.mat),
                                           colnames(trade.df.mat))])

tradetestfiltered2 <- read.table(textConnection(""), col.names = colnames(trade.df.mat))


tradetestfiltered <- rbind.fill(tradetestfiltered1, tradetestfiltered2)

ncol(tradetestfiltered)


# subset moneyfx testing data by colnames (ie. terms) from moneyfx training data


moneyfx.df.mat <- as.matrix(moneyfx.df)
moneyfxtest.df.mat <-  as.matrix(moneyfxtest.df)


moneyfxtestfiltered1 <- data.frame(moneyfxtest.df.mat[,intersect(colnames(moneyfxtest.df.mat),
                                                             colnames(moneyfx.df.mat))])

moneyfxtestfiltered2 <- read.table(textConnection(""), col.names = colnames(moneyfx.df.mat))


moneyfxtestfiltered <- rbind.fill(moneyfxtestfiltered1, moneyfxtestfiltered2)

ncol(moneyfxtestfiltered)




# subset crude testing data by colnames (ie. terms) from crude training data


crude.df.mat <- as.matrix(crude.df)
crudetest.df.mat <-  as.matrix(crudetest.df)


crudetestfiltered1 <- data.frame(crudetest.df.mat[,intersect(colnames(crudetest.df.mat),
                                                                 colnames(crude.df.mat))])

crudetestfiltered2 <- read.table(textConnection(""), col.names = colnames(crude.df.mat))


crudetestfiltered <- rbind.fill(crudetestfiltered1, crudetestfiltered2)

ncol(crudetestfiltered)



# Binding 'Category' column to the data frame

trade.df <- cbind(trade.df, category= rep("trade"))
moneyfx.df <- cbind(moneyfx.df, category = rep("moneyfx"))
crude.df <- cbind(crude.df, category = rep("crude"))


tradetestfiltered <- cbind(tradetestfiltered, category= rep("trade"))
moneyfxtestfiltered <- cbind(moneyfxtestfiltered, category = rep("moneyfx"))
crudetestfiltered <- cbind(crudetestfiltered, category = rep("crude"))

# We now have two dataframes  , one each category , with the content as rows and the terms as variables / columns.
# and a category column appended to denote the category.
# We now stack botht the tdms 

# Stacking the TDMS of different categories for training data

tdm.stackedcategory <- rbind.fill(trade.df, moneyfx.df, crude.df)

nrow(trade.df)
nrow(moneyfx.df)
nrow(crude.df)
nrow(tdm.stackedcategory)

tdm.stackedcategory [is.na(tdm.stackedcategory)] <- 0

ncol(trade.df)
ncol(moneyfx.df)
ncol(crude.df)
ncol(tdm.stackedcategory)

# Stacking the TDMS of different categories for test data
tdm.teststackedcategory <- rbind.fill(tradetestfiltered, moneyfxtestfiltered, crudetestfiltered)

nrow(tradetestfiltered)
nrow(moneyfxtestfiltered)
nrow(crudetestfiltered)
nrow(tdm.teststackedcategory)

tdm.teststackedcategory [is.na(tdm.teststackedcategory)] <- 0

ncol(tradetestfiltered)
ncol(moneyfxtestfiltered)
ncol(crudetestfiltered)
ncol(tdm.teststackedcategory) 


# We now see the number of total rows in stacked trainging TDM and stacked test TDM is equivalent to the sum of total rows for each category TDM.


tdm.traincategorycol <- tdm.stackedcategory[,"category"]
tdm.train <- tdm.stackedcategory[,!colnames(tdm.stackedcategory) %in% "category"]

tdm.testcategorycol <- tdm.teststackedcategory[,"category"]
tdm.test <- tdm.teststackedcategory[,!colnames(tdm.teststackedcategory) %in% "category"]


# MODEL KNN (K Nearest Neighbour)

knn.pred <- knn(tdm.train, tdm.test, tdm.traincategorycol)


# accuracy test

confusion_matx <- table("Predictions" = knn.pred, Actual = tdm.testcategorycol)
confusion_matx

# Calcculating Accuracy
accuracy <- sum(diag(confusion_matx))/ nrow(tdm.test) * 100

# Another way to calculate accuracy

true_moneyfx <- 179
true_trade <- 112

false_moneyfx <- 5
false_trade <- 0
     
     
total <- true_moneyfx + true_trade + false_moneyfx + false_trade
accuracy <- (true_moneyfx + true_trade) / total 

error <- 1 - accuracy




container <- create_container(tdm.train, tdm.stackedcategory$category, trainSize=1:1296, virgin=FALSE)

predtestcontainer <- create_container(tdm.test, labels= 0:485, testSize = 1:485, virgin=FALSE)


# MODEL SVM

SVM <- train_model(container,"SVM")
results.SVM <- classify_model(predtestcontainer, SVM)
results.SVM


# MODEL MAXENT

MAXENT <- train_model(container,"MAXENT")
results.MAXENT <- classify_model(predtestcontainer, MAXENT)
results.MAXENT


# MODEL BAGGING

BAGGING <- train_model(container,"BAGGING")
results.BAGGING <- classify_model(predtestcontainer, BAGGING)
results.BAGGING


# MODEL Random Forest

RF <- train_model(container,"RF")
results.RF <- classify_model(predtestcontainer, RF)
results.RF



# MODEL TREE

TREE <- train_model(container,"TREE")
results.TREE <- classify_model(predtestcontainer, TREE)
results.TREE


cat_check <- data.frame(
     correct_cat = tdm.testcategorycol[1:485],
     svm = as.character(results.SVM[,1]),
     maxent = as.character(results.MAXENT[,1]),
     bagging = as.character(results.BAGGING[,1]),
     rf = as.character(results.RF[,1]),
     tree = as.character(results.TREE[,1]),
     stringAsFactors = F)

#SVM Performance
svm_table <- table(cat_check[,1] == cat_check[,2])
addmargins(svm_table)
psvm <- prop.table(svm_table)
psvm <-as.data.frame(psvm)
psvm$Model <- "SVM"

# MAXENT Performance
maxent_table <- table(cat_check[,1] == cat_check[,3])
addmargins(maxent_table)
pmaxent <- prop.table(maxent_table)
pmaxent <- as.data.frame(pmaxent)
pmaxent$Model <- "MAXENT"

# Bagging Performance
bagging_table <- table(cat_check[,1] == cat_check[,4])
addmargins(bagging_table)
pbag <- prop.table(bagging_table)
pbag <-as.data.frame(pbag)
pbag$Model <- "BAGGING"

# RF Performanace
rf_table <- table(cat_check[,1] == cat_check[,5])
addmargins(rf_table)
prf <- prop.table(rf_table)
prf <- as.data.frame(prf)
prf$Model <- "RF"

# Tree Performanace
tree_table <- table(cat_check[,1] == cat_check[,6])
addmargins(tree_table)
ptree <- prop.table(tree_table)
ptree <-as.data.frame(ptree)
ptree$Model <- "TREE"

pmodels <- rbind.fill(as.data.frame(psvm)
,as.data.frame(pmaxent)
,as.data.frame(pbag)
,as.data.frame(prf),as.data.frame(ptree)
)



colnames(pmodels) <- c("Status" , "Frequency", "Model")

pmodels <- rbind(pmodels, c('FALSE',error,"KNN"),c('TRUE',accuracy,"KNN"))
datatable(pmodels)





ggplot(pmodels, aes(Model, Frequency)) +     geom_point(aes(colour = Status), size = 5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))




#GLMNET <- train_model(container,"GLMNET")

#SLDA <- train_model(container,"SLDA")
#BOOSTING <- train_model(container,"BOOSTING")




# Word cloud

#vfont <- c("gothic english","plain")
#pal <- brewer.pal(8,"Dark2")

#wordcloud(trade.cleancorpus, max.words =20 , random.order = FALSE, colors = brewer.pal(8,"Dark2"),  scale = c(4,0.8), vfont = c("gothic english","plain"))
#wordcloud(moneyfx.cleancorpus,max.words =20 , random.order = FALSE,colors = brewer.pal(9,"Reds") ,  scale = c(3,0.5), vfont=c("serif","plain"))

#wordcloud(crude.cleancorpus,max.words =20 , random.order = FALSE,colors = brewer.pal(9,"Reds") ,  scale = c(3,0.5), vfont=c("serif","plain"))


colnames(moneyfx.df)

