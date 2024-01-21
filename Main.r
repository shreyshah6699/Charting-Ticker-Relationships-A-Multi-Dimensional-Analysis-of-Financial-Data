rm(list = ls())

filename <- file.choose()
dataset_fundamentals <- read.csv(filename)

dataset_fundamentals<- subset(dataset_fundamentals, For.Year == 2013)
#Removing the null values from the data
dataset_fundamentals<- na.omit(dataset_fundamentals)
#Considering 100 tickers
dataset_fundamentals<- (dataset_fundamentals[1:100,])
#Normalizing the quantitative variables and hence finding the
zscore <- function(value)
{
  (value-mean(value))/sd(value)
}
dataset_fundamentals$After.Tax.ROE <-
  zscore(dataset_fundamentals$After.Tax.ROE)
dataset_fundamentals$Cash.Ratio<- zscore(dataset_fundamentals$Cash.Ratio)
dataset_fundamentals$Current.Ratio <-
  zscore(dataset_fundamentals$Current.Ratio)
dataset_fundamentals$Operating.Margin <-
  zscore(dataset_fundamentals$Operating.Margin)
dataset_fundamentals$Pre.Tax.Margin <-
  zscore(dataset_fundamentals$Pre.Tax.Margin)
dataset_fundamentals$Pre.Tax.ROE <- zscore(dataset_fundamentals$Pre.Tax.ROE)
dataset_fundamentals$Profit.Margin <-
  zscore(dataset_fundamentals$Profit.Margin)
dataset_fundamentals$Quick.Ratio <- zscore(dataset_fundamentals$Quick.Ratio)
dataset_fundamentals$Total.Assets <-
  zscore(dataset_fundamentals$Total.Assets)
dataset_fundamentals$Total.Liabilities <-
  zscore(dataset_fundamentals$Total.Liabilities)
dataset_fundamentals$Earnings.Per.Share <-
  zscore(dataset_fundamentals$Earnings.Per.Share)

dataset1 <- data.frame(dataset_fundamentals$Ticker.Symbol,
                       dataset_fundamentals$After.Tax.ROE,
                       dataset_fundamentals$Cash.Ratio,
                       dataset_fundamentals$Current.Ratio,
                       dataset_fundamentals$Operating.Margin,
                       dataset_fundamentals$Pre.Tax.Margin,
                       dataset_fundamentals$Pre.Tax.ROE,
                       dataset_fundamentals$Profit.Margin,
                       dataset_fundamentals$Quick.Ratio,
                       dataset_fundamentals$Total.Assets,
                       dataset_fundamentals$Total.Liabilities,
                       dataset_fundamentals$Earnings.Per.Share)
#Giving the summary of the main data
summary(dataset1)

dataset1<- t(dataset1)
#Removing the first column and first row(V1,V2,V3) from the main datacolnames(dataset1) <- as.character(unlist(dataset1[1,]))
dataset1<- (dataset1[-1,])
#Creation of a dataframe for the z scores
dataset1<- data.frame(apply(dataset1, 2,function(x)
  as.numeric(as.character(x))))

#lp norm calculation
lp_norm <- function(File,p){
  stockC<- c()
  stockD <- c()
  lp_norm <-c()
  for (i in 1:ncol(File)) {
    for (j in 1:ncol(File)) {
      if (colnames(File)[i] == colnames(File)[j]) {
      } else {
        Sum <- (abs((File[,i])-(File[,j])))^p
        Distance = (sum(Sum))^1/p
        stockC <- c(stockC, colnames(File)[i])
        stockD <- c(stockD, colnames(File)[j])
        lp_norm <- c(lp_norm,
                     Distance)
      }
    }
  }
  DataFrame <- data.frame(stockC, stockD, lp_norm)
  DataFrame <- na.omit(DataFrame)
  DataFrame <- DataFrame[-seq(0,nrow(DataFrame),2),]
  DataFrame <- DataFrame[order(DataFrame$lp_norm),]
}
#a lp-norm for p= 1
lp1 <- lp_norm(dataset1,1)
head(lp1,10)
tail(lp1,10)
summary(lp1)

#b) 𝐿𝑝-norm for 𝑝 = 2
lp2 <- lp_norm(dataset1,2)
head(lp2,10)
tail(lp2,10)
summary(lp2)

#c) 𝐿𝑝-norm for 𝑝 = 3

lp3 <- lp_norm(dataset1,3)
head(lp3,10)
tail(lp3,10)
summary(lp3)

#d) 𝐿𝑝-norm for 𝑝 = 10

lp10 <- lp_norm(dataset1,10)
head(lp10,10)
tail(lp10,10)
summary(lp10)


summary(c(lp1,lp2,lp3,lp10))


#e) Minkovski distance (assign different weights for the feature components in the Lp-norm
#based on your assessment on the importance of the features)


Minkovski <- function(File,a,b){
  stockC <- c()
  stockD <- c()
  lpnorm.c <- c()
  for (i in 1:ncol(File)) {
    for (j in 1:ncol(File)) {
      if (colnames(File)[i] == colnames(File)[j]) {
      } else {
        Sum <- (abs((File[,i])-(File[,j])))^a
        Distance = (b*(sum(Sum)))^1/a
        stockC <- c(stockC, colnames(File)[i])
        stockD <- c(stockD, colnames(File)[j])
        lpnorm.c <- c(lpnorm.c, Distance)
      }
    }
  }
  Dataframe <- data.frame(stockC, stockD, lpnorm.c)
  Dataframe <- na.omit(Dataframe)
  Dataframe <- Dataframe[-seq(0,nrow(Dataframe),2),]
  Dataframe <- Dataframe[order(Dataframe$lpnorm.c),]
}
Weights <- c(0.1,0.2,0.15,0.2,0.1,0.1,0.05, 0.05,0.03,0.02)
Minkovski_distance <- Minkovski(dataset1,2,Weights)
head(Minkovski_distance,10)
tail(Minkovski_distance,10)
summary(Minkovski_distance)

#f.) Match-Based Similarity Computation
Match_Based <-function(r,k){
  stockC <- c()
  stockD <- c()
  match<- c()
  for (i in 1:ncol(r)) {
    
    for (j in 1:ncol(r)) {
      if (colnames(r)[i] == colnames(r)[j]) {
        next
      } else {
        m <-
          
          max(as.numeric(as.character(r[,i])),as.numeric(as.character(r[,i])))
        n <-
          
          min(as.numeric(as.character(r[,j])),as.numeric(as.character(r[,j])))
        Sum <- (1-((abs((r[,i])-(r[,j])))/(m-n)))^k
        Distance = ((sum(Sum)))^1/k
        stockC <- c(stockC, colnames(r)[i])
        stockD <- c(stockD, colnames(r)[j])
        match<- c(match, Distance)
      }
    }
  }
  Dataframe <- data.frame(stockC, stockD, match)
  Dataframe <- na.omit(Dataframe)
  Dataframe <- Dataframe[-seq(0,nrow(Dataframe),2),]
  Dataframe <- Dataframe[order(Dataframe$match),]
}
Match.Based_Comput <- Match_Based(dataset1,1)
head(Match.Based_Comput,10)
tail(Match.Based_Comput,10)
summary(Match.Based_Comput)

#g.) Mahalanobis distance calculation
library(StatMatch)
options(warn=-1)
Mahalanobis_Distance <- function(File){
  stockC <- c()
  stockD <- c()
  Mahalanobis_Distance <- c()
  for (i in 1:ncol(File)) {
    for (j in 1:ncol(File)) {
      if (colnames(File)[i] == colnames(File)[j]) {
      } else {
        Distance <- sum(mahalanobis.dist(File[,i], File[,j], vc =
                                           cov(File[,i], File[,j])))
        stockC <- c(stockC, colnames(File)[i])
        stockD <- c(stockD, colnames(File)[j])
        Mahalanobis_Distance <- c(Mahalanobis_Distance, Distance)}}
  }
  Dataframe <- data.frame(stockC, stockD, Mahalanobis_Distance)
  Dataframe<- na.omit(Dataframe)
  Dataframe<- Dataframe[-seq(0,nrow(Dataframe),2),]
  Dataframe<- Dataframe[order(Dataframe$Mahalanobis_Distance),]
}
result <- Mahalanobis_Distance(dataset1)
head(result,10)
tail(result,10)
summary(result)


## Now for the similarity calculation, we will be loading the categorical data
## i.e. basically securities.csv
filename <- file.choose()

SECS <- read.csv(filename, stringsAsFactors = F)
TK_Symbol <- c(dataset_fundamentals$Ticker.Symbol)
SECS <- SECS[SECS$Ticker.symbol %in% TK_Symbol,]
SECS <- SECS[-c(2,3,7,8)]

SECS$Sector <- as.numeric(factor(SECS$GICS.Sector))

SECS$Industry <- as.numeric(factor(SECS$GICS.Sub.Industry))
# Removing the columns that contains character data
SECS <- SECS[-c(2,3,4)]
# Transposing it to arrange
SECS <- as.data.frame(t(SECS))
# giving coloumn the names
colnames(SECS) <- as.character(unlist(SECS[1,]))
SECS <- SECS[-1, ]
head(SECS)

# h) Similarity: overlap measure

Overlap_Measure <- function(x){
  k <- 0
  stock1 <- c()
  stock2 <- c()
  similarity <- c()
  for (i in 1:ncol(x)) {
    for (j in 1:ncol(x)) {
      if (colnames(x)[i] == colnames(x)[j]) {
        next
      } else {
        for (count1 in 1:nrow(x)) {
          if (identical(x[count1,i], x[count1,j])) {
            k <- k + 1}
        }
        stock1 <- c(stock1, colnames(x)[i])
        stock2 <- c(stock2, colnames(x)[j])
        similarity <- c(similarity, k)}}
  }
  Dataframe <- data.frame(stock1, stock2, similarity)
  colnames(Dataframe) <- c("stock1", "stock2", "Overlaps")
  Dataframe <- na.omit(Dataframe)
  Dataframe <- Dataframe[-seq(0,nrow(Dataframe),2),]
  Dataframe <- Dataframe[order(Dataframe$Overlap),]
}
Overlap <- Overlap_Measure(SECS)
head(Overlap,10)
tail(Overlap,10)
summary(Overlap)

#i) Similarity: inverse frequency

inverse_frequency_similarity <- function(data) {
  num_rows <- nrow(data)
  num_columns <- ncol(data)
  
  inv_freq_sim <- matrix(0, nrow = num_rows, ncol = num_rows)
  
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      num_common_terms <- sum(data[i, ] == data[j, ])
      inv_freq_sim[i, j] <- num_common_terms / num_columns
    }
  }
  
  return(inv_freq_sim)
}


inverse_frequency_similarity(SECS)

#J) Similarity: Goodall

library("nomclust")

goodall_similarity <- function(data) {
  num_rows <- nrow(data)
  num_columns <- ncol(data)
  
  # Calculate Goodall similarity
  goodall_sim <- matrix(0, nrow = num_rows, ncol = num_rows)
  
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      num_common_terms <- sum(data[i, ] == data[j, ])
      goodall_sim[i, j] <- 2 * num_common_terms / (num_columns + num_common_terms)
    }
  }
  
  return(goodall_sim)
}


goodall_similarity(SECS)

#k) Overall similarity between tickers by using mixed type data (choose a 𝜆 value for
#calculation)


custom_mixed_similarity <- function(data, lambda) {
  num_rows <- nrow(data)
  
  # Calculate mixed type similarity with custom lambda
  mixed_sim <- matrix(0, nrow = num_rows, ncol = num_rows)
  
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      num_common_terms <- sum(data[i, ] == data[j, ])
      mixed_sim[i, j] <- (1 - lambda) * num_common_terms / num_rows + lambda * (1 - num_common_terms / num_rows)
    }
  }
  
  return(mixed_sim)
}


custom_mixed_similarity(SECS, .75)

#l) Overall normalized similarity between tickers by using mixed type data (choose a 𝜆
#value for calculation)

calculate_custom_normalized_similarity <- function(data, lambda) {
  # Calculate the mixed type similarity
  num_rows <- nrow(data)
  mixed_sim <- matrix(0, nrow = num_rows, ncol = num_rows)
  
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      num_common_terms <- sum(data[i, ] == data[j, ])
      mixed_sim[i, j] <- (1 - lambda) * num_common_terms / num_rows + lambda * (1 - num_common_terms / num_rows)
    }
  }
  
  # Normalize the similarity matrix
  normalized_sim <- matrix(0, nrow = num_rows, ncol = num_rows)
  
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      max_value <- max(mixed_sim[i, ])
      min_value <- min(mixed_sim[i, ])
      normalized_sim[i, j] <- (mixed_sim[i, j] - min_value) / (max_value - min_value)
    }
  }
  
  return(normalized_sim)
}
normalized_similarity <- calculate_custom_normalized_similarity(SECS,0.75)
print(normalized_similarity)

