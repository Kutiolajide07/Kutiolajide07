- 👋 Hi, I’m @Kutiolajide07
- 👀 I’m interested in ...
- 🌱 I’m currently learning ...
- 💞️ I’m looking to collaborate on ...
- 📫 How to reach me ...

<!---
Kutiolajide07/Kutiolajide07 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
rm(list = ls())

title: "Classification"
output: pdf_format

#set up libraries
install.packages("class")
library(class)

install.packages("dplyr")
library(dplyr)

install.packages("data.table")
library(data.table)

install.packages("ggplot2")
library(ggplot2)

install.packages("corrplot")
library(corrplot)

install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)
#library(MLmetrics)
install.packages("car")
library(car)

install.packages("MASS")
library(MASS)

install.packages("factoextra")
library(factoextra)

install.packages("reshape2")
library(reshape2)

install.packages("caret")
library(caret)
?caret

install.packages('tidyverse')
library(tidyverse)

install.packages("fastDummies")
library(fastDummies)

install.packages("shinydashboard")
library(shinydashboard)

install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)
 

Australia <- read.csv("australian.dat", header = FALSE, sep = " ")

View(Australia)


#clean Data

#check for missing observation
colSums(is.na(Australia))

# rename column
colnames(Australia) <- c("Male", "Age", "Debt", "BankCustomer", "EducationLevel", "Ethnicity",
                         "YearsEmployed", "PriorDefault", "Employed", "CreditSCore", "DriversLicense", "Citizen",
                         "Zipcode", "Income", "Approved")

#check data structure
str(Australia)

# dataset distribution

summary(Australia)


dim(Australia)


# Separate Categorical variables from continous columns


cont_col <- Australia[,c(2,3,7)]
cat_col <- Australia[,-c(2,3,7)]

View(cont_col)
View(cat_col)

# Inspect the distribution of the continous column
summary(cont_col)

par(mfrow = c(2,3))
for(column in 1:length(cont_col)){
  hist(cont_col[, column], main = colnames(cont_col)[column])
}


par(mfcol = c(2,3))

for(column in 1:length(cont_col)){
  hist(cont_col[, column], main = colnames(cont_col)[column])
}


# Inspect the distribution of the categorical column

View(cat_col)

summary(cat_col)

barplot(table(cat_col$Employed), main = "Barplot of Employed card holders")


pie(table(cat_col$Employed), main = "Barplot of Employed card holders")


##barplot of all categorical columns
plot(1:20)
par(mfcol = c(3,5))

for(column in 1:length(cat_col)){
  barplot(table(cat_col[, column]), main = colnames(cat_col)[column])
}


par(mfcol = c(3,3))

for(column in 1:length(cat_col)){
  barplot(table(cat_col[, column]), main = colnames(cat_col)[column])
}

#Zipcode column will not  be used for the analysis so it has to be dropped

Australia <- na.omit(Australia)
colSums(is.na(Australia))


#Remove zipcode because it will nor be used for the analysis
Australia <- Australia[,-13]

summary(Australia)

y <- Australia[, ncol(Australia)]
(table(y))     #383 not approved, 307 approved

#Look into the y variable using percentages

prop.table((table(y)))

#using different chart to show the credit card approval and not approved
pie(prop.table((table(y))))
barplot(prop.table((table(y))))
hist(prop.table((table(y))))


#checking correlation of x variables(numeric variables)
str(Australia)
num_cols <- Australia[, c(2,3, 7)]
cor(num_cols)      #output shows a symmetry matrix

#PLOT THE CORRELATION

#Convert the y column into numeric values (2, 1) and Convert the output to 0s and 1s

print(obs)

# Convert all categorical column to factor variable
Australia$Male  <- as.factor(Australia$Male)
Australia$BankCustomer <- as.factor(Australia$BankCustomer)
Australia$EducationLevel <- as.factor(Australia$EducationLevel)
Australia$Ethnicity <- as.factor(Australia$Ethnicity)
Australia$PriorDefault  <- as.factor(Australia$PriorDefault)
Australia$Employed <- as.factor(Australia$Employed)
Australia$DriversLicense <- as.factor(Australia$DriversLicense)
Australia$Citizen <- as.factor(Australia$Citizen)
Australia$Approved <- as.factor(Australia$Approved)

str(Australia)

# Drop variables with many of too many levels

Australia <- Australia %>% dplyr::select(-c(EducationLevel, Ethnicity, BankCustomer))
str(Australia)

write.csv(Australia, "Australia.csv", quote = FALSE, row.names = TRUE)

#set seed in order to have a conpact training set i.e this help achieve same result whenever we run the code

#ID here will be the sample function which will randomly select values for me

#Data Splitting
random_seed = 50  ## for reproducible results
set.seed(random_seed)
id <- sample(2, nrow(Australia), replace = TRUE, prob = c(0.70, 0.30))
train_set <- Australia[id == 1,]
test_set <- Australia[id == 2,]


# Scaling (Standardization) of variables colmuns
num_cols_train <- train_set %>% dplyr::select(c(Age, Debt, YearsEmployed))
cat_cols_train  <- train_set %>% dplyr::select(-c(Age, Debt, YearsEmployed))
mean_num <- apply(num_cols_train, 2, mean)
sd_num <- apply(num_cols_train, 2, sd)

scaled_train <- scale(num_cols_train)
scaled_train <- cbind(scaled_train, cat_cols_train)

num_cols_test <- test_set %>% dplyr::select(c(Age, Debt, YearsEmployed))
cat_cols_test  <- test_set %>% dplyr::select(-c(Age, Debt, YearsEmployed))

scaled_test <- scale(num_cols_test, center = mean_num, scale = sd_num)
scaled_test <- cbind(scaled_test, cat_cols_test)


#Logistic Regression (Generalise Linear Model)

logit_model <- glm(Approved ~., data = scaled_train, family = 'binomial')

summary(logit_model)

#Predict Using the Logistic Regression
colnames(scaled_test)
predict_LR <- predict(logit_model, scaled_test[,-ncol(scaled_test)], type = 'response')
predict_LR


#values greater than 0.5 should return 1 other wise 0
predicted_LR <- ifelse(predict_LR > 0.5,1,0)
predicted_LR


#Model Assessment through confusion matrix
Confussion_matrix <- table(matrix_predict = predicted_LR, actual = scaled_test[,ncol(scaled_test)])
Confussion_matrix


# Evaluation Metrics
#Accuracy: How often is my prediction correct, it shows the probability of that predition will be equal to true value

#percentage of model correctness
Model_Accuracy <- (92+73)/(92+10+18+73)
Model_Accuracy

85.49% is the percentage of model Accuracy.

#using caret to arrive at the confusion matrix
new_confusion_matrix <- caret::confusionMatrix(data = factor(predicted_LR), reference = scaled_test[,ncol(scaled_test)], positive = '1')
new_confusion_matrix

data.frame(values = new_confusion_matrix$byClass)
#to improve our model using step-wise regression, a model in traditional statistics called AKAIKE INFORMATION CRITERION
#AIC helps to compare 2 model and know the one that is better off
#step wise will help us disover the AIC of each factor variable, and compare which one is higher or lower

step_wise <- stepAIC(logit_model, direction = 'both', trace = FALSE)
summary(step_wise)

#the stepwise model is has reduced the variables and as such seems better than the earlier generalize model

#Stepwise Model prediction

predict_stepwise <- predict(step_wise, scaled_test[,-ncol(scaled_test)], type = 'response')
predict_stepwise

#in stepwise model with values greater than 0.5 should return 1 other wise 0
prediction_stepwise <- ifelse(predict_stepwise > 0.5, 1, 0)
prediction_stepwise

conf_stepwise <- caret::confusionMatrix(data = factor(prediction_stepwise), reference = scaled_test[,ncol(scaled_test)], positive = '1')
conf_stepwise

#the confusion matric in stepwise is marginally better than in the first matrix if we look at the positive prediction value

#variable importance
Variable_Imp <- data.frame(coef = names(logit_model$coefficients[-1]), 
                  vale = logit_model$coefficients[-1], row.names = NULL)
Variable_Imp %>%
  ggplot(aes(x = reorder(coef, abs(vale)), y = abs(vale))) +
  geom_bar(stat = "identity") +
  ylab("Importance") + xlab("Variable") +theme_bw() + coord_flip()

#comparison plot

data <- c("R", "SAS EM")
Accuracy <- c(85.49, 82.78); Recall <- c(87.95, 82.78); 
Precision <- c(80.02, 79.38)

data_frame <- data.frame(data , Accuracy, Precision,  Recall)
New_data_frame <- gather(data_frame, type, value, -data )

ggplot(New_data_frame, aes(type, value)) + 
  geom_bar(aes(fill = factor(data )), stat = "identity", position = "dodge") +
  xlab('Rstudio vs SAS EM comparison plot') + ylab('Score (%)') 


#K-MEANS-CLUSTERING
#Objective:  

#i want to know the number of observation in each cluster

cluster_scaled_data <- scale(num_cols_train)
cluster_scaled_data

# Determine optimal number of clust matches approval category
set.seed(100)

#clusters using all the observations we are likely going to have problem doing clustering with categorical variable, 
hence we use only the numerical variable which is the numeric column trained observation
also not that we are calculatinf distance, K-means will not be applicable
here, our focus is not on sensitivity but looking at the best way to put our data into groups
within sum of square = wss
it is worth noting that after the elbow, the movement seems slow and minimal as it move from one cluster to the other

fviz_nbclust(cluster_scaled_data, kmeans, method = 'wss', k.max = 20)
#optimal number of cluster according to the elbow here shows 4 as the first minimal point

# fviz_nbcluster will help select the optima number of cluster based on the criteria given to the model
fviz_nbclust(cluster_scaled_data, kmeans, method = 'silhouette')

# silhoutte is saying the optimal number of cluster is 2 

km <- kmeans(cluster_scaled_data, 2, nstart = 25)
km
#K-means clustering with 2 clusters of sizes 376, 95

#To visualize k-means
fviz_cluster(km, data = cluster_scaled_data)

table(scaled_train$Approved)
# to know if the cluster has 2 seperate classes we use faceted bar chart.

# we create a dataframe in R using data frame function
clusters <- data.frame(cluster = ifelse(km$cluster == 1, "Cluster 1", "Cluster 2"))

clusters$category <- scaled_train$Approved
clusters$category

#This has helped the cluster seperate the approved from not approved
clusters_matrix <- table(clusters$cluster, clusters$category)
clusters_matrix
#plot ggplot

melt(clusters_matrix)

reshape2::melt(clusters_matrix)

 

#ASSOCIATION RULE

market_basket <- read.csv("online_retail_2.csv", header = T)
View(market_basket)
str(market_basket)

#search for missing observation
apply(market_basket, 2, function(x) sum(is.na(x)))


info <- c()
for(row in 1:nrow(market_basket)){
  if (startsWith(market_basket['Invoice'][row,], "C")){
    info <- c(info, row)
  }
}

# drop invoice no with c

market_basket <- market_basket[-info,]
market_basket

# drop missing obervation in description column
market_basket <- subset(market_basket, !is.na(market_basket$Description))

#check for missing observation
apply(market_basket, 2, function(x) sum(is.na(x)))

#remove negative sales which is mostly likely return sales
market_basket$Qtty <- abs(market_basket$Quantity)

#drop transactions without customer ID so that SAS does give rule to Transactions without customer ID

#drop trasactions where customer ID is empty
new_market_basket <-  na.omit(market_basket)
View(new_market_basket)
write.csv(new_market_basket, "new_market_basket.csv")

#change the invoice date from numeric to date format
market_basket$InvoiceDate <- as.Date(market_basket$InvoiceDate)
market_basket$Description <- as.factor(market_basket$Description)
itemset_grouping <- plyr::ddply(market_basket, c("Invoice"), function(x) paste(x$Description, collapse = ","))
write.csv(itemset_grouping, "itemset_grouping.csv", quote = FALSE, row.names = TRUE)


# Main Association Rule

basket <- read.transactions("itemset_grouping.csv", format = 'basket', sep = ",")

summary(basket)

View(basket)

itemFrequencyPlot(basket, topN = 10, type = 'absolute', col = rainbow(5))

Market_rules <- apriori(basket, parameter = list(supp = 0.015, conf = 0.85, minlen = 2, maxlen = 3))

inspect(Market_rules)

# Sort Rules by Lift
Sorted_Lift <- sort(Market_rules, by = 'lift')
inspect(Sorted_Lift)

# Plotting the rules manually
plot(Sorted_Lift)

plot(Sorted_Lift, method = 'graph')

# Dropping the Redundant rules

Redundant_rules <- is.redundant(Sorted_Lift, measure = 'confidence')
Redundant_rules

which(Redundant_rules)

new_rules <- Sorted_Lift[Redundant_rules]
new_rules

inspect(new_rules)

View(new_rules)




#SENTIMENT ANALYSIS
# a whole lot of character encoding will be done as to put the reviews in binary form for the understanding of the PC. PC dont understand a normal english hence it speaks in bnary form,
Charater encoding is a way of converting text into binary numbers. while doing character encoding, there are often challenges that can seem like a bug
attcking the PC, this takes a whole lot of time for it to be solved, the way a pc understand a text can be refer to character encoding

pacman::p_load(tm, wordcloud2, wordcloud, syuzhet, lubridate, reshape2, scales, stringr,
               tidyverse, tidytext)


Hotel_Restrnt_Review <- read.csv('tourist_accommodation_reviews.csv', header = TRUE)
View (Hotel_Restrnt_Review)
str(Hotel_Restrnt_Review)
head(Hotel_Restrnt_Review)

#Selecting the 30 tourist reviews with the Hotel/restaurant randomly

Hotel_Restrnt_Names <- c("Vista", "Chilli Kitchen", "Black Ginger", "Beach Bar", "Blue Manao",
                         "DeDos", "Cafe Java", "Bai Toey", "The Place", "Cucina",
                         "Baan Mai", "WAI thai", "CatFish Cafe", "Crust", "Bocconcino",
                         "Butterfly Bistro", "Veranda", "Da Mario", "Wok", "Delish Cafe",
                         "Bodega & Grill", "Black Cat", "Bellini", "Chez Nicolas", "Doo Dee",
                         "Andaman Grill", "Coyote", "Ann Restaurant", "Amalfi", "Anchor Inn")

Hotel_Restrnt_df <- subset(Hotel_Restrnt_Review, Hotel_Restrnt_Review$Hotel.Restaurant.name %in% Hotel_Restrnt_Names)
View(Hotel_Restrnt_df)

Hotel_Restrnt_df <- Hotel_Restrnt_df %>% dplyr::select(-c(Review.Date, Location))
View(Hotel_Restrnt_df)

Hotel_Restrnt_df %>% 
  group_by(Hotel.Restaurant.name) %>% 
  summarise(reviews = n_distinct(ID)) %>% 
  ggplot(aes(x = reorder(Hotel.Restaurant.name, reviews), y = reviews)) +
  geom_col() +
  coord_flip() +
  xlab('Hotel and Restaurant Names') +
  ylab('Numbers of Reviews per Hotel and Restaurant') +
  ggtitle('Reviews for selected 30 Hotels')

#tospaces <- content_transformer(function(word, symbol) gsub(symbol, " ", word))


#instead of cleaning each hotel seperatly, Tring to clean all the reviews at once by replacing "\n, @, /,\, with spaces"
# this will help reduces the sentiment analysis to around 100 lines of codes, doing it seperatley can take a whole lot 
#of time and codes
# this shows the collection of all reviews from the selected 30 hotels and restaurant
Hotel.Restrnt.corpus <- iconv(Hotel_Restrnt_df$Review, to = "latin1")
Hotel.Restrnt.corpus

Hotel_Restrnt_corpus <- data.frame(id = c(1), Reviews = Hotel.Restrnt.corpus)
Hotel_Restrnt_corpus
# select hotels without missing observation

Corpus <- iconv(Hotel_Restrnt_df$Review, "UTF-8", "UTF-8", "byte")
doc_ids <- c(1)
new_corpus <- data.frame(doc_id = doc_ids, text = Corpus)
new_corpus
new_corpus <- Corpus(DataframeSource(new_corpus))
new_corpus

new_corpus <- tm_map(new_corpus, tolower)
new_corpus <- tm_map(new_corpus, removePunctuation)
new_corpus <- tm_map(new_corpus, removeNumbers)
new_corpus <- tm_map(new_corpus, removeWords, stopwords('english'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
new_corpus <- tm_map(new_corpus, content_transformer(removeURL))
new_cleaned_corpus <- tm_map(new_corpus, stripWhitespace)

inspect(new_cleaned_corpus[105])
inspect(new_cleaned_corpus[1:5])

View(Hotel_Restrnt_df)

docs <- c()
for(i in 1:nrow(Hotel_Restrnt_df)){
  docs <- c(docs, new_cleaned_corpus[[i]]$content)
}

# Getting a dataframe of the of all review clean documents

new_dataframe <- data.frame(Hotel.Restrnt = Hotel_Restrnt_df$Hotel.Restaurant.name, 
                            Review = docs)


#documents and metrics for each hotels and restaurant

Da_Mario <- subset(new_dataframe, Hotel.Restrnt == 'Da Mario')
View (Da_Mario)

#for further analysis in SAS EM
write.csv(Da_Mario, "Da_Mario.csv") 


# TDM(Term-document Matrix)

Da_Mario.tdm <- as.matrix(TermDocumentMatrix(Corpus(VectorSource(Da_Mario$Review))))
View(Da_Mario.tdm)

# Sentiment Analysis using word cloud

Da_Mario_word_cloud <- wordcloud(words = names(rowSums(Da_Mario.tdm)), freq = rowSums(Da_Mario.tdm))

set.seed(50)
wordcloud(words = names(rowSums(Da_Mario.tdm)), freq = rowSums(Da_Mario.tdm),
          random.order = F, min.freq = 5, colors = brewer.pal(8, 'Dark2'),
          scale = c(6, 0.5), rot.per = 0.5, main = "s")

Total_sentiment <- get_nrc_sentiment(Da_Mario$Review)
Total_sentiment
barplot(prop.table(colSums(Total_sentiment)), las = 2, col = rainbow(10), ylab = 'Percentage', xlab = "Da_Mario Sentiment")

Summary_sentiment <- get_nrc_sentiment(Da_Mario$Review)[, c(9,10)]
Summary_sentiment
barplot(prop.table(colSums(Summary_sentiment)), las = 2, col = rainbow(2), ylab = 'Percentage', xlab = "Da Mario summary sentiment")

overall_sentiment_plot <- round(prop.table(colSums(Summary_sentiment)),2)
overall_sentiment_plot

#Sentiment Analysis for all hotels

return_sentiment <- function(single_hotel_df){
  set.seed(1)
  par(mfrow = c(1,2))
  ## Generate a document term matrix
  tdm <- as.matrix(TermDocumentMatrix(Corpus(VectorSource(single_hotel_df$Review))))
  
  #Word Cloud
  wordcloud(words = names(rowSums(tdm)), freq = rowSums(tdm),
            random.order = F, min.freq = 5, colors = brewer.pal(8, 'Dark2'),
            scale = c(6, 0.5), rot.per = 0.5, main = "s")
  
  #Generate Positive and Negative Sentiments
  sentiment <- get_nrc_sentiment(single_hotel_df$Review)[, c(9,10)]
  barplot(prop.table(colSums(sentiment)), las = 2, 
          col = rainbow(2), 
          ylab = 'Percentage', 
          xlab = "Sentiment")
  
  overall_positive_percent <- round(prop.table(colSums(sentiment)),2)
  overall_sentiment <- paste('Positive Sentiment Percentage:', overall_positive_percent[2], "%,",
                             'Negative Sentiment Percentage:', overall_positive_percent[1], "%")
  return(overall_sentiment)
}

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Da Mario'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Vista'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'CatFish Cafe'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Bodega & Grill'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Ann Restaurant'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Chilli Kitchen'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Black Ginger'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Beach Bar'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Blue Manao'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'DeDos'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Cafe Java'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Bai Toey'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'The Place'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Cucina'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Baan Mai'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'WAI thai'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Crust'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Bocconcino'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Butterfly Bistro'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Veranda'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Wok'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Delish Cafe'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Black Cat'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Bellini'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Chez Nicolas'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Doo Dee'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Andaman Grill'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Coyote'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Amalfi'))

return_sentiment(subset(new_dataframe, Hotel.Restrnt == 'Anchor Inn'))

