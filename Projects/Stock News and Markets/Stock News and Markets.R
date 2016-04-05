#We go for a sentiment analysis on News realted to KALINDEE Railways
#Parsing News
#setInternet2()
library("XML")
library("stringr")
url <- "http://economictimes.indiatimes.com/kalindee-rail-nirman-%28engineers%29-ltd/stocksupdate/companyid-9110.cms"
#On analysis of web page with Firebug(A Mozilla Extension) we go ahead
#with creating a handler function which only selects div with class="eachStory"
#h1 <- list(
 # startElement <- function(node){
  #  name <- xmlName(node)
   # class_value <- xmlGetAttr(node, "class")
    #if(name=="script"){NULL}
    #else{node}
  #}
#)
parsed_kalindee <- htmlParse(file = url)

#We note that the financial articles have been divided into News, 
#Announcement and Recos.Among which only News carry links

#corpus.df is a data frame with Stock name, Type of source,
#Date and Time of article posted
numrows <- xpathSApply(parsed_kalindee, 
                       path="count(//div[@class='eachStory'])")
corpus.df <- as.data.frame(matrix(data = NA, nrow = numrows, ncol=4))
colnames(corpus.df) <- c("Stock", "DandT", "secCategory", "Corpus")

stock_name <- xpathSApply(parsed_kalindee,path="//section[@id='pageContent']/h1",xmlValue)
name <- rep(stock_name,numrows)
#Now we parse date and time of the news, so that it becomes easy to 
#measure the stock returns as a function of news sentiments
#Granger Causality test
date <- xpathSApply(parsed_kalindee, path="//div[@class='storyDate']/time",
                    fun=xmlValue)

secCategory <- xpathSApply(parsed_kalindee, 
                           path="//span[@class='secCategory']", 
                           fun=xmlValue)

# Cleaning secCategory
#library(stringr)
secCategory <- as.character(str_extract_all(secCategory,"[:alpha:]+"))

corpus.df$Stock <- name
corpus.df$DandT <- date #Note Date and time are not in proper format
corpus.df$secCategory <- secCategory
#Formalising The data collection procedure
#1)
#Extracting Text from the <h3> and <p> tags and date form <time> tag
#Now to deal with the children of XML node we use xPathSApply
#Which use XML path language to deal with nodes and its children of a XML path
#Note that these links are only for News items listed on economic times
#For announcement and recos we have to do different parsing
links<-xpathSApply(parsed_kalindee, path="//div[@class='eachStory']/h3/a",
                   fun=xmlGetAttr, "href")
#Appending "http://economictimes.indiatimes.com" to create links to read the full
#article
#install.packages("stringr")
#We use lapply where each new link is a seperate element of a list
#library(stringr)
links_f <- lapply(links,
                  FUN = function(x){
                    str_c("http://economictimes.indiatimes.com",
                          x,collapse = NULL)})
parsed_kalindee_articles <- sapply(links_f, function(x){htmlParse(x)})
#Creating corpus for announ_recos
#announ_recos_corpus <- xpathSApply(parsed_kalindee, 
 #                                  path="//div[@class='eachStory']//p", 
  #                                 fun=xmlValue)

#combve to combine elements of vector to a single element
#Workd for numeric and character datatype
#note character is returned
combve <- function(x){
  vec <- character()
  for(n in seq_along(x)){
    vec<-str_c(vec,x[n])
  }
  vec
}
i <- 0
j <- 0#:-index for news vector
k <- 0#:-index for ann_rec vector
for(i in 1:numrows){
  if(corpus.df$secCategory[i]=='News'){
    j <- j+1
    news_s <- xpathSApply(parsed_kalindee_articles[[j]],
                          path="//div//p",xmlValue)
   
    corpus.df$Corpus[i] <- combve(news_s)
  }
  else{
    k <- k+1
    corpus.df$Corpus[i] <- xpathSApply(parsed_kalindee, path="//div[@class='eachStory']//p",
                                       fun=xmlValue)[[i]]
  }
}


#Now after creating our basic data frame we can add the opening and 
#Adjusted closing price for the stock, which gives the returns
#Now it can happen that the posted news is late due to obvious reasons
#Hence we need to observe the trend for at least 3 days 
#(prev day, pres day, next day)
#This assumption ensures that pieces of news are associated with 
#market movements that happen after they are published.

#We can obtain opening and closing price using quantmod package
library("quantmod")
getSymbols("KALINDEE.NS", src="yahoo",
                       from="2014-01-01", to=Sys.Date())
kalindee <- `KALINDEE.NS`
colnames(kalindee) <- make.names(c("Open","High","Low","Close",
                                   "Volume","Adjusted"))
#For more info go to 
#http://stackoverflow.com/questions/14760622/access-odd-named-
#object-returned-by-getsymbols
#library("stringr")
date_str <- unlist(str_replace_all(corpus.df$DandT,","," "))
corpus_date <- toupper(unlist(str_extract_all(date_str,
                                      "[[:digit:]]{2}[[:space:]][[:alpha:]]{3}[[:space:]][[:digit:]]{4}")))

corpus_date <- as.Date(corpus_date, format='%d %b %Y')
#POS: No of positive words
#NEG: No of negative words
#To adjust for the tone of editor we will [Tetlock et al.] normalize 
#the POS and NEG words
#Create a df called return.df containing columns
#Openprev|Closeprev|Openpres|Closepres|Opennext|Closenext
#Since information is absorbed almost instantly(~20min) we take 3days window
return.df <- as.data.frame(matrix(NA,nrow = numrows, ncol = 7))
colnames(return.df) <-  c("Date","Openprev","Closeprev","Openpres","Closepres",
                          "Opennext","Closenext")
return.df$Date <- corpus_date
l <- 0 #Index for dates 
m <- 0 #index for columns
for(l in 1:numrows){
  date0 <- corpus_date[l]
  for(m in 2:7){
    if(m %in% c(2,3)){date <- date0 - 1}
    else if(m %in% c(4,5)){date <- date0}
    else {date <- date0 + 1}
    if(m%%2==0){
      return.df[,m][l] <- 
        ifelse(length(as.numeric(kalindee[date]$Open)!=0),
         as.numeric(kalindee[date]$Open), NA)
    }
    else{
      return.df[,m][l] <- 
        ifelse(length(as.numeric(kalindee[date]$Adjusted)!=0),
               as.numeric(kalindee[date]$Adjusted), NA)
    }
  }
}

#Calculating Returns
return.df$Retprev <- (return.df$Closeprev - 
                        return.df$Openprev)/return.df$Openprev

return.df$Retpres <- (return.df$Closepres - 
                        return.df$Openpres)/return.df$Openpres

return.df$Retnext <- (return.df$Closenext - 
                        return.df$Opennext)/return.df$Opennext

#Sentiment Analysis-Approach 1
#Note files should be in your working directory
#added long and buy| added sell and short [common stock terms]
opinion.lexicon.pos <- scan('positive-words.txt',
                            what='character', comment.char=';')
opinion.lexicon.neg<-scan("negative-words.txt",
                          what="character",comment.char=";")

dummy <- str_split(corpus.df$Corpus," ")
#removing common prepositions, adverbs, conjuctions, pronounsetc.
#Loading the datset, Source:wordfrequency.info
common.df <- read.csv("Common.csv", stringsAsFactors = F)
common_words <- c(common.df$common_adverbs,common.df$common_prepositions,
                  common.df$common_pronouns,common.df$common_conjunctions,
                  "a","an","the","is","are","if","''","be")
common_words <- tolower(common_words)
corpus.df$Corpus <- tolower(corpus.df$Corpus)
bag.of.words <- sapply(corpus.df$Corpus, function(x){str_split(x," ")})
index <- integer(0)
for(i in 1:numrows){
  for(j in seq_along(common_words)){
    index <- c(index,which(bag.of.words[[i]]==common_words[j]))
  }
  #print(index)
  bag.of.words[[i]] <- unique(bag.of.words[[i]][-index])
  index <- integer(0)
}

#Since there is a lack of any concrete financial words database divided
#base on sentiments
#We will follow the following method:-
#1) compare returns pres to prev and next ot pres
#2) Negative == 0, Positive == 1
#The words which are there in the corpus @ a particular date and
#news shows positive return is assumed to belong to positive lexicon 
#and negative lexicon otherwise

return.df$ret1 <- return.df$Retpres - return.df$Retprev
return.df$ret2 <- return.df$Retnext - return.df$Retpres

return.df$ret1 <- sapply(return.df$ret1,function(x){ifelse(x<0|is.na(x),0,1)})
return.df$ret2 <- sapply(return.df$ret2,function(x){ifelse(x<0|is.na(x),0,1)})

#Now (ret1,ret2)==(0,1) | (ret1,ret2)==(1,1) implies postive sentiment
#(ret1,ret2)==(1,0) | (ret1,ret2)==(0,0) implies negative sentiment
sp1 <- integer(0)
sp2 <- integer(0)
sn1 <- integer(0)
sn2 <- integer(0)
for(i in 1:numrows){
  sp1 <- c(sp1,sum(c(return.df$ret1[i],return.df$ret2[i])== 
                 c(0,1)))
  sp2 <- c(sp2,sum(c(return.df$ret1[i],return.df$ret2[i])== 
                   c(1,1)))
  sn1 <- c(sn1,sum(c(return.df$ret1[i],return.df$ret2[i])== 
                     c(1,0)))
  sn2 <- c(sn2,sum(c(return.df$ret1[i],return.df$ret2[i])== 
                     c(0,0)))
}
#Creating a sentiment data frame to create a finanical lexicon
sentiment.df <- as.data.frame(matrix(NA,nrow=numrows,ncol=2))
colnames(sentiment.df)<- c("bag.of.words","Sentiment")
sentiment.df$bag.of.words <- bag.of.words
sentiment.df$Sentiment[which(sp1==2|sp2==2)] <- "P"
sentiment.df$Sentiment[which(sn1==2|sn2==2)] <- "N"

p <- unique(unlist(sentiment.df$bag.of.words[which(sentiment.df$Sentiment=="P")]))
n <- unique(unlist(sentiment.df$bag.of.words[which(sentiment.df$Sentiment=="N")]))

#Scrape the Economic Times website like this
library(XML)
scrape_extended <- function(x)
if(is.character(x)){
  url <- paste0("http://economictimes.indiatimes.com/markets/stocks/stock-quotes?ticker=",x)
  parsed_url <- htmlParse(url)
  search_link_list <- xpathSApply(parsed_url, 
                                  "//ul[@class='companyList']//li/a",
                                  xmlGetAttr,"href")
  company_page <- sapply(search_link_list,
                         function(x){paste0("http://economictimes.indiatimes.com",x)})
  
  company_page_parse <- sapply(company_page,htmlParse)
  company_news_links <- sapply(company_page_parse,
                               function(x){xpathSApply(x,"div[@class='headerText']/h2/a",xmlGetAttr,"href")})
  #continuing this way we can find corpus for companies
}

#We create a corpus for ET's Top50 Stocks
library(XML)
scrape_links <- function(){
  url<- "http://economictimes.indiatimes.com/markets/stocks/stock-quotes"
  parsed_url <- htmlParse(url)
  search_link_list <- xpathSApply(parsed_url, 
                                  "//ul[@class='companyList']//li/a",
                                  xmlGetAttr,"href")
  company_page <- sapply(search_link_list,
                         function(x){paste0("http://economictimes.indiatimes.com",x)})
  
  company_page_parse <- sapply(company_page,htmlParse)
  company_page_parse <- company_page_parse[-51]
  company_news_links <- character(0)
  for(i in seq_along(company_page_parse)){
    company_news_links <-c(company_news_links, 
                           xpathSApply(company_page_parse[[i]],"//h2/a",xmlGetAttr,"href")[1])
}
  
  article_link <- sapply(company_news_links,
                         function(x){paste0("http://economictimes.indiatimes.com",x)})
  
}
  
#Now to use NLP packages we use rJava and and JAVA running
#check java by using java -version
#install.packages("rJava")
#install.packages(c("NLP","openNLP","RWeka","qdap"))
#install.packages("magrittr")
#To use a language other than English, we can download openNLP models from a
#Repository Datacube, to install models, substituting a language code for en as appropriate

install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")

library(rJava)
library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)
#We perform tokenization:-tokenization, 
#because we are breaking up the text into units of meaning, called tokens.





















