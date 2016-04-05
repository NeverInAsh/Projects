# Twitter Sentiment Analysis

catch.error = function(x)
{
  # let us create a missing value for test purpose
  y <- NA
  # Try to catch that error (NA) we just created
  catch_error <- tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y <- tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

cleanTweets<- function(tweet,fun=catch.error){
  # Clean the tweet for sentiment analysis
  # remove html links, which are not required for sentiment analysis
  tweet <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"," ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet <- gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet <- gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet <- gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet <- gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet <- gsub("[ \t]{2,}", " ", tweet)
  tweet <- gsub("^\\s+|\\s+$", "", tweet)
  # if anything else, you feel, should be removed, you can.
  #For example slang words etc using the above function and methods.
  # Next we'll convert all the word in lower case.
  #This makes uniform pattern.
  tweet <- catch.error(tweet)
  tweet
}
cleanTweetsAndRemoveNAs<- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

MeruTweetsCleaned = cleanTweetsAndRemoveNAs(MeruTweets)
OlaTweetsCleaned = cleanTweetsAndRemoveNAs(OlaTweets)
TaxiForSureTweetsCleaned <-
  cleanTweetsAndRemoveNAs(TaxiForSureTweets)
UberTweetsCleaned = cleanTweetsAndRemoveNAs(UberTweets)

#Sentiment Analysis
opinion.lexicon.pos <- scan('positive-words.txt',
                            what='character', comment.char=';')
opinion.lexicon.neg<-scan("negative-words.txt",
                          what="character",comment.char=";")

#We'll add a few industry-specific and/or especially emphatic terms based on
#our requirements:
pos.words <- c(opinion.lexicon.pos,'upgrade')
neg.words <- c(opinion.lexicon.neg,'wait','waiting', 'wtf', 'cancellation')
#detach("package:dplyr", unload=TRUE)
#Sentiment Score
getSentimentScore = function(sentences,words.positive=pos.words,
                             words.negative=neg.words,.progress='none'){
  require(plyr)
  require(stringr)
  scores = laply(sentences,function(sentence, words.positive, words.negative) {
    # Let first remove the Digit, Punctuation character and Control characters:
    sentence = gsub('[[:cntrl:]]', '', 
                    gsub('[[:punct:]]', '',gsub('\\d+', '', sentence)))
    # Then lets convert all to lower sentence case:
    sentence = tolower(sentence)
    # Now lets split each sentence by the space delimiter
    words = unlist(str_split(sentence, '\\s+'))
    # Get the boolean match of each words with the positive & negative opinion-lexicon
    pos.matches = !is.na(match(words, words.positive))
    neg.matches = !is.na(match(words, words.negative))
    # Now get the score as total positive sentiment minus the total negatives
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  # Return a data frame with respective sentence and the score
  return(data.frame(text=sentences, score=scores))
}

MeruResult = getSentimentScore(MeruTweetsCleaned, words.positive ,words.negative)
OlaResult = getSentimentScore(OlaTweetsCleaned, words.positive ,words.negative)
TaxiForSureResult = getSentimentScore(TaxiForSureTweetsCleaned,words.positive , words.negative)
UberResult = getSentimentScore(UberTweetsCleaned, words.positive , words.negative)

#Overall Sentiment
ls<-list(Meru=MeruResult$score,Ola=OlaResult$score,TFS=TaxiForSureResult$score,Uber=UberResult$score)
sapply(ls,sum)

require(Rstem)
require(sentiment)

# classify_emotion function returns an object of class data frame 
#with seven columns (anger, disgust, fear, joy, sadness, surprise and best fit)
MeruTweetsClassEmo = classify_emotion(MeruTweetsCleaned,algorithm="bayes", prior=1.0)
OlaTweetsClassEmo = classify_emotion(OlaTweetsCleaned,algorithm="bayes", prior=1.0)
TaxiForSureTweetsClassEmo =classify_emotion(TaxiForSureTweetsCleaned, algorithm="bayes",prior=1.0)
UberTweetsClassEmo = classify_emotion(UberTweetsCleaned,algorithm="bayes", prior=1.0)

emotions<-unique(MeruTweetsClassEmo[,7]) #or any other
emotions<-emotions[!is.na(emotions)]
countofemotions<-function(emovec,emotions=emotions){
  out<-0
  for(vec in emotions){
    c<-sum(str_count(emovec[,7],vec),na.rm=T)
    out<-c(out,c)
    
  }
  c<-as.data.frame(out[-1])
  rownames(c)<-emotions
  colnames(c)<-"Count"
  
  barplot(c$Count,names.arg=emotions,legend.text=T,border=T,
          col=c("green","red","blue","grey","orange","aquamarine2"),xlab="Emotions Categories",
          ylab="No. of Tweets",main="Emotions Analysis")
  
}

#classify_polarity log classifier
MeruTweetsClassPol = classify_polarity(MeruTweetsCleaned, algorithm="bayes")
OlaTweetsClassPol = classify_polarity(OlaTweetsCleaned, algorithm="bayes")
TaxiForSureTweetsClassPol = classify_polarity(TaxiForSureTweetsCleaned, algorithm="bayes")
UberTweetsClassPol = classify_polarity(UberTweetsCleaned, algorithm="bayes")

polarity<-c("positive","negative",'neutral')

countofpolarity<-function(polvec,polarity=polarity){
  out<-0
  for(vec in polarity){
    c<-sum(str_count(polvec[,4],vec),na.rm=T)
    out<-c(out,c)
    
  }
  c<-as.data.frame(out[-1])
  rownames(c)<-polarity
  colnames(c)<-"Count"
  barplot(c$Count,names.arg=polarity,legend.text=T,border=T,
          col=terrain.colors(3),xlab="Polarity Categories",
          ylab="No. of Tweets",main="Polarity Analysis")
  
}


