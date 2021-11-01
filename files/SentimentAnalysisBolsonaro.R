
# Carregando os pacotes
library(twitteR)
library(httr)
library(knitr)
library(rmarkdown)

library(SnowballC)
library(tm)

library(RColorBrewer)
library(wordcloud)

library(ggplot2)

library(stringr)
library(plyr)

# Carregando a biblioteca
source('utils.R')

options(warn=-1)

# Criando autenticação no Twitter
key <- "enter your key"
secret <- "enter your secret"
token <- "token"
tokensecret <- "enter your tokensecret"

# Autenticação. Responda 1 (Yes) quando perguntado sobre utilizar direct connection.
setup_twitter_oauth(key, secret, token, tokensecret)

# Coleta de tweets que mencionam Bolsonaro e Lula
tema_Bol <- "Bolsonaro"
qtd_tweets <- 10000
lingua <- "en"
tweets_Bol = searchTwitter(tema_Bol, n = qtd_tweets, lang = lingua)

# Visualizando as primeiras linhas dos objetos tweets
head(tweets_Bol)

# Tratamento (limpeza, organização e transformação) dos dados coletados
tweetlist_Bol <- sapply(tweets_Bol, function(x) x$getText())
tweetlist_Bol <- iconv(tweetlist_Bol, to = "utf-8", sub="")
tweetlist_Bol <- limpaTweets(tweetlist_Bol)
tweetcorpus_Bol <- Corpus(VectorSource(tweetlist_Bol))
tweetcorpus_Bol <- tm_map(tweetcorpus_Bol, removePunctuation)
tweetcorpus_Bol <- tm_map(tweetcorpus_Bol, content_transformer(tolower))
tweetcorpus_Bol <- tm_map(tweetcorpus_Bol, function(x)removeWords(x, stopwords()))

# Gerando uma nuvem palavras
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetcorpus_Bol, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 50, 
          random.order = F,
          colors = pal2)

# Criando uma função para avaliar o sentimento
sentimento.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  
  # Criando um array de scores com lapply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     
                     # Tratamento de Erro
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Mapeando as palavras positivas e negativas
pos = readLines("data/palavras_positivas.txt")
neg = readLines("data/palavras_negativas.txt")

# Obtendo os textos dos tweets
txt_Bol = sapply(tweets_Bol, function(x) x$getText())

# Vetor com o numero de tweets para cada politico
number_Tweet = length(txt_Bol)

# Aplicando função para calcular o score de sentimento
scores = sentimento.score(txt_Bol, pos, neg, .progress = 'text')

# Calculando o score por país
scores$txt_Bol = factor(rep("Bolsonaro", number_Tweet))
scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)
head(scores)

# Calculando o total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

# Calculando a pontuação global
global_score = round( 100 * numpos / (numpos + numneg) )
global_score

# Gerando um boxplot
boxplot(score ~ txt_Bol, data = scores)

# Gerando um histograma
ggplot(data= scores) +
  geom_histogram(aes(x=score), binwidth = 1) +
  xlab("Pontuação") +
  ylab("Número de ocorrências") +
  ggtitle("Pontuação para os políticos Bolsonaro e Lula") +
  theme_bw()

meansSentimentPolitician <- data.frame(MeanBolsonaro = mean(scores[scores[,3] == 'Bolsonaro',]$score))
meansSentimentPolitician

