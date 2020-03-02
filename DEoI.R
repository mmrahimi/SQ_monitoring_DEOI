#### Detecing Events of Interest for Event Detection in transport nodes
# Created by Masoud Rahimi 10/11/2019
# For JOSIS special issue on Geocomputation

#### Instructions
# 1. Download and unzip the pack
# 2. Install the required packages
# 3. Run the code. It will take around 20 minutes

#### Setup ####
cat("\014")
rm(list=ls())
options(warn=-1)
dev.off()
dev.new()

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh(c("trinker/stansent", "trinker/sentimentr"))

pacman::p_load(dplyr)
library(dplyr); library(magrittr)
combined_emoji <- update_polarity_table(
  lexicon::hash_sentiment_jockers_rinker,
  x = lexicon::hash_sentiment_emojis
)

library(fitdistrplus)
library(AnomalyDetection)
library(aplpack)
library(tidyverse)
library(anomalize)
library(xlsx)
library(reticulate)
library(fBasics)
library(keras)
library(tsoutliers)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = "./Classes", full.names= TRUE)
sapply(files.sources, source)

#### Fixed Parameters ####
ALPHA<-0.05

#### Loading Data ####

lines <- read.csv(file="./Data/SouthernCross.csv", header=FALSE, sep=",", encoding = "UTF-8")
aspects <- read.csv(file="./Data/ids.csv", header=TRUE, sep=",")
aspects$Aspects <- vapply(aspects$Aspects, paste, collapse = ", ", character(1L))
aspects$ID <- vapply(aspects$ID, paste, collapse = ", ", character(1L))

classes <- vector(mode="list", length=length(aspects$Aspects))
names(classes) <- aspects$Aspects

for (i in 1:length(lines$V4))
{
  class_split <- unlist(strsplit(as.character(lines$V4[i]),",", fixed=TRUE))
  for (j in 1:length(class_split)){
    if (as.numeric(class_split[j])>=0){
      classes[[as.numeric(class_split[j])+1]] <- c(classes[[as.numeric(class_split[j])+1]], i)
    }
  }
}

tweets <- vector(mode="list", length=length(aspects$Aspects))
names(tweets) <- aspects$Aspects

dt <- vector(mode="list", length=length(aspects$Aspects))
names(dt) <- aspects$Aspects

for (i in 1:length(aspects$Aspects))
{
  ids = classes[[i]]
  tweets[[i]] <- as.character(lines$V3[ids])
  dt[[i]] <- parsedate::parse_date(lines$V2[ids])
}

#### Sentiment Analysis for each Aspect ####
sentiments_sentr <- vector(mode="list", length=length(aspects$Aspects))
names(sentiments_sentr) <- aspects$Aspects

for (i in 1:length(tweets)){
  texts = replace_emoji_identifier(tweets[[i]])
  dates = dt[[i]]
  
  # SentimentR for each day
  dates = format(dates,'%Y-%m-%d')
  texts_sentences <- (get_sentences(texts))
  
  df_sr = data.frame(dt = dates,txt=texts, sentiment = sentiment_by(texts_sentences, polarity_dt = combined_emoji, averaging.function = average_weighted_mixed_sentiment))
  
  (sentiments_sentr[[i]] <- with(
    df_sr, 
    sentiment_by(
      texts, 
      list(dates)
    )
  ))
}



#### Daily Analyses ####
#### Calculating Sentiments for each day ####
df_Frequency <- vector(mode="list", length=length(aspects$Aspects))
names(df_Frequency) <- aspects$Aspects

df_Sentiments_sr <- vector(mode="list", length=length(aspects$Aspects))
names(df_Sentiments_sr) <- aspects$Aspects

uniqueDates <- vector(mode="list", length=length(aspects$Aspects))
names(uniqueDates) <- aspects$Aspects

for (i in 1:length(tweets)){
  texts = tweets[[i]]
  dates = format(dt[[i]],'%Y-%m-%d')
  # df = data.frame(dt = dates, syuzhet = sentiments_syuzhet[[i]], bing = sentiments_bing[[i]], afinn = sentiments_afinn[[i]], nrc =  sentiments_nrc[[i]])
  uniqueDates[[i]] = unique(dates)
  freq<- array(rep(NaN, length(uniqueDates[[i]])*3*1),dim = c(length(uniqueDates[[i]]),3,1))
  polarity <- array(rep(NaN, length(uniqueDates[[i]])*5*3),dim = c(length(uniqueDates[[i]]),5,3))
  
  df = data.frame(dt = dates, text = texts)
  
  for (j in 1:length(uniqueDates[[i]])){
    dfTemp <- df[df$dt == uniqueDates[[i]][j],]
    df_sr = sentiments_sentr[[i]]
    dfTemp_sr <- df_sr[df_sr$dates == uniqueDates[[i]][j],]
    dfTemp_sr_Negative <- dfTemp_sr[dfTemp_sr$ave_sentiment<0,]
    dfTemp_sr_Positive <- dfTemp_sr[dfTemp_sr$ave_sentiment>0,]

    freq[j,,] <- unlist(count(dfTemp))
    polarity[j,1,] = unlist(c(sum(dfTemp_sr_Negative$ave_sentiment),sum(dfTemp_sr_Positive$ave_sentiment),sum(dfTemp_sr$ave_sentiment)))
  }
  
  df_Frequency[[i]] = data.frame(dt=uniqueDates[[i]],o=freq[,1,1])
  df_Sentiments_sr[[i]] = data.frame(dt=uniqueDates[[i]],n=polarity[,1,1],p=polarity[,1,2],o=df_sr$ave_sentiment)

}

barpos<-barplot(df_Sentiments_sr[[4]]$o,
                main="Overall Sentiments Per Day For 'Service Reliability' Class",
                ylim = range(pretty(c(-1,1))),
                xlab = "Time",
                ylab="",
                border="grey",
                col="black",
                las=2,
                mgp=c(4,1,0)
)
axis(1, at=barpos[seq(1, 297, by=5)], labels=(df_Sentiments_sr[[4]]$dt[seq(1, 297, by=5)]), las=2, cex.axis=0.6)
title(ylab = "Overall Sentiment", mgp = c(2.5, 1, 0))




#### Frequency and Sentiment-based Event Detection (FSED) ####
## (Fitting Distribution Functions + Finding Candidate Events)
outlier_dFun_sr_p <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_dFun_sr_p) <- aspects$Aspects

outlier_dFun_sr_n <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_dFun_sr_n) <- aspects$Aspects

outlier_dFun_sr_o <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_dFun_sr_o) <- aspects$Aspects


for (i in 1:length(tweets)){

    ####### FREQUENCY
  freq <- df_Frequency[[i]]$o
  
  # hist(freq)
  # descdist(freq, discrete=TRUE, boot=500)
  
  discrete_dists = c('geom','pois','binom','nbinom')
  dist_list <- vector(mode="list", length=4)
  names(dist_list) <- discrete_dists
  
  tryCatch(
    expr = {
      fit.geom <- fitdist(freq, "geom")
      dist_list['geom'] = summary(fit.geom)$loglik
    },
    error = function(e){ 
      print('GEOM not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.poi  <- fitdist(freq, "pois")
      dist_list['poi'] = summary(fit.poi)$loglik
    },
    error = function(e){ 
      print('POI not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.b  <- fitdist(freq, "binom",fix.arg=list(size=length(freq)),start=list(prob=0.3))
      dist_list['binom'] = summary(fit.b)$loglik
    },
    error = function(e){ 
      print('BINOM not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.nb <- fitdist(freq,"nbinom")
      dist_list['nbinom'] = summary(fit.nb)$loglik
    },
    error = function(e){ 
      print('Nbinom not applicable')
    }
  )
  
  
  max_loglik <- -100000000
  max_name <- 0
  for (j in 1:length(dist_list)){
    if (!is.null(dist_list[[j]])) { 
      if (dist_list[[j]]>max_loglik) {
        max_loglik <- dist_list[[j]]
        max_name <- names(dist_list[j])
      }  
    }
  }
  
  switch(max_name,
         geom={
           # plot(fit.geom)
           pFun_Frequency <- pgeom(freq,prob =fit.geom$estimate, lower.tail = FALSE, log.p = FALSE)
         },
         pois={
           # plot(fit.poi)
           pFun_Frequency <- ppois(freq,lambda = fit.poi$estimate['lambda'], lower.tail = FALSE, log.p = FALSE)
         },
         binom={
           # plot(fit.b)
           pFun_Frequency <- pbinom(freq, size = fit.b$fix.arg$size, prob = fit.b$estimate[1], lower.tail = FALSE)
         },
         nbinom={
           # plot(fit.nb)
           pFun_Frequency <- pnbinom(freq,size=fit.nb$estimate['size'],mu = fit.nb$estimate['mu'], lower.tail = FALSE)
         })
  
  
  
  ####### SENTIMENTS
  sr_p <- abs(df_Sentiments_sr[[i]]$p)
  sr_n <- abs(df_Sentiments_sr[[i]]$n)
  sr_o <- abs(df_Sentiments_sr[[i]]$o)
  sr_mean <- abs(sentiments_sentr[[i]]$ave_sentiment)
  
  # descdist(sr_p, discrete=FALSE, boot=500)
  # descdist(sr_n, discrete=FALSE, boot=500)
  # descdist(sr_o, discrete=FALSE, boot=500)
  # descdist(sr_mean, discrete=FALSE, boot=500)
  
  ## Positive
  
  discrete_dists = c('exp','gamma','lnorm','beta','weibull')
  dist_list <- vector(mode="list", length=5)
  names(dist_list) <- discrete_dists
  
  tryCatch(
    expr = {
      fit.exp  <- fitdist(sr_p+0.00001, "exp")
      dist_list['exp'] = summary(fit.exp)$loglik
    },
    error = function(e){ 
      print('Exp not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.g  <- fitdist(sr_p+0.00001, "gamma")
      dist_list['gamma'] = summary(fit.g)$loglik
    },
    error = function(e){ 
      print('Gamma not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.ln <- fitdist(sr_p+0.00001, "lnorm")
      dist_list['lnorm'] = summary(fit.ln)$loglik
    },
    error = function(e){ 
      print('LN not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.beta <- fitdist(sr_p+0.00001, "beta")
      dist_list['beta'] = summary(fit.beta)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.w  <- fitdist(sr_p+0.00001, "weibull")
      dist_list['weibull'] = summary(fit.w)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  
  max_loglik <- 0
  max_name <- 0
  for (j in 1:length(dist_list)){
    if (!is.null(dist_list[[j]])) { 
      if (dist_list[[j]]>max_loglik) {
        max_loglik <- dist_list[[j]]
        max_name <- names(dist_list[j])
      }  
    }
  }
    
  switch(max_name,
         exp={
           # plot(fit.exp)
           pFun_Sent_sr_p <- pexp(sr_p, rate = fit.exp$estimate['rate'],lower.tail = FALSE)
         },
         lnorm={
           # plot(fit.ln)
           pFun_Sent_sr_p <- plnorm(sr_p, meanlog = summary(fit.ln)$estimate[1], sdlog = summary(fit.ln)$estimate[2], lower.tail = FALSE)
         },
         gamma={
           # plot(fit.g)
           pFun_Sent_sr_p <- pgamma(sr_p, shape=fit.g$estimate['shape'], scale = 1/fit.g$estimate['rate'], lower.tail = FALSE, log.p = FALSE)
         },
         beta={
           # plot(fit.beta)
           pFun_Sent_sr_p <- pbeta(sr_p, shape1=fit.beta$estimate[1], shape2=fit.beta$estimate[2], lower.tail = FALSE, log.p = FALSE)
         },
         weibull={
           # plot(fit.w)
           pFun_Sent_sr_p <- pweibull(sr_p, shape=fit.w$estimate['shape'], scale = fit.g$estimate['scale'], lower.tail = FALSE, log.p = FALSE)
         })
  
  
  # negative
  
  discrete_dists = c('exp','gamma','lnorm','beta','weibull')
  dist_list <- vector(mode="list", length=5)
  names(dist_list) <- discrete_dists
  
  tryCatch(
    expr = {
      fit.exp  <- fitdist(sr_n+0.00001, "exp")
      dist_list['exp'] = summary(fit.exp)$loglik
    },
    error = function(e){ 
      print('Exp not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.g  <- fitdist(sr_n+0.00001, "gamma")
      dist_list['gamma'] = summary(fit.g)$loglik
    },
    error = function(e){ 
      print('Gamma not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.ln <- fitdist(sr_n+0.00001, "lnorm")
      dist_list['lnorm'] = summary(fit.ln)$loglik
    },
    error = function(e){ 
      print('LN not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.beta <- fitdist(sr_n+0.00001, "beta")
      dist_list['beta'] = summary(fit.beta)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.w  <- fitdist(sr_n+0.00001, "weibull")
      dist_list['weibull'] = summary(fit.w)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  
  max_loglik <- 0
  max_name <- 0
  for (j in 1:length(dist_list)){
    if (!is.null(dist_list[[j]])) { 
      if (dist_list[[j]]>max_loglik) {
        max_loglik <- dist_list[[j]]
        max_name <- names(dist_list[j])
      }  
    }
  }
  
  switch(max_name,
         exp={
           # plot(fit.exp)
           pFun_Sent_sr_n <- pexp(sr_n, rate = fit.exp$estimate['rate'],lower.tail = FALSE)
         },
         lnorm={
           # plot(fit.ln)
           pFun_Sent_sr_n <- plnorm(sr_n, meanlog = summary(fit.ln)$estimate[1], sdlog = summary(fit.ln)$estimate[2], lower.tail = FALSE)
         },
         gamma={
           # plot(fit.g)
           pFun_Sent_sr_n <- pgamma(sr_n, shape=fit.g$estimate['shape'], scale = 1/fit.g$estimate['rate'], lower.tail = FALSE, log.p = FALSE)
         },
         beta={
           # plot(fit.beta)
           pFun_Sent_sr_n <- pbeta(sr_n, shape1=fit.beta$estimate[1], shape2=fit.beta$estimate[2], lower.tail = FALSE, log.p = FALSE)
         },
         weibull={
           # plot(fit.w)
           pFun_Sent_sr_n <- pweibull(sr_n, shape=fit.w$estimate['shape'], scale = fit.g$estimate['scale'], lower.tail = FALSE, log.p = FALSE)
         })
  
  
  
  
  # polarity

  discrete_dists = c('exp','gamma','lnorm','beta','weibull')
  dist_list <- vector(mode="list", length=5)
  names(dist_list) <- discrete_dists
  
  tryCatch(
    expr = {
      fit.exp  <- fitdist(sr_o+0.00001, "exp")
      dist_list['exp'] = summary(fit.exp)$loglik
    },
    error = function(e){ 
      print('Exp not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.g  <- fitdist(sr_o+0.00001, "gamma")
      dist_list['gamma'] = summary(fit.g)$loglik
    },
    error = function(e){ 
      print('Gamma not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.ln <- fitdist(sr_o+0.00001, "lnorm")
      dist_list['lnorm'] = summary(fit.ln)$loglik
    },
    error = function(e){ 
      print('LN not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.beta <- fitdist(sr_o+0.00001, "beta")
      dist_list['beta'] = summary(fit.beta)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.w  <- fitdist(sr_o+0.00001, "weibull")
      dist_list['weibull'] = summary(fit.w)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  
  max_loglik <- 0
  max_name <- 0
  for (j in 1:length(dist_list)){
    if (!is.null(dist_list[[j]])) { 
      if (dist_list[[j]]>max_loglik) {
        max_loglik <- dist_list[[j]]
        max_name <- names(dist_list[j])
      }  
    }
  }
  
  switch(max_name,
         exp={
           # plot(fit.exp)
           pFun_Sent_sr_o <- pexp(sr_o, rate = fit.exp$estimate['rate'],lower.tail = FALSE)
         },
         lnorm={
           # plot(fit.ln)
           pFun_Sent_sr_o <- plnorm(sr_o, meanlog = summary(fit.ln)$estimate[1], sdlog = summary(fit.ln)$estimate[2], lower.tail = FALSE)
         },
         gamma={
           # plot(fit.g)
           pFun_Sent_sr_o <- pgamma(sr_o, shape=fit.g$estimate['shape'], scale = 1/fit.g$estimate['rate'], lower.tail = FALSE, log.p = FALSE)
         },
         beta={
           # plot(fit.beta)
           pFun_Sent_sr_o <- pbeta(sr_o, shape1=fit.beta$estimate[1], shape2=fit.beta$estimate[2], lower.tail = FALSE, log.p = FALSE)
         },
         weibull={
           # plot(fit.w)
           pFun_Sent_sr_o <- pweibull(sr_o, shape=fit.w$estimate['shape'], scale = fit.g$estimate['scale'], lower.tail = FALSE, log.p = FALSE)
         })
  
  
  
  # mean

  discrete_dists = c('exp','gamma','lnorm','beta','weibull')
  dist_list <- vector(mode="list", length=5)
  names(dist_list) <- discrete_dists
  
  tryCatch(
    expr = {
      fit.exp  <- fitdist(sr_mean+0.00001, "exp")
      dist_list['exp'] = summary(fit.exp)$loglik
    },
    error = function(e){ 
      print('Exp not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.g  <- fitdist(sr_mean+0.00001, "gamma")
      dist_list['gamma'] = summary(fit.g)$loglik
    },
    error = function(e){ 
      print('Gamma not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.ln <- fitdist(sr_mean+0.00001, "lnorm")
      dist_list['lnorm'] = summary(fit.ln)$loglik
    },
    error = function(e){ 
      print('LN not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.beta <- fitdist(sr_mean+0.00001, "beta")
      dist_list['beta'] = summary(fit.beta)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  tryCatch(
    expr = {
      fit.w  <- fitdist(sr_mean+0.00001, "weibull")
      dist_list['weibull'] = summary(fit.w)$loglik
    },
    error = function(e){ 
      print('Beta not applicable')
    }
  )
  
  max_loglik <- 0
  max_name <- 0
  for (j in 1:length(dist_list)){
    if (!is.null(dist_list[[j]])) { 
      if (dist_list[[j]]>max_loglik) {
        max_loglik <- dist_list[[j]]
        max_name <- names(dist_list[j])
      }  
    }
  }
  
  switch(max_name,
         exp={
           # plot(fit.exp)
           pFun_Sent_sr_mean <- pexp(sr_mean, rate = fit.exp$estimate['rate'],lower.tail = FALSE)
         },
         lnorm={
           # plot(fit.ln)
           pFun_Sent_sr_mean <- plnorm(sr_mean, meanlog = summary(fit.ln)$estimate[1], sdlog = summary(fit.ln)$estimate[2], lower.tail = FALSE)
         },
         gamma={
           # plot(fit.g)
           pFun_Sent_sr_mean <- pgamma(sr_mean, shape=fit.g$estimate['shape'], scale = 1/fit.g$estimate['rate'], lower.tail = FALSE, log.p = FALSE)
         },
         beta={
           # plot(fit.beta)
           pFun_Sent_sr_mean <- pbeta(sr_mean, shape1=fit.beta$estimate[1], shape2=fit.beta$estimate[2], lower.tail = FALSE, log.p = FALSE)
         },
         weibull={
           # plot(fit.w)
           pFun_Sent_sr_mean <- pweibull(sr_mean, shape=fit.w$estimate['shape'], scale = fit.g$estimate['scale'], lower.tail = FALSE, log.p = FALSE)
         })
  
  
  
  weights_sr_p <- pFun_Sent_sr_p * pFun_Frequency
  weights_sr_n <- pFun_Sent_sr_n * pFun_Frequency
  weights_sr_o <- pFun_Sent_sr_o * pFun_Frequency
  weights_sr_mean <- pFun_Sent_sr_mean * pFun_Frequency
  
  outlier_dFun_sr_p[[i]] <- which(weights_sr_p<ALPHA)
  outlier_dFun_sr_n[[i]] <- which(weights_sr_n<ALPHA)
  outlier_dFun_sr_o[[i]] <- which(weights_sr_o<ALPHA)
  
}




#### Time-series analysis ####
#### Bivariate Outlier Detection - Finding bivariate outliers -- bagplot ####
outlier_bag_sr_p <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_bag_sr_p) <- aspects$Aspects

outlier_bag_sr_n <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_bag_sr_n) <- aspects$Aspects

outlier_bag_sr_o <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_bag_sr_o) <- aspects$Aspects

for (i in 1:length(tweets)){

aplDF<- data.frame(x = df_Frequency[[i]]$o, y = abs(df_Sentiments_sr[[i]]$o))
outlier_bag_sr_o[[i]] <- calc_bagplot(aplDF)

aplDF<- data.frame(x = df_Frequency[[i]]$o, y = abs(df_Sentiments_sr[[i]]$p))
outlier_bag_sr_p[[i]] <- calc_bagplot(aplDF)

aplDF<- data.frame(x = df_Frequency[[i]]$o, y = abs(df_Sentiments_sr[[i]]$n))
outlier_bag_sr_n[[i]] <- calc_bagplot(aplDF)

}


#### S-H-ESD ####
outlier_shesd_sr_n <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_shesd_sr_n) <- aspects$Aspects

outlier_shesd_sr_p <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_shesd_sr_p) <- aspects$Aspects

outlier_shesd_sr_o <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_shesd_sr_o) <- aspects$Aspects

for (i in 1:length(tweets)){

outlier_shesd_sr_n[[i]] <- calc_shesd(df_Frequency[[i]]$dt,df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$n, ALPHA)
outlier_shesd_sr_p[[i]] <- calc_shesd(df_Frequency[[i]]$dt,df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$p, ALPHA)
outlier_shesd_sr_o[[i]] <- calc_shesd(df_Frequency[[i]]$dt,df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$o, ALPHA)

outlier_shesd_sr_n[[i]]$c_plot$labels$title <- "S-H-ESD on Frequency Time-Series"
}



#### tsoutliers: Detection of Outliers in Time Series ####
outlier_tso_sr_n <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_tso_sr_n) <- aspects$Aspects

outlier_tso_sr_p <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_tso_sr_p) <- aspects$Aspects

outlier_tso_sr_o <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_tso_sr_o) <- aspects$Aspects

for (i in 1:length(tweets)){

outlier_tso_sr_n[[i]] <- calc_tso (df_Frequency[[i]]$o, df_Sentiments_sr[[i]]$n)
outlier_tso_sr_p[[i]] <- calc_tso (df_Frequency[[i]]$o, df_Sentiments_sr[[i]]$p)
outlier_tso_sr_o[[i]] <- calc_tso (df_Frequency[[i]]$o, df_Sentiments_sr[[i]]$o)
}


#### Anomalize ####
outlier_anomalize_sr_n <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_anomalize_sr_n) <- aspects$Aspects

outlier_anomalize_sr_p <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_anomalize_sr_p) <- aspects$Aspects

outlier_anomalize_sr_o <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_anomalize_sr_o) <- aspects$Aspects

for (i in 1:length(tweets)){

outlier_anomalize_sr_n[[i]] <- calc_anomalize(df_Frequency[[i]]$dt,df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$n, ALPHA)
outlier_anomalize_sr_p[[i]] <- calc_anomalize(df_Frequency[[i]]$dt,df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$p, ALPHA)
outlier_anomalize_sr_o[[i]] <- calc_anomalize(df_Frequency[[i]]$dt,df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$o, ALPHA)
}


#### LSTM ####
outlier_lstm_sr_n <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_lstm_sr_n) <- aspects$Aspects

outlier_lstm_sr_p <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_lstm_sr_p) <- aspects$Aspects

outlier_lstm_sr_o <- vector(mode="list", length=length(aspects$Aspects))
names(outlier_lstm_sr_o) <- aspects$Aspects

for (i in 1:length(tweets)){

outlier_lstm_sr_n[[i]] <- calc_lstm(df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$n)
outlier_lstm_sr_p[[i]] <- calc_lstm(df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$p)
outlier_lstm_sr_o[[i]] <- calc_lstm(df_Frequency[[i]]$o,df_Sentiments_sr[[i]]$o)

}



#### Final Aggregation ####
report <- vector(mode="list", length=length(aspects$Aspects))
names(report) <- aspects$Aspects

for (j in 1:length(tweets)){

  report[[j]]<- array(rep(NaN, length(uniqueDates[[j]])*18*3),dim = c(length(uniqueDates[[j]]),18,3))
  
  for (i in 1:length(uniqueDates[[j]]))
  {
    #distribution function column
    if (i %in% outlier_dFun_sr_p[[j]])
      report[[j]][i,1,1] <- 1
    if (i %in% outlier_dFun_sr_n[[j]])
      report[[j]][i,1,2] <- 1
    if (i %in% outlier_dFun_sr_o[[j]])
      report[[j]][i,1,3] <- 1
  
    #bivariate outlier column
    if (i %in% outlier_bag_sr_p[[j]])
      report[[j]][i,2,1] <- 1
    if (i %in% outlier_bag_sr_n[[j]])
      report[[j]][i,2,2] <- 1
    if (i %in% outlier_bag_sr_o[[j]])
      report[[j]][i,2,3] <- 1

    # shesd
    tmp_A <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_p[[j]]$A)))>0)
    if (i %in% tmp_A)
      report[[j]][i,3,1] <- 1
    tmp_A <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_n[[j]]$A)))>0)
    if (i %in% tmp_A)
      report[[j]][i,3,2] <- 1
    tmp_A <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_o[[j]]$A)))>0)
    if (i %in% tmp_A)
      report[[j]][i,3,3] <- 1

    tmp_B <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_p[[j]]$B)))>0)
    if (i %in% tmp_B)
      report[[j]][i,4,1] <- 1
    tmp_B <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_n[[j]]$B)))>0)
    if (i %in% tmp_B)
      report[[j]][i,4,2] <- 1
    tmp_B <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_o[[j]]$B)))>0)
    if (i %in% tmp_B)
      report[[j]][i,4,3] <- 1

    tmp_U <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_p[[j]]$union)))>0)
    if (i %in% tmp_U)
      report[[j]][i,5,1] <- 1
    tmp_U <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_n[[j]]$union)))>0)
    if (i %in% tmp_U)
      report[[j]][i,5,2] <- 1
    tmp_U <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_o[[j]]$union)))>0)
    if (i %in% tmp_U)
      report[[j]][i,5,3] <- 1

    tmp_I <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_p[[j]]$intersect)))>0)
    if (i %in% tmp_I)
      report[[j]][i,6,1] <- 1
    tmp_I <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_n[[j]]$intersect)))>0)
    if (i %in% tmp_I)
      report[[j]][i,6,2] <- 1
    tmp_I <- which(match(df_Frequency[[j]]$dt,as.factor(asDate(outlier_shesd_sr_o[[j]]$intersect)))>0)
    if (i %in% tmp_I)
      report[[j]][i,6,3] <- 1

    #tsoutliers

    if (i %in% outlier_tso_sr_p[[j]]$A)
      report[[j]][i,7,1] <- 1
    if (i %in% outlier_tso_sr_n[[j]]$A)
      report[[j]][i,7,2] <- 1
    if (i %in% outlier_tso_sr_o[[j]]$A)
      report[[j]][i,7,3] <- 1


    if (i %in% outlier_tso_sr_p[[j]]$B)
      report[[j]][i,8,1] <- 1
    if (i %in% outlier_tso_sr_n[[j]]$B)
      report[[j]][i,8,2] <- 1
    if (i %in% outlier_tso_sr_o[[j]]$B)
      report[[j]][i,8,3] <- 1

    if (i %in% outlier_tso_sr_p[[j]]$union)
      report[[j]][i,9,1] <- 1
    if (i %in% outlier_tso_sr_n[[j]]$union)
      report[[j]][i,9,2] <- 1
    if (i %in% outlier_tso_sr_o[[j]]$union)
      report[[j]][i,9,3] <- 1

    if (i %in% outlier_tso_sr_p[[j]]$intersect)
      report[[j]][i,10,1] <- 1
    if (i %in% outlier_tso_sr_n[[j]]$intersect)
      report[[j]][i,10,2] <- 1
    if (i %in% outlier_tso_sr_o[[j]]$intersect)
      report[[j]][i,10,3] <- 1

    #anomalize

    if (i %in% outlier_anomalize_sr_p[[j]]$A)
      report[[j]][i,11,1] <- 1
    if (i %in% outlier_anomalize_sr_n[[j]]$A)
      report[[j]][i,11,2] <- 1
    if (i %in% outlier_anomalize_sr_o[[j]]$A)
      report[[j]][i,11,3] <- 1


    if (i %in% outlier_anomalize_sr_p[[j]]$B)
      report[[j]][i,12,1] <- 1
    if (i %in% outlier_anomalize_sr_n[[j]]$B)
      report[[j]][i,12,2] <- 1
    if (i %in% outlier_anomalize_sr_o[[j]]$B)
      report[[j]][i,12,3] <- 1


    if (i %in% outlier_anomalize_sr_p[[j]]$union)
      report[[j]][i,13,1] <- 1
    if (i %in% outlier_anomalize_sr_n[[j]]$union)
      report[[j]][i,13,2] <- 1
    if (i %in% outlier_anomalize_sr_o[[j]]$union)
      report[[j]][i,13,3] <- 1


    if (i %in% outlier_anomalize_sr_p[[j]]$intersect)
      report[[j]][i,14,1] <- 1
    if (i %in% outlier_anomalize_sr_n[[j]]$intersect)
      report[[j]][i,14,2] <- 1
    if (i %in% outlier_anomalize_sr_o[[j]]$intersect)
      report[[j]][i,14,3] <- 1

    #lstm

    if (i %in% outlier_lstm_sr_p[[j]]$A)
      report[[j]][i,15,1] <- 1
    if (i %in% outlier_lstm_sr_n[[j]]$A)
      report[[j]][i,15,2] <- 1
    if (i %in% outlier_lstm_sr_o[[j]]$A)
      report[[j]][i,15,3] <- 1


    if (i %in% outlier_lstm_sr_p[[j]]$B)
      report[[j]][i,16,1] <- 1
    if (i %in% outlier_lstm_sr_n[[j]]$B)
      report[[j]][i,16,2] <- 1
    if (i %in% outlier_lstm_sr_o[[j]]$B)
      report[[j]][i,16,3] <- 1

    if (i %in% outlier_lstm_sr_p[[j]]$union)
      report[[j]][i,17,1] <- 1
    if (i %in% outlier_lstm_sr_n[[j]]$union)
      report[[j]][i,17,2] <- 1
    if (i %in% outlier_lstm_sr_o[[j]]$union)
      report[[j]][i,17,3] <- 1

    if (i %in% outlier_lstm_sr_p[[j]]$intersect)
      report[[j]][i,18,1] <- 1
    if (i %in% outlier_lstm_sr_n[[j]]$intersect)
      report[[j]][i,18,2] <- 1
    if (i %in% outlier_lstm_sr_o[[j]]$intersect)
      report[[j]][i,18,3] <- 1
  
  }
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
for (i in 1:length(tweets))
{
  if (file.exists(paste("EXPORT-Aspect",i,".xlsx"))) {
    file.remove(paste("EXPORT-Aspect",i,".xlsx"))
  }
  
  for (j in 1:3)
  {
    m1 <- matrix(report[[i]][,,j], ncol=18, byrow=FALSE)
    d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
    df<- cbind(uniqueDates[[i]], d1)
    df2 <- df[rowSums(is.na(df[,2:19])) != ncol(df[,2:19]),]
    write.xlsx(df2,file=paste("EXPORT-Aspect",i,".xlsx"), sheetName=str(j),append = T)
  }
}


events <- read.csv(file="./Data/EVENTS.csv", header=TRUE, sep=",")
events <- cbind(ID=events[,1],DATE=substr(parsedate::parse_date(events[,2]),0,10),events[,6:12])

precisions_mat <- vector(mode="list", length=length(aspects$Aspects))
names(precisions_mat) <- aspects$Aspects

recall_mat <- vector(mode="list", length=length(aspects$Aspects))
names(recall_mat) <- aspects$Aspects

for (l in 1:length(tweets)){
  
  precisions_mat[[l]] <-matrix(0, nrow=3, ncol=18)
  recall_mat[[l]] <-matrix(0, nrow=3, ncol=18)
  
  # j --> sentiment class
  # k --> method
  # i --> date (rows)
  
  for (j in 1:3){ 
    for (k in 1:18){
      TT = 0
      TF= 0
      FT= 0
      FF=0
      for (i in 1:length(uniqueDates[[l]]))
      {
        id_in_uniqueDates = match(events$DATE[i],uniqueDates[[l]])
        if (!is.na(id_in_uniqueDates))
        {
          stat = report[[l]][id_in_uniqueDates,k,j]
          event_stat = events[i,l+2]
          if (is.na(event_stat)){
            event_stat = 0
          }
          
          if (event_stat==1 && !is.nan(stat))
            TT <- TT+1
          else if (event_stat==1 && is.nan(stat))
            TF <- TF+1
          else if (event_stat==0 && !is.nan(stat)){
            FT <- FT+1}
          else if (event_stat==0 && is.nan(stat))
            FF <- FF+1
        }
      }
      
      precision <- TT/(TT+FT)
      recall <- TT/(TT+TF)
      
      precisions_mat[[l]][[j,k]] <- precision
      recall_mat[[l]][[j,k]] <- recall
    }
  }
}


if (file.exists('precision-all.xlsx')) {
  file.remove('precision-all.xlsx')
  file.remove('recall-all.xlsx')
}

for (i in 1:length(tweets))
{
  df_precision <- as.data.frame(precisions_mat[[i]])
  colnames(df_precision) <- c('FSED', 'BiVariate','SHESD-F','SHESD-S','SHESD-F&S','SHESD-F/S','TSO-F','TSO-S','TSO-F&S','TSO-F/S','Anom-F','Anom-S','Anom-F&S','Anom-F/S','LSTM-F','LSTM-S','LSTM-F&S','LSTM-F/S')
  rownames(df_precision) <- c('Negative','Positive','Overall')
  
  df_recall <- as.data.frame(recall_mat[[i]])
  colnames(df_recall) <- c('FSED', 'BiVariate','SHESD-F','SHESD-S','SHESD-F&S','SHESD-F/S','TSO-F','TSO-S','TSO-F&S','TSO-F/S','Anom-F','Anom-S','Anom-F&S','Anom-F/S','LSTM-F','LSTM-S','LSTM-F&S','LSTM-F/S')
  rownames(df_recall) <- c('Negative','Positive','Overall')
  
  write.xlsx(df_precision,file="precision-all.xlsx", sheetName=aspects$Aspects[i],append = T)
  write.xlsx(df_recall,file="recall-all.xlsx", sheetName=aspects$Aspects[i],append = T)
}

save.image("wd.RData")

