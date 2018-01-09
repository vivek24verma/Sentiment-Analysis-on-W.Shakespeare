

# Sentiment Analysis

library(syuzhet)
library(ggplot2)
library("magrittr")
library("rvest")
library("NLP")
library("rlist")
library("tidyr")
library("stringr")




if(!file.exists("Books.Shakespeare")) {
  Hamlet <- "Hamlet.txt" %>% 
    get_text_as_string()
  Macbeth <- "Macbeth.txt" %>% 
    get_text_as_string()
  Othello <- "Othello.txt" %>% 
    get_text_as_string()
  RomeoJuliet <- "RomeoJuliet.txt" %>% 
    get_text_as_string()
  AnthonyCleopatra <- "AnthonyCleopatra.txt" %>%
    get_text_as_string()

  books <- list(Hamlet = Hamlet, 
                Macbeth= Macbeth, 
                Othello = Othello, 
                RomeoJuliet= RomeoJuliet, 
                AnthonyCleopatra = AnthonyCleopatra) %>% 
    
  lapply(get_sentences)
  
  save(books, file = "Books.Shakespeare")
} else {
  load("Books.Shakespeare")
}

# Multi-sentiment analysis

multi_sentiment <- function(sentences) {
  list(afinn = get_sentiment(sentences, method = "afinn"),
       nrc   = get_sentiment(sentences, method = "nrc")
  )
}

sentiment <- books %>% 
  lapply(multi_sentiment)

sentiment

# Comparison of plays in summary terms

sum_up_sentiment <- function(x) {
  apply_sentiment <- function(vec) {
    list(sum = sum(vec),
         mean = mean(vec),
         summary = summary(vec))
  }
  
  if(is.list(x))
    lapply(x, apply_sentiment)
  else
    apply_sentiment(x)
}

sentiment %>% 
  lapply(sum_up_sentiment) %>% 
  list.unzip()

# Sentiment plot

plot_sentiment <- function(x, title) {
  plot(x,
       type = "l",
       main = title,
       xlab = "Narrative time",
       ylab = "Emotion"
  )
  abline(h = 0, col = 3, lty = 2)
}
sentiment %>% 
  list.flatten() %>% 
  lapply(get_percentage_values) %>% 
  Map(plot_sentiment, ., names(.))

# NRC method

bind_pos <- function(df) {
  pos <- data.frame(position = 1:nrow(df))
  cbind(df, pos)
}

plot_nrc <- function(df, title) {
  ggplot(df,
         aes(x = position, y = value, color = emotion)) +
    geom_smooth(size = 2, se = FALSE) +
    xlab("Narrative position") +
    ylab("Prevalence") +
    theme_classic() +
    ggtitle(title)
}

books %>% 
  lapply(get_nrc_sentiment) %>% 
  lapply(bind_pos) %>% 
  lapply(gather, emotion, value, -position, -negative, -positive) %>% 
  Map(plot_nrc, ., names(.))




  