library(jiebaR)
library(dplyr)
library(tidytext)
library(tibble)
library(readr)
library(tidyverse)
library(stringr)
library(anytime)
library(quanteda)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

#讀檔
ted_main <- read_csv("D:/R/ted_main.csv")
transcript <- read_csv("D:/R/tran.csv")

#合併兩個檔案+新增speech編號
ted <- inner_join(ted_main, transcript, by = "url")
id <- rep(1: nrow(transcript))
ted <- cbind(ted,speech_num = id)

#斷詞
seg <- worker()
docs_segged <- rep("", 2467)
for (i in seq_along(ted$transcript)) {
  segged <- segment(ted$transcript[i], seg)
  docs_segged[i] <- paste0(segged, collapse = " ")
}

#tidytext transcript
docs_segged_data <- tibble(speech_num = id, transcript = docs_segged)
tidy_text_tran <- docs_segged_data %>% unnest_tokens(output = "word", input = "transcript", token = "regex", pattern = " ")

#找出每篇出現laughter的次數
find_laughter <- tidy_text_tran %>% filter(word == "laughter") %>% group_by(speech_num) %>% summarise(laughter_count = n())

#有出現laughter的演說的專屬df
ted_with_laughter <- inner_join(ted, find_laughter, by = "speech_num")

#取出funny的rating數
seg <- worker() 

funny<- rep("", length(ted_with_laughter))
for (i in seq_along(ted_with_laughter$ratings)) {
  segged_funny_rate <- segment(ted_with_laughter$ratings[i], seg)
  funny[i] <- paste0(segged_funny_rate, collapse = " ")
}

f <- kwic(funny, pattern = "Funny",window = 2)
count_num <- f$post

corp_count_num <- corpus(count_num)
toks_count_num <- tokens(corp_count_num)
f_num <- kwic(toks_count_num, pattern = "count",window = 1)
funny_rate_count <- f_num$post
funny_rate_count <- as.numeric(funny_rate_count)

#將funny_rate_count跟ted_with_laughter合併
data_only_funny_rate <- as.data.frame(funny_rate_count)
ted_with_laughter <- cbind(ted_with_laughter, data_only_funny_rate)


#視覺化
##laughter_count&funny_rate_count
ted_with_laughter_a <- ted_with_laughter[-1,]
laughter_funny_lm <- lm(funny_rate_count ~ laughter_count, data = ted_with_laughter_a) 
vis_laughter_funny <- ggplot(ted_with_laughter_a, aes(y = funny_rate_count, x = laughter_count))+
  geom_point()+
  geom_hline(yintercept = mean(ted_with_laughter_a$funny_rate_count), color = "red", size = 1 )+
  ggtitle("Laughter次數與Funny Ratings數的關聯")+
  labs(x = "Laughter次數", y = "funny ratings 數")+
  theme_bw()+
  geom_smooth(method = lm)

##laughter_count&views
laughter_view_lm <- lm(views/10000 ~ laughter_count, data = ted_with_laughter_a)
vis_laughter_view <- ggplot(ted_with_laughter_a, aes(y = views/10000, x = laughter_count))+
  geom_point()+
  geom_hline(yintercept = mean(ted_with_laughter_a$views)/10000, color = "red", size = 1 )+
  ggtitle("Laughter次數與觀看次數的關聯")+
  labs(x = "Laughter次數", y = "觀看人數(萬)")+
  theme_bw()+
  geom_smooth(method = lm)

#蒐集data science speaker 的資料
data_occu <- unique(str_subset(ted$speaker_occupation, "^Data"))
linguist_occu <- unique(str_subset(ted$speaker_occupation, "Linguist"))

scientist_df <- data.frame()
scientist_df <- filter(ted, ted$speaker_occupation == data_occu[1])
for(i in c(2:11)){
  scientist_df <- rbind(scientist_df,filter(ted,ted$speaker_occupation == data_occu[i]))}
for(i in c(1:2)){
  scientist_df <- rbind(scientist_df,filter(ted,ted$speaker_occupation == linguist_occu[i]))}

#data文字雲
scientist_speech_num <- scientist_df$speech_num
scientist_text <- data.frame()
scientist_text <- filter(tidy_text_tran, speech_num == scientist_speech_num[1])
for(i in c(2:20)){
  scientist_text <- rbind(scientist_text, filter(tidy_text_tran, speech_num== scientist_speech_num[i]))
}

scientist_word<- scientist_text %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n))
all_word <- tidy_text_tran %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n))

scientist_word <- filter(scientist_word,!word%in% c("the","and","to","of","a","that","in","is","s","this","so","it","re"))
all_word <- filter(all_word,!word%in% c("the","and","to","of","a","that","in","is","s","this","so","it","re"))

all_word_wc <- wordcloud(all_word$word, all_word$n, random.order = F,colors=brewer.pal(9, "Dark2"), max.words = 100)
scientist_word_wc <- wordcloud(scientist_word$word,scientist_word$n,random.order = F, ordered.colors = T, max.words = 100)

scientist_word_wc <- wordcloud(scientist_word$word, scientist_word$n, random.order = F,colors=brewer.pal(8,"Dark2"),max.words = 100, scale = c(6,0.6))
all_word_wc <- wordcloud(all_word$word, all_word$n, random.order = F,colors=brewer.pal(8,"Dark2"), max.words = 100, scale = c(6, 0.6))
 

