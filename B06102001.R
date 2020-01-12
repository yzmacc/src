library(stringr)
library(dplyr)
library(quanteda)
library(jiebaR)
library(tidytext)
library(ggplot2)

#Adding new column for first five sentences
ted <- ted %>% mutate(first_five =
                        str_extract(transcript, '.*?[a-z][.?!](?= |\\().*?[a-z][.?!](?= |\\().*?[a-z][.?!](?= |\\().*?[a-z][.?!](?= |\\().*?[a-z][.?!](?= |\\()'))
#Calculating laughter times for first five sentences

ted <- ted %>% mutate(laugh_first_five =
                        str_count(first_five, "Laughter"))

#No laughter in first five sentences (2068)
sum(ted$laugh_first_five == "0", na.rm = TRUE)
#With Laughter (380)
sum(ted$laugh_first_five >= 1, na.rm = TRUE)


#extracting sentences before and after laughter
corps_trans <- corpus(transcripts, text_field = "transcript")
toks_trans <- tokens(corps_trans)
kw_laughter <- kwic(toks_trans, pattern = "Laughter", window = 10) #bracket still there
#number of "?"
sum(str_count(kw_laughter$pre, '\\?')) #1144/10351
#number of "!" 
sum(str_count(kw_laughter$pre, '\\!')) #373/10351
#number of '"' (quote)
sum(str_count(kw_laughter$pre, '"')) #2279/10351


punct_laughter_chart <- tibble(punct = c("question?", "exclamation!", "quotation"),
                               count = c(1144, 373, 2279))
library(tidyverse)
library(lubridate)
punct_laughter_chart <- as.data.frame(punct_laughter_chart)
ggplot(data = punct_laughter_chart) +
  geom_bar(mapping = aes(x = punct, y = count, width = 0.5), 
           stat = "identity", fill =  "#F8766D") +theme_light(base_size = 12)

#number of 'Laughter' (laughing within 5 words)
sum(str_count(kw_laughter$pre, 'Laughter')) #376/10351

#number of 'you'
you1 <- sum(str_count(kw_laughter$pre, 'you')) #1707/10351
you2 <- sum(str_count(kw_laughter$pre, 'You')) #172
#number of 'your'
your1 <- sum(str_count(kw_laughter$pre, 'your')) #331
your2 <- sum(str_count(kw_laughter$pre, 'Your')) #10
#sum of you/your
you_your <- sum(you1, you2, your1, your2)


#number of 'she'
she1 <- sum(str_count(kw_laughter$pre, 'she')) #159
she2 <- sum(str_count(kw_laughter$pre, 'She')) #32
she_She <- sum(she1, she2)


#versus number of 'he'
he1 <- sum(str_count(kw_laughter$pre, 'he')) #5704
he2 <- sum(str_count(kw_laughter$pre, 'He')) #113
he_He <- sum(he1, he2)

#versus number of 'I'
I <- sum(str_count(kw_laughter$pre, 'I')) #2220

#creating a data frame with pronouns
pronoun_laughter_chart <- tibble(pronoun = c("you_Your", "she_She", "he_He", "I"),
                                 count = c(2220, 191, 5817, 2220))
library(tidyverse)
library(lubridate)
pronoun_laughter_chart <- as.data.frame(pronoun_laughter_chart)

#making the bar chart for pronouns
ggplot(data = pronoun_laughter_chart) +
  geom_bar(mapping = aes(x = pronoun, y = count, width = 0.5), 
           stat = "identity", fill =  "#F8766D") +theme_light(base_size = 12)