library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(anytime)
library(tidyr)
library(tidytext)

# 讀擋
ted_main <- read_csv("./ted_main.csv")
scripts_all <- read_csv("./transcripts.csv")

# (1) Tags相關研究

# 選出觀看次數前100的影片
top100 <- ted_main %>%
  arrange(desc(views)) %>%
  head(100)

# 整理tags資料(原為character)
tags <- top100$tags %>% 
  strsplit(',') %>% 
  str_match_all('[A-Z|a-z]{2,}') %>%
  unlist()

# 計算tags出現次數並照次數排列
tags_df <- data.frame(tags = tags)%>% 
  group_by(tags) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))

# 視覺化
tags_df %>% 
  filter(n > 9) %>%
  ggplot() +
    geom_histogram(aes(tags, n), width = 0.8,
                   fill = "#F8766D",
                   stat = 'identity') +
    geom_text(aes(tags, n, label = n), vjust = -0.5, size = 3) +
    scale_y_continuous(limits = c(0,45)) +
    labs(title = 'Top100出現十次以上tags') +
    ylab('次數') +
    theme_light() +
    theme(axis.text=element_text(size=10), 
          axis.text.x = element_text(angle=45, hjust=1))

# 選出後100
last100 <- ted_main %>%
  arrange(views) %>%
  head(100)

# 整理tags資料
tags_last <- last100$tags %>% 
  strsplit(',') %>% 
  str_match_all('[A-Z|a-z]{2,}') %>%
  unlist()

# 計算出現次數並照次數排列
tags_last_df <- data.frame(tags = tags_last)%>% 
  group_by(tags) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))

# 視覺化
tags_last_df %>% 
  filter(n > 9) %>%
  ggplot() +
  geom_histogram(aes(tags, n),
                 width = 0.8,
                 fill = "#00BFC4",
                 stat = 'identity') +
  geom_text(aes(tags, n, label = n), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0,45)) +
  labs(title = 'Last100出現十次以上tags') +
  ylab('次數') +
  theme_light() +
  theme(axis.text=element_text(size=10), 
        axis.text.x = element_text(angle=45, hjust=1))


# (2)共同tag不同觀看次數差異

# 分別找出觀看次數前100出現10次以上的tags
top_tags <- tags_df %>% 
  filter(n > 9) %>%
  # 去除活動類型tags
  filter(tags != 'TEDx')

last_tags <- tags_last_df %>% 
  filter(n > 9) %>%
  filter(tags != 'TEDx')

# 找出在前後100都出現超過10次的tags
same_tag <- rbind(top_tags %>%
                    # 增加排名前後100標記以利之後增加出現次數
                    mutate(rank = "top"),
                  last_tags %>%
                    mutate(rank = "last")) %>%
  group_by(tags) %>%
  mutate(n = n()) %>%
  filter(n == 2)

# 找出重複tags在前100名影片累積出現的次數
st_t <- same_tag %>% filter(rank == "top")
nr_t <- vector('numeric', 9)
for (i in seq_along(nr_t)) {
  # 從前100名影片tags的data frame中選出次數
  stt <- top_tags %>%
    filter(tags %in% st_t$tags[i])
  nr_t[i] <- stt$n
}

# 找出重複tags在後100名影片累積出現的次數
st_l <- same_tag %>% filter(rank == "last")
nr_l <- vector('numeric', 9)
for (i in seq_along(nr_l)) {
  stl <- last_tags %>%
    filter(tags %in% st_l$tags[i])
  nr_l[i] <- stl$n
}

# 將數據增加至data frame
same_tag <- same_tag %>%
  cbind(nr = c(nr_t, nr_l))

# 視覺化
ggplot(same_tag) +
  geom_histogram(aes(tags, nr, group = desc(rank), fill = rank),
                 width = 0.8, stat = "identity", position = position_dodge(0.8)) +
  geom_text(aes(tags, nr, label = nr, group = desc(rank)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  labs(title = '觀看次數前後100共同出現10次以上之tags') +
  xlab('tags') +
  ylab('次數') +
  scale_y_continuous(limits = c(0,45)) +
  theme_light() +
  theme(axis.text = element_text(size = 12), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(reverse = T)) +
  scale_fill_manual(values = c(top = "#F8766D", last = "#00BFC4"))

# 分析相同tag觀看次數差異，以technology為例
ted_technology <- ted_main %>%
  # 將tags整理成list(原為character)
  mutate(tag_list = tags %>%
           strsplit(',') %>% 
           str_match_all('[A-Z|a-z]{2,}')) %>%
  select(tag_list, url, duration, tags, views) %>%
  # 將tags資料展開，一個tag有一組資料(url, views等)
  unnest(tag_list) %>%
  # 挑出technology的資料，意即該影片有出現technology的tag
  filter(tag_list == "technology") %>%
  arrange(desc(views)) %>%
  mutate(tag_seg = tags %>%
           strsplit(',') %>% 
           str_match_all('[A-Z|a-z]{2,}'))

# 有technology tag影片觀看次數前30名長度、tags數量
tech_top30 <- head(ted_technology, 30)
num_tags_t <- vector('numeric', 30)
for (i in 1:30) {
  num_tags_t[i] <- length(tech_top30$tag_seg[[i]])
}

tech_top30_df <- data.frame(duration = tech_top30$duration,
                            num_tags = num_tags_t,
                            rank = "top")

# 計算後30個影片長度、tags數量
tech_last30 <- tail(ted_technology, 30)
num_tags_l <- vector('numeric', 30)
for (i in 1:30) {
  num_tags_l[i] <- length(tech_last30$tag_seg[[i]])
}

tech_last30_df <- data.frame(duration = tech_last30$duration,
                             num_tags = num_tags_l,
                             rank = "last")

tech_df <- rbind(tech_top30_df, tech_last30_df)

# technology tag數量機率分布
tech_df %>% 
  ggplot() +
  geom_density(aes(num_tags, color = rank)) +
  labs(title = 'technology相關影片tag數量分布') +
  xlab('次數') +
  theme_light()

# technology影片長度機率分布
tech_df %>% 
  ggplot() +
  geom_density(aes(duration, color = rank)) +
  labs(title = 'technology相關影片長度分布') +
  xlab('長度（秒）') +
  theme_light()

# 利用url配對篩選出technology前後30的逐字稿
scripts_tech_top30 <- filter(scripts_all, url %in% tech_top30$url)
scripts_tech_last30 <- filter(scripts_all, url %in% tech_last30$url)

# 前30名影片詞頻表
scripts_tech_top30 <- scripts_tech_top30 %>%
  select(transcript) %>%
  unnest_tokens(output = "word", input = "transcript",
                # 觀眾的笑聲和鼓掌在逐字稿用(laughter)和(applause)方式呈現
                # 為區分講者內容和觀眾反應，不用左括號分割
                # 但因(laughter)和(applause)與下一個單字間無空格，用右括號切割
                token = "regex", pattern = "[ ,:;\\.\"—)]") %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# 後30名影片詞頻表
scripts_tech_last30 <- scripts_tech_last30 %>%
  select(transcript) %>%
  unnest_tokens(output = "word", input = "transcript",
                token = "regex", pattern = "[ ,:;\\.\"—)]") %>%
  anti_join(stop_words, by = c("word" = "word")) %>% 
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# 視覺化(僅取詞頻前15名)
scripts_tech_top30 %>%
  mutate(word = reorder(word, n)) %>%
  filter(word !='em') %>%
  filter(word !='ca') %>% # 去除講者姓名縮寫
  top_n(15, n) %>%
  ggplot() +
  geom_bar(aes(word, n), stat = "identity", fill = "#F8766D") +
  coord_flip() +
  scale_y_continuous(limits = c(0,225)) +
  geom_text(aes(word, n, label = n),
            hjust = -0.3, size = 3) +
  labs(title = 'technology觀看前30之詞頻') +
  ylab('次數') +
  theme_light()

scripts_tech_last30 %>%
  mutate(word = reorder(word, n)) %>%
  top_n(15, n) %>%
  ggplot() +
  geom_bar(aes(word, n), stat = "identity", fill = "#00BFC4") +
  coord_flip() +
  scale_y_continuous(limits = c(0,170)) +
  geom_text(aes(word, n, label = n),
            hjust = -0.3, size = 3) +
  labs(title = 'technology觀看後30之詞頻') +
  ylab('次數') +
  theme_light()


# (3) 人稱相關研究

# 利用ted_main的url的vector順序找出scripts觀看排名
rankings <- vector('numeric', 2467)
for (i in seq_along(rankings)) {
  rankings[i] <- which(ted_main$url == scripts_all$url[i])
}

# 增加逐字稿斷詞並換成全小寫
scripts_seg <- scripts_all %>%
  mutate(ranking = rankings,
         seg = transcript %>%
           tolower() %>%
           strsplit('[\',( ]{1,2}')) %>%
  arrange(ranking)

# 找出第一次出現I的位置
positionI <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionI[i] <- min(which(scripts_seg$seg[[i]] == "i"))
  # 若未出現I則呈現NA
  if (positionI[i] == Inf) {
    positionI[i] <- NA
  }
}

# 找出I次數
timesI <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timesI[i] <- str_count(scripts_seg$transcript[i], "i|I")
}

# 找出第一個we的位置
positionwe <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionwe[i] <- min(which(scripts_seg$seg[[i]] == "we"))
  if (positionwe[i] == Inf) {
    positionwe[i] <- NA
  }
}

# 找出we次數
timeswe <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timeswe[i] <- str_count(scripts_seg$transcript[i], "we|We")
}

# 找出第一個you的位置
positionyou <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionyou[i] <- min(which(scripts_seg$seg[[i]] == "you"))
  if (positionyou[i] == Inf) {
    positionyou[i] <- NA
  }
}

# 找出you次數
timesyou <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timesyou[i] <- str_count(scripts_seg$transcript[i], "you|You")
}

# 找出第一個he的位置
positionhe <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionhe[i] <- min(which(scripts_seg$seg[[i]] == "he"))
  if (positionhe[i] == Inf) {
    positionhe[i] <- NA
  }
}

# 找出he次數
timeshe <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timeshe[i] <- str_count(scripts_seg$transcript[i], "he|He")
}

# 找出第一個she的位置
positionshe <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionshe[i] <- min(which(scripts_seg$seg[[i]] == "she"))
  if (positionshe[i] == Inf) {
    positionshe[i] <- NA
  }
}

# 找出she次數
timesshe <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timesshe[i] <- str_count(scripts_seg$transcript[i], "she|She")
}

# 找出第一個they的位置
positionthey <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionthey[i] <- min(which(scripts_seg$seg[[i]] == "they"))
  if (positionthey[i] == Inf) {
    positionthey[i] <- NA
  }
}

# 找出they次數
timesthey <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timesthey[i] <- str_count(scripts_seg$transcript[i], "they|They")
}

# 找出第一個they的位置
positionit <- vector('numeric', 100)
for (i in seq_along(scripts_seg$seg)) {
  positionit[i] <- min(which(scripts_seg$seg[[i]] == "it"))
  if (positionit[i] == Inf) {
    positionit[i] <- NA
  }
}

# 找出it次數
timesit <- vector('numeric', 100)
for (i in seq_along(scripts_seg$transcript)) {
  timesit[i] <- str_count(scripts_seg$transcript[i], "it|It")
}

# 建立data frame
anyl_test <- tibble::tibble(Ranking = scripts_seg$ranking,
                            "times_I" = timesI, "First 'I' Position" = positionI,
                            "First 'We' Position" = positionwe, "times_we" = timeswe,
                            "First 'You' Position" = positionyou, "times_you" = timesyou,
                            "First 'He' Position" = positionhe, "times_he" = timeshe,
                            "First 'She' Position" = positionshe, "times_she" = timesshe,
                            "First 'They' Position" = positionthey, "times_they" = timesthey,
                            "First 'It' Position" = positionit, "times_it" = timesit)

# 視覺化
ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'I' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `times_I`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'I' Position`)) +
  geom_smooth(aes(Ranking, `times_I`)) +
  scale_y_continuous(name = expression('First " I " Position'), 
                     sec.axis = sec_axis(~., name = 'Times of " I "')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" I " 第一次出現位置以及次數')

ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'We' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `times_we`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'We' Position`)) +
  geom_smooth(aes(Ranking, `times_we`)) +
  scale_y_continuous(name = expression('First " We " Position'), 
                     sec.axis = sec_axis(~., name = 'Times of " We "')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" We " 第一次出現位置以及次數')

ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'You' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `times_you`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'You' Position`)) +
  geom_smooth(aes(Ranking, `times_you`)) +
  scale_y_continuous(name = expression('First " You " Position'), 
                     sec.axis = sec_axis(~., name = 'Times of " You "')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" You " 第一次出現位置以及次數')

ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'He' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `timeshe`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'He' Position`)) +
  geom_smooth(aes(Ranking, `times_he`)) +
  scale_y_continuous(name = expression('First "He" Position'), 
                     sec.axis = sec_axis(~., name = 'Times of "He"')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" He " 第一次出現位置以及次數')

ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'She' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `timesshe`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'She' Position`)) +
  geom_smooth(aes(Ranking, `times_she`)) +
  scale_y_continuous(name = expression('First "She" Position'), 
                     sec.axis = sec_axis(~., name = 'Times of "She"')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" She " 第一次出現位置以及次數')

ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'It' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `timesit`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'It' Position`)) +
  geom_smooth(aes(Ranking, `timesit`)) +
  scale_y_continuous(name = expression('First "It" Position'), 
                     sec.axis = sec_axis(~., name = 'Times of "It"')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" It " 第一次出現位置以及次數')

ggplot(anyl_test) +
  geom_point(aes(Ranking, `First 'They' Position`), color = 'darkcyan', size = 0.5) +
  geom_point(aes(Ranking, `timesthey`), color = 'darkolivegreen3', size = 0.5) +
  geom_smooth(aes(Ranking, `First 'They' Position`)) +
  geom_smooth(aes(Ranking, `timesthey`)) +
  scale_y_continuous(name = expression('First "They" Position'), 
                     sec.axis = sec_axis(~., name = 'Times of "They"')) +
  theme_light() +
  theme(axis.title.y = element_text(color = "darkcyan"),
        axis.title.y.right = element_text(color = "darkolivegreen3")) +
  labs(title = '" They " 第一次出現位置以及次數')


# (4) tag歷年流變

# 將timestamp轉成日期格式
ted_main$film_date <- anydate(ted_main$film_date)

# 選出每年份最多的tags
tags_year <- ted_main %>%
  # 增加拍攝年份column
  mutate(film_year = format(ted_main$film_date, "%Y")) %>%
  select(film_year, tags) %>%
  mutate(tags = tags %>%
           strsplit(',') %>% 
           str_match_all('[A-Z|a-z]{2,}')) %>% 
  unnest(tags) %>%
  group_by(film_year, tags) %>%
  summarise(n = n()) %>%
  arrange(film_year, desc(n)) %>%
  # 去除活動類型tag
  filter(tags != 'TEDx') %>%
  filter(n == max(n))

# 視覺化
ggplot(tags_year %>%
         filter(film_year > 2001)) +
  geom_histogram(aes(film_year, n, fill = tags), stat="identity",position='dodge') +
  geom_text(aes(film_year, n, label = n), vjust = -0.5, size = 3) +
  labs(title = '2002-2017年次數最高tags') +
  xlab('年份') +
  ylab('次數') +
  scale_y_continuous(limits = c(0,110)) +
  theme_light() +
  theme(axis.text = element_text(size = 10), 
        axis.text.x = element_text(angle = 45, hjust = 1))
