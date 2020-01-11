1. 將資料corpus和token

# 全體 corpus and tokens
corp_title <- corpus(ted,text_field = "title")
toks_title <- tokens(corp_title)

# Views前100名，corpus and tokens
ted_top100 <- ted  %>%
  arrange(desc(views))
ted_famous <- ted_top100[1:100,]

corp_title_f <- corpus(ted_famous,text_field = "title")
toks_title_f <- tokens(corp_title_f)

# views後100名，corpus and tokens
ted_last100 <- ted  %>%
  arrange(views)
ted_last <- ted_last100[1:100,]

corp_title_l <- corpus(ted_last,text_field = "title")
toks_title_l <- tokens(corp_title_l)

2. 計算各群組標題長度

##### 全體標題長度
sum_of_length <- vector("numeric", 2550)
for(i in 1:2550){ 
  sum_of_length[i] <- length(toks_title[[i]])
}

mean(sum_of_length)
summary(sum_of_length)

##### famous title length
length_fam <- vector("numeric", 100)
for(i in 1:100){ 
  length_fam[i] <- length(toks_title_f[[i]])
}

mean(length_fam)
summary(length_fam)

##### Last title length
length_last <- vector("numeric", 100)
for(i in 1:100){ 
  length_last[i] <- length(toks_title_l[[i]])
}

mean(length_last)
summary(length_last)



3. 計算各群組使用問句的比例

# 全體使用問句開頭的情形

# 標題以Why 開頭
sum(str_detect(corp_title,'^Why')) # 128/2550 = .05

# 標題以what開頭
sum(str_detect(corp_title,'^What')) # 119 = .047

# 標題以how開頭
sum(str_detect(corp_title,'^How')) # 305/2550 = .12

# 標題以Do開頭
sum(str_detect(corp_title,'^Do')) # 25/2550 = .01

# 標題以Should開頭
sum(str_detect(corp_title,'^Should')) # 4/2550 = .


# famous中使用問句的情形

# 標題以why 開頭
sum(str_detect(corp_title_f,'^Why')) # 9/100 = .09

# 標題以what開頭
sum(str_detect(corp_title_f,'^What')) # 3/100 = .03

# 標題以how開頭
sum(str_detect(corp_title_f,'^How')) # 12/100 = .12

# 標題以Do開頭
sum(str_detect(corp_title_f,'^Do')) # 1個

# 標題以should開頭
sum(str_detect(corp_title_f,'^Should')) # 0


# last中使用問句的情形

# 標題以why 開頭
sum(str_detect(corp_title_l,'^Why')) # 1/100 = .01

# 標題以what開頭
sum(str_detect(corp_title_l,'^What')) # 3/100 = .03

# 標題以how開頭
sum(str_detect(corp_title_l,'^How')) # 5/100 = .05

# 標題以Do開頭
sum(str_detect(corp_title_l,'^Do')) # 

# 標題以Should開頭
sum(str_detect(corp_title_l,'^Should')) # 0


4. 計算各群組使用You/Your以及大寫字母的比例

# 全體
sum(str_detect(all_title_ttf$word,"(Y|y)our?$")) 
# 211/15833 = .013

# famous

sum(str_detect(famous_title_ttf$word,"(Y|y)our?$"))
# 17/616 = .028

# last
sum(str_detect(last_title_ttf$word,"(Y|y)our?$"))
# 3/486 = .006


#### 大寫字母的使用

# 全體

upper_all <- str_extract_all(all_title_df,
                             '(?<=.)[A-Z]\\w+')
length(upper_all[[2]])

# famous

upper_fam <- str_extract_all(famous_title_df,
                             '(?<=.)[A-Z]\\w+')
length(upper_fam[[2]])

# last

upper_last <- str_extract_all(last_title_df,'(?<=.)[A-Z]\\w+')
length(upper_last[[2]])




