#Environmentの整理
rm(list = ls())

#必要なパッケージのインストール＆ロード
install.packages("pacman")
pacman::p_load("haven", "tidyverse")

#データの読み込み
df_survey2 <- read_dta("raw_data/survey2_clean.dta")


#新しい列（一週間の期待値）
df_survey2 <-df_survey2 %>% 
  mutate(med_expected　= as.numeric(med_expect_daysperweek)/7) %>% 
  mutate(nut_expected　= as.numeric(nut_expect_daysperweek)/7) %>%
  mutate(exer_expected　= as.numeric(exer_expect_daysperweek)/7) %>%
  mutate(sleep_expected　= as.numeric(sleep_expect_daysperwee)/7) %>% 
  mutate(med_hoped　= as.numeric(med_hope_daysperweek)/7) %>% 
  mutate(nut_hoped　= as.numeric(nut_hope_daysperweek)/7) %>% 
  mutate(exer_hoped　= as.numeric(exer_hope_daysperweek)/7) %>% 
  mutate(sleep_hoped　= as.numeric(sleep_hope_daysperweek)/7)


#列分け(med)
df_survey2 <- df_survey2 %>%
  separate_wider_delim(
    med_expect_timeofday,
    delim = ",",
    names = paste0("med_time_", 1:24),
    too_few = "align_start"
  )


