#Environmentの整理
rm(list = ls())

#必要なパッケージのインストール＆ロード
install.packages("pacman")
pacman::p_load("haven", "tidyverse")

#データの読み込み
df_survey1 <- read_dta("data/survey1_clean.dta")

#通知頻度のグループ分け
summary(df_survey1$dailynotif_selfreport)
df_survey1 <- df_survey1 %>% 
  mutate(highnotifications = ifelse(dailynotif_selfreport >= median(df_survey1$dailynotif_selfreport, na.rm = TRUE), 1, 0))
df_survey1 <- df_survey1 %>% 
  mutate(lownotifications = highnotifications - 1) %>% 
  mutate(dailynotif_act = dailynotif_remind)
df_survey1 <-  mutate(df_survey1, highnotifremind = ifelse(dailynotif_act >= median(df_survey1$dailynotif_act, na.rm = TRUE), 1, 0)) 
df_survey1 <- mutate(df_survey1, dailynotif_dist = dailynotif_msg + dailynotif_update)
df_survey1 <- mutate(df_survey1, highnotifdist = ifelse(dailynotif_dist >= median(df_survey1$dailynotif_dist, na.rm = TRUE), 1, 0)) 

# 瞑想、食事記録の選好＋処置内容の総合難易度
df_survey1 <- df_survey1 %>% mutate(net_rel_ben_med = rel_ben_med - rel_cost_med)
df_survey1 <- mutate(df_survey1, highbenmed = ifelse(
  net_rel_ben_med >= median(df_survey1$net_rel_ben_med, na.rm = TRUE),
  1,
  0
))
df_survey1 <- mutate(df_survey1, totaldifficulty = diff_med + diff_nut)
df_survey1 <- mutate(df_survey1, highdifficulty = ifelse(totaldifficulty >= median(df_survey1$totaldifficulty, na.rm = TRUE), 1, 0))


#総合経験値
df_survey1 <- df_survey1 %>% mutate(med_ever = ifelse(med_last==5, 0, 1)) %>% 
  mutate(nuttrack_ever = ifelse(nuttrack_last==5, 0, 1)) %>% 
  mutate(med_experience = med_last_month + med_daily_ever + med_ever) %>% 
  mutate(nut_experience = nuttrack_last_month + nuttrack_daily_ever + nuttrack_ever) %>% 
  mutate(experience = med_experience + nut_experience)

df_survey1 <- mutate(df_survey1, highexperience = ifelse(experience >= median(df_survey1$experience, na.rm = TRUE), 1, 0))
df_survey1 <- mutate(df_survey1, lowexperience = ifelse(experience < median(df_survey1$experience, na.rm = TRUE), 1, 0))

# 列の選択
df_survey1 <- df_survey1 %>%
  select(
    email,
    highnotifications,
    highbenmed,
    highexperience,
    lowexperience,
    med_experience,
    nut_experience,
    highnotifdist,
    highnotifremind,
    highdifficulty,
    starts_with("dailynotif"),
    starts_with("highnotif"),
    starts_with("lownotif"),
    notifyingapps,
    rel_ben_med,
    rel_cost_med,
    net_rel_ben_med,
    med_ever,
    nuttrack_ever,
    med_last_month,
    nuttrack_last_month,
    med_daily_ever,
    nuttrack_daily_ever,
    med_experience,
    nut_experience,
    experience,
    starts_with("imp"),
    starts_with("fun"),
    starts_with("diff")
  )

df_survey1 <- df_survey1 %>% 
  select(-starts_with("dailynotif_app"))  

#データの書き出し
  write_csv(df_survey1, "cleaning_data/vars_s1.csv")