#Environmentの整理
rm(list=ls())

#必要なパッケージのインストール＆ロード
install.packages("pacman")
pacman::p_load(
  tidyverse,
  estimatr,
  haven,
  stargazer,
  gt,
  modelsummary,
  ggthemes,
  gridExtra,
  tableGrob,
  readxl,
  KableExtra
)

#データの読み込み
df_master <- read_dta("raw_data/final.dta")

#作成順：処置群分け->要約統計->メインの結果(図)->メインの結果(表)
#----処置群分け表の作成----
#処置群分け表の作成
df_collapse <- df_master %>% 
  group_by(email, treatment) %>% 
  summarize(num_obs = n(), .groups = "drop")

num_treatment1 <- sum(df_collapse$treatment == 1)
num_treatment2 <- sum(df_collapse$treatment == 2)
num_treatment3 <- sum(df_collapse$treatment == 3)
num_treatment4 <- sum(df_collapse$treatment == 4)
num_treatment5 <- sum(df_collapse$treatment == 5)
num_treatment1 <- as.numeric(num_treatment1)
num_treatment2 <- as.numeric(num_treatment2)
num_treatment3 <- as.numeric(num_treatment3)
num_treatment4 <- as.numeric(num_treatment4)
num_treatment5 <- as.numeric(num_treatment5)


#----要約統計----
# 要約統計
df_mini <- df_master %>%
  select(
    treatment,
    female,
    college,
    age,
    dailynotif,
    med_daily_ever,
    med_last_month,
    nuttrack_daily_ever,
    nuttrack_last_month,
    rel_ben_med,
    rel_cost_med
  )

#treatmentごとに分割
df_mini_1 <-df_mini %>% 
  filter(treatment == 1)
df_mini_2 <-df_mini %>% 
  filter(treatment == 2)
df_mini_3 <-df_mini %>% 
  filter(treatment == 3)
df_mini_4 <-df_mini %>% 
  filter(treatment == 4)
df_mini_5 <-df_mini %>% 
  filter(treatment == 5)

#平均・分散の出力
#処理１
treatment1_mean <- sapply(df_mini_1, mean)
treatment1_sd <- sapply(df_mini_1, sd)
treatment1_stats <- data.frame(Mean=treatment1_mean, SD=treatment1_sd)
treatment1_stats <- treatment1_stats[-1, ]


#処理２
treatment2_mean <- sapply(df_mini_2, mean)
treatment2_sd <- sapply(df_mini_2, sd)
treatment2_stats <- data.frame(Mean=treatment2_mean, SD=treatment2_sd)
treatment2_stats <- treatment2_stats[-1, ]

#処理3
treatment3_mean <- sapply(df_mini_3, mean)
treatment3_sd <- sapply(df_mini_3, sd)
treatment3_stats <- data.frame(Mean=treatment3_mean, SD=treatment3_sd)
treatment3_stats <- treatment3_stats[-1, ]

#処理4
treatment4_mean <- sapply(df_mini_4, mean)
treatment4_sd <- sapply(df_mini_4, sd)
treatment4_stats <- data.frame(Mean=treatment4_mean, SD=treatment4_sd)
treatment4_stats <- treatment4_stats[-1, ]

#処理5
treatment5_mean <- sapply(df_mini_5, mean)
treatment5_sd <- sapply(df_mini_5, sd)
treatment5_stats <- data.frame(Mean=treatment5_mean, SD=treatment5_sd)
treatment5_stats <- treatment5_stats[-1, ]

combinedresults <- bind_cols(
  treatment1_stats,
  treatment2_stats,
  treatment3_stats,
  treatment4_stats,
  treatment5_stats
)


#----図の出力----
#パネルA
df_figure1 <- df_master %>%
  select(treatment, day, meditated, loggedmeal) %>%
  group_by(treatment, day) %>%
  summarise(mean_meditaiton = mean(meditated),
            mean_loggedmeal = mean(loggedmeal)) %>%
  mutate(label = factor(
    treatment,
    1:5,
    c("Control", "Med msg", "Nut msg", "Med and nut msg", "Nut inc")
  ))
 

ggplot(df_figure1,
       aes(
         x = day,
         y = mean_meditaiton,
         group = treatment,
         color = label
       )) + 
  geom_line(linewidth = 0.8) +
  labs(
    x = "処置後経過日数",
    y = "瞑想率",
    color = "処置群",
    title = "処置群ごとの瞑想率"
  ) +
  geom_vline(xintercept = 27) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 54, by = 3)) + 
  theme_fivethirtyeight() + scale_color_manual(
    values = c(
      "Control" = "black",
      "Med msg" = "blue",
      "Nut msg" = "red",
      "Med and nut msg" = "green",
      "Nut inc" = "purple"
    )
  )

#パネルB
ggplot(df_figure1, aes(x = day,
                       y = mean_loggedmeal,
                       group = treatment,
                       color = label)) + 
  geom_line(linewidth = 0.8) +
  labs(
    x = "処置後経過日数",
    y = "食事記録率",
    color = "処置群",
    title = "処置群ごとの食事記録率"
  ) +
  geom_vline(xintercept = 27) +
  scale_x_continuous(breaks = seq(from = 0, to = 54, by = 3)) + 
  theme_fivethirtyeight() + 
  scale_color_manual(
    values = c(
      "Control" = "black",
      "Med msg" = "blue",
      "Nut msg" = "red",
      "Med and nut msg" = "green",
      "Nut inc" = "purple"))



#----回帰分析----
df_treatment <- df_master %>% 
  filter(day <= 27 & day >= 1)

df_posttreatment <- df_master %>% 
  filter(day > 27)

# model 1
result1 <- lm_robust(
  meditated ~ mx + my + mxmy + zy + female + college + dailynotif + med_last_month + nuttrack_last_month,
  data = df_treatment,
  clusters = email
)
# model 2
result2 <- lm_robust(
  loggedmeal ~ mx + my + mxmy + zy + female + college + dailynotif + med_last_month + nuttrack_last_month,
  data = df_treatment,
  clusters = email
)
# model 3
result3 <- lm_robust(
  meditated ~ mx + my + mxmy + zy + female + college + dailynotif + med_last_month + nuttrack_last_month,
  data = df_posttreatment,
  clusters = email
)
# model 4
result4 <- lm_robust(
  loggedmeal ~ mx + my + mxmy + zy + female + college + dailynotif + med_last_month + nuttrack_last_month,
  data = df_posttreatment,
  clusters = email
)


combinedresult <- list(result1, result2, result3, result4)
modelsummary(combinedresult, coef_map = c("mx", "my", "mxmy", "zy"), gof_map = c("nobs"))
