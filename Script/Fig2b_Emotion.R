setwd('~/vaccineTweets/')
library(did)
library(lubridate)
library(data.table)
library(foreach)
library(doParallel)
library(tidyverse)
library(ggalt)
library(ggrepel)

source('script/compute.aggte.R')


# Loading individual-level weekly panel data ------------------------------

dta.week <- readRDS('weekly_panel_rmVac.rds')

dta.week <- dta.week %>%
  mutate(emotion = hedono, 
         emotion_std = (emotion - mean(emotion, na.rm = T)) / sd(emotion, na.rm = T)
  ) %>%
  mutate(time = week_created - min(week_created) + 1) %>% 
  group_by(user_id) %>% 
  mutate(week_freq = sum(week_created <= 26)) %>% 
  ungroup()


dta.week <- dta.week %>% 
  mutate(soc = soc_family + soc_family, 
         con = con_work + con_leisure + con_home + con_money, 
         cog = cog_insight + cog_causal + cog_certainty + cog_different)


dimension <- colnames(dta.week)[c(32:36)]

stopImplicitCluster()
registerDoParallel(cores = 5)

dim.list <- foreach(dim = dimension) %dopar% {
  
  print(paste0("Start processing ", dim))
  
  dta.week$dim <- dta.week[[dim]]
  
  dta.week <- dta.week %>%
    mutate(dim_std = (dim - mean(dim, na.rm = T)) / sd(dim, na.rm = T))
  
  reg_attgt <- att_gt(yname = "dim_std",
                      tname = "week_created",
                      idname = "user_id",
                      gname = "week_first", 
                      control_group = 'notyettreated', 
                      allow_unbalanced_panel = T, 
                      bstrap = F,
                      cband = F,
                      base_period = 'universal', 
                      anticipation = 0,
                      data = dta.week %>% 
                        filter(week_freq >= 6) %>%
                        filter((tweet_freq > as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.05))) &
                                 (tweet_freq < as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.95)))) %>%
                        mutate(user_id = factor(user_id, unique(user_id)), 
                               user_id = as.integer(user_id)) %>% 
                        filter(week_created <= 27) %>%
                        mutate(week_first = ifelse(week_first > 27, 0, week_first)),
                      pl = T, 
                      cores = 6)
  
  print(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -10, max_e = 15))
  
  return(list(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -9, max_e = 15), 
              aggte(reg_attgt, type = "simple", na.rm = T, max_e = 15), 
              aggte(reg_attgt, type = "simple", na.rm = T, min_e = 4, max_e = 7), 
              aggte(reg_attgt, type = "simple", na.rm = T, min_e = 8, max_e = 15), 
              aggte(reg_attgt, type = "group", na.rm = T, max_e = 15), 
              aggte(reg_attgt, type = "group", na.rm = T, min_e = 4, max_e = 7), 
              aggte(reg_attgt, type = "group", na.rm = T, min_e = 8, max_e = 15)))
}

dim.list <- dim.list %>% 
  `names<-`(dimension)


reg_dta <- bind_rows(foreach(dim = names(dim.list)[1:5]) %do% {
  
  data.frame(coef = dim.list[[dim]][[1]]$att.egt, 
             se = dim.list[[dim]][[1]]$se.egt) %>% 
    mutate(time = c(-9:15)) %>% 
    arrange(time) %>%
    mutate(ci_low = coef - 1.96 * se, 
           ci_high = coef + 1.96 * se) %>% 
    mutate(dim = dim)
}) %>% 
  mutate(group = ifelse(str_detect(dim, 'nrc_'), 'Emotion', 
                        ifelse(str_detect(dim, 'per_'), 'Self-focus', 
                               ifelse(str_detect(dim, 'soc_'), 'Social processes', 
                                      ifelse(str_detect(dim, 'cog_'), 'Cognitive processes', 
                                             'Personal concerns'))))) %>% 
  mutate(dim = gsub('.*?_', '', dim)) %>% 
  mutate(dim = factor(dim, levels = rev(unique(dim))), 
         group = factor(group, levels = rev(unique(group)))) %>% 
  mutate(ci_low = coef - 1.96 * se, 
         ci_high = coef + 1.96 * se, 
         t = coef / se, 
         p = 1 - cdf(StudentsT(df = 1500), abs(t)) + cdf(StudentsT(df = 1500), -abs(t)), 
         star = ifelse(p < 0.001, 
                       "***", 
                       ifelse(p < 0.01, 
                              "**",
                              ifelse(p < 0.05, 
                                     "*", 
                                     ""))))

reg_dta <- reg_dta %>% 
  mutate(cate = ifelse(dim %in% c('anger', 'disgust', 'fear', 'sadness'), 
                       'Negative emotion', 
                       'Positive emotion'), 
         cate = factor(cate, levels = c('Positive emotion', 'Negative emotion'))) 

ggplot(data = reg_dta, aes(x = time, y = coef)) + 
  geom_hline(yintercept = 0, color = "grey50", lwd = 0.3) +
  geom_vline(xintercept = -1, color = "grey70", lwd = 0.25, linetype = 'dashed') +
  geom_xspline(aes(x = time, y = coef, group = dim, col = cate, size = 'basic'),
               alpha = 0.6) +
  geom_xspline(aes(x = time, y = coef, group = dim, col = cate, size = 'star'), 
               reg_dta %>% filter(dim == 'fear' & time >= 3 & time <= 15), # 3-11,13-15
               alpha = 1) +
  geom_xspline(aes(x = time, y = coef, group = dim, col = cate, size = 'star'), 
               reg_dta %>% filter(dim == 'sadness' & time >= 3 & time <= 15), # 5-15
               alpha = 1) +
  geom_xspline(aes(x = time, y = coef, group = dim, col = cate, size = 'star'), 
               reg_dta %>% filter(dim == 'disgust' & time >= 0 & time <= 15), 
               alpha = 1) +
  geom_xspline(aes(x = time, y = coef, group = dim, col = cate, size = 'star'), 
               reg_dta %>% filter(dim == 'anger' & time >= 1 & time <= 15), 
               alpha = 1) +
  geom_xspline(aes(x = time, y = coef, group = dim, col = cate, size = 'star'), 
               reg_dta %>% filter(dim == 'joy' & time >= 2 & time <= 14),  # 2-12
               alpha = 1) + 
  geom_text_repel(aes(x = time, y = coef, label = str_to_title(dim), col = cate), 
                  reg_dta %>% 
                    filter(time == 15) %>% 
                    mutate(time = time + 0.5) %>% 
                    filter(!dim %in% c('anti', 'trust', 'surprise')), 
                  min.segment.length = 50, size = 3, 
                  hjust = 0, max.overlaps = 50, direction = 'y', vjust = 0.5, 
                  show.legend = F) + 
  scale_color_manual(values = c("#9632B8", "#357EBD")) +
  scale_size_manual(values = c(0.3, 0.6)) +
  scale_x_continuous(limits = c(-9, 20.5), expand = c(0.02, 0.02), breaks = seq(-5, 15, 5)) + 
  scale_y_continuous(limits = c(-0.06, 0.035), expand = c(0.002, 0.002), 
                     breaks = seq(-0.06, 0.03, 0.03)) + 
  xlab('Weeks before (-) / after (+) vaccination') + 
  ylab('Sentiment change in s.d.') + 
  theme_classic() + 
  theme(aspect.ratio = 0.6,
        text = element_text(size = 14),
        line = element_line(linewidth = 0.3),
        plot.title = element_text(size = 12.5), 
        axis.title = element_text(size = 12),
        strip.background = element_blank(),
        legend.background = element_blank(), 
        legend.direction = 'vertical',
        legend.position = c(0.25, 0.25),
        legend.key.size= unit(0.5, "cm"), 
        legend.text = element_text(size = 10)) + 
  guides(color = guide_legend(''), 
         size = guide_none())

ggsave('fig/Fig2b-Emotion.pdf', width = 4, height = 3.5)



