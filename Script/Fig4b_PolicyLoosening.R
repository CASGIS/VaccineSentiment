library(did)
library(lubridate)
library(data.table)
library(foreach)
library(doParallel)
library(tidyverse)

source('script/compute.aggte.R')


# Loading individual-level panel data -------------------------------------

dta.week <- readRDS('weekly_panel_rmVac.rds')

dta.week <- dta.week %>%
  mutate(emotion = hedono, 
         emotion_std = (emotion - mean(emotion, na.rm = T)) / sd(emotion, na.rm = T)
  ) %>%
  mutate(time = week_created - min(week_created) + 1) %>% 
  group_by(user_id) %>% 
  mutate(week_freq = sum(week_created <= 26)) %>% 
  ungroup()



# Linking panel data with policy stringency -------------------------------

policy <- read_csv('US_State_NPIpolicy.csv') %>% 
  as.data.frame() %>% 
  filter(CountryCode == "USA" & Jurisdiction == "STATE_TOTAL") %>% 
  mutate(date = as.Date(as.character(Date), format = "%Y%m%d")) %>% 
  filter(date >= as.Date("2021-01-01") & date < as.Date("2021-12-31"))

policy.Jan <- policy %>% 
  filter(date >= as.Date("2021-01-01") & date < as.Date("2021-01-31")) %>% 
  group_by(RegionCode) %>% 
  summarise(C4_Restrictions_Jan = round(mean(`C4_Restrictions on gatherings`)), 
            C6_Stay_Jan = round(mean(`C6_Stay at home requirements`)), 
            H6_Facial_Jan = round(mean(`H6_Facial Coverings`))) %>% 
  ungroup()

policy <- policy %>% 
  left_join(policy.Jan, by = 'RegionCode') %>% 
  mutate(gathering_policy = ifelse(`C4_Restrictions on gatherings` >= C4_Restrictions_Jan, 1, 0), 
         stayathome_policy = ifelse(`C6_Stay at home requirements` >= C6_Stay_Jan, 1, 0), 
         facemask_policy = ifelse(`H6_Facial Coverings` >= H6_Facial_Jan, 1, 0), 
         admin1Abb = str_sub(RegionCode, -2, -1)) %>%
  dplyr::select(date, admin1Abb, gathering_policy, stayathome_policy, facemask_policy) %>% 
  filter(!is.na(gathering_policy) & !is.na(stayathome_policy) & !is.na(facemask_policy)) %>% 
  mutate(week = week(date)) %>% 
  group_by(admin1Abb, week) %>% 
  summarise(gathering_policy = ifelse(sum(gathering_policy) > 3, 1, 0), 
            stayathome_policy = ifelse(sum(stayathome_policy) > 3, 1, 0), 
            facemask_policy = ifelse(sum(facemask_policy) > 3, 1, 0)) %>% 
  ungroup()


dta.week <- dta.week %>% 
  left_join(policy %>%
              rename(week_first = week,
                     gathering_vac = gathering_policy,
                     stayathome_vac = stayathome_policy,
                     facemask_vac = facemask_policy),
            by = c('week_first', 'admin1Abb')) %>%
  left_join(policy %>% 
              rename(week_second = week,
                     gathering_fully = gathering_policy, 
                     stayathome_fully = stayathome_policy, 
                     facemask_fully = facemask_policy), 
            by = c('week_second', 'admin1Abb')) %>% 
  left_join(policy %>%
              rename(week_created = week,
                     gathering_post = gathering_policy,
                     stayathome_post = stayathome_policy,
                     facemask_post = facemask_policy),
            by = c('week_created', 'admin1Abb'))



# The effect of policy loosening on vaccination-induced sentiment  ---------

dta.week <- dta.week %>% 
  filter(week_freq >= 6) %>%
  filter((tweet_freq > as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.05))) &
           (tweet_freq < as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.95))))

sample.list <- list(dta.week %>% filter(week_first > 27 | ((week_first <= 27) & (gathering_fully == 1))),
                    dta.week %>% filter(week_first > 27 | ((week_first <= 27) & (gathering_fully == 0))),
                    dta.week %>% filter(week_first > 27 | ((week_first <= 27) & (stayathome_fully == 1))),
                    dta.week %>% filter(week_first > 27 | ((week_first <= 27) & (stayathome_fully == 0))),
                    dta.week %>% filter(week_first > 27 | ((week_first <= 27) & (facemask_fully == 1))),
                    dta.week %>% filter(week_first > 27 | ((week_first <= 27) & (facemask_fully == 0))))


stopImplicitCluster()
registerDoParallel(cores = 10)

policy.list <- foreach(i = 1:length(sample.list)) %dopar% {
  
  reg_attgt <- att_gt(yname = "emotion_std",
                      tname = "week_created",
                      idname = "user_id",
                      gname = "week_first", 
                      control_group = 'notyettreated', 
                      allow_unbalanced_panel = T, 
                      bstrap = F,
                      cband = F,
                      base_period = 'universal', 
                      anticipation = 0,
                      data = sample.list[[i]] %>% 
                        mutate(user_id = factor(user_id, unique(user_id)), 
                               user_id = as.integer(user_id)) %>% 
                        filter(week_created <= 27) %>%
                        mutate(week_first = ifelse(week_first > 27, 0, week_first)),
                      pl = T, 
                      cores = 6)
  
  try <- try(print(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -2, max_e = 2)), 
             silent = T)
  
  if (class(try) == 'try-error') {
    return(NULL)
  } else {  
    return(list(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -9, max_e = 15), 
                aggte(reg_attgt, type = "simple", na.rm = T, max_e = 15), 
                aggte(reg_attgt, type = "simple", na.rm = T, min_e = 4, max_e = 7), 
                aggte(reg_attgt, type = "simple", na.rm = T, min_e = 8, max_e = 15), 
                aggte(reg_attgt, type = "group", na.rm = T, max_e = 15), 
                aggte(reg_attgt, type = "group", na.rm = T, min_e = 4, max_e = 7), 
                aggte(reg_attgt, type = "group", na.rm = T, min_e = 8, max_e = 15)))
  }
}

saveRDS(policy.list, 'policyList_loose_Jan.rds', compress = T)



# Plot differences --------------------------------------------------------

policy <- readRDS('policyList_loose_Jan.rds')

att <- bind_rows(foreach(i = 1:length(policy)) %do% {
  data.frame(coef = policy[[i]][[4]]$overall.att,
             se = policy[[i]][[4]]$overall.se)
}) %>%
  mutate(group = rep(c('No policy loosening', 'Policy loosening'), 3),
         group = factor(group, levels = unique(group)),
         policy = rep(c('Gathering\nprohibition', 'Stay-at-home', 'Facemask\nrequirement'), each = 2), 
         policy = factor(policy, levels = unique(policy)))

ggplot(att) +
  geom_hline(yintercept = 0, col = 'grey') + 
  geom_bar(aes(x = policy, y = coef, fill = group), width = 0.8, 
           position = position_dodge2(), stat ='identity', alpha = 0.25) +
  geom_errorbar(aes(policy, coef, ymin = coef - 1.96 * se, ymax = coef + 1.96 * se,
                    col = group), lwd = 0.6, width = 0.08, 
                position = position_dodge(width = 0.8), stat = 'identity') +
  geom_point(aes(policy, coef, col = group), pch = 19, fill = 'white', size = 2, 
             position = position_dodge2(width = 0.8), stat ='identity') +
  scale_y_continuous(expand = c(0.01, 0.005), breaks = seq(-0.03, 0.09, 0.03)) +
  scale_color_manual(values = c("#4D8D74", "#3C5488")) +
  scale_fill_manual(values = c("#4D8D74", "#3C5488")) +
  ylab('Sentiment change in s.d.') +
  xlab('') +
  theme_classic() +
  theme(aspect.ratio = 1,
        text = element_text(size = 16),
        legend.background = element_blank(), 
        legend.position = c(0.7, 0.18),
        legend.direction = 'vertical') +
  guides(color = guide_none(), 
         fill = guide_legend(''))

ggsave('fig/Fig4b-PolicyLoosening.pdf', width = 6, height = 5.5)

