setwd('~/vaccineTweets/')
library(tidyverse)
library(did)
library(lubridate)
library(distributions3)
options(nwarnings = 10000)

source('script/compute.aggte.R')


# Matching vaccination rate to individual-level panel data -----------------

vac.supply <- read_csv('US_State_vacSupply.csv') %>%
  select(Province_State, Date, Vaccine_Type, FIPS,
         Doses_alloc, Doses_admin, Stage_One_Doses, Stage_Two_Doses) %>%
  rename(state = Province_State,
         date = Date) %>%
  filter(Vaccine_Type == 'All')

state.pop <- read_csv('US_State_ACSpop.csv') %>%
  dplyr::select(Geo_STUSAB, Geo_QName, total_population) %>%
  rename(admin1Abb = Geo_STUSAB,
         state = Geo_QName) %>%
  mutate(admin1Abb = toupper(admin1Abb))

state.case <- read_csv('US_State_Case.csv') %>%
  dplyr::select(state, date, cases_avg)

state.vaccrate <- vac.supply %>%
  left_join(state.pop, by = 'state') %>%
  left_join(state.case, by = c('state', 'date')) %>%
  mutate(one_dose_rate = Stage_One_Doses/total_population,
         two_dose_rate = Stage_Two_Doses/total_population) %>%
  dplyr::select(admin1Abb, date, one_dose_rate, two_dose_rate, cases_avg) %>%
  mutate(date = as.character(date)) %>%
  filter(one_dose_rate >= 0 & one_dose_rate <= 1) %>%
  filter(date >= '2021-01-01' & date <= '2021-12-31') %>%
  mutate(week = week(date),
         date = as.Date(date))

dta.week <- dta.week %>%
  filter(week_freq >= 6) %>%
  filter((tweet_freq > as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.05))) &
           (tweet_freq < as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.95))))

dta.rate <- dta %>% 
  filter(user_id %in% dta.week$user_id) %>% 
  dplyr::left_join(state.vaccrate %>%
                     select(admin1Abb, date, one_dose_rate) %>%
                     rename(date_first = date,
                            one_dose_rate_vac = one_dose_rate),
                   by = c('admin1Abb', 'date_first')) %>%
  dplyr::left_join(state.vaccrate %>%
                     select(admin1Abb, date, one_dose_rate) %>%
                     rename(created_at = date,
                            one_dose_rate_post = one_dose_rate),
                   by = c('admin1Abb', 'created_at')) %>%
  
  mutate(rate_vac_first = case_when(
    one_dose_rate_vac <= 0.05 ~ 1,
    one_dose_rate_vac > 0.05 & one_dose_rate_vac <= 0.1 ~ 2,
    one_dose_rate_vac > 0.1 & one_dose_rate_vac <= 0.15 ~ 3,
    one_dose_rate_vac > 0.15 & one_dose_rate_vac <= 0.2 ~ 4,
    one_dose_rate_vac > 0.2 & one_dose_rate_vac <= 0.25 ~ 5,
    one_dose_rate_vac > 0.25 & one_dose_rate_vac <= 0.3 ~ 6,
    one_dose_rate_vac > 0.3 & one_dose_rate_vac <= 0.35 ~ 7,
    one_dose_rate_vac > 0.35 & one_dose_rate_vac <= 0.4 ~ 8,
    one_dose_rate_vac > 0.4 & one_dose_rate_vac <= 0.45 ~ 9,
    one_dose_rate_vac > 0.45 & one_dose_rate_vac <= 0.5 ~ 10,
    one_dose_rate_vac > 0.5 & one_dose_rate_vac <= 0.55 ~ 11,
    one_dose_rate_vac > 0.55 & one_dose_rate_vac <= 0.6 ~ 12,
    one_dose_rate_vac > 0.6 & one_dose_rate_vac <= 0.65 ~ 13,
    one_dose_rate_vac > 0.65 & one_dose_rate_vac <= 0.7 ~ 14,
    one_dose_rate_vac > 0.7 ~ 15)) %>%
  mutate(rate_vac_post = case_when(
    one_dose_rate_post <= 0.05 ~ 1,
    one_dose_rate_post > 0.05 & one_dose_rate_post <= 0.1 ~ 2,
    one_dose_rate_post > 0.1 & one_dose_rate_post <= 0.15 ~ 3,
    one_dose_rate_post > 0.15 & one_dose_rate_post <= 0.2 ~ 4,
    one_dose_rate_post > 0.2 & one_dose_rate_post <= 0.25 ~ 5,
    one_dose_rate_post > 0.25 & one_dose_rate_post <= 0.3 ~ 6,
    one_dose_rate_post > 0.3 & one_dose_rate_post <= 0.35 ~ 7,
    one_dose_rate_post > 0.35 & one_dose_rate_post <= 0.4 ~ 8,
    one_dose_rate_post > 0.4 & one_dose_rate_post <= 0.45 ~ 9,
    one_dose_rate_post > 0.45 & one_dose_rate_post <= 0.5 ~ 10,
    one_dose_rate_post > 0.5 & one_dose_rate_post <= 0.55 ~ 11,
    one_dose_rate_post > 0.55 & one_dose_rate_post <= 0.6 ~ 12,
    one_dose_rate_post > 0.6 & one_dose_rate_post <= 0.65 ~ 13,
    one_dose_rate_post > 0.65 & one_dose_rate_post <= 0.7 ~ 14,
    one_dose_rate_post > 0.7 ~ 15)) %>%
  
  as.data.table() %>% 
  summarise(vader = mean(vader, na.rm = T),
            hedono = mean(hedono, na.rm = T),
            hedono_1 = mean(hedono_1, na.rm = T),
            liwc_score = mean(liwc_score, na.rm = T),
            nrc_score = mean(nrc_score, na.rm = T), 
            .by = c(user_id, admin1Abb, source, criteria,
                    tweet_n, tweet_freq, tweet_days, tweet_daily,
                    rate_vac_first, rate_vac_post)) %>%
  mutate(emotion = hedono,
         emotion_std = (emotion - mean(emotion, na.rm = T)) / sd(emotion, na.rm = T)
  )

saveRDS(dta.rate, 'dtaRateFull_5bin.rds', compress = T)



# Dynamic treatment effect of vaccination on expressed sentiment -----------
# regressing on local vaccination rate increase 

dta.rate <- readRDS('dtaRateFull_bin5.rds')

reg_attgt <- att_gt(yname = "emotion_std",
                    tname = "rate_vac_post",
                    idname = "user_id",
                    gname = "rate_vac_first",
                    control_group = 'notyettreated', 
                    allow_unbalanced_panel = T,
                    bstrap = F,
                    cband = F,
                    base_period = 'universal', 
                    data = dta.rate %>%
                      mutate(rate_vac_post = ifelse(is.na(rate_vac_post), 1, rate_vac_post), 
                             rate_vac_first = ifelse(is.na(rate_vac_first), 1, rate_vac_first)) %>% 
                      mutate(user_id = factor(user_id, unique(user_id)),
                             user_id = as.integer(user_id)) %>%
                      filter(rate_vac_post <= 10) %>% 
                      group_by(user_id) %>%
                      mutate(intervalcount = n()) %>%
                      ungroup() %>%
                      filter(intervalcount >= 4) %>%
                      mutate(rate_vac_first = ifelse(rate_vac_first > 10, 0, rate_vac_first)),
                    pl = T,
                    cores = 10)

ggdid(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = 0, max_e = 10))

reg.tb <- data.frame(coef = aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = 0, max_e = 10)$att.egt,
                     se = aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = 0, max_e = 10)$se.egt) %>%
  mutate(x = 0:(nrow(.)-1)) %>%
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



# Plot together with tweets distribution ----------------------------------

dta.sum <- readRDS('dtaSum_Rate_bin5.rds')

bar.tb <- dta.sum %>%
  mutate(delta_rate = as.integer(delta_rate)) %>%
  arrange(delta_rate) %>%
  filter(!is.na(delta_rate) & delta_rate >= 0 & delta_rate <= max(reg.tb$x))

val <- lapply(bar.tb$count, function(y) seq(0, y/1000000, by = 0.01))
val.n <- unlist(lapply(1:length(val), function(i) length(val[[i]])))
mid <- unlist(lapply(0:(length(val.n)-1), function(i) rep(i, val.n[i+1])))
bar.tb <- data.frame(x = mid - 0.44,
                     xend = mid + 0.44,
                     y = unlist(val),
                     yend = unlist(val))


p1 <- ggplot(reg.tb %>% filter(x >= 0)) +
  geom_hline(yintercept = 0, col = 'grey') + 
  geom_bar(aes(x = x, y = coef), fill = '#4D8D74', alpha = 0.25, stat = 'identity') +
  geom_errorbar(aes(x, coef, ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                lwd = 0.6, width = 0.08, col = '#4D8D74', show.legend = F) +
  geom_point(aes(x, coef), col = '#4D8D74',
             pch = 23, size = 2, fill = '#4D8D74', stroke = 0.9, show.legend = F) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.0367, ymax = 0.0723), 
            fill = '#357EBD', alpha = 0.01) + 
  geom_hline(yintercept = 0.0545, col = '#357EBD', linetype = 'dashed', lwd = 0.3) + 
  annotate(geom = "text", x = -0.45, y = 0.063, 
           label = 'Average effect of vaccination', hjust = 0, col = '#3C5488') + 
  scale_x_continuous(expand = c(0, 0), limits = c(-0.7, 8.7)) +
  ylab('Sentiment change in SD') +
  theme_classic() +
  theme(aspect.ratio = 0.8,
        text = element_text(size = 16),
        axis.title.y = element_text(size = 14.5, margin = margin(r = 5)),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p2 <- ggplot(data = bar.tb %>% filter(mid >= 0),
             aes(x = x, xend = xend, y = y, yend = yend, color = y)) +
  geom_segment(show.legend = F) +
  geom_segment(data = bar.tb %>%
                 filter(mid >= 0) %>%
                 group_by(x) %>%
                 filter(y == max(y)) %>%
                 ungroup(),
               color = 'black') +
  scale_color_gradient2(low = "white", mid = "grey80", high = "grey60",
                        midpoint = max(bar.tb$y)/2) +
  scale_x_continuous('Change in local vaccination rate',
                     expand = c(0, 0), limits = c(-0.7, 8.7), breaks = 0:8,
                     labels = c('5% bin\nat vaccination', paste0('+', seq(5, 40, 5), '%'))) +
  scale_y_continuous('Number of Tweets\n(million)', expand = c(0, 0), breaks = c(0, 2, 4, 6)) +
  theme_classic() +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 11, margin = margin(t = 3)),
        axis.title = element_text(size = 14.5),
        axis.title.y = element_text(size = 14.5, margin = margin(r = 7)),
        aspect.ratio = 0.2)

p1 / p2

ggsave('fig/Fig4b-VaccinationRate.pdf', width = 6, height = 5.5)
