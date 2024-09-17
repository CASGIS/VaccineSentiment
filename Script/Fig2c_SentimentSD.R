setwd('~/vaccineTweets/')
library(tidyverse)
library(did)
library(lubridate)
library(distributions3)
options(nwarnings = 10000)

source('script/compute.aggte.R')

# Loading individual-level weekly panel data of expressed sentiment -------

dta.week <- readRDS('weekly_panel_rmVac.rds')

dta.week <- dta.week %>%
  mutate(emotion = hedono, 
         emotion_std = (emotion - mean(emotion, na.rm = T)) / sd(emotion, na.rm = T)
  ) %>%
  mutate(time = week_created - min(week_created) + 1) %>% 
  group_by(user_id) %>% 
  mutate(week_freq = sum(week_created <= 26)) %>% 
  ungroup()

# Dynamic treatment effect of vaccination on the standard deviation of expressed sentiment
reg_attgt <- att_gt(yname = "hedono_sd",
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
                    cores = 6
)

agg.es <- aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -9, max_e = 15)

reg.tb <- data.frame(coef = agg.es$att.egt,
                     se = agg.es$se.egt) %>%
  mutate(time = c(-9:15)) %>%
  arrange(time) %>%
  mutate(ci_low = coef - 1.96 * se,
         ci_high = coef + 1.96 * se) %>%
  filter(time >= -9 & time <= 15)

ggplot(data = reg.tb, aes(x = time, y = coef)) +
  geom_hline(yintercept = 0, color = "grey50", lwd = 0.3) +
  geom_vline(xintercept = -1, color = "grey70", lwd = 0.25, linetype = 'dashed') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), col = '#357EBD', lwd = 0.35, width = 0.25) +
  geom_point(aes(x = time, y = coef),
             col = '#357EBD', fill = '#357EBD', shape = 23, size = 0.9, show.legend = F) +
  scale_x_continuous(expand = c(0.02, 0.02), limits = c(-9.5, 15.5)) +
  xlab('Weeks before (-) / after (+) vaccination') +
  ylab('Change in sentiment s.d.') +
  theme_classic() +
  theme(aspect.ratio = 0.6,
        text = element_text(size = 14),
        line = element_line(linewidth = 0.3),
        plot.title = element_text(size = 12.5), 
        axis.title = element_text(size = 12),
        strip.background = element_blank(),
        legend.direction = 'horizontal',
        legend.position = 'top',
        legend.key.size= unit(0.8, "cm"))

ggsave('fig/Fig2c-sentimentSD.pdf', width = 4, height = 3.5)
