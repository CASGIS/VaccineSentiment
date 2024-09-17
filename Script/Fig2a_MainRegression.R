setwd('~/vaccineTweets/')
library(tidyverse)
library(did)
library(lubridate)
library(distributions3)
options(nwarnings = 10000)

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


# Dynamic treatment effect of vaccination on expressed sentiment ----------
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
                    data = dta.week %>%
                      filter(week_freq >= 6) %>%
                      filter((tweet_freq > as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.05))) &
                               (tweet_freq < as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.95)))) %>%
                      mutate(user_id = factor(user_id, unique(user_id)), 
                             user_id = as.integer(user_id)) %>% 
                      filter(week_created <= 27) %>%
                      mutate(week_first = ifelse(week_first > 27, 0, week_first)),
                    pl = T, 
                    cores = 12
)


agg.es <- aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -9, max_e = 15)

reg.tb <- data.frame(coef = agg.es$att.egt,
                     se = agg.es$se.egt) %>%
  mutate(time = c(-9:15)) %>%
  arrange(time) %>%
  mutate(ci_low = coef - 1.96 * se,
         ci_high = coef + 1.96 * se) %>%
  filter(time >= -9 & time <= 15)

p1 <- ggplot(data = reg.tb, aes(x = time, y = coef)) +
  geom_segment(aes(x = 0, xend = 0, y = 0.02, yend = 0.03), 
               col = '#3C5488', arrow = arrow(length = unit(0.03, "inches")), lwd = 0.2) +
  annotate(geom = "text", x = 0, y = 0.04, label = 'First dose of\nCOVID-19 vaccine', hjust = 1, col = '#3C5488') + 
  
  geom_segment(aes(x = 4, xend = 4, y = 0.05, yend = 0.06), 
               col = '#3C5488', arrow = arrow(length = unit(0.03, "inches")), lwd = 0.2) +
  annotate(geom = "text", x = 4, y = 0.07, label = 'Second dose of\nCOVID-19 vaccine', hjust = 1, col = '#3C5488') + 
  
  geom_rect(aes(xmin = 7.5, xmax = 15.5, ymin = -Inf, ymax = Inf), fill = 'grey97') +
  
  annotate(geom = "text", x = 11.5, y = 0.112, label = 'Average effect of\nfull vaccination', 
           hjust = 0.5, col = '#3C5488', fontface = 'bold') + 
  
  geom_hline(yintercept = 0, color = "grey50", lwd = 0.4) +
  geom_vline(xintercept = -1, color = "grey70", lwd = 0.3, linetype = 'dashed') + 
  
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), reg.tb %>% filter(time < 0),
                col = 'grey40', lwd = 0.55, width = 0.25) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), reg.tb %>% filter(time >= 0),
                col = '#357EBDB3', lwd = 0.55, width = 0.25) +
  
  geom_point(aes(x = time, y = coef), reg.tb %>% filter(time < 0), 
             col = 'grey40', fill = 'grey40', shape = 23, size = 1.5, show.legend = F) +
  geom_point(aes(x = time, y = coef), reg.tb %>% filter(time >= 0), 
             col = '#357EBD', fill = '#357EBD', shape = 23, size = 1.5, show.legend = F) +
  
  scale_x_continuous(expand = c(0.02, 0.02), limits = c(-9.5, 15.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.025, 0.125), breaks = seq(-0.02, 0.12, 0.02)) +
  xlab('Weeks before (-) / after (+) vaccination') +
  ylab('Sentiment change in s.d.') +
  theme_classic() +
  theme(text = element_text(size = 16),
        line = element_line(linewidth = 0.3),
        axis.title.y = element_text(size = 14.5, margin = margin(r = 7)),
        aspect.ratio = 0.65,
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.direction = 'horizontal',
        legend.position = 'top',
        legend.key.size = unit(0.8, "cm"))


# The number of tweets falling into different time bins

dta.sum <- readRDS('version2/dtaSum.rds')

val <- lapply(dta.sum[dta.sum$rel_week >= -9 & dta.sum$rel_week <= 15, ]$count,
              function(y) seq(0, y/1000000, by = 0.01))
val.n <- unlist(lapply(1:length(val), function(i) length(val[[i]])))
mid <- unlist(lapply(-9:15, function(i) rep(i, val.n[i+10])))
dta.sum.grad <- data.frame(x = mid - 0.4,
                           xend = mid + 0.4,
                           y = unlist(val),
                           yend = unlist(val))

p2 <- ggplot(data = dta.sum.grad,
             aes(x = x, xend = xend, y = y, yend = yend, color = y)) +
  geom_segment(show.legend = F) +
  geom_segment(data = dta.sum.grad %>%
                 group_by(x) %>%
                 filter(y == max(y)) %>%
                 ungroup(),
               color = 'black') +
  scale_color_gradient2(low = "white", mid = "grey80", high = "grey60",
                        midpoint = max(dta.sum.grad$y)/2) +
  scale_x_continuous('Weeks before (-) / after (+) vaccination',
                     expand = c(0.02, 0.02), limits = c(-9.5, 15.5), breaks = seq(-5, 15, 5)) +
  scale_y_continuous('Number of tweets\n(million)', breaks = c(0, 1, 2), expand = c(0, 0)) +
  theme_classic() +
  theme(text = element_text(size = 16),
        line = element_line(linewidth = 0.3),
        axis.title = element_text(size = 14.5),
        axis.title.y = element_text(margin = margin(r = 10)),
        aspect.ratio = 0.15)

p1 / p2

ggsave('fig/Fig2a-MainRegerssion.pdf', width = 6.5, height = 5)


