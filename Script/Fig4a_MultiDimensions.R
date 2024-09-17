setwd('~/vaccineTweets/')
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



# The effect of vaccination on multiple mental health dimensions ----------

dta.week <- dta.week %>% 
  mutate(soc = soc_family + soc_family, 
         con = con_work + con_leisure + con_home + con_money, 
         cog = cog_insight + cog_causal + cog_certainty + cog_different)


dimension <- colnames(dta.week)[c(43:56, 61:63)]

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


# Plot together -----------------------------------------------------------

i <- 4
att <- bind_rows(bind_rows(foreach(dim = names(dim.list)[15:17]) %do% {
                              data.frame(dim = dim,
                                         coef = dim.list[[dim]][[i]]$overall.att,
                                         se = dim.list[[dim]][[i]]$overall.se)
                            }) %>% 
                              mutate(dim = c('Social processes', 'Personal concerns', 'Cognitive processes')) %>% 
                              mutate(group = dim) %>% 
                              mutate(level = 'Major'), 
                  bind_rows(foreach(dim = names(dim.list)[1:14]) %do% {
                    data.frame(dim = dim,
                               coef = dim.list[[dim]][[i]]$overall.att,
                               se = dim.list[[dim]][[i]]$overall.se)
                  }) %>%
                    mutate(group = ifelse(str_detect(dim, 'soc_'), 'Social processes', 
                                          ifelse(str_detect(dim, 'cog_'), 'Cognitive processes', 
                                                 'Personal concerns')), 
                           dim = gsub('.*?_', '', dim)) %>% 
                    filter(!dim %in% c('discre', 'causal', 'religion', 'death')) %>% 
                    mutate(level = 'Minor')) %>% 
  mutate(dim = ifelse(level == 'Minor', paste0('  ', dim), dim), 
         dim = factor(dim, levels = rev(unique(dim)[c(2,10:13,1,4:5,3,6:9)])), 
         group = factor(group, levels = rev(unique(group)[c(2,1,3)])), 
         level = factor(level, levels = c('Minor', 'Major'))) %>% 
  arrange(group, dim) %>% 
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
                                     "")))) %>% 
  mutate(label = paste0(ifelse(coef > 0, paste0(' ', sprintf('%.03f', coef)), sprintf('%.03f', coef)), ' [',
                        ifelse(ci_low > 0, paste0(' ', sprintf('%.03f', ci_low)), sprintf('%.03f', ci_low)), ', ',
                        ifelse(ci_high > 0, paste0(' ', sprintf('%.03f', ci_high)), sprintf('%.03f', ci_high)), ']'))

muh_grob <- grid::rectGrob(x = 0, y = 1:13, 
                           gp = gpar(col = 'white', lwd = 2,
                                     fill = c(rep("#EADAF3", 4), '#D5B5E7', 
                                              rep("#DEEEFA", 2), '#B6CBE4', 
                                              rep('#E1F0E8', 4), '#A6D5CE')))

ggplot(att %>%
         mutate(index = factor(dim, levels = rev(unique(dim))),
                index = rev(as.integer(index)))) +
  geom_vline(xintercept = 0, col = 'grey') +
  geom_segment(aes(x = coef - se, xend = coef + se, y = dim, yend = dim,
                   col = group, size = level), alpha = 0.9, 
               lineend = 'round') + 
  geom_segment(aes(x = coef - 1.96 * se, xend = coef + 1.96 * se, y = dim, yend = dim,
                   col = group, size = level), alpha = 0.4, 
               lineend = 'round') + 
  geom_rect(aes(xmin = -0.072, xmax = 0.098, 
                ymin = index - 0.5, ymax = index + 0.5, 
                fill = level), lwd = 1.1, 
            col = 'white', alpha = 0.1, show.legend = F) + 
  geom_point(aes(coef, dim, size = level, col = group), pch = 21,
             fill = 'white', stroke = 1, show.legend = F) +
  coord_cartesian(clip = 'off') +
  annotation_custom(grob = muh_grob, xmin = -0.094, xmax = -0.05, ymin = 0, ymax = 1) + 
  geom_text(aes(-0.074, dim, label = dim), 
            att %>% filter(level == 'Major'), 
            hjust = 1, fontface = 'bold') + 
  geom_text(aes(-0.074, dim, label = str_to_title(dim)), 
            att %>% filter(level == 'Minor'), 
            hjust = 1) + 
  geom_text(aes(0.055, dim, label = label),
            att %>% filter(level == 'Major'), 
            hjust = 0, fontface = 'bold') +
  geom_text(aes(0.055, dim, label = label),
            att %>% filter(level == 'Minor'), 
            hjust = 0) +
  scale_size_manual(values = c(2, 4)) +
  scale_fill_manual(values = c('grey85', 'grey30')) + 
  scale_color_manual(values = c("#9632B8", '#357EBD', "#00A087", "#9632B8")) +
  scale_x_continuous('Sentiment change in s.d.', 
                     limits = c(-0.095, 0.098), expand = c(0, 0), 
                     breaks = c(-0.05, -0.025, 0, 0.025, 0.05)) +
  scale_y_discrete('', expand = c(0, 0)) +
  theme_classic() +
  theme(aspect.ratio = 0.6, 
        axis.title.y = element_text(margin = margin(l = 10)), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank()) + 
  guides(x = guide_axis_truncated(trunc_lower = -0.072,
                                  trunc_upper = 0.05)) + 
  theme(text = element_text(size = 16),
        axis.title.x = element_text(size = 14, hjust = 0.47, 
                                    margin = margin(t = 5)),
        axis.text.y = element_blank()) + 
  guides(color = guide_none(),
         shape = guide_legend(''),
         alpha = guide_none(), 
         size = guide_none())

ggsave('fig/Fig2d-MultiDimensions.pdf', width = 9.5, height = 5.5)



