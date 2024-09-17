setwd('~/vaccineTweets/')
library(foreach)
library(doParallel)
library(tidyverse)
library(distributions3)
library(ggsignif)
library(ggh4x)
library(did)
library(lubridate)
library(data.table)

source('script/compute.aggte.R')



# Loading weekly panel data -----------------------------------------------

dta.week <- readRDS('weekly_panel_rmVac.rds')

dta.week <- dta.week %>%
  mutate(emotion = hedono, 
         emotion_std = (emotion - mean(emotion, na.rm = T)) / sd(emotion, na.rm = T)
  ) %>%
  mutate(time = week_created - min(week_created) + 1) %>% 
  group_by(user_id) %>% 
  mutate(week_freq = sum(week_created <= 26)) %>% 
  ungroup()


# Heterogeneity across demography ------------------------------------------

users.demo <- readRDS('usersDemography_encrypted.rds')

dta.week <- dta.week %>% 
  filter(user_id %in% users.demo$user_id) %>% 
  left_join(users.demo, by = 'user_id')

dta.week <- dta.week %>%
  mutate(age_above40 = ifelse(!is.na(age) & age == "above40", 1, 0),
         age_19_39 = ifelse(!is.na(age) & age == "19_39", 1, 0),
         age_under18 = ifelse(!is.na(age) & age == "under18", 1, 0),
         female = ifelse(!is.na(gender) & gender == "female", 1, 0),
         male = ifelse(!is.na(gender) & gender == "male", 1, 0),
         race_white = ifelse(!is.na(race) & race == "white", 1, 0),
         race_black = ifelse(!is.na(race) & race == "black", 1, 0),
         race_asian = ifelse(!is.na(race) & race == "asian", 1, 0),
         race_hispanic = ifelse(!is.na(race) & race == "latino hispanic", 1, 0),
         race_other = ifelse(!is.na(race) & (race == "indian" | race == "middle eastern"), 1, 0))

dta.week <- dta.week %>% 
  filter(week_freq >= 6) %>%
  filter((tweet_freq > as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.05))) &
           (tweet_freq < as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.95))))

heter_dimension <- c("female", "male", 
                     "age_above40", "age_19_39", "age_under18",
                     "race_white", "race_black", "race_asian", "race_hispanic")

stopImplicitCluster()
registerDoParallel(cores = 8)

heter.list <- foreach(i = 1:length(heter_dimension)) %dopar% {
  
  heter <- heter_dimension[i]
  
  print(paste0("Start processing ", heter))
  
  dta.week$heter <- dta.week[[heter]]
  
  reg_attgt <- att_gt(yname = "emotion_std",
                      tname = "week_created",
                      idname = "user_id",
                      gname = "week_first", 
                      control_group = 'notyettreated',   # nevertreated
                      allow_unbalanced_panel = T, 
                      bstrap = F,
                      cband = F,
                      base_period = 'universal',   # varying
                      anticipation = 0,
                      data = dta.week %>% 
                        filter(heter == 1) %>% 
                        mutate(user_id = factor(user_id, unique(user_id)), 
                               user_id = as.integer(user_id)) %>% 
                        filter(week_created <= 27) %>%
                        mutate(week_first = ifelse(week_first > 27, 0, week_first)),
                      pl = T, 
                      cores = 6)
  
  print(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -2, max_e = 2))
  
  return(list(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -9, max_e = 15), 
              aggte(reg_attgt, type = "simple", na.rm = T, max_e = 15), 
              aggte(reg_attgt, type = "simple", na.rm = T, min_e = 4, max_e = 7), 
              aggte(reg_attgt, type = "simple", na.rm = T, min_e = 8, max_e = 15), 
              aggte(reg_attgt, type = "group", na.rm = T, max_e = 15), 
              aggte(reg_attgt, type = "group", na.rm = T, min_e = 4, max_e = 7), 
              aggte(reg_attgt, type = "group", na.rm = T, min_e = 8, max_e = 15)))
  
}

heter.list <- heter.list %>% 
  `names<-`(heter_dimension)

saveRDS(heter.list, 'heteroList.rds', compress = T)


# Heterogeneity across geography ------------------------------------------

party <- read_csv('Red_blue_states.csv') %>%
  mutate(Y2004 = ifelse(Y2004 == 'Bush', 'red', 'blue'),
         Y2008 = ifelse(Y2008 == 'Obama', 'blue', 'red'),
         Y2012 = ifelse(Y2012 == 'Obama', 'blue', 'red'),
         Y2016 = ifelse(Y2016 == 'Trump', 'red', 'blue'),
         Y2020 = ifelse(Y2020 == 'Biden', 'blue', 'red')) %>%
  mutate(party = (Y2004 == 'red') + (Y2008 == 'red') +
           (Y2012 == 'red') + (Y2016 == 'red') +
           (Y2020 == 'red'),
         party = ifelse(party >= 3, 'red', 'blue')) %>%
  left_join(readRDS('gadm36_USA_1_sf.rds') %>%
              mutate(NAME_1_Abb = gsub('.*?[.](.*?)', '\\1', HASC_1),
                     geometry = NULL) %>%
              as.data.frame() %>%
              dplyr::select(NAME_1, NAME_1_Abb),
            by = 'NAME_1') %>% 
  as.data.frame()

statecase <- read_csv('US_State_Case.csv') %>%
  filter(date >= '2020-10-01' & date <= '2021-01-01') %>%
  group_by(state) %>%
  summarise(cases_cum = sum(cases),
            deaths_cum = sum(deaths)) %>%
  left_join(read_csv('US_state_ACSpop.csv') %>%
              select(Geo_QName, total_population),
            by = c('state' = 'Geo_QName')) %>%
  mutate(cases_per = cases_cum / total_population * 1e6,
         deaths_per = deaths_cum / total_population * 1e6) %>%
  filter(state %in% party$NAME_1) %>%
  left_join(party[c('NAME_1', 'NAME_1_Abb')], by = c('state' = 'NAME_1')) %>%
  select(NAME_1_Abb, cases_per, deaths_per, cases_cum, deaths_cum)

stategdp <- read_csv('US_State_GDP.csv') %>% 
  mutate(state = ifelse(duplicated(state), 'District of Columbia', state)) %>% 
  left_join(party[c('NAME_1', 'NAME_1_Abb')], by = c('state' = 'NAME_1')) %>% 
  mutate(GDPper2021 = gsub('\\$|,', '', GDPper2021), 
         GDPper2021 = as.integer(GDPper2021)) %>% 
  select(NAME_1_Abb, GDPper2021)


dta.week <- dta.week %>%
  left_join(party[c('NAME_1_Abb', 'party')], by = c('admin1Abb' = 'NAME_1_Abb')) %>%
  left_join(statecase, by = c('admin1Abb' = 'NAME_1_Abb')) %>%
  left_join(stategdp, by = c('admin1Abb' = 'NAME_1_Abb'))

dta.week <- dta.week %>%
  filter(week_freq >= 6) %>%
  filter((tweet_freq > as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.05))) &
           (tweet_freq < as.numeric(quantile(.[!duplicated(.$user_id), ]$tweet_freq, 0.95))))

sample.list <- list(dta.week %>% filter(party == 'red'),
                    dta.week %>% filter(party == 'blue'),
                    dta.week %>% filter(GDPper2021 <= median(stategdp$GDPper2021, na.rm = T)),
                    dta.week %>% filter(GDPper2021 > median(stategdp$GDPper2021, na.rm = T)),
                    
                    dta.week %>% filter(cases_cum <= median(statecase$cases_cum, na.rm = T)),
                    dta.week %>% filter(cases_cum > median(statecase$cases_cum, na.rm = T)),
                    
                    dta.week %>% filter(deaths_cum <= median(statecase$deaths_cum, na.rm = T)),
                    dta.week %>% filter(deaths_cum > median(statecase$deaths_cum, na.rm = T)))


stopImplicitCluster()
registerDoParallel(cores = 8)

party.list <- foreach(i = 1:length(sample.list)) %dopar% {
  
  reg_attgt <- att_gt(yname = "emotion_std",
                      tname = "week_created",
                      idname = "user_id",
                      gname = "week_first",
                      control_group = 'notyettreated',   # nevertreated
                      allow_unbalanced_panel = T,
                      bstrap = F,
                      cband = F,
                      base_period = 'universal',   # varying
                      anticipation = 0,
                      data = sample.list[[i]] %>% 
                        mutate(user_id = factor(user_id, unique(user_id)), 
                               user_id = as.integer(user_id)) %>% 
                        filter(week_created <= 27) %>%
                        mutate(week_first = ifelse(week_first > 27, 0, week_first)),
                      pl = T, 
                      cores = 6)
  
  print(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -2, max_e = 2))
  
  return(list(aggte(reg_attgt, type = "dynamic", na.rm = T, min_e = -9, max_e = 15),
              aggte(reg_attgt, type = "simple", na.rm = T, max_e = 15),
              aggte(reg_attgt, type = "simple", na.rm = T, min_e = 4, max_e = 7),
              aggte(reg_attgt, type = "simple", na.rm = T, min_e = 8, max_e = 15),
              aggte(reg_attgt, type = "group", na.rm = T, max_e = 15),
              aggte(reg_attgt, type = "group", na.rm = T, min_e = 4, max_e = 7),
              aggte(reg_attgt, type = "group", na.rm = T, min_e = 8, max_e = 15)))
  
}


party.list <- party.list %>%
  `names<-`(c('red', 'blue', 'gdpBelow', 'gdpAbove',
              'caseCumBelow', 'caseCumAbove', 'deathCumBelow', 'deathCumAbove'))


saveRDS(party.list, 'partyList.rds', compress = T)



# Plot together -----------------------------------------------------------
party <- readRDS('partyList.rds')

heter <- readRDS('heteroList.rds')

i <- 4
att <- bind_rows(
  
  # Demography
  bind_rows(foreach(dim = names(heter)) %do% {
    data.frame(heter = dim,
               coef = heter[[dim]][[i]]$overall.att,
               se = heter[[dim]][[i]]$overall.se)
  }) %>%
    mutate(heter = c('Female', 'Male', '≥ 40', '19 - 39', '≤ 18',
                     'White people', 'African American', 'Asian', 'Hispanic'),
           group = c(rep('Gender', 2), rep('Ages', 3), rep('Race', 4))), 
  
  # Death rate before
  bind_rows(foreach(dim = names(party)) %do% {
    data.frame(heter = dim,
               coef = party[[dim]][[i]]$overall.att,
               se = party[[dim]][[i]]$overall.se)
  }) %>%
    mutate(heter = c('Red state', 'Blue state',
                     rep(c('Low', 'High'), 3)),
           group = rep(c('Red or blue state', 
                         'GDP per capita', 
                         'Number of infections',
                         'Number of deaths'), each = 2)) 
  
) %>%
  
  mutate(group = factor(group, levels = unique(group)),
         heter = factor(heter, levels = rev(unique(heter)))) %>%
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
  arrange(group, heter) %>%
  mutate(ci_low = sprintf('%.03f', ci_low),
         ci_high = sprintf('%.03f', ci_high), 
         ci_low = ifelse(as.numeric(ci_low) > 0, paste0(' ', ci_low), ci_low), 
         label = paste0(sprintf('%.03f', coef), 
                        ' [', ci_low, ', ', ci_high, ']'), 
         ci_low = as.numeric(ci_low), 
         ci_high = as.numeric(ci_high))


ggplot(att, aes(heter, coef)) + 
  geom_hline(yintercept = 0, col = 'grey50', lwd = 0.4) +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), col = 'grey30', show.legend = F) + 
  geom_linerange(aes(ymin = coef - se, ymax = coef + se), col = 'grey70', lwd = 1.5, show.legend = F) +
  geom_point(col = 'grey20', fill = 'white', size = 3, pch = 21, show.legend = F) + 
  geom_text(col = 'grey20', label = att$star,
            size = 4, nudge_x = 0.2, nudge_y = 0.006, show.legend = F) +
  geom_text(aes(heter, 0.215, label = label),
            size = 4) +
  coord_flip() + 
  scale_y_continuous('Sentiment change in s.d.', limits = c(-0.015, 0.3), 
                     breaks = seq(0, 0.16, 0.04), expand = c(0, 0)) + 
  xlab('Red or\nBlue state') + 
  theme_classic() + 
  theme(text = element_text(size = 15), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 14, hjust = 0.15, margin = margin(t = 6))) + 
  guides(x = guide_axis_truncated(trunc_lower = -0.015,
                                  trunc_upper = 0.162)) + 
  theme(strip.text.y.left = element_text(angle = 0, face = 'bold'),
        strip.background = element_rect(fill = '#F7F4F9', color = NA), 
        strip.placement = "outside", 
        strip.switch.pad.grid = unit(0.15, "in")) + 
  facet_grid2(group ~ ., switch = 'y', scales = 'free', space = 'free_y')

ggsave('fig/Fig3-Heter-medium.pdf', width = 8.5, height = 5.8)


