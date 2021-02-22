
library(tidyverse)
library(cowplot)

rm(list = ls())


dat0 <- readRDS('mayv-foi/may-data.RDS')

datasets <- unique(dat0$survey)

prev <- data.frame()
for (i in datasets) 
{
  dati <- readRDS(paste0('mayv-foi/best/',i,  '.RDS'))
  dati <- dati$prevalence %>% filter(best == 'best1')
  prev <- rbind(prev, dati)

  rm(dati)
  
}


get_foi <- function(dat1, best_model = 'best1'){
  study1 <- dat1$dataset
  country <- unique(dat0$country[dat0$survey == study1])
  tsur    <- unique(dat0$tsur[dat0$survey == study1])
  test    <- unique(dat0$Test[dat0$survey == study1])
  foi  <- dat1$foi_cent_est %>% filter(best == best_model) %>% mutate(survey = study1)
  from <- length(unique(foi$year)) * .2
  years_select <- unique(foi$year)[from: length(unique(foi$year))]
  foi  <- filter(foi, year %in% years_select)
  foi$country  <- country
  foi$tsur     <- tsur
  foi$test     <- test
  return(foi)
}


foi <- data.frame()
for (i in datasets) 
{
  dat1 <- readRDS(paste0('mayv-foi/best/',i,  '.RDS'))
  foii <- get_foi(dat1)
  foi <- rbind(foi, foii)
  rm(dat1, foii)
  
}


surveys <- foi %>% group_by(country, tsur, survey, test)  %>% summarise(n=n()) %>%
  mutate(subtitle = paste(country, tsur))
surveys$subtitle[surveys$subtitle == 'Brazil 1970'] <- c('Brazil\n(1970a)', 'Brazil\n(1970b)')
surveys$subtitle[surveys$subtitle == 'Brazil 1972'] <- c('Brazil\n(1972a)', 'Brazil\n(1972b)')

prevs <- merge(prev, surveys, by = 'survey')
fois  <- merge(foi, surveys, by =c('country', 'tsur', 'survey', 'test'))

# foi$survey <- factor(foi$survey, levels = unique(prev$survey))
# prev$survey <- factor(prev$survey, levels = unique(prev$survey))


prevs <- filter(prevs, !test== 'HI', !subtitle == 'Ecuador \n(1997)' )
fois <- filter(fois, !test== 'HI', !subtitle == 'Ecuador \n(1997) '  )

colour_foi <- 'grey50'
colour_prev <- 'grey20'

fois$subtitle[fois$subtitle == "Trinidad and Tobago 1958"] <- "Trinidad\n&Tobago\n(1958)"
fois$subtitle[fois$subtitle == "Brazil 2007"]   <- "Brazil\n(2007)"
fois$subtitle[fois$subtitle == "Colombia 1960"] <-  "Colombia\n(1960)" 
fois$subtitle[fois$subtitle == "Colombia 1966"] <- "Colombia\n(1966)" 
fois$subtitle[fois$subtitle == "Ecuador 1997"]  <- "Ecuador\n(1997)" 
fois$subtitle[fois$subtitle == "Guyana 1956"]   <- "Guyana\n(1956)"
fois$subtitle[fois$subtitle == "Peru 2011"]     <- "Peru\n(2011)"
fois$subtitle[fois$subtitle == "Surinam 1964"]  <- "Surinam\n(1964)"
fois$subtitle[fois$subtitle == "Guyana 1958" ]  <- "Guyana\n(1958)" 


fois  <- fois %>% rename(Test = test)
prevs <- prevs %>% rename(Test = test)

pp1 <- 
  ggplot(fois) +
  geom_line(aes (x = year, y = median, colour = Test), size = 1.5) +
  geom_ribbon(aes(x = year, ymin = lower, ymax = upper, fill = Test), alpha = .3)+
  theme_bw(40) +
  coord_cartesian(ylim = c(0,.10)) +
  facet_wrap(~subtitle, nrow = 1, scales = 'free_x') +
  xlab('Year') + ylab('Force of Infection') +
  theme(legend.position = "none") +
  
  theme(strip.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # geom_text(aes(x = min(year), y = .1, label = subtitle, hjust = 0), size = 5) +
  theme(panel.grid.major = element_blank()) 
 



pp2 <- 
  ggplot(prevs) +
  geom_line(aes (x = age, y = medianv, colour = Test), size = 1.5) +
  geom_ribbon(aes(x = age, ymin = plower, ymax = pupper, fill = Test), alpha = .3)+
  geom_errorbar(aes (x = age, ymin = pobslo, ymax = pobsup, colour = Test), alpha = .7) + 
  geom_point(aes (x = age, y = pobs, fill = Test), pch = 21, ) + 
  theme_bw(40) +
  coord_cartesian(ylim = c(0,1), xlim = c(0, 45)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position = "bottom") +
  facet_wrap(~subtitle, nrow = 1) +
  xlab('Age (years)') + ylab('Seroprevalence') +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  theme(panel.grid.major = element_blank()) 
  




pp <- 
  plot_grid (pp1, NULL, pp2, rel_heights = c(1.3, 0.01, 1.2), 
             nrow = 3, labels = 'AUTO', label_size = 30, align = "v")



tiff(file = "figs/Fig5_mayv_force-of-infection.tiff", 
    bg = "transparent",
    width = 1500 * 1.5, height = 800 * 1.5)
pp
dev.off()

