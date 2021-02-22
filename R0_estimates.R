
library(tidyverse)
library(cowplot)

rm(list = ls())


dat0 <- readRDS('mayaro_foi/may-data.RDS')

datasets <- unique(dat0$survey)

prev <- data.frame()
for (i in datasets) 
{
  dati <- readRDS(paste0('mayaro_foi/best/',i,  '.RDS'))
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
  dat1 <- readRDS(paste0('mayaro_foi/best/',i,  '.RDS'))
  foii <- get_foi(dat1)
  foi <- rbind(foi, foii)
  rm(dat1, foii)
  
}


surveys <- foi %>% group_by(country, tsur, survey, test)  %>% summarise(n=n()) %>%
  mutate(subtitle = paste(country, tsur))
surveys$subtitle[surveys$subtitle == 'Brazil 1970'] <- c('Brazil 1970(a)', 'Brazil 1970(b)')
surveys$subtitle[surveys$subtitle == 'Brazil 1972'] <- c('Brazil 1972(a)', 'Brazil 1972(b)')

prevs <- merge(prev, surveys, by = 'survey')
fois  <- merge(foi, surveys, by =c('country', 'tsur', 'survey', 'test'))



life_ex <- read_csv('data/life-expectancy.csv')
surveys <- unique(fois$survey)

R0_estimates <- data.frame()

for ( i in surveys )
{
  foid <- filter(foi, survey == i)
  country_name <- foid$country[1]
  if (country_name == "Surinam") {country_name = 'Guyana'}
  if (country_name ==  "French Guiania") {country_name = "French Guiana" }
  
  year_survey  <- max(foid$year)
  life_expect_country <- as.numeric(filter(life_ex, Entity == country_name, Year == year_survey)[4])
  
  foid$median
  
  R0_endemic         <-  round(mean(foid$median) * life_expect_country /(1-exp(- mean(foid$median) * life_expect_country)),2)
  R0_endemic_lower   <-  round(mean(foid$lower) * life_expect_country /(1-exp(-mean(foid$lower) * life_expect_country)),2)
  R0_endemic_upper   <-  round(mean(foid$upper) * life_expect_country /(1-exp(-mean(foid$upper)  * life_expect_country)),2)
  
  R0_estimates <- rbind (R0_estimates, 
                         data.frame(survey = i, 
                                    country = country_name, 
                                    `Study Year` = unique(foid$tsur),
                                    Test = unique(foid$test),
                                    R0 = R0_endemic, 
                                    `95% CrI` = paste0(R0_endemic_lower,'-',  R0_endemic_upper)))
  
}

R0_estimates$country <- as.character(R0_estimates$country)
R0_estimates$country[R0_estimates$survey == 'Van Tongere 1965'] 	<- 'Surinam'

write_csv2(R0_estimates, 'R0_estimates_mayaro_rev.csv')

#### On;y for Colombia Groot

# p_infected <- max(prev$medianv[prev$survey == 'Groot et al 1964'])
# p_infected_lower <- max(prev$plower[prev$survey == 'Groot et al 1964'])
# p_infected_upper <- max(prev$pupper[prev$survey == 'Groot et al 1964'])
# I_o <- 0
# 
# R0_epidemic         <- round((log((1-p_infected)/(1-I_o)))/(I_o -p_infected),2)
# R0_epidemic_lower   <- round((log((1-p_infected_lower)/(1-I_o)))/(I_o -p_infected_lower), 2)
# R0_epidemic_upper   <- round((log((1-p_infected_upper)/(1-I_o)))/(I_o -p_infected_upper), 2)

