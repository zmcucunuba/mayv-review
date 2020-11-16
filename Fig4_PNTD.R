###Figure: Evidence of Mayaro virus in animals populations

rm(list = ls())

library(readxl)
library(ggplot2)
library(dplyr)

#Load data
animal <- read_excel("data/animals_amanecer10-19.xlsx", 
                     sheet = "ANIMALES")
animal <- as.data.frame(animal)
conf <- data.frame(Hmisc::binconf(animal$positive, animal$total), method = "exact")
surveillance <- cbind(animal,conf)
dat <- surveillance

dat$specie_new <- dat$specie
dat$family_new <- dat$family
dat$specie_new[is.na(dat$specie_new)] <- ''
dat$family_new[is.na(dat$family)]<-  ''
dat$Idnf <- factor(1:NROW(dat))
dat$admin0[dat$admin0 == 'Brasil'] <- 'Brazil'
dat$Class[dat$Class == 'Aves'] <- 'Birds'

p1 <-
ggplot(dat %>% filter(Order != 'Primates')) +
  geom_errorbarh(aes(y = Idnf, xmin = Lower,  xmax = Upper), height = .2, show.legend = FALSE) +
  geom_point(aes(y = Idnf, x = PointEst, fill = admin0), shape = 21, size = 4) + 
  theme_classic(25) +
  # theme_bw(25) + 
  theme(axis.text.x = element_text(angle = 0)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  facet_grid(Class+Order~., scales= 'free_y', space="free") +
  theme(strip.text.y = element_text(angle = 0)) +
  xlab("Seroprevalence") +  ylab("Order") +
  theme(legend.position = 'bottom') +
  labs(fill = '') +
  scale_fill_viridis_d() 

p2 <-
  ggplot(dat %>% filter(Order == 'Primates')) +
  geom_errorbarh(aes(y = Idnf, xmin = Lower,  xmax = Upper), height = .2, show.legend = FALSE) +
  geom_point(aes(y = Idnf, x = PointEst, fill = admin0), shape = 21, size = 4) + 
  theme_classic(25) +
  # theme_bw(25) + 
  theme(axis.text.x = element_text(angle = 0)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  facet_grid(family + Genus~., scales= 'free_y', space="free") +
  theme(strip.text.y = element_text(angle = 0)) +
  xlab("Seroprevalence") +  ylab("Family") +
  theme(legend.position = 'bottom') +
  labs(fill = '') +
  scale_fill_viridis_d()




  
png(file = "figs/Fig4.animals.png", 
    bg = "transparent",
    width = 1200, height = 1080)
plot_grid(p1, p2, nrow = 1, labels = c('A', 'B'), label_size = 30)
dev.off()



