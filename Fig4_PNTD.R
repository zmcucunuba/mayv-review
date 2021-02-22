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
dat$Genus[dat$Genus == "No describe"] <- "Unknown"
dat$Genus[dat$Genus == "No describe"] <- "Unknown"


dat_primates <- dat %>% filter(Order == 'Primates')
la <- length(unique(dat_primates$Genus))
p1 <-
  ggplot(dat_primates) +
  geom_pointrange(aes(y = Genus, x = PointEst, xmin = Lower, xmax = Upper, fill = admin0),
                  position=position_jitter(height=0.2),
                  colour = 'black', shape = 21, size = 2) +
  
  cowplot::theme_half_open(55) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(family_new  ~ . , scales= 'free_y', space="free") +
  theme(strip.text.y = element_text(angle = 0)) +
  xlab("Seroprevalence") +  ylab("Genus") +
  theme(legend.position = 'none') +
  ggtitle(label = "", subtitle = "(Family)") +
  
  labs(fill = '') + 
  theme(plot.subtitle = element_text(color = "black", hjust=2, face="bold")) +
  scale_fill_viridis_d() +
  coord_cartesian(xlim = c(0,1))

non_primats <- dat %>% filter(Order != 'Primates')

p2 <-
  ggplot(non_primats) +
  cowplot::theme_half_open(55) + 
  geom_pointrange(aes(y = Order, x = PointEst, xmin = Lower, xmax = Upper, fill = admin0),
                  position=position_jitter(height=0.2),
                  colour = 'black', shape = 21, size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Class~., scales= 'free_y', space="free") +
  theme(strip.text.y = element_text(angle = 0)) +
  xlab("Seroprevalence") +  ylab("Order") +
  theme(legend.position = 'right') +
  labs(fill = '') +
  theme(plot.subtitle = element_text(color = "black", hjust=2, face="bold")) +
  ggtitle(label = "", subtitle = "(Class)") +
  scale_fill_viridis_d() +
  coord_cartesian(xlim = c(0,1))




png(file = "figs/Fig4_mayv_animals.tiff", 
    # bg = "transparent",
    width = 1200 * 2.5, height = 1080 * 2)
plot_grid(p1, NULL, p2, rel_widths = c(1, 0.2, 1.5), 
          nrow = 1, labels = c('A', "",  'B'), label_size = 50, align = "hv")
dev.off()



