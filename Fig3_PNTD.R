#####FIGURE: EVIDENCE OF MAYARO VIRUS IN HOSPITAL-BASED SURVEILLANCE####

#Libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

rm(list=ls())

#LOAD DATA
all <- read_excel("data/infection_detection.xlsx", 
                  sheet = "surveillance")
data <- filter(all, places == "hospital" )
conf <- data.frame(Hmisc::binconf(data$positive , data$total), method = "exact")
dat <- cbind(data,conf)
dat$year <- factor(dat$year_max, levels = rev(sort(unique(dat$year_max))))
dat$test <- dat$diagnostic_method
dat$test[dat$test %in% c( "IgM ELISA", 
                          "IgM MAC-ELISA",
                          "IgM ELISA and HI",
                          "IgM EIA-ICC ELISA",
                          "IgM MAC-ELISA and HI",
                          "IgM-ELISA")] <- 'IgM' 
dat$test[dat$test %in% c("Culture, Immunofluoresence assay, RT-PCR",
                         "Culture, RT PCR, IgM ELISA",
                         "Culture, RT-PCR, seroconversion of IgM")] <- 'Isolate/PCR/IgM' 
dat$test[dat$test %in% c("HI seroconversion" )] <- 'HI' 
dat$test[dat$test %in% c("RT-PCR or IgM ELISA")] <- 'PCR/IgM' 
dat$test[dat$test %in% c("RT-PCR")] <- 'PCR' 
dat$zone[dat$zone == 'mixed'] <- 'unknown'

dat <- filter (dat, test != 'HI')

p1 <-
  ggplot(data = dat, aes(x = year, y = PointEst))+
  geom_pointrange(aes(x = year, y = PointEst, ymin = Lower, ymax = Upper, fill = test),
                  position=position_jitter(width=0.5),
                  colour = 'black', shape = 21, size = 2) +
  theme_classic(40) +
  labs(fill = "") +
  xlab("") +
  ylab("proportion of positives") +
  facet_grid(Admin0 ~., scales = 'free', space = 'free') +
  coord_flip() +
  theme(legend.position = 'bottom') +
  theme(strip.text.y = element_text(angle = 0)) +
  # ylim (c(0,.4)) +
  ggtitle("Hospital based studies")


cross_sectional <- read_excel("data/infection_detection.xlsx", sheet = "Cross_sectional2") 
cross_sectional <- as.data.frame(cross_sectional)
conf <- data.frame(Hmisc::binconf(cross_sectional$positive , cross_sectional$total), method = "exact")
crosssect <- cbind(cross_sectional,conf)


###
crosssect$diagnostic_method[crosssect$diagnostic_method == "IgG ELISA and PRNT"] <- 'ELISA and PRNT'
crosssect$year <- factor(crosssect$year_max, levels = rev(unique(crosssect$year_max)))
dat2 <- crosssect
dat2$test <- dat2$diagnostic_method

p2 <-
  ggplot(dat2)+
  geom_pointrange(aes(x = year, y = PointEst, ymin = Lower, ymax = Upper, fill = test),
                  position=position_jitter(width=0.5),
                  colour = 'black', shape = 21, size = 2) +
  theme_classic(40) +
  labs(fill = "") +
  xlab("") +
  ylab("seroprevalence") +
  facet_grid(Admin0 ~., scales = 'free', space = 'free') +
  coord_flip() +
  theme(legend.position = 'bottom') +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_fill_manual(values = c('#66c2a5', '#fc8d62', '#8da0cb')) +
  ylim (c(0,1))+
  ggtitle("Population based studies")





tiff(file = "Figs/Fig3_mayv_humans.tiff", bg = "transparent",
     width = 1500 * 1.2, height = 780 * 1.5)

plot_grid(p1, p2, nrow = 1, labels = c('A', 'B'), label_size = 30)

dev.off()




