rm(list = ls())

library(EpiEstim)
library(tidyverse)
library(lubridate)
library(readxl)
library(incidence)

dat1 <- read_excel('data/outbreak_okinawa1954.xlsx', sheet = 'outbreak')
incid1 <- rep(dat1$dates, dat1$n_cases)
incid_object1 <- incidence(incid1, interval = 7)

p1 <- plot(incid_object1) +
  theme_classic(30) +
  ggtitle('Santa Cruz, Bolivia 1954-1955 ') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))

dat2 <- read_excel('data/outbreak_belterra1978.xlsx', sheet = 'outbreak')
incid2 <- rep(dat2$dates, dat2$n_cases)
incid_object2 <- incidence(incid2, interval = 28)

p2 <- plot(incid_object2) +
  theme_classic(30) +
  ggtitle('Belterra, Brazil 1978') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1))





