#install.packages("gghighlight")

library(tidyverse)
library(here)
library(rio)
library(knitr)
library(janitor)
library(patchwork)
library(ggplot2)
library(forcats)
library(lubridate)
library(gghighlight)
library(ggtext)
#library(colorblindr)

hprod <- import(here("data", "honeyproduction.csv")) %>% 
  clean_names() %>% 
  as_tibble()

states <- unique(hprod$state)
label_color <- ifelse(states == "CA" |states == "OR" | states == "WA", 
                      "#C55644",
                      "gray30")

label_face <- ifelse(states == "CA" |states == "OR" | states == "WA", 
                     "bold",
                     "plain")

#plot1 heatmap needs scale or binning adjusted? too many values
plot1 <- hprod %>% 
  mutate(production=totalprod/10000) %>% 
  ggplot(aes(year, state))+
  geom_tile(aes(fill=production), color="white")+
  scale_fill_viridis_c(option="magma")+
  scale_x_continuous(expand = c(0, 0)) +
#next 2 lines not working
  theme(axis.text.y = element_text(color = label_color,
                                  face = label_face))
plot1 

#plot2 is VERY hard to read; do not recommend using
plot2 <- hprod %>% 
  mutate(production=totalprod/10000) %>%
  ggplot(aes(year, production)) +
  geom_line(aes(color = state))
plot2