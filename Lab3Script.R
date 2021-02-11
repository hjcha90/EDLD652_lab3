
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
library(albersusa)
#library(colorblindr)


hprod <- import(here("data", "honeyproduction.csv")) %>% 
  clean_names() %>% 
  as_tibble()

#adjusted labels to where CA,  OR, WA end up after reordering
#CA ends up at WY, OR at NY, WA at ND
states <- unique(hprod$state)
label_color <- ifelse(states == "WY" |states == "NY" | states == "ND", 
                      "#C55644",
                      "gray30")

label_face <- ifelse(states == "WY" |states == "NY" | states == "ND", 
                     "bold",
                     "plain")

    
#plot1 heatmap needs scale or binning adjusted? too many values
plot1 <- hprod %>% 
  mutate(production=totalprod/10000) %>% 
  ggplot(aes(year, fct_reorder(state, production)))+
  geom_tile(aes(fill=production))+
  scale_fill_viridis_c(option="magma")+
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.text.y = element_text(color = label_color,
                                  face = label_face)) 

  
plot1 

#plot2 is VERY hard to read; do not recommend using
plot2 <- hprod %>% 
  mutate(production=totalprod/10000) %>%
  ggplot(aes(year, production)) +
  geom_line(aes(color = state))
plot2