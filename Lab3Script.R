
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

#plot1 alternative
plot1b <- hprod %>% 
  mutate(production=totalprod/10000) %>%
  ggplot(aes(year, production)) +
  geom_line(aes(color = state), size = 2) +
  gghighlight(state == "CA" |state == "WA" | state =="OR") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal()
plot1b



#Download the file here denoting the region and division of each state.
hprod <- import(here("data", "honeyproduction.csv")) %>% 
  clean_names() %>% 
  as_tibble()

stregdiv <- import(here("data", "state_reg_div.xlsx")) %>% 
  clean_names() %>% 
  as_tibble()

#Join the file with your honey file.
p4_data <- hprod %>% 
  left_join(stregdiv, by = c("state" = "state_code")) %>% 
  rename(state_code = state, state = state.y)

p4_data %>% 
  group_by(state) %>% 
  summarize(avghoney= mean(totalprod))

p4_data %>% 
  summarize(avghoney= mean(totalprod))


#plot without highlights
p4_data %>% 
  group_by(state_code, region) %>% 
  summarize(avghoney= mean(totalprod)) %>%
  ggplot(aes(fct_reorder(state_code, avghoney), avghoney/1000)) + 
  geom_col(aes(fill=region)) + 
  coord_flip() + 
  theme_minimal() + 
  labs(title = "Average Honey Production by State, from 1998 to 2012",
       subtitle = "Highliting above-average performing states",
       x = "State",
       y = "Average Honey Production (in 1,000lbs)",
       fill = "Region")

#plot with highlights
p4_data %>% 
  group_by(state_code, region) %>% 
  summarize(avghoney= mean(totalprod)) %>%
  ggplot(aes(fct_reorder(state_code, avghoney), avghoney/1000)) + 
  geom_col(aes(fill=region)) + 
  coord_flip() + 
  gghighlight(avghoney > 4169086) + #average honey production across all state and year
  theme_minimal() + 
  labs(title = "Average Honey Production by State, from 1998 to 2012",
       subtitle = "Highliting above-average performing states",
       x = "State",
       y = "Average Honey Production (in 1,000lbs)",
       fill = "Region")
#it show that no state from Northeast produced above average amount of honey!