devtools::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
devtools::install_github("clauswilke/colorblindr")

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
library(colorblindr)


hprod <- import(here("data", "honeyproduction.csv")) %>% 
  clean_names() %>% 
  as_tibble()


hprod2 <- hprod %>% 
  mutate(production=totalprod/10000)

plot1b <- ggplot(hprod2, aes(year, production)) + 
  geom_line(aes(group=state),color="grey") + 
  geom_line(data = hprod2 %>% 
            filter(state == "CA"| state == "OR"|state == "WA"), 
            aes(color=state),size=1) + 
  scale_color_viridis_d() +
  labs(title = "Total Honey Production by State, from 1998 to 2012",
       subtitle = "Highlighting the West Coast",
       x = "Year",
       y = "Honey Production (in 10,000lbs)") +
  theme_minimal()
plot1b

#plot2
colorblindr::cvd_grid(plot1b)


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