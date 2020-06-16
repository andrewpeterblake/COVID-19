library(tidyverse)
library(readr)

nine <- c("China","Indonesia","South Africa","Ghana","Morocco","Sierra Leone","Jordan")

broad <- c("Afghanistan", "Bangladesh", "Burma", "Democratic Republic of Congo", "Congo (Kinshasa)",
           "Ethiopia", "India", "Iraq", "Kenya", "Kyrgyzstan", "Lebanon", "Liberia", "Malawi", 
           "Mozambique", "Nepal", "Nigeria", "Occupied Palestinian Territories", "Pakistan", 
           "Rwanda", "Somalia", "Sudan", "South Sudan", "Syria", "Tajikistan", "Tanzania", 
           "Uganda", "Yemen", "Zambia", "Zimbabwe")

whr <- broad
d   <- read_csv("time_series_covid19_confirmed_global.csv")

cc <- read_csv("time_series_covid19_confirmed_global.csv") %>% 
  select(-Lat, -Long) %>% 
  unite("Area", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  pivot_longer(names_to = "Date", values_to = "Cases", cols=-Area) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Cases=as.numeric(Cases)) %>% 
  mutate(Cases = na_if(Cases, 0)) %>%
  filter(Area %in% whr, !is.na(Cases)) %>%
  group_by(Area) %>%
  mutate(`Days since Xth case` = 1:n()) %>%
  ungroup()

dd <- read_csv("time_series_covid19_deaths_global.csv") %>%
  select(-Lat, -Long) %>% 
  unite("Area", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  filter(Area %in% whr) %>% 
  pivot_longer(names_to = "Date", values_to = "Deaths", cols=-Area) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Deaths=as.numeric(Deaths)) %>% 
  mutate(Deaths = na_if(Deaths, 0)) %>%
  filter(!is.na(Deaths)) %>%
  group_by(Area) %>%
  mutate(`Days since Xth death` = 1:n()) %>%
  ungroup()

pdeaths <- ggplot(filter(dd, `Days since Xth death`<36)) + 
  geom_line(aes(x=`Days since Xth death`, y=Deaths, group=Area, color=Area), show.legend = TRUE) +
  geom_point(aes(x=`Days since Xth death`, y=Deaths, group=Area, color=Area), show.legend = FALSE) +
  scale_y_log10(limits = c(1,150)) +
  labs(x="Days since first death", y="Deaths (log scale)", title="Number of deaths") + 
  theme_light()

pcases <- ggplot(filter(cc, `Days since Xth case`<36)) + 
  geom_line(aes(x=`Days since Xth case`, y=Cases, group=Area, color=Area), show.legend = TRUE) +
  geom_point(aes(x=`Days since Xth case`, y=Cases, group=Area, color=Area), show.legend = FALSE) +
  scale_y_log10(limits = c(1,5000)) + 
  labs(x="Days since first case", y="Cases (log scale)", title="Recorded cases") + 
  theme_light()

pcases
pdeaths

library(plotly)

ggplotly(pdeaths)
ggplotly(pcases)

#Deaths = na_if(Deaths, 1),
#Deaths = na_if(Deaths, 2), 
#Deaths = na_if(Deaths, 3),
#Deaths = na_if(Deaths, 4),
#Deaths = na_if(Deaths, 5),
#Deaths = na_if(Deaths, 6),
#Deaths = na_if(Deaths, 7),
#Deaths = na_if(Deaths, 8),
#Deaths = na_if(Deaths, 9),
#Deaths = na_if(Deaths, 10)