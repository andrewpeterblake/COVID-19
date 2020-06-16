library(tidyverse)
library(ggthemes)
library(readr)
library(countrycode)

nine <- c("China","Indonesia","South Africa","Ghana","Morocco","Sierra Leone","Jordan")

broad <- c("Afghanistan", "Bangladesh", "Burma", "Democratic Republic of Congo", "Congo (Kinshasa)",
           "Ethiopia", "India", "Iraq", "Kenya", "Kyrgyzstan", "Lebanon", "Liberia", "Malawi", 
           "Mozambique", "Nepal", "Nigeria", "Occupied Palestinian Territories", "Pakistan", 
           "Rwanda", "Somalia", "Sudan", "South Sudan", "Syria", "Tajikistan", "Tanzania", 
           "Uganda", "Yemen", "Zambia", "Zimbabwe")

whr <- broad

all_c <- read_csv("time_series_covid19_confirmed_global.csv") %>% 
  select(-Lat, -Long) %>% 
  mutate(Continent = countrycode(`Country/Region`, origin = "country.name", destination = "continent")) %>%
  unite("Area", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  pivot_longer(names_to = "Date", values_to = "Cases", cols=-c(Area,Continent)) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Cases=as.numeric(Cases)) %>% 
  mutate(Cases = na_if(Cases, 0)) %>%
  filter(!is.na(Cases), !is.na(Continent)) %>%
  group_by(Area) %>%
  mutate(`Days since Xth case` = 1:n()) %>%
  ungroup()

all_d <- read_csv("time_series_covid19_deaths_global.csv") %>%
  select(-Lat, -Long) %>% 
  mutate(Continent = countrycode(`Country/Region`, origin = "country.name", destination = "continent")) %>%
  unite("Area", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  pivot_longer(names_to = "Date", values_to = "Deaths", cols=-c(Area,Continent)) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Deaths=as.numeric(Deaths)) %>% 
  mutate(Deaths = na_if(Deaths, 0)) %>%
  filter(!is.na(Deaths), !is.na(Continent)) %>%
  group_by(Area) %>%
  mutate(`Days since Xth death` = 1:n()) %>%
  ungroup()

cc <- read_csv("time_series_covid19_confirmed_global.csv") %>% 
  select(-Lat, -Long) %>% 
  mutate(Continent = countrycode(`Country/Region`, origin = "country.name", destination = "continent")) %>%
  unite("Area", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  pivot_longer(names_to = "Date", values_to = "Cases", cols=-c(Area,Continent)) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Cases=as.numeric(Cases)) %>% 
  mutate(Cases = na_if(Cases, 0)) %>%
  filter(Area %in% whr, !is.na(Cases)) %>%
  group_by(Area) %>%
  mutate(`Days since Xth case` = 1:n()) %>%
  ungroup()

dd <- read_csv("time_series_covid19_deaths_global.csv") %>%
  select(-Lat, -Long) %>% 
  mutate(Continent = countrycode(`Country/Region`, origin = "country.name", destination = "continent")) %>%
  unite("Area", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  filter(Area %in% whr) %>% 
  pivot_longer(names_to = "Date", values_to = "Deaths", cols=-c(Area,Continent)) %>% 
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
  theme_light() +
  facet_wrap(~Continent)

pcases <- ggplot(filter(cc, `Days since Xth case`<36)) + 
  geom_line(aes(x=`Days since Xth case`, y=Cases, group=Area, color=Area), show.legend = TRUE) +
  geom_point(aes(x=`Days since Xth case`, y=Cases, group=Area, color=Area), show.legend = FALSE) +
  scale_y_log10(limits = c(1,5000)) + 
  labs(x="Days since first case", y="Cases (log scale)", title="Recorded cases") + 
  theme_light() +
  facet_wrap(~Continent)

pcasesa <- ggplot(all_c) + 
  geom_line(aes(x=`Days since Xth case`, y=Cases, group=Area, color=Area)) +
  #geom_point(aes(x=`Days since Xth case`, y=Cases, group=Area, color=Area)) +
  scale_y_log10() + 
  labs(x="Days since first case", y="Cases (log scale)", title="Recorded cases") + 
  theme_stata() +
  theme(legend.position = "none") +
  facet_wrap(~Continent)
pcasesa

pdeathsa <- ggplot(all_d) + 
  geom_line(aes(x=`Days since Xth death`, y=Deaths, group=Area, color=Area), show.legend = TRUE) +
  scale_y_log10() +
  labs(x="Days since first death", y="Deaths (log scale)", title="Number of deaths") + 
  theme_stata() +
  theme(legend.position = "none") +
  facet_wrap(~Continent)
pdeathsa

