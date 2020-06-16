#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)

library(tidyverse)
library(ggthemes)
library(lubridate)
library(readr)

fls <- list.files(".","ft_excess_deaths.csv")
uu  <- 100
if(length(fls) == 1) {
  fls.m <- file.mtime(fls)
  uu    <- as.numeric(difftime(Sys.time(), fls.m, units="hours"))
}

whn <- paste(day(Sys.Date()), month(Sys.Date(), label=T, abbr=F), year(Sys.Date()))
sou <- paste("Source: https://github.com/Financial-Times/coronavirus-excess-mortality-data, retrieved", whn)

fil <- "ft_excess_deaths.csv"
URL <- "https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/"
if(uu > 4) download.file(paste0(URL, fil), fil) 

ft <- read_csv(fil) %>% 
  group_by(country) %>% 
  filter(year > 2014) %>%
  mutate(n_regions = length(unique(region))) %>%
  mutate(sumexcess = sum(excess_deathsna.rm=TRUE)) %>% 
  ungroup() 

Countries = sort(unique(ft$country))

# labels
key <- "Recent data -- light grey; expected deaths -- dark grey; deaths 2020 -- red"

# Define UI for application that draws deaths data
ui <- fluidPage(theme = shinytheme("superhero"),
                
      # Application title
      titlePanel(paste("FT COVID-19 deaths data")),
      sidebarLayout(
        sidebarPanel(
          selectInput("chosen", 
                      "Choose country:", 
                      choices  = Countries,
                      multiple = FALSE,
                      selected = Countries[1]),
          checkboxInput("regions", "Show regions (where available)", FALSE), 
          checkboxInput("auto", "Auto-shade (WARNING: easily confused)", FALSE), 
          width = 3),
        mainPanel(
          plotOutput("CountryPlot", height="700px"), 
          width = 9)
          )
)

# Define server logic
server <- function(input, output) {
  
  output$CountryPlot <- renderPlot({

    dat  <- filter(ft, country %in% input$chosen)     
    dats <- dat %>% 
      filter(year == 2020) %>%
      group_by(region) %>%
      mutate(ispos = if_else(excess_deaths > 0, 1, 0), d = ispos-lag(ispos)) %>%
      mutate(d = replace_na(d, 0), d = if_else(d<0, 0, d), sd = cumsum(d), md = max(sd)) %>%
      ungroup() %>%
      filter(sd >= md) 
    
    if(!input$regions) {
      if (dat$country[1] %in% dat$region) {
      dat  <- dat %>% 
        filter(country == region | n_regions == 1)
      dats <- dats %>% 
        filter(country == region | n_regions == 1)
      }
    }
    
    per <- "week"
    if(dat$period[1] == "month") per <- "month"
    
    p <- ggplot(dat) 
    x <- ""
    if (dats$sumexcess[1] > 0 & input$auto) {
      p <- p + geom_ribbon(data = dats, aes(x=get(per), ymin=expected_deaths, ymax=deaths), fill="blue", alpha=.22) 
      x <- "; excess deaths -- shaded"
    }
    p <- p + geom_line(aes(x=get(per), y=deaths, group=year), color = "grey77") +
      geom_line(data = filter(dat, year==2020), aes(x=get(per), y=expected_deaths, group=year), 
                color = "grey22", size = 1.2) +
      geom_line(data = filter(dat, year==2020), aes(x=get(per), y=deaths, group=year), 
                color = "red", size = 1.2) +
      facet_wrap(~region, scales = "free_y") + 
      theme_stata() +
      theme(legend.position = "none") + 
      labs(title    = "Deaths and expected deaths as collated by Financial Times",
           subtitle = paste0(key,x),
           caption  = sou, x = paste("Numbered", per, "of 2020"))
    p
  })
}

# Run application 
shinyApp(ui = ui, server = server)
