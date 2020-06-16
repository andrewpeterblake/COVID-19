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

library(countrycode)

fls   <- list.files(".","^time_series_covid19.*global.csv$")
uu <- 100
if(length(fls) == 2) {
  fls.m <- file.mtime(fls)
  u1    <- as.numeric(difftime(Sys.time(), fls.m[1], units="hours"))
  u2    <- as.numeric(difftime(Sys.time(), fls.m[2], units="hours"))
  uu    <- max(c(u1,u2))
}

dts  <- Sys.Date()
when <- paste(day(dts), month(dts, label=T, abbr=F), year(dts))

URL      <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
s_cases  <- "time_series_covid19_confirmed_global.csv"
s_deaths <- "time_series_covid19_deaths_global.csv"

if(uu > 4)  {
  download.file(paste0(URL, s_cases), s_cases)
  download.file(paste0(URL, s_deaths), s_deaths)
  }

s_cases <- read_csv(s_cases) %>% 
  select(-Lat, -Long) %>% 
  mutate(Continent = countrycode(`Country/Region`, origin = "country.name", destination = "continent", warn=FALSE)) %>%
  unite("Country", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  mutate(Country = as_factor(Country)) %>% 
  pivot_longer(names_to = "Date", values_to = "Vals", cols=-c(Country, Continent)) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Vals=as.numeric(Vals)) %>% 
  mutate(Vals = na_if(Vals, 0)) %>%
  filter(!is.na(Vals), !is.na(Continent)) %>% 
  group_by(Country) %>%
  mutate(Days = 0:(n()-1)) %>% 
  ungroup()

s_deaths <- read_csv(s_deaths) %>% 
  select(-Lat, -Long) %>% 
  mutate(Continent = countrycode(`Country/Region`, origin = "country.name", destination = "continent", warn=FALSE)) %>%
  unite("Country", c(`Country/Region`, `Province/State`), na.rm=TRUE) %>%
  mutate(Country = as_factor(Country)) %>% 
  pivot_longer(names_to = "Date", values_to = "Vals", cols=-c(Country, Continent)) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"), Vals=as.numeric(Vals)) %>% 
  mutate(Vals = na_if(Vals, 0)) %>%
  filter(!is.na(Vals), !is.na(Continent)) %>% 
  group_by(Country) %>%
  mutate(Days = 0:(n()-1)) %>% 
  ungroup()

Countries = sort(unique(as.character(s_cases$Country)))

# Define UI for application that plots COVID data
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel(paste("COVID-19 data")),
   sidebarLayout(
     sidebarPanel(
       selectInput("chosen", 
                   "Choose country:", 
                   choices  = Countries,
                   multiple = TRUE,
                   selected = "United Kingdom"),
       radioButtons("series", 
                    "Which series?", 
                    choices  = c("Cases", "Deaths"),
                    selected = "Cases") ,
       radioButtons("TotalorDaily", 
                    "Total or daily", 
                    choices  = c("Total", "Daily"),
                    selected = "Total"),
       br(),
       sliderInput("Av", "Average of how many periods:", min=1, max=10, value=1),
       checkboxInput("scales", "Country-specific scales", FALSE),
       checkboxInput("logscale", "Logscale", TRUE),
       checkboxInput("line", "Lines not bars", FALSE),
       width = 4),
     mainPanel(
       plotOutput("CountryPlot", height="700px"), 
       width = 8)
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$CountryPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     if (input$series == "Cases") { cc <- s_cases } else {  cc <- s_deaths }
     
     dat <- filter(cc, Country %in% input$chosen)     
     if (input$TotalorDaily == "Daily") {
       dat <- dat %>% 
         group_by(Country) %>% 
         mutate(Vals = Vals-lag(Vals,1)) %>%
         ungroup()
       }
     if (input$Av > 1) {
       dat <- dat %>% 
         group_by(Country) %>% 
         mutate(Vals = zoo::rollmean(Vals, input$Av, align='right', fill=NA)) %>% 
         ungroup()
       rider <- paste0("(", input$Av, "-day moving average)")
       } else {
         rider <- ""
       }
     
     maxy <- max(dat$Vals)
     
     svar <- "fixed"
     if (input$scales) svar <- "free_y"
     ls   <- ""
     if (input$logscale) ls <- "(log scale)"
     
     # draw plot
     p <- ggplot(dat, aes(x=Days, y=Vals, group=Country)) + 
       labs(x=paste("Days since first reported", tolower(input$series)), 
            y=paste("Number", ls), 
            title=paste("Recorded", tolower(input$TotalorDaily), tolower(input$series), rider),
            caption=paste("Source: Johns Hopkins CSSE, github.com/CSSEGISandData/COVID-19, retrieved", when)) + 
       theme_stata() +
       theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
       facet_wrap(~ Country, scales=svar) + theme(legend.position="none") 
     if (input$logscale) p <- p + scale_y_log10()
     if (!input$line) {p <- p + geom_col(aes(fill=Country), colour="white", position="dodge")} 
     else             {p <- p + geom_line(aes(color=Country))}
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
