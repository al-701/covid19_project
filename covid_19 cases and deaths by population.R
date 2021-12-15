#------------EDA on the WHP covid-19 dataset-------------

#set environment, import data, join files, format data...............

library(tidyverse)
library(readxl)
setwd("C:/Users/dolphin/Desktop/projects/COVID_19")

global_data <- read.csv("WHO-COVID-19-global-data.csv", stringsAsFactors = FALSE)
str(global_data)

world_population <- read_xlsx("WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES - Copy.xlsx", sheet = "ESTIMATES")
str(world_population)

#transform world population data and join covid global data ..............
names(world_population)[names(world_population) %in% c("Region, subregion, country or area *", "2020")] <- c("name","population")

world_population1 <- world_population %>%
    filter(Type == "Country/Area") %>%
    select(name,population) %>%
    mutate(name = trimws(name), population=as.numeric(population)*1000)

world_population1$name[world_population1$name == "United Kingdom"] <- "The United Kingdom"

covid_file <- inner_join(
    global_data, world_population1,
    by = c("Country" = "name"),
    suffix = c(".x", ".y")
    )

library(lubridate)

#format and clean covid file  ................

clean_covid_file <- covid_file %>% 
    mutate(Date_reported = as.POSIXct(ï..Date_reported),
           year = year(Date_reported),
           month = month(Date_reported),
           week = week(Date_reported),
           cases_per_capita = New_cases/population*100000,
           deaths_per_capita = New_deaths/population*100000) %>%
    rename(date_reported=Date_reported,
           country=Country,
           new_cases=New_cases,
           cumulative_cases=Cumulative_cases,
           new_deaths=New_deaths,
           cumulative_deaths=Cumulative_deaths,
           who_region=WHO_region) %>%
    select(year,month,week,date_reported,country,who_region,
           new_cases,cumulative_cases,new_deaths,cumulative_deaths,
           cases_per_capita,deaths_per_capita)

library(gridExtra)  # grid.arrange() function
library(scales)     # scales function

#parameters
selected_date <- "2021-11-13"

selected_country <- "The United Kingdom"

selected_six <- c("The United Kingdom",
                  "South Africa",
                  "Canada",
                  "United States of America",
                  "Barbados",
                  "Germany")
#"Australia"
#"United States of America"

region_names <- c("AFRO" = "Africa",
                "AMRO" = "Americas",
                "EMRO" = "Eastern Mediterrean",
                "EURO" = "Europe",
                "SEARO" = "South-East Asia",
                "WPRO" = "Western Pacific")

#visualization functions ..................
plot_country <- function(x,y,z){
    ggplot(data_by_country,aes(col=country)) +
        geom_col(aes(date_reported,x)) +
        facet_wrap(vars(country)) +
        labs(title=y, col="Country") +
        xlab("Date Reported") +
        ylab(z) +
        theme(legend.position="none", axis.text.x = element_text(angle=45)) +
        scale_y_continuous(labels=comma)
}
plot_region <- function(x,y,z){
    ggplot(data_by_region,aes(col=who_region)) +
        geom_col(aes(date_reported,x)) +
        facet_wrap(vars(who_region),labeller= as_labeller(region_names)) +
        labs(title=y, col="WHO Region") +
        xlab("Date Reported") +
        ylab(z) +
        theme(legend.position="none", axis.text.x = element_text(angle=45)) +
        scale_y_continuous(labels=comma)
}

#view cases and deaths in selected country ...........
data_by_country <- clean_covid_file %>% 
    filter(date_reported >= selected_date) %>%
    filter(country == selected_country) %>%
    group_by(country,date_reported) %>% 
    summarise_at(vars(cases_per_capita,deaths_per_capita),sum)

cases_by_country <- plot_country(data_by_country$cases_per_capita, "Covid-19 Cases per 100000 for 1 Country", "Cases/100000")
deaths_by_country <- plot_country(data_by_country$deaths_per_capita,"Covid-19 Deaths per 100000 for 1 Country", "Deaths/100000")
grid.arrange(cases_by_country,deaths_by_country,nrow=2)

#view cases and deaths in six countries ...........
data_by_country <- clean_covid_file %>% 
    filter(date_reported >= selected_date) %>%
    filter(country %in% selected_six) %>%
    group_by(country,date_reported) %>% 
    summarise_at(vars(cases_per_capita, deaths_per_capita),sum)

cases_by_country <- plot_country(data_by_country$cases_per_capita, "Covid-19 Cases per 100000 for 6 Countries", "Cases/100000")
deaths_by_country <- plot_country(data_by_country$deaths_per_capita,"Covid-19 Deaths per 100000 for 6 Countries", "Deaths/100000")
grid.arrange(cases_by_country,deaths_by_country,nrow=2)

#view cases and deaths worldwide by region .................
data_by_region <- clean_covid_file %>% 
    filter(date_reported >= selected_date) %>%
    filter(who_region !="Other") %>%
    group_by(who_region,date_reported) %>% 
    summarise_at(vars(cases_per_capita,deaths_per_capita),sum)

cases_by_region <- plot_region(data_by_region$cases_per_capita, "Covid-19 Cases per 100000 worldwide", "Cases/100000")
deaths_by_region <- plot_region(data_by_region$deaths_per_capita, "Covid-19 Deaths per 100000 worldwide", "Deaths/100000")
grid.arrange(cases_by_region,deaths_by_region,nrow=2)
