#------------covid_19 cases and deaths by population-------------

library(tidyverse)
library(readxl)

# ---------------------------------------------------------------

# COVID-19 source data is licensed under Creative Commons License: CC BY-NC-SA 3.0 IGO
# and sourced from WHO COVID-19 Dashboard. Geneva: World Health Organization, 2020. Available online: https://covid19.who.int/ (last cited:)
print(date())


global_data <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors = FALSE)
str(global_data)


# UN population source data is licensed under Creative Commons License: CC BY 3.0 IGO: http://creativecommons.org/licenses/by/3.0/igo/
# and sourced from United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1.
# https://population.un.org/wpp/Download/Standard/Population/
# download link "Total Population - Both Sexes (XLSX, 2.4 MB)"


world_population <- read_xlsx(file.choose(), sheet = "ESTIMATES")
str(world_population)

# All underlying r code and software in this project is subject to MIT License

# --------------------------------------------------------------

# join files, format data...............

#transform world population data and join with covid-19 global data ..............

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
#    mutate(Date_reported = as.POSIXct(ï..Date_reported),
    mutate(Date_reported = as.Date(ï..Date_reported),
           year = year(Date_reported),
           month = month(Date_reported),
           week = week(Date_reported),
           cases_per_capita = New_cases/population*1000000,
           deaths_per_capita = New_deaths/population*1000000) %>%
    rename(date_reported=Date_reported,
           country=Country,
           new_cases=New_cases,
           cumulative_cases=Cumulative_cases,
           new_deaths=New_deaths,
           cumulative_deaths=Cumulative_deaths,
           who_region=WHO_region) %>%
    select(year,month,week,date_reported,country,who_region,
           new_cases,cumulative_cases,new_deaths,cumulative_deaths,
           cases_per_capita,deaths_per_capita,population) %>%
    group_by(who_region,country,date_reported) %>%
    summarise_at(vars(cases_per_capita,deaths_per_capita),sum)
#    summarise_at(vars(cases_per_capita,deaths_per_capita),sum) %>%
#    filter(country %in% c("Canada","Barbados") & 
#               (date_reported == "2021-12-01" |
#               date_reported == "2021-12-02"))

gc() # garbage collection
#gc() # garbage collection

library(gridExtra)  # grid.arrange() function
library(scales)     # scales function

#parameters
selected_date <- "2021-12-01"
date_intervals <- "1 day"
date_range <- paste("From", selected_date, "to", today()-2,
                    "/ Plot interval:", date_intervals)
select_america <- factor(c("The United Kingdom", "Canada", "United States of America"),
                          levels=c("The United Kingdom", "United States of America", "Canada"))
select_europe <- factor(c("The United Kingdom", "Denmark", "Italy"),
                          levels=c("The United Kingdom", "Denmark", "Italy"))
                  #"Spain",
                  #"Australia"
                  #"Turkey"
region_group <- factor(c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO"),
                       levels=c("EURO", "AMRO", "AFRO", "EMRO", "SEARO", "WPRO"))

#plotting function ..................

plotting <- function(datafile, facetby, x, y, x_label, y_label, plot_heading){
    ggplot(datafile, aes(x, y, col = facetby)) +
        geom_point() +
        geom_segment(xend=x, yend=0) +
        facet_wrap(vars(facetby),labeller=as_labeller(group_names)) +
        labs(title=plot_heading, subtitle=date_range,
             caption="WHO COVID-19 data, UN world population data") +
        xlab(x_label) +
        ylab(y_label) +
        theme(legend.position="none") +
        theme(axis.text.x = element_text(angle=90,vjust=0.5,size=6),
              axis.text.y = element_text(size=8),
              axis.title.y = element_text(size=9),
              axis.title.x = element_blank(),
              plot.caption = element_text(size=6)) +
        scale_x_date(date_breaks=date_intervals, date_labels="%d %b %Y") +
        scale_y_continuous(labels=comma)
}

#plot execution ..........

group_names <- NULL

#plot cases and deaths for countries in UK and North America ...........

country_data <- clean_covid_file %>% 
    filter(date_reported >= selected_date) %>%
    filter(country %in% select_america)
country_data$facetby <-factor(country_data$country, levels=levels(select_america))

plot_cases <- plotting(country_data, country_data$facetby, country_data$date_reported, country_data$cases_per_capita, "Date Reported", "Daily Cases / million", "Covid-19 daily cases per million for UK vs. North America")
plot_deaths <- plotting(country_data, country_data$facetby, country_data$date_reported, country_data$deaths_per_capita, "Date Reported", "Daily Deaths / million", "Covid-19 daily deaths per million for UK vs. North America")
grid.arrange(plot_cases,plot_deaths,nrow=2)

#plot cases and deaths for countries in UK and Europe ...........

country_data <- clean_covid_file %>% 
    filter(date_reported >= selected_date) %>%
    filter(country %in% select_europe)
country_data$facetby <-factor(country_data$country, levels=levels(select_europe))

plot_cases <- plotting(country_data, country_data$facetby, country_data$date_reported, country_data$cases_per_capita, "Date Reported", "Daily Cases / million", "Covid-19 daily cases per million for UK vs. Europe")
plot_deaths <- plotting(country_data, country_data$facetby, country_data$date_reported, country_data$deaths_per_capita, "Date Reported", "Daily Deaths / million", "Covid-19 daily deaths per million for UK vs. Europe")
grid.arrange(plot_cases,plot_deaths,nrow=2)

#plot cases and deaths worldwide by region .................

group_names <- c("AFRO" = "Africa",
                 "AMRO" = "Americas",
                 "EMRO" = "Eastern Mediterrean",
                 "EURO" = "Europe",
                 "SEARO" = "South-East Asia",
                 "WPRO" = "Western Pacific")

region_data <- clean_covid_file %>% 
    filter(date_reported >= selected_date) %>%
    filter(who_region !="Other") %>%
    group_by(who_region,date_reported) %>%
    summarise_at(vars(cases_per_capita,deaths_per_capita),sum)
region_data$facetby <- factor(region_data$who_region, levels=levels(region_group))

plot_cases <- plotting(region_data, region_data$facetby,region_data$date_reported, region_data$cases_per_capita, "Date Reported", "Daily Cases / million", "Covid-19 daily cases per million worldwide")
plot_deaths <- plotting(region_data, region_data$facetby,region_data$date_reported, region_data$deaths_per_capita, "Date Reported", "Daily Deaths / million", "Covid-19 daily deaths per million worldwide")
grid.arrange(plot_cases, plot_deaths, nrow=2)

gc() # garbage collection

