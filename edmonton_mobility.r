library(tidyverse)
library(RSocrata)
library(lubridate)

# Load Edmonton data
edmonton_raw <- read.socrata("https://data.edmonton.ca/resource/tq23-qn4m.json?$where=log_timstamp between '2020-01-01T00:00:00.000' and '2021-07-04T23:59:59.000'")

# Load Apple data from my Github repo
apple_raw <- read_csv("https://raw.githubusercontent.com/najsaqib/naj_lab/main/applemobilitytrends-2021-07-04.csv")

# Pre-process
## Civic data
edmonton_civic <- edmonton_raw %>%
        mutate(date = date(log_timstamp)) %>%
        filter(counter_configuration == "Pedestrian") %>%
        filter(counter_location_description %in% c("Funicular @ 100 St", "Keillor Point (EBD into Keillor Point/WBD out of Keillor Point)")) %>%
        group_by(date) %>%
        summarise(daily_ped = sum(as.numeric(total_pedestrian_count))) %>%
        filter(date >= "2020-01-13") %>%
        mutate(civic_index = (daily_ped/daily_ped[which.min(date)]*100)) %>%
        mutate(civic_index = round(civic_index, 2)) %>%
        select(-daily_ped)
        
        
## Apple data
edmonton_apple <- apple_raw %>%
        filter(region == "Edmonton") %>%
        filter(transportation_type == "walking") %>%
        pivot_longer(
                cols = starts_with("202"),
                names_to = "date",
                values_to = "apple_index"
        ) %>%
        select(date, apple_index) %>%
        mutate(date = as.Date(date))
        
# Join
edmonton_combined <- left_join(edmonton_civic, edmonton_apple, by = "date") %>%
        pivot_longer(
                cols = ends_with("index"),
                names_to = "type",
                values_to = "index"
        )

# Plot
ggplot(edmonton_combined,
       aes(x = date, y = index, colour = type)) +
        geom_line() +
        geom_smooth()
