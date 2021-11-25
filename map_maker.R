library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)
library(stringi)

# Set parameters

location <- "Ottawa, Canada"
city <- stri_extract(location, regex='[^,]*')
loadfonts(device = "win", quiet = TRUE) 
background_color<-'#FFFBF6'
street_color<-'#13130c'
small_street_color<-'#37261a'
coast_color<-'#5985ab'
font_color<-'#13130c'
chart_font<-"Glamor Med Cond"


# Get spatial data

bbox <- getbb(location)


streets <- bbox %>%
        opq() %>%
        add_osm_feature(key = "highway", 
                        value = c("motorway", "primary", 
                                  "secondary", "tertiary")) %>%
        osmdata_sf()



small_streets <- bbox %>%
        opq() %>%
        add_osm_feature(key = "highway", 
                        value = c("residential", "living_street",
                                  "unclassified",
                                  "service", "footway")) %>%
        osmdata_sf()


# Plot the map

city_map <- ggplot() +
        geom_sf(data = streets$osm_lines,
                inherit.aes = FALSE,
                color = street_color,
                size = .6,
                alpha = .8) +
        geom_sf(data = small_streets$osm_lines,
                inherit.aes = FALSE,
                color = small_street_color,
                size = .3,
                alpha = .6) +
        coord_sf(expand = FALSE) +
        theme_void() +
        theme(panel.border = element_blank(),
              plot.background = element_blank(),
              plot.caption = element_text(hjust = 0.5, size = 112, family = chart_font)) +
        labs(caption = city)


# Save the map

file_type <- "jpg"

ggsave(paste0(city, file_type), 
       city_map, 
       units = "in",
       width = 8,
       height = 10,
       dpi = 300, 
       device = file_type)