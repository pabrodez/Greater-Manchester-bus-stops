# data
# bus stops coordinates: https://data.gov.uk/dataset/05252e3a-acdf-428b-9314-80ac7b17ab76/gm-bus-stopping-points
# wards: https://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html

# bus stops per capita in gm
# libraries
library(raster); library(sp);
library(readxl);library(tidyverse);library(gridExtra);library(ggthemes);
library(ggmap);library(sf); library(viridis); library(wellknown)

# read data for wards borders and bus stops ------------------------------------------------------------
uk_wards <- st_read("https://opendata.arcgis.com/datasets/afcc88affe5f450e9c03970b237a7999_3.geojson", quiet = TRUE, stringsAsFactors = FALSE)
gm_wards <- uk_wards %>% dplyr::filter(lad16nm %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan"))
gm_wards <- gm_wards %>% dplyr::select(code = wd16cd, ward_name = wd16nm, geometry = geometry, long, lat, District = lad16nm)

# bus stop data
bus_stop <- read.csv("./data/TfGMStoppingPoints.csv", stringsAsFactors = FALSE)
bus_stop <- bus_stop %>% dplyr::filter(Status == "act") %>% dplyr::select(lat = Latitude, long = Longitude)
bus_sf <- st_as_sf(bus_stop, coords = c("long", "lat"), 
                           crs = 4326, agr = "constant")

# read data for borough borders
# eng_bound <- st_read("https://opendata.arcgis.com/datasets/0b09996863af4b5db78058225bac5d1b_2.geojson", quiet = TRUE, stringsAsFactors = FALSE)
# gm_borough <- eng_bound %>% filter(ctyua15nm %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan"))
# gm_borough <- gm_borough %>% select(area_code = ctyua15cd, area_name = ctyua15nm, geometry)
# 
# gm_borough <- gm_borough %>% mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
#                                     lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

# Use these bounds for more detailed bounds
# eng_bound <- st_read("./data/Polling Districts England/TAB", stringsAsFactors = FALSE)  # https://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html
# gm_wards <- eng_bound %>% filter(County == "Greater_Manchester") %>% rename(District = Distric_Bo)

bio_data <-  st_read("./data/GMEU SBI Sites March 2018/TAB")

stop_persom <- round(( nrow(bus_stop) / 2782141) * 100, digits = 2)

# transform crs to the gm_wards's
gm_wards <- st_transform(gm_wards, 
                       crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=375,-111,431,0,0,0,0 +units=m +no_defs")
bus_sf <- st_transform(bus_sf, 
                       crs = "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=375,-111,431,0,0,0,0 +units=m +no_defs")
# https://stackoverflow.com/questions/49141192/st-intersects-errors-for-st-crsx-st-crsy-is-not-true

# get number of points covered by each polygon
lengths(st_covers(gm_wards, bus_sf))
gm_wards$stops_n <- lengths(st_covers(gm_wards, bus_sf))
# https://stackoverflow.com/questions/45314094/equivalent-of-poly-counts-to-count-lat-long-pairs-falling-inside-of-polygons-w

# m ---------------------------------------------------------------
# map
bus_map <- ggplot() +
        # geom_sf(data = bio_data, fill = "green", colour = "green", size = 1) +
        geom_sf(data = gm_wards, aes(fill = stops_n), size = 0.2, colour = NA) +
        # geom_sf(data = bus_sf, alpha = 0.5, size = 0.2) +
        labs(title = "Active bus stops per ward in GM",
             subtitle = paste("There are around", stop_persom, "bus stops per 100 people"),
             fill = "Stops") +
        scale_fill_viridis() +
        theme_fivethirtyeight() +
        theme(legend.position = "right",
              legend.direction = "vertical",
              panel.background = element_blank(),                                       
              line = element_blank(),                                                     
              axis.text = element_blank(),                                                
              axis.title = element_blank()) +
        coord_sf(datum = NA)

ggsave(filename = "./bus_map.png", plot = bus_map, 
       device = "png", dpi = 600, units = "cm", height = 15, width = 20)

# Read ward population data
wards_pop <- read_excel("./Ward_Population_Estimates_2016.xls", 
                                             sheet = "Mid-2016 Persons", skip = 3, range = cell_cols("A:D"))
pop_names <- as.character(wards_pop[4,])  # if wanted to keep original column names
wards_pop <- wards_pop[-c(1:4),]
colnames(wards_pop) <- c("code", "Name", "Local_authority", "Population")
wards_pop <- wards_pop %>% filter(Local_authority %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan"))

wards_bus_pop <- inner_join(gm_wards, wards_pop, by = "code")
wards_bus_pop <- wards_bus_pop %>% mutate(Population = as.numeric(Population)) %>% 
                        mutate(prop = round(stops_n/Population * 100, digits = 2))

# map
bus_pop_map <- ggplot() +
        # geom_sf(data = bio_data, fill = "green", colour = "green", size = 1) +
        geom_sf(data = wards_bus_pop, aes(fill = prop), size = 0.2, colour = NA) +
        # geom_sf(data = bus_sf, alpha = 0.5, size = 0.2) +
        labs(title = "Number of bus stops per 100 people in GM",
             fill = "Stops/people") +
        scale_fill_viridis() +
        theme_fivethirtyeight() +
        theme(legend.position = "right",
              legend.direction = "vertical",
              panel.background = element_blank(),                                       
              line = element_blank(),                                                     
              axis.text = element_blank(),                                                
              axis.title = element_blank()) +
        coord_sf(datum = NA)

ggsave(filename = "./bus_pop_map.png", plot = bus_pop_map, 
       device = "png", dpi = 600, units = "cm", height = 15, width = 20)

# sbi map
sbi_map <- ggplot() +
        geom_sf(data = bio_data, fill = "green", colour = "green", size = 1) +
        geom_sf(data = wards_bus_pop, fill = NA, size = 0.2, colour = "black") +
        geom_sf(data = bus_sf, colour= "orange", alpha = 0.3, size = 0.2) +
        labs(title = "Sites of Biological Interest and bus stops") +
        theme_fivethirtyeight() +
        theme(legend.position = "right",
              legend.direction = "vertical",
              panel.background = element_blank(),                                       
              line = element_blank(),                                                     
              axis.text = element_blank(),                                                
              axis.title = element_blank()) +
        coord_sf(datum = NA)
ggsave(filename = "./sbi_map.png", plot = sbi_map, 
       device = "png", dpi = 600, units = "cm", height = 15, width = 20)
