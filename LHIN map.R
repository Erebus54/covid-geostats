# hospitlizations 


library(stringr) 


LHIN_summary <- read.csv("./dataset/COVID-19 hospital metrics in Ontario by Local Health Integration Network (LHIN) regions.csv", encoding = "UTF-8")
LHIN_summary$date <- as.Date(LHIN_summary$date)

#read in boundaries file 
LHIN_bounds <- st_read("./spatial files/Ontario LHIN region boundaries/Ontario_Health_Regions.shp") %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

st_geometry_type(LHIN_bounds)
st_crs(LHIN_bounds)

#simplify 

LHIN_bounds <- rmapshaper::ms_simplify(LHIN_bounds, keep = 0.01, keep_shapes = TRUE)
LHIN_bounds <- LHIN_bounds %>% 
  dplyr::arrange(REGION) %>%
  dplyr::mutate(REGION = str_to_title(REGION))



LHIN_summary <- LHIN_summary %>% 
  dplyr::filter(date == max(date)) %>% 
  dplyr::mutate(lhin_region = str_to_title(lhin_region))
  dplyr::arrange(lhin_region) %>% 
  #dplyr::rename(REGION = lhin_region) %>% 
  dplyr::select(-date)


lhin_map <- merge(LHIN_bounds, LHIN_summary, by.x = "REGION", by.y = "lhin_region" )

pal <- colorNumeric("viridis", domain = lhin_map$ICU_vented, reverse = F)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data  = lhin_map,
              fillColor = ~pal(ICU_vented), 
              weight = 1, 
              opacity = 1, 
              
              color = "white", 
              dashArray = 3, 
              fillOpacity = 0.5, 
              
              highlightOptions = highlightOptions(
                weight = 2, 
                color = "red", 
                fillOpacity = 0.5, 
                bringToFront = T)) %>%
  
  addFullscreenControl() %>% 
  addResetMapButton() 
  # addLegend("bottomright", pal = pal, values = lhin_map$hospitalizations, 
  #           title = "Hospitalizations", 
  #           opacity = 0.5)
