# Google Maps Direction API call function

# Created by Murilo Junqueira (m.junqueira@yahoo.com.br)

# Creation Date: 2020-04-03


library(dplyr)
library(lubridate)

# Debug:
# raw_result = raw_results[[4]][["EastWest"]]
# CityCode = GeoSel$Munic_Id[[i]]
# Cityname = GeoSel$Munic_Nome[[i]]
# NowTime = raw_results[[i]][["EastWest"]][["NowTime"]]
# Directions = "EW"


GDirections_DF <- function(raw_result, CityCode, Cityname, NowTime, Directions = NULL) {
  
  QueryKey <- paste(CityCode, Cityname, NowTime, Directions)
  
  QueriesIndex <- as_tibble(matrix(NA, 1, 0)) %>% 
    mutate(QueryKey = QueryKey) %>% 
    mutate(CityCode = CityCode) %>% 
    mutate(Directions = Directions) %>% 
    mutate(Cityname = Cityname) %>% 
    mutate(NowTime = NowTime) %>% 
    mutate(Status = raw_result$status) %>% 
    mutate(Num_routes = length(raw_result$routes$legs))
  
  # Retorna só QueriesIndex se a busca foi falha
  if(raw_result[["status"]] != "OK") {
    
    Output_Directions <- list(QueriesIndex, 
                              geocoded_waypoints = NULL, 
                              Routes = NULL, 
                              Steps = NULL)
    return(Output_Directions)
  }
  
  # Tabelas
  geocoded_waypoints <- raw_result$geocoded_waypoints %>% 
    as_tibble() %>% 
    mutate(QueryKey = QueryKey) %>% 
    mutate(types = as.character(types)) %>% 
    select(QueryKey, everything())
  
  QueriesRoutes <- raw_result$routes$bounds %>% 
    as_tibble() %>% 
    unlist() %>% 
    tibble::enframe() %>% 
    separate(col = name, into = c("geo", "coor_route")) %>% 
    mutate(coor_route = str_replace(coor_route, "lat", "lat.")) %>% 
    mutate(coor_route = str_replace(coor_route, "lng", "lng.")) %>% 
    separate(col = coor_route, into = c("coor", "route")) %>% 
    select(route, everything()) %>% 
    mutate(geo_coor = paste0(geo, "_", coor)) %>% 
    mutate(QueryKey = QueryKey) %>% 
    select(-geo, -coor) %>% 
    select(QueryKey, everything()) %>% 
    spread(geo_coor, value) %>% 
    bind_cols(raw_result$routes$overview_polyline)
  
  Routes_New <- tibble()
  
  for (i in seq_len(length(raw_result$routes$legs))) {
    
    # i <- 1
    
    Routes_NewRow <- list()
    
    Routes_NewRow$summary <- raw_result$routes$summary[[i]]
    Routes_NewRow$RoteLengthText <- raw_result$routes$legs[[i]]$distance$text
    Routes_NewRow$RoteLengthValue <- raw_result$routes$legs[[i]]$distance$value # distancia em metros
    Routes_NewRow$RoteTimeText <- raw_result$routes$legs[[i]]$duration$text
    Routes_NewRow$RoteTimeValue <- raw_result$routes$legs[[i]]$duration$value # tempo em segundos
    Routes_NewRow$RoteTrafficText <- raw_result$routes$legs[[i]]$duration_in_traffic$text
    Routes_NewRow$RoteTrafficValue <- raw_result$routes$legs[[i]]$duration_in_traffic$value
    Routes_NewRow$RoteStartAddress <- raw_result$routes$legs[[i]]$start_address
    Routes_NewRow$RoteEndAddress <- raw_result$routes$legs[[i]]$end_address
    Routes_NewRow$RoteStartLocationLat <- raw_result$routes$legs[[i]]$start_location$lat
    Routes_NewRow$RoteStartLocationLon <- raw_result$routes$legs[[i]]$start_location$lng
    Routes_NewRow$RoteEndLocationLat <- raw_result$routes$legs[[i]]$end_location$lat
    Routes_NewRow$RoteEndLocationLon <- raw_result$routes$legs[[i]]$end_location$lat
    Routes_NewRow$warning <- ifelse(length(raw_result$routes$warnings[[i]]) == 0, NA, 
                                    as.character(raw_result$routes$warnings[[i]]))
    
    Routes_New <- bind_rows(Routes_New, bind_cols(Routes_NewRow))
    
  }
  
  Routes <- bind_cols(QueriesRoutes, Routes_New)
  
  # Formata os passos (steps) do caminho
  
  Steps_New <- tibble()
  
  for (i in seq_len(length(raw_result$routes$legs))) {
    
    # i <- 1
    Steps_NewRow <- list()
    
    Steps_NewRow$route <- rep(i, length(raw_result$routes$legs[[i]]$steps[[1]]$distance$text))
    Steps_NewRow$distance_text <- raw_result$routes$legs[[i]]$steps[[1]]$distance$text
    Steps_NewRow$distance_value <- raw_result$routes$legs[[i]]$steps[[1]]$distance$value
    Steps_NewRow$duration_text <- raw_result$routes$legs[[i]]$steps[[1]]$duration$text
    Steps_NewRow$duration_value <- raw_result$routes$legs[[i]]$steps[[1]]$duration$value
    Steps_NewRow$end_location_lat <- raw_result$routes$legs[[i]]$steps[[1]]$end_location$lat
    Steps_NewRow$end_location_lng <- raw_result$routes$legs[[i]]$steps[[1]]$end_location$lng
    Steps_NewRow$start_location_lat <- raw_result$routes$legs[[i]]$steps[[1]]$start_location$lat
    Steps_NewRow$start_location_lng <- raw_result$routes$legs[[i]]$steps[[1]]$start_location$lng
    Steps_NewRow$html_instructions <- raw_result$routes$legs[[i]]$steps[[1]]$html_instructions
    Steps_NewRow$travel_mode <- raw_result$routes$legs[[i]]$steps[[1]]$travel_mode
    Steps_NewRow$maneuver <- raw_result$routes$legs[[i]]$steps[[1]]$maneuver
    Steps_NewRow$polyline <- raw_result$routes$legs[[i]]$steps[[1]]$polyline$points
    
    Steps_New <- bind_rows(Steps_New, bind_cols(Steps_NewRow))
    
  }
  
  
  Steps <- Steps_New %>% 
    mutate(QueryKey = QueryKey) %>% 
    select(QueryKey, everything()) 
  
  
  Output_Directions <- list(QueriesIndex, geocoded_waypoints, Routes, Steps)
  
  return(Output_Directions)
  
}


# End