# Google Maps Direction API call function

# Created by Murilo Junqueira (m.junqueira@yahoo.com.br)

# Creation Date: 2020-04-03


library(jsonlite)

# Debug:
# from = "-23.5921009,-46.8056180"
# to = "-23.4859244,-46.3800909"
# apiKey = ""
# parameters = c("units", "departure_time", "alternatives")
# units = "metric"
# departure = "now"
# alternatives = "true"
# parametersValues = c(units, departure, alternatives)


GDirectionsCall <- function(from, to, apiKey, parameters = NULL, parametersValues = NULL) {
  
  if(!is.null(parameters)) {
    
    formatparameters <- paste0("&", parameters, "=", parametersValues)
    formatparameters <- paste0(formatparameters, collapse = "")
    
  } else {
    formatparameters <- NULL
  }
  
  DirectionsAPI <- "https://maps.googleapis.com/maps/api/directions/json?"
  
  
  request <- paste0(DirectionsAPI, 
                    "origin=",
                    from,
                    "&destination=", 
                    to,
                    formatparameters,
                    "&key=",
                    apiKey)
  
  done <- FALSE
  countLoop <- 0
  
  while(!isTRUE(done) && countLoop < 5) {

    raw_result <- try(jsonlite::fromJSON(request))
    
    # Ver Estudo de código "Error&LogicConditions.R"
    
    if(class(raw_result)[1] == "try-error" || !isTRUE(raw_result[["status"]] == "OK")) {
      
      countLoop <- countLoop + 1
      message("Bad response, trying again - ", countLoop, "/5")
      Sys.sleep(3)
      
    } else {
      done <- TRUE
    }
  }
  
  if(countLoop == 5) {
    message('Unable to contat server')
    return(list(Status = "Error"))
  }
  
  return(raw_result)
}



