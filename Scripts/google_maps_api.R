

build_params <- function(param_list) {
  ## builds the parameter list
  source("Scripts/Helper_Functions/helper_functions.R")
  params <- " "

  for (param in param_list) {
    
    ## checks for empty parameters
    if (str_right(param) != "="){
      params <- paste(params, param, sep = "&")
    }
  }

  params <- gsub(" &", "", params)

  return(params)
}

build_api_request <- function(url, param_list) {
  ## builds the api request
  params <- build_params(param_list)
  
  ## combine url with parameters
  request <- paste(url, params, sep = "?")
  
  return(request)
}

get_maps_data <- function(query, location, num_of_meters, region, 
                          type, key, token = NULL) {
  library("httr")
  library("curl")
  
  base_url <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  
  params <- c(paste("query=", query, sep = ""),
              paste("location=", location, sep = ""),
              paste("radius=", num_of_meters, sep = ""),
              paste("region=", region, sep = ""),
              paste("type=", type, sep = ""),
              paste("key=", key, sep = ""),
              paste("pagetoken=", token, sep = ""))
  
  request <- build_api_request(url= base_url, param_list = params)
  Sys.sleep(2)

  response <- GET(request)
  
  return(response)
}

store_maps_data <- function(){
  library("stringr")
  library("jsonlite")
  
  query <- "church"
  type <- "church"
  num_of_meters <- "1000"
  region <- "us"
  key <- Sys.getenv("maps_api_key")

  ## TODO utilize another api to gather the spread out coordinates to use to 
  ## capture the data from every city.
  coordinates <- c("36.920803, -76.191098",
                   "36.932330, -76.236447",
                   "36.940563, -76.307525",
                   "36.897194, -76.311648",
                   "36.888957, -76.258054",
                   "36.884564, -76.203773",
                   "36.849406, -76.216141",
                   "36.860944, -76.266986",
                   "36.877424, -76.311648")
  final_df <- NULL
  
  ## loops through each of the coordinates and searches for all of the addresses
  ## within a 1000 meter radius.
  for (coordinate in coordinates) {
    
    run <- 1
    next_pg_token <- NULL
    response_df <- NULL
    
    ## runs the loop until the break
    while (run>0) {
      
      ## replaces the spaces with"%20" for the url
      location <- gsub(", ",",%20",coordinate)
      response <- get_maps_data(query = query, type = type, location = location,
                                num_of_meters = num_of_meters,
                                region = region, key = key, token = next_pg_token)

      status <- response$status_code
      response <- content(response, as = "text")
      response_df <- as.data.frame(fromJSON(response)[[3]])
      
      ## pulls the next page token so that the next request will be for the next
      ## page.
      next_pg_token <- fromJSON(response)[[2]]
      
      ## breaks the code if the response is null.
      if (run > 1){
        if (is.null(response_df$name) == TRUE){
          break
        }
      }
      
      ## takes the data frames in the response and returns them as columns. 
      response_df$location.lat <- response_df$geometry$location$lat
      response_df$location.lng <- response_df$geometry$location$lng
      response_df$categories <-lapply(response_df$types, function(x){
        x <- as.character(x)
      })
      
      
      ## removes the data frame columns that were reformatted above.
      response_df <- subset(response_df,select = -c(geometry, 
                                           types,
                                           photos, 
                                           plus_code,
                                           opening_hours))
      
      ## stores the data frames together in one final_df
      if (is.null(final_df)==FALSE) {
        final_df <- rbind(final_df,response_df)
      } else {
        final_df <- response_df
      }
      run <- run + 1
    }

  }
  ## returns the final data frame.
  return(final_df)
}

clean_maps_data <- function(){
  ## TODO change the names from snake case to have a period
  ## TODO remove duplicates
  ## TODO break out the formatted address into street, city, state, and zip
  ## TODO remove items that are not actually in the city in question
  
}