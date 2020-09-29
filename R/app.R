source("libraries.R")
source('make_equivalent_df.R')
source('get_percentage_change.R')

### Section 1: Read & clean data ###

# Read API data 

#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
  endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
  results      <- list()
  current_page <- 1
  
  repeat {
    
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      timeout(10)
    ) -> response
    
    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1;
    
  }
  
  return(results)
  
}


# Create filters:
query_filters <- c(
  "areaType=utla"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  daily      = "newCasesBySpecimenDate",
  cumulative = "cumCasesBySpecimenDate"
)

result <- get_paginated_data(query_filters, query_structure)

# Read leafet msp data
uk <- getData('GADM', country='GBR', level = 2) 
national_names <- uk$NAME_1
regional_names <- uk$NAME_2

# 1.2. Preprocessing

# # Latest info
api_in_map_1st_day <- make_equivalent_df(data1 = result, n = 1, data2 = regional_names)

# Day before
api_in_map_2nd_day <- make_equivalent_df(data1 = result, n = 2, data2 = regional_names)

# Two days before
api_in_map_3rd_day <- make_equivalent_df(data1 = result, data2 = regional_names, n = 3)

# Calculate percentage change between previous 2nd and 3rd days. 
daily_percent_change <- get_percentage_change(data1 = result, data2 = regional_names, n1 = 2, n2 = 3)

### Section 2. Make app ###

ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  leafletOutput("my_leaf", width = "50%", height = "100%")
)

server <- function(input, output, session){
  
  output$my_leaf <- renderLeaflet({
    
    # creating a colour palette that provides a diff colour for regions
    # in different country i.e., Scotland, Ireland, Wales, etc.  
    pal <- colorFactor("Blues", uk$GID_2)
    leaflet(uk) %>% 
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~pal(GID_2),
                  highlightOptions = highlightOptions(color = "white", weight =2,
                                                      bringToFront = TRUE),
                  label = ~paste0(NAME_2, ",", NAME_1),
                  popup = ~paste0("<div style='font-size: 15px'><b><h5>",
                                NAME_2, ", ", NAME_1, "</h1></b>",
                                "Date: ", api_in_map_1st_day$date, "<br>",
                                "Daily Cases: ", api_in_map_1st_day$daily, "<br>",
                                "Cumulative Cases: ", api_in_map_1st_day$cumulative, "<br><br>",
                                "Date: ", api_in_map_2nd_day$date, "<br>",
                                "Daily Cases: ", api_in_map_2nd_day$daily, "<br>",
                                "Cumulative Cases: ", api_in_map_2nd_day$cumulative, "<br><br>",
                                "Change in Daily Cases: ", daily_percent_change$percent, "%", "<br><br>",
                                "Date: ", api_in_map_3rd_day$date, "<br>",
                                "Daily Cases: ", api_in_map_3rd_day$daily, "<br>",
                                "Cumulative Cases: ", api_in_map_3rd_day$cumulative, "<br>"))  
    
  })

  
}

shinyApp(ui, server)