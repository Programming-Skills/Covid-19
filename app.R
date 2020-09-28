source("libraries.R")

### Section 1: Read & clean data ###

# Read API data from 

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

list(
  "Shape"                = dim(result),
  "Data (first 3 items)" = result[0:100,]
) -> report



uk <- getData('GADM', country='GBR', level = 2) 
national_names <- uk$NAME_1
regional_names <- uk$NAME_2

# 1.2. Preprocessing

api_in_map_top_3_days <- result %>% 
  group_by(name) %>% 
  top_n(3, date) %>% 
  filter(name %in% regional_names) %>% 
  ungroup() 

api_in_map_2nd_day_ <- api_in_map_top_3_days %>% 
  group_by(name) %>% 
  filter(row_number() %in% c(2)) %>% 
  ungroup()

api_in_map_3rd_day_ <- api_in_map_top_3_days %>% 
  group_by(name) %>% 
  filter(row_number() %in% c(3)) %>% 
  ungroup()

daily_percent_change <- api_in_map_3rd_day_ %>% 
  left_join(api_in_map_2nd_day_, by = "name") %>% 
  mutate(percent = signif(((daily.y-daily.x)/daily.x)*100, 2)) %>% 
  dplyr::select(name, percent)

# Code to match the dimensions of the API and the map dataframe
b <- which(regional_names %in% daily_percent_change$name)

daily_percent_change <- daily_percent_change[match(regional_names[b], daily_percent_change$name),]

a <- seq(1:length(regional_names))

c <- setdiff(a,b)

for(i in seq_along(c)){
  daily_percent_change <- add_row(daily_percent_change, .after = (setdiff(a,b)-1)[i])
}

daily_percent_change <- tidyr::replace_na(daily_percent_change, list(name = "Data Not Available", percent = "Data Not Available"))


# Latest info
api_in_map <- result %>% 
  group_by(name) %>% 
  top_n(1, date) %>% 
  filter(name %in% regional_names) %>% 
  ungroup() 

# Code to match the dimensions of the API and the map dataframe
e <- which(regional_names %in% api_in_map$name)

api_in_map <- api_in_map[match(regional_names[e], api_in_map$name),]

d <- seq(1:length(regional_names))

f <- setdiff(d,e)

for(i in seq_along(f)){
  api_in_map <- add_row(api_in_map, .after = (setdiff(d,e)-1)[i])
}

api_in_map <- tidyr::replace_na(api_in_map, list(date = "Data Not Available", 
                                                 daily = "Data Not Available", 
                                                 cumulative = "Data Not Available"))

# Day before
api_in_map_2nd_day <- api_in_map_top_3_days %>% 
  group_by(name) %>% 
  filter(row_number() %in% c(2)) %>% 
  ungroup()

# Code to match the dimensions of the API and the map dataframe
h <- which(regional_names %in% api_in_map_2nd_day$name)

api_in_map_2nd_day <- api_in_map_2nd_day[match(regional_names[b], api_in_map_2nd_day$name),]

g <- seq(1:length(regional_names))

i <- setdiff(g,h)

for(i in seq_along(i)){
  api_in_map_2nd_day <- add_row(api_in_map_2nd_day, .after = (setdiff(g,h)-1)[i])
}

api_in_map_2nd_day <- tidyr::replace_na(api_in_map_2nd_day, list(date = "Data Not Available", 
                                                 daily = "Data Not Available", 
                                                 cumulative = "Data Not Available"))


# Two days before
api_in_map_3rd_day <- api_in_map_top_3_days %>% 
  group_by(name) %>% 
  filter(row_number() %in% c(3)) %>% 
  ungroup()

# Code to match the dimensions of the API and the map dataframe
k <- which(regional_names %in% api_in_map_3rd_day$name)

api_in_map_3rd_day <- api_in_map_3rd_day[match(regional_names[b], api_in_map_3rd_day$name),]

j <- seq(1:length(regional_names))

l <- setdiff(a,b)

for(i in seq_along(l)){
  api_in_map_3rd_day <- add_row(api_in_map_3rd_day, .after = (setdiff(j,k)-1)[i])
}

api_in_map_3rd_day <- tidyr::replace_na(api_in_map_3rd_day, list(date = "Data Not Available", 
                                                                 daily = "Data Not Available", 
                                                                 cumulative = "Data Not Available"))


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
                                "Date: ", api_in_map$date, "<br>",
                                "Daily Cases: ", api_in_map$daily, "<br>",
                                "Cumulative Cases: ", api_in_map$cumulative, "<br><br>",
                                "Date: ", api_in_map_2nd_day$date, "<br>",
                                "Daily Cases: ", api_in_map_2nd_day$daily, "<br>",
                                "Cumulative Cases: ", api_in_map_2nd_day$cumulative, "<br><br>",
                                "Percentage Change on Previous days' Daily Cases: ", daily_percent_change$percent, "%", "<br><br>",
                                "Date: ", api_in_map_3rd_day$date, "<br>",
                                "Daily Cases: ", api_in_map_3rd_day$daily, "<br>",
                                "Cumulative Cases: ", api_in_map_3rd_day$cumulative, "<br>"))  
    
  })

  
}

shinyApp(ui, server)