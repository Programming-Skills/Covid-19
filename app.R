source("libraries.R")

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

`%!in%` = Negate(`%in%`)

uk <- getData('GADM', country='GBR', level = 2) 
national_names <- uk$NAME_1
regional_names <- uk$NAME_2

api_in_map <- result %>% 
  group_by(name) %>% 
  top_n(1, date) %>% 
  filter(name %in% regional_names) %>% 
  ungroup() 

api_in_map <- api_in_map[match(regional_names[y], api_in_map$name),]

a <- seq(1:length(regional_names))

b <- which(regional_names %in% api_in_map$name)

c <- setdiff(a,b)

for(i in seq_along(c)){
  api_in_map <- add_row(api_in_map, .after = (setdiff(a,b)-1)[i])
}

api_in_map <- tidyr::replace_na(api_in_map, list(daily = "Unknown", cumulative = "Unknown"))

api_in_map

