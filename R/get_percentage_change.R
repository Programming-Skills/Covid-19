get_percentage_change <- function(data1, data2, n1, n2){
  "Args: 
        data1 is the GOV.uk Covid-19 API data.
        data2 is the leafet map regional data.
        n1 smaller number of days in the past.
        n2 larger number of days in the past
  Result:
        returns a df with the percentage change between n1 and n2 days (missing data is filled with 'Data Not Available')
  "
  x <- data1 %>% 
    group_by(name) %>% 
    top_n(3, date) %>% 
    filter(name %in% data2) %>% 
    ungroup() 
  
  y <- x %>% 
    group_by(name) %>% 
    filter(row_number() %in% c(n1)) %>% 
    ungroup()
  
  z <- x %>% 
    group_by(name) %>% 
    filter(row_number() %in% c(n2)) %>% 
    ungroup()
  
  daily_percent_change <- z %>% 
    left_join(y, by = "name") %>% 
    mutate(percent = signif(((daily.y-daily.x)/daily.x)*100, 2)) %>% 
    dplyr::select(name, percent)
  
  # Code to match the dimensions of the API and the map dataframe
  b <- which(data2 %in% daily_percent_change$name)
  
  daily_percent_change <- daily_percent_change[match(data2[b], daily_percent_change$name),]
  
  a <- seq(1:length(data2))
  
  c <- setdiff(a,b)
  
  for(i in seq_along(c)){
    daily_percent_change <- add_row(daily_percent_change, .after = (setdiff(a,b)-1)[i])
  }
  
  daily_percent_change <- tidyr::replace_na(daily_percent_change, list(name = "Data Not Available", percent = "Data Not Available"))
  
  return(daily_percent_change)
}