make_equivalent_df <- function(data1, n, data2){
  "Args: 
        data1 is the GOV.uk Covid-19 API data.
        n is the number of days the user wants to include in the results.
        data2 is the leafet map regional data.
  Result:
        returns a df with the regions matching in the API and leafet map data (missing data is filled with 'Data Not Available')
  "
  res <- data1 %>% 
    group_by(name) %>% 
    top_n(3, date) %>% 
    filter(name %in% data2) %>% 
    filter(row_number() %in% c(n)) %>%
    ungroup()
  
  b <- which(data2 %in% res$name)
  res <- res[match(data2[b], res$name),]
  a <- seq(1:length(data2))
  c <- setdiff(a,b)
  
  for(i in seq_along(c)){
    res <- add_row(res, .after = (setdiff(a,b)-1)[i])
  }
  
  res <- tidyr::replace_na(res, list(date = "Data Not Available", 
                                     daily = "Data Not Available", 
                                     cumulative = "Data Not Available"))
  
  return(res)
}