#' Almond Yield Anomaly
#' 
#' Function to calculate the almond yield difference from the 1980-2003 average (ton/acre)
#' Function output: a matrix with two columns. Column 1 is input years and column 2 is calculated yield anomaly
#' @param input.dataframe A dataframe with daily climate observations. Must include columns for month (named "month"), year (named "year"), daily minimum temperature (titled "tmin_c"), and daily precip (named "precip")
#' @param years.calc A vector of the years for which you would like to calculate yield anomaly
#' @examples 
#' yield_anomaly(input.dataframe = clim.df, years.calc = c(2000,2001,2002))
#' @authors
#' Pat Byrne, Hannah Garcia, and Yani Pohl 
#' @source
#' Lobell, D. B., Field, C. B., Cahill, K. N., & Bonfils, C. (2006). Impacts of future climate change on California perennial crop yields: Model projections with climate and crop uncertainties. Agricultural and Forest Meteorology, 141(2–4), 208–218. 
#' https://doi.org/10.1016/j.agrformet.2006.10.006


yield_anomaly = function(input.dataframe, years.calc, t1=-0.015, t2=-0.0046, p1=-0.07, p2=0.0043, inter=0.28) {
  
  calc.anomaly <- c() # Defining the vector in which the function will store calculated anomalies
  
  for (i in 1:length(years.calc)) {
    intermed.Jan <- input.dataframe %>% filter(year == years.calc[i],
                                           month == 1) # Intermediate dataframe of January data in the year desired
    intermed.Feb <- input.dataframe %>% filter(year == years.calc[i],
                                               month == 2) # Intermediate of Feb data in year desired
    
    temp_min <- mean(intermed.Feb$tmin_c) # Find the average minimum temp in Feb
    precip <- sum(intermed.Jan$precip) # Find total Jan precipitation
    
    calc.anomaly[i] <- t1*temp_min + t2*temp_min^2 + p1*precip + p2*precip^2 + inter # Calculate yield anomaly in year desired based on Lobell equation
  }
  
  res.matrix <- matrix(,nrow = length(years.calc), ncol = 2) # Create a matrix to store resuls
  res.matrix[,1] = years.calc # Column 1 of results matrix is years that you inputted for calculation
  res.matrix[,2] = calc.anomaly # Column 2 is calculated anomaly for those years
  
  return(res.matrix)
}

