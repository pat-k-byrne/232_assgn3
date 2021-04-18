#' Almond Yield Anomaly
#' 
#' Function to calculate the almond yield difference from the 1980-2003 average (ton/acre)
#' Function output: a matrix with two columns. Column 1 is input years and column 2 is calculated yield anomaly
#' @param precip A vector containing a timeseries of total precipitation in the month of January (in mm)
#' @param min_temp A vector containing a timeseries of mean minimum daily temperature in the month of February
#' @param years.calc A vector of the years for which you would like to calculate yield anomaly (must be the same lenth as precip and min_temp)
#' @authors
#' Pat Byrne, Hannah Garcia, and Yani Pohl 
#' @source
#' Lobell, D. B., Field, C. B., Cahill, K. N., & Bonfils, C. (2006). Impacts of future climate change on California perennial crop yields: Model projections with climate and crop uncertainties. Agricultural and Forest Meteorology, 141(2–4), 208–218. 
#' https://doi.org/10.1016/j.agrformet.2006.10.006


yield_anomaly = function(precip, min_temp, t1=-0.015, t2=-0.0046, p1=-0.07, p2=0.0043, inter=0.28) {
  
  calc.anomaly <- t1*min_temp + t2*min_temp^2 + p1*precip + p2*precip^2 + inter # Calculate yield anomaly in year desired based on Lobell equation
  
  return(calc.anomaly)
}


