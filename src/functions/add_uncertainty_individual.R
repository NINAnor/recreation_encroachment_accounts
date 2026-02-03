#' Compile the color-based uncertainty of the individual uncertainty sources
#' linked to extent accounts
#'
#' This function allows to assign a colored uncertainty to the individual
#' sources of uncertainty when compiling extent accounts. Another function ()
#' allows then to give a "global" uncertainty based on these individual color-
#' based uncertainties. Color code is as follows: "green" for "quite certain", 
#' "yellow" for "rather certain", "orange" for "rather uncertain", and "red" for
#' "quite uncertain".
#' 
#' @param data the dataframe to which the uncertainty column needs to be appended.
#' 
#' @param source1 this sources is the uncertainty of prediction of encroachment.
#' Its value is only TRUE or FALSE, default is FALSE.
#' 
#' @param source2 this sources is the uncertainty of temporal discrepancy between
#' the year of mapping of outdoor recreation areas and the year of detection of an 
#' encroachment. Its value is only TRUE or FALSE, default is FALSE.
#' 
#' @param source3 this sources is the uncertainty of temporal discrepancy between
#' the year of mapping of outdoor recreation areas and the year of mapping of a
#' ''særlig viktig natur''. Its value is only TRUE or FALSE, default is FALSE.
#' 
#' @param source4 this sources is the uncertainty of temporal discrepancy between
#' the year of detection of an encroachment and the year of mapping of a ''særlig
#' viktig natur''. Its value is only TRUE or FALSE, default is FALSE.
#' 
#' @param yrsource1 this is the name of the column containing the years of detection
#' of the encroachment polygons.
#' 
#' @param yrsource2 this is a vector with the names of the name of the columns
#' containing the years of mapping of outdoor recreation areas and the detection year
#' of the encroachment.
#' 
#' @param yrsource3 this is a vector with the names of the name of the columns
#' containing the years of mapping of outdoor recreation areas and the mapping year
#' of ''særlig viktig natur''.
#' 
#' @param yrsource4 this is a vector with the names of the name of the columns
#' containing the detection year of encroachment and the mapping year of
#'''særlig viktig natur''.
#'
#' @return a vector of character containing the colored-uncertainty.
#' @export
#'
#' @examples

add_uncertainty_individual <- function(data, source1 = FALSE, source2 = FALSE, source3 = FALSE, source4 = FALSE, yrsource1, yrsource2, yrsource3, yrsource4) {
  
  # Only Source 1
 if(source1 == TRUE){
   
   yr1 <- as.numeric(data[[yrsource1]])
   
   if(isTRUE(yr1 == 2023)){
    
    uncertainty_source1 <- "Green"
    
   }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024))){
     
     uncertainty_source1 <- "Yellow"
     
   }
   
   return(uncertainty_source1)
 }
  
  # Sources 1 and 4
  if(source4 == TRUE){
    
    yr1 <- as.numeric(data[[yrsource4[1]]])
    yr4 <- as.numeric(data[[yrsource4[2]]])
    diff_yr4 <- abs(yr1 - yr4)
    
    if(isTRUE(yr1 == 2023)){
      
      uncertainty_source1 <- "Green"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024))){
      
      uncertainty_source1 <- "Yellow"
      
    }
    
    if(isTRUE(diff_yr4 <= 2)){
      
      uncertainty_source4 <- "Green"
      
    }else if(isTRUE(diff_yr4 > 2 && diff_yr4 <= 4)){
      
      uncertainty_source4 <- "Yellow"
      
    }else if(isTRUE(diff_yr4 > 4 && diff_yr4 <= 6)){
      
      uncertainty_source4 <- "Orange"
      
    }else if(isTRUE(diff_yr4 > 6)){
      
      uncertainty_source4 <- "Red"
      
    }
    
    return(c(uncertainty_source1, uncertainty_source4))
  }
  

}
