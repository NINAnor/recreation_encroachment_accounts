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
#' the year of detection of encroachment data and the the year of mapping of
#' another dataset (e.g., recreation areas, søærlig vkiting naturtyper).
#' Its value is only TRUE or FALSE, default is FALSE.
#' 
#' @param yrsource1 this is the name of the column containing the years of detection
#' of the encroachment polygons.
#' 
#' @param yrsource2 this is a vector with the names of the name of the column
#' containing the years of mapping of both datasets.
#'
#' @return a vector of character containing the colored-uncertainty.
#' @export
#'
#' @examples

add_uncertainty_individual <- function(data, source1 = FALSE, source2 = FALSE,  yrsource1, yrsource2) {
  
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
  
  # Sources 1 and 2
  if(source2 == TRUE){
    
    yr1 <- as.numeric(data[[yrsource2[1]]])
    yr2 <- as.numeric(data[[yrsource2[2]]])
    diff_yr <- abs(yr1 - yr2)
    
    if(isTRUE(yr1 == 2023) && isTRUE(diff_yr <= 3)){
      
      uncertainty_source1 <- "Green"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr <= 3)){
      
      uncertainty_source1 <- "Yellow"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr <= 3)){
      
      uncertainty_source1 <- "Yellow"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr <= 3)){
      
      uncertainty_source1 <- "Orange"
      
    }else if(isTRUE(yr1 == 2023) && isTRUE(diff_yr > 3 && diff_yr <= 6)){
      
      uncertainty_source1 <- "Yellow"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 3 && diff_yr <= 6)){
      
      uncertainty_source1 <- "Yellow"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 3 && diff_yr <= 6)){
      
      uncertainty_source1 <- "Orange"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 3 && diff_yr <= 6)){
      
      uncertainty_source1 <- "Red"
      
    }else if(isTRUE(yr1 == 2023) && isTRUE(diff_yr > 6 && diff_yr <= 8)){
      
      uncertainty_source1 <- "Yellow"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 6 && diff_yr <= 8)){
      
      uncertainty_source1 <- "Orange"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 6 && diff_yr <= 8)){
      
      uncertainty_source1 <- "Orange"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 6 && diff_yr <= 8)){
      
      uncertainty_source1 <- "Red"
      
    }else if(isTRUE(yr1 == 2023) && isTRUE(diff_yr > 8)){
      
      uncertainty_source1 <- "Orange"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 8)){
      
      uncertainty_source1 <- "Orange"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 8)){
      
      uncertainty_source1 <- "Red"
      
    }else if(isTRUE(yr1 %in% c(2018, 2019, 2020, 2021, 2022, 2024)) && isTRUE(diff_yr > 8)){
      
      uncertainty_source1 <- "Red"
      
    }
    
    return(c(uncertainty_source1))
  }
  

}
