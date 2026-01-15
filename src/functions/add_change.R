#' Compile the change in area and add it as a row in an accounting table
#'
#' This function allows to calculate the change in area between
#' the opening and closing year of an accounting period for an extent 
#' account. 

#' @param data the dataframe or tibble of the extent account. This dataframe 
#' should already be formatted as an extent account with years in rows.
#' 
#' @param year_column the column where the years are displayed.
#' 
#' @param area1 the name of column with the areas. Should be a character.
#' 
#' @param area2 the name column with the areas. Should be a character. By default, it is set to NULL. This
#' column is useful if you have several area columns for different spatial units such as m2, km2, etc.)
#' 
#' @param area3 the name of column with the areas. Should be a character. By default, it is set to NULL. This
#' column is useful if you have several area columns for different spatial units such as m2, km2, etc.
#'
#' @return The function returns the same accounting table with one additional row for the change.
#' @export
#'
#' @examples

add_change <- function(data, year_column, area1, area2 = NULL, area3 = NULL) {
  
  if(is.null(area2)){
    #1# Compile the  change for all accounting area metrics
    change_1 <- last(data[[area1]]) - first(data[[area1]])
    
    #2# Create an  change row and bind it to the other rows
    new_row <- data[1,] %>% # take the first row of data and edit it with mutate
      mutate({{year_column}} := " Change 2018-2024",
             {{area1}} := change_1,
             across(where(is.numeric) & -{{area1}} & -{{year_column}}, 
                    ~ NA_real_))
    
    #3# Add the row to the table 
    data_new <- bind_rows(data, new_row)
    
    return(data_new)
    
  }else if(is.null(area3)){
    
    #1# Compile the  change for all accounting area metrics
    change_1 <- last(data[[area1]]) - first(data[[area1]])
    change_2 <- last(data[[area2]]) - first(data[[area2]])
    
    #2# Create an  change row and bind it to the other rows
    new_row <- data[1,] %>% # take the first row of data and edit it with mutate
      mutate({{year_column}} := " Change 2018-2024",
             {{area1}} := change_1,
             {{area2}} := change_2,
             across(where(is.numeric) & -{{area1}} & -{{area2}} & -{{year_column}}, 
                    ~ NA_real_))
    
    #3# Add the row to the table 
    data_new <- bind_rows(data, new_row)
    
    return(data_new)
    
  }else{
  #1# Compile the  change for all accounting area metrics
    change_1 <- last(data[[area1]]) - first(data[[area1]])
    change_2 <- last(data[[area2]]) - first(data[[area2]])
    change_3 <- last(data[[area3]]) - first(data[[area3]])
  
  #2# Create an  change row and bind it to the other rows
  new_row <- data[1,] %>% # take the first row of data and edit it with mutate
                mutate({{year_column}} := " Change 2018-2024",
                       {{area1}} := change_1,
                       {{area2}} := change_2,
                       {{area3}} := change_3,
                       across(where(is.numeric) & -{{area1}} & -{{area2}} & -{{area3}} & -{{year_column}}, 
                              ~ NA_real_))
  
  #3# Add the row to the table 
  data_new <- bind_rows(data, new_row)
  
  return(data_new)
  }
}
