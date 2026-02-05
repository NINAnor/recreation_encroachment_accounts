#' Concatenate all color-based uncertainty into one column
#'
#' This function allows to concatenate all the individual color-based uncertainties
#' into one column. This is the first step before calculating a "global" uncertainty.
#' What we call "individual color-based uncertainties" refers to the color uncertainty
#' that was given to all the course of uncertainties present in the analyses. These were 
#' calculated with the function add_uncertainty_individual().
#' 
#' @param data the dataframe to which the uncertainty column needs to be appended.
#' 
#' @param col_column_names names of the columns containing each individual
#' color-based uncertainty. This should be a vector of character.
#' 
#' @param kom_num name of the column containing the municipality number. This should
#' not be a character.
#' 
#' @param accounting_year name of the column containing the years of accounting.
#' This should not be a character. Default is NULL. If NULL, habitat_col should not be NULL.
#' If not NULL; concatenation of color uncertainty is done by accounting years.
#' 
#' @param habitat_col name of the column containing habitats for which the accounting is done.
#' This should be a character. Default is NULL, if not NULL accounting_year is ignored and
#' the concatenation of color uncertainties is done by habitats instead of accounting year.
#' 
#' @return a vector of character containing all the individual colored-uncertainty.
#' @export
#'
#' @examples

concatenate_fn <- function(data, col_column_names, kom_num, accounting_year = NULL, habitat_col = NULL) {
  
  if(is_null(habitat_col)){
    
    # Concatenate the color value of all sources of uncertainty per accounting year only
    unc_vec <- data %>%
      unite(., col = unc_concat, all_of(col_column_names), sep = " ") %>%
      group_by(across(all_of(c(kom_num, accounting_year)))) %>%
      summarise(unc_concat_full = paste(unc_concat, collapse = " "))
  
    
  }else{
    # Concatenate the color values of all uncertainty sources per year of accounting and habitat types
    unc_vec <- data %>%
                unite(., col = unc_concat, all_of(col_column_names), sep = " ") %>%
                group_by(across(all_of(c(kom_num, habitat_col)))) %>%
                summarise(unc_concat_full = paste(unc_concat, collapse = " "))
    
  }
  
  return(unc_vec)
  
}
