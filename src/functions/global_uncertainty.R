#' Gives the global uncertainty of one line of a dataset
#'
#' This function allows to global color-based uncertainty of one line of a dataset
#' bsed on the individual color-based uncertainties calculated with the function 
#' add_uncertainty_individual(). This is the second step before appending a "global" 
#' uncertainty to each row of a dataset by applying the add_uncertainty_global() function.
#' .
#' 
#' @param data the dataframe to which the uncertainty column needs to be appended.
#' 
#' @param row the number of the row to which the function applies
#' 
#' @param the name of the column containing the concatenated uncertainties of
#' interest.
#'
#' @return a single character corresponding to a colored uncertainty: "green" for
#' "quite certain", "yellow" for "rather certain", "orange" for "rather uncertain", 
#' and "red" for "quite uncertain".
#' 
#' @export
#'
#' @examples

global_fn <- function(df, row, uncertainty_column){
  
  # create the frequency table
  col_gr <- df[[uncertainty_column]][row][[1]] %>%
              table() %>%
              as.data.frame() %>%
              mutate(importance_perc = Freq/sum(Freq)) %>%
              arrange(., ".") # Order so it's easier for the selection of colors afterwards
  
    colnames(col_gr) <- c("color", "freq", "importance_perc")
    
    # assign global uncertainty based on the combinations of color
    
    if(isTRUE(nrow(col_gr) == 1)){ # if there is one color, just return it
      
      global_unc <- as.character(col_gr[1,1])
      
    }else if(isTRUE(length(which(col_gr$importance_perc >= 0.49)== TRUE) == 1)){
      
      global_unc <- col_gr$color[which(col_gr$importance_perc >= 0.49)] %>%
        as.character()
      
    }else if(isTRUE(length(which(col_gr$importance_perc >= 0.49)== TRUE) == 2)){
      
      main_col <- col_gr$color[which(col_gr$importance_perc >= 0.49)] %>%
        as.character()
      
      if(isTRUE(all.equal(main_col, c("Green", "Yellow"))) |  isTRUE(all.equal(main_col, c("Green", "Orange")))){ # Only two options as the rows are ordered so my colors will always appear in alphabetic order
        
        global_unc <- "Yellow"
        
      }else if(isTRUE(all.equal(main_col, c("Green", "Red"))) |  isTRUE(all.equal(main_col, c("Orange", "Yellow")))){
        
        global_unc <- "Orange"
        
      }else if(isTRUE(all.equal(main_col, c("Red", "Yellow"))) |  isTRUE(all.equal(main_col, c("Orange", "Red")))){
        
        global_unc <- "Red"
      }
      
      
    }else if(isTRUE(length(which(col_gr$importance_perc >= 0.29 & col_gr$importance_perc < 0.49) == TRUE) == 3)){
      
      main_col <- col_gr$color[which(col_gr$importance_perc >= 0.29)] %>%
        as.character()
      
      if(isTRUE(all.equal(main_col, c("Green", "Orange", "Yellow")))){ # Only two options as the rows are ordered so my colors will always appear in alphabetic order
        
        global_unc <- "Yellow"
        
      }else if(isTRUE(all.equal(main_col, c("Green", "Red", "Yellow"))) |  isTRUE(all.equal(main_col, c("Green", "Orange", "Red")))){
        
        global_unc <- "Orange"
        
      }else if(isTRUE(all.equal(main_col, c("Orange", "Red", "Yellow")))){
        
        global_unc <- "Red"
      }
      
    }else if(isTRUE(length(which(col_gr$importance_perc >= 0.20 & col_gr$importance_perc < 0.29) == TRUE) == 4)){
      
      global_unc <- "Orange"
      
    }else{
      
      global_unc <- "ERROR"
      
    }
    
  return(global_unc)
}
