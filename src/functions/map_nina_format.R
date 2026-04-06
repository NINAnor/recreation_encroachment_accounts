#' Add a variable to dispaly the maps in maps.nina.no
#'
#' This function adds a variable to a dataset that groups the variable so that
#' the color display in maps.nina.no is easier. 
#' .
#' 
#' @param data the dataframe to which the variable needs to be appended.
#' 
#' @param mapping_variable the variable that should be used for determining
#' the groups. This should be a character.
#' 
#' @param mapping_groups the number of groups needed. 
#' 
#' @param groups_thresholds the different threshold that should be used to
#' group the data for mapping. Should be a vector. Interval closure are to 
#' the left, e.g., `[0:5[`. The vector should only contain the right interval
#' closures.
#'
#' @return the same dataset with an additional column containing the grouping for
#' mapping.
#' 
#' @export
#'
#' @examples

map_nina_format <- function(data, mapping_variable, mapping_groups, groups_thresholds){
  
  # retrieve variable that will be used for determining groups
    mapping_var <- {{mapping_variable}}
    gr_threshold <- {{groups_thresholds}}
    
  # add a new column to data
    data_new <- mutate(data, mapping_groups = "NA")
  
  
  # Group
  if(isFALSE(mapping_groups %in% c(4, 5, 6))){
    
    return(c("Mapping function needs to be adjusted, it only allows for 4, 5 or 6 groups"))
  
  }else if(isTRUE(mapping_groups == 4)){
    
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[1])] <- "Group 1"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[2] & data[[mapping_var]] >= groups_thresholds[1])] <- "Group 2"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[3] & data[[mapping_var]] >= groups_thresholds[2])] <- "Group 3"
    data_new$mapping_groups[which(data[[mapping_var]] >= groups_thresholds[3])] <- "Group 4"
    
    return(data_new)
    
  }else if(isTRUE(mapping_groups == 5)){
      
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[1])] <- "Group 1"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[2] & data[[mapping_var]] >= groups_thresholds[1])] <- "Group 2"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[3] & data[[mapping_var]] >= groups_thresholds[2])] <- "Group 3"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[4] & data[[mapping_var]] >= groups_thresholds[3])] <- "Group 4"
    data_new$mapping_groups[which(data[[mapping_var]] >= groups_thresholds[4])] <- "Group 5"
    
    return(data_new)
    
    }else if(isTRUE(mapping_groups == 6)){
    
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[1])] <- "Group 1"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[2] & data[[mapping_var]] >= groups_thresholds[1])] <- "Group 2"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[3] & data[[mapping_var]] >= groups_thresholds[2])] <- "Group 3"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[4] & data[[mapping_var]] >= groups_thresholds[3])] <- "Group 4"
    data_new$mapping_groups[which(data[[mapping_var]] < groups_thresholds[5] & data[[mapping_var]] >= groups_thresholds[4])] <- "Group 5"
    data_new$mapping_groups[which(data[[mapping_var]] >= groups_thresholds[5])] <- "Group 6"
    
    return(data_new)
    
  }

}
  
  