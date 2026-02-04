#' Compile the ''global'' color-based uncertainty based on individual
#' color-based uncertainties
#'
#' This function allows to assign a ''global'' colored uncertainty based on
#' the individual sources of uncertainty when compiling extent accounts. Color 
#' code is as follows: "green" for "quite certain", "yellow" for "rather certain", 
#' "orange" for "rather uncertain", and "red" for "quite uncertain". The ''global''
#' color-based uncertainty is assigned based on the frequency of the color of the
#' individual sources of uncertainty.
#' 
#' @param data the dataframe to which the uncertainty column needs to be appended.
#' 
#' @param uncertainty_col_name names of the columns containing each individual
#' color-based uncertainty concatenated. This should be a vector of character.
#' 
#' @param kom_num name of the column containing the municipality number. This should
#' not be a character.
#' 
#' @param accounting_year name of the column containing the years of accounting.
#' This should not be a character
#'
#' @return the original dataframe with a yearly uncertainty and a cumulative
#' uncertainty.
#' 
#' @export
#'
#' @examples

add_uncertainty_global <- function(data, uncertainty_col_name, kom_num, accounting_year){

    # Capture column names
    kommune_nr <- {{kom_num}}
    accounting_yr <- {{accounting_year}}
    unc_col <- {{uncertainty_col_name}}
    
    # Call function to add global uncertainty for one row of a dataset'
    functions_path <- here::here(project_path, "src/functions")
    source(paste0(functions_path, "/global_uncertainty.R"))
    
    
    # Create a column with yearly uncertainty split so we can look for their frequencies
    data_split <- data %>%
                    ungroup() %>%
                    mutate(unc_yearly_sep = strsplit(.[[unc_col]], " ")) %>%
                    as.data.frame()
    
    # Apply function to data_split to have yearly uncertainty
    unc_yearly <- data_split %>%
                    rowwise() %>%
                    mutate(global_yearly_uncertainty = global_fn(df = ., row = row_number(), uncertainty_column = "unc_yearly_sep"))
 
    # Create a column with the cumulative uncertainties 
    col_cum <- unc_yearly %>%
                select(all_of(kommune_nr), all_of(accounting_yr), all_of(unc_col)) %>%
                group_by(across(all_of(kommune_nr))) %>%
                group_split(.keep = TRUE) %>%
                map(~ mutate(.x, 
                             unc_acc = accumulate(.x[[unc_col]], ~ paste(.x, .y), sep = " "),
                             unc_acc_sep = strsplit(unc_acc, " "))) %>%
                bind_rows %>%
                as.data.frame()

  
    # Apply function to col_cum to have cumulative uncertainty
    col_global <- col_cum %>%
                    rowwise() %>%
                    mutate(global_cumulative_uncertainty = global_fn(df = ., row = row_number(), uncertainty_column = "unc_acc_sep"))
  
    # Append the two uncertainty columns to data
    global_df <- data_split %>%
                  select(!unc_yearly_sep) %>%
                  mutate(global_yearly_uncertainty = unc_yearly$global_yearly_uncertainty,
                         global_cumulative_uncertainty = col_global$global_cumulative_uncertainty)
    
    # Return result
    return(global_df)

}
  
  

