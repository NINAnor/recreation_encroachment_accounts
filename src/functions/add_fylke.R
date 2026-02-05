#' Add fylke
#'
#' This function adds the fylke number and name based on the municipalities 
#' numbers or the fylke numbers.

#' @param kommune_nummer is the municipality number. Should be a character.
#' Default if NULL.
#' 
#'  @param fylke_nummer is the fylke number. Should be a character.
#' Default if NULL.
#' 
#' @return a vector with the fylke number and the fylke name
#' 
#' @export
#'
#' @examples

add_fylke <- function(kommune_nummer = NULL, fylke_nummer = NULL){
  
  
  # Create a fylke name and number dataset
  fylke_nb <- c("03", "11", "15", "18", "31", "32", "33", "34", "39", "40", "42", "46", "50", "55", "56")
  fylke_nm <- c("Oslo", "Rogaland", "Møre og Romsdal", "Nordland - Nordlánnda", "Østfold", "Akershus",
                "Buskerud", "Innlandet", "Vestfold", "Telemark", "Agder", "Vestland", "Trøndelag - Trööndelage",
                "Troms – Romsa – Tromssa", "Finnmark – Finnmárku – Finmarkku")
  
  fylke_df <- data.frame(fylke_number = fylke_nb,
                         fylke_name = fylke_nm)
  
  
  if(is_null(kommune_nummer) == TRUE){
    
    # Make fylke_nummer a df
    fylke_nummer <- as.data.frame(fylke_nummer)
    
    # Join
    fylke_name <- left_join(fylke_nummer, fylke_df, join_by(fylke_nummer == "fylke_number")) %>%
                    select(!fylke_nummer)
    
    # Return the column with the name
    return(fylke_name[["fylke_name"]])
    
  }else{
    
    # Fylke number
    fylke_nb <- substr(as.character(kommune_nummer), 1, 2)
    
    # Join with the fylke name and number dataframe
    fylke_name <- left_join(as.data.frame(fylke_nb), fylke_df, join_by(fylke_nb == "fylke_number")) %>%
                    select(!fylke_nb)
    
    return(list(fylke_nb, fylke_name[["fylke_name"]]))
  }
  

}
