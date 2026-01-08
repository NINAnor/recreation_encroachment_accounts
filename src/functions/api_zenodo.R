#' Read data from Zenodo through an API request
#'
#' This function aims at reading the encroachment, or built-in areas, in 
#' nature in Norway stored in a [Zenodo repository](https://zenodo.org/records/15130992)
#' through an API request. It is not guaranteed that this function will 
#' work "as is" with other datasets from Zenodo. Minor adjustments might
#' be needed.

#' @param zenodo_url the url of the Zenodo dataset. It should be entered as a
#' character.
#'
#' @return a downloadable url that can then be read using the appropriate 
#' function relevant to the class of the data downloaded.
#' @export
#'
#' @examples

api_zenodo <- function(zenodo_url){
  
  if(is.character(zenodo_url) == FALSE){
    
    return("The URL should be entered as a character.")
    
  }else{
    
    #1# Extract dataset ID from Zenodo
    dt_id <- sub(".*records/([0-9]+).*", "\\1", zenodo_url)
    
    #2# Get recorded metadata to find downloadable urls
    metadata_url <- paste0("https://zenodo.org/api/records/", dt_id)
    metadata_resp <- GET(metadata_url)
    
    #3# Stop the function and return error if necessary, or continue if url is correct
    if (status_code(metadata_resp) >= 300) {
      error_msg <- content(metadata_resp, "text", encoding = "UTF-8") %>% 
        fromJSON()
      return(list(Error = "An error occured, below are the server's status and message.", 
                  Server_errors = error_msg))
    }else{
      
      #4# Extract the metadata
      metada_zen <- content(metadata_resp, "text", encoding = "UTF-8") %>%
        fromJSON()
      
      #5# Read the data
      geojson_url <- metada_zen$files$links
    }
    
    return(geojson_url)
  }
  
}

