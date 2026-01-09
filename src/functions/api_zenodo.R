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
    
    #2# Link to metadata to find downloadable urls
    metadata_url <- paste0("https://zenodo.org/api/records/", dt_id)
    
    #3# Send metadata request to the server while suppressing possible server's erros
    metadata_resp <- request(metadata_url) %>% # create request
                      req_headers("Accept" = "application/json") %>%
                      req_error(.,
                                is_error = \(resp) FALSE, # never throw R error
                                body = .$Body) %>% # extract JSON from error response
                      req_perform() # perform server request

    
    #4# Stop the function and return error if there is a server error detected, or continue if no error
    if (resp_is_error(metadata_resp) == TRUE) {
      error_msg <- rawToChar(metadata_resp$body) %>%
                    gsub("[\r\n\t ]+", " ", .)
        
      return(cat("A server error occured. Here is the message and status of the error sent by the server:", error_msg, "\n"))
      
    }else{
      
      #4# Extract the metadata
      metada_zen <- request(metadata_url) %>%
                      req_headers("Accept" = "application/json") %>%
                      req_perform()
      
      #5# Read the data
      geojson_url <- resp_body_json(metada_zen)$files[[1]]$links$self
    }
    
    return(geojson_url)
  }
  
}

