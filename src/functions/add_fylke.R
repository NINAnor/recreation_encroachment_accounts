#' Add fylke
#'
#' This function adds the fylke number based on the municipalities numbers.

#' @param kommune_nummer is the municipality number. Should be a character.
#' 
#' @return the fylke number
#' 
#' @export
#'
#' @examples

add_fylke <- function(kommune_nummer){
  
  fylke_nb <- substr(as.character(kommune_nummer), 1, 2)
  
  return(fylke_nb)
  }