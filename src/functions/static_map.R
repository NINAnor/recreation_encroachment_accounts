#' Create an static map
#'
#' This function creates either one single static map or two static
#' maps displayed side-by-side. This function uses tmap version 4.

#' @param data1 sf object of the data that should be mapped.
#' 
#' @param data2 sf object of the data that should be mapped. To be used only if
#' one needs to display tow interactive maps side-by-side. data2 can be the same
#' sf object as data1.
#' 
#' @param fill_variable1 column name to be used to fill the polygons when mapping 
#' data1. Should be a character.
#' 
#' @param fill_variable2 column name to be used to fill the polygons when mapping 
#' data2. Should be a character.
#' 
#' @param legend_title1 title of the legend for data1 map. Should be a character.
#' 
#' @param legend_title2 title of the legend for data1 map. Should be a character.
#' 
#' @param map_title1 title of data1 map. Should be a character.
#' 
#' @param map_title2 title of data2 map. Should be a character.
#' 
#' @return an interactive map.
#' 
#' @export
#'
#' @examples

static_map <- function(data1, data2 = NULL, fill_variable1, fill_variable2 = NULL, legend_title1, legend_title2 = NULL, map_title1, map_title2 = NULL){
  # Set interactive mode
  tmap_mode("plot")
  
  if(is.null(data2)){
  
  # Create first map
  map1 <-
    
    tm_shape(data1) +
    
    tm_polygons(fill = {{fill_variable1}},
                fill.scale = tm_scale_intervals(n = 5, style = "quantile", values = "brewer.reds"),
                col = "black",
                lwd = 1,
                fill.legend = tm_legend(title = {{legend_title1}},
                                        orientation = "landscape",
                                        position = c("center", "bottom"))) +
    
    
    tm_layout(inner.margins = c(0.22, 0.05, 0.1, 0.05),
              legend.outside = TRUE,
              legend.size = 0.1,
              frame = FALSE) +
    
    tm_title(text = {{map_title1}},
             size = 1,
             fontface = "bold",
             position = c("left", "top")) 
  
  # Return results
  return(map1)
  
  }else{
    
    # Create first map
    map1 <-
      
      tm_shape(data1) +
      
      tm_polygons(fill = {{fill_variable1}},
                  fill.scale = tm_scale_intervals(n = 5, style = "quantile", values = "brewer.reds"),
                  col = "black",
                  lwd = 1,
                  fill.legend = tm_legend(title = {{legend_title1}},
                                          orientation = "landscape",
                                          position = c("center", "bottom"))) +
      
      
      tm_layout(inner.margins = c(0.22, 0.05, 0.1, 0.05),
                legend.outside = TRUE,
                legend.size = 0.1,
                frame = FALSE) +
      
      tm_title(text = {{map_title1}},
               size = 1,
               fontface = "bold",
               position = c("left", "top"))
      
    # Create second map
    map2 <-
      
      tm_shape(data2) +
      
      tm_polygons(fill = {{fill_variable2}},
                  fill.scale = tm_scale_intervals(n = 5, style = "quantile", values = "brewer.reds"),
                  col = "black",
                  lwd = 1,
                  fill.legend = tm_legend(title = {{legend_title2}},
                                          orientation = "landscape",
                                          position = c("center", "bottom"))) +
      
      
      tm_layout(inner.margins = c(0.22, 0.05, 0.1, 0.05),
                legend.outside = TRUE,
                legend.size = 0.1,
                frame = FALSE) +
      
      tm_title(text = {{map_title2}},
               size = 1,
               fontface = "bold",
               position = c("left", "top"))
    
    # Create the map display
    map_display <- tmap_arrange(map1, map2, ncol = 2, sync = FALSE)
    
    # Return results
    return(map_display)
  }
  
}





