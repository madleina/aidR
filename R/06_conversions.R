##################################
#                                #
#  Conversion to other formats   #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Convert CGM data to a format compatible with the R-package iglu.
#'
#' @param data A list with all data found for one or multiple individuals.
#'
#' @return A data frame compatible with the R-package iglu.
#' @export
get_iglu_format <- function(data){
  if (!inherits(data, "list") || 
      !("cgm" %in% names(data)) || 
      !("value" %in% names(data$cgm))){
    stop("Require a list with attribute 'cgm'. 
         Please make sure parse_data() was run with clean = TRUE.")
  }
  
  iglu_df <- iglu::process_data(data$cgm, id = "id", timestamp = "timestamp", glu = "value")
  return(iglu_df)
}
