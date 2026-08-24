##################################
#                                #
#   Merge data across all ids    #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Merge data available for all individuals, separately per data type
#'
#' @param data A named list with the data of each individual (the output of
#'   \code{\link{parse_data}}), named by participant identifier.
#' @param types A character vector with the data type names to be merged.
#'   Defaults to the five standardized types and the two daily insulin totals.
#'
#' @return A named list with one data frame per data type, holding the data of
#'   all individuals. Individuals without data of a given type contribute no rows.
#' @export
merge_all <- function(data, types = c("cgm", "basal", "bolus", "carbs", "SMBG",
                                      "total_basal", "total_bolus")){
  # Merge: CGM, basal, bolus, carbs and SMBG (standardized formats)
  
  result <- list()
  for (type in types){
    type_list <- list()
    
    for (id in names(data)){
      df <- data[[id]][[type]]
      if (is.null(df)) next  # type doesn't exist for this id
      type_list[[id]] <- df %>% 
        mutate(across(where(is.character), \(x) type.convert(x, as.is = TRUE))) # to avoid type-mismatches
    }
    
    result[[type]] <- bind_rows(type_list)
  }
  
  return(result)
}
