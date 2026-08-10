##################################
#                                #
#   Merge data across all ids    #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Merge data available for all individuals, separately per data type.
#'
#' @param data A list with data found for all individual.
#'
#' @return A list with data found for all individual, merged for each data type.
#' @export
merge_all <- function(data){
  # Merge: CGM, basal, bolus and carbs (standardized formats)
  
  result <- list()
  for (type in c("cgm", "basal", "bolus", "carbs")){
    type_list <- list()
    
    for (id in names(data)){
      df <- data[[id]][[type]]
      if (is.null(df)) next  # type doesn't exist for this id
      type_list[[id]] <- df %>% 
        mutate(id = id) %>% 
        relocate(id) %>% # move id to front
        mutate(across(where(is.character), \(x) type.convert(x, as.is = TRUE))) # to avoid type-mismatches
    }
    
    result[[type]] <- bind_rows(type_list)
  }
  
  return(result)
}
