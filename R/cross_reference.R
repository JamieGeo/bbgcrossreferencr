#' Cross-reference List of all IRIS Plants with Heritage Lists
#'
#' @param heritage a dataframe containing the list of heritage plants of concern
#' @param allplants a dataframe containing the entire catalogue of plants on IRIS
#' @param themecode the numerical code associated with that heritage list
#' @param themetype description of the heritage list theme
#'
#' @return A saved table containing plant names that appears on the heritage
#' list as well as the all plant list on IRIS
#' @export
#'
#' @examples
#' themecode <- "O1893"
#' themetype <- e.g. "Catalogue of Orchids 1893"
cross_reference <- function(heritage, allplants, themecode, themetype) {
  library(dplyr)
  library(readxl)
  library(tidyr)
  #filter the whole plant list to extract heritage plants
  filteredlist <- allplants %>%
    filter(TaxonName %in% heritage$Namematchname)

  #add in the theme code to database
  filteredlist <- filteredlist %>%
    mutate(ThemeTypeCode = if_else(is.na(ThemeTypeCode),
                                   themecode, ThemeTypeCode))

  #add in the theme type to database
  filteredlist <- filteredlist %>%
    mutate(ThemeType = if_else(is.na(ThemeType),
                               themetype, ThemeType))

  #save the table
  write.table(filteredlist,
              file = paste(themetype, "_present in gardens.csv", sep="")
              ,sep = ',')

}
