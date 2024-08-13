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
