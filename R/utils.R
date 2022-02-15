

#' Import data from QA directory
#' @param dir_data path to dashboard data in QA directory
import_data <- function(dir_data = dir_data){
  
  #list files in dir data
  csvs <- list.files(dir_data, pattern = ".csv", full.names = T)
  
  #define names of datasets
  names_geos <- str_extract(csvs,'(?>summary|interviews).*(?=\\.csv)' )
  
  #import all data sets
  my_data <-lapply(csvs, function(csv){
    
    rio::import(csv, encoding = "UTF-8")
    
    
  })
  
  #assign names to list
  names(my_data) <- names_geos
  
  return(my_data)
  
  
  
}
