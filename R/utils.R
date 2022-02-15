

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


#' create data for plot =======================================================

#' @param .data summary data of the province or city level
#' @param cols_pivot columns to keep constant after pivot lonter
#' @param ... Variables to group by

create_data_plot <- function(.data,
                             cols_pivot,
                             ...){
  
  nm <-deparse(substitute(cols_pivot))
  
  message(nm)
  
  data_plot <- .data %>%
    select(-status, - sampled, - Approved) %>%
    pivot_longer(cols = -{{cols_pivot}},
                 names_to = "Status") %>%
    group_by(...) %>%
    mutate(Total = sum(value),
           Perc = value / Total) %>%
    ungroup() %>%
    mutate(Status = factor(Status,
                           levels = rev(c("Completas", "Nao conseguimos", "Rejected", "Sim visitar")),
                           ordered = T)
    )
  
  if(str_detect(nm, "cidade")){
    
    data_plot <- data_plot %>% mutate(cidade = paste(provincia, cidade, sep = "-"))
    
    
  }
  
  return(data_plot)
  
}



#plot ---------------------------------------------------------------------------


plot_progress <- function(.data,
                          y_var){
  .data %>%
    ggplot(aes(y = {{y_var}},
               x = Perc, 
               fill = Status))+
    geom_col(width = .8) +
    scale_fill_manual(name = "Status",
                      values = rev(c(color_simVisitar, color_rejected, color_naoConseguimos, color_completed)),
                      breaks = c("Completas", "Nao conseguimos", "Rejected", "Sim visitar")
    ) +
    scale_x_continuous(labels = function(x) c(seq(0,75, 25), paste0(100, "%"))) +
    labs(y = "",
         x = "From sampled") +
    
    
    theme(legend.position = 'top',
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_line("#CCCCCC", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.ontop = TRUE)
  
  
  
}

