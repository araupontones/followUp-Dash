library(shiny)
library(stringr)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)




#------------------------------------







ui <- fluidPage(
  navbarPage("Follow UP",
             
             tabPanel("Provincias",
                      DT::DTOutput("table_provincias"),
                      plotOutput("chart_provincias"),
                      
                      
             ),
             tabPanel("Cidades",
                      DT::DTOutput("table_cidades"),
                      plotOutput("chart_cidades"),),
             tabPanel("Entrevistas",
                      DT::DTOutput("table_interviews"),)
  )  
  
  
)

server <- function(input, output, session) {
  
  #read data 
  qa_data <- import_data(dir_data)
  
  output$table_provincias <- DT::renderDataTable(
    datatable(
      
      qa_data$summary_provincias, 
      rownames = F
      
    ) %>%
      formatStyle(
        columns = 'status',
        backgroundColor = styleEqual("CHECK!", color_rejected),
        color = styleEqual("CHECK!", "white")
      )
    
  )
  
  output$chart_provincias <- renderPlot({
    
    
    
    data_plot <- create_data_plot(qa_data$summary_provincias,
                             cols_pivot = c(provincia),
                             provincia)
    
    
    plot_progress(data_plot,
                  y_var = provincia)
    
    
    
    
  }, res = 96)
  
  output$table_cidades <- DT::renderDataTable(
    datatable(
      qa_data$summary_cidades, 
      options = list( pageLength = 23),
      rownames = F
      
    ) %>%
      formatStyle(
        columns = 'status',
        backgroundColor = styleEqual("CHECK!", color_rejected),
        color = styleEqual("CHECK!", "white")
      )
    
  )
  
  
  output$chart_cidades <- renderPlot({
    
    data_plot <- create_data_plot(qa_data$summary_cidades,
                                  cols_pivot = c(provincia, cidade),
                                  provincia, cidade)
    
    
    plot_progress(data_plot,
                  y_var = cidade)
    
    
  })
  
  output$table_interviews <- DT::renderDataTable(
    datatable(
      qa_data$interviews,
      rownames = F,
      escape = F,
      filter = 'top',
      options = list( pageLength = 100)
      
    ) %>%
    formatStyle('Management', 
                target = 'row',
                backgroundColor = styleEqual(c("APPROVED", "REJECTED", "Sin visitar")
                                             , c(color_approved, color_rejected, color_naoConseguimos)),
                color = styleEqual(c("APPROVED", "REJECTED", "Sin visitar")
                                                      , c("black", "white", "black"))
    )
   
    
  )
}

shinyApp(ui, server)



