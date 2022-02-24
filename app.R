library(shiny)
library(stringr)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)



#------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300&family=Lato:wght@100;300;700&display=swap"),
    tags$link( href="https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap"),
    
    tags$link(rel="stylesheet", href="styles.css"),
    shiny::tags$link(rel = "icon", href ="imgs/favicon16.png?v=2"),
    tags$title("QA Follow-Up")
  ),
  
  tags$div(class = "container-header",
    
    tags$img(src = "imgs/logo-1.png", class = "logo"),
    uiOutput("last_modification", class = "text_update")
    ),
  
  navbarPage("Follow UP Survey",
             
             tabPanel("Provincias",
                      DT::DTOutput("table_provincias"),
                      plotOutput("chart_provincias", width = "90%"),
                      
                      
             ),
             tabPanel("Cidades",
                      DT::DTOutput("table_cidades"),
                      plotOutput("chart_cidades"),),
             tabPanel("Entrevistas",
                      DT::DTOutput("table_interviews"),)
  )  
  
  
)

server <- function(input, output, session) {
  
  #read data  --------------------------
  qa_data <- import_data(dir_data)
  
  output$last_modification <- renderUI({
    
    last_update <- get_last_update(file.path(dir_data,'interviews.csv'))
    
    last_update
    
    
  })
  
  output$table_provincias <- DT::renderDataTable(
    datatable(
      
      qa_data$summary_provincias, 
      options = list(pageLength = 12,
                     dom = 't',
                     columnDefs = list(list(className = 'dt-center', targets = 2:7),
                                       list(width = '50px', targets = c(2:7)),
                                       list(width = '1px', targets = c(0)),
                                       list(width = '100px', targets = c(1))
      )),
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
    
    
    provs = unique(data_plot$provincia)
    #print(which(provs=="Total"))
    
    data_plot <- data_plot %>%
      mutate(provincia = factor(provincia,
                                levels = rev(c(sort(provs[-which(provs=="Total")]), "Total"))))
    
    plot_progress(data_plot,
                  y_var = provincia)
    
    
    
    
  }, res = 96)
  
  output$chart_cidades <- renderPlot({
    
    data_plot <- create_data_plot(qa_data$summary_cidades,
                                  cols_pivot = c(provincia, cidade),
                                  provincia, cidade) %>%
      mutate(cidade = if_else(str_detect(cidade, "Total"), "Total", cidade))
    
    cidades = unique(data_plot$cidade)
   
    data_plot <- data_plot %>%
      mutate(cidade = factor(cidade,
                                levels = rev(c(sort(cidades[-which(cidades=="Total")]), "Total"))))
    
    plot_progress(data_plot,
                  y_var = cidade)
    
    
  },  res = 96)
  
  
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
  
  #output$tbl_c = DT::renderDataTable(datatable(data,options=list(search=list(search=iconv(input$tag,to = "UTF-8"),regex=T))))
  
  
  
  output$table_interviews <- DT::renderDataTable(
    datatable(
      qa_data$interviews %>%
        mutate(across(c(cidade, bairro, url), function(x)str_replace(x, "ç", "c")),
               url = glue::glue('<a href="http://my.muvasurveys.com/Interview/Review/{interview__id}"target="_blank">Link</a>')) %>%
        select(-interview__id) %>%
        relocate(inquiridor, participante, .after = ID_participant),
      rownames = F,
      escape = F,
      options = list( pageLength = 100
                      
                      
      )) %>%
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



