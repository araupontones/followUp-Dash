library(shiny)
library(stringr)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
#read data 
qa_data <- import_data(dir_data)



  





ui <- fluidPage(
navbarPage("Follow UP",
           
           tabPanel("Provincias",
                    DT::DTOutput("table_provincias"),
                    plotOutput("chart_provincias"),
                    
                    
                    ),
           tabPanel("Cidades"),
           tabPanel("Entrevistas")
           )  
  
  
)

server <- function(input, output, session) {
  
  output$table_provincias <- DT::renderDataTable(
    
    qa_data$summary_provincias
  )
  
  output$chart_provincias <- renderPlot({
    
    
    
    data_plot <- qa_data$summary_provincias %>%
      select(-status, - sampled, - Approved) %>%
      pivot_longer(cols = -provincia,
                   names_to = "Status") %>%
      group_by(provincia) %>%
      mutate(Total = sum(value),
             Perc = value / Total) %>%
      ungroup() %>%
      mutate(Status = factor(Status,
                             levels = rev(c("Completas", "Nao conseguimos", "Rejected", "Sim visitar")),
                             ordered = T)
      )
    
    
    data_plot %>%
      ggplot(aes(y = provincia,
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
    
    
    
  }, res = 96)
  
}

shinyApp(ui, server)



