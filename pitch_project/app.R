#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)

mlb_pitches_21 = readRDS(url('https://stat385.org/data/mlb_pitches_2021.rds'))
mlb_pitches_21 = drop_na(mlb_pitches_21)

degrom_starting = mlb_pitches_21 |> 
  filter(name == 'Jacob deGrom')


ui <- navbarPage(title = 'MLB Pitches',
    
     tabPanel('Visualization / Stats',
              titlePanel(title = 'Velocity vs Spin Rate Graph'),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "pitcher",
                              label = 'Pitcher:',
                              choices = sort(unique(mlb_pitches_21$name)), 
                              selected = 'Jacob deGrom'),
                  
                  numericInput(inputId = 'velo',
                               label = 'Pitch Velocity:',
                               min = 30,
                               max = 105,
                               value = 99,
                               step = 0.5),
                  
                  numericInput(inputId = 'spin',
                               label = 'Pitch Spin Speed',
                               min = 100,
                               max = 4000,
                               value = 2400,
                               step = 50), 
                
                dateInput(inputId = 'startDate',
                          label = 'Start Date:',
                          value = min(as.Date(degrom_starting$game_date))),
                
                dateInput(inputId = 'endDate',
                          label = 'End Date:',
                          value = max(as.Date(degrom_starting$game_date)))),
                
                  
                mainPanel(
                  plotOutput("scatter"),
                  tableOutput('pitch_stats'),
                  span(textOutput('pitch_guess'), style = ('font-size: 30px'))
                ))),
    
     tabPanel('Table', dataTableOutput('table')),
     tabPanel('About', includeMarkdown('About.Rmd')),                      
    )

server <- function(input, output) {
  
  mlb_pitcher = reactive({
    mlb_pitches_21 |> 
      filter(name == input$pitcher) 
  })
    
    observeEvent(
     eventExpr = input$pitcher,
      handlerExpr = {
       updateDateInput(inputId = 'startDate',
                       value = min(as.Date(mlb_pitcher()$game_date)))      
        }
    )
    
    observeEvent(
      eventExpr = input$pitcher,
      handlerExpr = {
        updateDateInput(inputId = 'endDate',
                        value = max(as.Date(mlb_pitcher()$game_date)))      
        }
    )
    
    
    output$scatter <- renderPlot({
      mlb_pitcher() |> 
        filter(game_date >= input$startDate & game_date <= input$endDate) |> 
        rename(Velocity = 'release_speed', Spin_Rate = 'release_spin_rate') |> 
        ggplot() +
        aes(x = Velocity, y = Spin_Rate, color = pitch_type) + 
        geom_point() +
        geom_point(aes(x = input$velo,
                       y = input$spin),
                   size = 3,
                   color = 'black')
             
    })
    
    output$pitch_stats <- renderTable({
      mlb_pitcher() |>  
        filter(game_date >= input$startDate & game_date <= input$endDate) |> 
        group_by(pitch_type) |> 
        summarise(frequency = n(),
                  Avg_Velo = mean(release_speed),
                  Sd_Velo = sd(release_speed),
                  Avg_Spin = mean(release_spin_rate),
                  Sd_Spin = sd(release_spin_rate)) |> 
        mutate(Thrown_Pct = round(frequency / sum(frequency), digits = 3))|> 
        select(-c(frequency))
        
    })
  
    output$pitch_guess <- renderText({
      result_table = mlb_pitcher() |>  
        filter(game_date >= input$startDate & game_date <= input$endDate) |> 
          group_by(pitch_type) |> 
          summarise(frequency = n(),
                    Avg_Velo = mean(release_speed),
                    Avg_Spin = mean(release_spin_rate),
                    Std_Velo = sd(release_speed),
                    Std_Spin = sd(release_spin_rate)) |> 
          mutate(Thrown_Pct = round(frequency / sum(frequency), digits = 3),
                 avg_dist = find_dist(input_velo = input$velo,
                                      input_spin = input$spin,
                                      avg_velo = Avg_Velo,
                                      avg_spin = Avg_Spin,
                                      sd_velo = Std_Velo,
                                      sd_spin = Std_Spin)) |>
          na.omit() |> 
          filter(avg_dist == min(avg_dist)) 
        
        statement = c('Predicted pitch:', result_table$pitch_type)
        return(statement)
        
    })
        
        output$table = renderDataTable({
          mlb_pitcher()
        })
}

# Run the application 
shinyApp(ui = ui, server = server)


