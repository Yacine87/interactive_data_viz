#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


  # Data 
  load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))
  
  # Load libraries 
  library(shinydashboard) 
  library(shiny) 
  library(ggplot2)
  
  # User interface 
  ui <- shinyUI(
    dashboardPage(
      dashboardHeader(title = "Interactive data visualisation"), 
      dashboardSidebar(
        
        tags$h3("Deux variables numériques"),
        # x 
        selectInput("x", "Choisir X", choices = c("imdb_num_votes", 
                                                  "dvd_rel_year", 
                                                  "critics_score", 
                                                  "runtime", 
                                                  "audience_score")), 
        # y 
        selectInput("y", "Choisir Y", choices = c("imdb_num_votes", 
                                                  "dvd_rel_year", 
                                                  "critics_score", 
                                                  "runtime", 
                                                  "audience_score")), 
        checkboxInput("choisirPlot", 
                      "Boxplot", 
                      value = FALSE, 
                      width = NULL), 
        
        checkboxInput("facet1", 
                      "Facet", 
                      value = FALSE, 
                      width = NULL), 
        
        # Facet1 : choisir variable catégorielle 
        selectInput("f1", 
                    "Facet", 
                    choices = c("best_dir_win", 
                                "title_type", 
                                "top200_box", 
                                "genre", 
                                "best_pic_nom", 
                                "best_pic_win", 
                                "critics_rating", 
                                "studio")),
        
        # Une variable catégorielle
        tags$h3("Une variable catégorielle"),
        # c1
        selectInput("c1", 
                    "Choisir une variable catégorielle", 
                    choices = c("best_dir_win", 
                                "title_type", 
                                "top200_box", 
                                "genre", 
                                "best_pic_nom", 
                                "best_pic_win", 
                                "critics_rating", 
                                "studio")), 
        
        checkboxInput("facet2", 
                      "Facet", 
                      value = FALSE, 
                      width = NULL), 
        
        # Facet2 : choisir une variable 
        selectInput("f2", 
                    "Choisir une variable catégorielle - Facet", 
                    choices = c("best_dir_win", 
                                "title_type", 
                                "top200_box", 
                                "genre", 
                                "best_pic_nom", 
                                "best_pic_win", 
                                "critics_rating", 
                                "studio")), 
        
        # Une variable num * une variable catégorielle : Boxplot()
        tags$h3("Une variable numérique vs une variable catégorielle"),
        
        selectInput("z1", 
                    "Choisir une variable numériuque", 
                    choices = c("imdb_num_votes", 
                                "dvd_rel_year", 
                                "critics_score", 
                                "runtime", 
                                "audience_score")), 
        
        selectInput("z2", 
                    "Choisir une variable catégorielle", 
                    choices = c("best_dir_win", 
                                "title_type", 
                                "top200_box", 
                                "genre", 
                                "best_pic_nom", 
                                "best_pic_win", 
                                "critics_rating", 
                                "studio")), 
        
        checkboxInput("facet3", 
                      "Facet", 
                      value = FALSE, 
                      width = NULL), 
        
        # Facet 3 : choisir une variable catégorielle 
        selectInput("f3", 
                    "Choisir une variable catégorielle", 
                    choices = c("best_dir_win", 
                                "title_type", 
                                "top200_box", 
                                "genre", 
                                "best_pic_nom", 
                                "best_pic_win", 
                                "critics_rating", 
                                "studio"))
      ), 
      
      # App's Body 
      dashboardBody(
        fluidRow(
          # Deux variables numériques
          box(plotOutput(outputId = "mainPlot_Num"), title = "Croisment de deux variabeles numériques"),
          box(plotOutput(outputId = "densityY"), title = "Densité de probabilité de Y"),
          box(plotOutput(outputId = "densityX"), title = "Densité de probabilité de X"), 
          
          # Une variable catégorielle
          box(plotOutput(outputId = "mainPlot_Cat"), title = "Visualisation de vos variables catégorielles"), 
          
          # Une variable num * une variable catégorielle 
          box(plotOutput(outputId = "mainPlot_Cat_Num"), title = "Croisment d'une variable numérique et une autre catégorielle")
          
        )
      )
    )
  )
  
  
  # Server 
  server <- shinyServer(function(input, output){
    
    # Deux variable numérique - facet option  
    output$mainPlot_Num <- renderPlot({
      
      if (input$choisirPlot == TRUE & input$facet1 == FALSE){
        ggplot(data = movies) + 
          aes(x = get(input$x), 
              y = get(input$y)) + 
          geom_boxplot() 
        
      } else if (input$choisirPlot == TRUE & input$facet1 == TRUE) {
        ggplot(data = movies) + 
          aes(x = get(input$x), 
              y = get(input$y)) + 
          geom_boxplot() + facet_wrap(~ get(input$f1)) 
        
      } else if (input$choisirPlot == FALSE & input$facet1 == FALSE) {
        ggplot(data = movies) + 
          aes(x = get(input$x), 
              y = get(input$y)) + 
          geom_point() 
        
      } else {
        ggplot(data = movies) + 
          aes(x = get(input$x), 
              y = get(input$y)) + 
          geom_point() + facet_wrap(~ get(input$f1))
        
      }
    })
    # Densité de y 
    output$densityY <- renderPlot({
      if (input$facet1 == FALSE){
        ggplot(data = movies) + 
          aes(x = get(input$y)) + 
          geom_density()  
      } else {
        ggplot(data = movies) + 
          aes(x = get(input$y)) + 
          geom_density() + facet_wrap(~ get(input$f1))
      }
    })
    
    # Densité de x 
    output$densityX <- renderPlot({
      
      if (input$facet1 == FALSE) {
        ggplot(data = movies) + 
          aes(x = get(input$x)) + 
          geom_density()
      } else {
        ggplot(data = movies) + 
          aes(x = get(input$x)) + 
          geom_density() + facet_wrap(~ get(input$f1))
      }
      
    })
    
    ####################
    
    # Une variable catégorielle - facet option  
    
    output$mainPlot_Cat <- renderPlot({
      if (input$facet2 == FALSE){
        ggplot(data = movies) + 
          aes(x = get(input$c1)) + 
          geom_bar()
      } else {
        ggplot(data = movies) + 
          aes(x = get(input$c1)) + 
          geom_bar() + facet_wrap(~ get(input$f2))
      }
      
    })
    
    ####################
    # Une variable catégorielle vs une variable numérique - facet option  
    output$mainPlot_Cat_Num <- renderPlot({
      
      if (input$facet3 == FALSE){
        ggplot(data = movies) + 
          aes(x = get(input$z2), y = get(input$z1)) + 
          geom_boxplot()
      } else {
        ggplot(data = movies) + 
          aes(x = get(input$z2), y = get(input$z1)) + 
          geom_boxplot() + facet_wrap(~ get(input$f3))
      }
    })
  })
  
  shinyApp(ui, server)

