#Tidy Tuesday Week 16

library(readxl)
library(shiny)
library(maptools)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(ggthemes)
library(plotly)
library(rsconnect)

rsconnect::setAccountInfo(name='emilyx',
                          token='16D0636507AB3B4EA31A97978661BB96',
                          secret='v7bz5jKqKrBNN9ttBvsk0nyDnSnZI5gchx4oa2+O')


exercise <- read_excel("data/week16_exercise.xlsx")

states <- map_data("state")
colnames(exercise) <- c("count", "region", "adults", "men", "women",
                        "men_working", "women_working", "men_nonworking",
                        "women_nonworking")
exercise$region <- tolower(exercise$region)

exercise_geo <- merge(states, exercise, by="region")
exercise_geo %>%
  mutate(men_nonworking = as.numeric(men_nonworking),
         women_nonworking = as.numeric(women_nonworking))

usa <- ggplot(exercise_geo, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill=adults), colour = "black") +
  theme_map() +
  labs(fill="adults") +
  scale_fill_gradientn(colours = c("pink", "blue")) +
  ggtitle("Geospatial visualization of state differences in meeting the federal exercise guideline") 

ggplotly(usa, tooltip = "fill")

## Build a shiny app

ui <- fluidPage(
  fluidRow(
    column(
      4, wellPanel(
        "An interactive tool to explore gender differences 
           between working and non-working adults in meeting the federal 
           exercise guideline. Data collected by the CDC, published in the
           National Health Statistics Reports.",
        radioButtons("gender", "Gender:",
                     c(Men = "men",
                       Women = "women",
                       Adults = "adults")),
        conditionalPanel(
          condition = "input.gender == 'men'",
          selectInput(
            "data1", "Breakdown",
            c("Working" = "men_working",
              "Non-working" = "men_nonworking")
          )
        ),
        conditionalPanel(
          condition = "input.gender == 'women'",
          selectInput(
            "data2", "Breakdown",
            c("Working" = "women_working",
              "Non-working" = "women_nonworking")
          )
        )
    
    ,
      conditionalPanel(
        condition = "input.gender == 'adults'",
        selectInput(
          "data3", "Breakdown",
          c("All" = "adults"
        )
      )
      
    ),
    actionButton("update", "Update"))),
    column(8,
           h2("Geospatial visualization of state differences in meeting the federal exercise guideline"),
           plotlyOutput("map"))
     
        
      
    
    
  
))

server <- function(input, output) {
  exercise <- read_excel("data/week16_exercise.xlsx")
  states <- map_data("state")
  colnames(exercise) <- c("count", "region", "adults", "men", "women",
                          "men_working", "women_working", "men_nonworking",
                          "women_nonworking")
  exercise$region <- tolower(exercise$region)
  
  exercise_geo <- merge(states, exercise, by="region")
  
  exercise_geo <- exercise_geo %>%
    mutate(men_nonworking = as.numeric(men_nonworking),
           women_nonworking = as.numeric(women_nonworking))
  
  update <- eventReactive (input$update, {
    if (input$gender == 'men') {
      if (input$data1 == "men_working") {
   
          ggplotly(ggplot(exercise_geo, aes(long, lat)) + 
            geom_polygon(aes(group=group, fill=get(input$data1), men=men_working, state=region), colour = "black") +
            theme_map() +
              labs(fill="Working men") +
              scale_fill_gradientn(colours = c("pink", "blue"))
        ,tooltip = c("men", "state"))
      } else {
       
          ggplotly(ggplot(exercise_geo, aes(long, lat)) + 
                     geom_polygon(aes(group=group, fill=get(input$data1),
                                      men=men_nonworking, state=region), colour = "black") +
                     theme_map() +
                     labs(fill="Non-working men") +
                     scale_fill_gradientn(colours = c("pink", "blue")),
                   tooltip = c("men", "state")
          ) 
        
      }
    } else if (input$gender == 'women'){
      if (input$data2 == "women_working") {
        
          ggplotly(ggplot(exercise_geo, aes(long, lat)) + 
                     geom_polygon(aes(group=group, fill=get(input$data2),
                                                            women=women_working,
                                      state=region), 
                                  colour = "black"
                                  ) +
                     theme_map() +
                     labs(fill="Working women") +
                     scale_fill_gradientn(colours = c("pink", "blue")),
                                          tooltip = c("women", "state")
          
        )
      } else  {
       
          ggplotly(ggplot(exercise_geo, aes(long, lat)) + 
                     geom_polygon(aes(group=group, fill=get(input$data2),
                                      women=women_nonworking,
                                      state=region), colour = "black"
                                  ) +
                     theme_map() +
                     labs(fill="Non-working women") +
                     scale_fill_gradientn(colours = c("pink", "blue")),
                   tooltip = c("women", "state")
          
        )
      }
    } else {
   
        ggplotly(ggplot(exercise_geo, aes(long, lat)) + 
                   geom_polygon(aes(group=group, fill=get(input$data3),
                                    adults=adults,
                                    state=region), colour = "black"
                               ) +
                   theme_map() +
                   labs(fill="All adults") +
                   scale_fill_gradientn(colours = c("pink", "blue")),
                                        tooltip=c("adults", "state")
        
      )
    }
    
    
  })
  
  output$map <- renderPlotly(
    update()
  )
}

shinyApp(ui, server)



