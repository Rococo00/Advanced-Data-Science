library(shiny)      
library(tidyverse)
library(gapminder)



ui = fluidPage(
  
  titlePanel("Shiny Gapminder"),#titolo
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(
        inputId = 'year',
        label = 'Anno',
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = 1952,
        step = 5
      ),
      
      selectInput(
        inputId = 'continent',
        label = 'Continente',
        choices = c('All',levels(gapminder$continent)),
        selected = 'All'
      ),
      
      radioButtons(
        inputId = 'x',
        label = 'X',
        choices = c('lifeExp','pop','gdpPercap'),
        selected = 'gdpPercap'
      ),
      radioButtons(
        inputId = 'y',
        label = 'Y',
        choices = c('lifeExp','pop','gdpPercap'),
        selected = 'lifeExp'
      )
    ),
    mainPanel(
      plotOutput('scatter')
    )
  )
)




server = function(input, output, session) {
  
  # Seleziona i dati dell'anno e del continente scelti.
  filtered_gapminder = reactive({
    
    dati = gapminder |>
      filter(year == input$year)
    
    if (input$continent != "All") {
      dati = dati |>
        filter(continent == input$continent)
    }
    
    return(dati)
  })
  
  
  output$scatter = renderPlot({
    
    plot = filtered_gapminder() |>
      ggplot(
        aes(
          x = .data[[input$x]],
          y = .data[[input$y]],
          size = pop,
          colour = country
        )
      ) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_x_log10() +
      scale_size(range=c(2,20))+
      labs(x = input$x, y = input$y) +
      theme_minimal()
    
    plot
  })
  
} 

shinyApp(ui, server)
