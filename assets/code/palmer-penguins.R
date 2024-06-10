library(shiny)
library(bslib)
library(ggplot2)
library(palmerpenguins)

shinyApp(
  ui = page_fluid(
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("sex", "Sex", unique(penguins$sex), "female"),
        numericInput(
          "year", 
          "Year", 
          2009, 
          min(unique(penguins$year)),
         max(unique(penguins$year))
        ),
        selectInput(
          "xvar", 
          "X var", 
          colnames(dplyr::select(penguins, where(is.numeric))),
          "flipper_length_mm"
        ),
        selectInput(
          "yvar",
          "Y var",
          colnames(dplyr::select(penguins, where(is.numeric))),
          "body_mass_g"
        ),
        selectInput(
          "color",
          "Color and shape",
          colnames(dplyr::select(penguins, where(is.factor))),
          "species"
        )
      ),
      plotOutput("plot")
    )
  ),
  server = function(input, output, session) {
    output$plot <- renderPlot({
      penguins |>
        filter(sex == !!input$sex, year == !!input$year) |>
        ggplot(aes(x = !!input$xvar, y = !!input$yvar)) +
        geom_point(aes(color = !!input$color, shape = !!input$color), size = 2)
    })
  }
)