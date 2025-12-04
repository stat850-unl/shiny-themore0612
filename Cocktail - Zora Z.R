library(shiny)
library(readr)
library(ggplot2)
library(dplyr)

cocktails <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-26/cocktails.csv")
head(cocktails)

ingredient_counts <- cocktails %>%
  filter(!is.na(ingredient)) %>%
  count(ingredient, name = "count") %>%
  arrange(desc(count))

ingredient_choices <- ingredient_counts %>%
  arrange(ingredient) %>%
  pull(ingredient)

category_counts <- cocktails %>%
  filter(!is.na(category), !is.na(drink)) %>%
  distinct(drink, category) %>%
  count(category, name = "n_drinks") %>%
  arrange(desc(n_drinks))

ui <- fluidPage(
  
  titlePanel("Cocktail Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "top_n",
        label   = "Number of top ingredients to show:",
        min     = 5,
        max     = 50,
        value   = 20
      ),
      
      selectInput(
        inputId  = "ingredient",
        label    = "Filter recipes by ingredient:",
        choices  = ingredient_choices,
        selected = "Vodka"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Top Ingredients",
          plotOutput("ingredientPlot", height = "500px")
        ),
        tabPanel(
          "Drinks by Category",
          plotOutput("categoryPlot", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #Top Ingredients
  output$ingredientPlot <- renderPlot({
    top_df <- ingredient_counts %>%
      slice_head(n = input$top_n)
    
    ggplot(top_df,
           aes(x = reorder(ingredient, count), y = count)) +
      geom_col(fill = "pink") +
      coord_flip() +
      labs(
        title = paste("Top", input$top_n, "cocktail ingredients"),
        x = "Ingredient",
        y = "Count"
      )
  })
  
  #Drinks by Category
  output$categoryPlot <- renderPlot({
    ggplot(category_counts,
           aes(x = reorder(category, n_drinks), y = n_drinks)) +
      geom_col(fill = "skyblue") +
      coord_flip() +
      labs(
        title = "Number of distinct drinks by category",
        x = "Category",
        y = "Number of drinks"
      )
  })
  
  filtered_recipes <- reactive({
    cocktails %>%
      filter(ingredient == input$ingredient) %>%
      select(name, category, alcoholic, glass, ingredient, measure)
  })
  
  output$recipesTitle <- renderText({
    n <- nrow(filtered_recipes())
    paste0("Found ", n, " rows using ingredient: ", input$ingredient)
  })
  
  output$recipesTable <- renderTable({
    filtered_recipes()
  })
}

shinyApp(ui = ui, server = server)