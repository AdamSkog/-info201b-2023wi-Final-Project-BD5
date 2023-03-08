#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

data <- read_delim("./Checkouts_by_Title.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Seattle Public Library Checkouts"),
  tabsetPanel(
    tabPanel("Overview",
             p("This app is geared towards helping library management figure out how to stock their shelves.
               The major questions we are exploring in this project are:"),
             p("1. What are the most popular types of media being checked out?"),
             p("2. How have the total checkouts of these media types changed over time?"),
             p("3. How have the total checkouts of the two usage classes changed over time?"),
             p("4. Which months have the most and fewest checkouts?"),
             p("This dataset includes a", em("monthly"),"count of Seattle Public Library checkouts by 
                        title for physical and electronic items."),
             p("The dataset begins with checkouts that occurred in April 2005."),
             p("We have", strong(nrow(data)), "rows of data and", strong(ncol(data)), "columns."),
             p("Here are the first few rows of the dataset."),
             mainPanel(tableOutput("headData"))),
    tabPanel(
      "Popular Plots",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("type",
                             "Which material type do you want?",
                             choices = list("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK"),
                             selected = list("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK")
          ),
          checkboxInput("display", "Display Line", TRUE)
        ),
        mainPanel(plotOutput("popular"))
      )
    ),
    tabPanel("Usage Class Data",
      sidebarLayout(
        sidebarPanel(
          p("There are two usage types that are accounted for in the Seattle Public Library,", strong("Physical"), 
            "and", strong("Digital."), "\nThe following plot can display these types, and show a",
            em("trend line"), "corresponding to its type."),
          checkboxInput("usageTypeDisplay", "Display Trend line", F),
          checkboxGroupInput("usagetype",
                             "Which usage type do you want to see?",
                             choices = list("Physical", "Digital"),
                             selected = list("Physical", "Digital")
            
          )
        ),
        mainPanel(
          plotOutput("usageclassplot"),
          textOutput("usageclass_summary"),
          textOutput("usageclass_max"),
          p(),
          p(strong("Why do we want to know this information?")),
          p("We learn the valuable information regarding which types are most popular, and we can predict the
            most purchased types in the future by use of the", em("trend line"), "which allows for the Seattle
            Public Library, and other libraries similar to it, to understand and accomodate for the types according 
            to the relative distributions of checkouts they have.")
        )
      )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # About
  output$headData <- renderTable({head(data)})
  
  # Popular Plot
  output$popular <- renderPlot({
    if(input$display) {
      data %>% filter(MaterialType %in% input$type, CheckoutYear != 2023) %>%
        group_by(CheckoutYear, MaterialType) %>%
        summarize(totalCheckouts = sum(Checkouts)) %>%
        ggplot(aes(x=CheckoutYear, y=totalCheckouts)) +
        geom_point(aes(color=MaterialType)) +
        geom_line(aes(color=MaterialType)) +
        labs(x = "Year", y = "Total Checkouts", title = "Scatterplot of 5 most popular types of media")
    }
    else {
      data %>% filter(MaterialType %in% input$type, CheckoutYear != 2023) %>%
        group_by(CheckoutYear, MaterialType) %>%
        summarize(totalCheckouts = sum(Checkouts)) %>%
        ggplot(aes(x=CheckoutYear, y=totalCheckouts)) +
        geom_point(aes(color=MaterialType)) +
        labs(x = "Year", y = "Total Checkouts", title = "Scatterplot of 5 most popular types of media")
    }
  })
  
  # Usage Class Data
  usageclassdata <- reactive({
    data %>% 
      filter(UsageClass %in% input$usagetype, CheckoutYear != 2023) %>% 
      group_by(CheckoutYear, UsageClass) %>% 
      summarize(checkoutsum = sum(Checkouts))
  })
  
  output$usageclassplot <- renderPlot({
    if (input$usageTypeDisplay) {
      usageclassdata() %>% 
        ggplot(aes(CheckoutYear, checkoutsum, col = UsageClass)) + geom_point() + geom_line() +
        geom_smooth(method = lm, se = F) +
        labs(x = "Time", y = "Number of Checkouts", col = "Type")
    } else {
      usageclassdata() %>% 
        ggplot(aes(CheckoutYear, checkoutsum, col = UsageClass)) + geom_point() + geom_line() +
        labs(x = "Time", y = "Number of Checkouts", col = "Type")
    }
  })
  
  output$usageclass_summary <- renderText({
    usageclassrows <- usageclassdata() %>% 
      nrow()
    if (usageclassrows != 0)
      paste("Number of years observed:", usageclassrows)
  })
  
  output$usageclass_max <- renderText({
    max <- usageclassdata()$checkoutsum %>% 
      max()
    if(!is.infinite(max)) {
      paste("Maximum checkouts of", max, "at year", usageclassdata()$CheckoutYear[usageclassdata()$checkoutsum == max])
    } else {
      ""
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
