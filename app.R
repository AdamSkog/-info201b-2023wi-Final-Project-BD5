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
#Extracting years and getting rid of any extra years
data$PublicationYear <- trimws(data$PublicationYear, which = c("left"))  #gets rid of leading spaces
data <- data %>% 
  extract(PublicationYear, into = "secYear", regex = "\\d{4}.*(\\d{4})", remove = FALSE, convert = TRUE) %>%
  extract(PublicationYear,into = "UpdatedYear", regex ="(\\d{4})", remove = FALSE, convert = TRUE)
#filtering out out of place numbers from UpdatedYear
data <- data %>% 
  mutate(UpdatedYear = replace(UpdatedYear, !UpdatedYear < 2023 | !UpdatedYear >= 1863, ""), 
         secYear = replace(secYear, !secYear < 2023, ""))

#Replaces an empty spaces in firstYear with secYear data
data <- data %>%
  mutate(UpdatedYear = ifelse(UpdatedYear == "",secYear, UpdatedYear ))

#Changing UpdatedYear to be numeric instead of a character
data$UpdatedYear <- as.numeric(as.character(data$UpdatedYear))

#Gets rid of PublicationYear and secYear to leave UpdatedYear by itself
data <-  data[,-12]
data <- data[,-13]



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Seattle Public Library Checkouts"),
  tabsetPanel(
    tabPanel("About", p("This dataset includes a", em("monthly"),"count of Seattle Public Library checkouts by title for physical and electronic items."),
             p("The dataset begins with checkouts that occurred in April 2005."),
             p("We have", strong(nrow(data)), "rows of data and", strong(ncol(data)), "columns."),
             p("Here are the first few rows of the dataset."),
             mainPanel(tableOutput("headdata"))),
    
    tabPanel(
      "PopularPlots",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("type",
                             "Which material type do you want?",
                             choices = list("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK"),
                             selected = list("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK")
          ),
          checkboxInput("display", "Display line", TRUE)
        ),
        mainPanel(plotOutput("popular"))
      )
    ),
    tabPanel(
      "Material Publication Data",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("material", "Select a form of media", choices = c("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK")),
          checkboxInput("publicationDisplay", "Display Graph", TRUE),
          
          p("The Seattle Public Library has a many forms of media and the amount that was released changed over time.")
        ), 
        mainPanel(plotOutput("publication"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$headdata <- renderTable({head(data)})
  
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
  
  output$publication <- renderPlot({
    if(input$publicationDisplay){
      data %>% 
        
        group_by(UpdatedYear, MaterialType) %>% 
        filter(MaterialType %in% input$material,!is.na(UpdatedYear)) %>%
        summarise(media_count = length(Title)) %>% 
        ggplot(aes(UpdatedYear, media_count, fill = MaterialType))+
        geom_col()+
        scale_x_continuous(breaks = seq(0, 2022, 4))+
        theme(axis.text = element_text(size = 10, hjust = 1, angle = 45), legend.key.size = unit(0.3, "line"))+
        labs(title = "Amount of Media Released Yearly",x = "Year",y = "Amount of Media Released",fill = "Material Type")
        
    }
    else{
      data %>% 
      group_by(UpdatedYear, MaterialType) %>% 
      filter(MaterialType %in% input$material,!is.na(UpdatedYear)) %>%
      summarise(media_count = length(Title)) %>% 
      ggplot(aes(UpdatedYear, media_count, fill = MaterialType))+
      labs(title = "Amount of Media Released Yearly",x = "Year",y = "Amount of Media Released",fill = "Material Type")
      
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
