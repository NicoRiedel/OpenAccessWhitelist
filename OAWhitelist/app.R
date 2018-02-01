#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)

OA_Whitelist <- readRDS("data/Journal_Whitelist_Table_2018-01-30.rds")
names(OA_Whitelist) <- c("Journal title", "SJR Impact", "SJR Subject Category Best Quartile",
                         "Journal article processing charges (APCs)", "Currency",
                         "APC in EUR (including 19% taxes)",
                         "APC below 2000 EUR", "APC information URL",
                         "Average time to publication (weeks)",
                         "Subject category", "Subject category 2",
                         "Journal license", "Journal URL",
                         "Publisher", "pISSN", "eISSN")

ui <- fluidPage(
  titlePanel("Open Access Journal Whitelist"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "subjects",
                  label = "Subject",
                  choices = unique(OA_Whitelist$`Subject category`)),
      checkboxGroupInput("show_vars", "Columns to show:",
                         names(OA_Whitelist), selected = names(OA_Whitelist)[c(-8, -11, -13, -15, -16)])
    ),
    mainPanel(
      DT::dataTableOutput("whitelist")
    )
  )
)


server <- function(input, output) {
  # choose columns to display
  output$whitelist <- DT::renderDataTable({
    DT::datatable(OA_Whitelist[OA_Whitelist[["Subject category"]] == input$subjects,][, input$show_vars, drop = FALSE], 
                  filter = 'top',
                  options = list(orderClasses = TRUE, pageLength = 20))
  })
}

shinyApp(ui, server)