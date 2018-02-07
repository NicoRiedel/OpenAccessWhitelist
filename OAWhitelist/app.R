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
library(tidyr)

OA_Whitelist <- readRDS("data/Journal_Whitelist_Table_2018-01-30.rds")
names(OA_Whitelist) <- c("Journal title", "SCImago Journal Rank (SJR)", "SJR Subject Category Quartile",
                         "Journal article processing charges (APCs)", "Currency",
                         "APC in EUR (incl. 19% taxes)",
                         "APC below 2000 EUR", "APC information URL",
                         "Average time to publication (weeks)",
                         "Subject category", "Subject category 2",
                         "Journal license", "Journal URL",
                         "Publisher", "pISSN", "eISSN")

# ui <- fluidPage(
#   titlePanel("Open Access Journal Whitelist"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(inputId = "subjects",
#                   label = "Subject",
#                   choices = c("All", unique(OA_Whitelist$`Subject category`))),
#       checkboxGroupInput("show_vars", "Columns to show:",
#                          names(OA_Whitelist), selected = names(OA_Whitelist)[c(-8, -11, -13, -15, -16)])
#     ),
#     mainPanel(
#       DT::dataTableOutput("whitelist")
#     )
#   )
# )

ui <- fluidPage(
  fluidRow(
    column(width = 3,
           tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 183, width = 280),href="https://www.bihealth.org/de/forschung/quest-center/")
    ),
    column(width = 9,
           h1("Open Access Journal Whitelist", align = "center"),
           h4("Contains biomedical open access journals that are listed on the Directory of Open Access Journals (DOAJ) and Pubmed Central.", align = "center"),
           div("Version date: 30.01.2018. This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license, visit https://creativecommons.org/licenses/by-sa/3.0/.", align = "center"),
           div("Authors: Bernard, René (Concept); Liebenau, Lisa (Concept); Riedel, Nico (Concept, Technical implementation)", align = "center")
           
           #conditionalPanel(condition = "input.manual == true",
            #                div("This ‘Open Access Journal Whitelist’ comprises international biomedical open access journals that obey certain quality standards. All journals are listed by the Directory of Open Access Journals (DOAJ) and Pubmed Central (PMC). DOAJ ensures high quality standards for journals; individual journals have to apply at DOAJ and are checked against a list of quality criteria. PMC stores the full-text version of open access articles and increases the visibility of research in that way. Only journals that are assigned to the DOAJ subject categories ‘Medicine’ or ‘Biology’ and that have English or German as full-text language are included."))
    )
  ),
  sidebarLayout(position = "left", fluid = TRUE,
                sidebarPanel(
                  selectInput(inputId = "subjects",
                              label = "Subject category",
                              choices = c("All", unique(OA_Whitelist$`Subject category`))),
                  checkboxGroupInput("show_vars", 
                                     "Columns to show:", names(OA_Whitelist), 
                                     selected = names(OA_Whitelist)[c(-8, -11, -13, -15, -16)]),
                  helpText('This ‘Open Access Journal Whitelist’ comprises international biomedical open access journals that 
                            obey certain quality standards. All journals are listed by the Directory of Open Access Journals (DOAJ) and 
                            Pubmed Central (PMC). DOAJ ensures high quality standards for journals; individual journals have to apply at DOAJ 
                            and are checked against a list of quality criteria. PMC stores the full-text version of open access articles and 
                            increases the visibility of research in that way. 
                            Only journals that are assigned to the DOAJ subject categories ‘Medicine’ or ‘Biology’ and that have English or German as full-text language are included.'),
                  helpText('The freely available SCImago Journal Rank (SJR) measures the journal impact, similar to the Journal Impact Factor.
                            The SJR quartiles indicate how the SJR of a journal compares to other journals for the same subject category.
                            Journals in the first quartile (Q1) are among those with the highest SJR in their respective subject category. 
                            The Qartiles are given with respect to the corresponding Scopus subject categories, while the subject 
                            categories used for this table are taken from DOAJ.'),
                  helpText('The 2000€ APC threshold is the relevant threshold for many funders (e.g. the DFG), 
                           as they do not fund articles above this sum. 
                           Due to either outdated APC information, variations in the currency exchange rates or 
                           due to differences in taxing for journals from different countries the information if an APC lies below 2000 EUR might be inaccurate. 
                           Before deciding on a journal, please check the journal costs yourself (using the APC info link provided in the next column).'),
                  width = 2
                ),
                mainPanel(DT::dataTableOutput("whitelist")))
)

#Contains biomedical open access journals that are listed on the Directory of Open Access Journals (DOAJ) and Pubmed Central
#Version date: 16.01.2018. This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license, visit https://creativecommons.org/licenses/by-sa/3.0/.  
#Authors: Bernard, René (Concept); Liebenau, Lisa (Concept); Riedel, Nico (Concept, Technical implementation)

server <- function(input, output) {
  # choose columns to display
  output$whitelist <- DT::renderDataTable({
    DT::datatable(if(input$subjects == "All") {
                    OA_Whitelist[, input$show_vars, drop = FALSE]
                  } else {
                    OA_Whitelist[OA_Whitelist[["Subject category"]] == input$subjects,][, input$show_vars, drop = FALSE]
                  }, 
                  filter = 'top',
                  options = list(orderClasses = TRUE, 
                                 pageLength = 20,
                                 lengthMenu = c(10, 20, 50, 100, 1000),
                                 columnDefs = list(list(className = 'dt-left', targets = 2),
                                                   list(className = 'dt-left', targets = 6),
                                                   list(className = 'dt-left', targets = 8))))
  })
  
  # #load Quest logo
  # output$QuestLogo <- renderImage({
  # 
  #   # When input$n is 3, filename is ./images/image3.jpeg
  #   filename <- normalizePath(file.path('./www',
  #                                       "Quest_Wortmarke_rgb.png"))
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = filename,
  #        alt = paste("Image number", input$n),
  #        width = 280,
  #        height = 183)
  #   
  # }, deleteFile = FALSE)

  # #count number of visits
  # output$counter <- renderText({
  #   if(!file.exists("counter.Rdata")) {
  #     counter <- 0
  #   } else {
  #     load(file = "counter.Rdata")
  #   }
  #   counter <- counter + 1
  #   save(counter, file="counter.Rdata")
  #   paste0("Number of visits: ", counter)
  # })
  
  cat(paste0("App visit at: ", Sys.time(), "\n"))

}

shinyApp(ui, server)