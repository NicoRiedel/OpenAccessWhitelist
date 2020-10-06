#This shiny app displays the Open Access Journal Whitelist in a 
#searchable and filterable format in the browser using the datatable package
#

library(shiny)
library(DT)
library(tidyr)

#read in .rds output from Journal_Whitelist_script.R here
OA_Whitelist <- readRDS("data/Journal_Whitelist_Table_2018-12-11.rds")

names(OA_Whitelist) <- c("Journal title", "SCImago Journal Rank (SJR)", "SJR Subject Category Quartile",
                         "Journal article processing charges (APCs)", "Currency",
                         "APC in EUR (incl. 19% taxes)",
                         "APC below 2000 EUR", "APC information URL",
                         "Average time to publication (weeks)",
                         "Subject category", "Subject category 2",
                         "Journal license", "Journal URL",
                         "Publisher", "pISSN", "eISSN")

url <- "https://twitter.com/intent/tweet?text=Hello%20world&url=https://shiny.rstudio.com/gallery/widget-gallery.html/"


ui <- fluidPage(
  fluidRow(
    column(width = 3,
           tags$a(img(src = "Quest_Wortmarke_rgb.png", height = 183, width = 280),href="https://www.bihealth.org/de/forschung/quest-center/")
    ),
    column(width = 9,
           h1("Open Access Journal Whitelist", align = "center"),
           h4("Contains biomedical open access journals that are listed on the Directory of Open Access Journals (DOAJ) and Pubmed Central.", align = "center"),
           h4(HTML(paste0('Please note that this Open Access Journal Whitelist is only a first information source for finding 
              a suitable open access journal and is by no means an exhaustive list of all valid open access journals. 
              For a more complete list please consider the ',a(href = 'https://doaj.org/', 'Directory of Open Access Journals (DOAJ)'),'. 
              We do not check the quality of the listed journals ourselves, but incorporate journals listed in the DOAJ, 
              which undertakes a quality assessment of the journals.')), align = "center", style="color:#c12075"),
           div("This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. To view a copy of this license, visit https://creativecommons.org/licenses/by-sa/3.0/.", align = "center"),
           div("Authors: Bernard, René (Concept); Liebenau, Lisa (Concept); Riedel, Nico (Concept, Technical implementation)", align = "center"),
           div("  ")
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
                  helpText('The 2000€ APC threshold is a relevant threshold for some funders (e.g. the DFG), 
                           as they do not fund articles above this sum. 
                           Due to either outdated APC information, variations in the currency exchange rates or 
                           due to differences in taxing for journals from different countries the information if an APC lies below 2000 EUR might be inaccurate. 
                           Before deciding on a journal, please check the journal costs yourself (using the APC info link provided in the next column). 
                           Some of the publishers have agreed on special terms with the Charité library 
                           such that the journals of these publishers have discounts on their fees. In many cases the APCs stay below 
                           the 2000€ threshold or have further discounts even if they were already below the 2000 EUR threshold 
                           (denoted as \'yes, library special terms\'). In some cases a dicount applies but the costs still stay above
                           2000€ (denoted as \'no, but library discount applies\'). For further information see 
                           https://bibliothek.charite.de/publizieren/open_access/verlagsvereinbarungen/'),
                  width = 3
                ),
                mainPanel(DT::dataTableOutput("whitelist")))
)

server <- function(input, output) {
  # choose columns to display
  output$whitelist <- DT::renderDataTable({
    DT::datatable( data = if(input$subjects == "All") {
                    OA_Whitelist[, input$show_vars, drop = FALSE]
                  } else {
                    OA_Whitelist[OA_Whitelist[["Subject category"]] == input$subjects,][, input$show_vars, drop = FALSE]
                  },
                  extensions = 'Buttons',
                  filter = 'top',
                  options = list(dom = 'Blfrtip',
                                 buttons = 
                                   list("copy", list(
                                     extend = "collection"
                                     , buttons = c("csv", "excel")
                                     , text = "Download"
                                   ) ),
                                 orderClasses = TRUE, 
                                 pageLength = 20,
                                 lengthMenu = list(c(10, 20, 50, 100, -1),
                                                  c(10, 20, 50, 100, "All")),
                                 columnDefs = list(list(className = 'dt-left', targets = 2),
                                                   list(className = 'dt-left', targets = 6),
                                                   list(className = 'dt-left', targets = 8))
                                 ))
  })
  
  write(paste0("App visit at: ", Sys.time()), "/var/log/shiny-server/visitors.txt", append = TRUE)
}

shinyApp(ui, server)