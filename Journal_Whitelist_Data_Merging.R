source('T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\R-Code\\Journal_Whitelist_functions.R')
library(tidyverse)
library(XML)

romeo_api_key <- "4EDDuZOh6GE"

#----------------------------------------------------------------------------------------------------------------------------
# DOAJ dataset
#----------------------------------------------------------------------------------------------------------------------------

doaj_data <- read_csv('T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\DOAJ_journal_list.csv')

useful_cols_doaj <- c('Journal title', 'Journal URL', 'Journal ISSN (print version)', 
                 'Journal EISSN (online version)', 'Publisher', 
                 'Journal article processing charges (APCs)', 'APC information URL', 
                 'APC amount', 'Currency', "Journal article submission fee",
                 "Submission fee URL", "Submission fee amount", "Submission fee currency",
                 "Full text language", "Average number of weeks between submission and publication",
                 "Journal license", "Subjects")


doaj_data <- doaj_data %>% select(one_of(useful_cols_doaj))
doaj_data <- doaj_data %>% 
  filter(`Full text language` == "English" | `Full text language` == "German") %>% #English only journals
  filter(grepl("Medicine", `Subjects`)) %>% #Which categories to use?
  filter(is.na(`Submission fee amount`)) %>% #Take only journals without submission fee
  filter(`Currency` == "EUR - Euro" | `Currency` == "GBP - Pound Sterling" | `Currency` == "USD - US Dollar" | is.na(`Currency`))

#rename some columns
doaj_data <- doaj_data %>% 
  rename(pISSN = `Journal ISSN (print version)`) %>%
  rename(eISSN = `Journal EISSN (online version)`)

#drop some columns
doaj_data <- doaj_data %>% 
  select(-`Journal article submission fee`, -`Submission fee URL`, -`Submission fee amount`, -`Submission fee currency`)



#----------------------------------------------------------------------------------------------------------------------------
# PMC dataset
#----------------------------------------------------------------------------------------------------------------------------

pmc_data <- read_csv('T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\PMC_journal_list.csv')

useful_cols_pmc <- c("Journal title", "pISSN", "eISSN", "Publisher",
                      "Free access", "Open access")

pmc_data <- pmc_data %>% select(one_of(useful_cols_pmc))


#----------------------------------------------------------------------------------------------------------------------------
# journal impact factor dataset - not clear if we will need it
#----------------------------------------------------------------------------------------------------------------------------

jif_data <- read_csv('T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\Journal_Impact_Factor.csv')

useful_cols_jif <- c("Full Journal Title", "Journal Impact Factor")

jif_data <- jif_data %>% select(one_of(useful_cols_jif))
jif_data <- jif_data %>%
  mutate(`Full Journal Title` = tolower(`Full Journal Title`))


#----------------------------------------------------------------------------------------------------------------------------
# join datasets
#----------------------------------------------------------------------------------------------------------------------------

#first join on print ISSN
joined_data_pISSN <- doaj_data %>% 
  filter(!is.na(pISSN)) %>% 
  left_join(select(pmc_data, -eISSN), by = "pISSN", suffix = c(".doaj", ".pmc")) #remove other key in pmc_data before joining to avoid double second key cols

#second join on eISSN
joined_data_eISSN <- doaj_data %>% 
  filter(!is.na(eISSN)) %>% 
  left_join(select(pmc_data, -pISSN), by = "eISSN", suffix = c(".doaj", ".pmc"))

#combine both join versions and remove duplicates
joined_data <- joined_data_pISSN %>%
  union(joined_data_eISSN)

#add column is_PMC_listed by computing which rows have journal title.pmc entry
joined_data <- joined_data %>%
  mutate(is_PMC_listed = !is.na(`Journal title.pmc`)) %>%
  mutate(is_PMC_listed = logical_to_yes_no(is_PMC_listed)) 
  
#only select PMC-listed journals?
joined_data <- joined_data %>%
  filter(is_PMC_listed == "yes")

#add journal impact factor column
joined_data <- joined_data %>%
  mutate(`Journal title match` = tolower(`Journal title.doaj`)) %>%
  left_join(jif_data, by = c("Journal title match" = "Full Journal Title")) %>%
  select(-`Journal title match`)
  

#----------------------------------------------------------------------------------------------------------------------------
# retrieve sherpa/romeo info and add to data table
#----------------------------------------------------------------------------------------------------------------------------

romeo_color_list <- rep("", dim(joined_data)[1])
is_hybrid <- rep("", dim(joined_data)[1])
for(i in 1:dim(joined_data)[1])
{
  #get sherpa entry for given issn
  issn <- getISSN(joined_data[["pISSN"]][i], joined_data[["eISSN"]][i])
  sherpa_entry <- readLines(paste0("http://www.sherpa.ac.uk/romeo/api29.php?issn=", issn, "&ak=", romeo_api_key))
  sherpa_entry <- xmlParse(sherpa_entry)
  
  #retrieve romeo color
  romeo_colour <- xpathSApply(sherpa_entry, "//romeocolour", fun = xmlValue)
  if(length(romeo_colour) == 0) { #handle cases with no or several entries
    romeo_colour <- ""
  } else {
    romeo_colour <- get_lowest_color(romeo_colour)
  }
  
  #retrieve hybrid info
  hybrid <- xpathSApply(sherpa_entry, "//paidaccessnotes", fun = xmlValue)
  hybrid <- hybrid == "A paid open access option is available for this journal."
  if(length(hybrid) == 0) {
    hybrid <- FALSE
  }
  hybrid <- all(hybrid) #sometimes the paidaccessnotes tag is given more than once
  
  #fill list
  print(paste0("i: ", i, " romeo_colour: ", romeo_colour, " is_hybrid: ", hybrid))
  romeo_color_list[i] <- romeo_colour[1]
  is_hybrid[i] <- hybrid
}
is_hybrid <- logical_to_yes_no(as.logical(is_hybrid))

#add romeo color and is_hybrid as new column
joined_data <- joined_data %>%
  add_column(romeo_color_list) %>%
  add_column(is_hybrid)


#----------------------------------------------------------------------------------------------------------------------------
# save current version of spreadsheet
#----------------------------------------------------------------------------------------------------------------------------

#rearrange columns
final_col <- c('Journal title.doaj', "is_PMC_listed", "Journal Impact Factor",
               "romeo_color_list", "Free access", "Open access", "is_hybrid", "Journal license",
               "Subjects", 'Journal article processing charges (APCs)', 'APC information URL', 
               'APC amount', 'Currency', "Average number of weeks between submission and publication",
               'Journal URL', 'Publisher.doaj', "pISSN", "eISSN")
joined_data <- joined_data %>% 
  select(one_of(final_col)) %>%
  rename(`Journal title` = `Journal title.doaj`) %>% 
  arrange(`Journal title`)

save_filename <- 'T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\Journal_Whitelist_Table.csv'
#write_csv(joined_data, save_filename)
