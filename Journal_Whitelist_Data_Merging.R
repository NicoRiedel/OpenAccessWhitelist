source('T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\R-Code\\Journal_Whitelist_functions.R')
library(tidyverse)
library(XML)

#currency excange rates
dollar_to_euro <- 0.8392
pound_to_euro <- 1.1369

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

#filter rows
doaj_data <- doaj_data %>% select(one_of(useful_cols_doaj))
doaj_data <- doaj_data %>% 
  filter(`Full text language` == "English" | `Full text language` == "German") %>% #English only journals
  filter(grepl("Medicine", `Subjects`)) %>% #Which categories to use?
  filter(is.na(`Submission fee amount`)) %>% #Take only journals without submission fee
  filter(`Currency` == "EUR - Euro" | `Currency` == "GBP - Pound Sterling" | `Currency` == "USD - US Dollar" | is.na(`Currency`))


#modify and merge APC columns to condese information in one column
doaj_data <- doaj_data %>% 
  mutate(Currency = substr(Currency, 1, 3)) %>% 
  mutate(APC = `APC amount`)

#value replacement for subset of rows is not properly implemented in dplyr as simple solution, so use usual R way
noAPC_idx <- which(doaj_data["Journal article processing charges (APCs)"] != "Yes")
doaj_data[noAPC_idx, "APC"] <- doaj_data[noAPC_idx, "Journal article processing charges (APCs)"]

#create column with APCs converted to EUR
doaj_data <- doaj_data %>% add_column(`APC in EUR` = NA)
doaj_data[which(doaj_data$Currency =="USD"), "APC in EUR"] <-
  round(doaj_data[which(doaj_data$Currency =="USD"), "APC amount"] * dollar_to_euro)
doaj_data[which(doaj_data$Currency =="GBP"), "APC in EUR"] <-
  round(doaj_data[which(doaj_data$Currency =="GBP"), "APC amount"] * pound_to_euro)
doaj_data[which(doaj_data$Currency =="EUR"), "APC in EUR"] <-
  doaj_data[which(doaj_data$Currency =="EUR"), "APC amount"] 
doaj_data <- doaj_data %>% mutate(`APC below 2000 EUR` = logical_to_yes_no(`APC in EUR` < 2000))


#rename some columns
doaj_data <- doaj_data %>% 
  rename(pISSN = `Journal ISSN (print version)`) %>%
  rename(eISSN = `Journal EISSN (online version)`) %>%
  rename(`APC drop` = `Journal article processing charges (APCs)`) %>%
  rename(`Journal article processing charges (APCs)` = APC)

#drop some columns
doaj_data <- doaj_data %>% 
  select(-`Journal article submission fee`, -`Submission fee URL`, 
         -`Submission fee amount`, -`Submission fee currency`, 
         -`APC drop`, -`APC amount`)

#----------------------------------------------------------------------------------------------------------------------------
# PMC dataset
#----------------------------------------------------------------------------------------------------------------------------

pmc_data <- read_csv('T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\PMC_journal_list.csv')

useful_cols_pmc <- c("Journal title", "pISSN", "eISSN", "Publisher")

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
# save current version of spreadsheet
#----------------------------------------------------------------------------------------------------------------------------

#rearrange columns
final_col <- c('Journal title.doaj',  
               'Journal article processing charges (APCs)', 'Currency',
               'APC in EUR', 'APC below 2000 EUR',
               'APC information URL', "Journal license", 
               "Average number of weeks between submission and publication",
               "Subjects", 'Journal URL', 'Publisher.doaj', "pISSN", "eISSN")
joined_data <- joined_data %>% 
  select(one_of(final_col)) %>%
  rename(`Journal title` = `Journal title.doaj`) %>% 
  rename(Publisher = `Publisher.doaj`) %>% 
  arrange(Subjects)

save_filename <- 'T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\Journal_Whitelist_Table.csv'
write_csv(joined_data, save_filename)
