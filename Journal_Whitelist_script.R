#This script creates an ‘Open Access Journal Whitelist’ by aggregating 
#information on Open Access Journals from the
#Directory of Open Access Journals (DOAJ) and Pubmed Central (PMC). 
#These two data sources ensure that the Journals obey certain quality standards.
#DOAJ ensures high quality standards for journals; individual journals have to apply at DOAJ and 
#are checked against a list of quality criteria. PMC stores the full-text version of 
#open access articles and increases the visibility of research in that way. 
#The current list focuses on biomedical journals but the included subjects 
#can be adjusted in the 'adjustable parameters' section
#Only journals that are assigned to the DOAJ subject categories ‘Medicine’ or ‘Biology’ and 
#that have English or German as full-text language are included.

#----------------------------------------------------------------------------------------------------------------------------
# adjustable parameters
#----------------------------------------------------------------------------------------------------------------------------

#those are parameters for filtering the entries for certain parameters
full_text_languages <- c("English", "German", "English, German")
subjects_included <- c("Medicine", "Biology", "Biotechnology", "Physiology")
#there might still be some more specialised subject categories we want to exclude
subjects_excluded <- c("Agriculture", "Plant culture")
APC_currencies_included <- c("EUR - Euro", "GBP - Pound Sterling", 
                             "USD - US Dollar", "CHF - Swiss Franc")


#----------------------------------------------------------------------------------------------------------------------------
# load packages and datasets
#----------------------------------------------------------------------------------------------------------------------------

source('Journal_Whitelist_functions.R')
require(tidyverse)
require(jsonlite)
require(gdata)

#necessary datasets (only Scopus data needs to be provided, the others are automatically downloaded to that file)
if(!dir.exists('Datasets/')) {
  dir.create('Datasets/')
}
doaj_filename <- 'Datasets/DOAJ_journal_list_upd.csv'
pmc_filename <- 'Datasets/PMC_journal_list_upd.csv'
sjr_filename <- 'Datasets/Scopus_SJR_2016.csv'

#update datasets
download.file('https://doaj.org/csv', doaj_filename)
download.file('https://www.ncbi.nlm.nih.gov/pmc/journals/?format=csv', pmc_filename)
download.file('http://www.scimagojr.com/journalrank.php?out=xls', sjr_filename, mode = "wb")

exchange_currencies <- str_sub(APC_currencies_included, 1, 3)
exchange_currencies <- exchange_currencies[exchange_currencies != "EUR"]
  
exchange_rates <- c(1.0, 
                    map_dbl(exchange_currencies, get_exchange_rate, currency_to = "EUR"),
                    NA)
names(exchange_rates) <- c("EUR", exchange_currencies, "-")


#----------------------------------------------------------------------------------------------------------------------------
# DOAJ dataset
#----------------------------------------------------------------------------------------------------------------------------

doaj_data <- read_csv(doaj_filename)

useful_cols_doaj <- c('Journal title', 'Journal URL', 'Journal ISSN (print version)', 
                      'Journal EISSN (online version)', 'Publisher', 
                      'Journal article processing charges (APCs)', 'APC information URL', 
                      'APC amount', 'Currency', "Journal article submission fee",
                      "Submission fee URL", "Submission fee amount", "Submission fee currency",
                      "Full text language", "Average number of weeks between submission and publication",
                      "Journal license", "Subjects")

#manually fix subject category for one journal where the information is missing in DOAJ
doaj_data$Subjects[doaj_data$`Journal title` == "Frontiers in Human Neuroscience"] <- "Medicine"

#filter rows
doaj_data <- doaj_data %>% select(one_of(useful_cols_doaj))
doaj_data <- doaj_data %>% 
  filter(`Full text language` %in% full_text_languages) %>% #English only journals
  filter(grepl(paste(subjects_included, collapse = "|"), `Subjects`)) %>% 
  filter(!grepl(paste(subjects_excluded, collapse = "|"), `Subjects`)) %>% #Which categories to use?
  filter(is.na(`Submission fee amount`)) %>% #Take only journals without submission fee
  filter(`Currency` %in% APC_currencies_included | is.na(`Currency`))


#filter rows that do not easily work with the dplyr filter
#remove journals with a regional focus (using non-comprehensive list of regional terms,
# - might exclude journals with an actual international focus!)
doaj_data <- doaj_data[!sapply(doaj_data$`Journal title`, is_regional_journal),]


#modify and merge APC columns to condese information in one column
doaj_data <- doaj_data %>% 
  mutate(Currency = substr(Currency, 1, 3)) %>% 
  mutate(APC = `APC amount`)
doaj_data[is.na(doaj_data$Currency), "Currency"] <- "-"


#value replacement for subset of rows is not properly implemented in dplyr as simple solution, so use usual R way
noAPC_idx <- which(doaj_data["Journal article processing charges (APCs)"] != "Yes")
doaj_data[noAPC_idx, "APC"] <- doaj_data[noAPC_idx, "Journal article processing charges (APCs)"]


#create column with APCs converted to EUR
doaj_data <- doaj_data %>% 
  mutate(`APC in EUR (including 19% taxes)` = round( (1/exchange_rates[Currency]) * `APC amount` * 1.19)) 

#for those journals without APCs set coverted amount to 0 Eur
no_APC_journal <- doaj_data$`Journal article processing charges (APCs)` == "No"
doaj_data$`APC in EUR (including 19% taxes)`[no_APC_journal] <- 0
doaj_data <- doaj_data %>% 
  mutate(`APC below 2000 EUR` = logical_to_yes_no(`APC in EUR (including 19% taxes)` < 2000))

#add information on special terms of the Charite library for specific journals
special_conditions_publishers <- c("Frontiers Media S.A.", "BMJ Publishing Group",
                                 "Cambridge University Press", "Karger Publishers",
                                 "MDPI AG", "JMIR")
special_conditions_journals <- c("PLoS ONE", "SAGE Open", "SAGE Open Medicine", 
                                 "SAGE Open Medical Case Reports")
has_special_conditions <- doaj_data$Publisher %in% special_conditions_publishers |
                          doaj_data$`Journal title` %in% special_conditions_journals
doaj_data[has_special_conditions,][["APC below 2000 EUR"]] <- "yes, library special terms"

#for some journals there is a reduction in the journal fee but they still don't fall under the 2000€ threshold
only_discount_journals <- c("BMJ Open Diabetes Research & Care")
is_discount_only <- doaj_data$`Journal title` %in% only_discount_journals
doaj_data[is_discount_only,][["APC below 2000 EUR"]] <- "no, but library discount applies"

#simplify subject categories
subjects_simplified <- lapply(doaj_data$Subjects, subject_simplification)
doaj_data <- doaj_data %>% 
  add_column(`Subject category 1` = sapply(subjects_simplified, "[[", 1)) %>%
  add_column(`Subject category 2` = sapply(subjects_simplified, "[[", 2))


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
         -`APC drop`, -`APC amount`, -Subjects)


#----------------------------------------------------------------------------------------------------------------------------
# PMC dataset
#----------------------------------------------------------------------------------------------------------------------------

pmc_data <- read_csv(pmc_filename)

useful_cols_pmc <- c("Journal title", "pISSN", "eISSN", "Publisher")
pmc_data <- pmc_data %>% select(one_of(useful_cols_pmc))


#----------------------------------------------------------------------------------------------------------------------------
# Scopus dataset
#----------------------------------------------------------------------------------------------------------------------------

scopus_data <-  read_delim(sjr_filename, delim = ";", locale = locale(decimal_mark = ","))#read.xls(sjr_filename) %>% as_tibble()

useful_cols_scopus <- c("Title", "Issn", "SJR", "SJR Best Quartile")
scopus_data <- scopus_data %>% 
  select(one_of(useful_cols_scopus)) %>%
  separate(Issn, into = c("eISSN", "pISSN")) %>%
  rename(`SJR Impact` = SJR) %>%
  mutate(`SJR Impact` = as.double(as.character(`SJR Impact`))) %>%
  mutate(`SJR Best Quartile` = as.character(`SJR Best Quartile`))
scopus_data$eISSN[scopus_data$eISSN == ""] <- NA

#introduce '-' character in the middle of eISSN/pISSN to make it consistent with other data sources
scopus_data$pISSN <- paste(str_sub(scopus_data$pISSN, 1, 4), str_sub(scopus_data$pISSN, 5, 8), sep = "-")
scopus_data$eISSN <- paste(substring(scopus_data$eISSN, 1, 4), substring(scopus_data$eISSN, 5, 8), sep = "-")

#unfortunately paste() doesn't know how to deal with NA's
scopus_data$pISSN[scopus_data$pISSN == "NA-NA"] <- NA
scopus_data$eISSN[scopus_data$eISSN == "NA-NA"] <- NA

#filter all entries that have neither an eISSN nor an pISSN
scopus_data <- scopus_data %>%
  filter(!(is.na(eISSN) & is.na(pISSN)))


#----------------------------------------------------------------------------------------------------------------------------
# join datasets
#----------------------------------------------------------------------------------------------------------------------------

#DOAJ and PMC join

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


#scopus join

#as the scopus eISSN and pISSN might be interchanged
#both have to be checked against both DOAJ pISSN/eISSN
#use self-defined function instead of regular join
SJR_vec <- map2_dbl(joined_data$eISSN, joined_data$pISSN, get_scopus_var, 
                scopus_data = scopus_data, varname = "SJR Impact")

#add the SJR quartile column
SJR_quartile_vec <- map2_chr(joined_data$eISSN, joined_data$pISSN, get_scopus_var, 
                             scopus_data = scopus_data, varname = "SJR Best Quartile")

joined_data <- joined_data %>%
  add_column(`SJR Impact` = SJR_vec) %>%
  add_column(`SJR Subject Category Best Quartile` = SJR_quartile_vec)


#----------------------------------------------------------------------------------------------------------------------------
# save current version of spreadsheet
#----------------------------------------------------------------------------------------------------------------------------

#rearrange columns
final_col <- c('Journal title.doaj', 
               'SJR Impact', 'SJR Subject Category Best Quartile',
               'Journal article processing charges (APCs)', 'Currency',
               'APC in EUR (including 19% taxes)', 'APC below 2000 EUR', 'APC information URL',
               "Average number of weeks between submission and publication",
               'Subject category 1', 'Subject category 2', 'Journal license', 
               'Journal URL', 'Publisher.doaj', "pISSN", "eISSN")
joined_data <- joined_data %>% 
  select(one_of(final_col)) %>%
  rename(`Journal title` = `Journal title.doaj`) %>% 
  rename(Publisher = `Publisher.doaj`) %>% 
  arrange(`Subject category 1`, desc(`SJR Impact`))

#filter duplicated journals (again, since some duplicates reappeared for some unknown reason)
joined_data <- joined_data[!duplicated(joined_data$eISSN),]

#convert NAs to "-" for final list
joined_data$`SJR Impact`[is.na(joined_data$`SJR Impact`)] <- "-"
joined_data$`SJR Subject Category Best Quartile`[is.na(joined_data$`SJR Subject Category Best Quartile`)] <- "-"

#save resulting table in two formats (.csv for e.g. loading into Excel) or .RDS for use in the R-Shiny app
if(!dir.exists('Table/')) {
  dir.create('Table/')
}
save_filename <- paste0('Table/Journal_Whitelist_Table_', Sys.Date(), '.csv')
write_csv(joined_data, save_filename)

save_filename_RDS <- paste0('Table/Journal_Whitelist_Table_', Sys.Date(), '.rds')
saveRDS(joined_data, save_filename_RDS)
