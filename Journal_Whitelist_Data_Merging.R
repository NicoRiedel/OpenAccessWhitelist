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
  mutate(APC = paste(`APC amount`, `Currency`))

#value replacement for subset of rows is not properly implemented in dplyr as simple solution, so use usual R way
noAPC_idx <- which(doaj_data["Journal article processing charges (APCs)"] != "Yes")
doaj_data[noAPC_idx, "APC"] <- doaj_data[noAPC_idx, "Journal article processing charges (APCs)"]


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
         -`APC drop`, -`APC amount`, -Currency)

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
# retrieve sherpa/romeo info and add to data table
#----------------------------------------------------------------------------------------------------------------------------

pre_archiving <- rep("", dim(joined_data)[1])
post_archiving <- rep("", dim(joined_data)[1])
post_restriction <- rep("", dim(joined_data)[1])
publ_archiving <- rep("", dim(joined_data)[1])
for(i in 1:dim(joined_data)[1])
{
  #get sherpa entry for given issn
  issn <- getISSN(joined_data[["pISSN"]][i], joined_data[["eISSN"]][i])
  sherpa_entry <- readLines(paste0("http://www.sherpa.ac.uk/romeo/api29.php?issn=", issn, "&ak=", romeo_api_key))
  sherpa_entry <- xmlParse(sherpa_entry)

  #retrieve archiving rights - only select first publisher entry, as there are sometimes conflicting results 
  #for different publishers and usually the first publisher is the current or most important one
  pre_arch <- xpathSApply(sherpa_entry, "//publisher[1]//prearchiving", fun = xmlValue)
  if(length(pre_arch) == 0) { pre_arch <- "" }#handle cases with no or several entries
  
  post_arch <- xpathSApply(sherpa_entry, "//publisher[1]//postarchiving", fun = xmlValue)
  if(length(post_arch) == 0) { post_arch <- "" }
  
  publ_arch <- xpathSApply(sherpa_entry, "//publisher[1]//pdfarchiving", fun = xmlValue)
  if(length(publ_arch) == 0) { publ_arch <- "" }
  
  #postprint restriction is different, need to extract timeframe from xml
  post_restr <- xpathSApply(sherpa_entry, "//publisher[1]//postprints//postrestriction", fun = xmlValue)
  if(length(post_restr) == 0) { 
    post_restr <- "" 
  } else {
    if(length(post_restr) == 1) {
      post_restr <- extract_post_restr(post_restr)
    } else {
      post_restr <- paste((sapply(post_restr, extract_post_restr)), collapse = "; ") 
    }
  }

  #fill list
  print(paste0("i: ", i, " pre_arch: ", pre_arch, " post_arch: ", post_arch, " publ_arch: ", publ_arch, " post_restr: ", post_restr))
  pre_archiving[i] <- pre_arch
  post_archiving[i] <- post_arch
  publ_archiving[i] <- publ_arch
  post_restriction[i] <- post_restr
  
}

#paste post restriction entry to post_archiving for the few cases where it is not empty
hasRestr_idx <- which(post_restriction != "")
post_archiving[hasRestr_idx] <- paste(post_archiving[hasRestr_idx], post_restriction[hasRestr_idx], sep = ": ")


#add romeo color and is_hybrid as new column
joined_data <- joined_data %>%
  add_column(pre_archiving) %>%
  add_column(post_archiving) %>%
  add_column(publ_archiving)

#rename new columns
joined_data <- joined_data %>% 
  rename(`Can archive pre-print` = pre_archiving) %>%
  rename(`Can archive post-print` = post_archiving) %>%
  rename(`Can archive publisher's version/PDF` = publ_archiving)

#----------------------------------------------------------------------------------------------------------------------------
# save current version of spreadsheet
#----------------------------------------------------------------------------------------------------------------------------

#rearrange columns
final_col <- c('Journal title.doaj',  
               'Journal article processing charges (APCs)', 'APC information URL',
               "Average number of weeks between submission and publication",
               "Can archive pre-print", "Can archive post-print",
               "Can archive publisher's version/PDF", "Journal license", "Subjects", 
               'Journal URL', 'Publisher.doaj', "pISSN", "eISSN")
joined_data <- joined_data %>% 
  select(one_of(final_col)) %>%
  rename(`Journal title` = `Journal title.doaj`) %>% 
  rename(Publisher = `Publisher.doaj`) %>% 
  arrange(`Journal title`)

save_filename <- 'T:\\Dokumente\\Projekte\\Open Access Journals\\Journal Whitelist\\Journal_Whitelist_Table.csv'
write_csv(joined_data, save_filename)
