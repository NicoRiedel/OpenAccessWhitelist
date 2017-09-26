#transforms logical columns to yes/no columns
logical_to_yes_no <- function(logic_vec) 
{
  yes_no <- c("no", "yes")
  return(yes_no[logic_vec + 1])
}


#checks if any of the regional keywords are contained in the journal name
is_regional_journal <- function(journal_name)
{
  regional_terms <- c("Korean", "Canadian", "Oman", "Libyan", "Iran",
                      "Macedonian", "Chinese", "India", "Scandinavi",
                      "Saudi", "Arab", "Asia", "Nigeria", "Yoga",
                      "Israel", "Australasian", "Bosnian", "Qaboos",
                      "Japan", "Ethiopian", "Africa", "Korea",
                      "Slovenian", "Polish", "Brazilian", "Malaysia",
                      "Egyptian", "Alexandria", "Bali", "Indonesia",
                      "Nepal", "Pakistan", "Jundishapur", "South Eastern",
                      "Anatolia", "Rambam Maimonides", "Sri Lanka",
                      "Bangabandhu Sheikh Mujib", "Irish", "Zahedan",
                      "Middle East", "Bangladesh", "Motriz", "Nordic",
                      "Medicinski", "Medyczne", "Dānish")
  grep_results <- sapply(regional_terms, grepl, x=as.character(journal_name))
  
  return(any(grep_results))
}


#splits the subject categories and selects only the most narrow subcategories
subject_simplification <- function(subject)
{
  subject_categories <- strsplit(subject, "|", fixed = TRUE)[[1]]
  subject_subcategories <- strsplit(subject_categories, ":")
  
  #if there are several different main categories take the most detailed category for each of them
  if(length(subject_categories) > 1) {
    subjects_simplified <- sapply(subject_subcategories, tail, n=1)
  } else {
    #if there is only one main category, take the two most detailed subcategories
    #unless there are only two hiracy levels, then only take one
    if(length(subject_subcategories) > 2) {
      subjects_simplified <- tail(subject_subcategories[[1]], n=2)   
      subjects_simplified <- rev(subjects_simplified) #reverse order such that the more specific is the first element
    } else {
      subjects_simplified <- c(tail(subject_subcategories[[1]], n=1), "")
    }
  }
  subjects_simplified <- trimws(subjects_simplified)
  
  return(subjects_simplified)
}


#define separate function instead of using regular join because the 
#scopus eISSN as well as pISSN has to be checked against both DOAJ pISSN/eISSN
get_SJR <- function(eISSN, pISSN, scopus_data)
{
  #try al matches of pISSN/eISSN
  ee_SJR <- get_SJR_sub(eISSN, scopus_data$eISSN, scopus_data$`SJR Impact`)
  ep_SJR <- get_SJR_sub(eISSN, scopus_data$pISSN, scopus_data$`SJR Impact`)
  pe_SJR <- get_SJR_sub(pISSN, scopus_data$eISSN, scopus_data$`SJR Impact`)
  pp_SJR <- get_SJR_sub(pISSN, scopus_data$pISSN, scopus_data$`SJR Impact`)
  
  #get nonzero entry
  SRJ_vec <- c(ee_SJR, ep_SJR, pe_SJR, pp_SJR)
  SRJ_vec <- SRJ_vec[!is.na(SRJ_vec)]
  if(length(SRJ_vec) == 0) {
    SRJ <- NA
  } else {
    SRJ <- SRJ_vec[1]
  }
  
  return(SRJ)
}


#calculates SJR for one combination of eISSN/pISSN
get_SJR_sub <- function(ISSN, scopus_ISSN, scopus_SJR)
{
  SRJ_idx <- match(ISSN, scopus_ISSN)
  if(is.na(SRJ_idx)) {
    SJR <- NA
  } else {
    SJR <- scopus_SJR[[SRJ_idx]]
  }
  
  return(SJR)
}

