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
                      "Medicinski", "Medyczne", "Dānish", "Instituto")
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
get_scopus_var <- function(eISSN, pISSN, scopus_data, varname)
{
  #try al matches of pISSN/eISSN
  ee_var <- get_var_sub(eISSN, scopus_data$eISSN, scopus_data[[varname]])
  ep_var <- get_var_sub(eISSN, scopus_data$pISSN, scopus_data[[varname]])
  pe_var <- get_var_sub(pISSN, scopus_data$eISSN, scopus_data[[varname]])
  pp_var <- get_var_sub(pISSN, scopus_data$pISSN, scopus_data[[varname]])
  
  #get nonzero entry
  var_vec <- c(ee_var, ep_var, pe_var, pp_var)
  var_vec <- var_vec[!is.na(var_vec)]
  if(length(var_vec) == 0) {
    var <- NA
  } else {
    var <- var_vec[1]
  }
  
  return(var)
}

#obtains var value for one combination of eISSN/pISSN
get_var_sub <- function(ISSN, scopus_ISSN, scopus_var)
{
  SRJ_idx <- match(ISSN, scopus_ISSN)
  if(is.na(SRJ_idx)) {
    var <- NA
  } else {
    var <- scopus_var[[SRJ_idx]]
  }
  
  return(var)
}

