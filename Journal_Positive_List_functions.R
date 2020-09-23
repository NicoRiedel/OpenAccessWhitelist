#currency excange rates for conversion of different API costs to 
get_exchange_rate <- function(currency_from, currency_to)
{
  exchange_rate <- paste0("https://free.currencyconverterapi.com/api/v5/convert?q=",
                          currency_to, "_", currency_from, "&compact=y&apiKey=25085f671031f92b8214") %>%
    readLines(warn = FALSE) %>%
    fromJSON() %>%
    unlist()
  
  return(exchange_rate)
}

#transforms logical columns to yes/no columns
logical_to_yes_no <- function(logic_vec) 
{
  yes_no <- c("no", "yes")
  return(yes_no[logic_vec + 1])
}


#there are some regionally focused journals that will not be that relevant
#for our scientists in Berlin
#non-comprehensive list of regional terms to filter those regional journals
#some of those excluded journals might still have a more international focus
#but I did not check all journals individually
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
                      "Medicinski", "Medyczne", "Dānish", "Instituto",
                      "Egyptian", "Balkan", "Turkish", "Istanbul",
                      "Brasileira", "Upsala", "stanbul", "Sahara")
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
  if(!is.na(eISSN)) {
    ee_var <- get_var_sub(eISSN, scopus_data$eISSN, scopus_data[[varname]])
    ep_var <- get_var_sub(eISSN, scopus_data$pISSN, scopus_data[[varname]])
  } else {
    ee_var <- NA
    ep_var <- NA
  }
  
  if(!is.na(pISSN)) {
    pe_var <- get_var_sub(pISSN, scopus_data$eISSN, scopus_data[[varname]])
    pp_var <- get_var_sub(pISSN, scopus_data$pISSN, scopus_data[[varname]])
  } else {
    pe_var <- NA
    pp_var <- NA
  }
  
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


#calculates JIF quartiles (beware that quartile definition is ambiguous)
calculate_quartiles <- function(n)
{
  q_vec <- rep("", n)
  q_vec[1:ceiling(n/4)] <- "Q1"
  q_vec[(ceiling(n/4) + 1):ceiling(n/2)] <- "Q2"
  q_vec[(ceiling(n/2) + 1):ceiling(n*3/4)] <- "Q3"
  q_vec[(ceiling(n*3/4) + 1):n] <- "Q4"
  
  return(q_vec)
}


