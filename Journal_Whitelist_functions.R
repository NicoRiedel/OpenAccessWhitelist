#transforms logical columns to yes/no columns
logical_to_yes_no <- function(logic_vec) 
{
  yes_no <- c("no", "yes")
  return(yes_no[logic_vec + 1])
}


#sometimes only pISSN or eISSN given, choose one
getISSN <- function(pISSN, eISSN)
{
  if(!is.na(pISSN)) {
    ISSN <- pISSN
  } else {
    ISSN <- eISSN
  }
  
  return(ISSN)
}


#in some cases several publishers are given for one journal - then pick lowest romeo col
get_lowest_color <- function(romeo_col_vec)
{
  color_list <- c("white", "gray", "yellow", "blue", "green")
  lowest_col_idx <- which(color_list %in% romeo_colour)[1]
  lowest_col <- color_list[lowest_col_idx]
  
  return(lowest_col)
}


#helper function to extract the postprint restriction info from the sherpa/romeo xml
extract_post_restr <- function(post_restr_str) 
{
  return(gsub("<.*?>", "", post_restr_str))
}
