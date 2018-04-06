regex_transform_and <- function(string){
  regex <- stri_split_fixed(string, "&")
  transform_regex <- function(x) {paste0(x[1],"\\b.*",x[2],"\\b|\\b",x[2],"\\b.*",x[1])} # regex and syntax
  regex <- lapply(regex, transform_regex)
  regex <- lapply(regex, function(x) paste0("\\b",x,"\\b")) # add word bounds
  regex <- unlist(regex)
  return(regex)
}