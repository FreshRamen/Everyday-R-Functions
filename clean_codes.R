clean_codes <- function(string){
  # Function cleans certain codes from a string
  unwanted_array = list(
    '<br />'='; ', # Remove <br... by ;
    '( ){2,}'='; ' # Replace multiple spaces by "; "
    )
  
  out <- string
  for(i in seq_along(unwanted_array))
      out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)  
  
  return(out)
}