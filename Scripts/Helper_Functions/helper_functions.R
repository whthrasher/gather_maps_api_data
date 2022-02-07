str_right <- function(str) {
  return_str <- str_sub(str, 
                        start = str_length(str), 
                        end = str_length(str))
  return(return_str)
}