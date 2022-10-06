coerce_cols <- function(to, from){
  to <- as.data.frame(to)
  from <- as.data.frame(from)
  
  to_cols <- names(to)
  from_cols <- names(from)
  
  case_diff <- setdiff(setdiff(to_cols, from_cols),
                       setdiff(tolower(to_cols), tolower(from_cols)))
  
  names(from)[which(tolower(from_cols) %in% case_diff)] <- case_diff
  from_cols <- names(from)
  
  if(any(!to_cols %in% from_cols)){
    warning("Missing columns from second dataframe: ",
            toString(setdiff(to_cols, from_cols)))
  }
  
  to <- to[, which(to_cols %in% from_cols)]
  to_cols <- names(to)
  
  from <- from[, to_cols]
  
  to_class <- sapply(to, class)
  from_class <- sapply(from, class)
  
  mismatch_class <- which(!to_class == from_class)
  
  for(i in mismatch_class){
    suppressWarnings(class(from[,i]) <- to_class[i])
    warning(names(from_class)[i], " coerced to class ", to_class[i])
  }
  
  return(from)
}
