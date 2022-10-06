col_id <- function(db1, db2, col_dir){
  db1 <- as.data.frame(db1)
  db2 <- as.data.frame(db2)
  col_dir <- as.data.frame(col_dir)
  
  db1_cols <- names(db1)
  db2_cols <- names(db2)
  
  if(any(!db1_cols %in% col_dir[,1])){
    stop("Missing columns from directory: ",
         toString(db1_cols[which(!db1_cols %in% col_dir[,1])]))
  }
  
  new_cols <- col_dir[,1]
  names(new_cols) <- col_dir[,2]
  new_cols <- new_cols[db2_cols]
  new_cols <- new_cols[!is.na(new_cols)]
  
  suppressWarnings(names(db2)[which(names(db2) %in% names(new_cols))] <- new_cols)
  
  return(db2)
}
