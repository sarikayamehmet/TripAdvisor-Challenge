threshold <- function(train_y, test_y){
  A <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  error <- rep(NA, 7)
  for(i in A){
    tmp_y <- sapply(test_y, function(y_row) {
                            if(y_row < i){
                              return(0)
                            }else{
                              return(1)
                            }})
    cat(table(tmp_y))
    error[(i-0.2) / 0.1 + 1] <- mean(tmp_y != train_y)
    print(error[(i-0.2) / 0.1 + 1])
  }
  min_index <- which.min(error)
  returned_y <- sapply(test_y, function(y_row) {
    if(y_row < A[min_index]){
      return(0)
    }else{
      return(1)
    }})
  return(list(A[min_index], returned_y))
}