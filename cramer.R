cramer <- function(matrix, result){
  
  det <- det(matrix)
  if (det != 0 ){
    
    st_col <- matrix(matrix[,1])
    nd_col <- matrix(matrix[,2])
    rd_col <- matrix(matrix[,3])
    
    Detx <- det(matrix(c(result, nd_col, rd_col), nrow = 3))
    Dety <- det(matrix(c(st_col, result, rd_col), nrow = 3))
    Detz <- det(matrix(c(st_col, nd_col, result), nrow = 3))
    
    x <- Detx / det
    y <- Dety / det
    z <- Detz / det
    print(paste0("X: ", x))
    print(paste0("Y: ", y))
    print(paste0("Z: ", z))
  } else {
    
    return(NULL)
  }
}
# Matrix 3v3

i <- 1
vectorMat <- c()
vectorRes <- c()

while (i <= 12) {
  
  repeat {
      num <- suppressWarnings(as.integer(readline("Type a number: ")))
    if(!is.na(num)) {
      break
    }
  }
  if (i > 9){
    vectorRes <- c(vectorRes, num)
  }
  else {
    vectorMat <- c(vectorMat, num)
  }
  
  i = i + 1
}

matrixEq <- matrix(vectorMat, byrow = FALSE, ncol = 3)
res <- matrix(vectorRes, ncol = 1, nrow = 3)

cramer(matrix = matrixEq, result = res)