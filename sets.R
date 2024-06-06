SetIntersection <- function(A, B) {
  AinB <- c()
  emptyset <- '∅'
  
  for (i in 1:length(A)){
    for (j in 1:length(B)) {
      if (A[i] == B[j]) {
        AinB = append(AinB, A[i])
      }
    }
  }
  if (length(AinB) == 0) {
    return(emptyset)
  } else {
    return(AinB)
  }
}



SetUnion = function (A, B) {
  AuB = c(A, B) #concatenate
  AuB = unique(sort(AuB))
  return(AuB)
}



SetComplement = function (A, B, U = c()) {
  AuB <- c(A,B)
  U <- c(U, AuB)|> 
    sort() |>
    unique() |>


  U <- U[!U %in% A] 
  
  return(U)
}


prime = c(2,3,7,9,11,13,17,19,23)
fibonacci = c(0,1,1,2,3,5,8,13,21)

SetIntersection(A = fibonacci,
                B = prime)
# [1]  2  3 13
SetUnion(A = fibonacci,
         B = prime)
# [1]  [1]  0  1  2  3  5  7  8  9 11 13 17 19 21 23
  
SetComplement(prime,fibonacci)
# [1]  0  1  5  8 21
