########################################################################
# Case1 : Generate Random Numbers following Multivariate Normal Dist'n 
# with mean mu and variance sigma using 'Spectral Decomposition' Method


random_numbers <- function(gen, mu_vec, var_mat){
  
  # gen: 몇 개의 random number set을 generate할 것인가?
  # X ~ N(mu_vec, var_mat)
  
  size <- nrow(var_mat)
  random_matrix <- matrix(0, gen, size) # gen x 3 모양의 matrix
  
  P <- eigen(var_mat)$vectors # P
  Lambda_half <- diag(sqrt(eigen(var_mat)$values), size, size) # Λ
  var_mat_sqrt <- P %*% Lambda_half %*% t(P)
  
  for (i in 1:gen){
  
    std_normal <- rnorm(size, 0, 1)
    normal_vec <- matrix(std_normal, size, 1)
    random_matrix[i,] <- var_mat_sqrt %*% normal_vec + mu_vec
  }
  
  return(random_matrix)
}


mu_vec <- matrix(0:2, 3, 1)
var_mat <- matrix(c(1, -0.5, 0.5, -0.5, 1, -0.5, 
                    0.5, -0.5, 1), 3, 3)

result <- random_numbers(200, mu_vec, var_mat)

pairs(result)



########################################################################
# Case2 : Generate Random Numbers following Multivariate Normal Dist'n 
# with mean mu and variance sigma using 'Cholesky Decomposition' Method

random_numbers <- function(gen, mu_vec, var_mat){

  size <-  nrow(var_mat)
  random_matrix <- matrix(0, gen, size)
  var_mat_sqrt <- t(chol(var_mat))

  for (i in 1:gen){
    
    z <- matrix(rnorm(size, 0, 1), size, 1)
    normal_vec <- var_mat_sqrt %*% z + mu_vec
    random_matrix[i,] <- normal_vec
    
  }
  
  return(random_matrix)
  
}


mu_vec <- matrix(0:2, 3, 1)
var_mat <- matrix(c(1, -0.5, 0.5, -0.5, 1, -0.5, 
                    0.5, -0.5, 1), 3, 3)
result <- random_numbers(200, mu_vec, var_mat)

pairs(result)
