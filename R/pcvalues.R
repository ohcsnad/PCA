#' PC values
#'
#'  \code{pcvalues()} produces results in a table format of eigenvectors, loadings, eigenvalues,
#'  and variance/accumulated proportion of variance explained of the principal components.
#'  The eigenvalues represent the amount of variance that can be explained by each principal component.
#'  The loadings represent the covariability of each variable with the principal component.
#'
#' @param data dataframe with quantitative variables
#' @param scale a boolean value to indicating whether the data should be scaled or not. Default \code{scale = FALSE}
#' @param value determines whether values outputted are "eigenvectors", "loadings", "eigenvalues", or "variance"
#' @param digits number of decimal digits. Default \code{digits = 4}
#'
#' @importFrom knitr kable
#'
#' @return a table with either eigenvectors, loadings, eigenvalues, or variance explained
#' @export
#'
#' @examples pcvalues(data = pima_diabetes, scale = T, value = "eigenvectors")
#' pcvalues(data = pima_diabetes, scale = T, value = "loadings")
#' pcvalues(data = pima_diabetes, scale = T, value = "eigenvalues", digits = 3)
#' pcvalues(data = pima_diabetes, scale = T, value = "variance", digits = 3)

pcvalues <- function(data = NULL, scale = FALSE, value = "null", digits = 4){
  n <- ncol(data)
  vect <- (1:ncol(data))
  var_names <- names(data)

  #scaling data
  if(scale == TRUE){
      data <- scale(data)
  }

  s <- cov(data) #covariance matrix
  e <- eigen(s)

  #matrix of eigenvectors
  P <- e$vectors

  #eigenvalues
  lambda <- e$values
  #sqrt lambda to get loadings
  sqrt_lambda <- sqrt(lambda)

  #component matrix: loadings
  loadings <- matrix(NA, n, n)
  for(i in 1:n){
    loadings[,i] <- P[,i]*sqrt_lambda[i]
  }

  #eigenvalues_computation: just to check that it matches lambda
  eigenvalues_computation <- numeric(n) #storage of replicates
  for(i in 1:n){
    eigenvalues_computation[i] <- sum((loadings[,i])^2)
  }

  #variance explained
  variance <- c(lambda / sum(lambda))
  #cumulative variance
  cum_variance <- c(cumsum(lambda) / sum(lambda))

  if(value == "eigenvectors") {
    df_P <- data.frame(P)
    colnames(df_P) <- c(paste("PC",vect))
    rownames(df_P) <- c(var_names)
    eigenvectors <- kable(df_P, digits=digits, caption = "Matrix of Eigenvectors ")
    return (eigenvectors)
  }

  if(value == "loadings"){
    df_loadings <- data.frame(loadings)
    colnames(df_loadings) <- c(paste("PC",vect))
    rownames(df_loadings) <- c(var_names)
    loadings <- kable(df_loadings, digits=digits, caption = "Component Matrix i.e. loadings")
    return (loadings)
  }

  if(value == "eigenvalues"){
    df_lambda <- data.frame(lambda)
    rownames(df_lambda) <- c(paste("PC",vect))
    colnames(df_lambda) <- c("Eigenvalue")
    eigenvalues <- kable(df_lambda, digits=digits, caption = "Eigenvalues")
    return(eigenvalues)
  }

  #eigen_comp: just to check that it matches lambda
  if(value=="eigenvalues_comp"){
    df_eigenvalues_computation <- data.frame(eigenvalues_computation)
    rownames(df_eigenvalues_computation) <- c(paste("PC",vect))
    colnames(df_eigenvalues_computation) <- c("Eigenvalue")
    eigen_comp <- kable(df_eigenvalues_computation, digits=digits, caption = "Eigenvalue (computed). Just to make sure computed values match")
    return(eigen_comp)

  }

  if(value == "variance"){
    df_var_cumvar <- data.frame(variance, cum_variance)
    colnames(df_var_cumvar) <- c("Var. Explained", "Cumul. Var. Explained")
    rownames(df_var_cumvar) <- c(paste("PC",vect))
    variance <- kable(df_var_cumvar, digits = digits, caption = "Variance")
    return(variance)
  }

  if(value != "eigenvalues" || value != "eigenvectors" || value != "eigenvalues_comp" || value != "loadings" || value != "variance" || value == "null")
    readline(prompt = '\nPlease set value argument to "eigenvectors", "loadings", "eigenvalues", or "variance"')
    stop()

}


