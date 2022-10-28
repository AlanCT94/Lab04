#' Ridge regression
#'
#' @param formula The Y and X variables of the regression
#' @param data A data frame
#' @param lambda Hyperparameter of the regression
#'
#' @return res
#' @export
#'
#' @examples data(iris)
#'           ridgereg(Petal.Length~Species, iris,2)
ridgereg <- function(formula, data, lambda = 0){
  stopifnot(class(formula) == "formula") #check if it's formula
  stopifnot(is.data.frame(data)) # check if it's data frame
  stopifnot(is.numeric(lambda), lambda >= 0) # check if lambda is a + number

  if (sum(is.na(data)) > 0) {
    stop("There are missing values (NA) in the data")
  }

  # Extract the X and Y
  X <- stats::model.matrix(formula, data)
  y <- data[,all.vars(formula)[1]]
  stopifnot(is.numeric(y))


  data1 <- format(deparse(substitute(data)))
  # Mean and sd of each column
  x_mean <- colMeans(X[,-1]) #We dont calculate the Interceptor column [,-1]
  sd_x <- apply(X[,-1], 2, stats::sd)

  # Normalize the X matrix (x_norm)
  x <- sweep(X[,-1], 2, x_mean, "-") # subtract the mean to each X
  X_norm <- sweep(x, 2, sd_x, "/") # Divide the subtract above by the sd

  n <- ncol(X_norm) #number of columns

  # Regression Coefficient (B)
  B_r <- solve(t(X_norm) %*% X_norm + lambda*diag(n)) %*% (t(X_norm)%*%y)
  B_r1 <- B_r/sd_x
  B<- as.vector(B_r1)

  # Interceptor approximation (B0)
  B0 <- mean(y) - B%*%x_mean # E[B0] = y - B*E[X]
  B_hat <- as.vector(c(B0, B)) # create a new vector
  names(B_hat) <- colnames(X) #adding names from X

  # Fitted values (y_h)
  y_h <- X_norm%*%B
  y_hat <- as.vector(y_h)

  # Save the results into a class object
  #call < -match.call()
  ridgereg <- list(call = call, formula = formula,
                   data= data1, coefficients = B_hat,
                   fitted_values = y_hat)
  class(ridgereg) = "ridgereg" #define the class of the list of results
  return(ridgereg)
}
