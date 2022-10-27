#' Print
#'
#'Print model call coefficients
#'
#' @param x Refers to the object return by ridge regression
#' @param ... argument to be passed to methods
#'
#' @return Coefficients
#' @export
#'
#' @examples data(iris)
#'           s<-ridgereg(formula=Petal.Length~Species, data=iris, 2)
#'           print(s)
print.ridgereg <- function(x, ...){
  stopifnot(class(x) == "ridgereg")
  cat("", sep="\n\n")

  cat("Call:")

  cat("", sep="\n\n")

  print(x$call)

  cat("", sep="\n\n")

  cat("Coefficients:")
  cat("", sep="\n\n")
  print(x$coefficients)
}

#' Predict
#'
#' @param object Refers to the object return by ridge regression
#' @param ndf New data frame to predict
#' @param ... argument to be passed to methods
#'
#' @return fitted values
#' @export
#'
#' @examples data(iris)
#'           s<-ridgereg(formula=Petal.Length~Species, data=iris, 2)
#'           predict(s)
predict.ridgereg <- function(object, ndf, ...){
  # we check the class of the object that comes from the function
  # we check that the data to predict is the same as the one using on the function
  # stopifnot(class(object) == "ridgereg") &
  #   (names(...) %in% names(object$coefficients)[-1])

  #Take B coefficients and B0 (interceptor)
  B_hat_c <- as.matrix(object$coefficients[-1])
  B_hat_0 <- as.matrix(object$coefficients[1])

  #If the nargs(number of arguments) is 1, return the fitted values
  if(nargs() == 1){
    y_pred <- object$y_hat
  }
  else if(nargs() > 1){  #If the nargs() > 1 there is a y values to predict
    stopifnot(is.data.frame(ndf)) #check that is a data frame
    stopifnot(names(ndf)%in%names(object$coefficients[-1]))
    x <- as.matrix(ndf) #convert to a matrix
    B_hatc_pred <- x%*%B_hat_c
    y_pred <- as.vector(sweep(B_hatc_pred,2,B_hat_0,'+'))
  }

  return(y_pred)
}

#' B coefficients
#'
#' @param object Refers to the object return by ridge regression
#' @param ... argument to be passed to methods
#'
#' @return Coefficients
#' @export
#'
#' @examples data(iris)
#' s<-ridgereg(formula=Petal.Length~Species, data=iris, 2)
#' coef(s)
coef.ridgereg <- function(object, ...){
  print(object$coefficients)
}

