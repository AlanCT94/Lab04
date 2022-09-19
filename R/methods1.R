#' Methods of function linreg
#'
#' @param obj Refers to the res variable return by the linear regression function
#'
#' @return c, obj$residuals, obj$fitted_values, format(obj$coefficients),
#' @export
#'
#' @examples data(iris)
#'           s<-linreg(Petal.Length~Species, iris)
#'           linreg.print(s)
#'
linreg.print <- function(obj){
  c<-as.vector(obj$coefficients)
  names(c)<- row.names(obj$coefficients)
  cat("\n")
  cat("Call:","\n")
  cat("linreg(",format(obj$formula),",data=",obj$data1,")","\n")
  cat("\n")
  cat("Coefficients:","\n")
  return(c)
}
linreg.resid <- function(obj){

  cat("Residuals:","\n")
  return(obj$residuals)

}
pred <- function(obj){
  cat("Predicted vaues y","\n")
  return(obj$fitted_values)
}
linreg.coef <- function(obj){
  cat("Returns the coefficients","\n")
  return(format(obj$coefficients))
}
linreg.summary <- function(obj){
  sumlin <- data.frame(c= row.names(obj$coefficients),
                       Estimate = as.vector(obj$coefficients),
                       Std.Error = sqrt(obj$coefficients_variance), t_value = as.vector(obj$coefficients_tvalues),
                       Prt= obj$coefficients_pvalues)
  colnames(sumlin) <- c("","Estimate","Std.Error","t value"," Pr(>|t|)")
  cat("\n","Coefficients","\n")
  print(format(sumlin, justify="left"), row.names=FALSE)
  cat("\n","Residual standard error:",sqrt(obj$residual_variance),"on",obj$df,"degrees of freedom")

}
