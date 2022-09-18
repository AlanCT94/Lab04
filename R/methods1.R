print.linreg <- function(obj){

  cat("Call:","\n")
  cat("lm(",format(obj$formula),",data=",obj$data1,")","\n")
  cat("\n")
  cat("Coefficients:","\n")
  print(obj$coefficients_variance)
}
resid.linreg <- function(obj){

  cat("Residuals:","\n")
  return(obj$residuals)

}
pred.linreg <- function(obj){
  cat("Predicted vaues y","\n")
  return(obj$fitted_values)
}
coef.linreg <- function(obj){
  cat("Returns the coefficients","\n")
  return(format(obj$coefficients))
}
summary.linreg <- function(obj){
  cat("Hola Mundo")
}
