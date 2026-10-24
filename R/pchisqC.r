#' Compares full v. reduced logistic regression models with Chi-Squared test
#' 
#' @description Compares full and reduced logistic regression models with Chi-Square Test to assess whether additional variables of full model are statistically significant.
#' @param reduced The reduced logistic regression model as an object. This is model with fewer independent variables. 
#' @param full The full logistic regression model as an object. This is model with more independent variables.
#' @param digits (Optional) The number of digits to display after decimal point, default is 3.
#' @return A formatted result object with the chi-squared statistic, df, and P-value to test the null hypothesis that the full model is no better than the reduced model.
#' @examples 
#'    library(RCPA4)
#'    
#'    \donttest{
#'    model_full <- logregC(battleground2020 ~ vep20_turnout + adv_or_more, data=states)
#'    model_reduced <- logregC(battleground2020 ~ vep20_turnout, data=states)
#'    
#'    pchisqC(reduced=model_reduced, full=model_full)
#'    }
#' @export
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 14. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), Chapter 9. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @keywords internal
#' @importFrom methods is
#' @importFrom stats pchisq
#' @md



pchisqC = function(reduced, full, digits=3)
{
  assert_required(reduced, full)  
  # check if user supplied glm objects 
  if(methods::is(reduced, "glm")!=TRUE) message("Warning: Reduced model is not a glm object.")
  if(methods::is(full, "glm")!=TRUE) message("Warning: Full model is not a glm object.")
  chisquared_stat = reduced$deviance - full$deviance
  n_diff_warning = "Warning: These models have different numbers of observations."
  if(reduced$df.null != full$df.null) { message(n_diff_warning) }
  df = length(full$coefficients) - length(reduced$coefficients)
  pvalue = stats::pchisq(chisquared_stat, df, lower.tail = FALSE)
  results <- c(
    format(round(chisquared_stat, digits), scientific = FALSE, trim = TRUE, nsmall = digits),
    format(round(df, 0), scientific = FALSE, trim = TRUE, nsmall = 0),
    format_p_value(pvalue, digits = digits)
  )
  names(results) = c("Chi-Squared","    DF","    P-Value")
  structure(results, class = "pchisqC_result")
}

#' @export
print.pchisqC_result <- function(x, ...) {
  print(noquote(unclass(x)))
  invisible(x)
}
