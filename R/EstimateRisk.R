#' Sex Specific Estimation for PredictRiskScores
#'
#' \code{EstimateRisk} performs the calculations using input values and beta-coefficients
#'
#' @usage ParamCheck(input, male, female, as.percent)
#'
#' @param input list of all input values inherited from the function call
#' @details
#' @return.
#' @author
#' Billy Wu (R Developer)
#'

value.score <- mapply(function(val, f.coeff, m.coeff){

  effect <- rep(0, length(input$sex))
  effect <- replace(effect, f.ind, val[f.ind] * f.coeff)
  effect <- replace(effect, m.ind, val[m.ind] * m.coeff)

  return(effect)
},
val = values,
f.coeff = fem.coeff,
m.coeff = male.coeff,
SIMPLIFY = F)

sum.score <- Reduce("+", value.score)

estimate <- rep(0, length(sum.score))
estimate <- replace(estimate, f.ind, 1 - 0.983169213058 ^ exp(sum.score[f.ind]))
estimate <- replace(estimate, m.ind, 1 - 0.974755526232 ^ exp(sum.score[m.ind]))

rounded.val <- as.numeric(formatC(round(estimate, dp),
                                  format = 'f',
                                  digits = dp))
