# logit and logistic transformations --------------------------------------
#' Logit and logistic functions
#'
#'\code{logit} and \code{logistic} implement the logit and logistic transformations, respectively.
#'
#' @param x A numeric vector.
#' @param p A numeric vector whose components are numbers between 0 and 1.
#' @return The functions apply the logit and logistic transformation to each element of the vector
#' passed as argument.
#' In particular, logit(p)=ln(p/(1-p)) and logistic(x)=exp(x)/(1+exp(x)).
#' @examples
#' logit(0.1)
#' logit(0.5)
#' logistic(0)
#' logistic(logit(0.25))
#' logit(logistic(2))

logit <- function(p) log(p / (1 - p));

#' @rdname logit
logistic <- function(x) 1 / (1 + exp(-x));

# CDF of the GiViTI calibration test statistic ----------------------------

#' CDF of the Calibration Statistic Under the Null Hypothesis
#'
#'\code{givitiStatCdf} returns the cumulative density function of the
#'calibration statistic under the null hypothesis.
#'
#' @param t The argument of the CDF. Must be a scalar value.
#' @param m The scalar integer representing the degree of the polynomial
#'  at the end of the forward selection.
#' @param devel A character string specifying if the model has been fit on
#'  the same dataset under evaluation (\code{internal}) or if the model has
#'  been developed on an external sample (\code{external}).
#' @param thres A numeric scalar between 0 and 1 representing the
#'  significance level adopted in the forward selection.
#' @return A number representing the value of the CDF evaluated in t.
#' @seealso \code{\link{givitiCalibrationBelt}} and \code{\link{plot.givitiCalibrationBelt}}
#'  to compute and plot the calibaration belt, and
#'  \code{\link{givitiCalibrationTest}} to perform the
#'  associated calibration test.
#' @examples
#' givitiStatCdf(3, 1, "external", .95)
#' givitiStatCdf(3, 2, "internal", .95)

givitiStatCdf<-function(t, m, devel, thres) {

  if (length(t) > 1 | length(m) > 1 | length(devel) > 1 | length(thres) > 1) {
    stop("CDF evaluation: the arguments 't', 'm' and 'devel' cannot be vectors.")
  }

  if (!devel %in% c("internal","external")) {
    stop("CDF evaluation: the 'devel' argument must be either 'internal' or external'")
  }

  if (!m %in% c(1, 2, 3, 4)) {
    stop("CDF evaluation: m must be an integer from 1 to 4")
  }

  if (thres < 0 | thres > 1 | !is.numeric(thres)) {
    stop("CDF evaluation: the argument 'thres' must be a number in [0,1]")
  }

  if (devel %in% "internal" & m %in% 1) {
    stop("CDF evaluation: if devel='internal', m must be an integer from 2 to 4")
  }

  pDegInc <- 1 - thres
  k <- qchisq(1 - pDegInc, df = 1)

  if(devel == "external") {

    if(t <= (m-1)*k) {

      cdfValue <- 0

    } else {

      if(m == 1){
        cdfValue <- (pchisq(t, df = 2))
      }

      if(m == 2){
        cdfValue <- ((pchisq(t, df = 1) - 1 + pDegInc +
                (-1) * sqrt(2) / sqrt(pi) * exp(-t / 2) * ( sqrt(t) - sqrt(k))) / pDegInc)
      }

      if(m==3){

        integrand1<-function(y) {
          return( (pchisq(t - y, df = 1) - 1 + pDegInc) * dchisq(y, df = 1) )
        }

        integrand2<-function(y) {
          return( (sqrt(t - y) - sqrt(k)) * 1 / sqrt(y) )
        }

        integral1 <- integrate(integrand1, lower = k, upper = t - k)$value
        integral2 <- integrate(integrand2, lower = k, upper = t - k)$value

        num <- (integral1 - exp(-t/2) / (2*pi) * 2 * integral2)
        den <- pDegInc^2

        cdfValue <- (num / den)
      }

      if(m==4){

        integrand<-function(r) {
          return(r^2 * (exp(-(r^2) / 2) - exp(-t/2)) *
                 (- pi * sqrt(k) / (2 * r) + 2 * sqrt(k) / r *
                  asin((r^2 / k - 1)^(-1/2)) - 2 * atan(( 1 - 2 * k / r^2)^(-1/2)) +
                  2 * sqrt(k) / r * atan((r^2 / k - 2)^(-1/2)) +
                  2 * atan(r / sqrt(k) * sqrt(r^2 / k - 2))
                  - 2 * sqrt(k) / r * atan(sqrt(r^2/ k - 2))))
        }

        integral <- integrate(integrand, lower = sqrt(3 * k),upper = sqrt(t))$value

        cdfValue <- ((2 / (pi * pDegInc^2))^(3 / 2) * integral)
      }
    }
  }

  if(devel == "internal") {

    if(t <= (m-2)*k) {

     cdfValue <- (0)

    } else {

      if(m == 2){

        cdfValue <- (pchisq(t, df = 1))

      }

      if(m == 3){

        integrand <- function(r) {
          return(r * exp(- (r^2) / 2) * acos(sqrt(k) / r))
        }

        integral <- integrate(integrand,
                              lower = sqrt(k),
                              upper = sqrt(t))$value

        cdfValue <- (2 / (pi * pDegInc) * integral)
      }

      if(m==4){

        integrand<-function(r) {
          return(r^2 * exp(-(r^2) / 2) * (atan(sqrt(r^2 / k * (r^2 / k - 2)))-
                               sqrt(k) / r * atan(sqrt(r^2 / k - 2)) -
                               sqrt(k) / r * acos((r^2 / k - 1)^(-1/2))))
        }

        integral <- integrate(integrand,
                              lower = sqrt(2 * k),
                              upper = sqrt(t))$value

        cdfValue <- ((2 / pi)^(3 / 2) * (pDegInc)^(-2) * integral)
      }
    }
  }

  if(cdfValue < (-0.001) | cdfValue > (1.001)) {
    stop("CDF evaluation: cdfValue outside [0,1]. ")
  }

  if(cdfValue > (-0.001) & cdfValue < 0) {
    output <- 0
  }
  if(cdfValue < (1.001) & cdfValue > 1) {
    output <- 1
  }
  if(cdfValue <= 1 & cdfValue >= 0) {
    output <- cdfValue
  }
  return(output)

}

# Select the best degree m -----------------------------------------------

#' Forward Selection in Polynomial Logistic Regression
#'
#'\code{polynomialLogRegrFw} implements a forward selection in a
#' polynomial logistic regression model.
#'
#' @param data A \code{data.frame} object with the numeric variables "o", "e"
#'  and "logite", representing the binary outcomes, the probabilities of the
#'  model under evaluation and the logit of the probabilities.
#'  The variable "e" must contain values between 0 and 1. The variable
#'  "o" must assume only the value 0 and 1.
#' @param thres A numeric scalar between 0 and 1 representing the significance level
#'  adopted in the forward selection.
#' @param maxDeg The maximum degree considered in the forward selection.
#' @param startDeg The starting degree in the forward selection.
#' @return A list containing the following components:
#' \describe{
#'   \item{fit}{An object of class \code{glm} containig the output of the fit
#'              of the logistic regression model at the end of the iterative
#'              forward selection.}
#'   \item{m}{The degree of the polynomial at the end of the forward selection.}
#' }
#' @examples
#' e <- runif(100)
#' logite <- logit(e)
#' o <- rbinom(100, size = 1, prob = e)
#' data <- data.frame(e = e, o = o, logite = logite)
#' polynomialLogRegrFw(data, .95, 4, 1)

polynomialLogRegrFw<-function(data, thres, maxDeg, startDeg) {

  #START local functions ****************************************************

  W <- NULL
  #Warning handler
  w.handler <- function(w){
    W <<- w
    invokeRestart("muffleWarning")
  }

  #END local functions ****************************************************

  if(startDeg > maxDeg) {
    stop("FW Selection in Polynomial Logistic Regression:
         starting degree greater than max degree")
  }

  if(startDeg == 1) {

    fitFormula <- formula( o ~ 1)

  } else {

    stringRhs <- paste(paste("I(logite^",
                             seq(from = 1, to = (startDeg - 1)),
                             ")", sep=""),
                       collapse=" + ")
    fitFormula<-as.formula( paste(" o ~ ", stringRhs, sep=""))

  }

  n <- startDeg
  while (n <= maxDeg){

    fitFormula <- update(fitFormula,
                         paste("~ . + I(logite^", n, ")", sep = ""))

    fitNew <- withCallingHandlers(glm(formula = fitFormula,
                           family = binomial(link = "logit"), data),
                       warning = w.handler)


    if(n > startDeg) {
      if(pchisq(fit$deviance - fitNew$deviance, 1) < thres) {
        m <- n-1
        break
      }
    }

    m <- n
    fit <- fitNew
    n <- n+1
  }

  return(list(fit = fit, m = m))
}

