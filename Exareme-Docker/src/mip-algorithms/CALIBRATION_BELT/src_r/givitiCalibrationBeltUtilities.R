#Calibration belt boundary ----------------------------------------------------
#' Calibration Belt Confidence Region
#'
#' \code{calibrationBeltPoints} computes the points defining the boundary
#' of the confidence region.
#'
#' @param data A \code{data.frame} object with the numeric variables "o", "e"
#'  and "logite", representing the binary outcomes, the probabilities of the
#'  model under evaluation and the logit of the probabilities, respectively.
#'  The variable "e" must contain values between 0 and 1. The variable
#'  "o" must assume only the values 0 and 1.
#' @param seqG A vector containing the logit of the probabilities where the points
#'  of the calibration belt will be evaluated.
#' @param m A scalar integer representing the degree of the polynomial
#'  at the end of the forward selection.
#' @param fit An object of class \code{glm} containig the output of the fit
#'  of the logistic regression model at the end of the iterative
#'  forward selection.
#' @param thres A numeric scalar between 0 and 1 representing 1 - the significance level
#'  adopted in the forward selection.
#' @param cLevel A numeric scalar between 0 and 1 representing the confidence level
#' that will be used for the confidence region.
#' @param devel A character string specifying if the model has been fit on
#'  the same dataset under evaluation (\code{internal}) or if the model has
#'  been developed on an external sample (\code{external}).
#' @return A \code{data.frame} object with two columns, "U" and "L", containing
#' the points of the upper and lower boundary of the \code{cLevel}*100\%-level calibration belt evaluated
#' at values \code{seqG}.
#' @seealso \code{\link{givitiCalibrationBelt}} and \code{\link{plot.givitiCalibrationBelt}}
#'  to compute and plot the calibaration belt, and
#'  \code{\link{givitiCalibrationTest}} to perform the
#'  associated calibration test.
#' @examples
#' e <- runif(100)
#' logite <- logit(e)
#' o <- rbinom(100, size = 1, prob = e)
#' data <- data.frame(e = e, o = o, logite = logite)
#'
#' seqG <- logit(seq(from = .01, to =.99, by = .01))
#'
#' fwLR <- polynomialLogRegrFw(data, .95, 4, 1)
#'
#' calibrationBeltPoints(data, seqG, fwLR$m, fwLR$fit, .95, .90, "external")
calibrationBeltPoints <- function(data, seqG, m, fit,
                                  thres, cLevel, devel) {

  #START local functions ****************************************************

  logLikelihood <- function(beta) {

    probBeta <- logistic(t(beta) %*%  G )

    return(sum((1 - data$o) * log(1 - probBeta) +
           data$o * log(probBeta), na.rm=T) - thresholdLogLik)

  }

  logLikelihoodRho <- function(rho, beta, direction) {

    return(logLikelihood(beta + rho * direction))

  }

  logLikelihoodRhoMin <- function(x) {

    logLikelihoodRho(x, beta = betaML, direction = ( - gradNorm) )

  }

  logLikelihoodRhoMax <- function(x) {

    logLikelihoodRho(x, beta = betaML, direction = gradNorm)

  }

  jacLogLikelihood <- function(beta){

    probBeta <- logistic(t(beta) %*%  G )

    return(t(G %*% t(data$o - probBeta)))

  }

  Fn <- function(x){

    numEq <- length(x)

    vecG <- g^seq(0, m)
    vecG <- vecG / sqrt(sum(vecG^2))

    probBeta <- logistic(crossprod(x[seq(1:(length(x)-1))],G))

    #Gradient of logLikelihood (constraint) parallel to (1,g,...,g^m)
    F1 <- t(tcrossprod(G, (data$o- probBeta))) - x[length(x)] * vecG

    #Likelihood equal to threshold value
    F2 <- sum((1 - data$o) * log(1 - probBeta) +
                data$o * log(probBeta), na.rm = T) - thresholdLogLik

    return(c(F1, F2))
  }

  JacobFn <- function(x){

    numEq <- length(x)

    vecG <- g^seq(0, m)
    vecG <- vecG / sqrt(sum(vecG^2))

    probBeta <- logistic( crossprod(x[seq(1:(length(x) - 1))],  G ))

    piOneMinusPiBeta <- (probBeta * (1 - probBeta))
    matrixProbBeta <- t(sapply(seq(from=1,to=(m+1)),function(x) {return(piOneMinusPiBeta)}))
    H <- tcrossprod(G * matrixProbBeta,G)

    J_F1 <- cbind(H, vecG)

    #Gradient of logLikelihood
    J_F2 <- c(t(tcrossprod(G, (data$o - probBeta))), 0)

    return(rbind(J_F1, J_F2))
  }

#   Fn <- function(x){
#
#     numEq <- length(x)
#
#     vecG <- g^seq(0, m)
#     vecG <- vecG / sqrt(sum(vecG^2))
#
#     probBeta <- logistic(t(x[seq(1:(length(x)-1))]) %*%  G )
#
#     #Gradient of logLikelihood (constraint) parallel to (1,g,...,g^m)
#     F1 <- t(G %*% t(data$o- probBeta)) - x[length(x)] * vecG
#
#     #Likelihood equal to threshold value
#     F2 <- sum((1 - data$o) * log(1 - probBeta) +
#               data$o * log(probBeta), na.rm = T) - thresholdLogLik
#
#     return(c(F1, F2))
#   }
#
#   JacobFn <- function(x){
#
#     numEq <- length(x)
#
#     vecG <- g^seq(0, m)
#     vecG <- vecG / sqrt(sum(vecG^2))
#
#     probBeta <- logistic(t(x[seq(1:(length(x) - 1))]) %*%  G )
#
#     #logLikelihood Hessian | d Fn/d epsilon
#     H <- t(t(rep(NA, m + 1))) %*% rep(NA, m + 1)
#
#     for(i in seq(1:(m + 1))) {
#       for(j in seq(1:(m + 1))) {
#
#         H[i,j] <- sum(G[j,] * G[i,] * probBeta * (1 - probBeta),
#                       na.rm = T)
#
#       }
#     }
#
#     J_F1 <- cbind(H, vecG)
#
#     #Gradient of logLikelihood
#     J_F2 <- c(t(G %*% t(data$o - probBeta)), 0)
#
#     return(rbind(J_F1, J_F2))
#   }

  objFun <- function(beta) {

    g^seq(0, m) %*% beta

  }

  gradObjFun<-function(beta) {

    return((g^seq(0, m)))

  }

  qCalibDistr <- function(x) {

    return( givitiStatCdf(x, m, devel = devel, thres) - cLevel)

  }

  #Warning handler
  W <- NULL
  w.handler <- function(w){
    W <<- w
    invokeRestart("muffleWarning")
  }

  # END local functions **************************************************

  if(devel == "external"){

    inverseCumulativeStart <- (m - 1) * qchisq(thres, 1)

  }

  if(devel == "internal"){

    inverseCumulativeStart <- (m - 2) * qchisq(thres, 1)

    firstTrial<-try(givitiStatCdf(inverseCumulativeStart, m,
                                  devel = devel, thres), silent = T)

    if(is.character(firstTrial)) {

      inverseCumulativeStart <- inverseCumulativeStart + 0.0001

    }

  }



  calibK <- withCallingHandlers(uniroot(qCalibDistr, c(inverseCumulativeStart, 40))$root,
                                warning = w.handler)

  betaML <- coef(fit)

  logLikOpt<-as.numeric(logLik(fit))

  rhoLim<-10 * max(sqrt(eigen(vcov(fit))$values))

  thresholdLogLik <- ( - calibK / 2 + logLikOpt)

  G <- sapply(data$logite, FUN="^", seq(from = 0, to = m))

  lowerBound <- NULL
  upperBound <- NULL

  #Good starting conditions
  g <- seqG[1]
  grad <- sapply(g, FUN = "^", seq(from = 0, to = m))
  gradNorm <- grad / sqrt(sum(grad^2))

  rhoMin <- withCallingHandlers(uniroot(logLikelihoodRhoMin, c(0, rhoLim))$root,
                                warning = w.handler)
  rhoMax <- withCallingHandlers(uniroot(logLikelihoodRhoMax, c(0, rhoLim))$root,
                                warning = w.handler)

  betaBoundMin <- betaML + rhoMin * (-gradNorm)
  betaBoundMax <- betaML + rhoMax * gradNorm

  epsilonMin <- sqrt(sum(jacLogLikelihood(betaBoundMin)^2))
  epsilonMax <- (-sqrt(sum(jacLogLikelihood(betaBoundMax)^2)))

  parMin <- c(betaBoundMin, epsilonMin)
  parMax <- c(betaBoundMax, epsilonMax)

  for(g in seqG) {

    p <- logistic(g)

    grad <- sapply(g, FUN="^", seq(from = 0, to = m))
    gradNorm <- grad / sqrt(sum(grad^2))

    parMinTemp <- try(rootSolve::multiroot(f = Fn,
                                           start = parMin,
                                           jacfunc = JacobFn,
                                           rtol=1e-2, ctol=1e-2, atol=1e-2,
                                          useFortran = F)$root,
                      silent = T)

    if(is.character(parMinTemp)){

      betaMin <- alabama::constrOptim.nl(par = as.vector(betaML),
                                 fn = objFun,
                                 gr = gradObjFun,
                                 heq = logLikelihood,
                                 heq.jac =jacLogLikelihood,
                                 control.outer = list(eps = 1e-5, trace = FALSE))$par

      parMin <- c(betaMin, sqrt(sum(jacLogLikelihood(betaMin)^2)))

    } else {

      betaMin <- parMinTemp[1:(m + 1)]

      parMin <- parMinTemp

    }

    parMaxTemp <- try(rootSolve::multiroot(f = Fn,
                                start = parMax,
                                jacfunc = JacobFn,
                                rtol = 1e-2, ctol = 1e-2, atol = 1e-2,
                                useFortran = F)$root,
                      silent = T)

    if(is.character(parMaxTemp)) {

      betaMax <- alabama::constrOptim.nl(par = as.vector(betaML),
                                fn = function(x) {return(-objFun(x))},
                                gr = function(x) {return(-gradObjFun(x))},
                                heq = logLikelihood,
                                heq.jac = jacLogLikelihood,
                                control.outer = list(eps = 1e-5, trace = FALSE))$par

      parMax<-c(betaMax, sqrt(sum(jacLogLikelihood(betaMax)^2)))

    } else {

      betaMax <- parMaxTemp[1:(m + 1)]

      parMax <- parMaxTemp

    }

    betaBisector <- rep(0,length(betaML))
    betaBisector[2]<-1

    incongruence_TestVSBelt <- ((logistic(sum(grad * betaMin)) > p |
                                 logistic(sum(grad * betaMax)) < p)
                                & logLikelihood(betaBisector) > 0.01)

    # If incongruence, break the loop and start a new
    # slower but more precise algorithm

    if(incongruence_TestVSBelt) {
      break #-> and start the more precise algorithm
    }

    if(sum(abs(parMin - parMax) / abs(parMin)) < 0.01) {

      betaMin <- alabama::constrOptim.nl(par = as.vector(betaML),
                                fn = objFun,
                                gr = gradObjFun,
                                heq = logLikelihood,
                                heq.jac = jacLogLikelihood,
                                control.outer = list(eps = 1e-5, trace = FALSE))$par

      parMin <- c(betaMin, sqrt(sum(jacLogLikelihood(betaMin)^2)))

      betaMax <- alabama::constrOptim.nl(par = as.vector(betaML),
                                fn =  function(x) {return(-objFun(x))},
                                gr = function(x) {return(-gradObjFun(x))},
                                heq = logLikelihood,
                                heq.jac =jacLogLikelihood,
                                control.outer = list(eps = 1e-5, trace = FALSE))$par

      parMax <- c(betaMax, sqrt(sum(jacLogLikelihood(betaMax)^2)))

    }

    pointLowerBound <- logistic(sum(grad * betaMin))
    lowerBound <- c(lowerBound, pointLowerBound)

    pointUpperBound <- logistic(sum(grad * betaMax))
    upperBound <- c(upperBound, pointUpperBound)

  }

  # -> SLOWER, MORE PRECISE ALGORITHM
  if (incongruence_TestVSBelt) {

    lowerBound<-NULL
    upperBound<-NULL

    parMin <- c(betaBoundMin, epsilonMin)
    parMax <- c(betaBoundMax, epsilonMax)

    for(g in seqG) {

      p <- logistic(g)

      grad <- sapply(g, FUN = "^", seq(from = 0, to = m))
      gradNorm <- grad / sqrt(sum(grad^2))

      betaMin <- alabama::constrOptim.nl(par = as.vector(betaML),
                                fn = objFun,
                                gr = gradObjFun,
                                heq = logLikelihood,
                                heq.jac =jacLogLikelihood,
                                control.outer = list(eps = 1e-5, trace = FALSE))$par

      parMin <- c(betaMin, sqrt(sum(jacLogLikelihood(betaMin)^2)))

      betaMax <- alabama::constrOptim.nl(par = as.vector(betaML),
                                fn =  function(x) {return(-objFun(x))},
                                gr = function(x) {return(-gradObjFun(x))},
                                heq = logLikelihood,
                                heq.jac = jacLogLikelihood,
                                control.outer = list(eps = 1e-5, trace = FALSE))$par

      parMax <- c(betaMax, sqrt(sum(jacLogLikelihood(betaMax)^2)))

      pointLowerBound <- logistic(sum(grad * betaMin))
      lowerBound <- c(lowerBound, pointLowerBound)

      pointUpperBound <- logistic(sum(grad * betaMax))
      upperBound <- c(upperBound, pointUpperBound)

    }

  }

  cbBound <- data.frame(L = lowerBound, U = upperBound)

  return(cbBound)

}

#Intersections of Calibration belt with bisector --------------------------
#' Calibration Belt Significant Deviations
#'
#' \code{calibrationBeltIntersections} returns the
#'  intervals where the calibration belt significantly deviates
#'  from the bisector.
#' @param cbBound A \code{data.frame} object with the numeric variables "U"
#'  and "L", representing the upper and lower boundary of the calibration belt.
#' @param seqP The vector of the the probabilities where the points
#'  of the calibration belt have been evaluated.
#' @param minMax A list with two elements, named \code{min} and \code{max},
#' representing the minimum and maximum probabilities in the model under evaluation.
#' @return A list with two components, \code{overBisector} and \code{underBisector}.
#' Each component is a list containing all the intervals where the calibration
#' belt is significantly over/under the bisector.
#' @seealso \code{\link{givitiCalibrationBelt}} and \code{\link{plot.givitiCalibrationBelt}}
#'  to compute and plot the calibaration belt, and
#'  \code{\link{givitiCalibrationTest}} to perform the
#'  associated calibration test.
#' @examples
#' e <- runif(1000)
#' logite <- logit(e)
#' eMod <- logistic(logit(e) +  (logit(e))^2)
#' o <- rbinom(1000, size = 1, prob = eMod)
#' data <- data.frame(e = e, o = o, logite = logite)
#'
#' seqP <- seq(from = .01, to =.99, by = .01)
#' seqG <- logit(seqP)
#'
#' minMax <- list(min = min(e), max = max(e))
#'
#' fwLR <- polynomialLogRegrFw(data, .95, 4, 1)
#' cbBound <- calibrationBeltPoints(data, seqG, fwLR$m, fwLR$fit, .95, .90, "external")
#' calibrationBeltIntersections(cbBound, seqP, minMax)
calibrationBeltIntersections <- function(cbBound, seqP, minMax) {

  #START local functions ---------------------------------------------------

  borderInterval <- function(bound,intersection) {

    return((bound[c(FALSE, intersection)] * seqP[c(intersection,FALSE)] -
            seqP[c(FALSE,intersection)] * bound[c(intersection,FALSE)]) /
            ((seqP[c(intersection,FALSE)] - seqP[c(FALSE, intersection)]) -
            (bound[c(intersection,FALSE)]-bound[c(FALSE,intersection)])))

  }

  #END local functions ---------------------------------------------------

  lowerBound <- cbBound$L
  upperBound <- cbBound$U

  numPoints <- nrow(cbBound)

  intersections <- list(overBisector = list(),
                        underBisector = list())

  if( sum(cbBound$L > seqP) == numPoints ) {

    intersections$overBisector[[1]] <- c(minMax$min, minMax$max)

  }

  if ( sum(cbBound$U < seqP) == numPoints ) {

    intersections$underBisector[[1]] <- c(minMax$min, minMax$max)

  }

  if( sum(cbBound$L > seqP) != numPoints &
      sum(cbBound$U < seqP) != numPoints) {

    from2toEnd<- seq(2, numPoints)
    from1ToEndMin1<- seq(1, numPoints-1)

    intersectionLowBoundInc <- (lowerBound[from2toEnd] > seqP[from2toEnd] &
                                lowerBound[from1ToEndMin1] < seqP[from1ToEndMin1])

    intersectionLowBoundDec <- (lowerBound[from2toEnd] < seqP[from2toEnd] &
                                lowerBound[from1ToEndMin1] > seqP[from1ToEndMin1])

    intersectionUppBoundInc <- (upperBound[from2toEnd] > seqP[from2toEnd] &
                                upperBound[from1ToEndMin1] < seqP[from1ToEndMin1])

    intersectionUppBoundDec <- (upperBound[from2toEnd] < seqP[from2toEnd] &
                                upperBound[from1ToEndMin1] > seqP[from1ToEndMin1])

    startLowerBoundOver <- borderInterval(lowerBound, intersectionLowBoundInc)
    startLowerBoundUnder <- borderInterval(lowerBound, intersectionLowBoundDec)
    startUpperBoundOver <- borderInterval(upperBound, intersectionUppBoundInc)
    startUpperBoundUnder <- borderInterval(upperBound, intersectionUppBoundDec)

    if(length(startLowerBoundOver) == 0 & length(startLowerBoundUnder) != 0) {

      intersections$overBisector[[1]] <- c(minMax$min, startLowerBoundUnder[1])

    }

    if(length(startLowerBoundOver) != 0 & length(startLowerBoundUnder) == 0) {

      intersections$overBisector[[1]] <- c(startLowerBoundOver[1], minMax$max)

    }

    if(length(startLowerBoundOver) != 0 & length(startLowerBoundUnder) != 0) {

      if(startLowerBoundOver[1] > startLowerBoundUnder[1]) {

        startLowerBoundOver <- c(minMax$min, startLowerBoundOver)

      }

      if( length(startLowerBoundOver) != length(startLowerBoundUnder)) {

        startLowerBoundUnder <- c(startLowerBoundUnder, minMax$max)

      }

      intersections$overBisector[seq(from = 1,
                   to = length(startLowerBoundUnder))] <- mapply(startLowerBoundOver,
                                                          startLowerBoundUnder,
                                                          FUN=c,
                                                          SIMPLIFY=F)

    }

    if(length(startUpperBoundOver) == 0 & length(startUpperBoundUnder) != 0) {

      intersections$underBisector[[1]] <- c(startUpperBoundUnder[1], minMax$max)

    }

    if(length(startUpperBoundOver) != 0 & length(startUpperBoundUnder) == 0) {

      intersections$underBisector[[1]] <- c(minMax$min, startUpperBoundOver[1])

    }

    if(length(startUpperBoundOver) !=0 & length(startUpperBoundUnder) != 0) {

      if(startUpperBoundUnder[1] > startUpperBoundOver[1]) {

        startUpperBoundUnder<-c(minMax$min,startUpperBoundUnder)

      }

      if(length(startUpperBoundOver) != length(startUpperBoundUnder)) {

        startUpperBoundOver<-c(startUpperBoundOver,minMax$max)

      }
      intersections$underBisector[seq(from = 1,
                   to = length(startUpperBoundUnder))] <- mapply(startUpperBoundUnder,
                                                          startUpperBoundOver,
                                                          FUN=c,
                                                          SIMPLIFY=F)

    }

  }

  return(intersections)
}
