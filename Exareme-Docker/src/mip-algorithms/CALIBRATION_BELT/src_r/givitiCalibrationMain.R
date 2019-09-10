#Compute the calibration test ---------------------------------------------
#' Computation of the Calibration Test
#'
#' \code{givitiCalibrationTestComp} implements the computations necessary to
#' perform the calibration test associated to the calibration belt.
#'
#' @param o A numeric vector representing the binary outcomes.
#'  The elements must assume only the values 0 or 1. The predictions
#'  in \code{e} must represent the probability of the event
#'  coded as 1.
#' @param e A numeric vector containing the probabilities of the
#'  model under evaluation. The elements must be numeric and between 0 and 1.
#'  The lenght of the vector must be equal to the length of the vector \code{o}.
#' @param devel A character string specifying if the model has been fit on
#'  the same dataset under evaluation (\code{internal}) or if the model has
#'  been developed on an external sample (\code{external}). See also the 'Details' sections.
#' @param thres A numeric scalar between 0 and 1 representing 1 - the significance level
#'  adopted in the forward selection.
#' @param maxDeg The maximum degree considered in the forward selection.
#' @return A list containing the following components:
#' \describe{
#'   \item{data}{A \code{data.frame} object with the numeric variables "o", "e" provided
#'               in the input and the variable "logite", the logit of the probabilities.}
#'   \item{nrowOrigData}{The size of the original sample, i.e. the length of the
#'                       vectors \code{e} and \code{o}.}
#'   \item{calibrationStat}{The value of the test's statistic.}
#'   \item{calibrationP}{The p-value of the test.}
#'   \item{m}{The degree of the polynomial at the end of the forward selection.}
#'   \item{fit}{An object of class \code{glm} containig the output of the fit
#'              of the logistic regression model at the end of the iterative
#'              forward selection.}
#' }
#' @details The calibration belt and the associated test can be used both to evaluate
#' the calibration of the model in external samples or in the development dataset. However,
#' the two cases have different requirements. When a model is evaluated on independent
#' samples, the calibration belt and the related test can be applied whatever is the
#' method used to fit the model. Conversely, they can be used on the development set
#' only if the model is fitted with logistic regression.
#' @seealso \code{\link{givitiCalibrationBelt}} and \code{\link{plot.givitiCalibrationBelt}}
#'  to compute and plot the calibaration belt, and
#'  \code{\link{givitiCalibrationTest}} to perform the
#'  associated calibration test.
#' @examples
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = e)
#' givitiCalibrationTestComp(o, e, "external", .95, 4)
givitiCalibrationTestComp <- function(o, e, devel, thres, maxDeg) {

  data <- data.frame(e = e,o = o)

  nrowOrigData <- nrow(data)

  data <- na.omit(data)
  data$logite <- logit(data$e)

  #Development sample external: start from 1st degree;
  #Development sample internal: start from 2nd degree;

  if(devel == "external"){

    startDeg <- 1

  }

  if(devel == "internal"){

    startDeg <- 2

  }

  #Best model selection (m and fit)
  resultPolyLogRegrFw <- polynomialLogRegrFw(data, thres, maxDeg, startDeg)

  m <- resultPolyLogRegrFw$m
  fit <- resultPolyLogRegrFw$fit


  #GiViTI Calibration Test
  logLikBisector <- sum((1 - data$o) * log(1 - data$e) + data$o * log(data$e))

  calibrationStat <- 2 * (as.numeric(logLik(fit)) - logLikBisector)

  calibrationP <- 1 - givitiStatCdf(calibrationStat, m, devel, thres)

  return(list(data = data,
              nrowOrigData = nrowOrigData,
              calibrationStat = calibrationStat,
              calibrationP = calibrationP,
              m = m,
              fit = fit))
}


#Check the value of arguments ---------------------------------------------
#' Check of the argument's values
#'
#' Check of the coherence of the values passed to the functions
#' \code{givitiCalibrationTest} and \code{givitiCalibrationBelt}.
#'
#' @param o A numeric vector representing the binary outcomes.
#'  The elements must assume only the values 0 or 1. The predictions
#'  in \code{e} must represent the probability of the event
#'  coded as 1.
#' @param e A numeric vector containing the probabilities of the
#'  model under evaluation. The elements must be numeric and between 0 and 1.
#'  The lenght of the vector must be equal to the length of the vector \code{o}.
#' @param devel A character string specifying if the model has been fit on
#'  the same dataset under evaluation (\code{internal}) or if the model has
#'  been developed on an external sample (\code{external}).
#' @param thres A numeric scalar between 0 and 1 representing 1 - the significance level
#'  adopted in the forward selection.
#' @param maxDeg The maximum degree considered in the forward selection.
#' @return The function produce an error if the elements provided
#'  through the arguments do not meet the constraints reported.
givitiCheckArgs <- function(o, e, devel, thres, maxDeg) {

  if(length(o) != length(e)) {
    stop(" The vectors 'e' and 'o' have different length. ")
  }

  if(!is.numeric(o) | (!sum(o %in% c(0,1) , na.rm=T) > 0)) {
    stop(" The vector 'o' must be a numeric vectors with 0/1 values. ")
  }

  if(!is.numeric(e) | sum(e<=0 | e>=1, na.rm=T) > 0) {
    stop(" The vector 'e' must be a numeric vectors with values in (0,1). ")
  }

  if (!devel %in% c("internal","external")) {
    stop(" The 'devel' argument must be either 'internal' or external'")
  }

  if (thres < 0 | thres > 1 | !is.numeric(thres)) {
    stop(" The argument 'thres' must be a number in [0,1]")
  }

  if( abs(sum(e[!is.na(e) & !is.na(o)],na.rm=T) -
           sum(o[!is.na(e) & !is.na(o)],na.rm=T)) < 1e-4 &
      devel %in% "external") {
    warning("If the model evaluated has been fitted on this dataset, you must define \"devel = 'internal'\" ")
  }

  return(TRUE)
}

#Check problems in data ------------------------------------------------
#' Check of data
#'
#' The function verifies that the data are compatible with the
#' construction of the calibration belt. In particular,
#' the function checks that the predictions provided do not
#' complete separate the outcomes and that at least two events and non-events
#' are present in the data.
#'
#' @param o A numeric vector representing the binary outcomes.
#'  The elements must assume only the values 0 or 1. The predictions
#'  in \code{e} must represent the probability of the event
#'  coded as 1.
#' @param e A numeric vector containing the probabilities of the
#'  model under evaluation. The elements must be numeric and between 0 and 1.
#'  The lenght of the vector must be equal to the length of the vector \code{o}.
#' @return The output is \code{TRUE} if the data do not show any of the
#'  reported problems. Otherwise, the function returns a string describing the
#'  problem found.
givitiCheckData <- function(o, e) {

  data <- data.frame(e = e,o = o)
  data <- na.omit(data)
  o <- data$o
  e <- data$e

  dataSeparation<-(sum(o[order(e)] ==
                      sort(o), na.rm = T) == length(o))

  noOucomeVariation<-( (sum(o) <= 1) |
                       (sum(o) >= (length(o) - 1)))

  resultCheck <- NULL

  if(dataSeparation) {

    resultCheck <- " of the complete separation of the data"

  }

  if(noOucomeVariation) {

    resultCheck <- " the number of events/non events is less than 1."

  }

  return(resultCheck)

}

#Plot the table of intersection on the graph ------------------
#' Table of the Calibration Belt Significant Deviations
#'
#' \code{givitiCalibrationBeltTable} prints on the graphical area of the calibration
#'  belt plot the table that summarizes the significant deviations from the
#'  line of perfect calibration (i.e. the bisector of the I quadrant).
#'
#' @param cb A \code{givitiCalibrationBelt} object, to be generated with
#'  the function \code{givitiCalibrationBelt}.
#' @param tableStrings Optional. A list with four character elements named
#'  \code{overBisString},\code{underBisString},\code{confLevelString},
#'  \code{neverString}. The four strings of the list are printed instead of the
#'  texts "Over the bisector"/"Under the bisector"/"Confidence level"/"NEVER"
#'  in the table reporting the intersections of the calibration belt with
#'  the bisector.
#' @param xlim,ylim Numeric vectors of length 2, giving the
#'  x and y coordinates ranges. Default values are \code{c(0,1)}.
#' @param grayLevels A vector containing the code of the
#'  gray levels used in the plot of the calibration belt.
#' @return The function prints the table on the graphical area.
givitiCalibrationBeltTable <- function(cb,
                                       tableStrings,
                                       grayLevels,
                                       xlim,
                                       ylim){



  if(is.null(tableStrings)) {

    confLevelString <- "Confidence level"
    underBisString <- "Under the bisector"
    overBisString <- "Over the bisector"
    neverString <- "NEVER"

  } else {

    confLevelString <- tableStrings$confLevelString
    underBisString <- tableStrings$underBisString
    overBisString <- tableStrings$overBisString
    neverString <- tableStrings$neverString

  }

  confLevelString <- sub(" ", "\n", confLevelString)
  overBisString <- sub(" ", "\n", overBisString)
  underBisString <- sub(" ", "\n", underBisString)

  fromBottom <- ylim[1]

  arrows(xlim[1] + 0.4 * (xlim[2] - xlim[1]),
         fromBottom - 0.025 * (ylim[2] - ylim[1]),
         xlim[2],
         fromBottom - 0.025 * (ylim[2] - ylim[1]), length = 0)

  for(i in c(1 : length(cb$confLevels))) {

    intersI <- cb$intersByConfLevel[[i]]

    maxIntervalsI <- max(c(length(intersI$overBisector),
                           length(intersI$underBisector),
                           1))

    heightRow <- 0.05 * ( maxIntervalsI - 1 ) * (ylim[2] - ylim[1])

    yConfLevel <-(fromBottom + heightRow / 2 )

    text(x = xlim[1] + 0.5 * (xlim[2] - xlim[1]),
         y = yConfLevel,
         labels = paste(cb$confLevels[i]*100, "%", sep = ""),
         cex=.85)

    polygon(c(xlim[1] + 0.45 * (xlim[2] - xlim[1]),
              xlim[1] + 0.42 * (xlim[2] - xlim[1]),
              xlim[1] + 0.42 * (xlim[2] - xlim[1]),
              xlim[1] + 0.45 * (xlim[2] - xlim[1])),
            c(yConfLevel - 0.015 * (ylim[2] - ylim[1]),
              yConfLevel - 0.015 * (ylim[2] - ylim[1]),
              yConfLevel + 0.015 * (ylim[2] - ylim[1]),
              yConfLevel + 0.015 * (ylim[2] - ylim[1])),
            border = gray(grayLevels[i]), col = gray(grayLevels[i]))

    if(length(intersI$overBisector) >= 1) {

      if(length(intersI$overBisector) == maxIntervalsI) {

        for(iIntOver in seq(from = 1, to = length(intersI$overBisector))){

          text(x = xlim[1] + 0.9 * (xlim[2] - xlim[1]),
               y =  fromBottom + heightRow - (iIntOver - 1) * 0.05 * (ylim[2] - ylim[1]),
               labels = paste(sprintf("%.2f",
                                      intersI$overBisector[[iIntOver]]), collapse = " - "),
               cex=.85)
        }

      } else {

        spaceBetweenRows <- heightRow / (length(intersI$overBisector) + 1)

        for(iIntOver in seq(from = 1, to = length(intersI$overBisector))){

          text(x = xlim[1] + 0.9* (xlim[2] - xlim[1]),
               y =  fromBottom + heightRow - iIntOver * spaceBetweenRows,
               labels = paste(sprintf("%.2f",
                                      intersI$overBisector[[iIntOver]]), collapse = " - "),
               cex=.85)
        }


      }


    } else {

      text(x = xlim[1] + 0.9 * (xlim[2] - xlim[1]),
           y = yConfLevel,
           labels = neverString,
           cex=.85)

    }

    if(length(intersI$underBisector) >= 1) {

      if(length(intersI$underBisector) == maxIntervalsI) {

        for(iIntUnder in seq(from = 1, to = length(intersI$underBisector))){

          text(x = xlim[1] + 0.7 * (xlim[2] - xlim[1]),
               y =  fromBottom + heightRow - (iIntUnder - 1) * 0.05 * (ylim[2] - ylim[1]),
               labels = paste(sprintf("%.2f",
                                      intersI$underBisector[[iIntUnder]]), collapse = " - "),
               cex=.85)
        }

      } else {

        spaceBetweenRows <- heightRow / (length(intersI$underBisector) + 1)

        for(iIntUnder in seq(from = 1, to = length(intersI$underBisector))){

          text(x = xlim[1] + 0.7 * (xlim[2] - xlim[1]),
               y =  fromBottom + heightRow - iIntUnder * spaceBetweenRows,
               labels = paste(sprintf("%.2f",
                                      intersI$underBisector[[iIntUnder]]), collapse = " - "),
               cex=.85)
        }
      }

    } else {

      text(x = xlim[1] + 0.7 * (xlim[2] - xlim[1]),
           y = yConfLevel,
           labels = neverString,
           cex=.85)

    }

  fromBottom <- fromBottom + heightRow + 0.05 * (ylim[2] - ylim[1])

  arrows(xlim[1] + 0.4 * (xlim[2] - xlim[1]),
         fromBottom - 0.025 * (ylim[2] - ylim[1]),
         xlim[2],
         fromBottom - 0.025 * (ylim[2] - ylim[1]), length=0)

  }

  text(x = c(xlim[1] + 0.5 * (xlim[2] - xlim[1]),
             xlim[1] + 0.7 * (xlim[2] - xlim[1]),
             xlim[1] + 0.9 * (xlim[2] - xlim[1])),
       y = fromBottom + 0.02 * (ylim[2] - ylim[1]),
       labels = c(confLevelString, underBisString, overBisString),
       cex=.7)

  return(TRUE)
}

#Generate 'htest' object of the calibration test -------------------------
#' Calibration Test
#'
#' \code{givitiCalibrationTest} performs the calibration test associated to the
#' calibration belt.
#'
#' @param o A numeric vector representing the binary outcomes.
#'  The elements must assume only the values 0 or 1. The predictions
#'  in \code{e} must represent the probability of the event
#'  coded as 1.
#' @param e A numeric vector containing the probabilities of the
#'  model under evaluation. The elements must be numeric and between 0 and 1.
#'  The lenght of the vector must be equal to the length of the vector \code{o}.
#' @param devel A character string specifying if the model has been fit on
#'  the same dataset under evaluation (\code{internal}) or if the model has
#'  been developed on an external sample (\code{external}). See also the 'Details' section.
#' @param subset An optional boolean vector specifying the subset of observations
#' to be considered.
#' @param thres A numeric scalar between 0 and 1 representing 1 - the significance level
#'  adopted in the forward selection. By default is set to 0.95.
#' @param maxDeg The maximum degree considered in the forward selection.
#'  By default is set to 4.
#' @return A list of class \code{htest} containing the following components:
#' \describe{
#'   \item{statistic}{The value of the test's statistic.}
#'   \item{p.value}{The p-value of the test.}
#'   \item{null.value}{The vector of coefficients hypothesized under the null hypothesis,
#'                     that is, the parameters corresponding to the bisector.}
#'   \item{alternative}{A character string describing the alternative hypothesis.}
#'   \item{method}{A character string indicating what type of calibration
#'                 test (internal or external) was performed.}
#'   \item{estimate}{The estimate of the coefficients of the polynomial logistic
#'                    regression.}
#'   \item{data.name}{A character string giving the name(s) of the data.}
#' }
#' @details The calibration belt and the associated test can be used both to evaluate
#' the calibration of the model in external samples or in the development dataset. However,
#' the two cases have different requirements. When a model is evaluated on independent
#' samples, the calibration belt and the related test can be applied whatever is the
#' method used to fit the model. Conversely, they can be used on the development set
#' only if the model is fitted with logistic regression.
#' @seealso \code{\link{givitiCalibrationBelt}} and \code{\link{plot.givitiCalibrationBelt}}
#'  to compute and plot the calibaration belt.
#' @examples
#'
#' #Random by-construction well calibrated model
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = e)
#' givitiCalibrationTest(o, e, "external")
#'
#' #Random by-construction poorly calibrated model
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = logistic(logit(e)+2))
#' givitiCalibrationTest(o, e, "external")
givitiCalibrationTest <- function(o, e, devel,
                                  subset = NULL,
                                  thres = .95,
                                  maxDeg = 4) {

  givitiCheckArgs(o, e, devel, thres, maxDeg)

  if(!is.null(subset)) {

    o <- o[subset]
    e <- e[subset]

  }

  resultCheck <- givitiCheckData(o, e)

  if(!is.null(resultCheck)) {

    stop(paste("The calibration test cannot be computed because",
               resultCheck, sep = ""))

  }

  resultTest <- givitiCalibrationTestComp(o, e, devel, thres, maxDeg)

  m <- resultTest$m

  nullValue <- rep(0, m + 1)
  nullValue[2] <- 1


  names(nullValue) <- paste("beta", seq(from = 0, to = m), sep="")

  estimate <- as.vector(coef(resultTest$fit))
  names(estimate) <- paste("beta", seq(from = 0, to = m), sep="")

  parameter<- m
  names(parameter)<-"m"

  statistic <- resultTest$calibrationStat
  names(statistic) <- "Stat"

  method <- paste("GiViTI calibration test - ", devel,
                  " validation", sep ="")

  outputTest <- list(null.value = nullValue,
                     alternative = "two.sided",
                     method = method,
                     estimate = estimate,
                     data.name = "e = 'Predictions' and o = 'Binary outcome'",
                     statistic = statistic,
                     p.value = resultTest$calibrationP)

  class(outputTest) <- "htest"

  return(outputTest)
}


#Compute the calibration belt ---------------------------------------------
#' Calibration Belt
#'
#' \code{givitiCalibrationBelt} implements the computations necessary
#' to plot the calibration belt.
#'
#' @param o A numeric vector representing the binary outcomes.
#'  The elements must assume only the values 0 or 1. The predictions
#'  in \code{e} must represent the probability of the event
#'  coded as 1.
#' @param e A numeric vector containing the predictions of the
#'  model under evaluation. The elements must be numeric and between 0 and 1.
#'  The lenght of the vector must be equal to the length of the vector \code{o}.
#' @param devel A character string specifying if the model has been fit on
#'  the same dataset under evaluation (\code{internal}) or if the model has
#'  been developed on an external sample (\code{external}). See also the 'Details' section.
#' @param subset An optional boolean vector specifying the subset of observations
#'  to be considered.
#' @param confLevels A numeric vector containing the confidence levels
#'  of the calibration belt. The default values are set to .80 and .95.
#' @param thres A numeric scalar between 0 and 1 representing 1 - the significance level
#'  adopted in the forward selection. By default is set to 0.95.
#' @param maxDeg The maximum degree considered in the forward selection.
#'  By default is set to 4.
#' @param nPoints A numeric scalar indicating the number of points to be considered
#'  to plot the calibration belt. The default value is 200.
#' @return An object of class \code{givitiCalibrationBelt}.
#'  After computing the calibration belt with the present function,
#'  the \code{plot} method can be used to plot
#'  the calibration belt. The object returned is a list that contains the
#'  following components:
#' \describe{
#'   \item{n}{The size of the sample evaluated in the analysis, after discarding
#'            missing values from the vectors \code{o} and \code{e}.}
#'   \item{resultCheck}{Result of the check on the data. If the data are compatible with the
#'                      construction of the calibration belt, the value is the boolean
#'                      \code{TRUE}. Otherwise, the element contain a character string
#'                      describing the problem found.}
#'   \item{m}{The degree of the polynomial at the end of the forward selection.}
#'   \item{statistic}{The value of the test's statistic.}
#'   \item{p.value}{The p-value of the test.}
#'   \item{seqP}{The vector of the probabilities where the points
#'               of the calibration belt has been evaluated.}
#'   \item{minMax}{A list with two elements named \code{min} and \code{max}
#'                 representing the minimum and maximum probabilities in the model under evaluation}
#'   \item{confLevels}{The vector containing the confidence levels of the
#'                     calibration belt.}
#'   \item{intersByConfLevel}{A list whose elements report the intervals where the
#'                            calibration belt is significantly over/under the bisector
#'                            for each confidence level in \code{confLevels}.}
#' }
#' @details The calibration belt and the associated test can be used both to evaluate
#' the calibration of the model in external samples or in the development dataset. However,
#' the two cases have different requirements. When a model is evaluated on independent
#' samples, the calibration belt and the related test can be applied whatever is the
#' method used to fit the model. Conversely, they can be used on the development set
#' only if the model is fitted with logistic regression.
#' @seealso \code{\link{plot.givitiCalibrationBelt}}
#'  to plot the calibaration belt and
#'  \code{\link{givitiCalibrationTest}} to perform the
#'  associated calibration test.
#' @examples
#' #Random by-construction well calibrated model
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = e)
#' cb <- givitiCalibrationBelt(o, e, "external")
#' plot(cb)
#'
#' #Random by-construction poorly calibrated model
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = logistic(logit(e)+2))
#' cb <- givitiCalibrationBelt(o, e, "external")
#' plot(cb)
givitiCalibrationBelt <- function(o, e, devel,
                                      subset = NULL,
                                      confLevels = c(.80, .95),
                                      thres = .95, maxDeg = 4,
                                      nPoints = 200) {

  givitiCheckArgs(o, e, devel, thres, maxDeg)

  if(!is.null(subset)) {

    o <- o[subset]
    e <- e[subset]

  }


  resultCheck <- givitiCheckData(o, e)

  if(!is.null(resultCheck)) {

    warning(paste("The calibration belt cannot be computed because",
                   resultCheck, sep = ""))

    outputBelt <- list(n = length(e),
                       resultCheck = resultCheck,
                       m = NA,
                       statistic = NA,
                       p.value = NA,
                       seqP = NA,
                       minMax = data.frame(min = min(e, na.rm = T),
                                           max = max(e, na.rm = T)),
                       confLevels = confLevels,
                       cbBoundByConfLevel = NA,
                       intersByConfLevel = NA)

    class(outputBelt) <- "givitiCalibrationBelt"

    return(outputBelt)
  }


  resultTest <- givitiCalibrationTestComp(o, e, devel, thres, maxDeg)

  data <- resultTest$data
  m <- resultTest$m
  fit <- resultTest$fit

  confLevels <- sort(confLevels, decreasing = TRUE)

  halfEquispLogit <- seq(from = min(data$logite),
                         to = max(data$logite),
                         length = round(nPoints / 2, 0))

  halfEquispProb <- seq(from = min(data$e),
                        to = max(data$e),
                        length = round(nPoints / 2, 0))

  seqG <- sort(c(halfEquispLogit, logit(halfEquispProb)))
  seqP<-logistic(seqG)

  minMax<-list(min = min(data$e), max = max(data$e))

  intersByConfLevel <- list()
  cbBoundByConfLevel <- list()

  for(i in c(1 : length(confLevels))) {

    cbBoundI <- calibrationBeltPoints(data, seqG, m, fit,
                                     thres, confLevels[i], devel)

    intersI <- calibrationBeltIntersections(cbBoundI, seqP, minMax)

    cbBoundByConfLevel[[i]] <- cbBoundI
    intersByConfLevel[[i]] <- intersI

  }

  statistic <- resultTest$calibrationStat
  names(statistic) <- "Stat"

  outputBelt <- list(n = nrow(data),
                     resultCheck = resultCheck,
                     m = m,
                     statistic = statistic,
                     p.value = resultTest$calibrationP,
                     seqP = seqP,
                     minMax = minMax,
                     confLevels = confLevels,
                     cbBoundByConfLevel = cbBoundByConfLevel,
                     intersByConfLevel = intersByConfLevel)

  class(outputBelt) <- "givitiCalibrationBelt"

  return(outputBelt)
}


#Plot the calibration belt ---------------------------------------------
#' Calibration Belt Plot
#'
#' The \code{plot} method for calibration belt objects.
#'
#' @param x A \code{givitiCalibrationBelt} object, to be generated with
#'  the function \code{givitiCalibrationBelt}.
#' @param xlim,ylim Numeric vectors of length 2, giving the
#'  x and y coordinates ranges. Default values are \code{c(0,1)}.
#' @param colBis The color to be used for the bisector. The default
#'  value is red.
#' @param xlab,ylab Titles for the x and y axis. Default values are "e" and
#'  "o", repectively.
#' @param main The main title of the plot. The default value is "GiViTI Calibration Belt".
#' @param polynomialString If the value is FALSE, the degree of the polynomial is
#'  not printed on the graphical area. If the value is TRUE, the degree m is reported.
#'  If a string is passed to this argument, the string is reported instead of the
#'  text "Polynomial degree". The default value is TRUE.
#' @param pvalueString If the value is FALSE, the p-value of the test is
#'  not printed on the graphical area. If the value is TRUE, the p-value is reported.
#'  If a string is passed to this argument, the string is reported instead of the
#'  text "p-value". The default value is TRUE.
#' @param nString If the value is FALSE, the sample size is
#'  not printed on the graphical area. If the value is TRUE, the sample size is reported.
#'  If a string is passed to this argument, the string is reported instead of the
#'  text "n". The default value is TRUE.
#' @param table A boolean value indicating whether the table reporting the
#'  intersections of the calibration belt with the bisector should be
#'  printed on the plot.
#' @param tableStrings Optional. A list with four character elements named
#'  \code{overBisString},\code{underBisString},\code{confLevelString},
#'  \code{neverString}. The four strings of the list are printed instead of the
#'  texts "Over the bisector"/"Under the bisector"/"Confidence level"/"NEVER"
#'  in the table reporting the intersections of the calibration belt with
#'  the bisector.
#' @param unableToFitString Optional. If a string is passed to this argument,
#'  this string is reported in the plot area when the dataset is not compatible
#'  with the fit of the calibration belt (e.g. data separation or no positive events).
#'  By default, in such cases the text "Unable to fit the Calibration Belt" is
#'  reported.
#' @param ... Other graphical parameters passed to the generic \code{plot} method.
#' @return The function generates the calibration belt plot. In addition,
#' a list containing the following components is returned:
#' \describe{
#'   \item{p.value}{The p-value of the test.}
#'   \item{m}{The degree of the polynomial at the end of the forward selection.}
#' }
#' @seealso \code{\link{givitiCalibrationBelt}}
#'  to compute the calibaration belt and
#'  \code{\link{givitiCalibrationTest}} to perform the
#'  associated calibration test.
#' @examples
#'
#' #Random by-construction well calibrated model
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = e)
#' cb <- givitiCalibrationBelt(o, e, "external")
#' plot(cb)
#'
#' #Random by-construction poorly calibrated model
#' e <- runif(100)
#' o <- rbinom(100, size = 1, prob = logistic(logit(e)+2))
#' cb <- givitiCalibrationBelt(o, e, "external")
#' plot(cb)
plot.givitiCalibrationBelt <- function(x,
                                      xlim = c(0, 1), ylim = c(0, 1),
                                      colBis = "red",
                                      xlab = "e", ylab = "o",
                                      main = "GiViTI Calibration Belt",
                                      polynomialString = T,
                                      pvalueString = T,
                                      nString = T,
                                      table = T,
                                      tableStrings = NULL,
                                      unableToFitString = NULL,
                                      ...) {

  cb <- x

  if(class(cb) != "givitiCalibrationBelt"){
    stop("The 'cb' argument must be an object of 'givitiCalibrationBelt' class")
  }


  #frame();
  #plot.window(xlim = xlim, ylim = ylim , ...)
  #box()
  #title(xlab = xlab, ylab = ylab, main = main)
  #axis(1); axis(2);

  plot(NULL, xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = ylab, main = main, ...)

  if(!is.null(cb$resultCheck)) {

    if(is.null(unableToFitString)) {

        labelUnableToFit <- "Unable to fit the Calibration Belt"

    } else {

        labelUnableToFit <- unableToFitString
    }

    text(xlim[1] + 0.18 * (xlim[2] - xlim[1]),
         ylim[1] + 0.5* (ylim[2] - ylim[1]),
         labels = labelUnableToFit,
         adj = 0)

    return(list(m = cb$m, p.value = cb$p.value))

  }

  grayLevels<-seq(0.5, 0.8, length.out = length(cb$confLevels))

  seqP <- cb$seqP

  for(i in c(1 : length(cb$confLevels))) {

    cbBoundI <- cb$cbBoundByConfLevel[[i]]

    polygon(c(seqP, seqP[length(seqP) : 1]),
            c(cbBoundI$U, cbBoundI$L[length(cbBoundI$L) : 1]),
            col = gray(grayLevels[i]),
            border = NA)
  }

  lines(c(0, 1), c(0, 1), lwd = 1.75, col = colBis)

  #Writings on the plot area

  if (cb$p.value < .001) {

    pvalueChar <- "<0.001"

  } else {

    pvalueChar <- sprintf("%.3f", cb$p.value)

  }

  fromTop <- 0

  if(!polynomialString == F) {

    if(!is.character(polynomialString)) {

      polynomialString <- "Polynomial degree"

    }

    text(xlim[1], ylim[2] - fromTop , adj = 0,
         labels = paste(polynomialString, ": ", cb$m, sep=""))

    fromTop <- fromTop + 0.05 * (ylim[2] - ylim[1])
  }

  if(!pvalueString == F) {

    if(!is.character(pvalueString)) {

      pvalueString <- "p-value"

    }

    text(xlim[1], ylim[2] - fromTop, adj = 0,
         labels = paste(pvalueString, ": ", pvalueChar, sep=""))

    fromTop <- fromTop + 0.05 * (ylim[2] - ylim[1])
  }

  if(!nString == F) {

    if(!is.character(nString)) {

      nString <- "n"

    }

    text(xlim[1], ylim[2] - fromTop , adj = 0,
         labels = paste(nString, ": ", cb$n, sep=""))

    fromTop <- fromTop + 0.05 * (ylim[2] - ylim[1])
  }


  if (table == T) {

    givitiCalibrationBeltTable(cb,
                               tableStrings,
                               grayLevels,
                               xlim,
                               ylim)

  }

  return(list(m = cb$m, p.value = cb$p.value))
}


