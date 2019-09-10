#' givitiR: assessing the calibration of binary outcome models with the GiViTI calibration belt.
#'
#' The package `givitiR` provides the functions to plot the
#' GiViTI calibration belt and to compute the associated statistical test.
#'
#' The name of the approach derives from the GiViTI (Gruppo Italiano per la valutazione
#' degli interventi in Terapia Intensiva,
#' Italian Group for the Evaluation of the Interventions in Intensive Care Units), an
#' international network of intensive care units (ICU) established in Italy in 1992.
#' The group counts more than 400 ICUs from 7 countries, with about the half of the participating
#' centers continuosly collecting data on the admitted patients through the PROSAFE project (PROmoting patient SAFEty
#' and quality improvement in critical care). For further information, see the
#' package vignette and the references therein.
#'
#' The GiViTI calibration belt has been developed within the methodological research
#' promoted by the GiViTI network, with the purposes of a) enhancing the quality of the
#' logistic regression models built in the group's projects b) providing the participating ICUs
#' with a detailed feedback about their quality of care. A description of the approach
#' and examples of applications are reported in the package vignette.
#'
#' The main functions of the package are listed below.
#'
#' @section Fitting the calibration belt:
#'
#' \code{\link{givitiCalibrationBelt}} implements the computations necessary
#' to plot the calibration belt.
#'
#' @section Plotting the calibration belt:
#' \code{\link{plot.givitiCalibrationBelt}} plots the calibration belt.
#'
#' @section Computing the calibration test:
#'
#' \code{\link{givitiCalibrationTest}} performs the calibration test associated to the
#' calibration belt.
#'
#' @docType package
#' @name givitiR
NULL
