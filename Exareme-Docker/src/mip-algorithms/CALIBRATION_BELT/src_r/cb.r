# Copyright 2019, Stefano Finazzi
# stefano.finazzi@marionegri.it


library( givitiR )
source( 'logisticRegression.r' )
source( 'givitiCalibrationBasic.R')


coef.logisticRegressionForDistributedCB <- function( fit ){
  return( fit$par )
}

vcov.logisticRegressionForDistributedCB <- function( fit ){
  return( solve( fit$hessian ) )
}

logLik.logisticRegressionForDistributedCB <- function( fit ){
  return( fit$logLik )
}

distributedCalibrationBelt <- function(o, e, devel,
                                  subset = TRUE,
                                  server = 'localhost',
                                  port = 9000,
                                  confLevels = c(.80, .95),
                                  thres = .95, maxDeg = 4,
                                  nPoints = 200) {
  
  resultTest <- distributedCalibrationTestComp(o, e, devel, subset, server, port, thres, maxDeg)
  
  dataInfo <- resultTest$dataInfo
  minE <- dataInfo$minE
  maxE <- dataInfo$maxE
  nSample <- dataInfo$n
  m <- resultTest$m
  fit <- resultTest
  
  confLevels <- sort(confLevels, decreasing = TRUE)
  
  halfEquispLogit <- seq(from = logit(minE),
                         to = logit(maxE),
                         length = round(nPoints / 2, 0))
  
  halfEquispProb <- seq(from = minE,
                        to = maxE,
                        length = round(nPoints / 2, 0))
  
  seqG <- sort(c(halfEquispLogit, logit(halfEquispProb)))
  seqP<-logistic(seqG)
  
  minMax<-list(min = minE, max = maxE)
  
  intersByConfLevel <- list()
  cbBoundByConfLevel <- list()
  
  for(i in c(1 : length(confLevels))) {
    
    cbBoundI <- distributedCalibrationBeltPoints(
      server, port,
      dataInfo, seqG, m, fit,
      thres, confLevels[i], devel)
    
    intersI <- calibrationBeltIntersections(cbBoundI, seqP, minMax)
    
    cbBoundByConfLevel[[i]] <- cbBoundI
    intersByConfLevel[[i]] <- intersI
    
  }
  
  statistic <- resultTest$calibrationStat
  names(statistic) <- "Stat"
  
  outputBelt <- list(n = nSample,
                     # resultCheck = resultCheck,
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



distributedCalibrationTestComp <- function(o, e, devel, subset, server, port, thres, maxDeg) {
  
  if(devel == "external"){
    
    startDeg <- 1
    
  }
  
  if(devel == "internal"){
    
    startDeg <- 2
    
  }
  
  #Best model selection (m and fit)
  resultPolyLogRegrFw <- distributedPolynomialLogRegrFw(o, e, subset, server, port, thres, maxDeg, startDeg)
  
  m <- resultPolyLogRegrFw$m
  fit <- resultPolyLogRegrFw
  dataInfo <- resultPolyLogRegrFw$dataInfo
  
  
  
  #GiViTI Calibration Test
  logLikBisector <- getFunctionResults(
    server, port,
    "partiallogLikelihood",
    o = o, e = e,
    subset = subset,
    beta = c( 0, 1 )
  )$logLik
  
  calibrationStat <- 2 * (fit$logLik - logLikBisector )
  
  calibrationP <- 1 - givitiStatCdf(calibrationStat, m, devel, thres)
  
  fit$calibrationStat <- calibrationStat
  fit$calibrationP <- calibrationP
  
  return( fit )
}



distributedPolynomialLogRegrFw<-function(o, e, subset, server, port, thres, maxDeg, startDeg) {
  
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
  
  n <- startDeg
  while (n <= maxDeg){
    
      fitNew <- withCallingHandlers(
      logisticRegressionForDistributedCB( o = o, e = e, 
                                          subset = subset,
                                          server = server,
                                          port = port,
                                          n = n ),
      warning = w.handler
    )
    
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
  
  fit$m <- m
  
  return( fit )
}




distributedCalibrationBeltPoints <- function(
  server, port, 
  dataInfo, seqG, m, fit,
  thres, cLevel, devel
) {
  
  #START local functions ****************************************************
  
  logLikelihood <- function(beta) {
    
    return(
      getFunctionResults(
        server, port,
        "partiallogLikelihood",
        o = dataInfo$o, e = dataInfo$e,
        subset = dataInfo$subset,
        beta = beta
      )$logLik - thresholdLogLik
    )
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
    return(
      getFunctionResults(
        server, port,
        "partialjacLogLikelihood",
        o = dataInfo$o, e = dataInfo$e,
        subset = dataInfo$subset,
        beta = beta
      )
    )
  }
  
  hessianLogLikelihood <- function(beta){
    return(
      getFunctionResults(
        server, port,
        "partialhessianLogLikelihood",
        o = dataInfo$o, e = dataInfo$e,
        subset = dataInfo$subset,
        beta = beta
      )
    )
  }

  
  Fn <- function(x){
    
    numEq <- length(x)
    
    vecG <- g^seq(0, m)
    vecG <- vecG / sqrt(sum(vecG^2))
    
    #Gradient of logLikelihood (constraint) parallel to (1,g,...,g^m)
    F1 <- jacLogLikelihood( x[ -numEq ] ) - x[numEq] * vecG
    
    #Likelihood equal to threshold value
    F2 <- logLikelihood( x[ -numEq ] )
    
    return(c(F1, F2))
  }
  
  JacobFn <- function(x){
    
    numEq <- length(x)
    
    vecG <- g^seq(0, m)
    vecG <- vecG / sqrt(sum(vecG^2))
    
    H <- hessianLogLikelihood( x[-numEq] )
    
    J_F1 <- cbind(H, vecG)
    
    #Gradient of logLikelihood
    J_F2 <- c( jacLogLikelihood( x[-numEq] ), 0)
    
    return(rbind(J_F1, J_F2))
  }
  
  
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
  
  ####### sono arrivato qui!!!!!
  # G <- sapply(data$logite, FUN="^", seq(from = 0, to = m))
  
  lowerBound <- NULL
  upperBound <- NULL
  
  #Good starting conditions
  g <- seqG[1]
  #grad <- sapply(g, FUN = "^", seq(from = 0, to = m))
  grad <- g^seq(from = 0, to = m)
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
    
    #print( which( seqG == g ) )
    
    p <- logistic(g)
    
    grad <- sapply(g, FUN="^", seq(from = 0, to = m))
    gradNorm <- grad / sqrt(sum(grad^2))
    
    parMinTemp <- try(
      rootSolve::multiroot(
        f = Fn,
        start = parMin,
        jacfunc = JacobFn,
        rtol=1e-2, ctol=1e-2, atol=1e-2,
        useFortran = F)$root,
      silent = T )
    
    if(is.character(parMinTemp)){
      # sono arrivato qui
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
