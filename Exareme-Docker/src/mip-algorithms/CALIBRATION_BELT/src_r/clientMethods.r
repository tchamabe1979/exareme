# Copyright 2019, Stefano Finazzi
# stefano.finazzi@marionegri.it

library( jsonlite )


####################################################
TABLEDIR <- '.'
DATAFILE <- 'sample.Rdata'

loadData <- function( variables, subset = TRUE ){
  
  tableName <- load( file.path( TABLEDIR, DATAFILE ) )
  result <- na.omit( get( tableName )[
    with( get( tableName ),
          eval( parse( text = subset ) ) ), variables ] )
  attr( result, 'subset' ) <- subset
  attr( result, 'variables' ) <- variables
  return( result )
}
####################################################


#* @serializer contentType list(type="application/json")
#* @get /partiallogLikelihood
partiallogLikelihood <- function(
  o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
  subset = TRUE, beta = '[0,1]' ) {

  return(
    computeLikelihoodFunction(
      'logLikelihood', o, e, subset, beta,
      addDataInfo = TRUE, resultName = 'logLik' )
  )
}


#* @serializer contentType list(type="application/json")
#* @get /partialjacLogLikelihood
partialjacLogLikelihood <- function(
  o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
  subset = TRUE, beta = '[0,1]' ) {

  return( computeLikelihoodFunction( 'jacLogLikelihood', o, e, subset, beta ) )
}

#* @serializer contentType list(type="application/json")
#* @get /partialhessianLogLikelihood
partialhessianLogLikelihood <- function(
  o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
  subset = TRUE, beta = '[0,1]' ) {

  return( computeLikelihoodFunction( 'hessianLogLikelihood', o, e, subset, beta ) )
}


logit <- function(p) log(p / (1 - p))

logistic <- function(x) 1 / (1 + exp(-x))

logLikelihood <- function( beta, x, o ){
  G <- t(x)
  probBeta <- logistic(t(beta) %*%  G )

  return(sum((1 - o) * log(1 - probBeta) +
               o * log(probBeta), na.rm=T))
}


jacLogLikelihood <- function( beta, x, o ){
  G <- t(x)
  # probBeta <- logistic( t(beta) %*%  G )
  # return(t(G %*% t( o - probBeta ) ) )

  probBeta <- logistic(crossprod(beta,G))
  return( t(tcrossprod(G, (o- probBeta))) )
}

hessianLogLikelihood <- function( beta, x, o ){
  G <- t(x)
  probBeta <- logistic( crossprod( beta,G ) )

  piOneMinusPiBeta <- ( probBeta * (1 - probBeta) )
  matrixProbBeta <- t(sapply(seq(from=1,to=length(beta)),
                             function(x) {return(piOneMinusPiBeta)}))
  return( tcrossprod(G * matrixProbBeta,G) )
}

computeLikelihoodFunction <- function(
  funName, o, e, subset, beta,
  addDataInfo = FALSE, resultName = 'value' ){
  
  print( paste( 'Requested', funName, 'with o:', o, 'e:', e, 'beta:', beta, 'subset:', subset ) )

  if( is.character( beta ) ){
    beta <- fromJSON( beta )
  }
  m <- length( beta ) - 1

  loadDataIntoGlovalEnvir(
    variables = c( o, e ), subset = subset, objectName = 'dataTable' )

  data <- dataTable
  x <- outer( logit( data[ , e ] ), 0:m, '^' )

  result <- get( funName )( beta = beta, x = x, o = data[ , o ] )

  if( addDataInfo ){
    dataInfo <- list(
      n = nrow( data ),
      minE = min( data[ , e ] ),
      maxE = max( data[ , e ] ),
      o = o,
      e = e,
      subset = subset
    )

    result <- list(
      result,
      dataInfo = dataInfo
    )

    names( result )[1] <- resultName
  }

  return( toJSON( result, digits = 20 ) )
}


loadDataIntoGlovalEnvir <- function(
  variables, subset = TRUE, objectName = 'dataTable' ){

  reLoadData <- TRUE
  if( objectName %in% ls( .GlobalEnv ) ){
    data <- get( objectName, envir = .GlobalEnv )
    if( identical( attr( data, 'subset' ), subset ) &
        identical( attr( data, 'variables' ), variables ) ){
      reLoadData <- FALSE
    }
  }

  if( reLoadData ){
    assign(
      objectName,
      loadData( variables = variables, subset = subset ),
      envir = .GlobalEnv
    )
  }
}
