# Copyright 2019, Stefano Finazzi
# stefano.finazzi@marionegri.it


library( httr )
library( urltools )
library( jsonlite )

joinDataInfo <- function( x, y ){
  varNames <- names( x )
  if( ! identical( sort( varNames ), sort( names( y ) ) ) ){
    stop( 'incompatible names in dataInfo' )
  }
  # check identical var
  for( var in intersect( c( 'o', 'e' ), varNames) ){
    if( x[[ var ]] != y[[ var ]] )
      stop( 'incompatible variables' )
  }
  # sum variables
  for( var in intersect( c( 'n' ), varNames) ){
    x[[var]] <- x[[var]] + y[[var]]
  }
  # min variables
  for( var in intersect( c( 'minE' ), varNames) ){
    x[[var]] <- min( x[[var]], y[[var]] )
  }
  # max variables
  for( var in intersect( c( 'maxE' ), varNames) ){
    x[[var]] <- max( x[[var]], y[[var]] )
  }
  # join variables
  for( var in intersect( c( 'subset' ), varNames) ){
    x[[var]] <- c( x[[var]], y[[var]] )
  }
  
  return(x)
}

getFunctionResults <- function( server, port, fun, ... ){
  parameters <- list( ... )
  if( 'subset' %in% names( parameters ) ){
    subset <- parameters$subset
  }else{
    subset <- TRUE
  }
  connectionParameters <- c( 'server', 'port', 'subset' )
  nconnectionParameters <- sapply( connectionParameters, function(x)length( get(x) ) )
  ncall <- max( nconnectionParameters )
  if( ! all( nconnectionParameters %in% c( 1, ncall ) ) ){
    stop( 'incompatible number of connection parameters' )
  }else{
    for( var in connectionParameters ){
      assign( var, rep( get( var), length.out = ncall ) )
    }
  }
  
  results <- lapply(
    1:ncall,
    function(i)
      do.call(
        getFunctionPartialResults,
        c(
          list(
            server = server[i], port = port[i],
            fun = fun,
            subset = subset[i]
          ),
          parameters[ setdiff( names( parameters ), 'subset' ) ]
        )
      )
  )
  
  if( all( sapply( results, is.numeric ) ) ){
    result <- Reduce( '+', results )
  }
  
  if( all( sapply( results, function(x) 'dataInfo' %in% names( x ) ) ) ){
    result <- results[[1]]
    if( ncall > 1 ){
      for( res in results[-1] ){
        if( !identical( names( result ), names( res ) ) ){
          stop( 'incompatible results from servers' )
        }else{
          result$dataInfo <- joinDataInfo( result$dataInfo, res$dataInfo )
          for( var in setdiff( names( result ), 'dataInfo' ) ){
            result[[ var ]] <- result[[ var ]] + res[[ var ]]
          }
        }
      }
    }
  }
  
  return( result )
}




getFunctionPartialResults <- function( server, port, fun, ... ){
  urlString <- url_compose(
    list(
      scheme = 'http',
      domain = server,
      port = port,
      path = fun,
      parameter = NA,
      fragment = NA )
  )
  
  functionParameters <- list( ... )
  
  for( par in names( functionParameters ) ){
    value <- functionParameters[[ par ]]
    if( is.numeric( value ) ){
      value <- toJSON( value, digits = 15 )
    }
    urlString <- param_set( 
      urlString, 
      par, 
      url_encode( value ) )
  }
  
  httrObj <- GET( urlString, timeout( 60 ) )
  
  return( 
    content( 
      httrObj,
      encoding = 'utf8',
      as= 'parsed',
      simplifyVector = TRUE 
    )
  )
}


logisticRegressionForDistributedCB <- function( o, e, subset, server, port, n ){
  
  initialPar <- c( 0, 1 )
  initialPar <- c( initialPar, rep( 0, n - 1 ) )

  opt <- optim(
    initialPar,
    function( par ){
      lik <- getFunctionResults(
        server, port, 
        'partiallogLikelihood',
        o = o, e = e,
        subset = subset,
        beta = par
      )$logLik
      #print(lik)
      return( - lik)
    },
    method = 'BFGS',
    hessian = TRUE
  )
  
  opt$deviance <- 2 * opt$value
  
  minLogLik <- getFunctionResults(
    server, port,
    'partiallogLikelihood',
    o = o, e = e,
    subset = subset,
    beta = opt$par
  )
  
  opt <- c( opt, minLogLik )
  
  class( opt ) <- c( 'logisticRegressionForDistributedCB', class( opt ) )
  
  return( opt )
}

