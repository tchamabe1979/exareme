#!/usr/bin/env Rscript

# Copyright 2019, Stefano Finazzi
# stefano.finazzi@marionegri.it


library( plumber )
library( optparse )

option_list <- list(
  make_option(
    c("-p", "--port"), 
    type="numeric", default=9000, 
    help="webserver port", metavar="integer" )
)

opt_parser <- OptionParser( option_list = option_list )
opt <- parse_args( opt_parser )

api <- plumb( "clientMethods.r" )
api$run( port = opt$port, host = '0.0.0.0' )
