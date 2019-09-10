# Copyright 2019, Stefano Finazzi
# stefano.finazzi@marionegri.it


# First, start a few API in a terminal with, possibly on different hosts
#
# Rscript client.r -p 7000 &
# Rscript client.r -p 8000 &
# Rscript client.r -p 9000 &
#
# sample data are loaded from the file sample.Rdata throught the method loadData in clientMethods.
# this is the only function that should be overwritten to load data from a MIP database
#
# Then run to run the calibration belt with the appropriate host IP and ports


source( 'cb.r' )

cb <- distributedCalibrationBelt( o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
                                  devel = 'external',
                                  subset = c( 'centreCode==\'a\''),
                                  server = c( '127.0.0.1'),
                                  port = c( 9000), nPoints = 20 )
plot( cb )



