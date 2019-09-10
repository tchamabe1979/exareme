import rpy2.robjects as robjects

# R functions definitions
robjects.r('''
    setwd("./src_r/")
    source('clientMethods.r')   
    
    partialResLogLik = partiallogLikelihood(o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
                                            subset = 'centreCode=="a"', beta = '[0,1]')
    partialResJacLogLik = partialjacLogLikelihood(o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
                                                  subset = 'centreCode=="a"', beta = '[0,1]')       
    partialResHesLogLik = partialhessianLogLikelihood(o = 'hospOutcomeLatest_RIC10', e = 'probGiViTI_2017_Complessiva',
                                                      subset = 'centreCode=="a"', beta = '[0,1]')                                 
''')
r_partialResLogLik = robjects.globalenv['partialResLogLik']
r_partialResJacLogLik = robjects.globalenv['partialResJacLogLik']
r_partialResHesLogLik = robjects.globalenv['partialResHesLogLik']



print(r_partialResLogLik)
print(r_partialResJacLogLik)
print(r_partialResHesLogLik)