if (Sys.info()['sysname'] == "Windows"){
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    
}else{
    setwd(system("pwd", intern = T))}

load('rdata/classifier.RData')

"WE have both classifiers, we prefer you to try the classifier.pca because it is more estable"
#b
load('/rdata/parameters_classifier_pca.RData')

#classifier(parameters.pca, 'insert image path') # only takes one image at time

#d
load('/rdata/parameters_classifier.RData')

#classifier(parameters, 'insert image path') # only takes one image at time