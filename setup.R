## Fixes pca-face-recognizer folder as working directory 
    #All the other files are going to share the same working directory
setwd(dirname(sys.frame(1)$ofile))

## Installs if needed the packages specified in requirements.txt
    #If there are libraries not added yet, please add them to the txt file
req <- read.delim('requirements.txt', header = F, sep = "\b")[,1]
for (i in req) 
    if (!require(i, character.only = T)) 
        install.packages(i)
