install.packages(c('ggplot2','GGally','randomForest','caret','naivebayes',
            'dplyr','caret','ROCR','psych','gt'),dependencies = T)

install.packages("tidyverse")
install.packages("rmarkdown")


install.packages("githubinstall")
library(githubinstall)
githubinstall("KlausVigo/kknn")

writeLines("R_LIBS_USER=/srv/rlibs", "~/.Renviron")
