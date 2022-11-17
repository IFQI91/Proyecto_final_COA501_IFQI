install.packages(c('ggplot2','GGally','randomForest','caret','naivebayes',
            'dplyr','caret','ROCR','psych','gt'),dependencies = T)

install.packages("kknn")
install.packages("tidyverse")
install.packages("rmarkdown")
writeLines("R_LIBS_USER=/srv/rlibs", "~/.Renviron")
