#!/usr/bin/env Rscript
# do not do this:  !/usr/bin/R --no-save CMD BATCH
ip = as.data.frame(installed.packages()[,c(1,3:4)])
ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
ip
q()
