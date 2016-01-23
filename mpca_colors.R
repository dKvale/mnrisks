require(devtools)

#-- For image processing
#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
library(EBImage)

#-- For color analysis
#install_github("ramnathv/rblocks")
#install_github("woobe/rPlotter")
library(rPlotter)

#-- MPCA Logo
mpca <- extract_colours("http://wac.450f.edgecastcdn.net/80450F/wjon.com/files/2010/12/mpca.jpg", num_col = 6)

#-- plot extracted palette
pie(rep(1, 6), col = mpca, main = "Palette based on MPCA logo", cex.main=0.8)

structure(mpca, class = "palette", name = "mpca")
