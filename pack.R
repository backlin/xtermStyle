setwd("~/Documents/R/egna paket/xtermStyle")
library("roxygen2")
options(useFancyQuotes = FALSE)

roxygen.update.description()
roxygenize("xtermStyle", "xtermStyle.roxygen", unlink.target = TRUE)
system("rm -rf xtermStyle.roxygen/inst")
system("R CMD check xtermStyle.roxygen")

system("R CMD INSTALL xtermStyle.roxygen")
system("R CMD build xtermStyle.roxygen")


# When ready to publish to CRAN
#
# Run R CMD check on the tar.gz from bash, not in R.
# It might behave slightly different.
# Finally check package with GC-torture:
system("R CMD check xtermStyle.roxygen --use-gct")


# Optional
system("R CMD INSTALL --build --clean xtermStyle.roxygen") # Build binary


