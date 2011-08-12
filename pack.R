setwd("~/Documents/R/egna paket/xtermStyle")
library("roxygen2")
options(useFancyQuotes = FALSE)


roxygenize("xtermStyle", "xtermStyle.roxygen", unlink.target = TRUE)
system("rm -rf xtermStyle.roxygen/inst") # Empty folder that will generate check warning
system("R CMD check xtermStyle.roxygen") # Check package
system("R CMD INSTALL xtermStyle.roxygen") # Install package

system("R CMD build xtermStyle.roxygen") # Build package


# When ready to publish to CRAN
#
# Run R CMD check on the tar.gz from bash, not in R.
# It might behave slightly different.
# Finally check package with GC-torture:
system("R CMD check xtermStyle.roxygen --use-gct")


# Optional
system("R CMD INSTALL --build --clean xtermStyle.roxygen") # Build binary


