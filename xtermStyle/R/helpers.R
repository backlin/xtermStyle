##' Wrapper for several methods to test if a variable is empty.
##'
##' The following cases are considered empty:
##'  - NULL
##'  - Number of elements is 0
##'  - All elements are NA
##'  - All elements are empty strings
##'  - If \code{false.triggers=TRUE}: All elements are \code{FALSE}
##'
##' @param x A variable.
##' @param false.triggers Whether \code{FALSE} should be considered as empty.
##' @return Logical telling if variable is blank.
##' @examples
##' is.blank(NULL)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
is.blank <- function(x, false.triggers=FALSE){
    return(
        is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}

