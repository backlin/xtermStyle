cat("Datatypes:\t")
cat(style.auto(character(), "character"),
    style.auto(numeric(), "numeric"),
    style.auto(factor(), "factor"),
    style.auto(logical(), "logical"),
    style.auto(list(), "list"),
    style.auto(function() NULL, "function"),
    style.auto(NULL, "NULL"),
    "other", sep="\n\t\t")
cat("\nDimensions:\t")
cat(style.auto(NA, "scalar", fg=NULL),
    style.auto(c(NA, NA), "1-dimensional e.g. vector with length > 1", fg=NULL),
    style.auto(matrix(NA), "2-dimensional e.g. matrix", fg=NULL),
    style.auto(array(NA), "p-dimensional, p > 2 e.g. array", fg=NULL),
    sep="\n\t\t")
cat("\nExamples:\t")
cat(style.auto("string"),
    style.auto(data.frame(), "data.frame"),
    style.auto(matrix(NA, 3,3), "logical matrix"),
    style.auto(c("a", "b"), "character vector"),
    style.auto(array(0, c(2,2,2)), "3-d numerical matrix"),
    sep="\n\t\t")
cat("\n")
