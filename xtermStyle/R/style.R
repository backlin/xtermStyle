##' @include helpers.R
{}
##' @include control.R
{}
##' @include colors.R
{}

##' Adds formatting to a string.
##'
##' The xtermStyle package provides several methods for controlling the
##' appearance of output. To simplify for the user this function is a wrapper
##' that calls the appropriate other functions depending on the input. It is
##' probably the only formatting function in the package you need to care about.
##'
##' This package was designed on and primarily for xterm terminals supporting
##' 256 colours. However it is possible to run it on 16-colour terminals e.g. 
##' the OS X terminal with some loss of functionality. The predefined palettes, 
##' see \code{\link{xterm.pal}} will for instance not look very nice. The
##' package will automatically guess the appropriate mode for your terminal. If
##' you would like to change or disable coloured output entirely see
##' \code{\link{style.mode}}.
##'
##' @param string A string to add formatting to.
##' @param \dots Formatting arguments sent to \code{\link{style.set}}.
##' @return A string containing formatting escape sequences.
##' @examples
##' # This function has 3 usages:
##'
##' # 1. Set a default style to be used from now on:
##' cat(style(fg="yellow"))
##' cat("It's all yellow from here!\n")
##'
##' # 2. Temporary style change:
##' cat(style("Except this line that is green.\n", fg = "green"))
##' cat("But then we return to yellow again.\n")
##' 
##' # 3. Clear style to terminal default:
##' cat(style())
##' cat("Back to normal.\n")
##'
##' @seealso style.auto, style.dim, display.xterm.colors,
##'   styled.error, styled.warning
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
style <- function(string=NULL, ...) {
    if(is.null(options()$use.style)){
        options(use.style=.Platform$OS.type == "unix")
    }
    if(options()$use.style){
        if(is.null(string)) {
            if(is.blank(list(...))){
                return(style.clear())
            } else {
                return(style.set(...))
            }
        } else {
            if(is.blank(list(...))){
                stop("No formatting provided.")
            } else {
                return(paste(style.set(..., make.default = FALSE), string,
                    style.get(), sep=""))
            }
        }
    } else {
        return(string)
    }
}


##' Set, get and clear style.
##'
##' Generate formatting escape sequences from parameters.
##'
##' @param fg Foreground color i.e. color of the text. Can be any number in
##'   [0, 255] or a string such as \code{"grey"} or \code{"dark red"} (for the
##'   basic 16 colors).
##' @param bg Background color. Takes the same values as \code{fg}.
##' @param font.style A vector containg any combination of the following
##'   elements: \code{"normal"}, \code{"bold"}, \code{"underline"},
##'   \code{"blink"} (renders as bold on most terminals), \code{"inverse"}.
##' @param make.default If \code{TRUE} (default) this style will be the one
##'   reverted to after temporary changes in style e.g. with the \code{style}
##'   command or any display function like \code{\link[dataview]{entry.view}}.
##' @return Formatting escape sequences.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso style
##' @export
style.set <- function(fg = NULL, bg = NULL, font.style = NULL,
                      make.default = TRUE) {
    if(style.mode() == "off") return("")
    
    esc.seq <- ""

    for(f in font.style) {
        if(tolower(f) == "normal") f <- 0
        if(tolower(f) == "bold") f <- 1
        if(tolower(f) %in% c("underline", "underscore")) f <- 4
        if(tolower(f) == "blink") f <- 5 # Might render as bold
        if(tolower(f) == "inverse") f <- 7
        esc.seq <- c(esc.seq, sprintf("\033[%im", f))
    }

    if(style.mode() == "xterm-256color"){
        colors.16 <- c(0:15, 7, 8)
        COL <- c("red", "green", "yellow", "blue", "magenta", "cyan")
        names(colors.16) <- c("black", paste("dark", COL), "grey", "dark grey", COL,
                              "white", "gray", "dark gray")
        if(!is.blank(fg) && fg %in% names(colors.16)) fg <- colors.16[fg]
        if(!is.blank(bg) && bg %in% names(colors.16)) bg <- colors.16[bg]

        if(!is.blank(fg) && fg %in% 0:255)
            esc.seq <- c(esc.seq, sprintf("\033[38;5;%im", fg))
        if(!is.blank(bg) && bg %in% 0:255)
            esc.seq <- c(esc.seq, sprintf("\033[48;5;%im", bg))
    } else {
        # ANSI
        colors.16 <- 0:7
        names(colors.16) <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan", "white")
        xterm256.to.ansi <- function(x){
            # Function for mapping the 256 colors to the 8 ansi colors.
            binary.color <- round( c((x-16) %% 6, floor((x-16) %% 36 / 6), floor((x-16) / 36)) / 5)
            # Correspons to blue, green, red
            return(sum(c(4,2,1)*binary.color))
        }
        if(!is.blank(fg)){
            if(fg %in% names(colors.16)){ fg <- colors.16[fg]
            } else if(fg %in% 0:15){      fg <- fg %% 8
            } else if(fg %in% 16:231){    fg <- xterm256.to.ansi(fg)
            } else if(fg %in% 232:255){   fg <- 7*(fg > 243)
            } else fg <- NULL
            if(!is.blank(fg))
                esc.seq <- c(esc.seq, sprintf("\033[%im", 30 + fg))
        }
        if(!is.blank(bg)){
            if(bg %in% names(colors.16)){ bg <- colors.16[bg]
            } else if(bg %in% 0:15){      bg <- bg %% 8
            } else if(bg %in% 16:231){    bg <- xterm256.to.ansi(bg)
            } else if(bg %in% 232:255){   bg <- 7*(bg > 243)
            } else bg <- NULL
            if(!is.blank(bg))
                esc.seq <- c(esc.seq, sprintf("\033[%im", 40 + bg))
        }
    }
    esc.seq <- paste(esc.seq, collapse = "")
    if(make.default) options(current.style = esc.seq)
    return(esc.seq)
}


##' Get current default style.
##'
##' @return The formatting escape sequence generated by the last call to
##'   \code{style.set} (except those called with \code{make.default = FALSE},
##'   e.g. all package internal calls).
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname style.set 
##' @export
style.get <- function() {
    if(style.mode() == "off") return("")
    if(is.blank(options()$current.style)) {
        return(style.clear())
    } else {
        return(options()$current.style)
    }
}


##' Clear current style to terminal default.
##'
##' @return Formatting escape sequence for reseting style to default,
##'   \code{\\033[0m}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname style.set
##' @export
style.clear <- function(make.default=TRUE){
    if(style.mode() == "off") return("")
    esc.seq <- "\033[0m"
    if(make.default) options(current.style=esc.seq)
    return(esc.seq)
}


#' Shortcut for a neutral style.
#'
#' Useful for text that should not draw too much attention, e.g. the object
#' numbering in \code{\link[dataview]{whos}}.
#'
#' @param \dots Additional style parameters sent to \code{\link{style.set}}.
#' @return Formatting escape sequence for a neutral style.
#' @examples
#' MyVariable <- 1:6
#' cat(style.dim("MyVariable:"), style.auto(MyVariable), "\n")
#' @seealso style
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
style.dim <- function(...) style(..., fg = 249)


#' Automatic styling according to object properties.
#'
#' Returns formatting decided by an objects class and dimensions.
#' See \code{\link{style.auto.demo}} for a demonstration of how different
#' objects are styled.
#'
#' There are two color schemes adapted for either light text on dark background
#' (default) or dark text on light background. To switch to the dark on light
#' scheme use the function \code{\link{style.light}}, to switch back use
#' \code{\link{style.dark}}.
#'
#' @param obj Object to decide formatting from.
#' @param string String to be formatted. Optional, default: \code{print(obj)}.
#' @param fg Specify to overwrite automatic foreground color.
#' @param bg Specify to overwrite automatic background color.
#' @param \dots Additional style parameters sent to \code{\link{style.set}}.
#' @return A formatted string.
#' @examples
#' MyVariable <- 1:6
#' cat(style.dim("MyVariable:"), style.auto(MyVariable), "\n")
#' @seealso style.auto.demo, style.light, style
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
style.auto <- function(obj, string, fg=NULL, bg=NULL, ...){
    if(missing(string)) string <- obj

    opts <- getOption("color.scheme")

    if(!is.null(opts) && opts == "dark on light") {
        if(missing(fg)){
            if(is.null(obj)) { fg <- 246 # grey
            } else if(is.character(obj)) { fg <- 2 # green
            } else if(is.numeric(obj)) { fg <- 6 # cyan
            } else if(is.factor(obj)) { fg <- 5 # magenta
            } else if(is.logical(obj)) { fg <- 3 # yellow
            } else if(is.list(obj)) { fg <- 1 # red
            } else if(is.function(obj)) { fg <- 4 # blue
            }
        }
        if(missing(bg)){
            if(is.matrix(obj) || is.data.frame(obj)) { bg <- 195 # light blue
            } else if(is.array(obj)) { bg <- 224 # light red
            } else if(is.vector(obj) && length(obj) > 1) { bg <- 255 # light grey
            }
        }
    } else { # Light on dark
        if(missing(fg)){
            if(is.null(obj)) { fg <- 236 # Dark grey
            } else if(is.character(obj)) { fg <- 10 # green
            } else if(is.numeric(obj)) { fg <- 14 # cyan
            } else if(is.factor(obj)) { fg <- 13 # magenta
            } else if(is.logical(obj)) { fg <- 11 # yellow
            } else if(is.list(obj)) { fg <- 9 # red
            } else if(is.function(obj)){ fg <- 33 # blue
            }
        }
        if(missing(bg)){
            if(is.matrix(obj) || is.data.frame(obj)) { bg <- 18 # dark blue
            } else if(is.array(obj)) { bg <- 88 # dark red
            } else if(is.vector(obj) && length(obj) > 1) { bg <- 235 # dark grey
            }
        }
    }
    style(string, fg = fg, bg = bg, ...)
}


##' Demonstration of automatic styles.
##'
##' @return Nothing
##' @examples
##' style.auto.demo()
##' @seealso style.auto, style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
style.auto.demo <- function() {
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
}


##' Error display function with colored message.
##'
##' @param e Error object
##' @return Nothing
##' @examples
##' tryCatch({
##'     stop("Louisiana law is gonna get you Amos!")
##' }, error = styled.error)
##' @seealso styled.warning, style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
styled.error <- function(e) {
    cat(style.set(fg = 203), sep = "")
    print(e$call)
    cat(style(e$message, fg = 9), style.clear(), "\n", sep = "")
}


##' Warning display function with colored message.
##'
##' @param w Warning object
##' @return Nothing
##' @examples
##' tryCatch({
##'     warning("Do you feel lucky, little boy?")
##' }, warning = styled.warning)
##' @seealso styled.error, style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
styled.warning <- function(w) {
    cat(style.set(fg = 11), sep = "")
    print(w$call)
    cat(style(w$message, fg = 214), style.clear(), "\n", sep = "")
}

