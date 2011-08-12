##' Globally turn coloured output on or off.
##'
##' Wrappers for \code{\link{style.mode}}.
##'
##' @return Nothing
##' @examples
##' cat(style.auto(123456), "\n")
##' style.off()
##' cat(style.auto(123456), "\n")
##' style.on()
##' cat(style.auto(123456), "\n")
##' @seealso style.mode, style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
style.on <- function() style.mode(style.default.mode())

##' Turn style off.
##'
##' @return Nothing
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname style.on
##' @export
style.off <- function() style.mode("off")

##' Get default styling mode of the system
##'
##' This function will try to guess the appropriate styling mode for your
##' terminal. Basically it uses the \code{"sysname"} of \code{\link{Sys.info}}
##' and \code{"TERM"} of \code{\link{Sys.getenv}} and assigns \code{ansi} mode
##' to Macs (16 colours), \code{off} e.g. no styling to Windows machines and 
##' \code{xterm-256color} to anything else.
##'
##' If your system is not styled properly by default please let me know and I'll
##' fix it to the next version.
##'
##' @return The default styling mode of the system.
##' @examples
##' if(style.default.mode() == "xterm-256color"){
##'     cat(style("Sweetamajums!", fg=201, bg=20), "\n")
##' } else if(style.default.mode() == "ansi") {
##'     cat(style("Oh I believe in yesterday", fg=11, bg=1), "\n")
##' } else {
##'     cat(style("All code and no colour make HAL dull calculator!", fg=11), "\n")
##' }
##' @seealso style.mode, style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
style.default.mode <- function(){
    sn <- tolower(Sys.info()["sysname"])
    term <- tolower(Sys.getenv("TERM"))
    if(term %in% c("ansi", "apple_terminal")){
        return("ansi")
    } else if(sn == "windows" || is.blank(term)) {
        return("off")
    } else {
        return("xterm-256color") # Presumably linux or unix
    }
}

##' Set or get current style mode
##'
##' By default \code{\link{style.default.mode}} will try to guess the
##' appropriate style mode for your system. If it doesn't do it right you can
##' use this function to manually change it. Please also drop me an email and
##' I'll try to change it to the next version of the package.
##'
##' Supported modes are \code{xterm-256color}, \code{ansi}, \code{off}.
##'
##' @param x New style mode, case insensive character scalar.
##' @return If no new mode was given current mode is returned, otherwise
##'   nothing.
##' @examples
##' style.mode()
##' style.mode("off")
##' style.mode(style.default.mode())
##' @seealso style.default.mode, style.on, style.off, style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
style.mode <- function(x){
    if(missing(x)){
        if(is.null(options("style.mode")[[1]])){
            options(style.mode=style.default.mode())
        }
        return(options("style.mode")[[1]])
    } else {
        x <- tolower(x)
        if(!x %in% c("off", "ansi", "xterm-256color")){
            stop(sprintf("Style mode '%s' is not supported.", x))
        } else {
            options(style.mode=x)
        }
    }
}


##' Change color theme
##'
##' Light is designed for light backgrounds and dark for dark backgrounds.
##' This function merely sets a global option (\code{color.scheme}).
##' 
##' @return Nothing
##' @examples
##' style.light()
##' style.dark()
##' @seealso style.auto
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
style.light <- function(){
    options(color.scheme="dark on light")
}
##' @rdname style.light
##' @export
style.dark <- function(){
    options(color.scheme=NULL)
}

