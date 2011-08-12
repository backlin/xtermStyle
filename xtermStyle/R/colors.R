##' Display color table.
##'
##' The xterm colour table consist of the ANSI colours (16), the web
##' colour cube (216) and additional shades of grey not including full white and
##' black (16). However these are not strictly defined but can vary somewhat
##' between systems and configurations.
##'
##' @param numbers Logical, whether to display colour indices.
##' @param perm Rotation of the colour cube, supplied as a permutation of its
##'   dimensions. Sent to \code{\link[base]{aperm}}.
##' @return Nothing
##' @examples
##' display.xterm.colors()
##' display.xterm.colors(numbers=FALSE, perm=c(2,1,3))
##' @seealso style
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
display.xterm.colors <- function(numbers=TRUE, perm=1:3) {
    ## 16 ANSI colours
    for(i in 0:1) {
        for(j in 0:7)
            cat(sprintf("%s%4s",
                        style.set(fg = 8 * as.numeric(i == 0 & j == 0),
                                  bg = i * 8 + j,
                                  make.default=FALSE),
                        if(numbers) as.character(i * 8 + j) else ""))
        cat(sprintf("%s\n", style.clear(make.default=FALSE)))
    }
    cat("\n")
    
    ## 216 Web colours
    color.cube = aperm(array(16:231, c(6,6,6)), perm=perm)
    for(i in 1:6^3){
        x <- (i-1) %% 6 + 1
        y <- floor((i-1)%%72 / 12) + 1
        z <- 2*floor((i-1)/72) + ((i-1)%%12 >= 6) + 1
        my.color = color.cube[x,y,z]

        cat(sprintf("%s%4s",
                    style.set(fg = 8 * as.numeric(my.color == 16),
                              bg = my.color,
                              make.default=FALSE),
                    if(numbers) as.character(my.color) else ""))
        if(i %% 6 == 0 && i %% 12 != 0) cat(sprintf("%s  ", style.clear(make.default=FALSE)))
        if(i %% (2*6) == 0) cat(sprintf("%s\n", style.clear(make.default=FALSE)))
        if(i %% (2*6*6) == 0) cat(sprintf("%s\n", style.clear(make.default=FALSE)))
    }
    
    ## 16 
    for(i in 0:1) {
        for(j in 0:11)
            cat(sprintf("%s%4s",
                        style.set(fg = (238 + j) * as.numeric(i == 0),
                                  bg = 232 + i * 12 +j,
                                  make.default = FALSE),
                        if(numbers) as.character(232 + i * 12 + j) else ""))
        cat(sprintf("%s\n", style.clear(make.default=FALSE)))
    }
}
##' Display color table.
##'
##' @return Nothing
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname display.xterm.colors
##' @export
display.ansi.colors <- function(numbers=TRUE){
    ## 16 ANSI colours
    for(i in 0:1) {
        for(j in 0:7)
            cat(sprintf("%s%4s",
                        style.set(fg = 8 * as.numeric(i == 0 & j == 0),
                                  bg = i * 8 + j,
                                  make.default=FALSE),
                        if(numbers) as.character(i * 8 + j) else ""))
        cat(sprintf("%s\n", style.clear(make.default=FALSE)))
    }
    cat("\n")
}


##' Get predefined colour palettes
##'
##' All except "GnRd" and "long" are basen on the color brewer palettes, see
##' \code{\link[RColorBrewer]{brewer.pal}} of the \code{RColorBrewer} package.
##'
##' @param pal Palette name(s). Leave blank for all.
##' @return A list of vectors with colour indices.
##' @examples
##' display.xterm.pal()
##' display.xterm.pal(c("set1", "set2", "set3"))
##' pal <- xterm.pal("Accent")
##'
##' freqs <- runif(6)
##' fruits <- factor(sample(6, size=30, replace=TRUE, freqs/sum(freqs)),
##'                  labels=c("apple", "grapes", "banana", "lemon",
##'                           "blueberry", "raspberry"))
##' for(i in 1:length(fruits))
##'     cat(style(fruits[i], fg=pal$Accent[fruits[i]]), "\n")
##'
##' @seealso display.xterm.pal, display.xterm.colors
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
xterm.pal <- function(pal){
    if(missing(pal)) pal <- "all"
    pal <- tolower(pal)
    pals <- list()
    for(p in pal){
        if("ylorrd" == p || pal == "all")
            pals$YlOrRd <- c(230, 228, 221, 208, 202, 196, 160, 124, 88);
        if("ylorbr" == p || pal == "all")
            pals$YlOrBr <- c(230, 229, 228, 222, 215, 209, 166, 130, 94)
        if("ylgnbu" == p || pal == "all")
            pals$YlGnBu <- c(230, 194, 156, 115, 80, 38, 26, 20, 18)
        if("ylgnbu" == p || pal == "all")
            pals$YlGnBu <- c(230, 194, 156, 115, 80, 38, 26, 20, 18)
        if("ylgn" == p || pal == "all")
            pals$YlGn <- c(230, 228, 156, 113, 77, 41, 35, 28, 22)
        if("reds" == p || pal == "all")
            pals$Reds <- c(231, 224, 217, 211, 203, 196, 124, 88, 52)
        if("rdpu" == p || pal == "all")
            pals$RdPu <- c(225, 219, 213, 207, 201, 164, 127, 90, 53)
        if("purples" == p || pal == "all")
            pals$Purples <- c(231, 189, 183, 147, 141, 105, 99, 63, 57)
        if("purd" == p || pal == "all")
            pals$PuRd <- c(225, 183, 177, 170, 201, 163, 126, 89, 52)
        if("pubugn" == p || pal == "all")
            pals$PuBuGn <- c(159, 117, 74, 39, 38, 37, 36, 29, 23)
        if("pubu" == p || pal == "all")
            pals$PuBu <- c(159, 117, 81, 75, 38, 32, 26, 21, 18)
        if("orrd" == p || pal == "all")
            pals$OrRd <- c(230, 222, 215, 209, 203, 196, 160, 124, 88)
        if("oranges" == p || pal == "all")
            pals$Oranges <- c(230, 222, 215, 214, 208, 202, 166, 130, 94)
        if("greys" == p || pal == "all")
            pals$Greys <- c(254, 251, 249, 246, 243, 240, 237, 235, 234)
        if("greens" == p || pal == "all")
            pals$Greens <- c(194, 157, 120, 84, 77, 71, 34, 28, 22)
        if("gnbu" == p || pal == "all")
            pals$GnBu <- c(194, 157, 84, 42, 37, 31, 26, 20, 18)
        if("bupu" == p || pal == "all")
            pals$BuPu <- c(159, 117, 75, 69, 63, 57, 56, 55, 53)
        if("bugn" == p || pal == "all")
            pals$BuGn <- c(159, 117, 75, 39, 37, 36, 35, 28, 22)
        if("blues" == p || pal == "all")
            pals$Blues <- c(159, 117, 75, 39, 33, 27, 21, 19, 17)
        
        if("set3" == p || pal == "all")
            pals$Set3 <- c(80, 229, 61, 203, 75, 208, 76, 218, 248, 93, 158, 220)
        if("set2" == p || pal == "all")
            pals$Set2 <- c(79, 209, 104, 182, 155, 221, 180, 245)
        if("set1" == p || pal == "all")
            pals$Set1 <- c(160, 27, 35, 93, 208, 227, 130, 213, 244)
        if("pastel2" == p || pal == "all")
            pals$pastel2 <- c(122, 223, 117, 225, 158, 228, 224, 250)
        if("pastel1" == p || pal == "all")
            pals$pastel1 <- c(217, 117, 193, 147, 223, 230, 187, 225, 254)
        if("paired" == p || pal == "all")
            pals$paired <- c(117, 33, 157, 78, 218, 160, 216, 202, 183, 93, 229, 130)
        if("dark2" == p || pal == "all")
            pals$Dark2 <- c(30, 130, 62, 162, 70, 179, 94, 243)
        if("accent" == p || pal == "all")
            pals$Accent <- c(78, 141, 221, 229, 26, 162, 130, 243)
            
        if("spectral" == p || pal == "all")
            pals$Spectral <- c(53, 125, 203, 216, 222, 228, 191, 114, 36, 25, 57)
        if("rdylgn" == p || pal == "all")
            pals$RdYlGn <- c(124, 160, 202, 214, 221, 228, 191, 118, 76, 34, 22)
        if("rdylbu" == p || pal == "all")
            pals$RdYlBu <- c(124, 160, 202, 214, 221, 228, 159, 117, 75, 33, 26)
        if("rdgy" == p || pal == "all")
            pals$RdGy <- c(88, 124, 196, 203, 217, 231, 251, 246, 242, 237, 234)
        if("rdbu" == p || pal == "all")
            pals$RdBu <- c(88, 124, 196, 203, 217, 255, 159, 117, 75, 33, 26)
        if("puor" == p || pal == "all")
            pals$PuOr <- c(94, 166, 209, 215, 222, 255, 225, 177, 135, 92, 54)
        if("prgn" == p || pal == "all")
            pals$PRGn <- c(54, 92, 135, 177, 189, 255, 194, 119, 76, 34, 22)
        if("piyg" == p || pal == "all")
            pals$PiYG <- c(164, 171, 213, 219, 225, 255, 194, 119, 76, 34, 22)
        if("brbg" == p || pal == "all")
            pals$BrBG <- c(94, 166, 209, 215, 222, 255, 158, 115, 73, 30, 23)
            
        if("gnrd" == p || pal == "all")
            pals$GnRd <- c(seq(46,226,36), seq(220,196,-6))
        if("long" == p || pal == "all")
            pals$long <- c(18:20, seq(21,51,by=6), seq(87,195,by=36), 231:226, seq(220,196,by=-6), seq(160,88,by=-36))
        if("downup" == p || pal == "all")
            pals$DownUp <- c(seq(51, 21, by=-6), 20:16, seq(52, 196, by=36), seq(202, 226, by=6))
        if("bupuyl" == p || pal == "all")
            pals$BuPuYl <- c(seq(87, 57, by=-6), 56:53, seq(89, 197, by=36), seq(203, 227, by=6))
        if("jet" == p || pal == "all")
            pals$jet <- c(17:21, 27,33,39,45, 51:46, 82,118,154,190,226, 220,214,208,202, 196:201, 207,213,219,225,231)
    }
    return(pals)
}

##' Get the inverse, reverse or both of a palette
##'
##' Most color brewer palettes are designed for a white background. With these
##' functions you can easily adapt them for a dark background instead.
##'
##' @param pal Palette. Either vector of color indices or a string specifying a
##'   predefined palette.
##' @return The inverse of the palette
##' @examples
##' xterm.pal.inv("Set1")
##' xterm.pal.rev("Set2")
##' xterm.pal.revinv("Set3")
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso xterm.pal
##' @export
xterm.pal.inv <- function(pal){
    if(is.character(pal)) pal <- xterm.pal(pal)
    list.out <- is.list(pal)
    if(!list.out) pal <- list(pal)
    for(i in 1:length(pal)){
        p <- pal[[i]]
        new.r <- 5 - floor((p-16)/36)
        new.g <- 5 - floor((p-16) %% 36 / 6)
        new.b <- 5 - (p-16) %% 6
        new.ansi <- 15 - p
        new.grey <- 255 - p + 232
        pal[[i]][p > 15 & p < 232] <- (new.r * 36 + new.g * 6 + new.b + 16)[p > 15 & p < 232]
        pal[[i]][p < 16] <- new.ansi[p < 16]
        pal[[i]][p > 231] <- new.grey[p > 231]
    }
    if(!list.out) pal <- pal[[1]]
    return(pal)
}
##' Get the inverse, reverse or both of a palette
##'
##' @return The reverse of the palette
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname xterm.pal.inv
##' @export
xterm.pal.rev <- function(pal){
    if(is.character(pal)) pal <- xterm.pal(pal)
    if(is.list(pal)){
        for(i in 1:length(pal)) pal[[i]] <- rev(pal[[i]])
    } else {
        pal <- rev(pal)
    }    
    return(pal)
}
##' Get the inverse, reverse or both of a palette
##'
##' @return The reversed inverse of the palette
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname xterm.pal.inv
##' @export
xterm.pal.revinv <- function(pal){
    return(xterm.pal.rev(xterm.pal.inv(pal)))
}

##' Get predefined colour palettes
##'
##' @param pal Palette name(s). Leave blank for all.
##' @param numbers Whether to show colour indices.
##' @return Nothing
##' @examples
##' display.xterm.pal("BuPu")
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
display.xterm.pal <- function(pal, numbers=FALSE){
    if(missing(pal) || is.blank(pal)) pal <- xterm.pal()
    if(is.character(pal)) pal <- xterm.pal(pal)
    if(!is.list(pal)) pal <- list(` `=pal)

    nc <- max(nchar(names(pal)))
    cat("\n")
    for(i in 1:length(pal)){
        cat(sprintf(sprintf("%%%is: ", nc), names(pal)[i]))
        pos <- nc + 2
        terminal.width <- if(is.blank(Sys.getenv("COLUMNS"))) 80 else as.integer(Sys.getenv("COLUMNS"))
        cols.per.line <- floor((terminal.width - nc - 2) / 4)
        n.lines <- ceiling(length(pal[[i]]) / cols.per.line)
        cols.per.line <- ceiling(length(pal[[i]]) / n.lines)
        for(j in 1:length(pal[[i]])){
            cat(sprintf("%s%4s", style.set(bg=pal[[i]][j]), if(numbers) as.character(pal[[i]][j]) else ""))
            if(j %% cols.per.line == 0){
                cat(sprintf(sprintf("%s\n%%%is", style.clear(), nc+2), ""))
                pos <- nc + 2;
            }
        }
        cat(sprintf("%s\n", style.clear()))
    }
}
##' Get all predefined colour palettes
##'
##' @return Nothing
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname xterm.pal
##' @export
display.xterm.all <- function() display.xterm.pal()

##' Map numbers onto a palette
##' 
##' The continuous interval defined by \code{range} is divided into bins of
##' equal size. Each bin is mapped to a colour in the palette defined by \code{pal}.
##' The values in \code{x} are then assigned to the bins and their corresponding
##' colours are returned. Values outside the interval are assigned to the border
##' bins.
##' 
##' @param x Continuous numbers.
##' @param range The interval in \code{x} that will be mapped to the palette.
##' @param pal Palette. Can be the name of a predefined palette, as returned by
##'   \code{\link{xterm.pal}}, or a vector of colour indices directly.
##' @return Colour indices from \code{pal} corresponding to where in the range
##'   the values in \code{x} are.
##' @examples
##' error.rates <- .6*runif(10)
##' for(q in error.rates)
##'   cat(style(q, fg=discrete.color(q, c(0, .5), "GnRd")), "\n")
##' @seealso xterm.pal
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
discrete.color <- function(x, range=c(min(x), max(x)), pal="GnRd"){
    if(is.character(pal)) pal <- xterm.pal(pal)[[1]]
    n.pal <- length(pal)
    cuts <- seq(range[1], range[2], length=n.pal+1)
    col <- floor((n.pal+1)*(x - range[1])/(range[2] - range[1])) + 1
    col[col < 1] <- 1
    col[col > n.pal] <- n.pal
    col[1:length(col)] <- pal[col]
    return(col)
}

