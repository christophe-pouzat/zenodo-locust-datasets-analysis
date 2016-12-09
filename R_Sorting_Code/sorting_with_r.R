cstValueSgts <- function(x) {
    dx <- as.integer(abs(diff(x,2))/2 <= .Machine$double.eps) ## use "order 2" derivative estimates
    ## ddx is zero where the derivative did not change,
    ## it is one if the derivative becomes null and -1
    ## is the derivative stops being null
    ddx <- diff(dx) 
    ## s contains the indexes of the first
    ## points of segments where the
    ## derivative is null
    s <- (1:length(ddx))[ddx==1]
    sapply(s,
           function(b) {
               n <- ddx[(b+1):length(ddx)]
               c(b+1,min((1:length(n))[n==-1]))
           }
           )
}
explore <- function(x,...) {
  UseMethod("explore")
}
explore.ts <- function(x,
                       offsetFactor=0.5, 
                       ...) {

    stopifnot(inherits(x,"ts"))
    yRange <- range(x,na.rm=TRUE)
    plotPara <- list(tlim = start(x)[1] + c(0,0.1),
                     ylim = yRange,
                     yMin = yRange[1],
                     yMax = yRange[2],
                     firstTime = start(x)[1],
                     lastTime = end(x)[1],
                     keepGoing = TRUE)
    
    nFct <- function() {
        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        rightTime <- rightTime + timeRange
        if (rightTime > plotPara$lastTime) {
            cat("Recording end reached.\n ")
            rightTime <- plotPara$lastTime
        }
        plotPara$tlim <- c(rightTime - timeRange, rightTime)
        plotPara
    }
    fFct <- function() {
        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        leftTime <- leftTime - timeRange
        if (leftTime < plotPara$firstTime) {
            cat("Recording end reached.\n ")
            leftTime <- plotPara$firstTime
        }
        plotPara$tlim <- c(leftTime, leftTime + timeRange)
        plotPara
    }
    qFct <- function() {
        plotPara$keepGoing <- FALSE
        plotPara
    }
    ## Function tFct definition
    ## Allows the user to change the recording duration displayed on the window
    ## The user is invited to enter a factor which will be used to multiply the
    ## present duration displayed.
    ## If the resulting duration is too long a warning is given and the whole
    ## recording is shown.
    ## If possible the center of the displayed window is conserved.
    tFct <- function() {
        
        presentWindowLength <- diff(range(plotPara$tlim))
        tMessage <- paste("Present duration displayed: ", presentWindowLength, " \n", sep = "")
        tMessage <- paste(tMessage,
                          "By what factor do you want to multiply it? \n", sep = "")
        
        theFactor <- as.numeric(readline(tMessage))
        
        if (theFactor <= 0) {
            cat("A negative or null factor does not make sense.\n")
            return(plotPara)
        } ## End of conditional on theFactor <= 0
        
        ## Check that the new display length is reasonable
        totalLength <- plotPara$lastTime - plotPara$firstTime
        if (theFactor * presentWindowLength >= totalLength) {
            cat("Cannot show more data than available but only the entire record.\n ")
            plotPara$tlim[1] <- plotPara$firstTime
            plotPara$tlim[2] <- plotPara$lastTime
            return(plotPara)
        }

        windowCenter <- plotPara$tlim[1] + presentWindowLength / 2
        newLeft <- windowCenter - theFactor * presentWindowLength / 2
        newRight <- windowCenter + theFactor * presentWindowLength / 2
        
        if (!(newLeft >= plotPara$firstTime & newRight <= plotPara$lastTime)) {
            if (newLeft <= plotPara$firstTime) {
                cat("Cannot show data before the recording started, the displayed center wont be conserved.\n ")
                plotPara$tlim[1] <- plotPara$firstTime
                plotPara$tlim[2] <- plotPara$tlim[1] + theFactor * presentWindowLength
            }
            if (newRight >= plotPara$lastTime) {
                cat("Cannot show data after the recording ended, the displayed center wont be conserved.\n ")
                plotPara$tlim[2] <- plotPara$lastTime
                plotPara$tlim[1] <- plotPara$tlim[2] - theFactor * presentWindowLength
            }
            return(plotPara)
        } ## End of conditional on !(newLeft >= plotPara$firstTime & newRight <= plotPara$lastTime)

        plotPara$tlim[1] <- newLeft
        plotPara$tlim[2] <- newRight
        return(plotPara)
        
    }
    ## End of function tFct definition

    ## Function rFct definition
    ## Allows the user to change the maximal value displayed on the abscissa
    ## The user is invited to enter a value.
    rFct <- function() {
        
        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        tMessage <- paste("Present latest time displayed: ",
                          rightTime,
                          "\n", sep = "")
        tMessage <- paste(tMessage,
                          "What new latest time do want (return leaves things unchanged)? \n", sep = "")
        
        theNewTime <- as.numeric(readline(tMessage))
        
        if (is.na(theNewTime)) { ## Nothing entered, leave things unchanged 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theNewTime <= plotPara$firstTime) {
            ## This choice does not make sense
            cat("Cannot display data before recording started.\n")
            return(plotPara)
        }

        if (theNewTime > plotPara$lastTime) {
            cat("Recording end reached.\n ")
            rightTime <- plotPara$lastTime
        } else {
            if (theNewTime <= leftTime) {
                ## The new latest time entered is smaller that the earliest time displayed
                cat("The new latest time is smaller than the earliest, adjustement will be made.\n")
                leftTime <- theNewTime - timeRange
                if (leftTime < plotPara$firstTime) {
                    cat("Adjustment requires a change in displayed duration.\n")
                    leftTime <- plotPara$firstTime
                }
            } ## End of conditional on theNewTime <= leftTime 
            rightTime <- theNewTime
        } ## End of conditional on theNewTime > plotPara$lastTime
        
        plotPara$tlim <- c(leftTime, rightTime)
        plotPara
        
    }

    ## Function lFct definition
    ## Allows the user to change the minimal value displayed on the abscissa
    ## The user is invited to enter a value.
    lFct <- function() {
        
        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        tMessage <- paste("Present earliest time displayed: ",
                          leftTime,
                          "\n", sep = "")
        tMessage <- paste(tMessage,
                          "What new earliest time do want (return leaves things unchanged)? \n", sep = "")
        
        theNewTime <- as.numeric(readline(tMessage))
        
        if (is.na(theNewTime)) { ## Nothing entered, leave things unchanged 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theNewTime >= plotPara$lastTime) {
            ## This choice does not make sense
            cat("Cannot display data after recording ended.\n")
            return(plotPara)
        }

        if (theNewTime < plotPara$firstTime) {
            cat("Recording start reached.\n ")
            leftTime <- plotPara$firstTime
        } else {
            if (theNewTime >= rightTime) {
                ## The new earliest time entered is larger that the latest time displayed
                cat("The new earliest time is larger than the latest, adjustement will be made.\n")
                rightTime <- theNewTime + timeRange
                if (rightTime > plotPara$lastTime) {
                    cat("Adjustment requires a change in displayed duration.\n")
                    rightTime <- plotPara$lastTime
                }
            } ## End of conditional on theNewTime <= leftTime 
            leftTime <- theNewTime
        } ## End of conditional on theNewTime > plotPara$lastTime
        
        plotPara$tlim <- c(leftTime, rightTime)
        plotPara
        
    }
    

    ## Function yMaxFct definition
    ## Allows the user to change the maximal value displayed on the ordinate
    ## The user is invited to enter a value.
    yMaxFct <- function() {
        
        presentWindowRange <- range(plotPara$ylim)
        tMessage <- paste("Present range displayed: [",
                          paste(presentWindowRange, collapse = ","),
                          "] \n", sep = "")
        tMessage <- paste(tMessage,
                          "What new maximal ordinate value do want (return goes back to maximum)? \n", sep = "")
        
        theFactor <- as.numeric(readline(tMessage))

        if (is.na(theFactor)) {
            plotPara$ylim <- c(presentWindowRange[1],plotPara$yMax) 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theFactor <= plotPara$ylim[1]) {
            cat("The maximum should be larger than the minimum.\n")
            return(plotPara)
        } ## End of conditional on theFactor <= plotPara$ylim[1]

        plotPara$ylim <- c(presentWindowRange[1],theFactor) 
        return(plotPara)
        
    }
    ## End of function yMaxFct definition

    ## Function yMinFct definition
    ## Allows the user to change the minimal value displayed on the ordinate
    ## The user is invited to enter a value.
    yMinFct <- function() {

        presentWindowRange <- range(plotPara$ylim)
        tMessage <- paste("Present range displayed: [",
                          paste(presentWindowRange, collapse = ","),
                          "] \n", sep = "")
        tMessage <- paste(tMessage,
                          "What new minimal ordinate value do want (return goes back to minimum)? \n", sep = "")
        
        theFactor <- as.numeric(readline(tMessage))

        if (is.na(theFactor)) {
            plotPara$ylim <- c(plotPara$yMin, presentWindowRange[2]) 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theFactor >= plotPara$ylim[2]) {
            cat("The minimum should be smaller than the maximum.\n")
            return(plotPara)
        } ## End of conditional on theFactor >= plotPara$ylim[2]

        plotPara$ylim <- c(theFactor, presentWindowRange[2]) 
        return(plotPara)
        
    }
    ## End of function yMinFct definition

    show <- function(x,
                     plotPara,
                     ...) {

        s <- plotPara$tlim[1]
        e <- plotPara$tlim[2]
        y.m <- plotPara$ylim[1]
        y.M <- plotPara$ylim[2]
        m <- unclass(window(x,start=s,end=e))
        if (class(m) == "matrix") {
            m <- apply(m,2,function(x) ifelse(x < y.m, y.m,x))
            m <- apply(m,2,function(x) ifelse(x > y.M, y.M,x))
            ns <- dim(m)[2]
            offset <- c(0,-(1:(ns-1))*(y.M-y.m))
            m <- t(t(m)+offset*offsetFactor)
            matplot(m,type="l",lty=1,axes=FALSE,xlab="",ylab="",...)
        } else {
            m[m<y.m] <- y.m
            m[m>y.M] <- y.M
            plot(m,type="l",lty=1,axes=FALSE,xlab="",ylab="",ylim=c(y.m,y.M),...)
        }
    }

    plot.new()
    par(mar=c(0.5,0.5,0.5,0.5))
    show(x,plotPara,...)
    
    myMessage <- "Make a choice:\n n or 'return' (next); f (former); l (lower abscissa limit); r (upper abscissa limit) \n t (time scale); Y (upper ordinate limit); y (lower ordinate limit); q (quit) \n "

    while(plotPara$keepGoing) {
        
        myChoice <- readline(myMessage)

        plotPara <- switch(myChoice,
                           n = nFct(),
                           f = fFct(),
                           l = lFct(),
                           r = rFct(),
                           t = tFct(),
                           Y = yMaxFct(),
                           y = yMinFct(),
                           q = qFct(),
                           nFct()
                           )

        show(x,plotPara,...)
        
    } ## End of while loop on keepGoing

    dev.off()
    invisible()
}
peaks <- function(x,
                  minimalDist=15,
                  notZero=1e-3) {
    dx <- c(0,diff(x,2)/2,0)
    dx[abs(dx) < notZero] <- 0
    dx <- diff(sign(dx))
    res <- (1:length(dx))[dx < 0]
    res <- res[-length(res)][diff(res) > minimalDist]
    attr(res,"call") <- match.call()
    attr(res,"nIDx") <- length(x)
    class(res) <- "eventsPos"
    res
}
as.eventsPos <- function(x,
                         start,
                         end
                         ) {
    x <- as.integer(x)
    stopifnot(all(diff(x)>0))
    if (missing(start)) start <- floor(x)
    if (missing(end)) end <- ceiling(x)
    stopifnot(all(x>=start))
    stopifnot(all(x<=end))
    attr(x,"call") <- match.call()
    attr(x,"nIDx") <- end-start+1
    class(x) <- "eventsPos"
    x
}
print.eventsPos <- function(x, ...) {
  cat("\neventsPos object with indexes of ", length(x)," events. \n", sep = "")
  cat("  Mean inter event interval: ", round(mean(diff(x)),digits=2), " sampling points, corresponding SD: ", round(sd(diff(x)),digits=2), " sampling points \n", sep = "")
  cat("  Smallest and largest inter event intervals: ", paste(range(diff(x)),collapse=" and "), " sampling points. \n\n",sep= "")
}
explore.eventsPos <- function(x,y,
                              offsetFactor=0.5,
                              events.pch=16,
                              events.col=2,
                              ...) {
    stopifnot(inherits(y,"ts"))
    yRange <- range(y,na.rm=TRUE)
    plotPara <- list(tlim = start(y)[1] + c(0,0.1),
                     ylim = yRange,
                     yMin = yRange[1],
                     yMax = yRange[2],
                     firstTime = start(y)[1],
                     lastTime = end(y)[1],
                     keepGoing = TRUE)
    
    nFct <- function() {
        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        rightTime <- rightTime + timeRange
        if (rightTime > plotPara$lastTime) {
            cat("Recording end reached.\n ")
            rightTime <- plotPara$lastTime
        }
        plotPara$tlim <- c(rightTime - timeRange, rightTime)
        plotPara
    }
    fFct <- function() {
        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        leftTime <- leftTime - timeRange
        if (leftTime < plotPara$firstTime) {
            cat("Recording end reached.\n ")
            leftTime <- plotPara$firstTime
        }
        plotPara$tlim <- c(leftTime, leftTime + timeRange)
        plotPara
    }
    qFct <- function() {
        plotPara$keepGoing <- FALSE
        plotPara
    }
    ## Function tFct definition
    ## Allows the user to change the recording duration displayed on the window
    ## The user is invited to enter a factor which will be used to multiply the
    ## present duration displayed.
    ## If the resulting duration is too long a warning is given and the whole
    ## recording is shown.
    ## If possible the center of the displayed window is conserved.
    tFct <- function() {

        presentWindowLength <- diff(range(plotPara$tlim))
        tMessage <- paste("Present duration displayed: ", presentWindowLength, " \n", sep = "")
        tMessage <- paste(tMessage,
                          "By what factor do you want to multiply it? \n", sep = "")
        
        theFactor <- as.numeric(readline(tMessage))
        
        if (theFactor <= 0) {
            cat("A negative or null factor does not make sense.\n")
            return(plotPara)
        } ## End of conditional on theFactor <= 0

        ## Check that the new display length is reasonable
        totalLength <- plotPara$lastTime - plotPara$firstTime
        if (theFactor * presentWindowLength >= totalLength) {
            cat("Cannot show more data than available but only the entire record.\n ")
            plotPara$tlim[1] <- plotPara$firstTime
            plotPara$tlim[2] <- plotPara$lastTime
            return(plotPara)
        }

        windowCenter <- plotPara$tlim[1] + presentWindowLength / 2
        newLeft <- windowCenter - theFactor * presentWindowLength / 2
        newRight <- windowCenter + theFactor * presentWindowLength / 2
        
        if (!(newLeft >= plotPara$firstTime & newRight <= plotPara$lastTime)) {
            if (newLeft <= plotPara$firstTime) {
                cat("Cannot show data before the recording started, the displayed center wont be conserved.\n ")
                plotPara$tlim[1] <- plotPara$firstTime
                plotPara$tlim[2] <- plotPara$tlim[1] + theFactor * presentWindowLength
            }
            if (newRight >= plotPara$lastTime) {
                cat("Cannot show data after the recording ended, the displayed center wont be conserved.\n ")
                plotPara$tlim[2] <- plotPara$lastTime
                plotPara$tlim[1] <- plotPara$tlim[2] - theFactor * presentWindowLength
            }
            return(plotPara)
        } ## End of conditional on !(newLeft >= plotPara$firstTime & newRight <= plotPara$lastTime)

        plotPara$tlim[1] <- newLeft
        plotPara$tlim[2] <- newRight
        return(plotPara)
        
    }
    ## End of function tFct definition

    ## Function rFct definition
    ## Allows the user to change the maximal value displayed on the abscissa
    ## The user is invited to enter a value.
    rFct <- function() {

        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        tMessage <- paste("Present latest time displayed: ",
                          rightTime,
                          "\n", sep = "")
        tMessage <- paste(tMessage,
                          "What new latest time do want (return leaves things unchanged)? \n", sep = "")

        theNewTime <- as.numeric(readline(tMessage))
        
        if (is.na(theNewTime)) { ## Nothing entered, leave things unchanged 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theNewTime <= plotPara$firstTime) {
            ## This choice does not make sense
            cat("Cannot display data before recording started.\n")
            return(plotPara)
        }

        if (theNewTime > plotPara$lastTime) {
            cat("Recording end reached.\n ")
            rightTime <- plotPara$lastTime
        } else {
            if (theNewTime <= leftTime) {
                ## The new latest time entered is smaller that the earliest time displayed
                cat("The new latest time is smaller than the earliest, adjustement will be made.\n")
                leftTime <- theNewTime - timeRange
                if (leftTime < plotPara$firstTime) {
                    cat("Adjustment requires a change in displayed duration.\n")
                    leftTime <- plotPara$firstTime
                }
            } ## End of conditional on theNewTime <= leftTime 
            rightTime <- theNewTime
        } ## End of conditional on theNewTime > plotPara$lastTime
        
        plotPara$tlim <- c(leftTime, rightTime)
        plotPara
        
    }

    ## Function lFct definition
    ## Allows the user to change the minimal value displayed on the abscissa
    ## The user is invited to enter a value.
    lFct <- function() {

        leftTime <- plotPara$tlim[1]
        rightTime <- plotPara$tlim[2]
        timeRange <- rightTime - leftTime
        tMessage <- paste("Present earliest time displayed: ",
                          leftTime,
                          "\n", sep = "")
        tMessage <- paste(tMessage,
                          "What new earliest time do want (return leaves things unchanged)? \n", sep = "")

        theNewTime <- as.numeric(readline(tMessage))

        if (is.na(theNewTime)) { ## Nothing entered, leave things unchanged 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)

        if (theNewTime >= plotPara$lastTime) {
            ## This choice does not make sense
            cat("Cannot display data after recording ended.\n")
            return(plotPara)
        }

        if (theNewTime < plotPara$firstTime) {
            cat("Recording start reached.\n ")
            leftTime <- plotPara$firstTime
        } else {
            if (theNewTime >= rightTime) {
                ## The new earliest time entered is larger that the latest time displayed
                cat("The new earliest time is larger than the latest, adjustement will be made.\n")
                rightTime <- theNewTime + timeRange
                if (rightTime > plotPara$lastTime) {
                    cat("Adjustment requires a change in displayed duration.\n")
                    rightTime <- plotPara$lastTime
                }
            } ## End of conditional on theNewTime <= leftTime 
            leftTime <- theNewTime
        } ## End of conditional on theNewTime > plotPara$lastTime
        
        plotPara$tlim <- c(leftTime, rightTime)
        plotPara
        
    }


    ## Function yMaxFct definition
    ## Allows the user to change the maximal value displayed on the ordinate
    ## The user is invited to enter a value.
    yMaxFct <- function() {

        presentWindowRange <- range(plotPara$ylim)
        tMessage <- paste("Present range displayed: [",
                          paste(presentWindowRange, collapse = ","),
                          "] \n", sep = "")
        tMessage <- paste(tMessage,
                          "What new maximal ordinate value do want (return goes back to maximum)? \n", sep = "")
        
        theFactor <- as.numeric(readline(tMessage))

        if (is.na(theFactor)) {
            plotPara$ylim <- c(presentWindowRange[1],plotPara$yMax) 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theFactor <= plotPara$ylim[1]) {
            cat("The maximum should be larger than the minimum.\n")
            return(plotPara)
        } ## End of conditional on theFactor <= plotPara$ylim[1]

        plotPara$ylim <- c(presentWindowRange[1],theFactor) 
        return(plotPara)
        
    }
    ## End of function yMaxFct definition

    ## Function yMinFct definition
    ## Allows the user to change the minimal value displayed on the ordinate
    ## The user is invited to enter a value.
    yMinFct <- function() {

        presentWindowRange <- range(plotPara$ylim)
        tMessage <- paste("Present range displayed: [",
                          paste(presentWindowRange, collapse = ","),
                          "] \n", sep = "")
        tMessage <- paste(tMessage,
                          "What new minimal ordinate value do want (return goes back to minimum)? \n", sep = "")
        
        theFactor <- as.numeric(readline(tMessage))

        if (is.na(theFactor)) {
            plotPara$ylim <- c(plotPara$yMin, presentWindowRange[2]) 
            return(plotPara)
        } ## End of conditional on is.na(theFactor)
        
        if (theFactor >= plotPara$ylim[2]) {
            cat("The minimum should be smaller than the maximum.\n")
            return(plotPara)
        } ## End of conditional on theFactor >= plotPara$ylim[2]

        plotPara$ylim <- c(theFactor, presentWindowRange[2]) 
        return(plotPara)
        
    }
    ## End of function yMinFct definition

    show <- function(x,
                     y,
                     plotPara,
                     ...) {

        s <- plotPara$tlim[1]
        e <- plotPara$tlim[2]
        y.m <- plotPara$ylim[1]
        y.M <- plotPara$ylim[2]
        firstIdx <- round(max(1,s*frequency(y)))
        lastIdx <- round(min(end(y)[1]*frequency(y),e*frequency(y)))
        ii <- firstIdx:lastIdx
        xx <- x[firstIdx <= x & x <= lastIdx]
        if (class(y)[1] == "mts") {
            m <- y[ii,]
            if (length(xx) > 0)
                mAtx <- as.matrix(y)[xx,,drop=FALSE]
        } else {
            m <- y[ii]
            if (length(xx) > 0)
                mAtx <- y[xx]
        }
        if (class(m) == "matrix") {
            m <- apply(m,2,function(x) ifelse(x < y.m, y.m,x))
            m <- apply(m,2,function(x) ifelse(x > y.M, y.M,x))
            ns <- dim(m)[2]
            offset <- c(0,-(1:(ns-1))*(y.M-y.m))
            m <- t(t(m)+offset*offsetFactor)
            matplot(m,type="l",lty=1,axes=FALSE,xlab="",ylab="",...)
            if (length(xx) > 0) {
                mAtx <- t(t(mAtx)+offset*offsetFactor)
                matpoints(xx-ii[1]+1,mAtx,pch=events.pch,col=events.col)
            }
        } else {
            
            m[m<y.m] <- y.m
            m[m>y.M] <- y.M
            plot(m,type="l",lty=1,axes=FALSE,xlab="",ylab="",ylim=c(y.m,y.M),...)
            if (length(xx) > 0)
                points(xx-ii[1]+1,mAtx,pch=events.pch,col=events.col)
        }
    }

    plot.new()
    par(mar=c(0.5,0.5,0.5,0.5))
    show(x,y,plotPara,...)
    
    myMessage <- "Make a choice:\n n or 'return' (next); f (former); l (lower abscissa limit); r (upper abscissa limit) \n t (time scale); Y (upper ordinate limit); y (lower ordinate limit); q (quit) \n "

    while(plotPara$keepGoing) {
        
        myChoice <- readline(myMessage)

        plotPara <- switch(myChoice,
                           n = nFct(),
                           f = fFct(),
                           l = lFct(),
                           r = rFct(),
                           t = tFct(),
                           Y = yMaxFct(),
                           y = yMinFct(),
                           q = qFct(),
                           nFct()
                           )
        show(x,y,plotPara,...)
        
    } ## End of while loop on keepGoing

    dev.off()
    invisible()
}
cutSglEvt <- function(evtPos,
                      data,
                      before=14,
                      after=30
                      ) {
    evtPos <- as.integer(evtPos) ## make sure evtPos is an integer
    before <- as.integer(before) ## make sure before is an integer
    stopifnot(0 <= before) ## make sure before is positive or null
    after <- as.integer(after)
    stopifnot(0 <= after) ## make sure after is positive or null
    if (is.vector(data)) data <- matrix(data,nc=1)
    ns <- dim(data)[2]
    dl <- dim(data)[1]
    stopifnot(0<evtPos, evtPos<=dl) ## make sure evtPos is within range
    sl <- before+after+1 ## the length of the cut
    keep <- -before:after + evtPos
    within <- 1 <= keep & keep <= dl
    kw <- keep[within]
    res <- sapply(1:ns,
                  function(idx) {
                      v <- numeric(sl)
                      v[within] <- data[kw,idx]
                      v
                  }
                  )
    as.vector(res)
}
mkEvents <- function(positions,
                     data,
                     before=14,
                     after=30
                     ) {
    positions <- unclass(positions)
    data <- unclass(data)
    res <- sapply(positions,
                  cutSglEvt,
                  data,
                  before,
                  after)
    the.call <- match.call()
    attr(res,"positions") <- positions
    attr(res,"delta") <- NULL
    attr(res,"data") <- the.call[["data"]]
    attr(res,"before") <- before
    attr(res,"after") <- after
    attr(res,"numberOfSites") <- ifelse(is.matrix(data),dim(data)[2],1)
    attr(res,"call") <- match.call()
    class(res) <- "events"
    res
}
summary.events <- function(object,
                           ...) {
  b <- attr(object,"before")
  a <- attr(object,"after")
  ns <- attr(object,"numberOfSites")
  cat("\nevents object deriving from data set: ",attr(object,"data"),".\n",sep="")
  cat(" Events defined as cuts of ", a+b+1, " sampling points on each of the ",ns, " recording sites.\n",sep="")
  cat(" The 'reference' time of each event is located at point ", b+1, " of the cut.\n",sep="")
  if (!is.null(attr(object,"delta"))) {
    cat(" Events were realigned on median event.\n",sep="")
  }
  cat(" There are ", length(attr(object,"positions")), " events in the object.\n\n",sep="")
}
"[.events" <- function(x,i,j,drop = FALSE) {
    y <- NextMethod("[")
    if (!missing(i)) return(NULL)
    if (is.matrix(y) && dim(y)[2] > 1) {
        attr(y,"positions") <- attr(x,"positions")[j]
        attr(y,"delta") <- attr(x,"delta")
        attr(y,"data") <- attr(x,"data")
        attr(y,"before") <- attr(x,"before")
        attr(y,"after") <- attr(x,"after")
        attr(y,"numberOfSites") <- attr(x,"numberOfSites")
        attr(y,"call") <- match.call()
        class(y) <- "events"
    }
    y
}
t.events <- function(x) {
  t(unclass(x))
}
mean.events <- function(x,...) {
    apply(unclass(x),1,mean,...)
}

median.events <- function(x,na.rm = FALSE) {
    apply(unclass(x),1,median,na.rm)
}
'-.events' <- function(e1,e2) {
    stopifnot(length(e2) == dim(e1)[1])
    res <- unclass(e1)-e2
    attr(res,"positions") <- attr(e1,"positions")
    attr(res,"delta") <- attr(e1,"delta")
    attr(res,"data") <- attr(e1,"data")
    attr(res,"before") <- attr(e1,"before")
    attr(res,"after") <- attr(e1,"after")
    attr(res,"numberOfSites") <- attr(e1,"numberOfSites")
    attr(res,"call") <- match.call()
    class(res) <- "events"
    res
}
plot.events <- function(x,
                        y=NULL,
                        evts.lwd = 0.1,
                        medAndMad = TRUE,
                        evts.col = "black",
                        med.col = "red",
                        mad.col = "blue",
                        x.bar = NULL,
                        y.bar = NULL) {

    nsites <- attr(x,"numberOfSites")
    ne <- dim(x)[2]
    cl <- dim(x)[1]/nsites
    ylim <- range(x)
    matplot(x,type="n",xlab="",ylab="",axes=FALSE,ylim=ylim)
    if (nsites > 1) {
        ii <- 2*(1:(nsites %/% 2))
        rect((ii-1)*cl,ylim[1],ii*cl,ylim[2],col="grey80",border=NA)
    }
    matlines(x,col=evts.col,lty=1,lwd=evts.lwd)
    if (medAndMad) {
        med <- apply(x,1,median)
        mad <- apply(x,1,mad)
        lines(med,col=med.col)
        lines(mad,col=mad.col)
    }
    if (!is.null(x.bar)) segments(x0=0,y0=ylim[1]+0.1*diff(ylim),x1=x.bar)
    if (!is.null(y.bar)) segments(x0=0,y0=ylim[1]+0.1*diff(ylim),y1=ylim[1]+0.1*diff(ylim)+y.bar)
}
lines.events <- function(x,
                         evts.lwd = 0.1,
                         evts.col = "black",
                         ...
                         ) {
    matlines(x,col=evts.col,lty=1,lwd=evts.lwd,...)
}
print.events <- function(x, 
                         ... ) {
    plot.events(x,...)
}
mkNoise <- function(positions,
                    data,
                    before=14,
                    after=30,
                    safetyFactor=2,
                    size=2000) {
    positions <- unclass(positions)
    data <- unclass(data)
    if (!is.matrix(data)) data <- matrix(data,nc=1)
    size <- as.integer(size)
    stopifnot(0 < size) ## make sure size is a positive integer
    sl <- before+after+1
    ns <- dim(data)[2]
    i1 <- diff(positions) ## inter events intervals
    nbI <- (i1-round(safetyFactor*sl))%/%sl ## number of noise sweeps
    ## one can cut from each
    ## interval
    nbPossible <- min(size,
                      sum((nbI)[nbI>0])
                      )
    ## allocate next the memory for the noise events
    noiseMatrix <- matrix(0,
                          nr=ns*sl,
                          nc=nbPossible
                          )
    
    iV <- (1:length(i1))[nbI>0] ## A vector containing the indexes of
    ## the (inter event) intervals from
    ## which at least one noise sweep can be
    ## cut.
    iIdx <- 1 ## an index running over the inter event intervals from
    ## which noise events can be cut.
    nInI <- nbI[iV[iIdx]] ## the number of noise sweeps that can be cut
    ## from the "non empty" inter event interval
    ## iV[iIdx].
    nIdx <- 1 ## An index running over the noise sweeps.
    noisePositions <- integer(nbPossible)
    while (nIdx <= nbPossible) {
        uInI <- 1 ## An index running over the noise sweeps that will be
        ## cut from a given "non empty" inter event interval.
        iPos <- positions[iV[iIdx]] + round(safetyFactor*sl)
        noisePositions[nIdx] <- iPos
        while (uInI <= nInI & 
               nIdx <= nbPossible
               ) {
            ii <- (-before:after) + iPos
            ns <- as.vector(data[ii,])
            noiseMatrix[,nIdx] <- ns
            nIdx <- nIdx + 1
            iPos <- iPos + sl
            uInI <- uInI + 1
        } ## End of while loop on uInI
        iIdx <- iIdx + 1
        nInI <- nbI[iV[iIdx]]
    } ## End of while loop on nIdx

    the.call <- match.call()
    attr(noiseMatrix,"positions") <- noisePositions
    attr(noiseMatrix,"delta") <- NULL
    attr(noiseMatrix,"data") <- the.call[["data"]]
    attr(noiseMatrix,"before") <- before
    attr(noiseMatrix,"after") <- after
    attr(noiseMatrix,"numberOfSites") <- ifelse(is.matrix(data),dim(data)[2],1)
    attr(noiseMatrix,"call") <- match.call()
    class(noiseMatrix) <- "events"
    noiseMatrix
}
explore.prcomp <- function(x,
                           pc=1, ##<< an integer: the pc index to add to the mean.
                           factor=2, ##<< a numeric, the scaling factor; that is, the plot shows mean +/- factor * pc.
                           m.col="black", ##<< a character string or an integer, the color used for mean.
                           u.col="red", ##<< a character string or an integer, the color used for mean + factor * pc.
                           l.col="blue", ##<< a character string or an integer, the color used for mean - factor * pc.
                           xlab="Index", ##<< a character string with the abscissa label.
                           ylab="Amplitude", ##<< a character string with the ordinate label.
                           main, ##<< a character string with the title. If 'missing' (default) one is automatically generated.
                           ... ##<< additional arguments passed to 'plot'.
                           ) {
    if (missing(main)) {
        w <- x$sdev[pc]^2/sum(x$sdev^2)
        main <- paste("PC ",pc," (",round(100*w,digits=1),"%)",sep="")
    }
    u <- x$center + factor * x$rotation[,pc]
    l <- x$center - factor * x$rotation[,pc]
    ylim=range(c(l,u))
    plot(x$center,type="l",xlab=xlab,ylab=ylab,col=m.col,main=main,ylim=ylim,...)
    lines(u,col=u.col,...)
    lines(l,col=l.col,...)
}
get_jitter <- function(evts,
                       center,
                       centerD,
                       centerDD){
    
    centerD_norm2 <- sum(centerD^2)
    centerDD_norm2 <- sum(centerDD^2)
    centerD_dot_centerDD <- sum(centerD*centerDD)
    
    if (is.null(dim(evts))) evts <- matrix(evts, nc=1)
    
    evts <- evts - center
    h_dot_centerD <- centerD %*% evts 
    delta0 <- h_dot_centerD/centerD_norm2
    h_dot_centerDD <- centerDD %*% evts
    first <- -2*h_dot_centerD + 2*delta0*(centerD_norm2 - h_dot_centerDD) + 3*delta0^2*centerD_dot_centerDD + delta0^3*centerDD_norm2 
    second <- 2*(centerD_norm2 - h_dot_centerDD) + 6*delta0*centerD_dot_centerDD + 3*delta0^2*centerDD_norm2
    as.vector(delta0 - first/second)
}
    
mk_aligned_events <- function(positions,
                              data,
                              before=14,
                              after=30){
    dataD = apply(data,2,function(x) c(0,diff(x,2)/2,0))
    dataDD = apply(dataD,2,function(x) c(0,diff(x,2)/2,0))
    evts = mkEvents(positions, data, before, after)
    evtsD = mkEvents(positions, dataD, before, after)
    evtsDD = mkEvents(positions, dataDD, before, after)
    evts_median = apply(evts,1,median)
    evtsD_median = apply(evtsD,1,median)
    evtsDD_median = apply(evtsDD,1,median)
    evts_jitter = get_jitter(evts,evts_median,evtsD_median,evtsDD_median)
    ## positions = positions-[round(x.item(0)) for x in np.nditer(evts_jitter)]
    positions = positions-round(evts_jitter)
    evts = mkEvents(positions, data, before, after)
    evtsD = mkEvents(positions, dataD, before, after)
    evtsDD = mkEvents(positions, dataDD, before, after)
    evts_median = apply(evts,1,median)
    evtsD_median = apply(evtsD,1,median)
    evtsDD_median = apply(evtsDD,1,median)
    evts_jitter = get_jitter(evts,evts_median,evtsD_median,evtsDD_median)
    res = unclass(evts) - evtsD_median %o% evts_jitter - evtsDD_median %o% evts_jitter^2/2
    attributes(res) = attributes(evts)
    attr(res,"positions") <-  positions
    attr(res,"call") <- match.call()
    attr(res,"delta") <- evts_jitter
    res
}
mk_center_list = function(positions,
                           data,
                           before=49,
                           after=80) {
    dataD = apply(data,2,function(x) c(0,diff(x,2)/2,0))
    dataDD = apply(dataD,2,function(x) c(0,diff(x,2)/2,0))
    evts = mkEvents(positions, data, before, after)
    evtsD = mkEvents(positions, dataD, before, after)
    evtsDD = mkEvents(positions, dataDD, before, after)
    evts_median = apply(evts,1,median)
    evtsD_median = apply(evtsD,1,median)
    evtsDD_median = apply(evtsDD,1,median)
    list("center" = evts_median, 
         "centerD" = evtsD_median, 
         "centerDD" = evtsDD_median, 
         "centerD_norm2" = sum(evtsD_median^2),
         "centerDD_norm2" = sum(evtsDD_median^2),
         "centerD_dot_centerDD" = sum(evtsD_median*evtsDD_median), 
         "center_idx" = -before:after)
}
classify_and_align_evt <- function(evt_pos,
                                   data,
                                   centers,
                                   before=14,
                                   after=30
                                   ){
    cluster_names = names(centers)
    n_sites = dim(data)[2]
    centersM = sapply(cluster_names,
        function(cn) centers[[cn]][["center"]][rep(-before <= centers[[cn]][["center_idx"]] &
                                                   centers[[cn]][["center_idx"]] <= after,
                                                   n_sites)])
    
    evt = cutSglEvt(evt_pos,data=data,before=before, after=after)
    delta = -(centersM - evt)
    cluster_idx = which.min(apply(delta^2,2,sum))
    good_cluster_name = cluster_names[cluster_idx]
    good_cluster_idx = rep(-before <= centers[[good_cluster_name]][["center_idx"]] &
        centers[[good_cluster_name]][["center_idx"]] <= after,
        n_sites)
    centerD = centers[[good_cluster_name]][["centerD"]][good_cluster_idx]
    centerD_norm2 = sum(centerD^2)
    centerDD = centers[[good_cluster_name]][["centerDD"]][good_cluster_idx]
    centerDD_norm2 = sum(centerDD^2)
    centerD_dot_centerDD = sum(centerD*centerDD)
    h = delta[,cluster_idx]
    h_order0_norm2 = sum(h^2)
    h_dot_centerD = sum(h*centerD)
    jitter0 = h_dot_centerD/centerD_norm2
    h_order1_norm2 = sum((h-jitter0*centerD)^2) 
    if (h_order0_norm2 > h_order1_norm2) {
        h_dot_centerDD = sum(h*centerDD)
        first = -2*h_dot_centerD + 2*jitter0*(centerD_norm2 - h_dot_centerDD) +
            3*jitter0^2*centerD_dot_centerDD + jitter0^3*centerDD_norm2
        second = 2*(centerD_norm2 - h_dot_centerDD) + 6*jitter0*centerD_dot_centerDD + 
            3*jitter0^2*centerDD_norm2
        jitter1 = jitter0 - first/second
        h_order2_norm2 = sum((h-jitter1*centerD-jitter1^2/2*centerDD)^2)
        if (h_order1_norm2 <= h_order2_norm2) {
            jitter1 = jitter0
        }
    } else {
        jitter1 = 0
    }
    if (abs(round(jitter1)) > 0) {
        evt_pos = evt_pos - round(jitter1)
        evt = cutSglEvt(evt_pos,data=data,before=before, after=after)
        h = evt - centers[[good_cluster_name]][["center"]][good_cluster_idx]
        h_order0_norm2 = sum(h^2)
        h_dot_centerD = sum(h*centerD)
        jitter0 = h_dot_centerD/centerD_norm2
        h_order1_norm2 = sum((h-jitter0*centerD)^2) 
        if (h_order0_norm2 > h_order1_norm2) {
            h_dot_centerDD = sum(h*centerDD)
            first = -2*h_dot_centerD + 2*jitter0*(centerD_norm2 - h_dot_centerDD) + 
                3*jitter0^2*centerD_dot_centerDD + jitter0^3*centerDD_norm2
            second = 2*(centerD_norm2 - h_dot_centerDD) + 6*jitter0*centerD_dot_centerDD + 
                3*jitter0^2*centerDD_norm2
            jitter1 = jitter0 - first/second
            h_order2_norm2 = sum((h-jitter1*centerD-jitter1^2/2*centerDD)^2)
            if (h_order1_norm2 <= h_order2_norm2) {
                jitter1 = jitter0
            }
        } else {
            jitter1 = 0
        }
    }
    if (sum(evt^2) > sum((h-jitter1*centerD-jitter1^2/2*centerDD)^2)) 
        return(list(cluster_names[cluster_idx], evt_pos, jitter1))
    else 
        return(list('?',evt_pos, jitter1))
}
predict_data <- function(class_pos_jitter_list,
                         centers_list,
                         nb_channels=4,
                         data_length=300000) {
    
    res = matrix(0,nc=nb_channels, nr=data_length)
    for (class_pos_jitter in class_pos_jitter_list) {
        cluster_name = class_pos_jitter[[1]]
        if (cluster_name != '?') {
            center = centers_list[[cluster_name]][["center"]]
            centerD = centers_list[[cluster_name]][["centerD"]]
            centerDD = centers_list[[cluster_name]][["centerDD"]]
            jitter = class_pos_jitter[[3]]
            pred = center + jitter*centerD + jitter^2/2*centerDD
            pred = matrix(pred,nc=nb_channels)
            idx = centers_list[[cluster_name]][["center_idx"]] + class_pos_jitter[[2]]
            within = 0 < idx & idx <= data_length
            kw = idx[within]
            res[kw,] = res[kw,] + pred[within,]
        }
    }
    res
}

all_at_once = function(data, ## an unormalized data matrix
                       centers, ## a list of centers
                       thres=4*c(1,1,1,1), ## threshold vector
                       filter_length_1=5, ## length of first filter
                       filter_length=5, ## length of subsequent filters
                       minimalDist_1=15, ## dead time length imposed at first detection
                       minimalDist=10, ## dead time length imposed at subsequent detection
                       before, ## parameter of centers
                       after, ## parameter of centers
                       detection_cycle=c(0,1,2,3,4), ## where is detection done during the peeling
                       verbose=2 ## verbosity level
                       ) {
    n_samples = dim(data)[1] ## Number of sample points
    n_chan = dim(data)[2] ## Number of channels
    n_rounds = length(detection_cycle)
    if (verbose > 0) {
        ## print five number summary
        cat("The five number summary is:\n")
        print(summary(data,digits=2))
        cat("\n")
    }
    ## normalize the data
    data.mad = apply(data,2,mad)
    data = t((t(data)-apply(data,2,median))/data.mad)
    filtered_data_mad = sapply(c(filter_length_1,filter_length),
                               function(l) {
                                   lDf = -data
                                   lDf = filter(lDf,rep(1,l)/l)
                                   lDf[is.na(lDf)] = 0
                                   apply(lDf,2,mad)
                               })
    ## Define local function detecting spikes
    get_sp = function(dataM,
                      f_length,
                      MAD,
                      site_idx=0,
                      minimalDist=15) {
        lDf = -dataM
        lDf = filter(lDf,rep(1,f_length)/f_length)
        lDf[is.na(lDf)] = 0
        lDf = t(t(lDf)/MAD)
        bellow.thrs = t(t(lDf) < thres)
        lDfr = lDf
        lDfr[bellow.thrs] = 0
        if (site_idx == 0)
            res = peaks(apply(lDfr,1,sum),minimalDist)
        else
            res = peaks(lDfr[,site_idx],minimalDist)
        res[res>before & res < dim(dataM)[1]-after]
    }
    
    out_names = c(names(centers),"?") ## Possible names for classification
    data0 = data ## The normalized version of the data
    for (r_idx in 1:n_rounds) {
        s_idx = detection_cycle[r_idx]
        if (verbose > 1 && s_idx==0)
            cat(paste("Doing now round",r_idx-1,"detecting on all sites\n"))
        if (verbose > 1 && s_idx!=0)
            cat(paste("Doing now round",r_idx-1,"detecting on site",s_idx,"\n"))
        sp = get_sp(dataM=data,
                    f_length=ifelse(r_idx==1,filter_length_1,filter_length),
                    MAD=filtered_data_mad[,ifelse(r_idx==1,1,2)],
                    site_idx=s_idx,
                    minimalDist=ifelse(r_idx==1,minimalDist_1,minimalDist))
        if (length(sp)==0) next
        new_round = lapply(as.vector(sp),classify_and_align_evt,
                           data=data,centers=centers,
                           before=before,after=after)
        pred = predict_data(new_round,centers,
                            nb_channels = n_chan,
                            data_length = n_samples)
        data = data - pred
        res = sapply(out_names,
                     function(n) sum(sapply(new_round, function(l) l[[1]] == n)))
        res = c(length(sp),res)
        names(res) = c("Total",out_names)
        if (verbose > 1) {
            print(res)
            cat("\n")
        }
        if (r_idx==1)
            round_all = new_round
        else
            round_all = c(round_all,new_round)
    }

    ## Get the global prediction
    pred = predict_data(round_all,centers,
                        nb_channels=n_chan,
                        data_length=n_samples)
    ## Get the residuals
    resid = data0 - pred
    ## Repeat inital detection on resid
    sp = get_sp(dataM=resid,
                f_length=filter_length_1,
		MAD=filtered_data_mad[,1],
                site_idx=detection_cycle[1],
                minimalDist=minimalDist_1)
    ## make events objects from this stuff
    unknown = mkEvents(sp,resid,before,after)
    ## get global counts
    res = sapply(out_names,
                 function(n) sum(sapply(round_all, function(l) l[[1]] == n)))
    res["?"] = length(sp)
    res=c(sum(res),res)
    names(res) = c("Total",out_names)
    if (verbose > 0) {
        cat("Global counts at classification's end:\n")
        print(res)
    }
    ## Get centers
    obs_nb = lapply(out_names[-length(out_names)],
                    function(cn) sum(sapply(round_all, function(l) l[[1]]==cn)))
    names(obs_nb) = out_names[-length(out_names)]
    spike_trains = lapply(out_names[-length(out_names)],
                          function(cn) {
                              if (obs_nb[cn] <= 1)
                                  return(numeric(0))
                              else
                                  res = sapply(round_all[sapply(round_all, function(l) l[[1]]==cn)],
                                               function(l)
                                                   round(l[[2]]+l[[3]]))
                              res[res>0 & res<n_samples]
                          }
                              )    
    centersN = lapply(1:length(spike_trains),
                      function(st_idx) {
                          if (length(spike_trains[[st_idx]]) == 0)
                              centers[[st_idx]]
                          else
                              mk_center_list(spike_trains[[st_idx]],data0,
                                             before=before,after=after)
                      }
                      )
    names(centersN) = out_names[-length(out_names)]
    list(prediction=pred,
         residual=resid,
         counts=res,
         unknown=unknown,
         centers=centersN,
         classification=round_all)
}
get_data = function(trial_idx,
                    stim="Spontaneous_1",
                    channels=c("ch02","ch03","ch05","ch07"),
                    file="locust20010214_part1.hdf5") {
    prefix = ifelse(trial_idx<10,
                    paste0("/",stim,"/trial_0",trial_idx),
                    paste0("/",stim,"/trial_",trial_idx)
                    )
    sapply(channels,
           function(n) {
               h5read(file, paste0(prefix,"/",n))
           })
}
sort_many_trials = function(inter_trial_time,
                            get_data_fct,
                            stim_name,
                            trial_nbs,
                            centers,
                            counts,
                            all_at_once_call_list,
                            layout_matrix=matrix(1:10,nr=5),
                            new_weight_in_update=0.01
                            ) {
    centers_old = centers
    counts_old = counts
    counts_M = matrix(0,nr=length(trial_nbs),nc=length(counts_old))
    centers_L = lapply(centers,
                       function(c) {
                           res = matrix(0,nr=length(c$center),nc=length(trial_nbs))
                           res[,1] = c$center
                           res
                       }
                       )
    names(centers_L) = names(centers)
    nbc = length(centers)
    spike_trains = vector("list",nbc)
    names(spike_trains) = paste("Cluster",1:nbc)
    idx=1
    for (trial_idx in trial_nbs) {
        ref_data = get_data_fct(trial_idx,stim_name)
        cat(paste0("***************\nDoing now trial ",trial_idx," of ",stim_name,"\n"))
        analysis = do.call(all_at_once,
                           c(list(data=ref_data,centers=centers),all_at_once_call_list))
        cat(paste0("Trial ",trial_idx," done!\n******************\n"))
        centers_new = analysis$centers
        counts_new = analysis$counts
        counts_M[idx,] = counts_new
        centers = centers_new
        for (c_idx in 1:length(centers)){
            n = counts_new[c_idx+1]
            o = counts_old[c_idx+1]
            w = new_weight_in_update*ifelse(o>0,min(1,n/o),0) ## New template weight
            centers[[c_idx]]$center=w*centers_new[[c_idx]]$center+(1-w)*centers_old[[c_idx]]$center
            centers[[c_idx]]$centerD=w*centers_new[[c_idx]]$centerD+(1-w)*centers_old[[c_idx]]$centerD
            centers[[c_idx]]$centerDD=w*centers_new[[c_idx]]$centerDD+(1-w)*centers_old[[c_idx]]$centerDD
            centers[[c_idx]]$centerD_norm2=sum(centers[[c_idx]]$centerD^2)
            centers[[c_idx]]$centerDD_norm2=sum(centers[[c_idx]]$centerDD^2)
            centers[[c_idx]]$centerD_dot_centerDD=sum(centers[[c_idx]]$centerD*centers[[c_idx]]$centerDD)
            centers_L[[c_idx]][,idx] = centers[[c_idx]]$center
        }
        layout(layout_matrix)
        par(mar=c(1,3,3,1))
        the_pch = if (nbc<10) c(paste(1:nbc),"?")
                  else c(paste(1:9),letters[1:(nbc-9)],"?")
        matplot(counts_M[,2:length(counts_old)],type="b",
                pch=the_pch)
        c_range = range(sapply(centers_L,
                               function(m) range(m[,1:idx])))
        for (i in 1:nbc) {
            if (idx<3)
                matplot(centers_L[[i]][,1:idx],type="l",col=1,lty=1,lwd=0.5,
                        main=paste("Unit",i),ylim=c_range)
            else
                matplot(centers_L[[i]][,1:idx],type="l",col=c(4,rep(1,idx-2),2),
                        lty=1,lwd=0.5,main=paste("Unit",i),ylim=c_range)
        }
        centers_old = centers
        counts_old = counts_new
        round_all = analysis$classification
        st = lapply(paste("Cluster",1:nbc),
                    function(cn) sapply(round_all[sapply(round_all,
                                                         function(l) l[[1]]==cn)],
                                        function(l) l[[2]]+l[[3]]))
        names(st) = paste("Cluster",1:nbc)
        for (cn in paste("Cluster",1:nbc)) {
            if (length(st[[cn]]) > 0)
                spike_trains[[cn]] = c(spike_trains[[cn]],
                (trial_idx-1)*inter_trial_time + sort(st[[cn]]))
        }
        idx = idx+1
    }
    spike_trains = lapply(spike_trains,sort)
    list(centers=centers,
         counts=counts_new,
         spike_trains=spike_trains,
         counts_M=counts_M,
         centers_L=centers_L,
         trial_nbs=trial_nbs,
	 call=match.call())
}          
plot_isi = function(isi, ## vector of ISIs
                    xlab="ISI (s)",
                    ylab="ECFD",
                    xlim=c(0,0.5), 
                    sampling_frequency=15000,
                    ... ## additional arguments passed to plot
                    ) {
    isi = sort(isi)/sampling_frequency
    n = length(isi)
    plot(isi,(1:n)/n,type="s",
         xlab=xlab,ylab=ylab,
         xlim=xlim,...)
}
test_rt = function(ref_train, 
                   test_train,
                   sampling_frequency=15000,
                   nbins=50, ## the number of breaks in the histogram
		   single_trial_duration = ceiling(max(c(ref_train,test_train))/sampling_frequency), 
                   xlab="Recurrence time (s)",
                   ylab="Stabilized counts - stabilized expected counts",
		   subdivisions = 10000, ## argument of integrate
                   ... ## additional parameters passed to plot
                   ) {
    rt = ref_train/sampling_frequency
    tt = test_train/sampling_frequency
    rt_L = vector("list",0)
    tt_L = vector("list",0)
    idx_max = max(c(rt,tt))%/%single_trial_duration
    if ( idx_max == 0) {
        rt_L = list(rt)
        tt_L = list(tt)
    } else {
        idx = 0
        while (idx <= idx_max) {
            start_trial_time = idx*single_trial_duration
            end_trial_time = start_trial_time + single_trial_duration
            rt_t = rt[start_trial_time <= rt & rt < end_trial_time]
            tt_t = tt[start_trial_time <= tt & tt < end_trial_time]
            if (length(rt_t) > 0 && length(tt_t) > 0) {
                rt_L = c(rt_L,list(rt_t-start_trial_time))
                tt_L = c(tt_L,list(tt_t-start_trial_time))
            }
            idx = idx + 1
        }
    }
    tt_isi_L = lapply(tt_L,diff)
    it = unlist(tt_isi_L)
    p_it=ecdf(it) ## ECDF of ISI from test
    mu_it=mean(it)
    s_it=function(t) (1-p_it(t))/mu_it ## expected density of FRT/BRT under the null
    ## Get the BRT and FRT
    res = lapply(1:length(rt_L),
                 function(idx) {
                     rt_t = rt_L[[idx]]
                     tt_t = tt_L[[idx]]
                     rt_t = rt_t[min(tt_t) < rt_t & rt_t < max(tt_t)]
                     RT = sapply(rt_t,
                                 function(t) c(max(tt_t[tt_t<=t])-t,
                                               min(tt_t[tt_t>=t])-t)
                                 )
                 })
    frt = sort(unlist(lapply(res, function(l) l[2,])))
    brt = sort(-unlist(lapply(res, function(l) l[1,])))
    n = length(frt)
    frt_h = hist(frt,breaks=nbins,plot=FALSE)
    frt_c_s = sqrt(frt_h$counts)+sqrt(frt_h$counts+1) ## stabilized version of the FRT counts
    ## expected FRT counts under the null
    frt_c_e = sapply(1:(length(frt_h$breaks)-1),
                     function(i) integrate(s_it,frt_h$breaks[i],frt_h$breaks[i+1],subdivisions = subdivisions)$value
                     )
    frt_c_e_s = sqrt(frt_c_e*n) + sqrt(frt_c_e*n+1) ## stabilized version of the expected FRT counts
    brt_h = hist(brt,breaks=nbins,plot=FALSE)
    brt_c_s = sqrt(brt_h$counts)+sqrt(brt_h$counts+1) ## stabilized version of the BRT counts
    ## expected BRT counts under the null
    brt_c_e = sapply(1:(length(brt_h$breaks)-1),
                     function(i) integrate(s_it,brt_h$breaks[i],brt_h$breaks[i+1],subdivisions = subdivisions)$value
                     )
    brt_c_e_s = sqrt(brt_c_e*n) + sqrt(brt_c_e*n+1) ## stabilized version of the expected BRT counts
    X = c(rev(-brt_h$mids),frt_h$mids)
    Y = c(rev(brt_c_s-brt_c_e_s),frt_c_s-frt_c_e_s)
    plot(X,Y,type="n",
         xlab=xlab,
         ylab=ylab,
         ...)
    abline(h=0,col="grey")
    abline(v=0,col="grey")
    lines(X,Y,type="s")
}
counts_evolution = function(smt_res ## result of a sort_many_trials call
                           ) {
    nbc = length(smt_res$centers)
    the_pch = if (nbc<10) c(paste(1:nbc),"?")
                  else c(paste(1:9),letters[1:(nbc-9)],"?")
    matplot(smt_res$trial_nbs,
            smt_res$counts_M[,2:(nbc+2)],
            type="b",pch=the_pch,
            main="Counts evolution",xlab="Trial index",ylab="Number of events")
}
waveform_evolution = function(smt_res, ## result of a sort_many_trials call
                              threshold_factor=4, ## threshold used
                              layout_matrix=matrix(1:lenght(smt_res$centers_L),nr=lenght(smt_res$centers_L))
                              ) {
    nbc = length(smt_res$centers)
    nt = length(smt_res$trial_nbs)
    layout(layout_matrix)
    par(mar=c(1,3,4,1))
    for (i in 1:nbc) {
        matplot(smt_res$centers_L[[i]],
                type="l",col=c(4,rep(1,nt-2),2),lty=1,lwd=0.5,
                main=paste("Unit",i),ylab="")
        abline(h=-threshold_factor,col="grey")
        }
}
cp_isi=function(smt_res, ## result of a sort_many_trials call
                inter_trial_time=10, ## time between trials in seconds
                sampling_rate=15000, ## sampling rate in Hz
                nbins=50, ## number of bins for isi histogram
                isi_max=1, ## largest isi in isi histogram
                layout_matrix=matrix(1:(2*lenght(smt_res$centers_L)),nr=lenght(smt_res$centers_L),byrow=TRUE)
                ) {
    t_duration = inter_trial_time
    n_trials = length(smt_res$trial_nbs)
    nbc = length(smt_res$centers)
    still_there = nbc - sum(sapply(smt_res$spike_trains,
                                   function(l)
                                       is.null(l) || length(l) <= n_trials))
    layout(layout_matrix)
    par(mar=c(4,4,4,1))
    for (cn in names(smt_res$spike_trains)) {
        if (is.null(smt_res$spike_trains[[cn]]) || length(smt_res$spike_trains[[cn]]) <= n_trials) next
        st = smt_res$spike_trains[[cn]]/sampling_rate
        plot(st,1:length(st),
             main=paste("Observed CP for unit",cn),
             xlab="Time (s)",ylab="Nb of evts",type="s")
        isi = diff(st)
        isi = isi[isi <= isi_max]
        hist(isi,breaks=nbins,
             prob=TRUE,xlim=c(0,0.5),
             main=paste("ISI dist for unit",cn),
             xlab="Interval (s)",ylab="Density (1/s)")
    } 
}
cp_isi_raster=function(smt_res, ## result of a sort_many_trials call
                      inter_trial_time=10, ## time between trials in seconds
                      sampling_rate=15000, ## sampling rate in Hz
                      nbins=50, ## number of bins for isi histogram
                      isi_max=1, ## largest isi in isi histogram
                      layout_matrix=matrix(1:(3*lenght(smt_res$centers_L)),nr=lenght(smt_res$centers_L),byrow=TRUE)
                      ) {
    t_duration = inter_trial_time
    n_trials = length(smt_res$trial_nbs)
    nbc = length(smt_res$centers)
    still_there = nbc - sum(sapply(smt_res$spike_trains,
                                   function(l)
                                       is.null(l) || length(l) <= n_trials))
    layout(layout_matrix)
    par(mar=c(4,4,4,1))
    for (cn in names(smt_res$spike_trains)) {
        if (is.null(smt_res$spike_trains[[cn]]) || length(smt_res$spike_trains[[cn]]) <= n_trials) next
        st = smt_res$spike_trains[[cn]]/sampling_rate
        plot(st,1:length(st),
             main=paste("Observed CP for unit",cn),
             xlab="Time (s)",ylab="Nb of evts",type="s")
        isi = diff(st)
        isi = isi[isi <= isi_max]
        hist(isi,breaks=nbins,
             prob=TRUE,xlim=c(0,0.5),
             main=paste("ISI dist for unit",cn),
             xlab="Interval (s)",ylab="Density (1/s)")
        plot(c(0,t_duration),c(0,n_trials+1),type="n",axes=FALSE,
             xlab="",ylab="",main=paste("Raster of unit",cn))
        for (t_idx in 1:n_trials) {
            sub_st = st[(t_idx-1)*t_duration <= st &
                        st < t_idx*t_duration] - (t_idx-1)*t_duration
            if (length(sub_st) > 0)
                points(sub_st, rep(t_idx,length(sub_st)), pch=".")
        }
    } 
}
