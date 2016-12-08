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
