cld <- function(object, ...)
  UseMethod("cld")

cld.glht <- function(object, level = 0.05, decreasing = FALSE, ...)
  cld(summary(object), level = level, decreasing = decreasing)

extr <- function(object) {
  
  stopifnot(object$type == "Tukey")
  
  print("Using extr")
  
  
  mf <- model.frame(object$model)
  if (!is.null(attr(mf, "terms"))) {
    tm <- attr(mf, "terms")
  } else {
    tm <- try(terms(object$model))
    if (inherits(tm, "try-error")) stop("no terms component found")
  }
  
  
  ### <FIXME> not very nice    
  if(any(class(object$model) == "lme")){
    mf <- get_all_vars(tm, mf)
  }
  ### </FIXME>
  
  covar <- (length(attr(tm, "term.labels")) > 1)
  y <- mf[[1L]]
  yname <- colnames(mf)[[1L]]
  
  stopifnot(length(object$focus) == 1)
  x <- mf[[object$focus]]
  xname <- object$focus
  
  K <- contrMat(table(x), type = "Tukey")
  comps <- cbind(apply(K, 1, function(k) levels(x)[k == 1]),
                 apply(K, 1, function(k) levels(x)[k == -1]))
  
  f <- if (inherits(object$model, "coxph")) predict else fitted
  lp <- f(object$model)
  
  ret <- list(y = y, yname = yname,  
              x = x, xname = xname, 
              weights = model.weights(mf), 
              lp = lp, covar = covar, comps = comps)
  return(ret)
}

cld.summary.glht <- function(object, level = 0.05, decreasing = FALSE, ...) {
  
  ret <- extr(object)
  signif <- (object$test$pvalues < level)
  # Order the levels according to its mean
  # Tidy up: ret$y[1:length(ret$x)]], cox models concatenates a vector of live/dead
  # I think this way is easier than to deal with gsub later and it's more general
  lvl_order <- levels(ret$x)[order(tapply(as.numeric(ret$y)[1:length(ret$x)], ret$x, mean))]
  # names(signif) <- gsub("\\s", "", rownames(object$linfct))
  ret$signif <- signif
  ret$mcletters <- insert_absorb(signif, decreasing = decreasing, 
                                 comps = ret$comps, lvl_order = lvl_order, ...)
  
  # start edit
  
  ret$mcletters$Letters <- ret$mcletters$Letters[levels(ret$x)]
  ret$mcletters$monospacedLetters <- ret$mcletters$monospacedLetters[levels(ret$x)]
  ret$mcletters$LetterMatrix <- ret$mcletters$LetterMatrix[levels(ret$x),]
  
  # end edit
  
  
  class(ret) <- "cld"
  ret
}

cld.confint.glht <- function(object, decreasing = FALSE, ...) {
  

  ret <- extr(object)
  ### significant, if confidence interval does not contains 0
  signif <- !(object$confint[, "lwr"] < 0 & object$confint[, "upr"] > 0)
  # Tidy up: ret$y[1:length(ret$x)]], cox models concatenates a vector of live/dead
  # I think this way is easier than to deal with gsub later and it's more general
  lvl_order <- levels(ret$x)[order(tapply(as.numeric(ret$y)[1:length(ret$x)], ret$x, mean))]
  # names(signif) <- gsub("\\s", "", rownames(object$linfct))
  ret$signif <- signif
  ret$mcletters <- insert_absorb(signif, decreasing = decreasing, 
                                 comps = ret$comps, lvl_order = lvl_order, ...)
  
  # start edit
  
  ret$mcletters$Letters <- ret$mcletters$Letters[levels(ret$x)]
  ret$mcletters$monospacedLetters <- ret$mcletters$monospacedLetters[levels(ret$x)]
  ret$mcletters$LetterMatrix <- ret$mcletters$LetterMatrix[levels(ret$x),]
  
  # end edit                      
  
  class(ret) <- "cld" 
  ret
}

print.cld <- function(x, ...)
  print(x$mcletters$Letters)

plot.cld <- function(x, type = c("response", "lp"), ...) {
  
  mcletters <- x$mcletters
  ### ms = mono-spaced
  msletters <- mcletters$monospacedLetters
  ### v = vertical
  vletters <- sapply(msletters,
                     function(x) paste(strsplit(x, "")[[1]], "\n", collapse = ""))
  vletters <- vletters[gsub(" ", "", levels(x$x))]
  msletters <- msletters[gsub(" ", "", levels(x$x))]
  type <- match.arg(type)
  dat <- x[c("x", "y", "lp")]
  if (is.null(x$weights)) {
    dat$weights <- rep(1, NROW(x$y))
  } else {
    dat$weights <- x$weights
  }
  dat <- as.data.frame(dat)
  xn <- x$xname
  yn <- x$yname
  if (!is.null(list(...)$xlab)) xn <- list(...)$xlab
  if (!is.null(list(...)$ylab)) yn <- list(...)$ylab
  
  if (x$covar || type == "lp") {
    ### boxplot to make use of "..." argument
    boxplot(lp ~ x, data = dat, xlab = xn, ylab = "linear predictor", ...)
    axis(3, at = 1:nlevels(dat$x), labels = vletters)
  } else {
    if (is.integer(dat$y)) dat$y <- as.numeric(dat$y)
    switch(class(dat$y), 
           "numeric" = {
             ### boxplot to make use of "..." argument
             boxplot(y ~ x, data = dat, xlab = xn, ylab = yn, ...)
             axis(3, at = 1:nlevels(dat$x), labels = vletters)
           },
           "factor" = {
             at <- xtabs(weights ~ x, data = dat) / sum(dat$weights)
             at <- cumsum(at) - at / 2
             mosaicplot(xtabs(weights ~ x + y, data = dat), main = NULL,
                        xlab = xn, ylab = yn, ...)
             axis(3, at = at, labels = vletters, tick = FALSE)
           },
           "Surv" = {
             plot(survfit(y ~ x, data = dat), lty = 1:nlevels(dat$x), ...)
             nc <- nchar(levels(dat$x))                              
             spaces <- unlist(lapply( max(nc)-nc, function(x) return(paste( rep(" ",x) ,collapse="")))) 
             #                old.par <- par(family="mono") 
             legend("topright", lty = 1:nlevels(dat$x), 
                    legend = paste(levels(dat$x), spaces, ": ", msletters, sep=""), 
                    ...)
             #                par(old.par)
           })
  }
}


insert_absorb <- function( x, Letters=c(letters, LETTERS), separator=".", decreasing = FALSE, 
                           comps = NULL, lvl_order){
  print('Insert absorb')
  print(x)
  obj_x <- deparse(substitute(x))
  print('obj_x')
  print(obj_x)
  if (is.null(comps)) {
    namx <- names(x)
    namx <- gsub(" ", "", names(x))
    if(length(namx) != length(x))
      stop("Names required for ", obj_x)
    split_names <- strsplit(namx, "-")
    stopifnot( sapply(split_names, length) == 2 )
    comps <- t(as.matrix(as.data.frame(split_names)))
  } 
  rownames(comps) <- names(x)
  lvls <- lvl_order
  n <- length(lvls)
  lmat <- array(TRUE, dim=c(n,1), dimnames=list(lvls, NULL) )
  
  if( sum(x) == 0 ){                                                        # no differences
    ltrs <- rep(get_letters(1, Letters=Letters, separator=separator), length(lvls) )
    names(ltrs) <- lvls
    colnames(lmat) <- ltrs[1]
    msl <- ltrs
    ret <- list(Letters=ltrs, monospacedLetters=msl, LetterMatrix=lmat)
    class(ret) <- "multcompLetters"
    return(ret)
  }
  else{
    signifs <- comps[x,,drop=FALSE]
    
    absorb <- function(m){
      for(j in 1:(ncol(m)-1)){
        for(k in (j+1):ncol(m)){
          if( all(m[which(m[,k]),k] & m[which(m[,k]),j]) ){                 # column k fully contained in column j
            m <- m[,-k, drop=FALSE]
            return(absorb(m))
          }
          else if( all(m[which(m[,j]),k] & m[which(m[,j]),j]) ){            # column j fully contained in column k
            m <- m[,-j, drop=FALSE]
            return(absorb(m))
          }
        }
      }
      return(m)
    }
    for( i in 1:nrow(signifs) ){                                            # insert
      tmpcomp <- signifs[i,]
      wassert <- which(lmat[tmpcomp[1],] & lmat[tmpcomp[2],])               # which columns wrongly assert nonsignificance
      if(any(wassert)){
        tmpcols <- lmat[,wassert,drop=FALSE]
        tmpcols[tmpcomp[2],] <- FALSE
        lmat[tmpcomp[1],wassert] <- FALSE
        lmat <- cbind(lmat, tmpcols)
        colnames(lmat) <- get_letters( ncol(lmat), Letters=Letters,
                                       separator=separator)
        if(ncol(lmat) > 1){                                                 # absorb columns if possible
          lmat <- absorb(lmat)
          colnames(lmat) <- get_letters( ncol(lmat),  Letters=Letters,
                                         separator=separator )
        }
      }
    }
  }
  lmat <- lmat[,order(apply(lmat, 2, sum))]
  lmat <- sweepLetters(lmat)                                                                  # 1st
  lmat <- lmat[,names(sort(apply(lmat,2, function(x) return(min(which(x))))))]                # reorder columns
  colnames(lmat) <- get_letters( ncol(lmat),  Letters=Letters,
                                 separator=separator)
  lmat <- lmat[,order(apply(lmat, 2, sum))]                                                   # 2nd sweep
  lmat <- sweepLetters(lmat)
  lmat <- lmat[,names(sort(apply(lmat,2, function(x) return(min(which(x)))), 
                           decreasing = decreasing))]                # reorder columns
  colnames(lmat) <- get_letters( ncol(lmat),  Letters=Letters,
                                 separator=separator)
  ltrs <- apply(lmat,1,function(x) return(paste(names(x)[which(x)], sep="", collapse="") ) )
  msl <- matrix(ncol=ncol(lmat), nrow=nrow(lmat))                                             # prepare monospaced letters
  for( i in 1:nrow(lmat) ){
    msl[i,which(lmat[i,])] <- colnames(lmat)[which(lmat[i,])]
    absent <- which(!lmat[i,])
    if( length(absent) < 2 ){
      if( length(absent) == 0 )
        next
      else{
        msl[i,absent] <- paste( rep(" ", nchar(colnames(lmat)[absent])), collapse="" )
      }
    }
    else{
      msl[i,absent] <- unlist( lapply( sapply( nchar(colnames(lmat)[absent]),
                                               function(x) return(rep( " ",x)) ),
                                       paste, collapse="") )
    }
  }
  msl <- apply(msl, 1, paste, collapse="")
  names(msl) <- rownames(lmat)
  ret <- list( Letters=ltrs, monospacedLetters=msl, LetterMatrix=lmat, 
               aLetters = Letters, aseparator = separator )
  class(ret) <- "multcompLetters"
  return(ret)
}

### multiple comparison procedures
### set up a one-way ANOVA
data(warpbreaks)
amod <- aov(breaks ~ tension, data = warpbreaks)

### specify all pair-wise comparisons among levels of variable "tension"
tuk <- glht(amod, linfct = mcp(tension = "Tukey"))

### extract information
tuk.cld <- cld(tuk)

### use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.25,1), no.readonly = TRUE)

### plot
plot(tuk.cld)
par(old.par)


input_frame <- model.frame(tuk$model)

