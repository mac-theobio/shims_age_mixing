# User defined functions

DateCleaning <- function(daterel){
  
  # This function takes the date as recoreded in the SHIMS study
  # and cleans it to look like a standard date
  
  paste(substr(daterel,1,3),substr(daterel,4,5),sep = "/") %>%
    paste("01",sep = "/") %>%
    as.Date("%b/%y/%d")
}

AgeResAtRelOnset <- function(currentage, currentdate, daterelstart){
  
  # This function takes the current age reported by the participant
  # and the computes the difference betweeen the current date and when
  # the relationship started so as to get the age of the participant
  # when he/she started relationship with a partner
  as.numeric(currentage) - as.numeric(difftime(currentdate,
                                               daterelstart,
                                               units = "weeks")
  )/52.25
}


# Convert clmm() model components to a tidy dataframe
TidyCLMM <- function(m) {
  
  # This function takes the model output from the clmm() function and
  # outputs it as a tidy dataframe, which can then be used in ggplot
  # or dwplot() from the dotwhisker package.
  # The tidy dataframe has 'term', 'or', 'lb' and 'ub'
  # It also calculates profile confidence intervals, which
  # are supposed to be more accurate with prop odds
  
  require(dplyr)
  
  thresh <- length(m$y.levels)
  estimate <- exp(coef(m))
  conf.low <- exp(confint(m)[, 1])
  conf.high <- exp(confint(m)[, 2])
  
  ordf <- cbind(estimate, conf.low, conf.high) %>%
    data.frame() %>%
    rownames_to_column("term") %>% #function in tibble package(tidyverse)
    filter(between(row_number(), thresh, n()))
  
  return(ordf)
  
}

# Convert coxph() model components to a tidy dataframe
TidyCoxph <- function(m) {
  
  # This function takes the model output from the coxph() function and
  # outputs it as a tidy dataframe, which can then be used in ggplot
  # or dwplot() from the dotwhisker package.
  # The tidy dataframe has 'term', 'or', 'lb' and 'ub'
  
  require(dplyr)
  
  estimate <- exp(coef(m))
  conf.low <- exp(confint(m)[, 1])
  conf.high <- exp(confint(m)[, 2])
  
  ordf <- cbind(estimate, lb, ub) %>%
    data.frame() %>%
    rownames_to_column("term") 
  
  return(ordf)
  
}


# ===========================================================
# Functions developed by Mike Li to take clmm() model objects
# and derive fitted values with 95%CIs
# ===========================================================

# Provides predictions for CLMM
VarPred <- function(mod, varname, frame, isolate=FALSE, isoValue=NULL, level=0.05, steps=101, dfspec=100, vv=NULL){
  
  # Takes a clmm-class model object (mod), predictor variable name (varname), 
  # and data.frame(frame)containing only observations and variables used 
  # in model and returns a data.frame containing fitted values with 95% 
  # upper and lower confidence limits. The fitted values are on the scale of
  # the linear predictor.
  
  # Service functions
  eff <- function(mod){
    if (inherits(mod, "lm")) return (coef(mod))
    if (inherits(mod, "mer")) return (fixef(mod))
    if (inherits(mod, "clmm"))
    {
      ef <- c(0, mod$beta)
      names(ef) <- c("(Intercept)", names(mod$beta))
      return (ef)
    }
    stop("Don't recognize model type")
  }
  
  varfun <- function(vcol, steps){
    if(is.numeric(vcol)){
      if(is.null(steps)){return(sort(unique(vcol)))}
      return(seq(min(vcol), max(vcol), length.out=steps))
    }
    return(unique(vcol))
  }
  
  # Stats
  df <- ifelse(
    grepl("df.residual", paste(names(mod), collapse="")), 
    mod$df.residual, dfspec
  )
  mult <- qt(1-level/2, df)
  
  # Eliminate rows that were not used
  rframe <- frame[rownames(model.frame(mod)), ]
  
  # Find variable in data frame
  # ISSUE: Can't have a variable name that's a subset of another name
  pat <- paste("\\b", varname, sep="")
  fnames <- names(rframe)
  
  # print(pat); print(fnames)
  fCol <- grep(pat, fnames)
  
  #print(paste("Selected variable", fnames[fCol]))
  if(length(fCol)<1) {
    stop(paste("No matches to", varname, "in the frame", collapse=" "))
  }
  if(length(fCol)>1) {
    stop(paste("Too many matches:", fnames[fCol], collapse=" "))
  }
  
  if(is.null(vv)) {vv <- varfun(rframe[[fCol]], steps)}
  steps <- length(vv)
  
  # Mean row of model matrix
  modTerms <- delete.response(terms(mod))
  mm <- model.matrix(modTerms, rframe)
  rowbar<-matrix(apply(mm, 2, mean), nrow=1)
  mmbar<-rowbar[rep(1, steps), ]
  
  # Find variable in model matrix
  mmNames <- colnames(mm)
  mmCols <- grep(pat, mmNames)
  #print(paste(c("Selected columns:", mmNames[mmCols], "from the model matrix"), collapse=" "))
  if (length(mmCols)<1) 
    stop(paste("No matches to", varname, "in the model matrix", collapse=" "))
  
  # Model matrix with progression of focal variable 
  varframe <- rframe[rep(1, steps), ]
  varframe[fCol] <- vv
  mmvar <- mmbar
  mmnew <- model.matrix(modTerms, varframe)
  
  for(c in mmCols){
    mmvar[, c] <- mmnew[, c]
  }
  
  ef <- eff(mod)
  vc <- vcov(mod)
  if (inherits(mod, "clmm")){
    f <- c(names(mod$alpha)[[1]], names(mod$beta))
    vc <- vc[f, f]
  }
  
  if(!identical(colnames(mm), names(ef))){
    print(setdiff(colnames(mm), names(ef)))
    print(setdiff(names(ef), colnames(mm)))
    stop("Effect names do not match: check for empty factor levels?")
  }
  pred <- mmvar %*% eff(mod)
  
  # (Centered) predictions for SEs
  if (isolate) {
    if(!is.null(isoValue)){
      rframe[fCol] <- 0*rframe[fCol]+isoValue	
      mm <- model.matrix(modTerms, rframe)
      rowbar<-matrix(apply(mm, 2, mean), nrow=1)
      mmbar<-rowbar[rep(1, steps), ]
    }
    mmvar <- mmvar-mmbar
  }
  
  pse_var <- sqrt(diag(mmvar %*% tcrossprod(vc, mmvar)))
  
  df <- data.frame(
    var = vv,
    fit = pred,
    lwr = pred-mult*pse_var,
    upr = pred+mult*pse_var
  )
  names(df)[[1]] <- varname
  return(df)
}

# Transforms the predictions in the data.frame returned by varpred()

OrdTrans <- function(v, a){
  # This function takes a vector (v) from the data frame returned by varpred—
  # either the fitted value, or one of the confidence limits, and the intercepts 
  # for each of the ordinal outcomes. It returns transformed values by accounting for 
  # the threshold parameters and averaging through all levels.
  
  sapply(v, function(n){
    return(sum(plogis(n-a)))
  })
}

# Gets predictions from clmm object

OrdPred <- function(mod, n, modAns){
  # This function takes the model object (mod), predictor(n), and dataframe(modAns)
  # and returns a data.frame that has the original predictor values, fitted value
  # and upr and lower 95% intervals. The returned fitted value takes into account the
  # threshold values.
  
  
  v <- VarPred(mod, n, modAns, isolate=TRUE)
  v$fit <- OrdTrans(v$fit, mod$alpha)
  v$lwr <- OrdTrans(v$lwr, mod$alpha)
  v$upr <- OrdTrans(v$upr, mod$alpha)
  return(v)
}

