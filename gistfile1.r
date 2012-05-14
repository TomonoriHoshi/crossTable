setClass("CrossTable", contains="table")
setMethod("show", signature=c(object="CrossTable"), function(object){
  print(ftable(object))
})

crossTable <- function(...,
                       exclude = if (useNA == "no") c(NA, NaN),
                       useNA = c("no", "ifany", "always"),
                       dnn = NULL,
                       deparse.level = 1) {
  if (!missing(exclude) && is.null(exclude)) 
    useNA <- "always"
  useNA <- match.arg(useNA)
  if(is.null(dnn))
    table <- table(..., exclude=exclude, useNA=useNA,
                   deparse.level=deparse.level)
  else
    table <- table(..., exclude=exclude, useNA=useNA,
                   dnn=dnn, deparse.level=deparse.level)
  return(new("CrossTable", table))
}


summary.CrossTable <- function(x, digits=3, ...){
  twoDimTable <- function(x, digits=3, width=6){
    output <- NULL
    dim    <- dim(x)
    dimnames <- dimnames(x)
    varnames <- names(dimnames)
    x <- addmargins(x, margin=1)
    p <- prop.table(x, margin=1) * 100
    x <- addmargins(x, margin=2)
    p <- addmargins(p, margin=2)
    
    rowcat <- paste(c(dimnames[[1]], " "), " ", sep="\t", collapse="\t")
    rowcat <- strsplit(rowcat, "\t")[[1]]
    rowcat <- format(c(" ", " ", " ", rowcat), justify="left")
    rowvar <- c(" ", " ", " ", varnames[1], rep(" ", length(rowcat)-4))
    rowvar[length(rowvar)-1] <- "Total"
    rowvar <- format(rowvar)
    
    for(i in seq_len(dim[2]+1)){
      count   <- x[, i]
      
      percent <- format(p[, i], digits=digits)
      percent <- paste(percent, "%", sep="")
      col <- paste(count, percent, sep="\t", collapse="\t")
      col <- strsplit(col, "\t")[[1]]
      col <- format(col, justify="right", width=width)
      col <- c(ifelse(is.na(dimnames[[2]][i]), "Total", dimnames[[2]][i]), col)
      col <- format(col, justify="centre")
      output <- paste(output, col, sep=" ")
    }
    output <- paste(output, sep=" ")
    nchar  <- nchar(output[1], type="width")
    line1  <- paste(rep("-", nchar), collapse="")
    output <- format(c(varnames[2], line1, output), justify="centre")
    
    output <- paste(rowcat, output, sep=" ")
    nchar  <- nchar(output[1], type="width")
    line1  <- paste(rep("-", nchar), collapse="")
    
    output <- paste(rowvar, output, sep=" ")
    nchar  <- nchar(output[1], type="width")
    line2  <- paste(rep("=", nchar), collapse="")
    line1  <- paste(rep("-", nchar), collapse="")
    output <- c(line2, output[1:3], line1, output[4:length(output)], line2)
    output <- c(output[1:(length(output)-3)], line1,
                output[(length(output)-2):length(output)])
    return(output)
  }
  
  dim <- dim(x)
  if(length(dim) == 2) {
    # Two dimensional
    output <- twoDimTable(x)
    output <- paste(output, collapse="\n")
    cat(output, fill=TRUE)
    cat("\n")
    cat("Chi-Square Test for Independence", fill=TRUE)
    cat("\n")
    print(summary.table(x))
  } else {
    # Three Dimensional
    stratumcat <- dimnames(x)[[1]]
    stratumcat <- format(stratumcat, justify="left")
    stratumvar <- names(dimnames(x))[1]
    output <- list()
    col    <- list()
    width  <- nchar(as.character(max(x)))
    width  <- ifelse(width > 6, width, 6)
    for(i in seq_len(dim[1])) {
      x.tmp <- as.table(x[i, , ])
      output[[i]] <- twoDimTable(x.tmp, width=width)
    }
    output.header <- output[[1]][2:4]
    output <- lapply(output, function(x) return(x[ -c(1:5, length(x))]))
    for(i in seq_along(output)) {
      col         <- c(stratumcat[i], rep(" ", length(output[[i]])-1))
      output[[i]] <- paste(col, output[[i]], sep=" ")
      nchar  <- nchar(output[[i]][1], type="width")
      line   <- paste(rep("-", nchar), collapse="")
      output[[i]] <- c(output[[i]], line)
    }
    output <- unlist(output)
    output <- output[-length(output)]
    col    <- c(stratumvar, rep(" ", length(output)-1))
    col    <- format(col)
    output <- paste(col, output, sep=" ")
    
    nchar  <- nchar(output[1], type="width")
    line1   <- paste(rep("-", nchar), collapse="")
    line2   <- paste(rep("=", nchar), collapse="")
    output <- c(line2, output.header, line1, output, line2)
    output <- format(output, justify="right")
    output <- paste(output, collapse="\n")
    
    cat(output, fill=TRUE)
    cat("\n")
    cat("Chi-Square Test for Independence", fill=TRUE)
    cat("\n")
    for(i in seq_len(dim[1])) {
      x.tmp <- as.table(x[i, , ])
      cat(sprintf("%s : %s", stratumvar, stratumcat[i]), fill=TRUE)
      cat("\n")
      print(summary.table(x.tmp))
      cat("\n")
    }
  }
}
