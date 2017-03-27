writeForeignMySPSS = function (df, datafile, codefile, varnames = NULL, len = 32767) {
    adQuote <-  function (x) paste("\"", x, "\"", sep = "")
    
    # Last variable must not be empty for DATA LIST
    if (any(is.na(df[[length(df)]]))) {
        df$END_CASE = 0
    }
    
    # http://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
    decimalplaces <- function(x) {
        y = x[!is.na(x)]
        if (length(y) == 0) {
            return(0)
        }
        if (any((y %% 1) != 0)) {
            info = strsplit(sub('0+$', '', as.character(y)), ".", fixed=TRUE)
            info = info[sapply(info, FUN=length) == 2]
            if (length(info) >= 2) {
              dec = nchar(unlist(info))[seq(2, length(info), 2)]
            } else {
              return(0)
            }
            return(max(dec, na.rm=T))
        } else {
            return(0)
        }
    }
    
    dfn <- lapply(df, function(x) if (is.factor(x))
        as.numeric(x)
        else x)
    
    # Boolean variables (dummy coding)
    bv = sapply(dfn, is.logical)
    for (v in which(bv)) {
        dfn[[v]] = ifelse(dfn[[v]], 1, 0)
    }
    
    varlabels <- names(df)
    # Use comments where applicable
    for (i in 1:length(df)) {
      cm = comment(df[[i]])
      if (is.character(cm) && (length(cm) > 0)) {
        varlabels[i] = comment(df[[i]])
      }
    }
    
    if (is.null(varnames)) {
        varnames <- abbreviate(names(df), 8L)
        if (any(sapply(varnames, nchar) > 8L))
            stop("I cannot abbreviate the variable names to eight or fewer letters")
        if (any(varnames != varlabels))
            warning("some variable names were abbreviated")
    }
    varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
    dl.varnames <- varnames
    chv = sapply(df, is.character)
    if (any(chv)) {
        for (v in which(chv)) {
            dfn[[v]] = gsub("\\s", " ", dfn[[v]])
        }
        lengths <- sapply(df[chv], function(v) max(nchar(v), na.rm=T))
        if (any(lengths > len)) {
            warning(paste("Clipped strings in", names(df[chv]), "to", len, "characters"))
            for (v in which(chv)) {
                df[[v]] = substr(df[[v]], start=1, stop=len)
            }
        }
        lengths[is.infinite(lengths)] = 0
        lengths[lengths < 1] = 1
        lengths <- paste("(A", lengths, ")", sep = "")
        # star <- ifelse(c(FALSE, diff(which(chv) > 1)), " *",
        dl.varnames[chv] <- paste(dl.varnames[chv], lengths)
    }
    
    # Dates
    is.POSIXct = function(x) {
      inherits(x, "POSIXct")
    }
    chd = sapply(df, is.POSIXct)
    if (any(chd)) {
      for (v in which(chd)) {
        dfn[[v]] = format(dfn[[v]], format="%d-%m-%Y %H:%M:%S")
      }
      lengths = rep("DATE", length(df[chd]))
      dl.varnames[chd] = paste(dl.varnames[chd], " (DATETIME)", sep="")
    }
    
    # decimals and bools
    nmv = sapply(df, is.numeric)
    dbv = sapply(df, is.numeric)
    factors <- sapply(df, is.factor)
    nv = (nmv | dbv)
    if (any(nv)) {
      decimals = sapply(df[nv], FUN=decimalplaces)
      # if (length(decimals) == 0) {
      dl.varnames[nv] = paste(dl.varnames[nv], " (F", decimals+8, ".", decimals, ")", sep="")
      if (length(bv) > 0) {
          dl.varnames[bv] = paste(dl.varnames[bv], "(F1.0)")
      }
    }
    
    rmv = !(chv | nv | bv | chd)
    if (length(rmv) > 0) {
      dl.varnames[rmv] = paste(dl.varnames[rmv], "(F8.0)")
    }
    
    # Breaks in output
    brv = seq(1, length(dl.varnames), 10)
    dl.varnames[brv] = paste(dl.varnames[brv], "\n", sep=" ")
    
    cat("SET LOCALE = ENGLISH.\n", file = codefile)
    cat("DATA LIST FILE=", adQuote(datafile), " free (TAB)\n", file = codefile, append = TRUE)
    cat("/", dl.varnames, " .\n\n", file = codefile, append = TRUE)
    cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
    cat(paste(varnames, adQuote(varlabels), "\n"), ".\n", file = codefile,
        append = TRUE)
    
    if (any(factors)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for (v in which(factors)) {
            cat("/\n", file = codefile, append = TRUE)
            cat(varnames[v], " \n", file = codefile, append = TRUE)
            levs <- levels(df[[v]])
            cat(paste(1:length(levs), adQuote(levs), "\n", sep = " "),
                file = codefile, append = TRUE)
        }
        cat(".\n", file = codefile, append = TRUE)
    }
    
    # Labels stored in attr()
    attribs <- !unlist(lapply(sapply(df, FUN=attr, which="1"), FUN=is.null))
    if (any(attribs)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for (v in which(attribs)) {
            cat("/\n", file = codefile, append = TRUE)
            cat(varnames[v], " \n", file = codefile, append = TRUE)
            # Check labeled values
            tc = list()
            for (tcv in dimnames(table(df[[v]]))[[1]]) {
                if (!is.null(tcl <- attr(df[[v]], tcv))) {
                    tc[tcv] = tcl
                }
            }
            cat(paste(names(tc), tc, "\n", sep = " "),
                file = codefile, append = TRUE)
        }
        cat(".\n", file = codefile, append = TRUE)
    }
    
    ordinal <- sapply(df, is.ordered)
    if (any(ordinal)) {
        tmp = varnames[ordinal]
        brv = seq(1, length(tmp), 10)
        tmp[brv] = paste(tmp[brv], "\n")
        cat(paste("\nVARIABLE LEVEL", paste(tmp, collapse=" "), "(ORDINAL).\n"),
            file = codefile, append = TRUE)
    }
    num <- sapply(df, is.numeric)
    if (any(num)) {
        tmp = varnames[num]
        brv = seq(1, length(tmp), 10)
        tmp[brv] = paste(tmp[brv], "\n")
        cat(paste("\nVARIABLE LEVEL", paste(tmp, collapse=" "), "(SCALE).\n"),
            file = codefile, append = TRUE)
    }
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
    
    write.table(dfn, file = datafile, row = FALSE, col = FALSE,
                sep = "\t", quote = F, na = "", eol = "\n", fileEncoding="UTF-8")
}