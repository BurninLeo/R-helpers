# Recode values in x
# @param vector x
# @param named vector codes Recode the names to the values
# @param logical copyOther Copy values that are not listes in codes
recode = function(x, codes, copyOther=F) {
    wasFactor = is.factor(x)
    
    # Create empty result (or use the previous values)
    if (copyOther) {
        y = as.character(x)
    } else {
        y = rep(NA, length(x))
        names(y) = names(x)
    }
    
    # Always work with characters
    x = as.character(x)
    keys = names(codes)
    
    # Check for non-listes vales
    if (!copyOther) {
        hads = unique(x)
        hads = hads[!is.na(hads)]
        miss = hads[!(hads %in% keys)]
        if (length(miss) > 0) {
            warning(paste(c("Values in x will be recoded to NA:", miss), collapse="\n  - "))
        }
    }
    
    # Recode
    for (key in keys) {
        y[which(x == key)] = codes[[key]]
    }
    # Recreate factor, if necessary
    if (wasFactor) {
        y = as.factor(y)
    }
    
    return(y)
}

if (F) {
    test = as.factor(c("A" = "Alpha", "B" = "Beta", "Gamma", "Delta", NA, "Alpha"))
    res = recode(test, c(
        "Alpha" = "AA",
        "Beta" = "BB",
        "Gamma" = "CC"
    ), T)
    print(test)
    print(res)
}
