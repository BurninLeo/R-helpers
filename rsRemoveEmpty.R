# Remove variables from a data frame df that havecontain no data (NA)
# and/or that contain only a constant datum, if removeConstant is true (default)
# @param data.frame df
# @param logical removeConstant Not only remove NA-only-variables, but also variables with constant data
# @returns data.frame
removeEmpty = function(df, removeConstant=T) {
    n = nrow(df)
    # Can only identify constant values if having at least two columns
    if (n < 2) {
        removeConstant = F
    }
    if (n < 1) {
        return(df)
    }
    skipList = NULL
    
    for (key in names(df)) {
        # Quick check
        if (removeConstant) {
            if (is.na(df[[1, key]]) && is.na(df[[n, key]])) {
                # Same 
            } else if (is.na(df[[1, key]]) || is.na(df[[n, key]])) {
                # Different
                next
            } else if (df[[1, key]] != df[[n, key]]) {
                next
            }
        } else {
            if (!is.na(df[1, key])) {
                next
            }
        }
        # Long check: NA
        data = df[[key]]
        if (!any(!is.na(data))) {
            df[[key]] = NULL
            skipList = c(skipList, key)
            next
        }
        # Long check: Constants
        if (!removeConstant) {
            next
        }
        v0 = data[[1]]
        if (is.na(v0)) {
            # Cannot be constant NA any more, due to above filter
            next
        }
        anyDiff = F
        for (v1 in data[2:n]) {
            if (is.na(v1) || (v1 != v0)) {
                anyDiff = T
                break
            }
        }
        if (!anyDiff) {
            df[[key]] = NULL
            skipList = c(skipList, key)
        }
    }
    if (length(skipList) > 0) {
        message(paste("removeEmpty() removed ", length(skipList), " Variables from data.frame: ", paste(skipList, collapse=", "), sep=""))
    }
    return(df)
}