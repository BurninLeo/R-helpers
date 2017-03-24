# Better variables view
Varlist = function(sia) {
    # Init varlist output
    varlist = data.frame(row.names = names(sia))
    varlist[["comment"]] = NA
    varlist[["type"]] = NA
    varlist[["values"]] = NA
    varlist[["NAs"]] = NA
    # Fill with meta information
    for (var in names(sia)) {
        if (!is.null(comment(sia[[var]]))) {
            varlist[[var, "comment"]] = comment(sia[[var]])
        }
        varlist[[var, "NAs"]] = sum(is.na(sia[[var]]))
        if (is.factor(sia[[var]])) {
            varlist[[var, "type"]] = "factor"
            varlist[[var, "values"]] = paste(levels(sia[[var]]), collapse=", ")
        } else if (is.character(sia[[var]])) {
            varlist[[var, "type"]] = "character"
        } else if (is.logical(sia[[var]])) {
            varlist[[var, "type"]] = "logical"
            n = sum(!is.na(sia[[var]]))
            if (n > 0) {
                varlist[[var, "values"]] = paste(round(sum(sia[[var]], na.rm=T) / n * 100), "% TRUE", sep="")
            }
        } else if (is.numeric(sia[[var]])) {
            varlist[[var, "type"]] = typeof(sia[[var]])
            n = sum(!is.na(sia[[var]]))
            if (n > 0) {
                varlist[[var, "values"]] = paste(min(sia[[var]], na.rm=T), "...", max(sia[[var]], na.rm=T))
            }
        } else {
            varlist[[var, "type"]] = typeof(sia[[var]])
        }
    }
    View(varlist)
}