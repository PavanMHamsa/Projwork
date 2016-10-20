summaryMAE <- function (data, lev = NULL, model = NULL) {
    # Summary function for the caret package.skeleton
    # Uses the mean absolute error
    # See http://stackoverflow.com/a/22508678/1427069
    # See mae.R
    out <- mae(data$pred, data$obs)  
    names(out) <- "MAE"
    out
}
