summaryMAE <- function (data, lev = NULL, model = NULL) {
    # Summary function for the caret package
    # Uses the mean absolute error
    # See http://stackoverflow.com/a/22508678/1427069
    out <- mean(abs(data$obs - data$pred), na.rm = TRUE)
    names(out) <- "MAE"
    out
}
