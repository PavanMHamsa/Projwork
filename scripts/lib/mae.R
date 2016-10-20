mae <- function(pred, obs, na.rm = FALSE) {
    # Calculate mean absolute error
    # See https://www.kaggle.com/wiki/MeanAbsoluteError
    # Example: mae(yhat, y)
    mean(abs(pred, obs), na.rm = na.rm)
}
