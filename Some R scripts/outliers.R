detect_outliers <- function(model, data) {
    leverage <- hatvalues(model); student_resid <- rstudent(model); cook_d <- cooks.distance(model)
    cat("Number of model coefficients:", length(coef(model)), "\n")
    cat("Average leverage:", length(coef(model)) / nrow(data), "\nHighest leverage:", max(leverage), "\n\n")
    if (length(high_residuals <- which(abs(student_resid) > 4)) > 0) cat("Studentized residual > 4:", high_residuals, "\n") else cat("No studentized residual > 4.\n")
    if (length(high_cook_d <- which(cook_d > 2)) > 0) cat("Cook's distance > 2:", high_cook_d, "\n") else cat("No Cook's distance > 2.\n")}
