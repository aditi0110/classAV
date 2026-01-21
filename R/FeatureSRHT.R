#' Run FeatureSRHT Algorithm
#'
#' Runs variants of Feature Subsampled Randomized Hadamard Transform (FeatureSRHT)
#' for dimensionality reduction and regression.
#'
#' @param X Numeric matrix of predictors.
#' @param y Numeric vector of response variable.
#' @param csv Path to CSV file (alternative to X, y). Last column is response.
#' @param r Integer; target dimension size (features to select).
#' @param bins Integer; bins for Supervised method.
#' @param header Logical; if csv has header.
#' @param X_test Numeric matrix; optional test predictors.
#' @param y_test Numeric vector; optional test response.
#' @param method Character; Which methods to run. Options: "uniform" (default),
#'   "top_r", "leverage", "supervised", or "all".
#'
#' @useDynLib FeatureSRHT, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom data.table fread
#'
#' @return A list containing results for the selected methods.
#' @export
FeatureSRHT <- function(X = NULL, y = NULL, csv = NULL, 
                        r = 50, bins = 10, header = FALSE,
                        X_test = NULL, y_test = NULL,
                        method = "uniform") {

  # --- Method Parsing ---
  valid_methods <- c("uniform", "top_r", "leverage", "supervised", "all")
  method <- match.arg(method, valid_methods, several.ok = TRUE)

  if ("all" %in% method) {
      run_uni <- TRUE; run_top <- TRUE; run_lev <- TRUE; run_sup <- TRUE
  } else {
      run_uni <- "uniform" %in% method
      run_top <- "top_r" %in% method
      run_lev <- "leverage" %in% method
      run_sup <- "supervised" %in% method
  }

  # --- Data Loading ---
  if (!is.null(csv)) {
    if (!file.exists(csv)) stop("CSV file doesn't exist.")
    if (!is.null(X) || !is.null(y)) stop("Provide csv OR {X,y}.")
    dat <- data.table::fread(csv, header = header)
    dat <- as.matrix(dat)
    X <- dat[, -ncol(dat), drop = FALSE]
    y <- dat[, ncol(dat)]
  } else {
    if (is.null(X) || is.null(y)) stop("Provide csv OR {X, y}.")
    if (!is.matrix(X)) X <- as.matrix(X)
  }

  if (nrow(X) != length(y)) stop("Row mismatch in Train data.")

  # --- Test Data Check ---
  if (!is.null(X_test) && !is.null(y_test)) {
      if (!is.matrix(X_test)) X_test <- as.matrix(X_test)
      if (nrow(X_test) != length(y_test)) stop("Row mismatch in Test data.")
      if (ncol(X_test) != ncol(X)) stop("Column mismatch between Train and Test.")
  }

  if (r > ncol(X)) r <- ncol(X)

  # --- Execution ---
  res_list <- run_featuresrht_wrapper(X, y, X_test, y_test, 
                                      as.integer(r), as.integer(bins),
                                      run_uni, run_top, run_lev, run_sup)
  
  # Convert Indices to 1-based
  for (nm in names(res_list)) {
      res_list[[nm]]$Indices <- res_list[[nm]]$Indices + 1
  }

  return(res_list)
}
