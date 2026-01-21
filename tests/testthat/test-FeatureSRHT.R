test_that("sanity check", {
  set.seed(42)
  N <- 5000
  p <- 100
  
  X <- matrix(rnorm(N * p, mean = 0, sd = 1), nrow = N, ncol = p)
  y <- rnorm(N, mean = 0, sd = 1)
  
  dat = data.frame(X, y)
  data.table::fwrite(dat, file = "temp_test_data.csv", row.names = FALSE, col.names = FALSE)

  # Run both methods
  res1 <- FeatureSRHT(X = X, y = y, r = 50, bins = 10)
  res2 <- FeatureSRHT(csv = "temp_test_data.csv", r = 50, bins = 10, header = FALSE)

  # Compare results (checking 'Top-r' method as proxy for all)
  expect_equal(res1$`Top-r`$Indices, res2$`Top-r`$Indices)
  expect_equal(res1$`Top-r`$Coefficients, res2$`Top-r`$Coefficients)
  expect_equal(res1$`Top-r`$R_Squared, res2$`Top-r`$R_Squared)

  file.remove("temp_test_data.csv")
})


test_that("Time Test", {
  N <- 50000
  p <- 250
  r <- 50
  
  X = matrix(rnorm(N * p, 0, 10), N, p)
  y = rnorm(N)

  start <- Sys.time()
  res <- FeatureSRHT(X = X, y = y, r = r)
  end <- Sys.time()

  # Verify we got results for all 4 methods
  expect_named(res, c("Uniform", "Top-r", "Leverage", "Supervised"))
  
  # Verify dimensions for one method
  expect_equal(length(res$`Top-r`$Indices), r)
  
  cat("\nRows:", N, " Cols:", p)
  cat("\nTime taken:", end - start, "\n")
})
