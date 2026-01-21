test_that("sanity check", {
  set.seed(42)
  N <- 1000; p <- 50
  X <- matrix(rnorm(N * p), nrow = N, ncol = p)
  y <- rnorm(N)
  
  dat = data.frame(X, y)
  data.table::fwrite(dat, file = "temp_test_data.csv", row.names = FALSE, col.names = FALSE)

  # Check Default (Should only return Uniform)
  res_def <- FeatureSRHT(X = X, y = y, r = 10)
  expect_named(res_def, "Uniform")
  expect_false("Top-r" %in% names(res_def))

  # Check "all" via CSV
  res_all <- FeatureSRHT(csv = "temp_test_data.csv", r = 10, header = FALSE, method = "all")
  expect_named(res_all, c("Uniform", "Top-r", "Leverage", "Supervised"), ignore.order = TRUE)

  # Check specific selection
  res_sel <- FeatureSRHT(X = X, y = y, r = 10, method = c("top_r", "leverage"))
  expect_named(res_sel, c("Top-r", "Leverage"), ignore.order = TRUE)

  file.remove("temp_test_data.csv")
})

test_that("Time Test", {
  N <- 50000; p <- 250; r <- 50
  X = matrix(rnorm(N * p, 0, 10), N, p)
  y = rnorm(N)

  start <- Sys.time()
  # Running "all" for full benchmark comparison
  res <- FeatureSRHT(X = X, y = y, r = r, method = "all")
  end <- Sys.time()

  expect_true(length(res$`Top-r`$Indices) == r)
  
  cat(length(res$`Top-r`$Indices))
  cat("\nTime taken:", end - start, "\n")
})
