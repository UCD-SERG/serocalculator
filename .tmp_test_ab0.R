devtools::load_all(quiet = TRUE)
t <- seq(0, 730, by = 7)
cp <- list(y0 = 0.5, y1 = 5, t1 = 30, alpha = 0.01, r = 1.3)
withCallingHandlers({
  out <- ab0(t, cp)
  cat("no warning triggered, length:", length(out), "\n")
  print(out)
}, warning = function(w) {
  cat("WARNING:", conditionMessage(w), "\n")
  invokeRestart("muffleWarning")
})
