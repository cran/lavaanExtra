## ---- message = FALSE---------------------------------------------------------
library(lavaanExtra)

## -----------------------------------------------------------------------------
intercept <- c("mpg", "cyl", "disp")
constraint.equal <- list(b1 = "(b2 + b3)^2")
constraint.smaller <- list(b1 = "exp(b2 + b3)")
constraint.larger <- list(b1 = "exp(b2 + b3)")
custom <- "y1 + y2 ~ f1 + f2 + x1 + x2"

model.custom <- write_lavaan(
  intercept = intercept, constraint.equal = constraint.equal, 
  constraint.smaller = constraint.smaller, 
  constraint.larger = constraint.larger, custom = custom)
cat(model.custom)

