## ----prep, message = FALSE----------------------------------------------------
library(lavaan)
library(lavaanPlot)
model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'
fit <- sem(model, data = mtcars)

## ----plot---------------------------------------------------------------------
lavaanPlot(model = fit, edge_options = list(color = "grey"), coefs = FALSE,
           node_options = list(shape = "box", fontname = "Helvetica"))

