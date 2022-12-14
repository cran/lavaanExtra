## ----reg, message = FALSE-----------------------------------------------------
# Load libraries
library(lavaan)
library(lavaanExtra)

# Define our regression terms
regression <- list(mpg = names(mtcars)[2:5],
                   disp = names(mtcars)[4:7])

# Write the model, and check it
mtcars.model <- write_lavaan(regression = regression)
cat(mtcars.model)

# Fit the model with `lavaan`
fit.reg <- sem(mtcars.model, data = mtcars)

# Get regression parameters only
lavaan_reg(fit.reg)

# We can get it prettier with the `rempsyc::nice_table` integration
lavaan_reg(fit.reg, nice_table = TRUE, highlight = TRUE)


