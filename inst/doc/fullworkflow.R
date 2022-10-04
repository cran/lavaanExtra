## ----cfaplot1, message = FALSE------------------------------------------------
# Load library
library(lavaan)
library(lavaanExtra)

# Define latent variables
latent <- list(visual = paste0("x", 1:3),
               textual = paste0("x", 4:6),
               speed = paste0("x", 7:9))

# Write the model, and check it
cfa.model <- write_lavaan(latent = latent)
cat(cfa.model)

# Fit the model fit and plot with `lavaanExtra::cfa_fit_plot`
# to get the factor loadings visually (optionally as PDF)
fit.cfa <- cfa_fit_plot(cfa.model, HolzingerSwineford1939)

## ----cfa2---------------------------------------------------------------------
# Get fit indices
nice_fit(fit.cfa)

# We can get it prettier with the `rempsyc::nice_table` integration
nice_fit(fit.cfa, nice_table = TRUE)


## ----cfaplot4-----------------------------------------------------------------
# Fit the model fit and plot with `lavaanExtra::cfa_fit_plot`
# to get the factor loadings visually (as PDF)
fit.cfa2 <- cfa_fit_plot(cfa.model, HolzingerSwineford1939,
                         remove.items = paste0("x", c(2:3, 7)))

## ----cfaplot5-----------------------------------------------------------------
nice_fit(fit.cfa, fit.cfa2, nice_table = TRUE)

## ----cfaplot6-----------------------------------------------------------------
# Save fit table as an object
fit_table <- nice_fit(fit.cfa, fit.cfa2, nice_table = TRUE)

# Save fit table to Word!
save_as_docx(fit_table, path = "fit_table.docx")

## ----saturated----------------------------------------------------------------
# Calculate scale averages
data <- HolzingerSwineford1939
data$visual <- rowMeans(data[paste0("x", 1:3)])
data$textual <- rowMeans(data[paste0("x", 4:6)])
data$speed <- rowMeans(data[paste0("x", 7:9)])

# Define our variables
M <- "visual"
IV <- c("ageyr", "grade")
DV <- c("speed", "textual")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV)
regression <- list(speed = IV, textual = IV)
covariance <- list(speed = "textual", ageyr = "grade")

# Write the model, and check it
model.saturated <- write_lavaan(mediation, regression, covariance)
cat(model.saturated)


## ----saturated2---------------------------------------------------------------
# We can run the model again. 
# However, we set `label = TRUE` to get the path names
model.saturated <- write_lavaan(mediation, regression, covariance, label = TRUE)
cat(model.saturated)


## ----saturated3---------------------------------------------------------------
# Define indirect object
indirect <- list(ageyr_visual_speed = c("ageyr_visual", "visual_speed"),
                 ageyr_visual_textual = c("ageyr_visual", "visual_textual"),
                 grade_visual_speed = c("grade_visual", "visual_speed"),
                 grade_visual_textual = c("grade_visual", "visual_textual"))

# Write the model, and check it
model.saturated <- write_lavaan(mediation, regression, covariance, 
                                indirect, label = TRUE)
cat(model.saturated)


## ----letters------------------------------------------------------------------
# Write the model, and check it
model.saturated <- write_lavaan(mediation, regression, covariance, 
                                label = TRUE, use.letters = TRUE)
cat(model.saturated)

## ----letters2-----------------------------------------------------------------
# Define indirect object
indirect <- list(ageyr_visual_speed = c("a_visual", "a_speed"),
                 ageyr_visual_textual = c("a_visual", "a_textual"),
                 grade_visual_speed = c("b_visual", "a_speed"),
                 grade_visual_textual = c("b_visual", "a_textual"))

# Write the model, and check it
model.saturated <- write_lavaan(mediation, regression, covariance, 
                                indirect, label = TRUE, use.letters = TRUE)
cat(model.saturated)


## ----saturated4---------------------------------------------------------------
# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model.saturated <- write_lavaan(mediation, regression, covariance, 
                                indirect, label = TRUE)
cat(model.saturated)


## ----saturated5---------------------------------------------------------------
# Fit the model with `lavaan`
fit.saturated <- sem(model.saturated, data = data)

# Get regression parameters only 
# And make it pretty with the `rempsyc::nice_table` integration
lavaan_reg(fit.saturated, nice_table = TRUE, highlight = TRUE)


## ----path---------------------------------------------------------------------
regression <- list(speed = "grade", textual = IV)

# We can run the model again, setting `label = TRUE` to get the path names
model.path <- write_lavaan(mediation, regression, covariance, label = TRUE)
cat(model.path)
# We check that we have removed "ageyr" correctly from "speed" in the 
# regression section. OK.

# Define just our indirect effects of interest
indirect <- list(age_visual_speed = c("ageyr_visual", "visual_speed"),
                 grade_visual_textual = c("grade_visual", "visual_textual"))

# We run the model again, with the indirect effects
model.path <- write_lavaan(mediation, regression, covariance, 
                           indirect, label = TRUE)
cat(model.path)

# Fit the model with `lavaan`
fit.path <- sem(model.path, data = data)

# Get regression parameters only
lavaan_reg(fit.path)

# We can get it prettier with the `rempsyc::nice_table` integration
lavaan_reg(fit.path, nice_table = TRUE, highlight = TRUE)

# We only kept significant regressions. Good (for this demo).

# Get covariance indices
lavaan_cov(fit.path)

# We can get it prettier with the `rempsyc::nice_table` integration
lavaan_cov(fit.path, nice_table = TRUE)

# Get nice fit indices with the `rempsyc::nice_table` integration
nice_fit(fit.cfa, fit.saturated, fit.path, nice_table = TRUE)

# Let's get the indirect effects only
lavaan_ind(fit.path)

# We can get it prettier with the `rempsyc::nice_table` integration
lavaan_ind(fit.path, nice_table = TRUE)

# Get modification indices only
modindices(fit.path, sort = TRUE, maximum.number = 5)

## ----nice_tidySEM-------------------------------------------------------------
labels <- list(ageyr = "Age (year)",
               grade = "Grade",
               visual = "Visual",
               speed = "Speed",
               textual = "Textual")
layout <- list(IV = IV, M = M, DV = DV)

nice_tidySEM(fit.path, layout = layout, label = labels,
             hide_nonsig_edges = TRUE)


## ----latent-------------------------------------------------------------------
model.latent <- write_lavaan(mediation, regression, covariance, 
                             indirect, latent, label = TRUE)
cat(model.latent)

# Run model
fit.latent <- sem(model.latent, data = HolzingerSwineford1939)

# Get nice fit indices with the `rempsyc::nice_table` integration
nice_fit(fit.cfa, fit.saturated, fit.path, fit.latent, nice_table = TRUE)


