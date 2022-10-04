## ---- message = FALSE---------------------------------------------------------
library(lavaan)
library(lavaanExtra)

## -----------------------------------------------------------------------------
# Calculate scale averages
data <- HolzingerSwineford1939
data$visual <- rowMeans(data[paste0("x", 1:3)])
data$textual <- rowMeans(data[paste0("x", 4:6)])
data$speed <- rowMeans(data[paste0("x", 7:9)])

# Define our variables
IV <- "ageyr"
M <- "visual"
DV <- "speed"

# Define our lavaan lists
mediation <- list(speed = M, visual = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)


## -----------------------------------------------------------------------------
# Define our variables
DV <- c("speed", "textual")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
M <- c("visual", "grade")
DV <- "speed"

# Define our lavaan lists
mediation <- list(speed = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
DV <- c("speed", "textual")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
IV <- c("sex", "ageyr")
M <- "visual"
DV <- "speed"

# Define our lavaan lists
mediation <- list(speed = M, visual = IV)

# Define indirect object
indirect <- list(M = M, DV = DV, IV = IV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
DV <- c("speed", "textual")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV)

# Define indirect object
indirect <- list(M = M, DV = DV, IV = IV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
M <- c("visual", "grade")
DV <- "speed"

# Define our lavaan lists
mediation <- list(speed = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
DV <- c("speed", "textual")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
IV <- c("sex", "ageyr", "agemo")
DV <- "speed"

# Define our lavaan lists
mediation <- list(speed = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect)

## -----------------------------------------------------------------------------
# Define our variables
DV <- c("speed", "textual")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)

nice_tidySEM(fit, layout = indirect, hide_nonsig_edges = TRUE)

## -----------------------------------------------------------------------------
# Define our variables
IV <- c("sex", "ageyr", "agemo", "school", "x2", "x3")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)


## ---- fig.height = 6----------------------------------------------------------
nice_tidySEM(fit, layout = indirect, hide_nonsig_edges = TRUE)

## -----------------------------------------------------------------------------
# Define our variables
M <- c("visual", "grade", "x8")
DV <- c("speed", "textual", "x4", "x5", "x7")

# Define our lavaan lists
mediation <- list(speed = M, textual = M, x4 = M, x5 = M,
                   x7 = M, x8 = IV, visual = IV, grade = IV)

# Define indirect object
indirect <- list(IV = IV, M = M, DV = DV)

# Write the model, and check it
model <- write_lavaan(mediation, indirect = indirect, label = TRUE)
cat(model)

# Fit and plot
fit <- sem(model, data = data)
nice_lavaanPlot(fit)


## ---- fig.height = 8----------------------------------------------------------
labels <- list(sex = "Sex",
               ageyr = "Age (year)",
               agemo = "Age (month)",
               school = "School",
               x2 = "Item 2",
               x3 = "Item 3",
               visual = "Visual",
               grade = "Grade",
               x8 = "Item 8",
               speed = "Speed",
               textual = "Textual",
               x4 = "Item 4",
               x5 = "Item 5",
               x7 = "Item 7")

nice_tidySEM(fit, layout = indirect, hide_nonsig_edges = TRUE, label = labels)


