## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pcasimple)

## -----------------------------------------------------------------------------
pima_diabetes <- pima_diabetes[,-9]

## ---- echo = F----------------------------------------------------------------
knitr::opts_chunk$set(fig.width=8, fig.height=6) 

## -----------------------------------------------------------------------------
pima_scale <- scale(pima_diabetes)
plot2D(pima_scale)

## -----------------------------------------------------------------------------
pima_subset <- subset(pima_scale, select = c("Glucose", "BloodPressure"))
plot2D(pima_subset)

## -----------------------------------------------------------------------------
plot2D(pima_subset, type = "PC")

## -----------------------------------------------------------------------------
plot2D(pima_subset, arrows = TRUE)

plot2D(pima_subset, type = "PC", arrows = TRUE)

## -----------------------------------------------------------------------------
pima_subset_3 <- subset(pima_scale, select = c("Glucose", "BloodPressure", "BMI"))

plot3D(pima_subset_3)

## -----------------------------------------------------------------------------
plot3D(pima_subset_3, type = "PC")

## -----------------------------------------------------------------------------
plot3D(pima_subset_3, arrows = TRUE)

## -----------------------------------------------------------------------------
plot3D(pima_subset_3, type = "PC", arrows = TRUE)

## -----------------------------------------------------------------------------
pcvalues(data = pima_diabetes, scale = TRUE, value = "eigenvectors")
prcomp(scale(pima_diabetes))

## -----------------------------------------------------------------------------
pcvalues(data = pima_diabetes, scale = TRUE, value = "loadings")

## -----------------------------------------------------------------------------
pcvalues(data = pima_diabetes, scale = TRUE, value = "eigenvalues")

## -----------------------------------------------------------------------------
pcvalues(data = pima_diabetes, scale = TRUE, value = "variance")

## -----------------------------------------------------------------------------
screeplot(pima_diabetes, type = "eigenvalue", scale = T)

## -----------------------------------------------------------------------------
screeplot(pima_diabetes, type = "cumulative", scale = T)

## -----------------------------------------------------------------------------
screeplot(pima_diabetes, varexplain = 0.90, type = "cumulative", scale = T)

