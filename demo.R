# pcasimple demo
install.packages("../pcasimple_0.1.0.tar.gz", repos = NULL)
library(pcasimple)
library(rgl)
library(RColorBrewer)
library(ggplot2)
library(knitr)

### data set embedded in package
names(pima_diabetes)

###Subset dataset with only independent variables
pima_diabetes <- pima_diabetes[,-9]

### plot2D demo
?plot2D #documentation
pima_diabetes_2D <- scale(pima_diabetes[,3:4])
plot2D(data = pima_diabetes_2D)
plot2D(data = pima_diabetes_2D, type = "PC", arrows = TRUE)


### plot3D demo - NOTE: Mac need updated XQuarts
?plot3D #documentation
pima_diabetes_3D <- scale(pima_diabetes[,3:5])

plot3D(data = pima_diabetes_3D)
plot3D(data = pima_diabetes_3D, type = "PC", arrows = TRUE)


### pcvalues demo:
?pcvalues #documentation
pcvalues(data = pima_diabetes, scale = T, value = "eigenvectors")
pcvalues(data = pima_diabetes, scale = T, value = "loadings")
pcvalues(data = pima_diabetes, scale = T, value = "eigenvalues", digits = 3)
pcvalues(data = pima_diabetes, scale = T, value = "variance", digits = 3)


### screeplot demo
?screeplot #documentation
screeplot(data = pima_diabetes, varexplain = 0.80, type = "eigenvalue", scale = TRUE)
screeplot(data = pima_diabetes, varexplain = 0.80, type = "cumulative", scale = TRUE)
screeplot(data = pima_diabetes, varexplain = 0.90, type = "cumulative", scale = TRUE)

vignette("pca-simplified-vignette", package = "pcasimple")

cov(scale(pima_diabetes))
eigen(cov(scale(pima_diabetes)))
