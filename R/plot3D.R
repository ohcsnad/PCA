#' Plot 3D
#'
#'  \code{plot3D()} produces an 3D interactive scatter plot of the data, which can be customized via its arguments.
#'  This function displays an interactive scatter plot of the data with arguments to change orientation of the axes
#'  and overlay principle component vectors.
#'
#' @param data dataframe with 3 variables
#' @param type scatterplot type, either original or principle component coordinates
#' @param arrows boolean variable. Indicates whether or not to plot arrows
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rgl plot3d arrow3d legend3d
#'
#' @return NULL
#' @export
#'
#' @examples pima_diabetes_3D <- scale(pima_diabetes[,3:5])
#' plot3D(data = pima_diabetes_3D)
#' plot3D(data = pima_diabetes_3D, type = "PC")
#' plot3D(data = pima_diabetes_3D, type = "PC", arrows = TRUE)
plot3D <- function(data = NULL, type = "original", arrows = FALSE) {

  if (ncol(data) != 3) {
    print("Data should contain three variables.")
    return(NULL)
  }

  if (!(type == "original" || type == "PC")) {
    print("Type should either be original or PC.")
    return(NULL)
  }

  if (typeof(arrows) != "logical") {
    print("Arrow should be logical TRUE or FALSE.")
    return(NULL)
  }

  eq_cov <- cov(data)
  eq_eigen <- eigen(eq_cov)

  if (type == "original") {
    plot3d(data)

    if (arrows == TRUE) {
      pr_comp <- t(-sqrt(eq_eigen$values) * t(eq_eigen$vectors))
      arrow3d(p0 = c(0, 0, 0), p1 = c(pr_comp[1, 1], pr_comp[2, 1], pr_comp[3, 1]),
              col = brewer.pal(3, "Set1")[1])
      arrow3d(p0 = c(0, 0, 0), p1 = c(pr_comp[1, 2], pr_comp[2, 2], pr_comp[3, 2]),
              col = brewer.pal(3, "Set1")[2])
      arrow3d(p0 = c(0, 0, 0), p1 = c(pr_comp[1, 3], pr_comp[2, 3], pr_comp[3, 3]),
              col = brewer.pal(3, "Set1")[3])
      legend3d("topright", legend = c("PC1", "PC2", "PC3"),
               pch = 16, col = brewer.pal(3, "Set1"), cex=1, inset=c(0.02))
    }
  }

  if (type == "PC") {
    plot3d(data %*% eq_eigen$vectors, xlab = "PC1", ylab = "PC2", zlab = "PC3")

    if (arrows == TRUE) {
      pr_comp <- t(-sqrt(eq_eigen$values) * t(eq_eigen$vectors))
      arrow3d(p0 = c(0, 0, 0), p1 = c(sqrt(eq_eigen$values[1]), 0, 0),
              col = brewer.pal(3, "Set1")[1])
      arrow3d(p0 = c(0, 0, 0), p1 = c(0, sqrt(eq_eigen$values[2]), 0),
              col = brewer.pal(3, "Set1")[2])
      arrow3d(p0 = c(0, 0, 0), p1 = c(0, 0, sqrt(eq_eigen$values[3])),
              col = brewer.pal(3, "Set1")[3])
      legend3d("topright", legend = c("PC1", "PC2", "PC3"),
               pch = 16, col = brewer.pal(3, "Set1"), cex=1, inset=c(0.02))
    }
  }
}
