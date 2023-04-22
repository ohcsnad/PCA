#' Plot 2D
#'
#' \code{plot2D()} produces a 2D scatter plot of the data, which can be customized via its arguments.
#' This function displays a scatter plot of the data with arguments to change orientation of the axes and overlay principle component vectors.
#'
#' @param data dataframe with 2 variables
#' @param type scatterplot type, either original or principle component coordinates
#' @param arrows boolean variable. Indicates whether or not to plot arrows
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @return NULL
#' @export
#'
#' @examples pima_diabetes_2D <- scale(pima_diabetes[,1:2])
#' plot2D(data = pima_diabetes_2D)
#' plot2D(data = pima_diabetes_2D, type = "PC")
#' plot2D(data = pima_diabetes_2D, type = "PC", arrows = TRUE)
plot2D <- function(data = NULL, type = "original", arrows = FALSE) {

  if (ncol(data) != 2) {
    print("Data should contain two variables.")
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
    plot(data, asp = 1, main = "Scatterplot in Original Coordinates")

    if (arrows == TRUE) {
      pr_comp <- t(-sqrt(eq_eigen$values) * t(eq_eigen$vectors))
      arrows(x0 = 0, y0 = 0, x1 = pr_comp[1, 1], y1 = pr_comp[2, 1],
             lwd = 3, col = brewer.pal(3, "Set1")[1])
      arrows(x0 = 0, y0 = 0, x1 = pr_comp[1, 2], y1 = pr_comp[2, 2],
             lwd = 3, col = brewer.pal(3, "Set1")[2])
      legend("topright", legend = c("PC1", "PC2"),
             col = brewer.pal(3, "Set1")[1:2], lty = 1, lwd = 2)
    }
  }

  if (type == "PC") {
    plot(data %*% eq_eigen$vectors, asp = 1,
         main = "Scatterplot in Principal Component Coordinates",
         xlab = "PC1", ylab = "PC2")

    if (arrows == TRUE) {
      pr_comp <- t(-sqrt(eq_eigen$values) * t(eq_eigen$vectors))
      arrows(x0 = 0, y0 = 0, x1 = sqrt(eq_eigen$values[1]), y1 = 0,
             lwd = 3, col = brewer.pal(3, "Set1")[1])
      arrows(x0 = 0, y0 = 0, x1 = 0, y1 = sqrt(eq_eigen$values[2]),
             lwd = 3, col = brewer.pal(3, "Set1")[2])
      legend("topright", legend = c("PC1", "PC2"),
             col = brewer.pal(3, "Set1")[1:2], lty = 1, lwd = 2)
    }
  }
}
