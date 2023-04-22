#' Scree Plot
#'
#'  \code{screeplot()} produces a screeplot which can be customized via its arguments.
#'  The function allows the user to visualize the individual and cumulative variance explained by the principal components.
#'  The arguments within this function assist the user in determining the ideal number of principal components and the
#'  accumulated proportion of variance explained by the selected number of components.
#'
#' @param data dataframe with quantitative variables
#' @param varexplain proportion of variance explained. Default \code{varexplain = 0.80}
#' @param type eigenvalue or cumulative scree plot
#' @param scale boolean. Indicates whether data should be scaled. Default \code{scale = FALSE}
#'
#' @import ggplot2
#'
#' @returns plots
#' @export
#'
#' @examples screeplot(data = pima_diabetes, varexplain = 0.80, type = "eigenvalue", scale = TRUE)
#' screeplot(data = pima_diabetes, varexplain = 0.85, type = "cumulative", scale = TRUE)
screeplot <- function(data = NULL, varexplain = 0.80, type = "cumulative", scale = FALSE) {

  if(!(type == "eigenvalue" || type == "cumulative")) {
    print("Type should either be eigenvalue or cumulative.")
    stop()
  }

  if(scale == TRUE) {
    data <- scale(data)
  }

  lambda <- eigen(cov(data))$values

  if(type == "eigenvalue") {

    plot1 <- qplot(1:ncol(data), lambda) +
      geom_line() +
      xlab("Principal Components") +
      ylab("Eigenvalue") +
      ggtitle("Variance Explained by Principal Component", ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = 1:ncol(data), minor_breaks = 0)

      return(plot1)
  }

  if(type == "cumulative") {

    if(varexplain < 0) {
      print("Enter a value between 0 and 1")
      stop()
    }

    if(varexplain > 1) {
      print("Enter a value between 0 and 1")
      stop()
    }

    var_explain2 <- cumsum(lambda / sum(lambda))

    plot2 <- qplot(1:ncol(data), var_explain2) +
      geom_line() +
      xlab("Principal Components") +
      ylab("Proportion") +
      ggtitle("Accumulated Proportion of Variance Explained", ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylim(0,1) +
      geom_hline(yintercept = varexplain, linetype = "dashed", color = "red") +
      geom_text(aes(0, varexplain, label = varexplain, vjust = -1)) +
      scale_x_continuous(breaks = 1:ncol(data), minor_breaks = 0)

    return(plot2)
  }
}



