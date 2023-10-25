
#' plotting original observation mixture Poisson data with after clustering
#'
#' @param poismixem output from the function poissonEM in mixtureEM package
#'
#' @return a plot visualizing mixture poisson result
#' @export
#' @importFrom stats dpois
#' @importFrom stats rpois
#' @import ggplot2
#' @examples
poismixPlot <- function(poismixem) {
  
  component_colors <- c(
    "red",
    "blue",
    "green",
    "yellow",
    "orange",
    "purple",
    "darksalmon",
    "goldenrod2",
    "dodgerblue",
    "darkorange3",
    "burlywood4",
    "darkmagenta",
    "firebrick",
    "deeppink2",
    "darkseagreen1"
  )
  simudata <- c()
  label <- c()
  for (i in 1:length(poismixem$pi)) {
    simudata <- c(simudata, rpois(length(poismixem$x)*poismixem$pi[i], poismixem$lambda[i]))
    label <- c(label, rep(i, length(poismixem$x)*poismixem$pi[i]))
  }
  sim <- data.frame(cbind(simudata, label))
  
  dat<- data.frame(poismixem$x)
  lambda <-
  colnames(dat) <- "lambda"
  
  out_plot <- ggplot2::ggplot(dat) +
    ggplot2::geom_density(ggplot2::aes(x = lambda),
                          colour = "darkgray",
                          fill = "lightgray"
    )
  
  K = length(poismixem$pi)
  for (j in 1:K) {
    out_x <- sim[label==j,1]
    out_y <- dpois(out_x, poismixem$lambda[j])/K
    out_plot <- out_plot + 
      ggplot2::geom_line(data=data.frame(out_x, out_y), 
                         ggplot2::aes(x=out_x, y=out_y), 
                         color=component_colors[j]) +
      ggplot2::theme_minimal()
  }
  return(out_plot)
}
