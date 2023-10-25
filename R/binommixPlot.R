
#' Visualization of mixture binomial result
#'
#' @param binommixem an output from binommixEM
#'
#' @return a plot visualizing mixture binomial result
#' @export
#' @importFrom stats density
#'
#' @examples u_x <- rBinomMix(1000, alpha=c(0.4, 0.6), theta=c(0.3, 0.8), size=30) ;u_n <- rep(30, 1000)
#' test_case <- binommixEM(u_x, u_n, k=2, pi_inits=c(0.3, 0.7), p_inits=c(0.4, 0.2))
#' fit_plot <- binommixPlot(test_case)
 

binommixPlot <- function(binommixem) {

  
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
  
  x <- data.frame(binommixem$x)
  colnames(x) <- "density"
  
  out_plot <- ggplot2::ggplot(x) +
    ggplot2::geom_density(ggplot2::aes(x=density),
                          colour = "darkgray",
                          fill = "lightgray") +
    ggplot2::theme_minimal()
  
  K <- ncol(binommixem$cluster_ind)
  
  for (j in 1:K) {
    out_x <- binommixem$x * binommixem$cluster_ind[,j]
    out_y <- dbinom(out_x, binommixem$size[1], binommixem$p[j]) 
    
    out_plot <- out_plot + 
      geom_line(data=data.frame(out_x, out_y), 
                aes(x=out_x, y=out_y), 
                color=component_colors[j]) +
      ggplot2::theme_minimal()
  }
  
  return(out_plot)
  
}

# test case
# fit_plot <- binommixPlot(test_case)
