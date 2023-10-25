#' Visualization of EM optimization result of mixture of normal
#'
#' @param normalmixem The object of EM optimization result (The return object of function
#' mixnormal_EM).
#'
#' @return A plot of mixture normal distributions
#' @export
#' @importFrom stats density
#'
#' @examples
#' simu_x <- simu_mixnormal(1000)
#' test_case <- mixnormal_EM(simu_x$simObs$normObs,
#'   theta_vector = c(0.2, 0.8),
#'   mu_vector = c(0.5, 2), sigma_vector = c(2, 1)
#' )
#' normalmix_plot(test_case)
normalmix_plot <- function(normalmixem) {
  plot_mix_normal <- function(x, mu = NULL, sigma = NULL, lam = 1) {
    if (is.null(mu) | is.null(sigma) | is.null(lam)) {
      stop("Error: Check the input.")
    }
    lam * stats::dnorm(x, mean = mu, sd = sigma)
  }
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

  x <- data.frame(normalmixem$y)
  colnames(x) <- "density"

  out_plot <- ggplot2::ggplot(x) +
    ggplot2::geom_density(ggplot2::aes(x = density),
      colour = "darkgray",
      fill = "lightgray"
    ) +
    ggplot2::theme_minimal()

  K <- length(normalmixem$theta)

  for (i in 1:K) {
    out_plot <- out_plot +
      ggplot2::stat_function(
        geom = "line",
        fun = plot_mix_normal,
        args = list(
          mu = normalmixem$mu[i],
          sigma = normalmixem$sigma[i],
          lam = normalmixem$theta[i]
        ),
        colour = component_colors[i], lwd = 1
      ) +
      ggplot2::ylab("Density") +
      ggplot2::theme_minimal()
  }

  return(out_plot)
}
