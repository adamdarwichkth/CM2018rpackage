# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

ad_trial_data <- function(n_per_arm){

  n_samples <- n_per_arm

  x_nfl_control <- 3.354375
  sd_nfl_control <- 0.4660144
  e_nfl <- 0.67
  x_nfl_trt <- log(exp(x_nfl_control) * e_nfl)
  x_ptau181_control <- 0.5297517
  sd_ptau181_control <- 0.5347177
  e_ptau181 <- 0.81
  x_ptau181_trt <- log(exp(x_ptau181_control) * e_ptau181)

  nfl_ad <- rlnorm(n_samples, meanlog = x_nfl_control, sdlog = sd_nfl_control)
  nfl_trt <- rlnorm(n_samples, meanlog = x_nfl_trt, sdlog = sd_nfl_control)

  ptau_ad <- rlnorm(n_samples, meanlog = x_ptau181_control, sdlog = sd_ptau181_control)
  ptau_trt <- rlnorm(n_samples, meanlog = x_ptau181_trt, sdlog = sd_ptau181_control)

  # create data frame
  ID <- seq(1,n_samples,1)
  GRP_AD <- rep(0,n_samples)
  GRP_TRT <- rep(1, n_samples)
  ID <- c(ID,ID)
  GROUP <- c(GRP_AD, GRP_TRT)
  NFL <- round(c(nfl_ad, nfl_trt), digits = 3)
  PTAU181 <- round(c(ptau_ad, ptau_trt), digits = 3)

  ad_expstudy_dat <- data.frame(ID,GROUP, NFL, PTAU181)
  return(ad_expstudy_dat)
}
