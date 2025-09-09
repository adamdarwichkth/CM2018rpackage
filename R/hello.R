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

### Lecture 1 - functions ###
a_clinical_dataset <- function(){
  rlnorm(10, 0,1)
}

another_clinical_dataset <- function(){
  rnorm(10, 12, 2)
}

yet_another_clinical_dataset <- function(){
  rnorm(250, 30, 6)
}

#############################################################################
### seminar 1 - functions ###
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

#############################################################################
### lecture 2 - functions ###

## Exercise 1
the_bp_study <- function(k_sample){
  set.seed(123456)
  n_sample <- k_sample
  mean_i <- 140
  sd_i <- 17
  mean_j <- 138.5
  sd_j <- 17

  data_y1 <- rnorm(n_sample, mean_i, sd_i)
  data_y2 <- rnorm(n_sample, mean_j, sd_j)

  n_ids <- n_sample*2

  id <- seq(1:n_ids)
  trt <- c(rep(0,n_sample),rep(1,n_sample))
  bp <- c(data_y1,data_y2)

  bp_data <- data.frame(ID = id, BP = bp, TRT = trt)
  return(bp_data)
}

## Exercise 2
#  dataset 2.1
harpenden_potato <- function(){
  set.seed(123456)
  n_sample <- 5
  # x = variety, y = chloride, z = sulphate
  data_x1y1z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x1y1z2 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x1y1z3 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x1y2z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x1y2z2 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x1y2z3 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x1y3z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x1y3z2 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x1y3z3 <- rnorm(n_sample, mean = 2.1, sd = 0.1)

  data_x2y1z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x2y1z2 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x2y1z3 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x2y2z1 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x2y2z2 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x2y2z3 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x2y3z1 <- rnorm(n_sample, mean = 2.15, sd = 0.1)
  data_x2y3z2 <- rnorm(n_sample, mean = 2.15, sd = 0.1)
  data_x2y3z3 <- rnorm(n_sample, mean = 2.15, sd = 0.1)

  data_x3y1z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x3y1z2 <- rnorm(n_sample, mean = 2.1, sd = 0.1)
  data_x3y1z3 <- rnorm(n_sample, mean = 2.2, sd = 0.1)
  data_x3y2z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x3y2z2 <- rnorm(n_sample, mean = 2.15, sd = 0.1)
  data_x3y2z3 <- rnorm(n_sample, mean = 2.25, sd = 0.1)
  data_x3y3z1 <- rnorm(n_sample, mean = 2, sd = 0.1)
  data_x3y3z2 <- rnorm(n_sample, mean = 2.35, sd = 0.1)
  data_x3y3z3 <- rnorm(n_sample, mean = 2.38, sd = 0.1)

  yield_lbs <- c(data_x1y1z1, data_x1y1z2, data_x1y1z3, data_x1y2z1, data_x1y2z2, data_x1y2z3, data_x1y3z1, data_x1y3z2, data_x1y3z3,
                 data_x2y1z1, data_x2y1z2, data_x2y1z3, data_x2y2z1, data_x2y2z2, data_x2y2z3, data_x2y3z1, data_x2y3z2, data_x2y3z3,
                 data_x3y1z1, data_x3y1z2, data_x3y1z3, data_x3y2z1, data_x3y2z2, data_x3y2z3, data_x3y3z1, data_x3y3z2, data_x3y3z3)

  sulphate <- rep(c(rep("low",n_sample),rep("med",n_sample),rep("high",n_sample)),9)

  chloride1 <- c(rep("low",n_sample*3), rep("med",n_sample*3), rep("high",n_sample*3))
  chloride <- c(chloride1, chloride1, chloride1)

  variety <- c(rep(c(rep("Queen",n_sample)),9), rep(c(rep("Duke",n_sample)),9), rep(c(rep("Comrade",n_sample)),9))

  potato_dat <- data.frame(YIELD = yield_lbs, VARIETY = variety, CHLORIDE = chloride, SULPHATE = sulphate)

  return(potato_dat)

}

# Ask Fisher:
ask_fisher_about_potatoes <- function(){
  print("Do an ANOVA. It is something I just came up with looking at potato data! When the mean square deviation is significantly larger than the standard deviation of the differences we may conclude that the corresponding effect is not due to chance. Cheers!")
}

# dataset 2.2
harpenden_potatwo <- function(){
  set.seed(456789)

  n_samples <- 100
  sulphate <- runif(n_samples, 0, 10)
  chloride <- runif(n_samples, 0, 15)
  baseline <- rnorm(n_samples, 2.1, 0.15)
  variety <- round(runif(n_samples,0,2),0)
  variety <- as.factor(variety)

  b_chlor <- 0.05
  b_sulph <- 0.15
  err <- rnorm(n_samples, 0, 0.35)

  yield <- baseline + chloride*b_chlor + sulphate*b_sulph + err

  potatwo_dat <- data.frame(YIELD = yield, SULPHATE = sulphate, CHLORIDE = chloride, VARIETY = variety)
  return(potatwo_dat)
}

## Exercise 3
more_potato_data <- function(){
  n_samples <- 100
  sulphate <- c(rep(0,25),rep(1,25),rep(2,25),rep(3,25))
  baseline <- rnorm(n_samples, 2.1, 0.15)
  err <- rnorm(n_samples, 0, 0.35)
  b_sulph <- 0.175

  Yield <- baseline + sulphate*b_sulph + err

  Yield_0 <- Yield[1:25]
  Yield_1 <- Yield[26:50]
  Yield_2 <- Yield[51:75]
  Yield_3 <- Yield[76:100]

  pota3_dat <- data.frame(VARIETY = seq(1:25), YIELD_0KG = Yield_0, YIELD_1KG = Yield_1,
                          YIELD_2KG = Yield_2, YIELD_3KG = Yield_3)
  return(pota3_dat)
}

## Exercise 4
# B: Additivity and Linearity
data_4b <- function(){
  set.seed(123456)
  n_samples <- 100
  x <- runif(n_samples, 0.2, 10)
  baseline <- rnorm(n_samples, 11.1, 1.5)
  err <- rnorm(n_samples, 0, 0.9)
  b_x <- 0.34

  DV <- baseline + exp(x*b_x) + err

  data_4b <- data.frame(DV = DV, X = x)
  return(data_4b)
}

data_4b2 <- function(){
  set.seed(123456)
  n_samples <- 100
  x1 <- runif(n_samples, 0.2, 10)
  x2 <- runif(n_samples, 0.2, 10)
  baseline <- rnorm(n_samples, 11.1, 1.75)
  err <- rnorm(n_samples, 0, 0.9)
  b_x1 <- 0.34
  b_x2 <- 0.25
  b_x1x2 <- 0.3

  DV <- baseline + x1*b_x1 + x2*b_x2 + x1*x2*b_x1x2 + err

  data_4b2 <- data.frame(DV = DV, X1 = x1, X2 = x2)
  return(data_4b2)
}

# C: homoscedastisticy
data_4c <- function(){
  set.seed(123456)
  n_samples <- 100
  x1 <- runif(n_samples, 0.2, 10)
  x2 <- runif(n_samples, 0.2, 10)
  baseline <- rnorm(n_samples, 11.1, 1.75)
  err <- rnorm(n_samples, 0, 0.9)
  b_x1 <- 0.34
  b_x2 <- 0.25
  b_x1x2 <- 0.3

  DV <- baseline + x1*b_x1 + x2*b_x2 + x1*x2*b_x1x2 + err

  data_4c <- data.frame(DV = DV, X = x1)
  return(data_4c)
}

# D: Normality
data_4d <- function(){
  set.seed(123456)
  n_samples <- 100
  x1 <- runif(n_samples, 0.2, 10)
  baseline <- rnorm(n_samples, 11.1, 1.75)
  err <- rnorm(n_samples, 0, 0.9)
  b_x1 <- 0.34
  b_x2 <- 0.25
  b_x1x2 <- 0.3

  DV <- baseline + x1*b_x1 + err
  DV <- round(DV/3,digits = 0)
  data_4d <- data.frame(DV = DV, X = x1)
  return(data_4d)
}


#############################################################################




