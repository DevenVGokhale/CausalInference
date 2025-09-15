library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)
library(patchwork)
library(psych)
library(gt)


fake_data <- (
  tibble(X = rnorm(1000, 0, 1)) |> 
    mutate(
      y = rnorm(n(), (-1 + 2.3*X), 2.0)
    )
)

fake_data |> 
  ggplot(aes(x = X, y = y)) +
  geom_point(shape=21) +
  geom_smooth(method = "lm", alpha=0.5) +
  theme_minimal()

describe(fake_data) 

hello_brms <- (
  brm(
    y ~ X, 
    data = fake_data
)
)


summary(hello_brms)


pp_vals <- posterior_predict(hello_brms, draws = 10)
preds <- c("Intercept", "b_X", "sigma")

diag_plt1 <- mcmc_rank_overlay(hello_brms, preds) + theme(legend.position="None") + theme_bw()
diag_plt2 <- mcmc_hist(hello_brms, preds) + theme_bw()
diag_plt2 + diag_plt1 + 
  plot_layout(
    design = "
    A
    B
    ", 
    guides="collect") & 
  theme(legend.position = "bottom")


ppc1 <- (
  ppc_dens_overlay(y = fake_data |> pull(y), yrep = pp_vals) 
    + theme_bw()
    + theme(legend.position="None")
  )
ppc2 <- ppc_ecdf_overlay(y = fake_data |> pull(y), 
                         yrep = pp_vals) + theme_bw()
ppc1 + ppc2 +
  plot_layout(
    design = "
    A
    B
    ", 
    guides="collect") & 
  theme(legend.position = "bottom")

