library(brms)
library(tidybayes)


prior <- c(
    prior(normal(0, 5), class = "b"),        # Prior for coefficients
    prior(exponential(1), class = "sigma"),
    prior(student_t(10, 0, 1), class = "sd")
)

# Minimal dataset
dummy_data <- data.frame(
    y = rnorm(10),   # Random values; not used in the prior-only sampling
    x = rnorm(10),
    group = as.numeric(c(1:10))
)

# Create a model without data to view priors only
model <- brm(
    formula = y ~ x + (1 | group),                          # Dummy formula
    family = gaussian(),
    prior = prior,
    data = dummy_data,
    sample_prior = "only"                     # Only sample from the priors
)


# Extract prior samples using as_draws_df()
prior_samples <- as_draws_df(model)

# Density plot for prior on the intercept coefficient (e.g., b_Intercept)
ggplot(prior_samples, aes(x = b_Intercept)) +
    geom_density(fill = "skyblue", alpha = 0.5) +
    labs(title = "Density of Prior for Intercept Coefficient", x = "Intercept", y = "Density")

# Density plot for prior on sigma (residual SD)
ggplot(prior_samples, aes(x = sd_group__Intercept)) +
    geom_density(fill = "orange", alpha = 0.5) +
    labs(title = "Density of Prior for Residual SD", x = "Residual SD", y = "Density")
