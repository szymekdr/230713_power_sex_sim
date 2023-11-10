# parameters of importance
alpha <- 0
symm <- F

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha

A <- mock_df_generator2(no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm)


p_values2(no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm)

p_value_sim(n_rep = 5,
    no_per_gp = no_per_gp,
    variable_mean = variable_mean, variable_sd = variable_sd,
    treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
    sex_sd = sex_sd, symm = symm
)

p_values_for_power(n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm)



# quick test without interaction
A <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

B <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

C <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(A, B, C) %>%
    mutate(hetero = factor(highvar - lowvar,
    labels = c("none", "low", "large"))) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, col = hetero)) +
        geom_line() +
        theme_bw()


# quick test without interaction
ix_es2 <- 0.5
D <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

E <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

G <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(D, E, G) %>%
    mutate(hetero = factor(highvar - lowvar,
    labels = c("none", "low", "large"))) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, col = hetero)) +
        geom_line() +
        theme_bw()
    

# quick test without interaction 2
ix_es2 <- -0.5
H <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

J <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

K <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(H, J, K) %>%
    mutate(hetero = factor(highvar - lowvar,
    labels = c("none", "low", "large"))) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, col = hetero)) +
        geom_line() +
        theme_classic()