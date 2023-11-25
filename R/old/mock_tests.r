# parameters of importance


source(here("PhillipsKarpEtal_data/funs.r"))
source(here("R/fun2.r"))


alpha <- 0
symm <- F

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha

toydata <- mock_df_generator2(
    no_per_gp = no_per_gp,
    variable_mean = variable_mean, variable_sd = variable_sd,
    treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
    sex_sd = sex_sd, symm = symm
)


p_values2(
    no_per_gp = no_per_gp,
    variable_mean = variable_mean, variable_sd = variable_sd,
    treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
    sex_sd = sex_sd, symm = symm, lm_method = "aov"
)

p_value_sim(
    n_rep = 5,
    no_per_gp = no_per_gp,
    variable_mean = variable_mean, variable_sd = variable_sd,
    treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
    sex_sd = sex_sd, symm = symm, lm_method = "aov"
)

p_values_for_power(
    n_rep = 1000, no_per_gp = no_per_gp,
    variable_mean = variable_mean, variable_sd = variable_sd,
    treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
    sex_sd = sex_sd, symm = symm, lm_method = "aov"
)



# quick test without interaction
ix_es2 <- 0
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
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    geom_line() +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
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
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
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
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    geom_line() +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()




# GLS method
# quick test without interaction
ix_es2 <- 0
AA <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

BB <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

CC <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(A, BB, CC) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()


# quick test without interaction
ix_es2 <- 0.5
DD <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

EE <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

GG <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(D, EE, GG) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()


# quick test without interaction 2
ix_es2 <- -0.5
HH <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

JJ <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

KK <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "gls"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(H, JJ, KK) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    geom_line() +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()



# sandwich method
# quick test without interaction
ix_es2 <- 0
AAA <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

BBB <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

CCC <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(A, BBB, CCC) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()


# quick test without interaction
ix_es2 <- 0.5
DDD <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

EEE <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

GGG <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(D, EEE, GGG) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()


# quick test without interaction 2
ix_es2 <- -0.5
HHH <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

JJJ <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 1.5, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

KKK <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = 2, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "sandwich"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(H, JJJ, KKK) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    geom_line() +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()




# SYMMETRICAL VARIANCES
alpha <- 2.4
symm <- F
no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.35
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha
ix_es2 <- 0

Bs <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "aov"
    ), .progress = TRUE) %>%
    bind_rows()

alpha <- 15
symm <- F
no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.2
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha
ix_es2 <- 0

Cs <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "aov"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(A, Bs, Cs) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()


# quick test with interaction
alpha <- 2.4
symm <- F
no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.35
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha
ix_es2 <- 0.5

Es <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "aov"
    ), .progress = TRUE) %>%
    bind_rows()

alpha <- 15
symm <- F
no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.2
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha
ix_es2 <- 0.5

Gs <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "aov"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(D, Es, Gs) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()


# quick test with interaction 2
alpha <- 2.4
symm <- F
no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.35
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha
ix_es2 <- -0.5

Js <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "aov"
    ), .progress = TRUE) %>%
    bind_rows()

alpha <- 15
symm <- F
no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.2
treat_es2 <- 0
sex_es2 <- 1
ix_es2 <- 0
sex_sd <- 1 + alpha
ix_es2 <- -0.5

Ks <- seq(0.3, 0.7, 0.1) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment",
        fixed_power = TRUE, gsize_incr_rate = 0.1,
        lm_method = "aov"
    ), .progress = TRUE) %>%
    bind_rows()

bind_rows(H, Js, Ks) %>%
    mutate(hetero = factor(highvar - lowvar,
        labels = c("none", "low", "large")
    )) %>%
    ggplot(aes(x = treatment_es, y = no_per_gp, linetype = hetero)) +
    geom_line() +
    scale_linetype_manual(
        name = "Heterogeneity",
        values = c("dotted", "dashed", "solid"),
    ) +
    theme_bw()
