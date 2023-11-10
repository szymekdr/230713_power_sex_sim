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


c(0.1, 0.4, 0.6) %>%
    map(\(x) p_values_for_power(
        n_rep = 1000, no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = x, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, tweak_effect = "Treatment"
    ), .progress = TRUE) %>%
    bind_rows()

