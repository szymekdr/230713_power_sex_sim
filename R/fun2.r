mock_df_generator2 <- function(no_per_gp, variable_mean, variable_sd,
                               treat_es2, sex_es2, ix_es2,
                               sex_sd, symm = FALSE) {
    X <- cbind(
        rep(1, no_per_gp * 4), # intercept
        c(rep(0, no_per_gp * 2), rep(1, no_per_gp * 2)), # treatment
        c(
            rep(0, no_per_gp), rep(1, no_per_gp),
            rep(0, no_per_gp), rep(1, no_per_gp)
        ), # sex
        c(
            rep(0, no_per_gp), rep(0, no_per_gp),
            rep(0, no_per_gp), rep(1, no_per_gp)
        ) # interaction
    )

    X_sex <- X[, 3]
    if (symm) {
        X_sex <- ifelse(X_sex == 0, -1, 1)
    }


    beta <- c(variable_mean, treat_es2, sex_es2, ix_es2)

    if (symm) {
        lowv <- variable_sd * sqrt(rep(1, 4 * no_per_gp) + -1 * (sex_sd - 1))
        highv <- variable_sd * sqrt(rep(1, 4 * no_per_gp) + 1 * (sex_sd - 1))
    } else {
        lowv <- variable_sd * sqrt(rep(1, 4 * no_per_gp) + 0 * (sex_sd - 1))
        highv <- variable_sd * sqrt(rep(1, 4 * no_per_gp) + 1 * (sex_sd - 1))
    }

    y <- X %*% beta +
        rnorm(
            no_per_gp * 4, 0,
            variable_sd * sqrt(rep(1, 4 * no_per_gp) + X_sex * (sex_sd - 1))
        )

    Treatment <- c(
        rep.int("Control", times = no_per_gp * 2),
        rep.int("Treated", times = no_per_gp * 2)
    )
    Sex <- c(
        rep.int("Female", times = no_per_gp),
        rep.int("Male", times = no_per_gp),
        rep.int("Female", times = no_per_gp),
        rep.int("Male", times = no_per_gp)
    )

    sim_df <- data.frame(
        col1 = y, col2 = Sex, col3 = Treatment,
        col4 = lowv, col5 = highv
    )
    names(sim_df) <- c("dep_variable", "Sex", "Treatment", "lowv", "highv")
    return(sim_df)
}


p_values2 <- function(no_per_gp, variable_mean, variable_sd,
                      treat_es2, sex_es2, ix_es2,
                      sex_sd, symm = FALSE, lm_method) {
    sim_data <- mock_df_generator2(
        no_per_gp = no_per_gp,
        variable_mean = variable_mean, variable_sd = variable_sd,
        treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm
    )

    my_formula <- dep_variable ~ Treatment * Sex

    if (lm_method == "aov") {
        model <- aov(my_formula, sim_data)
        output <- as.data.frame(anova(model))
        output$Effect <- rownames(output)
        rownames(output) <- NULL
        names(output)[5] <- "p_value"
    } else if (lm_method == "gls") {
        model <- gls(my_formula, sim_data)
        output <- as.data.frame(anova(model))
        output$Effect <- rownames(output)
        rownames(output) <- NULL
        names(output)[3] <- "p_value"
    } else if (lm_method == "sandwich") {
        model <- lm(my_formula, sim_data)
        sandwiched <- coeftest(model, vcov = vcovHC)
        output <- as.data.frame(tidy(sandwiched))
        names(output)[1] <- "Effect"
        names(output)[5] <- "p_value"

        output$Effect <- c("(Intercept)", attr(
            terms.formula(my_formula),
            "term.labels"
        ))
    } else {
        stop("lm_method not recognized")
    }

    output <- output %>%
        select(Effect, p_value) %>%
        filter(Effect != "Residuals", Effect != "(Intercept)")

    output$lowvar <- sim_data[1, "lowv"]
    output$highvar <- sim_data[1, "highv"]

    return(output)
}

p_value_sim <- function(n_rep, no_per_gp, variable_mean, variable_sd,
                        treat_es2, sex_es2, ix_es2,
                        sex_sd, symm, lm_method) {
    out_p <- map(1:n_rep, \(x) p_values2(
        no_per_gp = no_per_gp, variable_mean = variable_mean,
        variable_sd = variable_sd,
        treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
        sex_sd = sex_sd, symm = symm, lm_method = lm_method
    ))

    return(out_p)
}

p_values_for_power <- function(
    n_rep, no_per_gp,
    variable_mean,
    variable_sd, sex_sd,
    treat_es2, sex_es2, ix_es2,
    fix_power = FALSE, assume_power = 0.8,
    symm, lm_method = "aov",
    tweak_param = "no_per_gp", tweak_effect = NULL,
    target_power = 0.8, gsize_incr_rate = 0.25,
    fixed_power = FALSE) {
    if (fixed_power & is.null(tweak_effect)) {
        stop("Select one model term to simulate sample sizes")
    }
    library(broom)
    library(nlme)
    library(sandwich)
    library(lmtest)

    real_power <- 0

    while (real_power < target_power) {
        out_p <- p_value_sim(
            n_rep = n_rep, no_per_gp = no_per_gp,
            variable_mean = variable_mean, variable_sd = variable_sd,
            treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
            sex_sd = sex_sd, symm = symm, lm_method = lm_method
        ) %>% bind_rows()

        out_power <- out_p %>%
            group_by(Effect) %>%
            summarise(
                power = mean(p_value < 0.05),
                lowvar = unique(lowvar),
                highvar = unique(highvar)
            )

        out_power$no_per_gp <- no_per_gp
        out_power$variable_mean <- variable_mean
        out_power$variable_sd <- variable_sd
        out_power$sex_sd <- sex_sd
        out_power$treatment_es <- treat_es2
        out_power$sex_es <- sex_es2
        out_power$ix_es <- ix_es2
        out_power$method <- lm_method

        if (!is.null(tweak_effect)) {
            out_power <- out_power %>%
                filter(Effect == tweak_effect)
        }

        if (fixed_power) {
            real_power <- out_power$power
            no_per_gp <- ceiling(no_per_gp * (1 + gsize_incr_rate))
        } else {
            real_power <- 1
        }

        if (no_per_gp > 10000) {
            break
        }
    }

    return(out_power)
}