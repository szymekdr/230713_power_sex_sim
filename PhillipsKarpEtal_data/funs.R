library(tidyverse)

# FUNCTIONS FOR BUILDING SIM DATA AND CALCULATING P VALUES FOR STATISTICAL TESTS

######################
# Function to construct a simulated dataframe
# Mock_df construction of normally distributed data for two treatment groups for
# two sexes assuming equal variance.
# construct is based on one batch and that there are no cage effects
# Reference group for sex will be Female
# Reference group for treatment will be control
# treat_es  is the main effect of treatment
# sex_es is the main effect of sex (effect of being male)
# male_es is the treatment effect when being male and there is an interaction
# female_es is the treatment effect when being female and there is an
# interaction

### MODIFIED 14/07/2023 Szymek Drobniak
### Adding the possibiulity of variance heterogeneity


# mock_df_generator <- function(no_per_gp, variable_mean, variable_sd,
#                               treat_es, sex_es, male_es, female_es,
#                               ) {
#  dep_variable_control_male <- rnorm(n = no_per_gp,
#                               mean = (variable_mean + sex_es), sd = variable_sd)
#   # treatment effect can be as main effect or added for each sex individually
#   dep_variable_treated_male <-
#     rnorm(n = no_per_gp, mean = (variable_mean + sex_es + male_es + treat_es),
#           sd = variable_sd)
#   dep_variable_control_female <-
#     rnorm(n = no_per_gp, mean = variable_mean, sd = variable_sd)
#   dep_variable_treated_female <-
#     rnorm(n = no_per_gp, mean = (variable_mean + female_es + treat_es),
#           sd = variable_sd)

#   dep_variable <- c(dep_variable_control_male, dep_variable_treated_male,
#                     dep_variable_control_female, dep_variable_treated_female)
#   Sex <- c(rep.int("Male", times = no_per_gp * 2),
#            rep.int("FeMale", times = no_per_gp * 2))
#   Treatment <- c(rep.int("Control", times = no_per_gp),
#                  rep.int("Treated", times = no_per_gp),
#                  rep.int("Control", times = no_per_gp),
#                  rep.int("Treated", times = no_per_gp))
#   sim_df <- data.frame(col1 = dep_variable, col2 = Treatment, col3 = Sex)
#   names(sim_df) <- c("dep_variable", "Treatment", "Sex")
#   return(sim_df)
# }


mock_df_generator <- function(no_per_gp, variable_mean, variable_sd,
                              treat_es, sex_es, male_es, female_es,
                              treat_es2, sex_es2, ix_es2,
                              sex_sd = 0) {
  X <- cbind(
    rep(1, no_per_gp * 4), # intercept
    c(rep(0, no_per_gp * 2), rep(1, no_per_gp * 2)), # treatment
    c(rep(0, no_per_gp), rep(1, no_per_gp), rep(0, no_per_gp), rep(1, no_per_gp)), # sex
    c(rep(0, no_per_gp), rep(0, no_per_gp), rep(0, no_per_gp), rep(1, no_per_gp)) # interaction
  )

  beta <- c(variable_mean, treat_es2, sex_es2, ix_es2)
  y <- X %*% beta + rnorm(no_per_gp * 4, 0, variable_sd * sqrt(rep(1, 4 * no_per_gp) + X[, 3] * (sex_sd - 1)))

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

  sim_df <- data.frame(col1 = y, col2 = Sex, col3 = Treatment)
  names(sim_df) <- c("dep_variable", "Sex", "Treatment")
  return(sim_df)
}

# A <- mock_df_generator(
#   no_per_gp = no_per_gp, variable_mean = variable_mean,
#   variable_sd = variable_sd,
#   treat_es = treat_es, sex_es = sex_es, male_es = male_es, female_es = female_es,
#   treat_es2 = treat_es2, sex_es2 = sex_es2, ix_es2 = ix_es2,
#   sex_sd = sex_sd
# )
# A



######################

p_value_interaction_crossed <- function(n_rep, no_per_gp, variable_mean,
                                        variable_sd, treat_es, sex_es, male_es,
                                        female_es,
                                        treat_es2, sex_es2, ix_es2,
                                        sex_sd = 1,
                                        fix_power = F,
                                        assume_power = 0.8,
                                        tweak_param = "no_per_gp", tweak_effect = "Treatment") {
  


  real_power <- 0

  while (real_power < assume_power) {
    p_values <- function() {
      sim_data <- mock_df_generator(
        no_per_gp, variable_mean, variable_sd,
        treat_es, sex_es, male_es, female_es,
        treat_es2, sex_es2, ix_es2,
        sex_sd
      )

      model <- aov(dep_variable ~ Treatment * Sex, sim_data)

      return(summary(model))
    }

    sim_p_val <- as.data.frame(replicate(n_rep, p_values()))

    full_results <- sim_p_val %>%
      select(starts_with("Pr..")) %>%
      as_tibble(rownames = "Effect") %>%
      pivot_longer(-Effect, values_to = "p_value") %>%
      mutate(male_es = rep(male_es)) %>%
      mutate(treat_es = rep(treat_es)) %>%
      mutate(sex_es = rep(sex_es)) %>%
      mutate(method = rep("ANOVA")) %>%
      mutate(heterosc = sex_sd) %>%
      mutate(treat_es2 = treat_es2) %>%
      mutate(sex_es2 = sex_es2) %>%
      mutate(ix_es2 = ix_es2) %>%
      mutate(Effect = dplyr::recode(Effect,
        "Treatment    " = "Treatment",
        "Sex          " = "Sex"
      )) %>%
      select(-name)

    temp_results <- full_results %>%
      filter(!Effect == "Residuals    ") %>%
      filter(Effect == tweak_effect)

    if (fix_power) {
      real_power <- sum(temp_results$p_value < 0.05) / nrow(temp_results)
      no_per_gp <- ceiling(no_per_gp * 1.25)
    } else {
      real_power <- 1
    }
    
    cat("Power:", real_power, "Sample size:", no_per_gp, "\n")

    if(no_per_gp > 10000) {
      break
    }
  }
  
  full_results <- full_results %>%
    mutate(power = real_power) %>%
    mutate(tweak_param = tweak_param) %>%
    mutate(tweak_effect = tweak_effect) %>%
    mutate(ssize = no_per_gp)
  
  return(full_results)
}

######################

t_test_interaction_crossed <- function(n_rep, no_per_gp, variable_mean,
                                       variable_sd, treat_es, sex_es, male_es,
                                       female_es) {
  p_values <- function() {
    sim_data <- mock_df_generator(
      no_per_gp, variable_mean, variable_sd,
      treat_es, sex_es, male_es, female_es
    )

    model <- t.test(dep_variable ~ Treatment, sim_data, var.equal = TRUE)

    return(model)
  }

  sim_p_val <- as.data.frame(replicate(n_rep, p_values()))

  full_results <- t(sim_p_val) %>%
    as_tibble(rownames = "Effect") %>%
    select(starts_with(c("p.value", "method"))) %>%
    # pivot_longer(-Effect, values_to = "p_value")  %>%
    mutate(male_es = rep(male_es)) %>%
    mutate(treat_es = rep(treat_es)) %>%
    mutate(sex_es = rep(sex_es)) %>%
    mutate(Effect = rep("T-Test Treatment Eff")) %>%
    rename(p_value = p.value)
}

######
p_value_posthoc_values <- function(n_rep, no_per_gp, variable_mean, variable_sd,
                                   treat_es, sex_es, male_es, female_es) {
  posthoc_p_values <- function() {
    sim_data <- mock_df_generator(
      no_per_gp, variable_mean, variable_sd,
      treat_es, sex_es, male_es, female_es
    )

    model <- aov(dep_variable ~ Treatment * Sex, sim_data)

    posthoc <- emmeans(model, pairwise ~ Treatment | Sex, adjust = "none")
    contrast_df <- as.data.frame(posthoc$contrasts)


    return(contrast_df)
  }

  sim_p_val <- as.list(lapply(seq_len(n_rep), function(x) posthoc_p_values()))

  df_long <- do.call(rbind, sim_p_val)

  full_results <- df_long %>%
    mutate(male_es = rep(male_es)) %>%
    mutate(treat_es = rep(treat_es)) %>%
    mutate(sex_es = rep(sex_es)) %>%
    mutate(method = rep("Tukey Posthoc"))

  return(full_results)
}

# AOV across genes
# want to implement a pipeline that controls the FDR to an acceptable level
# can adopt a similar appraoch to the following with using LRT for competing
# models followed by hochberg correction:
# https://www.nature.com/articles/ncomms15475

aov_model_expression_as_string <- "aov(log2(value)~ treatment*sex, data = .)"
grouping_variable <- "gene"

anova_wrapper <- function(data, model_expression_as_string,
                          grouping_variable, ...) {
  f_wrap <- paste0("function(.) {", model_expression_as_string, "}") %>%
    parse(text = .) %>%
    eval()
  data %>%
    group_by_(grouping_variable) %>%
    do(f_wrap(.) %>%
      Anova(... = ...) %>%
      tidy()) %>%
    return()
}
