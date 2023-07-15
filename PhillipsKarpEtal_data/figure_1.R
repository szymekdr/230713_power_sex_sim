#### FIG 1 ####

library(ggplot2)
library(pander)
library(emmeans)
library(tidyverse)
library(here)

## colourblind-friendly colours for ggplot
cbb_palette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7"
)

## Avoid table wrapping
options(width = 800)
set.seed(999)

source(here::here("funs.r"))

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 0
female_es <- 0

all_effs_sex_none <- rbind(
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  )
)

calc_power_sex_none <- all_effs_sex_none %>%
  filter(!Effect == "Residuals    ") %>%
  group_by(Effect, treat_es, male_es, sex_es, method) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(sex_eff_size = rep("none"))

calc_power_sex_none %>%
  ggplot(aes(x = treat_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  ylab("Power")

############


no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 0.5
female_es <- 0

all_effs_sex_small <- rbind(
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  )
)

calc_power_sex_small <- all_effs_sex_small %>%
  filter(!Effect == "Residuals    ") %>%
  group_by(Effect, treat_es, male_es, sex_es, method) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(sex_eff_size = rep("small"))

calc_power_sex_small %>%
  ggplot(aes(x = treat_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  ylab("Power")


##########

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 1
female_es <- 0

all_effs_sex_large <- rbind(
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  )
)

calc_power_sex_large <- all_effs_sex_large %>%
  filter(!Effect == "Residuals    ") %>%
  group_by(Effect, treat_es, male_es, sex_es, method) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(sex_eff_size = rep("large"))

calc_power_sex_large %>%
  ggplot(aes(x = treat_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  ylab("Power")


##### combine to plot

all_sex_effs_sex_only <- rbind(calc_power_sex_none, calc_power_sex_small,
                               calc_power_sex_large)
all_sex_effs_sex_only <- as.data.frame(lapply(all_sex_effs_sex_only, unlist))

all_sex_effs_sex_only %>%
  filter(Effect %in% c("Treatment", "T-Test Treatment Eff")) %>%
  mutate(sex_eff_size = factor(sex_eff_size,
                               levels = c("none", "small", "large"))) %>%
  mutate(method = recode(method,
                         " Two Sample t-test" = "Pooled",
                         "ANOVA" = "Factorial")) %>%
  ggplot(aes(x = treat_es, y = p_value, colour = sex_eff_size,
             group = interaction(sex_eff_size, method))) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(linetype = method)) +
  theme_classic() +
  ylab("Statistical power") +
  xlab("Main treatment effect") +
  scale_colour_manual(name = "Sex effect size", values = cbb_palette) +
  scale_linetype_discrete(name = "Stat method")

all_sex_effs_sex_only %>%
  filter(!Effect %in% c("T-Test Treatment Eff")) %>%
  mutate(sex_eff_size = factor(sex_eff_size,
                               levels = c("none", "small", "large"))) %>%
  ggplot(aes(x = treat_es, y = p_value, colour = Effect,
             group = interaction(sex_eff_size, Effect))) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(linetype = sex_eff_size)) +
  theme_classic() +
  ylab("Statistical power") +
  xlab("Main treatment effect") +
  scale_colour_manual(name = "Effect", values = cbb_palette) +
  scale_linetype_discrete(name = "Sex effect size")


## figure 1 posthoc

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 0
female_es <- 0

all_effs_posthoc_sex_none <- rbind(
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  )
)



calc_power_none_posthoc_sex <- all_effs_posthoc_sex_none %>%
  group_by(treat_es, male_es, sex_es, Sex) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(sex_eff_size = rep("none"))

calc_power_none_posthoc_sex %>%
  filter(male_es >= 0) %>%
  ggplot(aes(x = treat_es, y = p.value, colour = Sex)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  theme_classic() +
  xlab("Interaction Effect Size") +
  ylab("Statistical Power") +
  scale_color_manual(values = cbb_palette)


############

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 0.5
female_es <- 0

all_effs_posthoc_sex_small <- rbind(
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es,
    male_es = 0, female_es = 0
  )
)

calc_power_posthoc_sex_small <- all_effs_posthoc_sex_small %>%
  group_by(treat_es, male_es, sex_es, Sex) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(sex_eff_size = rep("small"))

calc_power_posthoc_sex_small %>%
  filter(male_es >= 0) %>%
  ggplot(aes(x = treat_es, y = p.value, colour = Sex)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  theme_classic() +
  xlab("Sex Effect Size") +
  ylab("Statistical Power") +
  scale_color_manual(values = cbb_palette)


##########

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 1
female_es <- 0

all_effs_posthoc_sex_large <- rbind(
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.1, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.2, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.3, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.4, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.5, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.6, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.7, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.8, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0.9, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 1, sex_es = sex_es, male_es = 0,
    female_es = 0
  )
)

calc_power_posthoc_sex_large <- all_effs_posthoc_sex_large %>%
  group_by(treat_es, male_es, sex_es, Sex) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(sex_eff_size = rep("large"))

calc_power_posthoc_sex_large %>%
  filter(male_es >= 0) %>%
  ggplot(aes(x = treat_es, y = p.value, colour = Sex)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  theme_classic() +
  xlab("Sex Effect Size") +
  ylab("Statistical Power") +
  scale_color_manual(values = cbb_palette)

## combine to plot

all_sex_effs_posthoc_sex_only <- rbind(calc_power_posthoc_sex_small,
                                       calc_power_posthoc_sex_large)

all_sex_effs_posthoc_sex_only <-
  as.data.frame(lapply(all_sex_effs_posthoc_sex_only, unlist))

all_sex_effs_posthoc_sex_only %>%
  mutate(Sex = dplyr::recode(Sex, FeMale = "Female")) %>%
  mutate(sex_eff_size = factor(sex_eff_size, levels = c("small", "large"))) %>%
  ggplot(aes(x = treat_es, y = p.value, colour = Sex,
             group = interaction(sex_eff_size, Sex))) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(linetype = sex_eff_size)) +
  theme_classic() +
  ylab("Statistical power") +
  xlab("Main treatment effect") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(name = "Sex", values = cbb_palette) +
  scale_linetype_discrete(name = "Sex effect size")

##
