#### FIG 4 ####

library(ggplot2)
library(pander)
library(emmeans)
library(tidyverse)

##colourblind-friendly colours for ggplot
cbb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                 "#0072B2", "#D55E00", "#CC79A7")

##Avoid table wrapping
options(width = 800)
set.seed(999)

source(here::here("funs.r"))

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 0
female_es <- NA
male_es <- NA

all_effs_opposite <- rbind(
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.1, female_es = -0.1
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.2, female_es = -0.2
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.3, female_es = -0.3
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.4, female_es = -0.4
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.5, female_es = -0.5
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.6, female_es = -0.6
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.7, female_es = -0.7
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.8, female_es = -0.8
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.9, female_es = -0.9
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 1, female_es = -1
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.1, female_es = -0.1
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.2, female_es = -0.2
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.3, female_es = -0.3
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.4, female_es = -0.4
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.5, female_es = -0.5
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.6, female_es = -0.6
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.7, female_es = -0.7
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.8, female_es = -0.8
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.9, female_es = -0.9
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 1, female_es = -1
  )
)

calc_power_opposite_effs <- all_effs_opposite %>%
  filter(!Effect == "Residuals    ") %>%
  group_by(Effect, treat_es, male_es, sex_es, method) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n())) %>%
  mutate(inter_eff_size = rep("none")) %>%
  ungroup()

calc_power_opposite_effs$method <- unlist(calc_power_opposite_effs$method)

calc_power_opposite_effs %>%
  filter(Effect %in% c("Treatment", "T-Test Treatment Eff")) %>%
  mutate(method = recode(method, " Two Sample t-test" = "Pooled",
                         "ANOVA" = "Factorial")) %>%
  ggplot(aes(x = male_es, y = p_value, colour = method, group = method)) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_line() +
  theme_classic() +
  ylab("Statistical power") +
  xlab("Effect in each sex (+/-)") +
  scale_colour_manual(name = "Stat method", values = cbb_palette) +
  scale_linetype_discrete(name = "Stat method")

calc_power_opposite_effs %>%
  filter(!Effect %in% c("T-Test Treatment Eff")) %>%
  ggplot(aes(x = male_es, y = p_value, colour = Effect, group = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line() +
  theme_classic() +
  ylab("Statistical power") +
  xlab("Effect in each sex (+/-)") +
  scale_colour_manual(name = "Effect", values = cbb_palette)

all_effs_opposite_posthoc <- rbind(
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.1, female_es = -0.1
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.2, female_es = -0.2
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.3, female_es = -0.3
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.4, female_es = -0.4
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.5, female_es = -0.5
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.6, female_es = -0.6
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.7, female_es = -0.7
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.8, female_es = -0.8
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 0.9, female_es = -0.9
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = 0, sex_es = sex_es,
    male_es = 1, female_es = -1
  )
)

calc_opposite_effs_posthoc <- all_effs_opposite_posthoc %>%
  group_by(male_es, sex_es, Sex) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n()))


calc_opposite_effs_posthoc %>%
  mutate(Sex = dplyr::recode(Sex, FeMale = "Female")) %>%
  ggplot(aes(x = male_es, y = p.value, colour = Sex, group = Sex)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Sex)) +
  theme_classic() +
  ylab("Statistical power") +
  xlab("Effect in each sex (+/-)") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_manual(name = "Sex", values = cbb_palette) +
  scale_linetype_discrete(name = "Interaction effect size")

##
