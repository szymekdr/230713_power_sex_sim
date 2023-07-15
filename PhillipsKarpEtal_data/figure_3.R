#### FIG 3 ####

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
female_es <- 0



all_effs_one_sex <- rbind(
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -2, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.6, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.2, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.8, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.4, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.4, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.8, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.2, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.6, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 2, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -2, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.6, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.2, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.8, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.4, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.4, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.8, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.2, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.6, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 2, female_es = 0
  )
)

calc_power_one_sex <- all_effs_one_sex %>%
  filter(!Effect == "Residuals    ") %>%
  group_by(Effect, male_es, method) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n()))


calc_power_one_sex %>%
  filter(male_es >= 0) %>%
  mutate(Effect = recode(Effect, "Treatment" = "Factorial",
                         "T-Test Treatment Eff" = "Pooled")) %>%
  filter(Effect %in% c("Factorial", "Pooled")) %>%
  ggplot(aes(x = male_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  scale_color_manual(name = "Stat method", values = cbb_palette) +
  xlab("Interaction Effect Size") +
  ylab("Statistical Power")


calc_power_one_sex %>%
  filter(male_es >= 0) %>%
  filter(!Effect == "T-Test Treatment Eff") %>%
  ggplot(aes(x = male_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  scale_color_manual(name = "Stat method", values = cbb_palette) +
  xlab("Interaction Effect Size") +
  ylab("Statistical Power")

all_effs_posthoc_one_sex <- rbind(
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -2, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.6, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.2, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.8, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.4, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.4, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.8, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.2, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.6, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 2, female_es = 0
  )
)

calc_power_posthoc_one_sex <- all_effs_posthoc_one_sex %>%
  group_by(male_es, method, Sex) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n()))

calc_power_posthoc_one_sex %>%
  mutate(Sex = dplyr::recode(Sex, FeMale = "Female")) %>%
  filter(male_es >= 0) %>%
  ggplot(aes(x = male_es, y = p.value, colour = Sex)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  theme_classic() +
  xlab("Interaction Effect Size") +
  ylab("Statistical Power") +
  scale_color_manual(values = cbb_palette)

## 10 males

no_per_gp <- 5
variable_mean <- 1
variable_sd <- 0.5
treat_es <- 0
sex_es <- 0
female_es <- 0



all_effs_one_sex <- rbind(
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -2, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.6, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.2, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.8, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.4, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.4, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.8, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.2, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.6, female_es = 0
  ),
  p_value_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 2, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -2, female_es = -2
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.6, female_es = -1.6
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.2, female_es = -1.2
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.8, female_es = -0.8
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.4, female_es = -0.4
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.4, female_es = 0.4
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.8, female_es = 0.8
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.2, female_es = 1.2
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.6, female_es = 1.6
  ),
  t_test_interaction_crossed(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 2, female_es = 2
  )
)

calc_power_one_sex <- all_effs_one_sex %>%
  filter(!Effect == "Residuals    ") %>%
  group_by(Effect, male_es, method) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n()))


calc_power_one_sex %>%
  filter(male_es >= 0) %>%
  mutate(Effect = recode(Effect, "Treatment" = "5F 5M",
                         "T-Test Treatment Eff" = "10M")) %>%
  filter(Effect %in% c("5F 5M", "10M")) %>%
  ggplot(aes(x = male_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  scale_color_manual(name = "Design strategy", values = cbb_palette) +
  xlab("Effect Size") +
  ylab("Statistical Power")


calc_power_one_sex %>%
  filter(male_es >= 0) %>%
  filter(!Effect == "T-Test Treatment Eff") %>%
  ggplot(aes(x = male_es, y = p_value, colour = Effect)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_line(aes(group = Effect)) +
  theme_classic() +
  scale_color_manual(name = "Stat method", values = cbb_palette) +
  xlab("Interaction Effect Size") +
  ylab("Statistical Power")


##### Posthoc power test

all_effs_posthoc_one_sex <- rbind(
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -2, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.6, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -1.2, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.8, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = -0.4, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.4, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 0.8, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.2, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 1.6, female_es = 0
  ),
  p_value_posthoc_values(
    n_rep = 1000, no_per_gp = no_per_gp, variable_mean = 1,
    variable_sd = variable_sd, treat_es = treat_es, sex_es = sex_es,
    male_es = 2, female_es = 0
  )
)


calc_power_posthoc_one_sex <- all_effs_posthoc_one_sex %>%
  group_by(male_es, method, Sex) %>%
  summarise_all(funs(sum(. < 0.05, na.rm = TRUE) / n()))

calc_power_posthoc_one_sex %>%
  mutate(Sex = dplyr::recode(Sex, FeMale = "Female")) %>%
  filter(male_es >= 0) %>%
  ggplot(aes(x = male_es, y = p.value, colour = Sex)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  theme_classic() +
  xlab("Interaction Effect Size") +
  ylab("Statistical Power") +
  scale_color_manual(values = cbb_palette)

###
