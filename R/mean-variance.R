library(readxl)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(patchwork)

#load data
data_all <- read_excel("./R/ALL_TRAITS_20200514.xlsx", sheet = "main")
names(data_all)


## analyses on adiposity [g] data only:

data_all %>% 
  filter(Trait == "Adiposity") %>% count(Paper_ID)

#offspring ages
summary(data_all$Age_at_Measurement_Days[data_all$Trait == "Adiposity"])

#species
summary(factor(data_all$Rodent_Type[data_all$Trait == "Adiposity"]))

#merge control and treatment data (all traits)
dataA <- tibble(mean = c(data_all$Mean_Treatment, data_all$Mean_Control), 
                sd = c(data_all$SD_Treatment, data_all$SD_Control),
                size = c(data_all$Sample_Size_n_Treatment, data_all$Sample_Size_n_Control),
                sex = c(data_all$Offspring_Sex, data_all$Offspring_Sex),
                trait = c(data_all$Trait, data_all$Trait),
                species = c(data_all$Rodent_Type, data_all$Rodent_Type))


## A. plot just Adiposity data - Mouse

dataA %>% 
  filter(trait == "Adiposity" & species == "Mouse") %$%
  cor.test(log(mean), log(sd)) -> cor_mice #calculate correlation

figA <- dataA %>% 
  filter(trait == "Adiposity") %>%
  filter(species == "Mouse") %>%
  ggplot(aes(x = log(mean), y = log(sd), col = sex)) + 
  geom_point(aes(size = size), alpha = 0.5) +
  geom_smooth(method = "lm",  se = FALSE) +
  #coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "A.  Mouse adiposity", x = "ln (mean)", y = "ln (SD)") + 
  geom_text(x = -5.2, y = 1.5, aes(label = sprintf("italic(r) == %.2f", cor_mice$estimate)), parse = TRUE, hjust = 0, col = "black", size = 5)


## B. plot just Adiposity data - Rat

dataA %>% 
  filter(trait == "Adiposity" & species == "Rat") %$%
  cor.test(log(mean), log(sd)) -> cor_rats #calculate correlation

figB <- dataA %>% 
  filter(trait == "Adiposity") %>%
  filter(species == "Rat") %>%
  ggplot(aes(x = log(mean), y = log(sd), col = sex)) + 
  geom_point(aes(size = size), alpha = 0.5) +
  geom_smooth(method = "lm",  se = FALSE) +
  #coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "B.  Rat adiposity", x = "ln (mean)", y = "ln (SD)") + 
  geom_text(x = -1, y = 4, aes(label = sprintf("italic(r) == %.2f", cor_rats$estimate)), parse = TRUE, hjust = 0, col = "black", size = 5)


## B. plot  Adiposity data - Mouse and Rat

#correlation - all
dataA %>% 
  filter(trait == "Adiposity") %$% 
  cor.test(log(mean), log(sd)) -> cor_all

figC <- dataA %>% 
  filter(trait == "Adiposity") %>%
  ggplot(aes(x = log(mean), y = log(sd), col = sex)) + 
  geom_point(aes(size = size), alpha = 0.5) +
  geom_smooth(method = "lm",  se = FALSE) +
  #coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "C.  Mouse and rat adiposity", x = "ln (mean)", y = "ln (SD)") + 
  geom_text(x = -5, y = 4, aes(label = sprintf("italic(r) == %.2f", cor_all$estimate)), parse = TRUE, hjust = 0, col = "black", size = 5)


#assemble the panels using patchwork package
figure1 <- figA / figB / figC + 
  plot_layout(ncol = 1, nrow = 3, heights = c(1, 1, 1.1)) #+
  #plot_annotation(tag_levels = "A")
#ggsave(plot = figure1, "Figure1_v0.png", width = 8, height = 14, units = "cm", dpi = "retina", scale = 2)
#ggsave(plot = figure1, "Figure1_v0.pdf", width = 8, height = 14, units = "cm", scale = 2)






## OLD


## plot just Adiposity data, all groups with facets
dataA %>% 
  filter(trait == "Adiposity") %>%
  ggplot(aes(x = log(mean), y = log(sd), col = sex)) + 
  geom_point(aes(size = size), alpha = 0.5) +
  geom_smooth(method = "lm",  se = FALSE) +
  #coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "Adiposity, all animals", x = "ln (mean)", y = "ln (SD)") +
  facet_grid("species")


## plot only Adiposity - Control animals
data_all %>% 
  filter(Trait == "Adiposity") %>%
  ggplot(aes(x = Mean_Control, y = SD_Control, size = Sample_Size_n_Control, col = Offspring_Sex)) + 
  geom_point(alpha = 0.5) +
  coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "right") +
  labs(title = "control animals", x = "log10 (mean)", y = "log10 (SD)") 

## plot only Adiposity - treatment animals
data_all %>% 
  filter(Trait == "Adiposity") %>%
  ggplot(aes(x = Mean_Treatment, y = SD_Treatment, size = Sample_Size_n_Treatment, col = Offspring_Sex)) + 
  geom_point(alpha = 0.5) +
  coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(title = "treatment animals", x = "log10 (mean)", y = "log10 (SD)") 


## Bodyweight - control
figureA <- data_all %>% 
  #filter(Trait == "Adiposity") %>%
  ggplot(aes(x = Mean_Control, y = SD_Control, size = Sample_Size_n_Control, col = Study_ID)) + 
  geom_point(alpha = 0.5) +
  coord_trans(x = "log10", y = "log10") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "control animals", x = "log10 (mean)", y = "log10 (SE)") +
  facet_grid(cols = vars(Trait))


#assemble the panels using patchwork package
figureABCD <- figureA / figureB / figureC / figureD +
  plot_layout(ncol = 2, nrow = 2) +
  plot_annotation(tag_levels = "A")
#ggsave(plot = figureA, here("plots", "FigureA_v0.png"), width = 18, height = 8, units = "cm", dpi = "retina", scale = 1.2)
#ggsave(plot = figureA, here("plots", "FigureA_v0.pdf"), width = 18, height = 8, units = "cm", scale = 1.2)

