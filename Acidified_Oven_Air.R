library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpmisc")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Acidified_Trial.csv", header = T)


Means <- Data %>%
  group_by(Sample) %>% 
  mutate(Carbon_mean = mean(X.C),
         Carbon_sd = sd(X.C),
         Isotope_mean = mean(δ13C),
         Isotope_sd = sd(δ13C)) %>%
  distinct(Carbon_mean, .keep_all = T) %>%
  mutate(Trial = case_when(grepl("Acidified", Sample) ~ "Acidified",
                           grepl("Oven", Sample) ~ "Oven Dried",
                           grepl("Unaltered", Sample) ~ "Unaltered")) %>%
  mutate(Field = case_when(grepl("E1", Sample) ~ "E1",
                           grepl("E3", Sample) ~ "E3",
                           grepl("S3_B", Sample) ~ "S3_B",
                           grepl("S3_C", Sample) ~ "S3_C",
                           grepl("SS1", Sample) ~ "SS1")) %>%
  ungroup()

E1 <- Means %>%
  subset(Sample %in% c("Acidified_E1_A", "Oven_dried_E1_A", "Unaltered_E1_A"))

Isotope_plot <- ggplot(E1, aes(x = Sample, y = Isotope_mean)) +
  geom_point() +
  geom_errorbar(aes(x = Sample, ymax = Isotope_mean + Isotope_sd, ymin = Isotope_mean - Isotope_sd))

Carbon_plot <- ggplot(E1, aes(x = Sample, y = Carbon_mean)) +
  geom_point() +
  geom_errorbar(aes(x = Sample, ymax = Carbon_mean + Carbon_sd, ymin = Carbon_mean - Carbon_sd))


Carbon_wide <- Means %>% select(c("Trial", "Carbon_mean", "Field")) %>%
  pivot_wider(names_from = "Trial", values_from = "Carbon_mean")
  

Carbon_correlation <- ggplot(Carbon_wide, aes(Acidified, Unaltered)) +
  geom_point() +
  stat_poly_line() +
  labs(title =" % Carbon") + 
  stat_poly_eq(use_label(c("eq", "R2")))

Isotope_wide <- Means %>% select(c("Trial", "Isotope_mean", "Field")) %>%
  pivot_wider(names_from = "Trial", values_from = "Isotope_mean")

Isotope_correlation <- ggplot(Isotope_wide, aes(Acidified, Unaltered)) +
  geom_point() +
  stat_poly_line() +
  labs(title =" Carbon-13") + 
  stat_poly_eq(use_label(c("eq", "R2")))

  