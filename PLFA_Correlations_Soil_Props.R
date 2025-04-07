library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpubr")
library("stringr")



setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20240520_LW Modified.csv", header = T)

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Filtered <- Data %>% select(Crop.2023,
                        Date_Rept,
                        Tillage,
                        Cover.Crop,
                        Season,
                        Field,
                        NO3_N_H2O,
                        ON_H2O_ppm,
                        Aggstab1_2mm,
                        Fungi_Bacteria,
                        Grampos_Gramneg,
                        Protozoan.,
                        Sat_Unsat,
                        Mono_Poly,
                        Predator_Prey,
                        Protozoa_biomasss) %>%
  #filter(str_detect(Date_Rept, "2023")) %>%
  rename(`Aggregate Stability (1-2mm)` = "Aggstab1_2mm",
         `Protozoan (%)` = "Protozoan.",
         `Protozoa (ug/g)` = "Protozoa_biomasss",
         `Water Extractable Nitrate (mg/kg)` = NO3_N_H2O,
         `Water Extractable Organic Nitrogen (mg/kg)` = ON_H2O_ppm)

Filtered[,7:length(Filtered)] <- as.numeric(unlist(Filtered[,7:length(Filtered)]))

Filtered_L <-  pivot_longer(Filtered, cols = c(10:length(Filtered)), names_to = "Variable", values_to = "Value")

Ag_PLFA <- ggplot(Filtered_L, aes(x = `Aggregate Stability (1-2mm)`, y = `Value`, color = Season)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = Season)) +
  theme_bw() +
  facet_wrap( ~Variable, scales = "free") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20),
        aspect.ratio = 1)


Nitrate_PLFA <- ggplot(Filtered_L, aes(y = `Water Extractable Nitrate (mg/kg)`, x = `Value`, color = Season, shape = Cover.Crop)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = Season, linetype = Cover.Crop)) +
  theme_bw() +
  facet_wrap( ~Variable, scales = "free") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20),
        aspect.ratio = 1)


ON_PLFA <- ggplot(Filtered_L, aes(y = `Water Extractable Organic Nitrogen (mg/kg)`, x = `Value`, color = Season, shape = Cover.Crop)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, aes(color = Season, linetype = Cover.Crop)) +
  theme_bw() +
  facet_wrap( ~Variable, scales = "free") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20),
        aspect.ratio = 1)

Test <- lm(formula = `Water Extractable Organic Nitrogen (mg/kg)` ~ Season + Cover.Crop + `Protozoa (ug/g)`*(Cover.Crop * Season), data = Filtered)
Test <- lm(formula = `Water Extractable Nitrate (mg/kg)` ~ Season + Cover.Crop + `Protozoa (ug/g)`*(Cover.Crop * Season), data = Filtered)



Test <- lm(formula = `Water Extractable Organic Nitrogen (mg/kg)` ~ Season + Cover.Crop + Tillage, data = Filtered)
Test <- lm(formula = `Water Extractable Nitrate (mg/kg)` ~ Season + Cover.Crop + Tillage, data = Filtered)


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("PLFA_Nitrate.pdf", Nitrate_PLFA, height = 8, width = 16)
ggsave("PLFA_Organic_Nitrogen.pdf", ON_PLFA, height = 8, width = 16)