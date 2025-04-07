library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpattern")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Kansas <- read.csv("Soil1_20240520_LW Modified.csv", header = T)
SHI_soil <- read.csv("SHI_Soil_Data.csv")
SHI_sites <- read.csv("SHI_Sites.csv")
SHI_manage <- read.csv("SHI_landmanagement.csv")
SHI_PLFA <- read.csv("SHI_PLFA.csv")
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )
Season_col <- c(Spring = "#56B4E9", Fall = "#E69900")


#Bringing all the Soil Health Institute Data together
SHI <- left_join(SHI_soil, SHI_PLFA)

SHI_data <- left_join(SHI, SHI_sites)
#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Kansas %>% dplyr::select(Season,
                                Tillage,
                                Cover.Crop,
                                Field,
                                pH,
                                OM.,
                                CO2_24hr,
                                OC_H2O_mg.kg,
                                ON_H2O_mg.kg,
                                H3A_NH4,
                                H3A_NO3,
                                H3A_IP,
                                H3A_OP,
                                H3A_ICAPK,
                                H3A_ICAPCa,
                                H3A_ICAPAl,
                                H3A_ICAPFe,
                                H3A_ICAPS,
                                H3A_ICAPMn,
                                H3A_ICAPZn,
                                H3A_ICAPNa,
                                Biomass) %>%
  rename("Organic Matter (%)" ="OM.",
         "Cover Crop" = "Cover.Crop",
         "H2O Extractable Organic Carbon (mg/kg)" = "OC_H2O_mg.kg",
         "H2O Extractable Organic Nitrogen (mg/kg)" = "ON_H2O_mg.kg",
         "Haney Extract NH4 (mg/kg)" = "H3A_NH4",
         "Haney Extract NO3 (mg/kg)" = "H3A_NO3",
         "Haney Extract IP (mg/kg)" = "H3A_IP",
         "Haney Extract OP (mg/kg)" = "H3A_OP",
         "Total Biomass (ng/g)" = "Biomass") %>%
  mutate(State = "Kansas",
         Study = "ROAR",
         Animals = recode(Field, "W1" = "A", 
                          "SS1" = "NA", 
                          "E1" = "NA",
                          "E2" = "NA", 
                          "E3" = "NA",
                          "S2" = "NA",
                          "S3" = "NA", 
                          "S4" = "NA",
                          "SS1" = "NA")) %>%
  relocate(State, .before = Season) %>%
  relocate(Animals, .before = pH) %>%
  mutate(Tillage = recode(Tillage, "Conventional Till" = 'CT', "No-Till" = "NT"),
         `Cover Crop` = recode(`Cover Crop`, "Cover Crop" = "CC", "No Cover Crop" = "NCC")) %>%
  dplyr::select(-c(Field)) %>%
  mutate(`24 hour CO2 Respiration` = CO2_24hr/2.83) %>%
  dplyr::select(-c(CO2_24hr))

SHI_select <- SHI_data %>% dplyr::select(State,
                                  site_disturbance,
                                  site_cover_crop,
                                  site_animals,
                                  pH,
                                  X.OM_LOI2,
                                  CO2_24hr,
                                  OC_H2O_mg.kg,
                                  WEON_mg.kg,
                                  H3A_NH4_mg.kg,
                                  H3A_NO3_mg.kg,
                                  H3A_NH4_mg.kg,
                                  H3A_IP_mg.kg,
                                  H3A_OP_mg.kg,
                                  H3A_ICAPK,
                                  H3A_ICAPCa,
                                  H3A_ICAPAl,
                                  H3A_ICAPFe,
                                  H3A_ICAPS,
                                  H3A_ICAPMn,
                                  H3A_ICAPZn,
                                  H3A_ICAPNa,
                                  Biomass) %>%
  rename("Organic Matter (%)" ="X.OM_LOI2",
         "Tillage" = "site_disturbance",
         "Cover Crop" = "site_cover_crop",
         "Animals" = "site_animals",
         "24 hour CO2 Respiration" = "CO2_24hr",
         "H2O Extractable Organic Carbon (mg/kg)" = "OC_H2O_mg.kg",
         "H2O Extractable Organic Nitrogen (mg/kg)" = "WEON_mg.kg",
         "Haney Extract NH4 (mg/kg)" = "H3A_NH4_mg.kg",
         "Haney Extract NO3 (mg/kg)" = "H3A_NO3_mg.kg",
         "Haney Extract IP (mg/kg)" = "H3A_IP_mg.kg",
         "Haney Extract OP (mg/kg)" = "H3A_OP_mg.kg",
         "Total Biomass (ng/g)" = "Biomass") %>%
  mutate(Season = NA,
         Study = "SHI") %>%
  relocate(Season, .after = State) %>%
  mutate(Tillage = recode(Tillage, "1" = 'CT', "0" = "NT"),
         `Cover Crop` = recode(`Cover Crop`, "1" = "CC", "0" = "NCC"),
         `Animals` = recode(`Animals`, "1" = "A", "0" = "NA"),) %>%
  filter(State == c("Kansas", "Nebraska", "Iowa", "Missouri"))


ROAR_SHI <- rbind(Haney_Test, SHI_select) %>%
  mutate(`Land Management` = paste(Tillage,",",`Cover Crop`)) %>%
  relocate(`Land Management`, .before = Tillage) %>%
  relocate(Study, .before = State)
ROAR_SHI[,8:length(ROAR_SHI)] <- as.numeric(unlist(ROAR_SHI[,8:length(ROAR_SHI)]))
ROAR_SHI_long <- pivot_longer(ROAR_SHI, c(8:length(ROAR_SHI)), names_to = "Variable", values_to = "Value") %>%
  
  mutate(`Land Management` = factor(`Land Management`, levels = c("CT , NCC",
                                                                  "CT , CC",
                                                                  "NT , NCC",
                                                                  "NT , CC")))


Compare_plot <- ggplot(ROAR_SHI_long, aes(x = `Land Management`, y = Value, fill = Study)) +
  geom_boxplot() + 
  facet_wrap(vars(Variable), scales = "free") + 
  geom_jitter(aes(color = Study), size = 0.4, alpha = 0.9) +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15))

#setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
#ggsave("ROAR_vs_SHI.pdf", Compare_plot, height = 15, width = 20 , limitsize = F)

ROAR_SHI_long2 <- pivot_longer(ROAR_SHI, c(8:length(ROAR_SHI)), names_to = "Variable", values_to = "Value") %>%
  
  mutate(`Land Management` = factor(`Land Management`, levels = c("CT , NCC",
                                                                  "CT , CC",
                                                                  "NT , NCC",
                                                                  "NT , CC"))) %>%
  mutate(`Study, Season` = paste(Study, ",",Season),
         `Study, Season` = recode(`Study, Season`,"SHI , NA" = "SHI"))

Compare_plot <- ggplot(ROAR_SHI_long2, aes(x = `Land Management`, y = Value, fill = `Study, Season`)) +
  geom_boxplot() + 
  facet_wrap(vars(Variable), scales = "free") + 
  geom_jitter(aes(color = `Study, Season`), size = 0.4, alpha = 0.9) +
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15))

#setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
#ggsave("ROAR_vs_SHI_seasons.pdf", Compare_plot, height = 15, width = 20 , limitsize = F)



ROAR <- ROAR_SHI_long2 %>% subset(Study %in% "ROAR") %>%
  group_by(`Land Management`, Variable, `Study, Season`) %>%
  mutate(Mean = case_when(Variable == "pH" ~ -log10(mean(10**-Value)),
                          Variable != "pH" ~ mean(Value)),
         Sd = sd(Value)) %>%
  mutate(Min = case_when(Mean - Sd < 0 ~ 0,
                         Mean - Sd >= 0 ~ Mean - Sd),
         Max = Mean + Sd ) %>%
  distinct(`Land Management`, Variable, Mean, Sd, `Study, Season`, .keep_all = T) %>%
  mutate(`Land Management` = factor(`Land Management`, levels = c("CT , NCC",
                                                                  "CT , CC",
                                                                  "NT , NCC",
                                                                  "NT , CC")))

SHI_range <- ROAR_SHI_long2 %>% subset(Study %in% "SHI") %>%
  group_by(`Land Management`, Variable) %>%
  mutate(Min = min(Value),
         Max = max(Value)) %>%
  distinct(`Land Management`, Variable, Min, Max) %>%
  mutate(`Land Management` = factor(`Land Management`, levels = c("CT , NCC",
                                                                  "CT , CC",
                                                                  "NT , NCC",
                                                                  "NT , CC")))

Greyplot <- ggplot() +
  geom_segment(SHI_range, mapping = aes(x = `Land Management`, y = Min, yend = Max), color = "grey", alpha = 0.5, linewidth = 24) + 
  geom_errorbar(ROAR, mapping = aes(x = `Land Management`, ymin = Min, ymax = Max, color = `Study, Season`), width = 0.4, position = position_dodge(width = 0.5)) + 
  geom_point(ROAR, mapping = aes(x = `Land Management`, y = Mean, color = `Study, Season`), position = position_dodge(width = 0.5), size = 4) +
  
  facet_wrap(vars(Variable), scales = "free") + 
  theme_bw()+ 
  ylab("Variable") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15))

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("ROAR_vs_SHI_seasons_greplot.pdf", Greyplot, height = 15, width = 25 , limitsize = F)
