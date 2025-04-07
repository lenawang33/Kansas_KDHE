library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpattern")
library("ggpubr")
library("viridis")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20240520_LW Modified.csv", header = T)
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )
Season_col <- c(Spring = "#56B4E9", Fall = "#E69900")


#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Crop.2023,
                              Tillage,
                              Cover.Crop,
                              Field,
                              Sample_ID,
                              Season,
                              OM.,
                              Ace.Protein.g.Kg,
                              NO3_N_H2O,
                              OC_H2O_mg.kg,
                              ON_H2O_mg.kg,
                              CN_H2O,
                              NH4_N_H2O,
                              CO2_24hr,
                              N_H2O,
                              X.MAC,
                              Aggstab1_2mm,
                              X..Clay,
                              X..Silt,
                              X..Sand,
                              CEC,
                              ON_release_ppm,
                              AA_Ca,
                              AA_Mg,
                              AA_K,
                              AA_Na,
                              X.Hsat,
                              X.Ksat,
                              X.Casat,
                              X.Mgsat,
                              X.Nasat,
                              H3A_NO3,
                              H3A_NH4,
                              H3A_P,
                              H3A_IP,
                              H3A_OP,
                              H3A_ICAPS,
                              H3A_ICAPK,
                              H3A_ICAPCa,
                              H3A_ICAPAl,
                              H3A_ICAPFe,
                              H3A_ICAPZn,
                              H3ACu,
                              H3A_ICAPMg,
                              H3A_ICAPNa,
                              SoilHealth,
                              Biomass,
                              Total_Fungi_Biomass,
                              UndifferentiatedBiomass,
                              TotalBacteriabiomass,
                              Fungi_Bacteria,
                              Protozoa_biomasss,
                              Salts,
                              Gram_neg_biomass,
                              gram_pos_biomass,
                              X.C,
                              X.13C,
                              X.N,
                              X.15N,
                              C.N
) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `Water Extractable Nitrate Nitrogen (mg/kg)` = "NO3_N_H2O",
         `Water Extractable Organic Carbon (mg/kg)` = "OC_H2O_mg.kg",
         `Water Extractable Organic Nitrogen (mg/kg)` = "ON_H2O_mg.kg",
         `WEOC:WEON` = "CN_H2O",
         `Calcium (mg/kg)` = "AA_Ca",
         `Potassium (mg/kg)` = "AA_K",
         `Magnesium (mg/kg)` = "AA_Mg",
         `Sodium (mg/kg)` = "AA_Na",
         `Water Extractable Ammonium Nitrogen (mg/kg)` = "NH4_N_H2O",
         `24 hour CO2 Respiration (mg/kg)` = "CO2_24hr",
         `Microbially Active Carbon (%)` = "X.MAC",
         `Autoclavable Citrate Protein (g/kg)` = "Ace.Protein.g.Kg",
         `Organic Nitrogen Release (mg/kg)` = "ON_release_ppm",
         `H3A Extracted Nitrate (mg/kg)` = "H3A_NO3",
         `H3A Extracted Ammonium (mg/kg)` = "H3A_NH4",
         `H3A Extracted Phosphorous (mg/kg)` = "H3A_P",
         `H3A Extracted Inorganic Phosphorous (mg/kg)` = "H3A_IP",
         `H3A Extracted Organic Phosphorous (mg/kg)` = "H3A_OP",
         `H3A Extracted Sulfur (mg/kg)` = "H3A_ICAPS",
         `H3A Extracted Potassium (mg/kg)` = "H3A_ICAPK",
         `Water Extractable Nitrogen (mg/kg)` = "N_H2O",
         `H3A Extracted Calcium (mg/kg)` = "H3A_ICAPCa",
         `H3A Extracted Aluminum (mg/kg)` = "H3A_ICAPAl",
         `H3A Extracted Iron (mg/kg)` = "H3A_ICAPFe",
         `H3A Extracted Zinc (mg/kg)` = "H3A_ICAPZn",
         `H3A Extracted Copper (mg/kg)` = "H3ACu",
         `H3A Extracted Magnesium (mg/kg)` = "H3A_ICAPMg",
         `H3A Extracted Sodium (mg/kg)` = "H3A_ICAPNa",
         `Clay (%)` = "X..Clay",
         `Silt (%)` ="X..Silt",
         `Sand (%)` ="X..Sand",
         `Electrical Conductivity (mmhos/cm)` = "Salts",
         `Aggregate Stability 1-2 mm (%)` = "Aggstab1_2mm",
         `Microbial Biomass (ug/g)` = "Biomass",
         `Fungal Biomass (ug/g)` = "Total_Fungi_Biomass",
         `Undifferentiated Biomass (ug/g)` = "UndifferentiatedBiomass",
         `Bacteria Biomass (ug/g)` = "TotalBacteriabiomass",
         `Fungi:Bacteria (ug/g)` = "Fungi_Bacteria",
         `Gram(-) Biomass (ug/g)` = "Gram_neg_biomass",
         `Gram(+) Biomass (ug/g)` = "gram_pos_biomass",
         `% BS H` = X.Hsat,
         `% BS K` = X.Ksat,
         `% BS Mg` = X.Mgsat,
         `% BS Ca` = X.Casat,
         `% BS Na` = X.Nasat,
         `Protozoa Biomass (ug/g)` = "Protozoa_biomasss",
         `% Carbon` = "X.C",
         `δ13 Carbon` = "X.13C",
         `% Nitrogen` =  "X.N",
         `δ15 Nitrogen` = "X.15N",
         `Carbon:Nitrogen Ratio` = "C.N"
  ) %>%
  mutate( `Carbon (%)` = `Organic Matter (%)`/1.72) %>%
  filter(`WEOC:WEON` < 80) %>%
  mutate(`Crop Diversity` = case_when(grepl("SS1", Field) ~ "Crop Diversity < 10",
                                      grepl("E1", Field) ~ "Crop Diversity < 10",
                                      grepl("E2", Field) ~ "Crop Diversity < 10",
                                      grepl("E3", Field) ~ "Crop Diversity < 10",
                                      grepl( "S2", Field) ~ "Crop Diversity < 10",
                                      grepl("S3", Field) ~ "Crop Diversity > 10",
                                      grepl("S4", Field) ~ "Crop Diversity > 10",
                                      grepl("W1", Field) ~ "Crop Diversity > 10")) %>%
  rename("Cover Crop" = "Cover.Crop") %>%
  mutate(Field = factor(Field, levels = c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1")),
         `Nitrogen (mg/kg)` = `% Nitrogen` *100)

#Nitrate
Cover_Crop <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrate Nitrogen (mg/kg)`)) +
  geom_point(aes(color = Season), size = 5) +
  #scale_color_manual(values = c("#15F3AE", "grey")) + 
  labs(x = quote(paste(delta^15, Nitrogen)),  y = quote(paste(NO[3]^`-`))) + 
  theme_bw() +
  scale_color_manual(values = Season_col) + 
  facet_grid(~Field) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_Nitrate_CC.pdf", Cover_Crop, height = 5, width = 20, limitsize = F)


## Convert things from percent to mg/kg
Haney_Test$Nitrogenmgkg <- Haney_Test$`% Nitrogen`* 10000
Haney_Test$Carbonmgkg <- Haney_Test$`% Carbon` * 10000


#Convert mg/kg to mol/kg
Haney_Test$Nitrogenmolkg <- Haney_Test$Nitrogenmgkg* (1/14.007) * 0.001
Haney_Test$Carbonmolkg <- Haney_Test$Carbonmgkg * (1/12.011) * 0.001


#Plotting Nitrogen isotope data 
library(cmocean)
N15_CN <- ggplot(Haney_Test) +
  geom_point(aes(x = Carbonmolkg,
                 y = Nitrogenmolkg,
                 color = Season), size = 5) +
  geom_abline(slope = 4/5, intercept = -0.80, linewidth = 2, color = "red") +
  #geom_abline(slope = 16/106, intercept = -0.80, linewidth = 2, color = "green") +
  xlab('Carbon (mol/kg)') +
  ylab('Nitrogen (mol/kg)') +
  theme_bw() +
  #scale_color_cmocean(name = "dense", limits = c(-5,15), alpha = 0.75 ) +
  # scale_color_viridis() + 
  scale_x_log10() +
  scale_y_log10() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15), aspect.ratio = 1) #Wanted to minimze nitrate, have the carbon. When looking at the other relationship, extractable nitrate made a much smaller pool than total nitrogen. Soil nitrogen - long term, sum/integration of the processes over the long years. 


ggplot() +
  #geom_point(data = Haney_Test, aes(x = `Water Extractable Nitrate Nitrogen (mg/kg)`, y = Nitrogenmgkg, color = "Nit N")) +
  geom_point(data = Haney_Test, aes(x = `Water Extractable Nitrogen (mg/kg)`, y = Nitrogenmgkg, color = "mgkg N")) +
  theme_bw()
##############################################################################
## Calculate inorganic carbon 
df_slab_2011$est_IO_CARBON <- df_slab_2011$CACO3_WETOX_2MM
df_slab_2011$EST_O_CARBON <- df_slab_2011$C_TOT_ELEM_ANLYS - df_slab_2011$est_IO_CARBON*0.12

# Conversion equation from Amanda for the organic carbon 
# # ================ Percent Organic Carbon Calculation ================
# 
# # Formula: % Total Carbon - (% CaCO3 x 0.12) = % Organic Carbon
# 
# # Use a correction factor of 0.12 to account for the amount of C (mass = 12.01) in CaCO3 (mass = 100.09)
#
##############################################################################
## Convert things from percent to mg/kg
#df_slab_2011$EST_O_CARBON_mgkg <- df_slab_2011$EST_O_CARBON * 10000
#df_slab_2011$C_TOT_ELEM_ANLYS_mgkg <- df_slab_2011$C_TOT_ELEM_ANLYS * 10000
#df_slab_2011$N_TOT_ELEM_ANLYS_mgkg <- df_slab_2011$N_TOT_ELEM_ANLYS * 10000

#Convert mg/kg to mol/kg
#df_slab_2011$EST_O_CARBON_molkg <- df_slab_2011$EST_O_CARBON_mgkg * (1/12.011) * 0.001
#df_slab_2011$N_TOT_ELEM_ANLYS_molkg <- df_slab_2011$N_TOT_ELEM_ANLYS_mgkg * (1/14.007) * 0.001

##############################################################################
## Code for plot 
Library(ggplot2)
Library(scales)
Library(cmocean)

plt_d15N <- ggplot() +
  geom_point(data = df_SOSI, aes(x = EST_O_CARBON_molkg, y = N_TOT_ELEM_ANLYS_molkg, color = delta15N), alpha = 0.75, size = 3) +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) +
  # guides(color = "none") +
  scale_color_cmocean(name = "dense", limits = c(-5,15)) +
  geom_abline(slope = 4/5, intercept = -0.80, size = 2, color = "red") +
  annotate("text", x = 0.4, y = 0.1, label = "Denitrification 4/5 (0.8)", fontface = "bold", angle = 49*4/5) +
  coord_fixed() +
  labs(x = "Organic C mol/kg (log10 scale)", y = "total N mol/kg  (log10 scale)") +
  theme_bold(txt_sz = 18)


















Tillage <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrate Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Tillage`), size = 5) +
  scale_color_manual(values = c("grey" ,"#AFADFD")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(NO[3]^`-`))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Crop_Diversity <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrate Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Crop Diversity`), size = 5) +
  scale_color_manual(values = c("grey", "#FF6C9E")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(NO[3]^`-`))) +  
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Season <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrate Nitrogen (mg/kg)`)) +
  geom_point(aes(color = Season), size = 5) +
  scale_color_manual(values = c(Spring = "#56B4E9", Fall = "#E69900")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(NO[3]^`-`))) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Arranged <- ggarrange(Cover_Crop, Tillage, Crop_Diversity, Season)
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_Nitrate.pdf", Arranged, height = 15, width = 15, limitsize = F)


#Nitrogen
Cover_Crop <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `% Nitrogen`*100)) +
  geom_point(aes(color = Season), size = 5) +
  #scale_color_manual(values = c("#15F3AE", "grey")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(Nitrogen, (mg/kg)))) + 
  theme_bw() +
  scale_color_manual(values = Season_col) + 
  facet_grid(~Field) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_Nitrogen_CC.pdf", Cover_Crop, height = 5, width = 20, limitsize = F)

Haney_Means <- Haney_Test %>%
  select(Field, Season, `Nitrogen (mg/kg)`, `δ15 Nitrogen`, `Water Extractable Nitrate Nitrogen (mg/kg)`) %>%
  group_by(Field, Season) %>%
  mutate(N_mean = mean(`Nitrogen (mg/kg)`, na.rm = T),
         NI_mean = mean(`δ15 Nitrogen`, na.rm = T),
         NO3_mean = mean(`Water Extractable Nitrate Nitrogen (mg/kg)`, na.rm = T)) %>%
  mutate(N_sd = sd(`Nitrogen (mg/kg)`, na.rm = T),
         NI_sd = sd(`δ15 Nitrogen`, na.rm = T),
         NO3_sd = sd(`Water Extractable Nitrate Nitrogen (mg/kg)`, na.rm = T)) %>%
  ungroup() %>% distinct(Field, Season, N_mean, NI_mean, NO3_mean, N_sd, NI_sd, NO3_sd)

#Nitrogen
Cover_Crop <- ggplot(Haney_Means, aes(x = `NI_mean`, y = `N_mean`)) +
  geom_point(aes(color = Season), size = 5) +
  geom_errorbar(aes(x = `NI_mean`, ymax = N_mean + N_sd, ymin = N_mean - NO3_sd)) + 
  geom_errorbarh(aes(y = `N_mean`, xmax =  NI_mean + NI_sd, xmin = NI_mean - NI_sd,)) +
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(Nitrogen, (mg/kg)))) + 
  theme_bw() +
  scale_color_manual(values = Season_col) + 
  facet_grid(~Field) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 23), aspect.ratio = 1)

ggsave("Isotope_Nitrogen_CC_means.pdf", Cover_Crop, height = 5, width = 20, limitsize = F)


Cover_Crop <- ggplot(Haney_Means, aes(x = `NI_mean`, y = `NO3_mean`)) +
  geom_point(aes(color = Season), size = 5) +
  geom_errorbar(aes(x = `NI_mean`, ymax = NO3_mean + NO3_sd, ymin = NO3_mean - NO3_sd)) + 
  geom_errorbarh(aes(y = `NO3_mean`, xmax =  NI_mean + NI_sd, xmin = NI_mean - NI_sd,)) +
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(NO[3]^`-`))) + 
  theme_bw() +
  scale_color_manual(values = Season_col) + 
  facet_grid(~Field) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 23), aspect.ratio = 1)

ggsave("Isotope_Nitrate_CC_means.pdf", Cover_Crop, height = 5, width = 20, limitsize = F)





#Water Extractable Nitrogen
Cover_Crop <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Cover Crop`), size = 5) +
  scale_color_manual(values = c("#15F3AE", "grey")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Nitrogen))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)


Tillage <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Tillage`), size = 5) +
  scale_color_manual(values = c("grey" ,"#AFADFD")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Nitrogen))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Crop_Diversity <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Crop Diversity`), size = 5) +
  scale_color_manual(values = c("grey", "#FF6C9E")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Nitrogen))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Season <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Nitrogen (mg/kg)`)) +
  geom_point(aes(color = Season), size = 5) +
  scale_color_manual(values = c(Spring = "#56B4E9", Fall = "#E69900")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Nitrogen))) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Arranged <- ggarrange(Cover_Crop, Tillage, Crop_Diversity, Season)
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_WEN.pdf", Arranged, height = 15, width = 15, limitsize = F)


#Water Extractable Organic Nitrogen
Cover_Crop <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Organic Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Cover Crop`), size = 5) +
  scale_color_manual(values = c("#15F3AE", "grey")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Organic,Nitrogen))) +  
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)


Tillage <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Organic Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Tillage`), size = 5) +
  scale_color_manual(values = c("grey" ,"#AFADFD")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Organic,Nitrogen))) +  
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Crop_Diversity <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Organic Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Crop Diversity`), size = 5) +
  scale_color_manual(values = c("grey", "#FF6C9E")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Organic,Nitrogen))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Season <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Organic Nitrogen (mg/kg)`)) +
  geom_point(aes(color = Season), size = 5) +
  scale_color_manual(values = c(Spring = "#56B4E9", Fall = "#E69900")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`, Organic,Nitrogen))) +  
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Arranged <- ggarrange(Cover_Crop, Tillage, Crop_Diversity, Season)
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_WEON.pdf", Arranged, height = 15, width = 15, limitsize = F)


#Water Ammonium
Cover_Crop <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Ammonium Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Cover Crop`), size = 5) +
  scale_color_manual(values = c("#15F3AE", "grey")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`,NH[4]^`+`))) +  
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)


Tillage <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Ammonium Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Tillage`), size = 5) +
  scale_color_manual(values = c("grey" ,"#AFADFD")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`,NH[4]^`+`))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Crop_Diversity <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Ammonium Nitrogen (mg/kg)`)) +
  geom_point(aes(color = `Crop Diversity`), size = 5) +
  scale_color_manual(values = c("grey", "#FF6C9E")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`,NH[4]^`+`))) + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Season <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Water Extractable Ammonium Nitrogen (mg/kg)`)) +
  geom_point(aes(color = Season), size = 5) +
  scale_color_manual(values = c(Spring = "#56B4E9", Fall = "#E69900")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = quote(paste(H[2], O,`-`,NH[4]^`+`))) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Arranged <- ggarrange(Cover_Crop, Tillage, Crop_Diversity, Season)
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_WENH4.pdf", Arranged, height = 15, width = 15, limitsize = F)


#Water Organic Matter
Cover_Crop <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Organic Matter (%)`)) +
  geom_point(aes(color = `Cover Crop`), size = 5) +
  scale_color_manual(values = c("#15F3AE", "grey")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = "% Organic Matter") +  
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)


Tillage <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Organic Matter (%)`)) +
  geom_point(aes(color = `Tillage`), size = 5) +
  scale_color_manual(values = c("grey" ,"#AFADFD")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = "% Organic Matter") + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Crop_Diversity <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Organic Matter (%)`)) +
  geom_point(aes(color = `Crop Diversity`), size = 5) +
  scale_color_manual(values = c("grey", "#FF6C9E")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = "% Organic Matter") + 
  theme_bw() +
  #facet_wrap(vars(Season)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Season <- ggplot(Haney_Test, aes(x = `δ15 Nitrogen`, y = `Organic Matter (%)`)) +
  geom_point(aes(color = Season), size = 5) +
  scale_color_manual(values = c(Spring = "#56B4E9", Fall = "#E69900")) + 
  labs(x = quote(paste(delta^15, Nitrogen)), y = "% Organic Matter") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25), aspect.ratio = 1)

Arranged <- ggarrange(Cover_Crop, Tillage, Crop_Diversity, Season)
setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Isotope_OM.pdf", Arranged, height = 15, width = 15, limitsize = F)
