library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpattern")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Kansas <- read.csv("Soil1_20240520_LW Modified.csv", header = T)
Nebraska <- read.csv("Krupek Index.csv", header = T)

Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )
Season_col <- c(Spring = "#56B4E9", Fall = "#E69900")

Nebraska <- Nebraska %>% select(c(1:6)) %>% rename("Index Value" = "Index_value")

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Kansas %>% dplyr::select(Season,
                                Field,
                                OM.,
                                CO2_24hr,
                                OC_H2O_mg.kg,
                                ON_H2O_mg.kg,
                                NO3_N_H2O, 
                                NH4_N_H2O,
                                H3A_NH4,
                                H3A_NO3,
                                H3A_IP,
                                H3A_OP,
                                SoilHealth) %>%
  rename("Organic Matter (%)" ="OM.",
         "H2O Extractable Organic Carbon (mg/kg)" = "OC_H2O_mg.kg",
         "H2O Extractable Organic Nitrogen (mg/kg)" = "ON_H2O_mg.kg",
         "Haney Extract NH4 (mg/kg)" = "H3A_NH4",
         "Haney Extract NO3 (mg/kg)" = "H3A_NO3",
         "Haney Extract IP (mg/kg)" = "H3A_IP",
         "Haney Extract OP (mg/kg)" = "H3A_OP",
         "Soil Health Score" = "SoilHealth") %>%
  mutate(`H2O Extractable Total Nitrogen (mg/kg)` = `H2O Extractable Organic Nitrogen (mg/kg)` +
                                                        NO3_N_H2O +
                                                        NH4_N_H2O,
         `Haney Extract IN (mg/kg)` = `Haney Extract NH4 (mg/kg)` +`Haney Extract NO3 (mg/kg)`) %>%
  dplyr::select(-c(NO3_N_H2O, NH4_N_H2O, `Haney Extract NH4 (mg/kg)`, `Haney Extract NO3 (mg/kg)` )) %>%
  mutate(`24 hour CO2 Respiration` = CO2_24hr/2.83) %>%
  dplyr::select(-c(CO2_24hr))

Kansas_long <- Haney_Test %>%
  pivot_longer(c(3:11), names_to = "Variable", values_to = "Value")

#Nebraska values: Crop Diversity 2-15, Cover Crop use: 0-12 years, years without soil disturbance 0-30 years, CL 0-5 years)
Kansas_Index <- data.frame(Field = c("E1", "E2", "E3", "S2", "S3", "S4", "SS1", 'W1'))
Kansas_Index <- Kansas_Index %>%
  mutate(`CDI` = case_when(Field == "E1" ~ 2/15,
                                      Field == "E2" ~ 2/15,
                                      Field == "E3" ~ 2/15,
                                      Field == "S2" ~ 5/15,
                                      Field == "S3" ~ 10/15,
                                      Field == "S4" ~ 10/15,
                                      Field == "SS1" ~ 2/15,
                                      Field == "W1" ~ 12/15),
         
         `CCU` = case_when(Field == "E1" ~ 0/12,
                                        Field == "E2" ~ 0/12,
                                        Field == "E3" ~ 0/12,
                                        Field == "S2" ~ 15/12,
                                        Field == "S3" ~ 15/12,
                                        Field == "S4" ~ 15/12,
                                        Field == "SS1" ~ 1/12,
                                        Field == "W1" ~ 20/12),
         `YWSD` = case_when(Field == "E1" ~ 5/12, #This is a guess, but I vaguely remember them saying they had started no till recently
                                                 Field == "E2" ~ 5/12, #This is a guess, but I vaguely remember them saying they had started no till recently
                                                 Field == "E3" ~ 5/12, #This is a guess, but I vaguely remember them saying they had started no till recently
                                                 Field == "S2" ~ 15/12,
                                                 Field == "S3" ~ 15/12, #Applying the same numbers from S2, same farmer
                                                 Field == "S4" ~ 15/12, #Applying the same numbers from S2, same farmer
                                                 Field == "SS1" ~ 0/12,
                                                 Field == "W1" ~ 20/30), 
         `CL` = case_when(Field == "E1" ~ 0/5, #Not grazed in the last five years
                                                  Field == "E2" ~ 0/5, #Not grazed in the last five years
                                                  Field == "E3" ~ 0/5, #Not grazed in the last five years
                                                  Field == "S2" ~ 1/5, # Grazed once in the last 5 years
                                                  Field == "S3" ~ 1/5, #Applying the same numbers from S2, same farmer
                                                  Field == "S4" ~ 1/5, #Applying the same numbers from S2, same farmer
                                                  Field == "SS1" ~ 0/5, #Not grazed in a very long time
                                                  Field == "W1" ~ 5/5)) %>% #Grazed every year in the last five years
  #Adjusting to the ranges set in the Supplementary 3 table
  mutate(`Crop Diversity` = case_when(CDI < 0.45 ~ "Low CD",
                                      CDI >= 0.45 ~ "High CD"),
         `Cover Crop Years` = case_when(CCU < 0.5 ~ "Few to No years w/ CC",
                                        CCU >= 0.5 ~ "Many years w/ CC"),
         `Years Without Disturbance` = case_when(YWSD < 0.5 ~ "Few to No years w/o SD",
                                               CCU >= 0.5 ~ "Many years w/o SD"),
         `Crop Livestock Integration` = case_when(CL < 0.5 ~ "Few to No years w/ A",
                                                  CL >= 0.5 ~ "Many years w/ A"))

Kansas_index_simp <- Kansas_Index %>%
  dplyr::select(c('Field', 6:9))


Kansas_data <- left_join(Kansas_long, Kansas_index_simp)
  
Kansas_data2 <- Kansas_data %>% 
  group_by(Season, Variable, `Crop Diversity`, `Cover Crop Years`,
           `Years Without Disturbance`,`Crop Livestock Integration`) %>%
  mutate(Mean = mean(Value),
         Sd = sd(Value))


Kansas_index_long <- Kansas_data2 %>% pivot_longer(c(5:8), names_to = "Index", values_to = "Index Value") %>% 
  dplyr::select(-c(Value)) %>%
  mutate(Max = Mean + Sd,
         Min = case_when(Mean - Sd < 0 ~ 0,
                         Mean - Sd >= 0 ~ Mean - Sd)) %>%
  rename("Field History" = "Index Value") %>%
  mutate("Field History" = factor(`Field History`, levels = c("Low CD","High CD",
                                                              "Few to No years w/ CC", "Many years w/ CC", 
                                                              "Few to No years w/o SD", "Many years w/o SD", 
                                                              "Few to No years w/ A", "Many years w/ A"))) %>%
  mutate(`Field & Season` = paste(Field, Season, sep = " ")) %>%
  mutate(`Field & Season` = factor(`Field & Season`, levels = c("SS1 Fall", "SS1 Spring",
                                                                 "E1 Fall", "E1 Spring",
                                                                 "E2 Fall", "E2 Spring",
                                                                 "E3 Fall", "E3 Spring",
                                                                 "S2 Fall", "S2 Spring",
                                                                 "S3 Fall", "S3 Spring",
                                                                 "S4 Fall", "S4 Spring",
                                                                 "W1 Fall", "W1 Spring")))


Field_season_col <- c("E1 Fall" = "#543fbf", "E1 Spring" = "#543fbf", 
               "E2 Fall" = "#117733", "E2 Spring" = "#117733",
               "E3 Fall" = "#44AA99", "E3 Spring" = "#44AA99",
               "SS1 Fall" = "#BB9D05", "SS1 Spring" = "#BB9D05" ,
               "S2 Fall" = "#CC6677", "S2 Spring" = "#CC6677",
               "S3 Fall" = "#88CCEE", "S3 Spring" = "#88CCEE",
               "S4 Fall" = "#AA4499", "S4 Spring" = "#AA4499", 
               "W1 Fall" ="#882255", "W1 Spring" ="#882255" )

Season_shape <- c("Fall" = 16 , "Spring" = 1)


Nebraska_mod <- Nebraska %>%
  mutate(Max = Mean + Sd,
         Min = case_when(Mean - Sd < 0 ~ 0,
                         Mean - Sd >= 0 ~ Mean - Sd)) %>%
  rename("Field History" = "Interpretation") %>%
  mutate("Field History" = factor(`Field History`, levels = c("Low CD","High CD",
                                                              "Few to No years w/ CC", "Many years w/ CC", 
                                                              "Few to No years w/o SD", "Many years w/o SD", 
                                                              "Few to No years w/ A", "Many years w/ A")))


Greyplot <- ggplot() +
  geom_segment(Nebraska_mod, mapping = aes(x = `Field History`, y = Min, yend = Max), color = "grey", alpha = 0.5, linewidth = 70) + 
  geom_errorbar(Kansas_index_long, mapping = aes(x = `Field History`, ymin = Min, ymax = Max, color = `Field & Season`), width = 0.4, position = position_dodge(width = 0.9)) + 
  geom_point(Kansas_index_long, mapping = aes(x = `Field History`, y = Mean, color = `Field & Season`, shape = Season), position = position_dodge(width = 0.9), size = 4, stroke = 2) +
  scale_color_manual(values = Field_season_col) + 
  scale_shape_manual(values = Season_shape) + 
  facet_wrap(vars(Variable), scales = "free") + 
  theme_bw()+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15))

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("ROAR_vs_Krupek_seasons_greplot.pdf", Greyplot, height = 15, width = 60 , limitsize = F)


#To get the means not by the site though
Kansas_index_mean <- Kansas_data %>% pivot_longer(c(5:8), names_to = "Index", values_to = "Index Value") %>% 
  group_by(Season, Variable, `Index Value`) %>%
  mutate(Mean = mean(Value),
         Sd = sd(Value)) %>%
  mutate(Max = Mean + Sd,
        Min = case_when(Mean - Sd < 0 ~ 0,
                        Mean - Sd >= 0 ~ Mean - Sd)) %>%
  rename("Field History" = "Index Value") %>%
  mutate("Field History" = factor(`Field History`, levels = c("Low CD","High CD",
                                                              "Few to No years w/ CC", "Many years w/ CC", 
                                                              "Few to No years w/o SD", "Many years w/o SD", 
                                                              "Few to No years w/ A", "Many years w/ A"))) %>%
  dplyr::select(-c(Value, Field)) %>%
  distinct(Season, Variable, Mean, Sd, `Field History`, .keep_all = T)


Greyplot <- ggplot() +
  geom_segment(Nebraska_mod, mapping = aes(x = `Field History`, y = Min, yend = Max), color = "grey", alpha = 0.5, linewidth = 40) + 
  geom_errorbar(Kansas_index_mean, mapping = aes(x = `Field History`, ymin = Min, ymax = Max, color = Season), width = 0.4, position = position_dodge(width = 0.9)) + 
  geom_point(Kansas_index_mean, mapping = aes(x = `Field History`, y = Mean, color = Season), position = position_dodge(width = 0.9), size = 4, stroke = 2) +
  facet_wrap(vars(Variable), scales = "free") + 
  theme_bw()+ 
  ylab("Variable") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15))

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("ROAR_vs_Krupek_seasons_greplot_broad.pdf", Greyplot, height = 10, width = 45 , limitsize = F)


