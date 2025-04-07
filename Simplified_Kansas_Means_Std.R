library("dplyr")
library("tidyr")
library("ggplot2")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20231127_LW Modified.csv", header = T)
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Crop.2023,
                              Tillage,
                              Cover.Crop,
                              Field,
                              Sample.ID,
                              OM.,
                              NO3_N_H2O,
                              CN_H2O,
                              OC_H2O_ppm,
                              CO2,
                              Aggstab1_2mm,
                              CEC) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `Water Extractable Nitrate Nitrogen (mg/L)` = "NO3_N_H2O",
         `Water Extractable Organic Carbon (mg/L)` = "OC_H2O_ppm",
         `WEOC:WEON` = "CN_H2O",
         `24 hr-CO2 Respiration (mg/kg)` = "CO2",
         `Aggregate Stability 1-2mm` = "Aggstab1_2mm") %>%
  mutate( `Carbon (%)` = `Organic Matter (%)`/1.72)

Haney_Test[,6:length(Haney_Test)] <- as.numeric(unlist(Haney_Test[,6:length(Haney_Test)]))

Long_H <-  pivot_longer(Haney_Test, cols = c(6:length(Haney_Test)), names_to = "Variable", values_to = "Value")
  
#Solving for the means and standard deviation
Haney_Means <- Long_H %>%
  group_by(Field, Variable) %>%
  mutate(Mean = mean(Value, na.rm = T),
         Sd = sd(Value, na.rm = T)) %>%
  distinct(Mean, Sd) %>% ungroup() %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

#Doing a separate dataframe for pH, because it needs to be unlogged to find a medium
pH <- Data %>% select(Crop.2023,
                      Tillage,
                      Cover.Crop,
                      Field,
                      Sample.ID,
                      pH) 

#Long format makes it the same format as Haney_Means
pH_long <-  pivot_longer(pH, cols = c(6), names_to = "Variable", values_to = "Value")
#Means, Sd, Min and Max for pH is all calculated here
pH <- pH_long %>%
  mutate(Sd = sd(Value)) %>%
  mutate(pH = 10**(-Value)) %>%
  group_by(Field, Variable) %>%
  mutate(Mean = mean(Value)) %>%
  mutate(pH = -log10(Value)) %>%
  distinct(Mean, Sd) %>% ungroup %>%
  mutate(Min = Mean - Sd,
         Max = Mean + Sd) %>%
  mutate(Min = replace(Min, which(Min < 0), 0))

#Bringing the two dataframes together
Haney_Means <- rbind(Haney_Means, pH)
         
#Limiting significant figures to 2 
Haney_Means$Mean <- signif(Haney_Means$Mean, digits = 2)


#Adding Boundaries from Kutz (Nebrabska) and Souza et al., 2023 (Kansas)
Haney_Means2 <- Haney_Means %>% 
  mutate(Boundaries = case_when(grepl("Aggregate Stability 1-2mm", Variable) ~ 39,
                                grepl("Organic Matter", Variable) ~ 5.54,
                                grepl("24 hr-CO2 Respiration", Variable) ~ 1040,
                                grepl("pH", Variable) ~ 6.83))


#write.csv(Haney_Means, "Simplified_Kansas_Means_Sd.csv")
#A barplot of all the variables read in, including error bars and the mean values
Haney_Plot <- ggplot(Haney_Means2, aes(x = Field, y = Mean, fill = Field)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) +
  geom_text(aes(label = Mean), size = 8, hjust = 1, vjust = 1, position = position_dodge(width = 0.9)) +
  geom_hline(aes(yintercept = Boundaries), linetype = "dotted") + 
  facet_wrap(vars(Variable), scales = "free") +
  scale_color_manual(values = Field_col) +
  scale_fill_manual(values = Field_col) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("Simplified_Initial_Barplot.pdf", Haney_Plot, height = 15, width = 25)




#For Just Field W1
W1 <- Haney_Means %>% subset(Field %in% "W1")
#A barplot of all the variables read in, including error bars and the mean values
Haney_Plot <- ggplot(W1, aes(x = Field, y = Mean, fill = Field)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(x = Field, ymax = Max, ymin = Min), position = position_dodge(width = 0.9), 
                width = 0.2) +
  geom_text(aes(label = Mean), size = 7, hjust = 1, vjust = 3, position = position_dodge(width = 2)) +
  facet_wrap(vars(Variable), scales = "free") +
  scale_color_manual(values = Field_col) +
  scale_fill_manual(values = Field_col) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("W1_Haney_Initial_Barplot.pdf", Haney_Plot, height = 15, width = 30)
