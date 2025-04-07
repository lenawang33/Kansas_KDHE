library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpubr")
library("stringr")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20231127_LW Modified.csv", header = T)
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
PLFA <- Data %>% select(Crop.2023,
                        Date_Rept,
                        Tillage,
                        Cover.Crop,
                        Field,
                        Sample_ID,
                        Fungi_Bacteria,
                        Grampos_Gramneg,
                        Sat_Unsat,
                        Mono_Poly,
                        Predator_Prey) %>%
  mutate(Field = factor(Field, levels = c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1"))) %>%
  filter(str_detect(Date_Rept, "2023"))
  

#Reading in just predator and prey data
Pred_Prey <- PLFA %>% select(Crop.2023,
                        Tillage,
                        Cover.Crop,
                        Field,
                        Sample_ID,
                        Predator_Prey) %>%
  mutate(Field = factor(Field, levels = c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1")))

#replacing All Prey with 0, because it falls in the "Very Poor" category according to Ward Labs
Pred_Prey$Predator_Prey[which(Pred_Prey$Predator_Prey == "ALL PREY")] <- -1

#replacing all numbers with the range that Ward Labs provided
Pred_Prey <- Pred_Prey %>%  mutate(`Predator:Prey` = case_when(Predator_Prey < 0.002 & Predator_Prey > 0.000 ~ "Very Poor",
                                                           Predator_Prey > 0.002 & Predator_Prey <= 0.005 ~ "Poor",
                                                           Predator_Prey > 0.005 & Predator_Prey <= 0.008 ~ "Slightly Below Average",
                                                           Predator_Prey > 0.008 & Predator_Prey <= 0.01 ~ "Average",
                                                           Predator_Prey > 0.01 & Predator_Prey <= 0.013 ~ "Slightly Above Average",
                                                           Predator_Prey > 0.013 & Predator_Prey <= 0.016 ~ "Good",
                                                           Predator_Prey > 0.016 & Predator_Prey <= 0.02 ~ "Very Good",
                                                           Predator_Prey > 0.02 ~ "Excellent",
                                                           Predator_Prey < 0.00 ~ "All Prey"))

#Factoring so that the legend is in the correct order from low to high
Pred_Prey <- Pred_Prey %>%
  mutate(`Predator:Prey` = factor(`Predator:Prey`, levels = c("Excellent", "Very Good", "Good", "Slightly Above Average", 
                                                   "Average", "Slightly Below Average", "Poor", "Very Poor", "All Prey"))) %>%
  #Groups by Field and Type of count, calculated the percentage
  group_by(Field, `Predator:Prey`) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ungroup()

#Adding Color Blind Friendly Colors
Quality_col <- c(`All Prey` = "Black" ,`Very Poor` = "#332288", `Poor` = "#117733", `Slightly Below Average` = "#44AA99",  Average = "#88CCEE", `Slightly Above Average` = "#BB9D05",  Good = "#CC6677",`Very Good` = "#AA4499", Excellent ="#882255" )


#Producing the plot, this is by count of sample, not by anything else. 
Pred_Prey_Plot <- ggplot(Pred_Prey, aes(x = Field, y = perc*100, fill = `Predator:Prey`)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_color_manual(values = Quality_col) +
  scale_fill_manual(values = Quality_col) +
  theme_bw() +
  ylab("Percentage") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


#Doing the same thing with Fungi:Bacteria Ratios
Fun_B <- PLFA %>% select(Crop.2023,
                        Tillage,
                        Cover.Crop,
                        Field,
                        Sample_ID,
                        Fungi_Bacteria)

Fun_B <- Fun_B %>%  mutate(Fungi_Bacteria = case_when(Fungi_Bacteria < 0.05 ~ "Very Poor",
                                                             Fungi_Bacteria > 0.05 & Fungi_Bacteria <= 0.1 ~ "Poor",
                                                             Fungi_Bacteria > 0.1 & Fungi_Bacteria <= 0.15 ~ "Slightly Below Average",
                                                             Fungi_Bacteria > 0.15 & Fungi_Bacteria <= 0.2 ~ "Average",
                                                             Fungi_Bacteria > 0.2 & Fungi_Bacteria <= 0.25 ~ "Slightly Above Average",
                                                             Fungi_Bacteria > 0.25 & Fungi_Bacteria <= 0.3 ~ "Good",
                                                             Fungi_Bacteria > 0.3 & Fungi_Bacteria <= 0.35 ~ "Very Good",
                                                             Fungi_Bacteria > 0.35 ~ "Excellent"))

Fun_B <- Fun_B %>%
  mutate(`Fungi:Bacteria` = factor(Fungi_Bacteria, levels = c("Excellent", "Very Good", "Good", "Slightly Above Average", 
                                                            "Average", "Slightly Below Average", "Poor", "Very Poor"))) %>%
  #Groups by Field and Type of count, calculated the percentage
  group_by(Field, `Fungi:Bacteria`) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ungroup()


#Producing the plot, this is by count of sample, not by anything else. 
FunB_Plot <- ggplot(Fun_B, aes(x = Field, y = perc*100, fill = `Fungi:Bacteria`)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_color_manual(values = Quality_col) +
  scale_fill_manual(values = Quality_col) +
  ylab("Percentage") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


#Doing the same thing with the Gram Negative and Positive Data

Grams <- PLFA %>% select(Crop.2023,
                        Tillage,
                        Cover.Crop,
                        Field,
                        Sample_ID,
                        Grampos_Gramneg) %>%
  mutate(Field = factor(Field, levels = c("SS1", "E2", "E3", "E1", "S2", "S3", "S4", "W1")))

Grams <- Grams %>%  mutate(Grampos_Gramneg = case_when(Grampos_Gramneg < 0.5 ~ "Gram(-) Dominated",
                                                      Grampos_Gramneg > 0.5 & Grampos_Gramneg <= 1 ~ "Slightly Gram(-) Dominated",
                                                      Grampos_Gramneg > 1 & Grampos_Gramneg <= 2 ~ "Balanced Bacterial Community",
                                                      Grampos_Gramneg > 2 & Grampos_Gramneg <= 3 ~ "Slightly Gram(+) Dominated",
                                                      Grampos_Gramneg > 3 & Grampos_Gramneg <= 4 ~ "Gram(+) Dominated",
                                                      Grampos_Gramneg > 4 ~ "Very Gram(+) Dominated"))

Grams <- Grams %>%
  mutate(`Gram(+):Gram(-)` = factor(Grampos_Gramneg, levels = c("Very Gram(+) Dominated", 
                                                                "Gram(+) Dominated", 
                                                              "Slightly Gram(+) Dominated",
                                                              "Balanced Bacterial Community",
                                                              "Slightly Gram(-) Dominated", 
                                                              "Gram(-) Dominated"))) %>%
  #Groups by Field and Type of count, calculated the percentage
  group_by(Field, `Gram(+):Gram(-)`) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ungroup()
  

Quality_col <- c(`Gram(-) Dominated` = "#332288",`Slightly Gram(-) Dominated` = "#44AA99",  
                 `Balanced Bacterial Community` = "#88CCEE", `Slightly Gram(+) Dominated` = "#CC6677",
                 `Gram(+) Dominated` = "#AA4499", `Very Gram(+) Dominated` ="#882255" )

GramPlot <- ggplot(Grams, aes(x = Field, y = perc*100, fill = `Gram(+):Gram(-)`)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_color_manual(values = Quality_col) +
  scale_fill_manual(values = Quality_col) +
  ylab("Percentage") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 20))


PLFA_plots <- ggarrange(Pred_Prey_Plot, FunB_Plot, GramPlot)

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("PLFA_Ratios.pdf", PLFA_plots, height = 8, width = 16)
