library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("data.table")
library("scales")
library("lubridate")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20231127_LW Modified.csv", header = T)

Field_col <- c(E1 = "#332288", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )



Crop_col <- c("Soybeans" = "#332288", "Heavy Crop Mix" = "#117733", "Corn" = "#BB9D05")

Tillage_Cover <- c("Cover Crop No-Till" = 17, "No Cover Crop No-Till" = 15, "No Cover Crop Conventional Till" = 0, "Cover Crop Conventional Till" = 2,
                   "NA NA" = 19)

#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Crop.2023,
                              Tillage,
                              Cover.Crop,
                              Field,
                              Sample.ID,
                              NO3_N_H2O,
                              pH,
                              SoilHealth,
                              OM.,
                              OC_H2O_ppm,
                              ON_H2O_ppm,
                              CN_H2O,
                              NH4_N_H2O,
                              CO2,
                              N_H2O,
                              X.MAC,
                              ON_release_ppm,
                              Aggstab1_2mm,
                              H3A_NO3,
                              H3A_NH4,
                              #H3A_IN,
                              Biomass,
                              Fungi_Bacteria
) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `WEN-NO3 (mg/L)` = "NO3_N_H2O",
         `WEOC (mg/L)` = "OC_H2O_ppm",
         `WEON (mg/L)` = "ON_H2O_ppm",
         `WEOC:WEON` = "CN_H2O",
         `WEN-NH4 (mg/L)` = "NH4_N_H2O",
         `CO2 Respiration (mg/L)` = "CO2",
         `MAC (%)` = "X.MAC",
         `Aggregate Stability 1-2mm (%)` = "Aggstab1_2mm",
         `ON Release (mg/L)` = "ON_release_ppm",
         `H3A-NO3(mg/L)` = "H3A_NO3",
         `H3A-NH4 (mg/L)` = "H3A_NH4",
         `WEN (mg/L)` = "N_H2O",
         `Fungi:Bacteria` = "Fungi_Bacteria")

Haney_Test[,7:22] <- as.numeric(unlist(Haney_Test[,7:22]))

Long_H <-  pivot_longer(Haney_Test, cols = c(7:22), names_to = "Variable", values_to = "Value")

Long_H$`Cover & Till` <- paste(Long_H$Cover.Crop, Long_H$Tillage)

WEN_NO3_Correlations <- ggplot(Long_H) + 
  geom_point(aes(x = `Value`, y = `WEN-NO3 (mg/L)`, color = Cash_Crop, shape = `Cover & Till`), size = 8) + 
  ggh4x::facet_grid2(Variable ~ Field, scales = "free", independent = "x") +
  scale_color_manual(values = Crop_col) +
  scale_shape_manual(values = Tillage_Cover) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25),
        aspect.ratio = 1)

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("WEN_NO3_Correlations_No_Mean.pdf", WEN_NO3_Correlations, height = 58, width = 58, limitsize = F)



##Spearman Correlations because they don't require normalization. Seeing what the bigger drivers could be by field
ModelDf <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(ModelDf) <- c("Field", "Correlation", "Variable", "P-value")
Df <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(Df) <- c("Field", "Correlation", "Variable", "P-value")
slist <- list("E1", "E2", "E3", "SS1", "S2", "S3", "S4", "W1")
elist <- list("pH","SoilHealth", "Organic Matter (%)", "WEOC (mg/L)","WEON (mg/L)", 
              "WEN-NH4 (mg/L)", "CO2 Respiration (mg/L)", "WEN (mg/L)", "MAC (%)",
              "H3A-NH4 (mg/L)", "Biomass", "Fungi:Bacteria", "Aggregate Stability 1-2mm (%)", 
              "WEOC:WEON", "ON Release (mg/L)", "H3A-NO3(mg/L)")


for (s in 1:length(slist)){
  Ranked <- Long_H %>% subset(Field %in% c(slist[[s]]))
  for (e in 1:length(elist)){
    Properties <- Ranked %>% subset(Variable %in% c(elist[[e]])) %>%
      mutate(Properties_Rank = rank(Value),
             WEN_NO3_Rank = rank(`WEN-NO3 (mg/L)`))
    Spearman <- cor.test(Properties$Properties_Rank, Properties$WEN_NO3_Rank, method = "spearman", exact = FALSE)
    Df[e, "Field"] <- paste(slist[[s]])
    Df[e,"Variable"]<- paste(elist[[e]])
    Df[e, "Correlation"] <- Spearman[[4]]
    Df[e, "P-value"] <- Spearman[[3]]
  }
  ModelDf <- rbind(ModelDf, Df)
}

ModelDf<-ModelDf[-1,] 

write.csv(ModelDf, "WEN_NO3_Spearman_Correlations.csv")


#For correlation plot
Corr <- left_join(Long_H, ModelDf)


WEN_Corplot <- ggplot(Corr) + 
  geom_point(aes(x = `Value`, y = `WEN-NO3 (mg/L)`, color = Correlation, shape = `Cover & Till`, size = `P-value`)) + 
  guides(shape = guide_legend(override.aes = list(size = 10))) +
  facet_wrap(vars(Variable), scales = "free", strip.position = "bottom") +
  scale_color_gradient(low = "#DC3220", high = "#005AB5") + 
  scale_shape_manual(values = Tillage_Cover) + 
  scale_size("p-values", range = c(10, 3), breaks = c(0.05, 0.1, 0.5, 1)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 25),
        aspect.ratio = 1,
        strip.placement = "outside")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
ggsave("WEN_NO3_Correlations_No_Mean_Spearman.pdf", WEN_Corplot, height = 30, width = 30, limitsize = F)

##Ignoring site differences altogether
ModelDf <- data.frame(matrix(nrow = 1, ncol = 3))
colnames(ModelDf) <- c("Correlation", "Variable", "P-value")
Df <- data.frame(matrix(nrow = 1, ncol = 3))
colnames(Df) <- c("Correlation", "Variable", "P-value")

elist <- list("pH","SoilHealth", "Organic Matter (%)", "WEOC (mg/L)","WEON (mg/L)", 
              "WEN-NH4 (mg/L)", "CO2 Respiration (mg/L)", "WEN (mg/L)", "MAC (%)",
              "H3A-NH4 (mg/L)", "Biomass", "Fungi:Bacteria", "Aggregate Stability 1-2mm (%)", 
              "WEOC:WEON", "ON Release (mg/L)", "H3A-NO3(mg/L)"
              )

for (e in 1:length(elist)){
  Properties <- Ranked %>% subset(Variable %in% c(elist[[e]])) %>%
    mutate(Properties_Rank = rank(Value),
           WEN_NO3_Rank = rank(`WEN-NO3 (mg/L)`))
  Spearman <- cor.test(Properties$Properties_Rank, Properties$WEN_NO3_Rank, method = "spearman", exact = FALSE)

  Df[e,"Variable"]<- paste(elist[[e]])
  Df[e, "Correlation"] <- Spearman[[4]]
  Df[e, "P-value"] <- Spearman[[3]]
}
ModelDf <- rbind(ModelDf, Df)



ModelDf<-ModelDf[-1,] 

write.csv(ModelDf, "WEN_NO3_Spearman_Correlations.csv")