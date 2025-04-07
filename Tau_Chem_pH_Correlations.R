library("ggplot2")
library("reshape2")
library("stringr")
library("ggpubr")
library("dplyr")
library("tidyverse")
library("ggnewscale")
library("ggh4x")

setwd("C:/Users/lenab/Box/Hydrology_Lab/Projects/Coal_Creek/Soil_Analysis")
Veg_col <- c(Aspen = "#FF9900", Conifer = "#006600")
Lith_col <- c(Granite = "#0066CC", Sandstone = '#FF3366')
Lith_shape <- c(Granite = 17, Sandstone = 16)
Lith_aspect <- c(`Granite NF` = 2, `Granite SF` = 17, `Sandstone NF` = 1, `Sandstone SF` = 16)
Lith_slope <- c(`Granite Backslope` = 2, `Granite Footslope` = 17, `Sandstone Backslope` = 1, `Sandstone Footslope` = 16)



Tau <- read.csv("Tau_Final.csv", header  = T)
pH <- read.csv("pH_CEC.csv", header = T)
Chemistry <- read.csv("Rainwater_Extracts.csv", header = T)
EOC <- read.csv("EOC_Rainwater_Extracts.csv", header = T)



##Tau and pH
pH_2 <- pH %>% subset(Name %in% c("pH")) %>% select(Site_Name, Depth, Mean, Sd)
Tau <- Tau %>% rename("Depth" = "Depth..cm.", "Lithology Slope" = "Lithology.Slope") %>% 
  select(Site_Name, Depth, Lithology, Vegetation, `Lithology Slope`, Element, Tau)
Tau_pH <- left_join(Tau, pH_2)


Aspen_df <- Tau_pH %>% filter(Vegetation == "Aspen") %>% mutate(`Aspen Depth` = Depth)
Conifer_df <- Tau_pH %>% filter(Vegetation == "Conifer") %>% mutate(`Conifer Depth` = Depth)
Aspen_df$`Aspen Depth` <- as.numeric(Aspen_df$`Aspen Depth`)
Conifer_df$`Conifer Depth` <- as.numeric(Conifer_df$`Conifer Depth`)



plot <- ggplot(mapping = aes(x = Mean, y = Tau)) +
  geom_point(data = Conifer_df, aes(color = `Conifer Depth`, shape = `Lithology Slope`), size = 5) +
  geom_errorbarh(data = Conifer_df, aes(y = Tau, xmin = Mean - Sd, xmax = Mean + Sd, color = `Conifer Depth`)) +
  scale_color_gradientn(colors = c('#0cc200', '#006600')) +
  new_scale_color() +
  geom_point(data = Aspen_df, aes(color = `Aspen Depth`, shape = `Lithology Slope`), size = 5) +  
  geom_errorbarh(data = Aspen_df, aes(y = Tau, xmin = Mean - Sd, xmax = Mean + Sd, color = `Aspen Depth`)) +
  scale_color_gradientn(colors = c('#FF9900', "#ff4800")) +
  facet_grid(Element ~ Site_Name, scales = "free_y") +
  scale_shape_manual(values = Lith_slope) + 
  theme_bw() + 
  xlab("pH") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1), aspect.ratio = 1)

##Correlations###
ModelDf <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(ModelDf) <- c("Site_Name", "Tau", "Correlation", "P-value")
Df <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(Df) <- c("Site_Name", "Tau", "Correlation","P-value")
slist <- list("CC1", "CC2", "CC3", "CC4", "CC5")
elist <- list("tau Al", "tau Ca", "tau Fe", "tau K", "tau Mg", "tau Si")

for (s in 1:length(slist)){
  Ranked <- Tau_pH %>% subset(Site_Name %in% c(slist[[s]]))
  for (e in 1:length(elist)){
    Elements <- Ranked %>% subset(Element %in% c(elist[[e]])) %>%
      mutate(pH_Rank = rank(Mean),
             Element_Rank = rank(Tau))
      Spearman <- cor.test(Elements$pH_Rank, Elements$Element_Rank, method = "spearman", exact = FALSE)
      Df[e, "Site_Name"] <- paste(slist[[s]])
      Df[e,"Tau"]<- paste(elist[[e]])
      Df[e, "Correlation"] <- Spearman[[4]]
      Df[e, "P-value"] <- Spearman[[3]]
    }
    ModelDf <- rbind(ModelDf, Df)
}

ModelDf<-ModelDf[-1,] 

write.csv(ModelDf, "Tau_pH_Spearman.csv")






##Tau and EOC
Tau <- read.csv("Tau_Final.csv", header  = T)
EOC_2 <- EOC %>% subset(Name %in% c("EOC (ug/g)")) %>% select(Site_Name, Depth, Mean, Sd)
Tau <- Tau %>% rename("Depth" = "Depth..cm.", "Lithology Slope" = "Lithology.Slope") %>% 
  select(Site_Name, Depth, Lithology, Vegetation, `Lithology Slope`, Element, Tau)
Tau_EOC <- left_join(Tau, EOC_2)

Aspen_df <- Tau_EOC %>% filter(Vegetation == "Aspen") %>% mutate(`Aspen Depth` = Depth)
Conifer_df <- Tau_EOC %>% filter(Vegetation == "Conifer") %>% mutate(`Conifer Depth` = Depth)
Aspen_df$`Aspen Depth` <- as.numeric(Aspen_df$`Aspen Depth`)
Conifer_df$`Conifer Depth` <- as.numeric(Conifer_df$`Conifer Depth`)

plot2 <- ggplot(mapping = aes(x = Mean, y = Tau)) +
  geom_point(data = Conifer_df, aes(color = `Conifer Depth`, shape = `Lithology Slope`), size = 5) +
  geom_errorbarh(data = Conifer_df, aes(y = Tau, xmin = Mean - Sd, xmax = Mean + Sd, color = `Conifer Depth`)) +
  scale_color_gradientn(colors = c('#0cc200', '#006600')) +
  new_scale_color() +
  geom_point(data = Aspen_df, aes(color = `Aspen Depth`, shape = `Lithology Slope`), size = 5) +  
  geom_errorbarh(data = Aspen_df, aes(y = Tau, xmin = Mean - Sd, xmax = Mean + Sd, color = `Aspen Depth`)) +
  scale_color_gradientn(colors = c('#FF9900', "#ff4800")) +
  facet_grid(Element ~ Site_Name, scales = "free_y") +
  scale_shape_manual(values = Lith_slope) + 
  scale_x_log10() + 
  theme_bw() + 
  xlab("EOC (ug/g)") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1), aspect.ratio = 1)



##Correlations###
ModelDf <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(ModelDf) <- c("Vegetation Type", "Root Type", "Carbon Type", "Correlation", "P-value")
Df <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(Df) <- c("Vegetation Type", "Root Type", "Carbon Type", "Correlation", "P-value")
vlist <- list("Aspen", "Conifer")
rlist <- list("Total Roots", "Fine Roots", "Coarse Roots")
slist <- list("SOC (mg/g)", "EOC (ug/g)")

ModelDf <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(ModelDf) <- c("Site_Name", "Tau", "Correlation", "P-value")
Df <- data.frame(matrix(nrow = 1, ncol = 4))
colnames(Df) <- c("Site_Name", "Tau", "Correlation","P-value")
slist <- list("CC1", "CC2", "CC3", "CC4", "CC5")
elist <- list("tau Al", "tau Ca", "tau Fe", "tau K", "tau Mg", "tau Si")

for (s in 1:length(slist)){
  Ranked <- Tau_EOC %>% subset(Site_Name %in% c(slist[[s]]))
  for (e in 1:length(elist)){
    Elements <- Ranked %>% subset(Element %in% c(elist[[e]])) %>%
      mutate(pH_Rank = rank(Mean),
             Element_Rank = rank(Tau))
    Spearman <- cor.test(Elements$pH_Rank, Elements$Element_Rank, method = "spearman", exact = FALSE)
    Df[e, "Site_Name"] <- paste(slist[[s]])
    Df[e,"Tau"]<- paste(elist[[e]])
    Df[e, "Correlation"] <- Spearman[[4]]
    Df[e, "P-value"] <- Spearman[[3]]
  }
  ModelDf <- rbind(ModelDf, Df)
}

ModelDf<-ModelDf[-1,] 

write.csv(ModelDf, "Tau_EOC_Spearman.csv")





###Roots to Tau####
setwd("C:/Users/lenab/Box/Hydrology_Lab/Projects/Coal_Creek/Root_Analysis")
Roots1 <- read.csv("20230108_AllPitRootData_Coal_Creek_Reece_counts.csv", header = T)
Roots2 <- read.csv("20230328_AllPitRootData_Coal_Creek_Reece_counts.csv", header = T)
Roots <- rbind(Roots1, Roots2)
setwd("C:/Users/lenab/Box/Hydrology_Lab/Projects/Coal_Creek/Soil_Analysis")

#Getting roots together
Roots_mean <- Roots %>% group_by(Vegetation, X10_cm_bins) %>%
  summarise(`Total Roots` = mean(Total),
            `Fine Roots` = mean(Fine),
            `Coarse Roots` = mean(Coarse)) %>% ungroup()

Mean_rl <- Roots_mean %>% pivot_longer(c(3:5), names_to = "Roots", values_to = "Root_Mean")

Roots_Sd <- Roots %>% group_by(Vegetation, X10_cm_bins) %>%
  summarise(`Total Roots` = sd(Total),
            `Fine Roots` = sd(Fine),
            `Coarse Roots` = sd(Coarse)) %>% ungroup()

Sd_rl <- Roots_Sd %>% pivot_longer(c(3:5), names_to = "Roots", values_to = "Root_Sd")

Root <- left_join(Mean_rl, Sd_rl) %>%
  rename("Depth" = X10_cm_bins)

Root$Depth <- as.numeric(Root$Depth)


Tau <- read.csv("Tau_Final.csv", header  = T)
Tau <- Tau %>% rename("Depth" = "Depth..cm.", "Lithology Slope" = "Lithology.Slope") %>% 
  select(Site_Name, Depth, Lithology, Vegetation, `Lithology Slope`, Element, Tau)
Tau_Root <- left_join(Tau, Root)
Tau_Root <- Tau_Root %>% mutate(Root = factor(Roots, levels = c("Total Roots", "Fine Roots", "Coarse Roots" )))
Aspen_df <- Tau_Root %>% filter(Vegetation == "Aspen") %>% mutate(`Aspen Depth` = Depth)
Conifer_df <- Tau_Root %>% filter(Vegetation == "Conifer") %>% mutate(`Conifer Depth` = Depth)
Aspen_df$`Aspen Depth` <- as.numeric(Aspen_df$`Aspen Depth`)
Conifer_df$`Conifer Depth` <- as.numeric(Conifer_df$`Conifer Depth`)

plot3 <- ggplot(mapping = aes(x = Root_Mean, y = Tau)) +
  geom_point(data = Conifer_df, aes(color = `Conifer Depth`, shape = `Lithology Slope`), size = 5) +
  geom_errorbarh(data = Conifer_df, aes(y = Tau, xmin = Root_Mean - Root_Sd, xmax = Root_Mean + Root_Sd, color = `Conifer Depth`)) +
  scale_color_gradientn(colors = c('#0cc200', '#006600')) +
  new_scale_color() +
  geom_point(data = Aspen_df, aes(color = `Aspen Depth`, shape = `Lithology Slope`), size = 5) +  
  geom_errorbarh(data = Aspen_df, aes(y = Tau, xmin = Root_Mean - Root_Sd, xmax = Root_Mean + Root_Sd, color = `Aspen Depth`)) +
  scale_color_gradientn(colors = c('#FF9900', "#ff4800")) +
  facet_nested(Element ~ Site_Name + Root, scales = "free_y") +
  scale_shape_manual(values = Lith_slope) + 
  theme_bw() + 
  xlab("Root Fractions") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 9),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1), aspect.ratio = 1)



##Correlations###
ModelDf <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(ModelDf) <- c("Site_Name", "Root Type", "Element", "Correlation", "P-value")
Df <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(Df) <- c("Site_Name", "Root Type", "Element", "Correlation", "P-value")
rlist <- list("Total Roots", "Fine Roots", "Coarse Roots")
slist <- list("CC1", "CC2", "CC3", "CC4", "CC5")
elist <- list("tau Al", "tau Ca", "tau Fe", "tau K", "tau Mg", "tau Si")

for (s in 1:length(slist)){
  Ranked <- Tau_Root %>% subset(Site_Name %in% c(slist[[s]]))
  for (e in 1:length(elist)){
    Elements <- Ranked %>% subset(Element %in% c(elist[[e]]))
    for (r in 1:length(rlist)){
      Rooter <- Elements %>% subset(Roots %in% c(rlist[[r]])) %>%
        mutate(Root_Rank = rank(Root_Mean),
               Elements_rank = rank(Tau))
      Spearman <- cor.test(Rooter$Elements_rank, Rooter$Root_Rank, method = "spearman", exact = FALSE)
      Df[r, "Site_Name"] <- paste(slist[[s]])
      Df[r,"Root Type"]<- paste(rlist[[r]])
      Df[r, "Element"] <- paste(elist[[e]])
      Df[r, "Correlation"] <- Spearman[[4]]
      Df[r, "P-value"] <- Spearman[[3]]
    }
    ModelDf <- rbind(ModelDf, Df)
  }
}
ModelDf<-ModelDf[-1,] 
write.csv(ModelDf, "Tau_Root_Spearman.csv")

