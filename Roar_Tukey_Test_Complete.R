library("dplyr")
library("tidyr")
library("ggplot2")
library("ggpattern")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20240520_LW Modified.csv", header = T)
Field_col <- c(E1 = "#543fbf", E2 = "#117733", E3 = "#44AA99", SS1 = "#BB9D05" , S2 = "#CC6677", S3 = "#88CCEE", S4 = "#AA4499" , W1 ="#882255" )
Season_col <- c(Spring = "#56B4E9", Fall = "#E69900")
#Selecting the variables that are connected to the Haney Test and Nitrogen, changing the names
Haney_Test <- Data %>% select(Crop.2023,
                              Tillage,
                              Cover.Crop,
                              Crop_Diversity,
                              Field,
                              Sample_ID,
                              Season,
                              OM.,
                              #Ace.Protein.g.Kg,
                              NO3_N_H2O,
                              OC_H2O_ppm,
                              ON_H2O_ppm,
                              CN_H2O,
                              NH4_N_H2O,
                              CO2,
                              N_H2O,
                              X.MAC,
                              #Aggstab1_2mm,
                              X..Clay,
                              X..Silt,
                              X..Sand,
                              CEC,
                              ON_release_ppm,
                              Ca_ppm,
                              Mg_ppm,
                              K_ppm,
                              Na_ppm,
                              SoilHealth,
                              Biomass,
                              Total_Fungi_Biomass,
                              UndifferentiatedBiomass,
                              TotalBacteriabiomass,
                              Fungi_Bacteria,
                              Protozoa_biomasss
) %>% 
  rename(`Organic Matter (%)` = "OM.",
         `Water Extractable Nitrate Nitrogen (mg/kg)` = "NO3_N_H2O",
         `Water Extractable Organic Carbon (mg/kg)` = "OC_H2O_ppm",
         `Water Extractable Organic Nitrogen (mg/kg)` = "ON_H2O_ppm",
         `WEOC:WEON` = "CN_H2O",
         `Calcium (mg/kg)` = "Ca_ppm",
         `Potassium (mg/kg)` = "K_ppm",
         `Magnesium (mg/kg)` = "Mg_ppm",
         `Sodium (mg/kg)` = "Na_ppm",
         `Water Extractable Ammonium Nitrogen (mg/kg)` = "NH4_N_H2O",
         `24 hour CO2 Respiration (mg/kg)` = "CO2",
         `Microbially Active Carbon (%)` = "X.MAC",
         #`Autoclavable Citrate Protein (g/kg)` = "Ace.Protein.g.Kg",
         `Organic Nitrogen Release (mg/kg)` = "ON_release_ppm",
         `Water Extractable Nitrogen (mg/kg)` = "N_H2O",
         `Clay (%)` = "X..Clay",
         `Silt (%)` ="X..Silt",
         `Sand (%)` ="X..Sand",
         #`Aggregate Stability 1-2 mm (%)` = "Aggstab1_2mm",
         `Microbial Biomass (ug/g)` = "Biomass",
         `Fungal Biomass (ug/g)` = "Total_Fungi_Biomass",
         `Undifferentiated Biomass (ug/g)` = "UndifferentiatedBiomass",
         `Bacteria Biomass (ug/g)` = "TotalBacteriabiomass",
         `Fungi:Bacteria (ug/g)` = "Fungi_Bacteria",
         `Protazoa Biomass (ug/g)` = "Protozoa_biomasss"
  ) %>%
  mutate( `Carbon (%)` = `Organic Matter (%)`/1.72) #%>%
  #filter(`WEOC:WEON` < 80)
Haney_Test[,8:length(Haney_Test)] <- as.numeric(unlist(Haney_Test[,8:length(Haney_Test)]))
Long_H <-  pivot_longer(Haney_Test, cols = c(8:length(Haney_Test)), names_to = "Variable", values_to = "Value")
Test <- Haney_Test %>% subset(Season %in% "Spring") %>%
  rename(Season2 = Season)
Test2 <- Haney_Test %>% subset(Season %in% "Fall")

Joined_test <- left_join(Test2, Test)
# Cover_Crop_dist <- ggplot(Long_H, aes(x = Value)) +  
#   geom_histogram() +
#   facet_wrap(Variable ~ Cover.Crop, scales = "free") + 
#   theme(aspect.ratio = 1)
# 
# 
# setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar/Plots")
# ggsave("Cover_Crop_distribution.pdf", Cover_Crop_dist, height = 70, width = 50, limitsize = F)

Spring_HT <- Long_H %>% subset(Season %in% "Spring")
Fall_HT <- Long_H %>% subset(Season %in% "Fall")

#T-tests iteration over Spring and Fall Cover Crop 
Spring_CC <- Spring_HT |>
  #Grouping by Variables, such as %OM, Respiration
  dplyr::group_nest(Variable) |>
  dplyr::mutate(
    #performs a t.test function on each grouped Variable
    t_test = purrr::map(.x = data, .f = \(x){
      t.test(x$Value ~ x$Cover.Crop, var.equal = F) |>
        broom::tidy()
    }) 
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(cols = t_test)

Fall_CC <- Fall_HT |>
  dplyr::group_nest(Variable) |>
  dplyr::mutate(
    t_test = purrr::map(.x = data, .f = \(x){
      t.test(x$Value ~ x$Cover.Crop, var.equal = F) |>
        broom::tidy()
    }) 
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(cols = t_test)

Spring_T <- Spring_HT |>
  #Grouping by Variables, such as %OM, Respiration
  dplyr::group_nest(Variable) |>
  dplyr::mutate(
    #performs a t.test function on each grouped Variable
    t_test = purrr::map(.x = data, .f = \(x){
      t.test(x$Value ~ x$Tillage, var.equal = F) |>
        broom::tidy()
    }) 
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(cols = t_test)

Fall_T <- Fall_HT |>
  dplyr::group_nest(Variable) |>
  dplyr::mutate(
    t_test = purrr::map(.x = data, .f = \(x){
      t.test(x$Value ~ x$Tillage, var.equal = F) |>
        broom::tidy()
    }) 
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(cols = t_test)


Season_test <- Long_H |>
  dplyr::group_nest(Variable) |>
  dplyr::mutate(
    t_test = purrr::map(.x = data, .f = \(x){
      t.test(x$Value ~ x$Season, var.equal = F, paired = T) |>
        broom::tidy()
    }) 
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(cols = t_test)


write.csv(Season_test,"Season_paired_T_test.csv", )
write.csv(Fall_T, "Fall_Tillage_WelchT_test.csv")
write.csv(Spring_T, "Spring_Tillage_WelchT_test.csv")
write.csv(Fall_CC, "Fall_Cover_Crop_WelchT_test.csv")
write.csv(Spring_CC, "Spring_Cover_Crop_WelchT_test.csv")





##### For looping####
names <- colnames(Haney_Test[,8:length(Haney_Test)])
Spring_HT <- Haney_Test %>% subset(Season %in% "Spring")
Fall_HT <- Haney_Test %>% subset(Season %in% "Fall")
#Creating an empty dataframe to write into
Cover_Crop <- data.frame(matrix(nrow = 0, ncol = 46))
colnames(Cover_Crop) <- names

#The iteration for determining the p-value of everything
for (i in 1:length(names)){
  Fall_test <- t.test(Fall_HT[,paste(names[[i]])] ~ Cover.Crop, var.equal = F, data = Fall_HT)
  Spring_test <- t.test(Spring_HT[,paste(names[[i]])] ~ Cover.Crop, var.equal = F, data = Spring_HT)
  Tlist <- list("p.value", "Mean in Cover Crop", "Mean in No Cover Crop", "Season")
  Slist <- list("Fall", "Spring")
    for (j in 1:length(Tlist)){
      Cover_Crop["p.value",i] <- Fall_test[["p.value"]]
      Cover_Crop["Mean in Cover Crop", i] <- Fall_test[["estimate"]][["mean in group Cover Crop"]]
      Cover_Crop["Mean in No Cover Crop",i] <- Fall_test[["estimate"]][["mean in group No Cover Crop"]]
  }
}

Crop_Diversity <- data.frame(matrix(nrow = 0, ncol = 46))
colnames(Crop_Diversity) <- names

#The iteration for determining the p-value of everything
for (i in 1:length(names)){
  T_test <- t.test(Haney_Test[,paste(names[[i]])] ~ Crop_Diversity, var.equal = F, data = Haney_Test)
  Tlist <- list("p.value", "Mean in High Crop Diversity", "Mean in Low Crop Diversity")
  for (j in 1:length(Tlist)){
    Crop_Diversity["p.value",i] <- T_test[["p.value"]]
    Crop_Diversity["Mean in High Crop Diversity", i] <- T_test[["estimate"]][["mean in group High Crop Diversity"]]
    Crop_Diversity["Mean in Low Crop Diversity",i] <- T_test[["estimate"]][["mean in group Low Crop Diversity"]]
  }
}


Tillage <- data.frame(matrix(nrow = 0, ncol = 46))
colnames(Tillage) <- names

#The iteration for determining the p-value of everything
for (i in 1:length(names)){
  T_test <- t.test(Haney_Test[,paste(names[[i]])] ~ Tillage, var.equal = F, data = Haney_Test)
  Tlist <- list("p.value", "Mean in Conventional Till", "Mean in No-Till")
  for (j in 1:length(Tlist)){
    Tillage["p.value",i] <- T_test[["p.value"]]
    Tillage["Mean in Conventional Till", i] <- T_test[["estimate"]][["mean in group Conventional Till"]]
    Tillage["Mean in No Till",i] <- T_test[["estimate"]][["mean in group No-Till"]]
  }
}


# Seasons <- data.frame(matrix(nrow = 0, ncol = 46))
# colnames(Seasons) <- names
# 
# #The iteration for determining the p-value of everything
# for (i in 1:length(names)){
#   T_test <- t.test(Haney_Test[,paste(names[[i]])] ~ Season, var.equal = F, data = Haney_Test)
#   Tlist <- list("p.value", "Mean in Fall", "Mean in Spring")
#   for (j in 1:length(Tlist)){
#     Seasons["p.value",i] <- T_test[["p.value"]]
#     Seasons["Mean in Fall", i] <- T_test[["estimate"]][["mean in group Fall"]]
#     Seasons["Mean in Spring",i] <- T_test[["estimate"]][["mean in group Spring"]]
#   }
# }



