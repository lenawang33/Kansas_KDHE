library("dplyr")
library("tidyr")
library("ggplot2")

setwd("C:/Users/lwang03/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Kansas_Roar")

Data <- read.csv("Soil1_20231127_LW Modified.csv", header = T)

#Commented out because I wanted to see if there would be any significant differences across all the sites.
# #Pulling out the important defining variables, i.e. cover crop, tillage, Cash Crop
# Important_var <- Data %>%
#   select(c(2:5))

#Solving for the means and dropping NA for Cash Crop, Tillage and cover crop
#Made sure we unlogged the pH before solving for means
# Means <- Data %>% mutate(unlog = 10**(-pH), 
#                          unlog_buffer = 10**(-Buffer_pH))
# 
# Means <- Means %>% 
#   group_by(Field) %>%
#   summarise(across(everything(), mean))
# 
# Means <- Means %>% 
#   mutate(pH = -log10(unlog),
#          Buffer_pH = -log10(unlog_buffer)) %>%
#   select(-c(3:9))
# 
# #Joining the two tables together again
# Data_Mean <- left_join(Important_var, Means)
# 
# #Here we are just filtering out the things we care about
# Data_Mean <- Data_Mean  %>%
#   filter(Field %in%  c("E1", "E2", "E3", "SS1"))
# 
# #Performing ANOVA test to determine Cash Crop, Tillage, Cover Crop Influence over variable
# 
# #Building two lists two iterate through, if there showed evidence of stronger differences across all sites. 
# Tests <- list("Cash_Crop", "Tillage", "Cover.Crop")
# Variables <- list(colnames(Data_Mean[6:82]))
# 
# 
# dfhold <- data.frame(matrix(nrow = 1, ncol = 6))
# 
# model <- aov(pH ~ Cash_Crop, data = Data_Mean)
# test <- summary(model)


###################Here I am doing a pairwise t-test#################

#Pairwise E1 and E3, Cover Crop vs No Cover Crop, removing columns that are totally NA, additionally remove Excess Lime Column
E1_E3 <- Data %>%
  filter(Field %in%  c("E1", "E3")) %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-c(13)) %>%
  mutate(pH = 10**(-pH),
         Buffer_pH = 10**(-Buffer_pH))

#Converting everything in numbers, therefore removing words like "All Prey, All Mono, Sand Present". This could be useful data 
#And I was need to figure out another way to analyze it. 
E1_E3[,10:length(E1_E3)] <- as.numeric(unlist(E1_E3[,10:length(E1_E3)]))

#Creating a vector to iterate over
Variables <- c(colnames(E1_E3[10:length(E1_E3)]))

#Creating an empty dataframe to write into
Cover_Crop <- data.frame(matrix(nrow = 0, ncol = 77))
colnames(Cover_Crop) <- colnames(E1_E3[10:length(E1_E3)])

#The iteration runs the t.test on everything and then pulls out the p-values, mean Cover Crop, and mean no Cover Crop
for (i in 1:length(Variables)){
  T_test <- t.test(E1_E3[,paste(Variables[[i]])] ~ Cover.Crop, var.equal= T, data = E1_E3)
  Tlist <- list("p-value", "mean Cover Crop", "Mean in no Cover Crop")
  for (j in 1:length(Tlist)){
  Cover_Crop["p.value",i] <- T_test[["p.value"]]
  Cover_Crop["mean Cover Crop", i] <- T_test[["estimate"]][["mean in group Cover Crop"]]
  Cover_Crop["mean no Cover Crop",i] <- T_test[["estimate"]][["mean in group No Cover Crop"]]
  }
}

#Converting the means of pH back into log10##
Cover_Crop[2,1] <- log10(Cover_Crop[2,1]) *(-1)
Cover_Crop[3,1] <- log10(Cover_Crop[3,1]) *(-1)

Cover_Crop[2,2] <- log10(Cover_Crop[2,2]) *(-1)
Cover_Crop[3,2] <- log10(Cover_Crop[3,2]) *(-1)






#Pairwise E2 and E3, Cover Crop vs No Cover Crop, removing columns that are totally NA, additionally remove Excess Lime Column
E2_E3 <- Data %>%
  filter(Field %in%  c("E2", "E3")) %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-c(13)) %>%
  mutate(pH = 10**(-pH),
         Buffer_pH = 10**(-Buffer_pH))

#Converting everything in numbers, therefore removing words like "All Prey, All Mono, Sand Present". This could be useful data 
#And I was need to figure out another way to analyze it. 
E2_E3[,10:length(E2_E3)] <- as.numeric(unlist(E2_E3[,10:length(E2_E3)]))

#Creating a vector to iterate over
Variables <- c(colnames(E2_E3[10:length(E2_E3)]))

#Creating an empty dataframe to write into
Cash_Crop <- data.frame(matrix(nrow = 0, ncol = 77))
colnames(Cash_Crop) <- colnames(E2_E3[10:length(E2_E3)])

#The iteration for determining the p-value of everything
for (i in 1:length(Variables)){
  T_test <- t.test(E2_E3[,paste(Variables[[i]])] ~ Cash_Crop, var.equal= T, data = E2_E3)
  Tlist <- list("p-value", "", "Mean in Corn", "Mean in Soybean")
  for (j in 1:length(Tlist)){
    Cash_Crop["p.value",i] <- T_test[["p.value"]]
    Cash_Crop["Mean in Corn", i] <- T_test[["estimate"]][["mean in group Corn"]]
    Cash_Crop["Mean in Soybean",i] <- T_test[["estimate"]][["mean in group Soybeans"]]
  }
}

Cash_Crop[2,1] <- log10(Cash_Crop[2,1]) *(-1)
Cash_Crop[3,1] <- log10(Cash_Crop[3,1]) *(-1)

Cash_Crop[2,2] <- log10(Cash_Crop[2,2]) *(-1)
Cash_Crop[3,2] <- log10(Cash_Crop[3,2]) *(-1)

#Pairwise SS1 and E3, Cover Crop vs No Cover Crop, removing columns that are totally NA, additionally remove Excess Lime Column
SS1_E3 <- Data %>%
  filter(Field %in%  c("SS1", "E3")) %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(-c(13)) %>%
  mutate(pH = 10**(-pH),
         Buffer_pH = 10**(-Buffer_pH))

#Converting everything in numbers, therefore removing words like "All Prey, All Mono, Sand Present". This could be useful data 
#And I was need to figure out another way to analyze it. 
SS1_E3[,10:length(SS1_E3)] <- as.numeric(unlist(SS1_E3[,10:length(SS1_E3)]))

#Creating a vector to iterate over
Variables <- c(colnames(SS1_E3[10:length(SS1_E3)]))

#Creating an empty dataframe to write into
Tillage <- data.frame(matrix(nrow = 0, ncol = 77))
colnames(Tillage) <- colnames(SS1_E3[10:length(SS1_E3)])

#The iteration for determining the p-value of everything
for (i in 1:length(Variables)){
  T_test <- t.test(SS1_E3[,paste(Variables[[i]])] ~ Tillage, var.equal= T, data = SS1_E3)
  Tlist <- list("p-value", "", "Mean in Conventional Till", "Mean in No Tillage")
  for (j in 1:length(Tlist)){
    Tillage["p.value",i] <- T_test[["p.value"]]
    Tillage["Mean in Conventional", i] <- T_test[["estimate"]][["mean in group Conventional Till"]]
    Tillage["Mean in No Tillage",i] <- T_test[["estimate"]][["mean in group No Till"]]
  }
}
Tillage[2,1] <- log10(Tillage[2,1]) *(-1)
Tillage[3,1] <- log10(Tillage[3,1]) *(-1)

Tillage[2,2] <- log10(Tillage[2,2]) *(-1)
Tillage[3,2] <- log10(Tillage[3,2]) *(-1)


Pvalue <- rbind(Cover_Crop, Tillage, Cash_Crop)
write.csv(Pvalue, "Means_Pvalue.csv")




