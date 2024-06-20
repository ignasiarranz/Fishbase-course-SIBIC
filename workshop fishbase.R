
### Set the environment
rm(list=ls(all=TRUE))

#Download the fishbase package
install.packages('rfishbase')
library(rfishbase)
available_releases()

#Generic table interface: All fishbase tables can be accessed by name using the fb_tbl() function:
test <- fb_tbl("ecology") #tbl is tibble table
str(test)

#How to know the fish names
test <- common_to_sci("trout")
test <- species_names(c(1:10))
test <- species_names(238)

#=================================================#
#                                                 #
#     How to extract fish information             #
#                                                 #
#=================================================#

#Ecology
#' @Reference: http://www.fishbase.org/manual/english/fishbasethe_ecology_table.htm
test <- ecology() #all species
colnames(test)
test <- species_names(test$SpecCode)

test <- ecology("Salmo trutta")
head(test)

#Ecosystem
test <- country()
test <- test[which(test$Freshwater == 1),]

#Fooditems
#' @Reference: http://www.fishbase.org/manual/english/fishbasethe_food_items_table.htm
test <- fooditems("Salmo trutta")

#Diet Items
test <- diet_items()
colnames(test)
head(test)

# Diet (TROPHIC LEVEL)
test <- diet()
colnames(test)
head(test)
test <- diet("Salmo trutta")
print(test,n=15,width = 50) #width gives the width of characters of the tibble data 

#=================================================#
#                                                 #
#                 Exercise 1:                     #
#   Plot trophic level vs maximum body size       #
#       freshwater and marine species             #
#                                                 #
#=================================================#



#Solution

ecosy <- country()
ecosy_fw <- ecosy[which(ecosy$Freshwater == 1),]
ecosy_mar <- ecosy[which(ecosy$Saltwater == 1),]

spp_names_fw <- species_names(ecosy_fw$SpecCode)
spp_names_mar <- species_names(ecosy_mar$SpecCode)

diet_fw <- diet(spp_names_fw$Species)
diet_mar <- diet(spp_names_mar$Species)

par(mar=c(4,4,4,4),mfrow=c(1,1),pty="s")#sets the bottom, left, top and right margins. mar = margin lines
plot(diet_fw$Troph~log10(diet_fw$SizeMax),xlab="SizeMax (log)",ylab="Trophic level")
abline(lm(diet_fw$Troph~log10(diet_fw$SizeMax)),col="green",lwd=3)
abline(lm(diet_mar$Troph~log10(diet_mar$SizeMax)),col="blue",lwd=3)
title("Blue line= marine species \n Green line= freshwater species")

#=================================================#
#                                                 #
#                 Exercise 2:                     #
#         Length-weight transformations           #
#                                                 #
#=================================================#

#Length transformations
#' @Reference: http://www.fishbase.org/manual/english/fishbasethe_length_weight_table.htm
#' @Reference: http://www.fishbase.org/manual/english/PDF/FB_Book_CBinohlan_Length-Length_RF_JG.pdf

LW <- length_weight()
LL <- length_length()

data_trout <- read.table("https://raw.githubusercontent.com/ignasiarranz/Fishbase-course-SIBIC/main/df_trout.csv",sep = ";",header = T)
data_trout <- read.table("https://raw.githubusercontent.com/ignasiarranz/Fishbase-course-SIBIC/main/df_trout.csv",sep = ";",header = T)

summary(data_trout$Mass_g)
plot(data_trout$Mass_g~data_trout$Length_mm,xlab="Length (mm)",ylab="Mass (g)")


code_trout <- spp_names_fw$SpecCode[which(spp_names_fw$Species =="Salmo trutta")]
trout_LW <- LW[which(LW$SpecCode ==code_trout & LW$Type == "TL"),]
trout_LW

a <- log(mean(trout_LW$a)) #it is the natural according to fishbase
b <- mean(trout_LW$b)

select <- which(is.na(data_trout$Mass_g))
length <- data_trout$Length_mm[select]
est.weight <- log10(length)*b
est.weight <- est.weight  + a
est.weight <- 10^est.weight

plot(log10(data_trout$Mass_g)~log10(data_trout$Length_mm),xlab="Log10 length (g)",ylab="Log10 mass (g)")
points(log10(est.weight)~log10(length),col="red")

# Parameters estimated by Irz et al. 2023
#' @Reference: https://www.kmae-journal.org/articles/kmae/full_html/2022/01/kmae220057/kmae220057.html

a <- -4.947458349
b <- 2.990411434	

select <- which(is.na(data_trout$Mass_g))
length <- data_trout$Length_mm[select]
est.weight <- log10(length)*b
est.weight <- est.weight  + a
est.weight <- 10^est.weight

plot(log10(data_trout$Mass_g)~log10(data_trout$Length_mm),xlab="Log10 length (g)",ylab="Log10 mass (g)")
points(log10(est.weight)~log10(length),col="red")

#=================================================#
#                                                 #
#                 Exercise 3:                     #
#   Calculate trophic index at a global scale     #
#                                                 #
#=================================================#

#Only for freshwater fish species
data_fish <- read.table("https://raw.githubusercontent.com/ignasiarranz/Fishbase-course-SIBIC/main/pres.abs.fish.world.csv",sep = ";")
colnames(data_fish) <- gsub(".", " ", colnames(data_fish), fixed = TRUE)

head(data_fish)
names_spp <- species_names(c(1:35134)) # Get all species names with the codes
info_eco <- ecology() #all species
colnames(info_eco)
unique(info_eco$FeedingType)
info_eco$FeedingType_mod <- NA
info_eco$FeedingType_mod[which(info_eco$FeedingType == "hunting macrofauna (predator)")] <- 1
info_eco$FeedingType_mod[which(info_eco$FeedingType != "hunting macrofauna (predator)")] <- 0

df_info_eco <- data.frame()
for(i in 1:dim(data_fish)[2]){
  spp <- colnames(data_fish)[i]
  select <- which(names_spp$Species == spp)
  if(length(select) == 0){
    next
  }
  code <- names_spp$SpecCode[select]
  df_info_eco[i,1] <- colnames(data_fish)[i]
  df_info_eco[i,2] <- mean(info_eco$FeedingType_mod[which(info_eco$SpecCode == select)])
  print(i)
}

colnames(df_info_eco) <- c("spp","piscivory")
df_info_eco <- df_info_eco[-which(is.na(df_info_eco$piscivory)),]


index_pred <- data.frame()

for(i in 1:dim(data_fish)[1]){
  country <- data_fish[i,]
  country <- country[,which(country == 1)]
  country <- as.data.frame(t(country))
  select <- rownames(country)
  rownames(country) <- NULL
  country$spp <- select
  country <- merge(country,df_info_eco,by="spp")
  index_pred[i,1] <- rownames(data_fish)[i]
  index_pred[i,2] <- sum(country$piscivory)
  print(i)  
}
colnames(index_pred) <- c("name_en","predation_index")

#Map predatory fish per country

library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

world$name_en[which(world$name_en == "People's Republic of China")] <- "China"
world$name_en[which(world$name_en == "United States of America")] <- "USA"

eps <- merge(world,index_pred,by="name_en",all.x=T)

ggplot(data = eps) +
  geom_sf(aes(fill = predation_index)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_minimal() +
  labs(fill = "Predation Index", title = "Global Map Colored by Predation pressure", 
       subtitle = "Using Viridis Color Scale") +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_line(color = "white"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
)
    
### End of the script ###

