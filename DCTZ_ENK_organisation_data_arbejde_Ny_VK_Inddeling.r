rm(list = ls())
rm(list = ls())

library(plyr)
library(stringr);
library(data.table)


# ENK data arbejde til forbereding af input data for ENK

# Importer ENK-forhold - HUSK at skrive til rigtig fil i bunden. 
#I cellen lige over skal udkommenteres alt efter om det er 2013-2017 data eller 2018 data der skal behandles

#Directories
input_raw <- "z:/Model_input_Fase_2/raw/"
output_raw <- "z:/Model_input_Fase_2/"

# 2013-2017 data
setwd(input_raw)
ENK_full <- read.csv("ENK_data.csv", sep = ";")
ENK_full$Anvendt <- as.character(ENK_full$Anvendt)
ENK_full$Anvendt <- gsub("-", "/", ENK_full$Anvendt)

# 2018 data
#ENK_full <- read.csv("C:/Punktlighed/std/ENK/ENK_2018.csv")

names(ENK_full)[4] <- "NAVN"

# ændrer TK til factor
str(ENK_full$TK)
ENK_full$TK <- as.factor(ENK_full$TK)
str(ENK_full)

# Importer strækningstabel
STRAEKNING <- read.csv("Straekningstabel.csv", sep = ";")


head(STRAEKNING)

#Benytter relevante kolonner
STRAEKNING_KOBLE <- STRAEKNING[,c("NAVN","Visualiseringskode")]

# Fjerner duplikater
STRAEKNING_unique <- unique(STRAEKNING_KOBLE)
head(STRAEKNING_unique)

nrow(STRAEKNING_KOBLE)
nrow(STRAEKNING_unique)

# Koble ENK data til visualiseringskode
ENK_visu <- merge(x=ENK_full, y=STRAEKNING_unique,by="NAVN", all.x=TRUE, all.y=FALSE)
head(ENK_visu)
str(ENK_visu)

# Behold spærringerne hørende til vedligehold, fornyelse og anlæg
#ENK_visu$Produkt[ENK_visu$Organization == "Produktion",] <- "Vedligehold"
#ENK_visu$Produkt[ENK_visu$Organization == "Teknisk Drift",] <- "Vedligehold"

index <- c("Anlæg og Fornyelse","Elektrificerings Programmet","Produktion","Ringsted-Femern Programmet","Signal Programmet","Teknisk Drift")
values <- c("Fornyelse","Anlæg","Vedligehold","Anlæg","Anlæg","Vedligehold")
ENK_visu$Produkt <- values[match(ENK_visu$Organization, index)]

# funktion til at smide ikke relevante rækker væk
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# ud med rækker, der ikke har en tekst i Produkt søjlen

#ENK_Produkt <- completeFun(ENK_visu,"Produkt") # udkommenter denne, hvis alle rækker skal med, nedenstående kode skal så ind
ENK_Produkt <- ENK_visu

# faktorer produkt koden inkl NA som factor
ENK_Produkt$Produkt <- factor(ENK_Produkt$Produkt, exclude = NULL)
str(ENK_Produkt)


ENK_Produkt %>% count("Produkt")

# antal ENK pr dag, produkt og visualiseringskode
# resterende variable har (0,1), (TRUE,FALSE) værdier hvor 1/TRUE angiver positiv tilkendegivelse i forhold
# til overskriften og 0/False angiver negativ i forhold til overskriften.
ENK_summary <- ENK_Produkt %>% count(c("Anvendt","Visualiseringskode","Produkt","Fri_bane","Akut","Interval","Dag_spaerring","Ej_benyttet"))
names(ENK_summary)[1] <- "Forste_koredato"
names(ENK_summary)[9] <- "antal_ENK"
ENK_summary$Forste_koredato <- as.factor(as.Date(ENK_summary$Forste_koredato, '%m/%d/%Y'))

head(ENK_summary)
nrow(ENK_summary)

#tjekker alle spærringer er med
sum(ENK_summary$antal_ENK)

# ændrer boolean til 0/1 værdier
cols <- sapply(ENK_summary, is.logical)
ENK_summary[,cols] <- lapply(ENK_summary[,cols], as.numeric)
head(ENK_summary)
str(ENK_summary)

ENK_summary$Fri_bane <- factor(ENK_summary$Fri_bane)
ENK_summary$Akut <- factor(ENK_summary$Akut)
ENK_summary$Interval <- factor(ENK_summary$Interval)
ENK_summary$Dag_spaerring <- factor(ENK_summary$Dag_spaerring)
ENK_summary$Ej_benyttet <- factor(ENK_summary$Ej_benyttet)
str(ENK_summary)

# Ændrer NA level i Produktion til "Andet"
levels <- levels(ENK_summary$Produkt)
levels[length(levels) + 1] <- "Andet" # tilføjer "Andet" til levels
levels

ENK_summary$Produkt <- factor(ENK_summary$Produkt, levels = levels) #tilføjer de nye levels til data
ENK_summary$Produkt[is.na(ENK_summary$Produkt)] <- "Andet" #substituerer level 

summary(ENK_summary)
head(ENK_summary)

# tilføjer årstal
ENK_summary$Year <- year(ENK_summary$Forste_koredato)

# data frame med data for årene 2013-2017
ENK_2013_2017 <- ENK_summary[ENK_summary$Year <= 2017 & ENK_summary$Year >= 2013,]
ENK_2013_2017$Year <- as.factor(ENK_2013_2017$Year)
str(ENK_2013_2017)

ENK_final <- ENK_2013_2017[,1:9]

# data frame med data for årene 2018
ENK_2018 <- ENK_summary[ENK_summary$Year == 2018,]
ENK_2018$Year <- as.factor(ENK_2018$Year)
str(ENK_2018)

ENK_2018 <- ENK_2018[,1:9]

#Fjern NA fra VK
ENK_final <- ENK_final[is.na(ENK_final$Visualiseringskode)==F,]
ENK_2018  <- ENK_2018[is.na(ENK_2018$Visualiseringskode)==F,]
# data er klar til modellen og gemmes

#Change the variable names
colnames(ENK_final)
colnames(ENK_final)[which(colnames(ENK_final) == "Visualiseringskode")]    <- "visualiseringskode"
colnames(ENK_final)[which(colnames(ENK_final) == "Forste_koredato")]    <- "dato"
colnames(ENK_2018)
colnames(ENK_2018)[which(colnames(ENK_2018) == "Visualiseringskode")]    <- "visualiseringskode"
colnames(ENK_2018)[which(colnames(ENK_2018) == "Forste_koredato")]    <- "dato"



#2013-2017 data
setwd(output_raw)
write.csv(ENK_final,file="aggregeret_ENK_TP_produkt_ny_inddeling_DCTZ.csv")
write.csv2(ENK_final,file="aggregeret_ENK_TP_produkt_ny_inddeling_DCTZ_ViewOnly.csv")

#2018 data
setwd(output_raw)
write.csv(ENK_2018,file="aggregeret_ENK_TP_produkt_2018_ny_inddeling_DCTZ.csv")
write.csv2(ENK_2018,file="aggregeret_ENK_TP_produkt_2018_ny_inddeling_DCTZ_viewOnly.csv")

ENK_final %>% count("Visualiseringskode")
