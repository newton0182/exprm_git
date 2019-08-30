## Generering af LA data input til ML 
## Redigeret: DCTZ
## Skabt: Anna fra Trafik
## November 2018
## Det er Annas script med de følgende ændringer:
## ny VK inddeling (VK 18, 25, 26)
## sti-ændringer





rm(list = ls())
rm(list = ls())

library(ggplot2)
library(plyr)

input_raw <- "z:/Model_input_Fase_2/raw/"
output_raw <- "z:/Model_input_Fase_2/"

# Importer la-forhold - HUSK at skriv til korrekt fil i bunden af dokumentet

# 2013-2017 datasæt
setwd(input_raw)
LA_forhold_full <- read.csv("Tilrettet_LA_2013_2018.csv")
#LA_forhold_full <- read.csv("Tilrettet_LA_2013_2018_NEW2.csv", sep = ";", dec = ",")


# 2018 datasæt
#LA_forhold_full <- read.csv("C:/Punktlighed/std/LA-forhold/LA_2018.csv")


# "folder" data ud så der findes en række for hver dato og hvert la-forhold

# Tjekker liggetid >= 0 dage
LA_forhold_0 <- LA_forhold_full[LA_forhold_full$AntagetLiggeTid>=0,]
#LA_forhold_0 <- LA_forhold_0[is.na(LA_forhold_0$AntagetLiggeTid) == F,]

# Kopierer datarækker i forhold til antal liggedage
LA.expanded <- LA_forhold_0[rep(row.names(LA_forhold_0), (LA_forhold_0$AntagetLiggeTid+1)), ]

# Kolonne indeholdende liggedage
data_raekke <- LA_forhold_0[,which(colnames(LA_forhold_0)=="AntagetLiggeTid")]

# array med tællende dage - folder liggetid 3 -> 0,1,2,3
a<-c()
for(x in c(1:length(data_raekke))){a<-c(a,0:(data_raekke[x]))}

# merges til LA.expanded 
LA.expanded$plus_dage <- a

# nye datoer beregnes - data for hver dag
LA.expanded$Dato <- as.Date(LA.expanded$LAStart,format="%m/%d/%Y") + LA.expanded$plus_dage

# Kigger på LA på fri bane
LA_forhold <- LA.expanded[which(grepl("-", LA.expanded$Straekning)),]

# beregner nedsat km/t 
LA_forhold <- transform(LA_forhold, HoejreNedsatFart = HoejreTIBKM - HoejreLAKM)
LA_forhold <- transform(LA_forhold, VenstreNedsatFart = VenstreTIBKM - VenstreLAKM)

# tæller la på hhv højre og venstre spor samt samlet
LA_forhold <- transform(LA_forhold, LAPaaH = !is.na(HoejreLgd))
LA_forhold <- transform(LA_forhold, LAPaaV = !is.na(VenstreLgd))
LA_forhold <- transform(LA_forhold, TotalLA = LAPaaH+LAPaaV)


# Importer strækningstabel
setwd(input_raw)
STRAEKNING <- read.csv("Straekningstabel.csv", sep=";")
names(STRAEKNING)[1]<-"Bane"

#
STRAEKNING_KOBLE <- STRAEKNING[,c("Bane","Visualiseringskode")]

# Fjerner duplikater
STRAEKNING_unique <- unique(STRAEKNING_KOBLE)
STRAEKNING_uden_banenr_21 <- STRAEKNING_unique[STRAEKNING_unique$Bane != 21,]

#Strækningstabel kun for banenr 21 som splittes i 2 VK
STRAEKNING_kunbanenr21 <- STRAEKNING[STRAEKNING$Bane==21, c("NAVN", "Visualiseringskode")]
STRAEKNING_kunbanenr21 <- unique(STRAEKNING_kunbanenr21)

# Koble LA data til visualiseringskode for alle banenr
LA_visu <- merge(x=STRAEKNING_uden_banenr_21,y=LA_forhold,by="Bane", all.x =TRUE, all.y = TRUE)
#str(LA_visu)
#count(LA_visu$Visualiseringskode)

#Alle banenr 21 skal indentificeres per Navn
Navne_for_bane21 <- as.data.frame(unique(LA_visu$Straekning[LA_visu$Bane==21]))
colnames(Navne_for_bane21)[1] <- "Straekning"
Navne_for_bane21$correct <- NA
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Lundby-Vordingborg" ] <- "Lundby - Vordingborg"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "NÃ¦stved-Lundby" ] <- "Næstved - Lundby"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Vordingborg-Lundby" ] <- "Lundby - Vordingborg"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Vordingborg-MasnedÃ¸" ] <- "Vordingborg - Masnedø"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Lundby-NÃ¦stved" ] <- "Næstved - Lundby"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Lundby - NÃ¦stved" ] <- "Næstved - Lundby"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Tingsted-NykÃ¸bing F" ] <- "Tingsted - Nykøbing Falster"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "GlumsÃ¸-NÃ¦stved" ] <- "Glumsø - Næstved"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "NÃ¦stved-GlumsÃ¸" ] <- "Glumsø - Næstved"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "Ringsted-GlumsÃ¸" ] <- "Ringsted - Glumsø"
Navne_for_bane21$correct[Navne_for_bane21$Straekning== "NÃ¸rre Alslev-Eskilstrup" ] <- "Nørre Alslev - Eskilstrup"
colnames(Navne_for_bane21)[2] <- "NAVN"

VK_for_bane21 <- merge(x=Navne_for_bane21,y=STRAEKNING_kunbanenr21,by="NAVN", all.x =TRUE)
VK_for_bane21 <- VK_for_bane21[is.na(VK_for_bane21$NAVN)==F,]
VK_for_bane21$NAVN <- NULL

for (j in 1:nrow(VK_for_bane21)){
  LA_visu$Visualiseringskode[LA_visu$Straekning == VK_for_bane21$Straekning[j]] <- VK_for_bane21$Visualiseringskode[j]
}

#Alt som har som stræknng "NykÃ¸bing F Vest-RÃ¸dby Ã˜st" er banenr 22 og Vk26
LA_visu$Bane[LA_visu$Straekning == "NykÃ¸bing F Vest-RÃ¸dby Ã˜st"] <- 22
LA_visu$Visualiseringskode[LA_visu$Straekning == "NykÃ¸bing F Vest-RÃ¸dby Ã˜st"] <- 26


# antal LA pr dag og visualiseringskode
LA_summary <- aggregate(LA_visu$TotalLA, by=list(Visualiseringskode=LA_visu$Visualiseringskode, Forste_koredato=LA_visu$Dato), FUN=sum)

names(LA_summary)[3] <- "antal_LA"

# total længde af LA per dag og visu

# beregner lgd for hhv højre og venstre
LA_visu$HoejreLgd[is.na(LA_visu$HoejreLgd)]<-0 # erstatter manglende værdier med 0
LA_visu$VenstreLgd[is.na(LA_visu$VenstreLgd)]<-0 # erstatter manglende værdier med 0

LA_lgd_h <- aggregate(LA_visu$HoejreLgd, by=list(Visualiseringskode=LA_visu$Visualiseringskode, Forste_koredato=LA_visu$Dato), FUN=sum)
LA_lgd_v <- aggregate(LA_visu$VenstreLgd, by=list(Visualiseringskode=LA_visu$Visualiseringskode, Forste_koredato=LA_visu$Dato), FUN=sum)

# merger til en tabel og erstatter NA med 0
LA_lgd <- merge(x=LA_lgd_h, y=LA_lgd_v ,by=c("Forste_koredato","Visualiseringskode"),all="TRUE")
LA_lgd[is.na(LA_lgd)]<-0 # erstatter manglende værdier med 0

# beregner total lgd i km
LA_lgd$Total_lgd <- rowSums(LA_lgd[,3:4])

names(LA_lgd)[3]<-"lgh_h"
names(LA_lgd)[4]<-"lgh_v"


# tilføjer lgd til LA_summary
LA_summary_lgd <- merge(x=LA_summary,y=LA_lgd, by=c("Forste_koredato","Visualiseringskode"),all=TRUE)
head(LA_summary_lgd)

# gennemsnitlig hastighedsnedsættelse på LA per dag og visu
LA_speed_h <- aggregate(LA_visu$HoejreNedsatFart, by=list(Visualiseringskode=LA_visu$Visualiseringskode, Forste_koredato=LA_visu$Dato), FUN=sum)
LA_speed_v <- aggregate(LA_visu$VenstreNedsatFart, by=list(Visualiseringskode=LA_visu$Visualiseringskode, Forste_koredato=LA_visu$Dato), FUN=sum)

# merger til en tabel og erstatter NA med 0
LA__speed_hv <- merge(x=LA_speed_h, y=LA_speed_v ,by=c("Forste_koredato","Visualiseringskode"),all="TRUE")
LA__speed_hv[is.na(LA__speed_hv)] <- 0 # erstatter manglende værdier med 0

# beregner total hastighedsnedsættelse i km
LA__speed_hv$Total_speed_tabt <- rowSums(LA__speed_hv[,3:4])

names(LA__speed_hv)[3]<-"speed_tabt_h"
names(LA__speed_hv)[4]<-"speed_tabt_v"

#tilføjer speed til LA_summary_lgd
LA_summary_speed <- merge(x=LA_summary_lgd,y=LA__speed_hv, by=c("Forste_koredato","Visualiseringskode"),all=TRUE)

# beregn gennemsnit af hastighedsnedsættelse
LA_summary_speed <- transform(LA_summary_speed, avg_speed_tabt = Total_speed_tabt / antal_LA)
LA_summary_speed <- transform(LA_summary_speed, avg_lgd = Total_lgd / antal_LA)

# normaliser LA-forhold længde i forhold til strækningslængde
# Importer strækningslængde og databehandling
setwd(input_raw)
Straekning_data <- read.csv("straekning_km_spor_ny_VK_fra_Anna.csv", sep = ";", dec = ",")
names(Straekning_data)[1] <- "Visualiseringskode"
names(Straekning_data)[4] <- "KM"
Straekning_data$Visualiseringskode <- as.factor(Straekning_data$Visualiseringskode)

# beregner km længde af spor, tager højde for kørsel i begge retninger på enkeltspor 
Straekning_data <- transform(Straekning_data, spor_km = 2*KM)
data_km <- Straekning_data[,c(1,5)]

# kobler med la-data
LA_norm <- merge(x=LA_summary_speed, y=data_km ,by="Visualiseringskode",all.x="TRUE")

#normaliser LA km i forhold til spor km
LA_norm <- transform(LA_norm, norm_total_km = Total_lgd / spor_km)
LA_norm <- transform(LA_norm, norm_avg_km = avg_lgd / spor_km)

#Change the name of a column
colnames(LA_norm)
colnames(LA_norm)[which(colnames(LA_norm) == "antal_LA")]    <- "tam_antal_la_forhold"
colnames(LA_norm)[which(colnames(LA_norm) == "Visualiseringskode")]    <- "visualiseringskode"
colnames(LA_norm)[which(colnames(LA_norm) == "avg_speed_tabt")]    <- "tibla_gms_hastighedsnedsaettelse_pga_la_forhold"
colnames(LA_norm)[which(colnames(LA_norm) == "norm_total_km")]    <- "tibla_andel_af_straekning_med_la_forhold"
colnames(LA_norm)[which(colnames(LA_norm) == "Forste_koredato")]    <- "dato"

#Keep only the variables we need
LA_norm <- LA_norm[,c("visualiseringskode", "dato", "tam_antal_la_forhold", "tibla_gms_hastighedsnedsaettelse_pga_la_forhold", "tibla_andel_af_straekning_med_la_forhold")]

# skriver data til fil - 
setwd(output_raw)
write.csv (LA_norm,file="aggregeret_LA_2013-apr2019_ny_Inddeling_DCTZ.csv")
write.csv2(LA_norm,file="aggregeret_LA_2013-apr2019_ny_Inddeling_DCTZ_viewOnly.csv")



test