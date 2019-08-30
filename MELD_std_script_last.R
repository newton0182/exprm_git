# Hent diverse R-pakker og kalibrering af tids- og datoformatter (dansk format)
cat("\014");
rm(list=ls());
Sys.setenv("LC_ALL"="Danish_Denmark.1252")
library(readr);
library(readxl);
#library(plyr);
library(dplyr);
library(stringr);

# Hent MELD raw-data
MELD <- read.csv("C:/Punktlighed/raw/rdsdata/MELD/MELD_2018_jan_jun.csv",dec=",", na="");
head(MELD,10);

# Omdøb den første kolonnenavn (for at fjerne det stupide kolonnenavn "X.U.FEFF." i den første kolonne)
names(MELD)[1] <- gsub("X.U.FEFF.","",names(MELD)[1]);
names(MELD);

# Tjek MELDs datastruktur FØR reformat
str(MELD);

# Reformat MELDs datastruktur
MELD$TOGNUMMER <- as.character(str_trim(MELD$TOGNUMMER));
MELD$FORSTE_KOREDATO <- as.POSIXct(str_trim(MELD$FORSTE_KOREDATO),format='%d-%m-%Y');
#MELD$FORSTE_KOREDATO <- as.Date(str_trim(MELD$FORSTE_KOREDATO),format='%d-%m-%Y', tz = "GMT");
MELD$STAT_KODE <- as.character(str_trim(MELD$STAT_KODE));
MELD$PROD_KODE <- as.character(str_trim(MELD$PROD_KODE));
MELD$MLD_TYP_KODE <- as.character(str_trim(MELD$MLD_TYP_KODE));
MELD$PLANDATO <- as.POSIXct(str_trim(MELD$PLANDATO),format='%d-%m-%Y');
#MELD$PLANDATO <- as.Date(str_trim(MELD$PLANDATO),format='%d-%m-%Y');

# --------------------------------
#MELD$PLANTID <- as.POSIXct(str_trim(MELD$PLANTID), format='%d-%m-%Y %H:%M:%S');
MELD$PLANTID <- as.character(str_trim(MELD$PLANTID));
rows_missing_time <- regexpr(":",MELD$PLANTID)==-1;
MELD$PLANTID[rows_missing_time] <- paste0(MELD$PLANTID[rows_missing_time]," 00:00:00");
MELD$PLANTID <- as.POSIXct(MELD$PLANTID, format='%d-%m-%Y %H:%M:%S');
# --------------------------------
MELD$DELSTR_NR <- as.character(str_trim(MELD$DELSTR_NR));
MELD$DELSTR_RETNING <- as.character(str_trim(MELD$DELSTR_RETNING));
MELD$MLD_KAT_KODE <- as.character(str_trim(MELD$MLD_KAT_KODE));
MELD$PLANAFVIGELSE <- as.numeric(str_trim(MELD$PLANAFVIGELSE));
MELD$FORS_TRIN_NUMMER <- as.integer(str_trim(MELD$FORS_TRIN_NUMMER));
MELD$MELDINGSKILDE <- as.character(str_trim(MELD$MELDINGSKILDE));
MELD$TRA_AR_TYP_KODE <- as.character(str_trim(MELD$TRA_AR_TYP_KODE));
MELD$ARSAG_TOGNUMMER <- as.character(str_trim(MELD$ARSAG_TOGNUMMER));
MELD$GLOBAL_TEKST <- as.character(str_trim(MELD$GLOBAL_TEKST));
MELD$ISOLATIONSNUMMER <- as.character(str_trim(MELD$ISOLATIONSNUMMER));
MELD$MELD_ID <- as.character(str_trim(MELD$MELD_ID));
MELD$KPL_TID_ID <- as.character(str_trim(MELD$KPL_TID_ID));
MELD$ISOLA_ID <- as.character(str_trim(MELD$ISOLA_ID));
MELD$MORGEN_EFTERM <- as.character(str_trim(MELD$MORGEN_EFTERM));

# Tjek MELDs datastruktur EFTER reformat
str(MELD);

# Hent registreringsstationstabel
Straekningstabel <- read_excel("C:/Punktlighed/raw/straekning/Straekningstabel.xlsx", sheet = "Registreringsstation RAW", skip = 1);
head(Straekningstabel);

# Berig MELD-tabel med markering af registeringsstation
MELD$REG_STAT <- MELD$STAT_KODE %in% Straekningstabel$Stationsforkortelse; 

# Fjern planaflyst tog fra MELD
MELD <- MELD[!((MELD$MLD_KAT_KODE == "A") & MELD$TRA_AR_TYP_KODE == "160"),];

# Hent togproduktstabel
prod <- read_excel("C:/Punktlighed/raw/produkt/mhon_produkt_tabel.xlsx");
head(prod);

# Tjek togproduktstabels datastruktur
str(prod);

# Omdøb det første kolonnenavn i togproduktstabel
names(prod)[1] <- "PROD_KODE";
names(prod);

# Berig MELD-tabel med info om togenes produkt (togproduktstabel)
MELD <- left_join(x = MELD, y = prod, by = "PROD_KODE");
head(MELD);

# Hent isolationstabel, hvori man kan finde oplysning om ATNS-korrektioner
rds_isolation <- read_excel("C:/Punktlighed/raw/rds_isolation/RDS_ISOLATION_RAW_20180515.xlsx");
head(rds_isolation);

# Tjek isolationstabels datastruktur FØR reformat
str(rds_isolation)

rds_isolation$ID <- as.character(str_trim(rds_isolation$ID));
rds_isolation$ISOLATIONSNUMMER <- as.character(str_trim(rds_isolation$ISOLATIONSNUMMER));
rds_isolation$STAT_KODE <- as.character(str_trim(rds_isolation$STAT_KODE));
rds_isolation$MLD_TYP_KODE <- as.character(str_trim(rds_isolation$MLD_TYP_KODE));
rds_isolation$ISOLATIONSTYPE <- as.character(str_trim(rds_isolation$ISOLATIONSTYPE));
rds_isolation$REGISTRERET_TIDSPUNKT <- as.POSIXct(str_trim(rds_isolation$ REGISTRERET_TIDSPUNKT), format='%Y-%m-%d %H:%M:%S');
rds_isolation$REGISTRERET_AF <- as.character(str_trim(rds_isolation$REGISTRERET_AF));
rds_isolation$ENDRET_TIDSPUNKT <- as.POSIXct(str_trim(rds_isolation$ENDRET_TIDSPUNKT), format='%Y-%m-%d %H:%M:%S');
rds_isolation$STAT_KODE_MELD <- as.character(str_trim(rds_isolation$STAT_KODE_MELD));
rds_isolation$DELSTR_NR <- as.character(str_trim(rds_isolation$DELSTR_NR));
rds_isolation$DELSTR_RETNING <- as.character(str_trim(rds_isolation$DELSTR_RETNING));
rds_isolation$STAT_KODE_MELD <- as.character(str_trim(rds_isolation$STAT_KODE_MELD));
rds_isolation$PLANAFVIGELSE_OFFSET <- as.numeric(str_trim(rds_isolation$PLANAFVIGELSE_OFFSET));
rds_isolation$SPORNR <- as.character(str_trim(rds_isolation$SPORNR));

# Tjek isolationstabels datastruktur FØR reformat
str(rds_isolation)

# Vi er kun interesseret i ATNS-korrektioner
rds_isolation <- rds_isolation[rds_isolation$ISOLATIONSTYPE == "ATNS",];

# Isolationstabel beriges med en ny kolonne, GYLDIG_FRA
rds_isolation$GYLDIG_FRA <- rds_isolation$ENDRET_TIDSPUNKT;
rds_isolation$GYLDIG_FRA[is.na(rds_isolation$ENDRET_TIDSPUNKT)] <- rds_isolation$REGISTRERET_TIDSPUNKT[is.na(rds_isolation$ENDRET_TIDSPUNKT)];

# Vi er kun interesseret i de 5 kolonner
rds_isolation <- rds_isolation[,c("ID","MLD_TYP_KODE","GYLDIG_FRA","ISOLATIONSTYPE","PLANAFVIGELSE_OFFSET","SPORNR")];

head(rds_isolation);

# Isolationstabels første kolonne "ID" omdøbes til "ISOLA_ID"
names(rds_isolation)[1] <- "ISOLA_ID";
names(rds_isolation);

MELD <- left_join(MELD, rds_isolation, by = c("ISOLA_ID","MLD_TYP_KODE"));
head(MELD,10)

# Før GYLDIG_FRA blev ATNS-korrektioner fortaget efter registrering.
# Men fra og med GYLDIG_FRA blev ATNS-korrektioner fortaget ved "roden", dvs. direkte i RDS,
# så efterfølgende korrektion ikke er nødvendig.
Skal_korrigeres <- rep(FALSE,length(MELD$GYLDIG_FRA));
Tilstraekkelig_info <- !is.na(MELD$GYLDIG_FRA) & !is.na(MELD$PLANTID)
Skal_korrigeres[Tilstraekkelig_info] <- (MELD$PLANTID[Tilstraekkelig_info] <  MELD$GYLDIG_FRA[Tilstraekkelig_info]);

MELD$PLANAFVIGELSE_KORR <- MELD$PLANAFVIGELSE;
MELD$PLANAFVIGELSE_KORR[Skal_korrigeres] <- round(MELD$PLANAFVIGELSE[Skal_korrigeres] + MELD$PLANAFVIGELSE_OFFSET[Skal_korrigeres]/60,2);
head(MELD[,c("TOGNUMMER","FORSTE_KOREDATO","STAT_KODE","MLD_TYP_KODE","MLD_KAT_KODE","PLANAFVIGELSE","GYLDIG_FRA","SPORNR","PLANAFVIGELSE_OFFSET","PLANAFVIGELSE_KORR")],10)

MELD$TOG_STATUS <- rep('fejl',nrow(MELD));

# Kun fjernbanen

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'K') & 
    (MELD$PROD_KODE != 'PS' | MELD$PROD_KODE != 'MS' | MELD$PROD_KODE != 'NS') &
    (MELD$PLANAFVIGELSE_KORR <= -3)
  ] <- 'forsinket';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'K') & 
    (MELD$PROD_KODE != 'PS' | MELD$PROD_KODE != 'MS' | MELD$PROD_KODE != 'NS') &
    (MELD$PLANAFVIGELSE_KORR > -3)
  ] <- 'rettidig';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'A') & 
    (MELD$TRA_AR_TYP_KODE == '150' | MELD$TRA_AR_TYP_KODE == '155')
  ] <- 'akutaflyst';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'A') & 
    (MELD$TRA_AR_TYP_KODE == '160')
  ] <- 'planaflyst';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'A') & 
    (MELD$TRA_AR_TYP_KODE == '151' | MELD$TRA_AR_TYP_KODE == '162')
  ] <- 'erstattet';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'A') & 
    (MELD$TRA_AR_TYP_KODE != '150' & MELD$TRA_AR_TYP_KODE != '151' & MELD$TRA_AR_TYP_KODE != '155' & MELD$TRA_AR_TYP_KODE != '160' & MELD$TRA_AR_TYP_KODE != '162')
  ] <- 'fejlaflyst';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'F')
  ] <- 'forventet';

MELD$TOG_STATUS[
  (MELD$MLD_KAT_KODE == 'Y' | MELD$MLD_KAT_KODE == 'Z')
  ] <- 'aflyst_stands';

tail(MELD)

MELD$PAAVIRKET <- rep("ikke-relevant",nrow(MELD));

MELD$PAAVIRKET[MELD$TOG_STATUS == "rettidig"] <- "ikke-paavirket";
MELD$PAAVIRKET[MELD$TOG_STATUS == "forsinket" | MELD$TOG_STATUS == "aflyst_stands" | MELD$TOG_STATUS == "akutaflyst"] <- "paavirket" 

MELD$TAELLES_MED_I_TP <- (
  (MELD$TOGTYPEKATEGORI == "Passagertog Fjernbanen Kontrak") &
    (MELD$REG_STAT) &
    (paste0(MELD$MLD_KAT_KODE, MELD$MLD_TYP_KODE) == "KI" | paste0(MELD$MLD_KAT_KODE, MELD$MLD_TYP_KODE) == "AI" | paste0(MELD$MLD_KAT_KODE, MELD$MLD_TYP_KODE) == "ZI" |  paste0(MELD$MLD_KAT_KODE, MELD$MLD_TYP_KODE) == "ZG") &
    (MELD$PAAVIRKET != "ikke-relevant")
);

head(MELD[MELD$PAAVIRKET=="ikke-relevant",c("PROD_KODE","TOGTYPEKATEGORI","STAT_KODE","REG_STAT","MLD_TYP_KODE","MLD_KAT_KODE","PLANAFVIGELSE_KORR","TOG_STATUS","PAAVIRKET","TAELLES_MED_I_TP")],10)

MELD <- (MELD %>% group_by(FORSTE_KOREDATO,TOGNUMMER,STAT_KODE) %>% mutate(HOLDETID = PLANTID - lag(PLANTID)));
head(MELD,30)

MELD$REALTID <- MELD$PLANTID;
aflyst <- MELD$MLD_KAT_KODE == "A" & !is.na(MELD$MLD_KAT_KODE);
MELD$REALTID[aflyst] <- NA;
ikke_aflyst <- MELD$MLD_KAT_KODE != "A" & !is.na(MELD$MLD_KAT_KODE);
MELD$REALTID[ikke_aflyst] <- MELD$PLANTID[ikke_aflyst] - (MELD$PLANAFVIGELSE_KORR[ikke_aflyst]*60);
head(MELD)

MELD <- (MELD %>% group_by(FORSTE_KOREDATO,TOGNUMMER,STAT_KODE) %>% mutate(REAL_HOLDETID = REALTID - lag(REALTID)));

MELD <- (MELD %>% group_by(FORSTE_KOREDATO,TOGNUMMER,STAT_KODE) %>% mutate(LAG_PLANAFV_KORR = lag(PLANAFVIGELSE_KORR)));

MELD$ANVENDT_HOLDETID <- MELD$REAL_HOLDETID
MELD$ANVENDT_HOLDETID[(MELD$LAG_PLANAFV_KORR) > 0 & !(is.na(MELD$LAG_PLANAFV_KORR))] <- MELD$REAL_HOLDETID[(MELD$LAG_PLANAFV_KORR) > 0 & !(is.na(MELD$LAG_PLANAFV_KORR))] - MELD$LAG_PLANAFV_KORR[(MELD$LAG_PLANAFV_KORR) > 0 & !(is.na(MELD$LAG_PLANAFV_KORR))]

MELD$OVERHOLDT_HOLDETID <- (MELD$ANVENDT_HOLDETID <= MELD$HOLDETID)
head(MELD[,c("TOGNUMMER","FORSTE_KOREDATO","STAT_KODE","MLD_TYP_KODE","PLANTID","MLD_KAT_KODE","PLANAFVIGELSE","PLANAFVIGELSE_KORR","HOLDETID","REALTID","REAL_HOLDETID","LAG_PLANAFV_KORR","ANVENDT_HOLDETID","OVERHOLDT_HOLDETID")],30)

MELD <- MELD[!is.na(MELD$TOGNUMMER),]

write.csv(MELD, file="C:/Punktlighed/std/rdsdata/MELD/MELD_2018_jan_jun_std_version3_overholdt_holdtid.csv", row.names=FALSE, na="")
