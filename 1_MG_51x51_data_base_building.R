######################## Armado de la Base de Datos ################
rm(list=ls())
library(tictoc)
tic()
directorio = "C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51"
setwd(directorio)
getwd()

library(readxl)
library(tidyverse)

#COMERCIO

Comercio <- read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/Comercio.xlsx", 
                       sheet = "Hoja1",
                       range = "A1:Y5328")


Comercio$Nomenclature = NULL 
Comercio$ProductCode = NULL
Comercio$ReporterName = NULL 
Comercio$PartnerName = NULL 
Comercio$TradeFlowCode = NULL 

Comercio = as_tibble(Comercio)


Comercio = Comercio %>% rename(Reporter = ReporterISO3, Partner = PartnerISO3) %>% #Rename
  filter(!Reporter == "All", !Partner == "WLD") %>% #filter para hacer un "drop" de Reporter == "All" & Partner = "WLD". Con el " ! " adeante de la var. excluimos esas obs
  mutate(Reporter = gsub("ZonaEuro", "EMU", Reporter)) %>% #Replace ZonaEuro by EMU 
  mutate(Partner = gsub("ZonaEuro", "EMU", Partner))  #Replace ZonaEuro by EMU

Comercio_expand =  Comercio %>% expand(Reporter, Partner, TradeFlowName) #Fill In de Stata
Comercio = Comercio_expand %>% dplyr::left_join(Comercio) #Fill In de Stata hacemos un left_join
Comercio = Comercio %>% filter(Reporter != Partner) #Eliminamos cuando Reporter = Partner


#EXPORTACIONES
Comercio_export = Comercio %>%
  filter(TradeFlowName == "Export") %>%
  pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "trade") %>%
  mutate(year = gsub("yr", "", year)) %>% #global substitute: substitute all occurrences of a pattern.
  mutate(year = as.integer(year)) %>%
  rename(Export = trade) %>%
  arrange(Reporter, Partner, year)
Comercio_export$TradeFlowName = NULL 
#View(Comercio_export)

#IMPORTACIONES  
Comercio_import = Comercio %>%
  filter(TradeFlowName == "Import") %>%
  pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "trade") %>%
  mutate(year = gsub("yr", "", year)) %>%
  mutate(year = as.integer(year)) %>%
  rename(Import = trade) %>%
  arrange(Reporter, Partner, year)
Comercio_import$TradeFlowName = NULL 
#View(Comercio_import)


########## PIB (en dolares corrientes)

PBI = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/PBI.xlsx",
                 sheet = "Paises Elegidos 2000-2016", range = "A1:S52")
PBI$CountryName = NULL 
PBI = PBI %>% pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "PBI") %>%
  mutate(year = gsub("yr", "", year)) %>%
  arrange(CountryCode, year, PBI)
PBI$year = as.integer(PBI$year)
class(PBI$year)
#View(PBI)
PBI

#Generamos PBI del Reporter
PBI_reporter = PBI %>% rename(Reporter = CountryCode, PBI_Reporter = PBI)
#View(PBI_reporter)

#Generamos PBI del Partner
PBI_partner = PBI %>% rename(Partner = CountryCode, PBI_Partner = PBI)
#View(PBI_partner)
########## TCRB

#Nota: la idea es montar con un loop pero de primera instancia va a estar dificil.... hacerl los 51 individualmente y ver como montar el loop luego





#ARGENTINA
TCRB_ARG = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-ARG",
                      range = "A46:BA63")
#View(TCRB_ARG)
TCRB_ARG$TCRB_ARG_BEN = NULL
TCRB_ARG = TCRB_ARG %>% pivot_longer(cols = TCRB_ARG_CAN:TCRB_ARG_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_ARG_","", Partner)) %>%
  mutate(Reporter = "ARG") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)


#AUSTRALIA
TCRB_AUS = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-AUS",
                      range = "A46:BA63")
TCRB_AUS$TCRB_AUS_BEN = NULL
TCRB_AUS = TCRB_AUS %>% pivot_longer(cols = TCRB_AUS_CAN:TCRB_AUS_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_AUS_","", Partner)) %>%
  mutate(Reporter = "AUS") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)
TCRB_AUS


#BRASIL
TCRB_BRA = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-BRA",
                      range = "A46:BA63")
TCRB_BRA$TCRB_BRA_BEN = NULL
TCRB_BRA = TCRB_BRA %>% pivot_longer(cols = TCRB_BRA_CAN:TCRB_BRA_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_BRA_","", Partner)) %>%
  mutate(Reporter = "BRA") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)
TCRB_BRA

#CANADA
TCRB_CAN = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-CAN",
                      range = "A46:BA63")
TCRB_CAN$TCRB_CAN_BEN = NULL
TCRB_CAN = TCRB_CAN %>% pivot_longer(cols = TCRB_CAN_CAN:TCRB_CAN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_CAN_","", Partner)) %>%
  mutate(Reporter = "CAN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)
TCRB_CAN

#SUiZA
TCRB_CHE = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-CHE",
                      range = "A46:BA63")
TCRB_CHE$TCRB_CHE_BEN = NULL
TCRB_CHE = TCRB_CHE %>% pivot_longer(cols = TCRB_CHE_CAN:TCRB_CHE_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_CHE_","", Partner)) %>%
  mutate(Reporter = "CHE") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)
TCRB_CHE

#CHILE
TCRB_CHL= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-CHL",
                     range = "A46:BA63")
TCRB_CHL$TCRB_CHL_BEN = NULL
TCRB_CHL = TCRB_CHL %>% pivot_longer(cols = TCRB_CHL_CAN:TCRB_CHL_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_CHL_","", Partner)) %>%
  mutate(Reporter = "CHL") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)
TCRB_CHL

#CHINA
TCRB_CHN= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-CHN",
                     range = "A46:BA63")
TCRB_CHN$TCRB_CHN_BEN = NULL
TCRB_CHN = TCRB_CHN %>% pivot_longer(cols = TCRB_CHN_CAN:TCRB_CHN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_CHN_","", Partner)) %>%
  mutate(Reporter = "CHN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year)
TCRB_CHN

#COL
TCRB_COL= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-COL",
                     range = "A46:BA63")
TCRB_COL$TCRB_COL_BEN = NULL
TCRB_COL = TCRB_COL %>% pivot_longer(cols = TCRB_COL_CAN:TCRB_COL_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_COL_","", Partner)) %>%
  mutate(Reporter = "COL") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_COL

#CRI
TCRB_CRI= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-CRI",
                     range = "A46:BA63")
TCRB_CRI$TCRB_CRI_BEN = NULL
TCRB_CRI = TCRB_CRI %>% pivot_longer(cols = TCRB_CRI_CAN:TCRB_CRI_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_CRI_","", Partner)) %>%
  mutate(Reporter = "CRI") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_CRI

#DNK
TCRB_DNK= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-DNK",
                     range = "A46:BA63")
TCRB_DNK$TCRB_DNK_BEN = NULL
TCRB_DNK = TCRB_DNK %>% pivot_longer(cols = TCRB_DNK_CAN:TCRB_DNK_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_DNK_","", Partner)) %>%
  mutate(Reporter = "DNK") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_DNK

#DZA
TCRB_DZA= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-DZA",
                     range = "A46:BA63")
TCRB_DZA$TCRB_DZA_BEN = NULL
TCRB_DZA = TCRB_DZA %>% pivot_longer(cols = TCRB_DZA_CAN:TCRB_DZA_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_DZA_","", Partner)) %>%
  mutate(Reporter = "DZA") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_DZA


#EMU
TCRB_EMU= read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                     sheet = "TCRB-EMU",
                     range = "A46:BA63")
TCRB_EMU$TCRB_EMU_BEN = NULL
TCRB_EMU = TCRB_EMU %>% pivot_longer(cols = TCRB_EMU_CAN:TCRB_EMU_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_EMU_","", Partner)) %>%
  mutate(Reporter = "EMU") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_EMU


#GBR
TCRB_GBR = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-GBR",
                      range = "A46:BA63")
TCRB_GBR$TCRB_GBR_BEN = NULL
TCRB_GBR = TCRB_GBR %>% pivot_longer(cols = TCRB_GBR_CAN:TCRB_GBR_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_GBR_","", Partner)) %>%
  mutate(Reporter = "GBR") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_GBR

#GTM
TCRB_GTM = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-GTM",
                      range = "A46:BA63")
TCRB_GTM$TCRB_GTM_BEN = NULL
TCRB_GTM = TCRB_GTM %>% pivot_longer(cols = TCRB_GTM_CAN:TCRB_GTM_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_GTM_","", Partner)) %>%
  mutate(Reporter = "GTM") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_GTM  

#HKG
TCRB_HKG = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-HKG",
                      range = "A46:BA63")
TCRB_HKG$TCRB_HKG_BEN = NULL
TCRB_HKG = TCRB_HKG %>% pivot_longer(cols = TCRB_HKG_CAN:TCRB_HKG_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_HKG_","", Partner)) %>%
  mutate(Reporter = "HKG") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_HKG

#HUN
TCRB_HUN = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-HUN",
                      range = "A46:BA63")
TCRB_HUN$TCRB_HUN_BEN = NULL
TCRB_HUN = TCRB_HUN %>% pivot_longer(cols = TCRB_HUN_CAN:TCRB_HUN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_HUN_","", Partner)) %>%
  mutate(Reporter = "HUN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_HUN

#IDN
TCRB_IDN = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-IDN",
                      range = "A46:BA63")
TCRB_IDN$TCRB_IDN_BEN = NULL
TCRB_IDN = TCRB_IDN %>% pivot_longer(cols = TCRB_IDN_CAN:TCRB_IDN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_IDN_","", Partner)) %>%
  mutate(Reporter = "IDN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_IDN


#IND
TCRB_IND = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-IND",
                      range = "A46:BA63")
TCRB_IND$TCRB_IND_BEN = NULL
TCRB_IND = TCRB_IND %>% pivot_longer(cols = TCRB_IND_CAN:TCRB_IND_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_IND_","", Partner)) %>%
  mutate(Reporter = "IND") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_IND


#ISR
TCRB_ISR = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-ISR",
                      range = "A46:BA63")
TCRB_ISR$TCRB_ISR_BEN = NULL
TCRB_ISR = TCRB_ISR %>% pivot_longer(cols = TCRB_ISR_CAN:TCRB_ISR_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_ISR_","", Partner)) %>%
  mutate(Reporter = "ISR") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_ISR

#JPN
TCRB_JPN = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-JPN",
                      range = "A46:BA63")
TCRB_JPN$TCRB_JPN_BEN = NULL
TCRB_JPN = TCRB_JPN %>% pivot_longer(cols = TCRB_JPN_CAN:TCRB_JPN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_JPN_","", Partner)) %>%
  mutate(Reporter = "JPN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_JPN

#KOR
TCRB_KOR = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-KOR",
                      range = "A46:BA63")
TCRB_KOR$TCRB_KOR_BEN = NULL
TCRB_KOR = TCRB_KOR %>% pivot_longer(cols = TCRB_KOR_CAN:TCRB_KOR_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_KOR_","", Partner)) %>%
  mutate(Reporter = "KOR") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_KOR

#MEX
TCRB_MEX = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-MEX",
                      range = "A46:BA63")
TCRB_MEX$TCRB_MEX_BEN = NULL
TCRB_MEX = TCRB_MEX %>% pivot_longer(cols = TCRB_MEX_CAN:TCRB_MEX_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_MEX_","", Partner)) %>%
  mutate(Reporter = "MEX") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_MEX

#MYS
TCRB_MYS = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-MYS",
                      range = "A46:BA63")
TCRB_MYS$TCRB_MYS_BEN = NULL
TCRB_MYS = TCRB_MYS %>% pivot_longer(cols = TCRB_MYS_CAN:TCRB_MYS_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_MYS_","", Partner)) %>%
  mutate(Reporter = "MYS") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_MYS

#RUS
TCRB_RUS = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-RUS",
                      range = "A46:BA63")
TCRB_RUS$TCRB_RUS_BEN = NULL
TCRB_RUS = TCRB_RUS %>% pivot_longer(cols = TCRB_RUS_CAN:TCRB_RUS_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_RUS_","", Partner)) %>%
  mutate(Reporter = "RUS") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_RUS

#SEN
TCRB_SEN = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-SEN",
                      range = "A46:BA63")
TCRB_SEN$TCRB_SEN_BEN = NULL
TCRB_SEN = TCRB_SEN %>% pivot_longer(cols = TCRB_SEN_CAN:TCRB_SEN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_SEN_","", Partner)) %>%
  mutate(Reporter = "SEN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_SEN

#SGP
TCRB_SGP = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-SGP",
                      range = "A46:BA63")
TCRB_SGP$TCRB_SGP_BEN = NULL
TCRB_SGP = TCRB_SGP %>% pivot_longer(cols = TCRB_SGP_CAN:TCRB_SGP_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_SGP_","", Partner)) %>%
  mutate(Reporter = "SGP") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_SGP

#SWE
TCRB_SWE = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-SWE",
                      range = "A46:BA63")
TCRB_SWE$TCRB_SWE_BEN = NULL
TCRB_SWE = TCRB_SWE %>% pivot_longer(cols = TCRB_SWE_CAN:TCRB_SWE_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_SWE_","", Partner)) %>%
  mutate(Reporter = "SWE") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_SWE

#TUR
TCRB_TUR = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-TUR",
                      range = "A46:BA63")
TCRB_TUR$TCRB_TUR_BEN = NULL
TCRB_TUR = TCRB_TUR %>% pivot_longer(cols = TCRB_TUR_CAN:TCRB_TUR_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_TUR_","", Partner)) %>%
  mutate(Reporter = "TUR") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_TUR


#TZA
TCRB_TZA = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-TZA",
                      range = "A46:BA63")
TCRB_TZA$TCRB_TZA_BEN = NULL
TCRB_TZA = TCRB_TZA %>% pivot_longer(cols = TCRB_TZA_CAN:TCRB_TZA_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_TZA_","", Partner)) %>%
  mutate(Reporter = "TZA") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_TZA

#URY
TCRB_URY = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-URY",
                      range = "A46:BA63")
TCRB_URY$TCRB_URY_BEN = NULL
TCRB_URY= TCRB_URY %>% pivot_longer(cols = TCRB_URY_CAN:TCRB_URY_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_URY_","", Partner)) %>%
  mutate(Reporter = "URY") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_URY

#USA
TCRB_USA = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-USA",
                      range = "A46:BA63")
TCRB_USA$TCRB_USA_BEN = NULL
TCRB_USA= TCRB_USA %>% pivot_longer(cols = TCRB_USA_CAN:TCRB_USA_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_USA_","", Partner)) %>%
  mutate(Reporter = "USA") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_USA

#ZAF
TCRB_ZAF = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-ZAF",
                      range = "A46:BA63")
TCRB_ZAF$TCRB_ZAF_BEN = NULL
TCRB_ZAF= TCRB_ZAF %>% pivot_longer(cols = TCRB_ZAF_CAN:TCRB_ZAF_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_ZAF_","", Partner)) %>%
  mutate(Reporter = "ZAF") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_ZAF

#CZE
TCRB_CZE = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-CZE",
                      range = "A46:BA63")
TCRB_CZE$TCRB_CZE_BEN = NULL
TCRB_CZE= TCRB_CZE %>% pivot_longer(cols = TCRB_CZE_CAN:TCRB_CZE_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_CZE_","", Partner)) %>%
  mutate(Reporter = "CZE") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_CZE

#ECU
TCRB_ECU = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-ECU",
                      range = "A46:BA63")
TCRB_ECU$TCRB_ECU_BEN = NULL
TCRB_ECU= TCRB_ECU %>% pivot_longer(cols = TCRB_ECU_CAN:TCRB_ECU_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_ECU_","", Partner)) %>%
  mutate(Reporter = "ECU") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_ECU

#SLV
TCRB_SLV = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-SLV",
                      range = "A46:BA63")
TCRB_SLV$TCRB_SLV_BEN = NULL
TCRB_SLV= TCRB_SLV %>% pivot_longer(cols = TCRB_SLV_CAN:TCRB_SLV_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_SLV_","", Partner)) %>%
  mutate(Reporter = "SLV") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_SLV


#JAM
TCRB_JAM = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-JAM",
                      range = "A46:BA63")
TCRB_JAM$TCRB_JAM_BEN = NULL
TCRB_JAM= TCRB_JAM %>% pivot_longer(cols = TCRB_JAM_CAN:TCRB_JAM_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_JAM_","", Partner)) %>%
  mutate(Reporter = "JAM") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_JAM

#JOR
TCRB_JOR = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-JOR",
                      range = "A46:BA63")
TCRB_JOR$TCRB_JOR_BEN = NULL
TCRB_JOR= TCRB_JOR %>% pivot_longer(cols = TCRB_JOR_CAN:TCRB_JOR_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_JOR_","", Partner)) %>%
  mutate(Reporter = "JOR") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_JOR

#PAN
TCRB_PAN = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-PAN",
                      range = "A46:BA63")
TCRB_PAN$TCRB_PAN_BEN = NULL
TCRB_PAN= TCRB_PAN %>% pivot_longer(cols = TCRB_PAN_CAN:TCRB_PAN_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_PAN_","", Partner)) %>%
  mutate(Reporter = "PAN") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_PAN

#PRY
TCRB_PRY = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-PRY",
                      range = "A46:BA63")
TCRB_PRY$TCRB_PRY_BEN = NULL
TCRB_PRY= TCRB_PRY %>% pivot_longer(cols = TCRB_PRY_CAN:TCRB_PRY_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_PRY_","", Partner)) %>%
  mutate(Reporter = "PRY") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_PRY

#PER
TCRB_PER = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-PER",
                      range = "A46:BA63")
TCRB_PER$TCRB_PER_BEN = NULL
TCRB_PER= TCRB_PER %>% pivot_longer(cols = TCRB_PER_CAN:TCRB_PER_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_PER_","", Partner)) %>%
  mutate(Reporter = "PER") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_PER

#BOL
TCRB_BOL = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-BOL",
                      range = "A46:BA63")
TCRB_BOL$TCRB_BOL_BEN = NULL
TCRB_BOL= TCRB_BOL %>% pivot_longer(cols = TCRB_BOL_CAN:TCRB_BOL_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_BOL_","", Partner)) %>%
  mutate(Reporter = "BOL") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_BOL

#SAU
TCRB_SAU = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-SAU",
                      range = "A46:BA63")
TCRB_SAU$TCRB_SAU_BEN = NULL
TCRB_SAU= TCRB_SAU %>% pivot_longer(cols = TCRB_SAU_CAN:TCRB_SAU_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_SAU_","", Partner)) %>%
  mutate(Reporter = "SAU") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_SAU

#THA
TCRB_THA = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-THA",
                      range = "A46:BA63")
TCRB_THA$TCRB_THA_BEN = NULL
TCRB_THA= TCRB_THA %>% pivot_longer(cols = TCRB_THA_CAN:TCRB_THA_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_THA_","", Partner)) %>%
  mutate(Reporter = "THA") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_THA

#TTO
TCRB_TTO = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-TTO",
                      range = "A46:BA63")
TCRB_TTO$TCRB_TTO_BEN = NULL
TCRB_TTO= TCRB_TTO %>% pivot_longer(cols = TCRB_TTO_CAN:TCRB_TTO_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_TTO_","", Partner)) %>%
  mutate(Reporter = "TTO") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_TTO

#LKA
TCRB_LKA = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-LKA",
                      range = "A46:BA63")
TCRB_LKA$TCRB_LKA_BEN = NULL
TCRB_LKA= TCRB_LKA %>% pivot_longer(cols = TCRB_LKA_CAN:TCRB_LKA_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_LKA_","", Partner)) %>%
  mutate(Reporter = "LKA") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_LKA

#NIC
TCRB_NIC = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-NIC",
                      range = "A46:BA63")
TCRB_NIC$TCRB_NIC_BEN = NULL
TCRB_NIC= TCRB_NIC %>% pivot_longer(cols = TCRB_NIC_CAN:TCRB_NIC_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_NIC_","", Partner)) %>%
  mutate(Reporter = "NIC") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_NIC

#HND
TCRB_HND = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-HND",
                      range = "A46:BA63")
TCRB_HND$TCRB_HND_BEN = NULL
TCRB_HND= TCRB_HND %>% pivot_longer(cols = TCRB_HND_CAN:TCRB_HND_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_HND_","", Partner)) %>%
  mutate(Reporter = "HND") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_HND

#DOM
TCRB_DOM = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-DOM",
                      range = "A46:BA63")
TCRB_DOM$TCRB_DOM_BEN = NULL
TCRB_DOM= TCRB_DOM %>% pivot_longer(cols = TCRB_DOM_CAN:TCRB_DOM_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_DOM_","", Partner)) %>%
  mutate(Reporter = "DOM") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_DOM

#NOR
TCRB_NOR = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-NOR",
                      range = "A46:BA63")
TCRB_NOR$TCRB_NOR_BEN = NULL
TCRB_NOR= TCRB_NOR %>% pivot_longer(cols = TCRB_NOR_CAN:TCRB_NOR_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_NOR_","", Partner)) %>%
  mutate(Reporter = "NOR") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_NOR

#NZL
TCRB_NZL = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-NZL",
                      range = "A46:BA63")
TCRB_NZL$TCRB_NZL_BEN = NULL
TCRB_NZL= TCRB_NZL %>% pivot_longer(cols = TCRB_NZL_CAN:TCRB_NZL_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_NZL_","", Partner)) %>%
  mutate(Reporter = "NZL") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_NZL

#POL
TCRB_POL = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/TCR.xlsx",
                      sheet = "TCRB-POL",
                      range = "A46:BA63")
TCRB_POL$TCRB_POL_BEN = NULL
TCRB_POL= TCRB_POL %>% pivot_longer(cols = TCRB_POL_CAN:TCRB_POL_USA, names_to = "Partner", values_to = "TCRB") %>%
  mutate(Partner = gsub("TCRB_POL_","", Partner)) %>%
  mutate(Reporter = "POL") %>%
  rename(year = años) %>%
  select(year, Reporter, Partner, TCRB) %>%
  arrange(Reporter, Partner, year) 
TCRB_POL

#Hacemos un bind_rows con todos los TCRB para obtener TCRB
TCRB = bind_rows(TCRB_ARG, #1
                 TCRB_AUS, #2
                 TCRB_BRA, #3
                 TCRB_CAN, #4
                 TCRB_CHE, #5
                 TCRB_CHL, #6
                 TCRB_CHN, #7
                 TCRB_COL, #8
                 TCRB_CRI, #9
                 TCRB_DNK,#10
                 TCRB_DZA,#11
                 TCRB_EMU,#12
                 TCRB_GBR,#13
                 TCRB_GTM,#14
                 TCRB_HKG,#15
                 TCRB_HUN,#16
                 TCRB_IDN,#17
                 TCRB_IND,#18
                 TCRB_ISR,#19
                 TCRB_JPN,#20
                 TCRB_KOR,#21
                 TCRB_MEX,#22
                 TCRB_MYS,#23
                 TCRB_RUS,#24
                 TCRB_SEN,#25
                 TCRB_SGP,#26
                 TCRB_SWE,#27
                 TCRB_TUR,#28
                 TCRB_TZA,#29
                 TCRB_URY,#30
                 TCRB_USA,#31
                 TCRB_ZAF,#32
                 TCRB_CZE,#33
                 TCRB_ECU,#34
                 TCRB_SLV,#35
                 TCRB_JAM,#36
                 TCRB_JOR,#37
                 TCRB_PAN,#38
                 TCRB_PRY,#39
                 TCRB_PER,#40
                 TCRB_BOL,#41
                 TCRB_SAU,#42
                 TCRB_THA,#43
                 TCRB_TTO,#44
                 TCRB_LKA,#45
                 TCRB_NIC,#46
                 TCRB_HND,#47
                 TCRB_DOM,#48
                 TCRB_NOR,#49
                 TCRB_NZL,#50
                 TCRB_POL,#51
) %>%
  arrange(Reporter, Partner, year, TCRB)    
#View(TCRB)

## CEPI (datos distancia)

distancia = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/Distancia.xlsx",
                       sheet = "51x51",
                       range = "A1:N2602")
distancia = distancia %>% rename(Reporter = iso_o) %>%
  rename(Partner = iso_d) %>%
  mutate(Reporter = gsub("BEL", "EMU", Reporter)) %>%
  mutate(Partner = gsub("BEL", "EMU", Partner)) %>%
  arrange(Reporter, Partner)
#View(distancia)


## Poblacion

Poblacion = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/Poblacion.xlsx")
#View(Poblacion)
Poblacion$CountryName = NULL
Poblacion = Poblacion %>% rename(Reporter = CountryCode) %>%
  pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "poblacion") %>%
  mutate(year = gsub("yr","", year)) %>%
  arrange(Reporter, year, poblacion)

#ARMADO DE LA BASE FINAL
#COMERCIO(X+M) + TCRB + POB + DIST.

MG_51x51 = Comercio_export
#View(MG_51x51)
MG_51x51 = MG_51x51 %>% 
  left_join(Comercio_import) %>%
  left_join(PBI_reporter) %>%
  left_join(PBI_partner) %>%
  left_join(TCRB) %>%
  left_join(distancia)

MG_51x51 = as_tibble(MG_51x51)

################## INSPECCIONAMOS LA BASE DE DATOS ###########################
summary(MG_51x51)

#Export --> tiene min = 0 y NA's = 51
#Import --> tiene min = 0 y NA's = 102
#TCRB --> tiene  NA's = 1530 ? quienes son?

#Analisis de TCRB
#TCRB_na_row_index = which(is.na(MG_51x51$TCRB))
#MG_51x51_na = MG_51x51[TCRB_na_row_index,]
#View(MG_51x51_na)

#YA LOS CORREGIMOS --> era porque estaba mal formateado el excel en el caso de TCRB_NZL y mal el c?digo


#Analisis sobre Export e Import
#Vemos cuantos son "0" cuanto son NA. 
#En este caso vamos a reemplazar los 0 y los NA con 1 por el tema de los logaritmos (log(1)=0 pero log(0)=-Inf)

#Vemos casos Export
#Casos de 0
Export_0_index = which(MG_51x51$Export == 0)
MG_51x51_export_0 = MG_51x51[Export_0_index,]
#View(MG_51x51_export_0)
#Son 1967 rows de Export con valor 0 entre Reporter-Partner. Reemplazamos con 1
MG_51x51$Export[which(MG_51x51$Export == 0)] = 1

#Casos de NA
Export_na_index = which(is.na(MG_51x51$Export))
MG_51x51_export_na = MG_51x51[Export_na_index,]
#View(MG_51x51_export_na)
#Son 51 rows de Export con valor NA entre Reporter-Partner. Reemplazamos con 1
MG_51x51$Export[which(is.na(MG_51x51$Export))] = 1
summary(MG_51x51$Export)

#Vemos casos Import
#Casos de 0 
Import_0_index = which(MG_51x51$Import == 0)
MG_51x51_import_0 = MG_51x51[Import_0_index, ]
#View(MG_51x51_import_0)
#Son 941 rows de Import con valor 0 entre Reporter-Partner. Reemplazamos con 1
MG_51x51$Import[which(MG_51x51$Import == 0)] = 1
summary(MG_51x51$Import)

#Casos de NA
Import_na_index = which(is.na(MG_51x51$Import == 0))
MG_51x51_import_na = MG_51x51[Import_na_index,]
#View(MG_51x51_import_na)
#Son 102 rows de Import con valor NA entre Reporter-Partner. Reemplazamos con 1
MG_51x51$Import[which(is.na(MG_51x51$Import))] = 1
summary(MG_51x51$Import)


#Double Check de que no queden 0 o 1 en las v. continuas:
summary(MG_51x51)
toc()

#Guardamos la base de datos FINAL en csv para su posterior usos
write.csv(MG_51x51, file = "C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/MG_DB_51x51.csv" )
