#MG_51x51_EDA


rm(list=ls())
directorio =  "C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/"
setwd(directorio)
getwd()

library(readxl)
library(tidyverse)
MG_51x51 = read.csv(file = "C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/MG_DB_51x51.csv",
                    header = TRUE,
                    sep = ",",
                    dec = ".")


Comercio = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/Comercio.xlsx", 
                      sheet = "Hoja1",
                      range = "A1:Y5328")

Comercio = Comercio %>% rename(Reporter = ReporterISO3, Partner = PartnerISO3) %>%#Rename variables
  select(Reporter, Partner, TradeFlowName, yr2000, yr2001, yr2002, yr2003, yr2004, yr2005, yr2006, yr2007, yr2008, yr2009, yr2010, yr2011, yr2012, yr2013, yr2014,yr2015, yr2016) %>%
  mutate(Reporter = gsub("ZonaEuro", "EMU", Reporter), #Cuando Reporter es = a ZonaEuro --> poner "EMU"
         Partner = gsub("ZonaEuro", "EMU", Partner)) #Cuando Partner es = a ZonaEuro --> poner "EMU"

Comercio_expand = Comercio %>% expand(Reporter, Partner, TradeFlowName) #Fill In de Stata
Comercio = Comercio_expand %>% left_join(Comercio) %>% #left_join para unir la base amplia con esta
  filter(Reporter != Partner) #Eliminamos cuando Reporter = Partner


#Exportaciones
Comercio_export = Comercio %>% arrange(Reporter, Partner, TradeFlowName) %>%
  filter(TradeFlowName == "Export") %>%
  rename(Export = TradeFlowName) %>%
  pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "Trade") %>%
  select(Reporter, Partner, year, Trade) %>% 
  mutate(year = as.integer(gsub("yr", "", year))) %>%
  rename(Export = Trade)


#Importaciones
Comercio_import = Comercio %>% arrange(Reporter, Partner, TradeFlowName) %>% 
  filter(TradeFlowName == "Import") %>% 
  pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "Trade") %>% 
  select(Reporter, Partner, year, Trade) %>% 
  mutate(year = as.integer(gsub("yr", "", year))) %>% 
  rename(Import = Trade)

#PBI Reporter
PBI_Reporter = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/PBI.xlsx",
                          sheet = "Paises Elegidos 2000-2016",
                          range = "A1:S52")
PBI_Reporter$CountryName = NULL
PBI_Reporter = PBI_Reporter %>% pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "PBI_R") %>% 
  mutate(year = as.integer(gsub("yr", "", year))) %>% 
  rename(Reporter = CountryCode) %>%
  arrange(Reporter, year, PBI_R)
View(PBI_Reporter)

#PBI Reporter:  
PBI_Partner = read_excel("C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/PBI.xlsx",
                         sheet = "Paises Elegidos 2000-2016",
                         range = "A1:S52")
PBI_Partner$CountryName = NULL
PBI_Partner = PBI_Partner %>%  pivot_longer(cols = yr2000:yr2016, names_to = "year", values_to = "PBI_P") %>% 
  mutate(year = as.integer(gsub("yr", "", year))) %>%
  rename(Partner = CountryCode) %>% 
  arrange(Partner, year, PBI_P)
View(PBI_Partner)


#EDA_MG_51x51 = Expo + Impo + PBI_Reporter + PBI_Partner

EDA_MG_51x51 = Comercio_export %>%
  left_join(Comercio_import) %>% 
  left_join(PBI_Reporter) %>% 
  left_join(PBI_Partner)



################ HAY QUE VER COMO HAGO LA SUMA DE LOS PBI PARA TODOS LOS A?OS
# EDA_MG_51x51_1 = EDA_MG_51x51 %>%
#   filter(Reporter == "All") %>%
#  select(year, PBI_Partner) %>% 
#   mutate(PBI_global = cumsum(PBI_Partner) )

############# Analisis Exploratorio de la base de datos MG_51x51 ################ 


#ARG
as_tibble(EDA_MG_51x51)
MG_51x51_ARG = MG_51x51 %>% filter(Reporter == "ARG") 

graf1 = ggplot(data = MG_51x51_ARG %>% filter(Reporter == "ARG", Partner == "USA")) +
  geom_line(mapping = aes(x =year, y=TCRB), colour = "red", size = 1.1) +
  labs(x = NULL, y = "TCRB ARG - USA", title = "TCRB entre Argentina y el USA", subtitle = "entre los a?os 2000 y 2016")+
  theme(legend.position = "top")
graf1  

graf2 = ggplot(data = EDA_MG_51x51 %>%
                 filter(Reporter == "ARG", Partner == "WLD") %>%
                 mutate(Flujo = Export - Import) %>% 
                 mutate(Export = Export/1000000, Import = Import/1000000, Flujo = Flujo/1000000))+
  geom_line(mapping = aes(x = year, y = Export), color = "green", size = 1)+
  geom_line(mapping = aes(x = year, y = Import), color = "red", size = 1)+
  geom_col(mapping = aes(x = year, y = Flujo))+
  labs(x = NULL, y = "Export & Import", title = "Exportaciones e Importaciones", subtitle = "Entre Argentina y el Mundo, en Millones US$")+
  theme(legend.position = "top")+
  guides(fill = "none")
graf2

PBI_Reporter$PBI_R

#Export Arg vs PBI Arg

#https://community.rstudio.com/t/assign-2-geom-lines-to-different-y-axis/14797/5
# Este link sirve para saber como ir armando el grafico

scl = 10 #scale
graf3 = ggplot(data = EDA_MG_51x51 %>% filter(Reporter == "ARG", Partner == "WLD") %>% 
                 mutate(PBI_R = PBI_R))+
  geom_line(mapping = aes(x = year, y = Export), colour = "green", size = 1.1) +
  geom_line(mapping = aes(x = year, y = PBI_R/scl), colour = "blue", size = 1.1) +
  scale_y_continuous(sec.axis = sec_axis(~.*scl, name = "PBI_R"))+
  scale_x_continuous("year", breaks = 2000:2016)+
  ggtitle(label = "PBI & Exportaciones (Argentina)", subtitle = "Entre 2000 y 2016; en US$")+
  theme(legend.position = "bottom")
graf3


#Participaci?n Expo de ARg por Paises

#correlograma?