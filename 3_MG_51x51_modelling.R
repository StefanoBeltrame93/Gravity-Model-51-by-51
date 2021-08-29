#MG_51x51_modelling
################ GENERACION DE V. en LOGS ################
directorio =  "C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51"
setwd(directorio)
getwd()

rm(list=ls())

MG_51x51 = read.csv(file = "C:/Users/Stefano/Desktop/Stefano/R-programming Proyects/Gravity Model 51x51/Data/MG_DB_51x51.csv",
                    header = TRUE,
                    sep = ",",
                    dec = ".")
View(MG_51x51)

#Creamos las varibles en logaritmos
MG_51x51 = MG_51x51 %>%  mutate(log_export       = log(MG_51x51$Export),
                                log_import       = log(MG_51x51$Import),
                                log_PBI_Reporter = log(MG_51x51$PBI_Reporter),
                                log_PBI_Partner  = log(MG_51x51$PBI_Partner),
                                log_TCRB         = log(MG_51x51$TCRB),
                                log_distwces     = log(MG_51x51$distwces)
)

#declaramos a las variables dummies como factors
MG_51x51$contig = as.factor(MG_51x51$contig)
MG_51x51$comlang_off = as.factor(MG_51x51$comlang_off)

class(MG_51x51$contig)
class(MG_51x51$comlang_off)

toc()
######################### ARMADO DEL PANEL  ############################ 
library(gravity)
library(plm)  
View(MG_51x51)

MG_51x51 = MG_51x51 %>% arrange(Reporter, Partner, year)


#Fijamos los sub indices "i"(reporter) y "j"(partner) como "pair_id"
MG_51x51$pair_id = group_indices(MG_51x51, Reporter, Partner)

#El index = c("pair_id", "year") --> VA DENTRO DE CADA MODELO. NO HACE FALTA ARMARLO APARTE




######################### modelado  ############################ 
# modelo a estimar:

# log_eport = b0 + b1.log_PBI_Reporter
#                + b2.log_PBI_Partner
#                + b3.log_TCRB
#                + b4.log_distwces

#                + b5.contig
#                + b6.comlang_off


#1) Cross Sectional Pooling 
cs_pooling_MG = plm(formula = log_export ~  log_PBI_Reporter
                    + log_PBI_Partner
                    + log_TCRB
                    + log_distwces
                    + contig
                    + comlang_off,
                    data = MG_51x51,
                    index = c("pair_id", "year"),
                    model = "pooling",
                    effect = "individual")

summary(cs_pooling_MG)



#2) Fixed Effects within estimator
FE_WE_MG = plm(formula = log_export ~  log_PBI_Reporter
               + log_PBI_Partner
               + log_TCRB
               + log_distwces
               + contig
               + comlang_off,
               data = MG_51x51,
               index = c("pair_id", "year"),
               model = "within",
               effect = "individual")
summary(FE_WE_MG)


#3) Random Effects - GLS
RE_MG = plm(formula = log_export ~  log_PBI_Reporter
            + log_PBI_Partner
            + log_TCRB
            + log_distwces
            + contig
            + comlang_off,
            data = MG_51x51,
            index = c("pair_id", "year"),
            model = "random",
            effect = "twoways")
summary(RE_MG)


#4) Efectos temporales
TE_MG = plm(formula = log_export ~  log_PBI_Reporter
            + log_PBI_Partner
            + log_TCRB
            + log_distwces
            + contig
            + comlang_off,
            data = MG_51x51,
            index = c("pair_id", "year"),
            model = "random",
            effect = "time")
summary(TE_MG)


#5) Two ways within estimator
TW_WE_MG = plm(formula = log_export ~  log_PBI_Reporter
               + log_PBI_Partner
               + log_TCRB
               + log_distwces
               + contig
               + comlang_off,
               data = MG_51x51,
               index = c("pair_id", "year"),
               model = "within",
               effect = "twoways")
summary(TW_WE_MG)


################### Test de Evaluacion #################

#i)Hausmann FE vs RE (GLS)
phtest(FE_WE_MG, RE_MG)
#FE es superior a RE ya que rechazamos H0


# ii) Tests de efectos individuales - F and LMtests
# Compara within one way con MCO
