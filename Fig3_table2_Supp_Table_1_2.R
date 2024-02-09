library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("devtools")
devtools::install_github("thomasp85/scico")
library(scico)


datos<- read_csv("./Data/defunciones-ocurridas-y-registradas-en-la-republica-argentina-entre-los-anos-2005-2022.csv.gz", locale = locale(encoding = "ISO-8859-1"))
names(datos)[3]<-"jurisdiccion_residencia_nombre"
#group datos by jurisdiccion_residencia_nombre, grupo_etario, anio, cie10_causa_id and sum cantidad 
datos2 <- datos %>%
  group_by(grupo_edad, anio, cie10_causa_id, cie10_clasificacion) %>%
  filter(grupo_edad == '01.De a 0  a 14 anios') %>%
  summarise(cantidad = sum(cantidad))

#Create a table of the top 10 causes of death by year
datos3 <- datos2 %>%
  group_by(anio) %>%
  top_n(10, cantidad) %>%
  arrange(anio, desc(cantidad))

#Use datos2 to create a table of the sum of annual deaths by "Hepatitis aguda tipo A"
#Hepatitis A
anio_vac=2006
hepa <- datos2 %>%
  filter(cie10_clasificacion == "Hepatitis aguda tipo A") %>%
  ungroup() %>%
  complete(anio = 2005:2022) %>%
  replace_na(list(cantidad = 0)) %>%
  group_by(anio) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(vac=ifelse(anio<=anio_vac, "pre-vac", ifelse(anio>anio_vac, "post-vac", "vac"))) %>%
  filter(vac=="pre-vac" | vac=="post-vac") %>%
#use mutate and paste0 to create the period for each vac value such as "min-max"
  group_by(vac) %>%
  mutate(period=paste0(min(anio),"-",max(anio))) %>%
  mutate(enf="Hepatitis A") %>%
  group_by(vac,enf,period) %>%
  #Calculate the average number of deaths per year
  summarise(average_anual_deaths = mean(cantidad))%>%
  arrange(period)

#HEPB
anio_vac=2009
hepb <- datos2 %>%
  filter(cie10_clasificacion == "Hepatitis aguda tipo B") %>%
  ungroup() %>%
  complete(anio = 2005:2022) %>%
  replace_na(list(cantidad = 0)) %>%
  group_by(anio) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(vac=ifelse(anio<=anio_vac, "pre-vac", ifelse(anio>anio_vac, "post-vac", "vac"))) %>%
  filter(vac=="pre-vac" | vac=="post-vac") %>%
#use mutate and paste0 to create the period for each vac value such as "min-max"
  group_by(vac) %>%
  mutate(period=paste0(min(anio),"-",max(anio))) %>%
  mutate(enf="Hepatitis B") %>%
  group_by(vac,enf,period) %>%
  #Calculate the average number of deaths per year
  summarise(average_anual_deaths = mean(cantidad))%>%
  arrange(period)

  #MENINGOCOCCUS
  anio_vac=2017
mening <- datos2 %>%
  filter(cie10_clasificacion == "Enfermedad meningocócica") %>%
  ungroup() %>%
  complete(anio = 2005:2022) %>%
  replace_na(list(cantidad = 0)) %>%
  group_by(anio) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(vac=ifelse(anio<=anio_vac, "pre-vac", ifelse(anio>anio_vac, "post-vac", "vac"))) %>%
  filter(vac=="pre-vac" | vac=="post-vac") %>%
#use mutate and paste0 to create the period for each vac value such as "min-max"
  group_by(vac) %>%
  mutate(period=paste0(min(anio),"-",max(anio))) %>%
  mutate(enf="Meningococcus") %>%
  group_by(vac,enf,period) %>%
  #Calculate the average number of deaths per year
  summarise(average_anual_deaths = mean(cantidad))%>%
  arrange(period)

#VARICELA
  anio_vac=2015
vari <- datos2 %>%
  filter(cie10_clasificacion == "Varicela") %>%
  ungroup() %>%
  complete(anio = 2005:2022) %>%
  replace_na(list(cantidad = 0)) %>%
  group_by(anio) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(vac=ifelse(anio<=anio_vac, "pre-vac", ifelse(anio>anio_vac, "post-vac", "vac"))) %>%
  filter(vac=="pre-vac" | vac=="post-vac") %>%
#use mutate and paste0 to create the period for each vac value such as "min-max"
  group_by(vac) %>%
  mutate(period=paste0(min(anio),"-",max(anio))) %>%
  mutate(enf="Varicella") %>%
  group_by(vac,enf,period) %>%
  #Calculate the average number of deaths per year
  summarise(average_anual_deaths = mean(cantidad)) %>%
  arrange(period)

#COVID
  anio_vac=2022
covid <- datos2 %>%
  filter(cie10_causa_id == "U07") %>% #U07 is the code for COVID-19 
  ungroup() %>%
  complete(anio = 2020:2022) %>%
  replace_na(list(cantidad = 0)) %>%
  group_by(anio) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(vac=ifelse(anio<=anio_vac, "pre-vac", ifelse(anio>anio_vac, "post-vac", "vac"))) %>%
  filter(vac=="pre-vac" | vac=="post-vac") %>%
#use mutate and paste0 to create the period for each vac value such as "min-max"
  group_by(vac) %>%
  mutate(period=paste0(min(anio),"-",max(anio))) %>%
  mutate(enf="Covid") %>%
  group_by(vac,enf,period) %>%
  #Calculate the average number of deaths per year
  summarise(average_anual_deaths = mean(cantidad))

#rbind all the tables
vacunas <- rbind(hepa,hepb,mening,vari,covid) 

#Pivot table to wide format with new column for pre-vac and post-vac period and average annual deaths
vacunas_wide <- vacunas %>%
  pivot_wider(names_from = vac, values_from = c(average_anual_deaths,period))

#Pivot long to wider for period column
vacunas_wide2 <- vacunas_wide %>%
  rename(Disease = enf,'Pre-vaccine period' = 'period_pre-vac', 'Annual pre-vaccine deaths' = 'average_anual_deaths_pre-vac','Post-vaccine period' = 'period_post-vac', 'Annual post-vaccine deaths' = 'average_anual_deaths_post-vac')
  #reorganize columns, order Disease, Pre-vaccine period, Annual pre-vaccine deaths, Post-vaccine period, Annual post-vaccine deaths
vacunas_wide2 <- vacunas_wide2[,c(1,4,2,5,3)]
#If numeric, format numbers to 2 decimal places with printf
vacunas_wide2 <- vacunas_wide2 %>%
  mutate(
    `Annual pre-vaccine deaths` = ifelse(is.numeric(`Annual pre-vaccine deaths`), sprintf("%.2f", `Annual pre-vaccine deaths`), `Annual pre-vaccine deaths`),
    `Annual post-vaccine deaths` = ifelse(is.numeric(`Annual post-vaccine deaths`), sprintf("%.2f", `Annual post-vaccine deaths`), `Annual post-vaccine deaths`)
  )
vacunas_wide2$'Post-vaccine period'[5]<-"-"
vacunas_wide2$'Annual post-vaccine deaths'[5]<- "-"
View(vacunas_wide2)
#Write table to csv
#write_csv(vacunas_wide2,file="0_14_tabla_mortalidad_vacunas.csv")
#write_csv(vacunas_wide2,file="Table2.csv")
library(openxlsx)
write.xlsx(vacunas_wide2,file="Table2.xlsx")



####### NEW DATASET
datos2<-datos %>% filter(grupo_edad=="01.De a 0  a 14 anios")
names(datos2)<-c("anio_def","juris_resid_id","juris_resid_nombre","cod_causa_muerte_CIE10","grupo_causa_defuncion_CIE10","sexo_id","sexo","muerte_materna_id","muerte_materna_clasificacion","grupo_edad","cantidad")
datamortpedfilt<-datos2 %>% group_by(anio_def,grupo_causa_defuncion_CIE10,cod_causa_muerte_CIE10) %>% summarize(cantidad=sum(cantidad))
datamortpedfilt$grupo_causa_defuncion_CIE10<-as.character(datamortpedfilt$grupo_causa_defuncion_CIE10)
datamortpedfilt<-datamortpedfilt %>% mutate(grupo_causa_defuncion_CIE10=ifelse(cod_causa_muerte_CIE10=="U07","COVID-19",grupo_causa_defuncion_CIE10))

#GRUPOS FLAXMAN
#P00-P96 Conditions originating in the Perinatal period
#V01-X59, Y85-Y86, J69 Accidents including P
#Q00-Q99 Malformations
#X85-Y09,Y20-Y34 Homicide
#X60-X84 Suicidio
#C00-C97 Malignant Neoplasms
#I00-I09, I11,I13,I20-I51 Diseases of heart
#U07 COVID-19
#J09-J18 Influenza and pneumonia
#I60-I69 Cerebrovascular diseases

icd10groups<-datamortpedfilt %>%
 mutate(ICD10_groups = case_when(cod_causa_muerte_CIE10 %in% paste0('P',sprintf('%0.2d', 0:96)) ~ 'Conditions originating in the perinatal period (P00-P96)',
                                 cod_causa_muerte_CIE10 %in% paste0('G',sprintf('%0.2d', 00:99)) ~ 'Diseases of the nervous system (G00-G99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 15:19)) ~ 'Tuberculosis (A15-A19)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 20:49)) ~ 'Other bacterial diseases (A20-A49)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 00:09)) ~ 'Intestinal infectious diseases (A00-A09)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 50:64)) ~ 'Infections with a predominantly sexual mode of transmission (A50-A64)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 20:22)) ~ 'Other acute lower respiratory infections (J20-J22)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 60:70)) ~ 'Accidents and/or Lung diseases due to external agents (J60-J70,V01-X59,Y85,Y86)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 40:47)) ~ 'Chronic lower respiratory diseases (J40-J47)',
                                 cod_causa_muerte_CIE10 %in% paste0('V',sprintf('%0.2d', 1:99)) ~ 'Accidents and/or Lung diseases due to external agents (J60-J70,V01-X59,Y85,Y86)',
                                 cod_causa_muerte_CIE10 %in% paste0('W',sprintf('%0.2d', 0:99)) ~ 'Accidents and/or Lung diseases due to external agents (J60-J70,V01-X59,Y85,Y86)',
                                 cod_causa_muerte_CIE10 %in% paste0('X',sprintf('%0.2d', 0:59)) ~ 'Accidents and/or Lung diseases due to external agents (J60-J70,V01-X59,Y85,Y86)',
                                 cod_causa_muerte_CIE10 %in% paste0('Y',sprintf('%0.2d', 85:86)) ~ 'Accidents and/or Lung diseases due to external agents (J60-J70,V01-X59,Y85,Y86)',
                                 cod_causa_muerte_CIE10 %in% paste0('Q',sprintf('%0.2d', 0:99)) ~ 'Congenital malformations (Q00-Q99)',
                                 cod_causa_muerte_CIE10 %in% paste0('X',sprintf('%0.2d', 85:99)) ~ 'Homicide/Event of undetermined intent (X85-Y09,Y20-Y34)',
                                 cod_causa_muerte_CIE10 %in% paste0('Y',sprintf('%0.2d', 0:9)) ~ 'Homicide/Event of undetermined intent (X85-Y09,Y20-Y34)',
                                 cod_causa_muerte_CIE10 %in% paste0('Y',sprintf('%0.2d', 20:34)) ~ 'Homicide/Event of undetermined intent (X85-Y09,Y20-Y34)',
                                 cod_causa_muerte_CIE10 %in% paste0('X',sprintf('%0.2d', 60:84)) ~ 'Suicide (X60-X84)',
                                 cod_causa_muerte_CIE10 %in% paste0('C',sprintf('%0.2d', 00:97)) ~ 'Malignant neoplasms (C00-C97)',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 0:9)) ~ 'Diseases of the heart (I00-I09,I11,I13,I20-I51)',
                                 cod_causa_muerte_CIE10 %in% paste0('G',sprintf('%0.2d', 89:99)) ~ 'Other disorders of the nervous system (G89-G99)',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 11:11)) ~ 'Diseases of the heart (I00-I09,I11,I13,I20-I51)',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 13:13)) ~ 'Diseases of the heart (I00-I09,I11,I13,I20-I51)',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 20:51)) ~ 'Diseases of the heart (I00-I09,I11,I13,I20-I51)',
                                 cod_causa_muerte_CIE10 %in% paste0('U',sprintf('%0.2d', 7:7)) ~ 'COVID-19 (U07)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 9:18)) ~ 'Influenza and pneumonia (J09-J18)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 0:6)) ~ 'Other diseases of the respiratory system (J00-J06,J30-J39,J67,J70-J98)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 30:39)) ~ 'Other diseases of the respiratory system (J00-J06,J30-J39,J67,J70-J98)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 67:67)) ~ 'Other diseases of the respiratory system (J00-J06,J30-J39,J67,J70-J98)',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 70:98)) ~ 'Other diseases of the respiratory system (J00-J06,J30-J39,J67,J70-J98)',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 60:69)) ~ 'Cerebrovascular disease (I60-I69)',
                                 cod_causa_muerte_CIE10 %in% paste0('D',sprintf('%0.2d', 00:48)) ~ 'Neoplasms (D00-D48)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 00:00)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 05:05)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 20:36)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 42:44)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 48:49)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 54:79)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 81:82)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 85:85)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 86:99)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 01:04)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 06:09)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 25:49)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 55:99)) ~ 'Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)',
                                 TRUE ~ "Others"))

icd10groups<-icd10groups %>%
 mutate(ICD10_groups_nocode = case_when(cod_causa_muerte_CIE10 %in% paste0('P',sprintf('%0.2d', 0:96)) ~ 'Conditions originating in the perinatal period',
                                 cod_causa_muerte_CIE10 %in% paste0('G',sprintf('%0.2d', 00:99)) ~ 'Diseases of the nervous system',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 15:19)) ~ 'Tuberculosis',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 20:49)) ~ 'Other bacterial diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 00:09)) ~ 'Intestinal infectious diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 50:64)) ~ 'Infections with a predominantly sexual mode of transmission',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 20:22)) ~ 'Other acute lower respiratory infections',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 60:70)) ~ 'Accidents and/or Lung diseases due to external agents',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 40:47)) ~ 'Chronic lower respiratory diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('V',sprintf('%0.2d', 1:99)) ~ 'Accidents and/or Lung diseases due to external agents',
                                 cod_causa_muerte_CIE10 %in% paste0('W',sprintf('%0.2d', 0:99)) ~ 'Accidents and/or Lung diseases due to external agents',
                                 cod_causa_muerte_CIE10 %in% paste0('X',sprintf('%0.2d', 0:59)) ~ 'Accidents and/or Lung diseases due to external agents',
                                 cod_causa_muerte_CIE10 %in% paste0('Y',sprintf('%0.2d', 85:86)) ~ 'Accidents and/or Lung diseases due to external agents',
                                 cod_causa_muerte_CIE10 %in% paste0('Q',sprintf('%0.2d', 0:99)) ~ 'Congenital malformations',
                                 cod_causa_muerte_CIE10 %in% paste0('X',sprintf('%0.2d', 85:99)) ~ 'Homicide/Event of undetermined intent',
                                 cod_causa_muerte_CIE10 %in% paste0('Y',sprintf('%0.2d', 0:9)) ~ 'Homicide/Event of undetermined intent',
                                 cod_causa_muerte_CIE10 %in% paste0('Y',sprintf('%0.2d', 20:34)) ~ 'Homicide/Event of undetermined intent',
                                 cod_causa_muerte_CIE10 %in% paste0('X',sprintf('%0.2d', 60:84)) ~ 'Suicide',
                                 cod_causa_muerte_CIE10 %in% paste0('C',sprintf('%0.2d', 00:97)) ~ 'Malignant neoplasms',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 0:9)) ~ 'Diseases of the heart',
                                 cod_causa_muerte_CIE10 %in% paste0('G',sprintf('%0.2d', 89:99)) ~ 'Other disorders of the nervous system',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 11:11)) ~ 'Diseases of the heart',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 13:13)) ~ 'Diseases of the heart',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 20:51)) ~ 'Diseases of the heart',
                                 cod_causa_muerte_CIE10 %in% paste0('U',sprintf('%0.2d', 7:7)) ~ 'COVID-19',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 9:18)) ~ 'Influenza and pneumonia',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 0:6)) ~ 'Other diseases of the respiratory system',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 30:39)) ~ 'Other diseases of the respiratory system',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 67:67)) ~ 'Other diseases of the respiratory system',
                                 cod_causa_muerte_CIE10 %in% paste0('J',sprintf('%0.2d', 70:98)) ~ 'Other diseases of the respiratory system',
                                 cod_causa_muerte_CIE10 %in% paste0('I',sprintf('%0.2d', 60:69)) ~ 'Cerebrovascular disease',
                                 cod_causa_muerte_CIE10 %in% paste0('D',sprintf('%0.2d', 00:48)) ~ 'Neoplasms',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 00:00)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 05:05)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 20:36)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 42:44)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 48:49)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 54:79)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 81:82)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 85:85)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('A',sprintf('%0.2d', 86:99)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 01:04)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 06:09)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 25:49)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 cod_causa_muerte_CIE10 %in% paste0('B',sprintf('%0.2d', 55:99)) ~ 'Other and unspecified infectious and parasitic diseases',
                                 TRUE ~ "Others"))

#icd10groups %>% filter(anio_def=="2020" & ICD10_groups!="Others") %>% group_by(ICD10_groups) %>% summarize(Deaths=sum(cantidad)) %>% arrange(-Deaths)
t2022<-icd10groups %>% filter(anio_def=="2022" & ICD10_groups!="Others") %>% group_by(ICD10_groups) %>% summarize(Deaths=sum(cantidad)) %>% arrange(-Deaths)
#Add rank column named "Rank 2021"
t2022$Rank_2022<-rank(-t2022$Deaths); t2022 <- t2022[,c(3,1,2)]
#icd10groups %>% filter((anio_def=="2020" | anio_def=="2021") & ICD10_groups!="Others") %>% group_by(ICD10_groups) %>% summarize(Deaths=sum(cantidad)) %>% arrange(-Deaths)
infecciosas_resp<-c("Influenza and pneumonia (J09-J18)","COVID-19 (U07)","Chronic lower respiratory diseases (J40-J47)","Infections with a predominantly sexual mode of transmission (A50-A64)","Tuberculosis (A15-A19)","Other bacterial diseases (A20-A49)","Other diseases of the respiratory system (J00-J06,J30-J39,J67,J70-J98)","Other acute lower respiratory infections (J20-J22)","Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)")
t2022infresp<-icd10groups %>% filter(ICD10_groups %in% infecciosas_resp & anio_def=="2022" & ICD10_groups!="Others") %>% group_by(ICD10_groups) %>% summarize(Deaths=sum(cantidad)) %>% arrange(-Deaths)
t2022infresp$Rank_2022<-rank(-t2022infresp$Deaths); t2022infresp <- t2022infresp[,c(3,1,2)]
#write_csv(t2021,file="0_14_tabla_mortalidad_2021.csv")
#write_csv(t2021infresp,file="0_14_tabla_mortalidad_2021_inf_resp.csv")



my_theme <- function(size=10) {
  color.background = "white"
  theme_light(base_size=size) +

    # Format background colors
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    theme(legend.position = "none")+
    theme(text = element_text(family = "Times New Roman")) 
}
ggtext_size <- function(base_size, ratio = 0.8) {
  ratio * base_size / ggplot2::.pt
}


#define bumpplot ggplot function
bumpplot <- function(data=data) {
ggplot(data = data, aes(x = anio_def, y = rank, group = ICD10_groups)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line(aes(color = ICD10_groups, alpha = 1), size = 2) +
  geom_point(aes(color = ICD10_groups, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_continuous()+
  scale_y_reverse(limits=c(10,1),breaks = 1:show.top.n.ranks) +
  scale_x_continuous(breaks = 2015:2022, minor_breaks = NULL, expand = c(.05, .05),limits=c(2012.5,2023.5)) +
  geom_label(family = "Times New Roman",data = data %>% filter(anio_def == 2015),
             aes(label = ICD10_groups, x = 2015-.15, y = rank, fill = ICD10_groups), 
             size= ggtext_size(8),color="white",hjust = 1, fontface = "bold", label.padding = unit(0.2, "lines"), label.size = 0.2) +
  geom_label(family = "Times New Roman",data = data %>% filter(anio_def == 2022),
             aes(label = ICD10_groups, x = 2022+.15, y = rank, fill = ICD10_groups), 
             size= ggtext_size(8),color="white",hjust = 0, fontface = "bold", label.padding = unit(0.2, "lines"), label.size = 0.2) +
    coord_cartesian(ylim = c(show.top.n.ranks,1)) + 
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Rank") +
  my_theme(size=10)
}

bumpplotnocode <- function(data=data) {
ggplot(data = data, aes(x = anio_def, y = rank, group = ICD10_groups_nocode)) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  geom_line(aes(color = ICD10_groups_nocode, alpha = 1), size = 2) +
  geom_point(aes(color = ICD10_groups_nocode, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_continuous()+
  scale_y_reverse(limits=c(10,1),breaks = 1:show.top.n.ranks) +
  scale_x_continuous(breaks = 2015:2022, minor_breaks = NULL, expand = c(.05, .05),limits=c(2012.5,2023.5)) +
  geom_label(family = "Times New Roman",data = data %>% filter(anio_def == 2015),
             aes(label = ICD10_groups_nocode, x = 2015-.15, y = rank, fill = ICD10_groups), 
             size= ggtext_size(8),color="white",hjust = 1, fontface = "bold", label.padding = unit(0.2, "lines"), label.size = 0.2) +
  geom_label(family = "Times New Roman",data = data %>% filter(anio_def == 2022),
             aes(label = ICD10_groups_nocode, x = 2022+.15, y = rank, fill = ICD10_groups), 
             size= ggtext_size(8),color="white",hjust = 0, fontface = "bold", label.padding = unit(0.2, "lines"), label.size = 0.2) +
    coord_cartesian(ylim = c(show.top.n.ranks,1)) + 
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Rank") +
  my_theme(size=10)
}

yearly_all<-icd10groups %>% filter(ICD10_groups!="Others") %>% group_by(anio_def,ICD10_groups) %>% summarize(Deaths=sum(cantidad)) %>%
#create new column with ranking of each group
mutate(rank = rank(-Deaths)) %>% arrange(-Deaths)
#count unique values in yearly_all$anio_def
yearly_all$ICD10_groups_nocode <- yearly_all$ICD10_groups
yearly_all$ICD10_groups <- gsub(",", ", ", yearly_all$ICD10_groups)
yearly_all$ICD10_groups <- stringr::str_wrap(yearly_all$ICD10_groups, 23)
show.top.n.ranks <- 10
years<-length(unique(yearly_all$anio_def))
pal<-scico(20, palette = 'batlow')
pal <- darken(pal, 0.2)
bumpplot(yearly_all %>% filter(anio_def>=2015))
#ggsave("0_14_mortality_bumpplot.png", width = 154, height = 154, units = "mm", dpi = 300)
yearly_all$ICD10_groups_nocode <- gsub("\\s\\(.*", "", yearly_all$ICD10_groups_nocode)
yearly_all$ICD10_groups_nocode <- stringr::str_wrap(yearly_all$ICD10_groups_nocode, 23)
bumpplotnocode(yearly_all %>% filter(anio_def>=2015))
#ggsave("0_14_mortality_bumpplot_nocode.png", width = 154, height = 77*1.5, units = "mm", dpi = 300)


#Get only respiratory and infectious diseases
yearly_all$ICD10_groups<-factor(yearly_all$ICD10_groups)
yearly_inf<-icd10groups %>% filter(ICD10_groups %in% infecciosas_resp & ICD10_groups!="Others") %>% group_by(anio_def,ICD10_groups) %>% summarize(Deaths=sum(cantidad)) %>% mutate(rank = rank(-Deaths)) %>% arrange(-Deaths)
show.top.n.ranks <- 10
pal<-scico(9, palette = 'batlow')
pal <- darken(pal, 0.2)
#Change yearly_inf$ICD10_groups from "Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)" to "Other and unspecified infectious and parasitic diseases (see legend for ICD10 codes)"
yearly_inf[yearly_inf$ICD10_groups=="Other and unspecified infectious and parasitic diseases and their sequelae (A00,A05,A20-A36,A42-A44,A48-A49,A54-A79,A81-A82,A85,A86-B04,B06-B09,B25-B49,B55-B99)",]$ICD10_groups <-"Other and unspecified infectious and parasitic diseases (see legend for ICD10 codes)"
yearly_inf$ICD10_groups_nocode <- yearly_inf$ICD10_groups
yearly_inf$ICD10_groups <- gsub(",", ", ", yearly_inf$ICD10_groups)
yearly_inf$ICD10_groups <- stringr::str_wrap(yearly_inf$ICD10_groups, 23)
bumpplot(yearly_inf %>% filter(anio_def>=2015))
#ggsave("0_14_mortality_inf_bumpplot.png", width = 154, height = 154, units = "mm", dpi = 300)
yearly_inf$ICD10_groups_nocode <- gsub("\\s\\(.*", "", yearly_inf$ICD10_groups_nocode)
yearly_inf$ICD10_groups_nocode <- stringr::str_wrap(yearly_inf$ICD10_groups_nocode, 23)
bumpplotnocode(yearly_inf %>% filter(anio_def>=2015))
#ggsave("0_14_mortality_inf_bumpplot_nocode.png", width = 154, height = 77*1.5, units = "mm", dpi = 300)
ggsave("Fig3.png", width = 154, height = 77*1.5, units = "mm", dpi = 300)
ggsave("Fig3.svg", width = 154, height = 77*1.5, units = "mm", dpi = 300)


#Generate table of deaths by group
table_yearly_inf <- yearly_inf %>%
  group_by(ICD10_groups, anio_def) %>%
  summarize(Deaths = sum(Deaths)) %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
  arrange(anio_def, ICD10_groups) %>%
  pivot_wider(names_from = anio_def, values_from = Deaths,values_fill = 0)

#Write excel file
library(xlsx)
#write_excel(table_yearly_inf,file="Supp_Table_1.xlsx")
write.xlsx(table_yearly_inf, file = "Supp_Table_1.xlsx", sheetName = "Sheet1", append = TRUE, rowNames = FALSE)


#Generate table of deaths by group
table_yearly_all <- yearly_all %>%
  group_by(ICD10_groups, anio_def) %>%
  summarize(Deaths = sum(Deaths)) %>%
  mutate(Deaths = ifelse(is.na(Deaths), 0, Deaths)) %>%
  arrange(anio_def, ICD10_groups) %>%
  pivot_wider(names_from = anio_def, values_from = Deaths,values_fill = 0)

  
#write_excel_csv(table_yearly_all,file="Supp_Table_2.xlsx")
write.xlsx(table_yearly_all, file = "Supp_Table_2.xlsx", sheetName = "Sheet1", append = TRUE, rowNames = FALSE)