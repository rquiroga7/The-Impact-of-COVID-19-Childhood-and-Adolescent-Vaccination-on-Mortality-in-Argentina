#Load vaccination data, produce Table 1
library(readxl)
library(dplyr)
library(readr)
data1 <- read_excel("./Data/vac1_2023.xlsx", col_types = c("text","text","text","text","numeric"))
names(data1)[4]<-"EDAD"
data1<-data1 %>% mutate(grupo_etario = case_when(EDAD == "<3" ~ "0-2",
                                           EDAD == "3-4" | EDAD == "5-11" ~ "3-11",
                                           EDAD == "12-17" ~ "12-17")) 
data1$grupo_etario<-as.factor(data1$grupo_etario)
data1 <- data1 %>% group_by(grupo_etario) %>% summarise(n1 = sum(CANTIDAD, na.rm = TRUE))
#Remove the NA row
data1<-data1[complete.cases(data1),]
data2 <- read_excel("./Data/vac2_2023.xlsx", col_types = c("text","text","text","text","numeric"))
names(data2)[4]<-"EDAD"
data2<-data2 %>% mutate(grupo_etario = case_when(EDAD == "<3" ~ "0-2",
                                           EDAD == "3-4" | EDAD == "5-11" ~ "3-11",
                                           EDAD == "12-17" ~ "12-17")) 
data2$grupo_etario<-as.factor(data2$grupo_etario)
data2 <- data2 %>% group_by(grupo_etario) %>% summarise(n2 = sum(CANTIDAD, na.rm = TRUE))
#Remove the NA row
data2<-data2[complete.cases(data2),]
data3 <- read_excel("./Data/vac3_2023.xlsx", col_types = c("text","text","text","text","numeric"))
names(data3)[4]<-"EDAD"
data3<-data3 %>% mutate(grupo_etario = case_when(EDAD == "<3" ~ "0-2",
                                           EDAD == "3-4" | EDAD == "5-11" ~ "3-11",
                                           EDAD == "12-17" ~ "12-17")) 
data3$grupo_etario<-as.factor(data3$grupo_etario)
data3 <- data3 %>% group_by(grupo_etario) %>% summarise(n3 = sum(CANTIDAD, na.rm = TRUE))
#Remove the NA row
data3<-data3[complete.cases(data3),]
poblacion <- read_csv("./Data/estructura_de_poblacion_identificada_residiendo_en_argentina.csv",
                      col_types = cols(
                        provincia_nombre = col_factor(),
                        edad_quinquenal = col_factor(),
                        cantidad = col_integer()
                      ))
# Sort the levels of poblacion$provincia_nombre and datos$jurisdiccion_residencia
levels(poblacion$provincia_nombre)[5]<-"CABA"
#Group poblacion by provincia_nombre and edad_quinquenal, and sum the cantidad column
poblacion2 <- poblacion %>%
  group_by(provincia_nombre, edad_quinquenal) %>%
  summarise(cantidad = sum(cantidad)) %>%
  ungroup()

names(poblacion2)<-c("jurisdiccion_residencia","grupo_etario","poblacion")
poblacion2<-poblacion2 %>% filter(grupo_etario=="0 a 4" | grupo_etario=="5 a 9" | grupo_etario=="10 a 14" | grupo_etario=="15 a 19")
#droplevels poblacion2$grupo_etario levels
poblacion2$grupo_etario<-droplevels(poblacion2$grupo_etario)
levels(poblacion2$grupo_etario)<-c("0-4","5-9","10-14","15-19")
#Create a poblacion3 dataframe where cantidad is estimated for the 0-11 age group and the 12-17 age group
poblacion3 <- poblacion2 %>%
  filter(grupo_etario == "0-4" | grupo_etario == "5-9" | grupo_etario == "10-14") %>%
  group_by(jurisdiccion_residencia) %>%
  summarise(poblacion = sum(ifelse(grupo_etario=="10-14",poblacion/5*2,ifelse(grupo_etario=="0-4",poblacion/5*2,poblacion)))) %>%
  mutate(grupo_etario = "3-11") 
poblacion4 <- poblacion2 %>%
    filter(grupo_etario=="10-14" | grupo_etario=="15-19" ) %>%
    group_by(jurisdiccion_residencia) %>%
    summarise(poblacion = sum(poblacion/5*3)) %>%
    mutate(grupo_etario = "12-17")
poblacion4b <- poblacion2 %>%
  filter(grupo_etario == "0-4") %>%
  group_by(jurisdiccion_residencia) %>%
  summarise(poblacion = sum(ifelse(grupo_etario=="0-4",poblacion/5*3))) %>%
  mutate(grupo_etario = "0-2") 

poblacion5<-bind_rows(poblacion3,poblacion4,poblacion4b)
poblacion6<-poblacion5 %>% group_by(grupo_etario) %>% summarise(poblacion=sum(poblacion))
poblacion6$grupo_etario<-as.factor(poblacion6$grupo_etario)
#Merge data1,data2,data3 and poblacion6 by grupo_etario

# Merge the data frames
merged_data <- Reduce(function(x, y) merge(x, y, by = "grupo_etario", all = TRUE), list(data1, data2, data3, poblacion6))
#Reorder grupo_etario levels
merged_data$grupo_etario<-factor(merged_data$grupo_etario,levels=c("0-2","3-11","12-17"))
# Calculate the doses
merged_data <- merged_data %>%
  mutate(`1 dose` = ifelse(!is.na(n1), n1/poblacion, 0),
         `2 doses` = ifelse(!is.na(n2), n2/poblacion, 0),
         `3+ doses` = ifelse(!is.na(n3), n3/poblacion, 0))


#Select columns output xlsx table
names(merged_data)[1]<-"Age Group (years)"
#Select columns output xslx table
table1<-merged_data %>% select('Age Group (years)',`1 dose`,`2 doses`,`3+ doses`)
#Round the values to percentages, add percentage sign 
table1[2:4]<-round(table1[2:4]*100,0)
table1[2:4] <- lapply(table1[2:4], function(x) paste0(x, "%"))

#Change row order to 1,3,2
table1<-table1[c(1,3,2),]
#load library
library(openxlsx)
write.xlsx(table1,"./Figures_Tables/Table1.xlsx")