library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)

data1 <- read_excel("./Data/fall_1.xlsx", col_types = c("text", "text", "numeric", "text", "text", "text", "date", "text", "text", "text", "text", rep("text", 31)))
data1$FECHA_FALLECIMIENTO<-as.Date(data1$FECHA_FALLECIMIENTO,format="%d/%m/%Y")
data2 <- read_excel("./Data/fall_2.xlsx", col_types = c("text", "text", "numeric", "text", "text", "text", "date", "text", "text", "text", "date", rep("text", 15)))
data1<-data1 %>% mutate(vac=ifelse(REFUERZO!="" & !is.na(REFUERZO),"refuerzo",ifelse(SEGUNDA!="" & !is.na(SEGUNDA) ,"2da",ifelse(PRIMERA!="" & !is.na(PRIMERA) ,"1ra",""))))
data2<-data2 %>% mutate(vac=ifelse(REFUERZO!="" & !is.na(REFUERZO),"refuerzo",ifelse(SEGUNDA!="" & !is.na(SEGUNDA) ,"2da",ifelse(PRIMERA!="" & !is.na(PRIMERA) ,"1ra",""))))
data1b<-data1[,c(1,3,7,11,43)]
data2b<-data2[,c(1,3,7,11,27)]
names(data2b)<-names(data1b)


data<-rbind(data1b,data2b)
data$vac<-as.factor(data$vac)
#leave only rows with unique IDEVENTOCASO
data <- data %>% distinct(IDEVENTOCASO, .keep_all = TRUE) %>% 
  arrange(FECHA_FALLECIMIENTO) %>%
  filter(EDAD_APERTURA<18) %>%
  mutate(grupo_etario= ifelse(EDAD_APERTURA<3 ,"0-2",ifelse(EDAD_APERTURA>2 & EDAD_APERTURA<12 ,"3-11","12-17") ) )
#Correct error in data
#CORRECT DATA ERRORS (Vaccination date after death date)
#IDEVENTOCASO =  3616406, change FECHA_FALLECIMIENTO from 2020-10-19 to 2022-10-19
data$FECHA_FALLECIMIENTO[data$IDEVENTOCASO==3616406]<-as.Date("2022-10-19")
#IDEVENTOCASO =  16177350, change FECHA_FALLECIMIENTO from 2021-07-23 to 2022-07-23
data$FECHA_FALLECIMIENTO[data$IDEVENTOCASO==16177350]<-as.Date("2022-07-23")
data3<-data %>% group_by(grupo_etario,FECHA_FALLECIMIENTO) %>% summarise(count=n()) %>% complete(FECHA_FALLECIMIENTO = seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day"),fill = list(count = 0)) %>% arrange(FECHA_FALLECIMIENTO) %>% mutate(cumcount=cumsum(count))
death_data4<-data %>% mutate(vac=ifelse(vac=="2da" | vac=="refuerzo","d2plus",ifelse(vac=="1ra" ,"d1","d0"))) %>% group_by(vac,grupo_etario,FECHA_FALLECIMIENTO) %>% summarise(count=n()) %>% complete(data=.,FECHA_FALLECIMIENTO = seq.Date(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day"),fill = list(count = 0)) %>% arrange(FECHA_FALLECIMIENTO)
death_data4$vac<-as.factor(death_data4$vac)



#Take data4 columns vac, grupo_etario, FECHA_FALLECIMIENTO, and from cumcount create three columns, according to vac column value
death_data5 <- death_data4 %>%
  pivot_wider(names_from = vac, values_from = count) %>%
  #Replace NA values with 0
  replace(is.na(.), 0) %>%
  mutate(cum_0 = cumsum(d0), cum_1 = cumsum(d1),cum_2 = cumsum(d2plus))



# Create a plot of cumulative deaths by date for each age group
#ggplot(data3, aes(x = FECHA_FALLECIMIENTO, y = cumcount, color = grupo_etario)) +
#  geom_line() +
#  labs(x = "Date", y = "Cumulative deaths", color = "Age group") +
#  scale_color_manual(values = c("0-2" = "red", "3-11" = "blue", "12-17" = "green")) +
#  theme_light()

#Get population numbers
poblacion <- read_csv("./Data/estructura_de_poblacion_identificada_residiendo_en_argentina.csv",
                      col_types = cols(
                        provincia_nombre = col_factor(),
                        edad_quinquenal = col_factor(),
                        cantidad = col_integer()
                      ))


# Sort the levels of poblacion$provincia_nombre and datos$jurisdiccion_residencia
levels(poblacion$provincia_nombre)[5]<-"CABA"
#poblacion$provincia_nombre <- factor(poblacion$provincia_nombre, levels = sort(levels(poblacion$provincia_nombre)))
#datos$jurisdiccion_residencia <- factor(datos$jurisdiccion_residencia, levels = sort(levels(datos$jurisdiccion_residencia)))

#Replace poblacion$provincia_nombre factor level names with datos$jurisdiccion_residencia factor level names
#levels(poblacion$provincia_nombre) <- levels(datos$jurisdiccion_residencia)
#head(poblacion$provincia_nombre)

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

grouped_df<-read_csv("./Data/vacnew.zip",col_types = cols(
    PROVINCIA = col_character(),
    DEPARTAMENTO = col_character(),
    SEXO = col_character(),
    ANIO = col_integer(),
    MES = col_character(),
    EDAD_ANIOS = col_character(),
    CANTIDAD = col_integer(),
    DOSIS = col_character()
  ))
names(grouped_df)[6]<-"EDAD"
grouped_df <- grouped_df %>%
mutate(MES = recode(MES,
  Enero = 01,
  Febrero = 02,
  Marzo = 03,
  Abril = 04,
  Mayo = 05,
  Junio = 06,
  Julio = 07,
  Agosto = 08,
  Septiembre = 09,
  Octubre = 10,
  Noviembre = 11,
  Diciembre = 12
))


#Create date column from ANIO and MES
grouped_df$fecha_aplicacion<- ymd(paste0(grouped_df$ANIO,"-",grouped_df$MES,"-01"))
#Change age group to 0-2, 3-11, 12-17
grouped_df<-grouped_df %>% mutate(grupo_etario = case_when(EDAD == "0-2" ~ "0-2",
                                           EDAD == "3-4" | EDAD == "5-11" ~ "3-11",
                                           EDAD == "12-17" ~ "12-17")) 
grouped_df<-grouped_df[complete.cases(grouped_df),]
grouped_df$grupo_etario<-as.factor(grouped_df$grupo_etario)
grouped_df2 <- grouped_df %>% group_by(grupo_etario,DOSIS,fecha_aplicacion) %>% summarise(count = sum(CANTIDAD, na.rm = TRUE))

# For each group, create a new column that contains the cumulative sum of the number of vaccinated for each month
date_seq <- as.Date(seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "month"))

#complete grouped_df2 with all dates present in the date_seq vector
grouped_df2$fecha_aplicacion <- as.Date(grouped_df2$fecha_aplicacion)
grouped_df2$DOSIS <- as.factor(grouped_df2$DOSIS)
grouped_df2$grupo_etario <- as.factor(grouped_df2$grupo_etario)
grouped_df2$count <- as.numeric(grouped_df2$count)
grouped_df_complete <- grouped_df2 %>% ungroup() %>% tidyr::complete(grupo_etario, DOSIS, fecha_aplicacion = date_seq, fill = list(count = 0))
grouped_df_complete2<-grouped_df_complete %>% 
  filter(fecha_aplicacion<as.Date("2023-01-01")) %>%
  group_by(grupo_etario,DOSIS) %>%
  mutate(cumulative_1d = ifelse(DOSIS==1,cumsum(count),0)) %>%
  mutate(cumulative_2d = ifelse(DOSIS==2,cumsum(count),0)) %>%
  mutate(cumulative_3d = ifelse(DOSIS==3,cumsum(count),0)) %>%
  group_by(grupo_etario,fecha_aplicacion) %>%
  summarise(cumulative_1d = sum(cumulative_1d),
            cumulative_2d = sum(cumulative_2d),
            cumulative_3d = sum(cumulative_3d))

#Merge death_data5 and poblacion6
names(death_data5)<-c("grupo_etario","fecha_fallecimiento","d0","d1","d2plus","cum_0","cum_1","cum_2")
death_data6<-merge(death_data5,poblacion6,by="grupo_etario") %>% arrange(fecha_fallecimiento)

death_data7<-death_data6 %>% left_join(grouped_df_complete2,by = c("fecha_fallecimiento" = "fecha_aplicacion","grupo_etario"="grupo_etario")) %>% arrange(fecha_fallecimiento)
#sort death_data7c grupo_etario to "0-2","3-11","12-17"
death_data7$grupo_etario<-factor(death_data7$grupo_etario,levels=c("0-2","3-11","12-17"))
#Change grupo_etario levels to "0-2 years old","3-11 years old","12-17 years old"
levels(death_data7$grupo_etario)<-c("0-2 years old","3-11 years old","12-17 years old")

#death_data7b <- death_data7 %>% mutate(vax=ifelse(!is.na(cumulative_vax),cumulative_vax,0),unvax=ifelse(!is.na(poblacion) & !is.na(cumulative_vax),poblacion-cumulative_vax,poblacion))
death_data7b <- death_data7 %>% 
    filter(fecha_fallecimiento>="2022-01-01" & fecha_fallecimiento<"2023-01-01") %>%
    #Keep only rows with fecha_fallecimiento in date_seq
    filter(fecha_fallecimiento %in% date_seq) %>%
    group_by(grupo_etario) %>%
    mutate(bl0=min(cum_0),bl1=min(cum_1),bl2=min(cum_2)) %>%
    #incidence with 0 doses
    mutate(inc0_cum=ifelse(cumulative_1d>0,100000*(cum_0-bl0)/(poblacion-cumulative_1d),100000*d0/poblacion)) %>%
    #incidence with 0 or 1 doses
    mutate(inc0b_cum=ifelse(cumulative_2d>0,100000*(cum_0+cum_1-bl0-bl1)/(poblacion-cumulative_2d),100000*d0/poblacion)) %>%
    #incidence with 1 or more doses
    mutate(inc1_cum = ifelse(cumulative_1d>0,100000*(cum_1+cum_2-bl1-bl2)/(cumulative_1d),0)) %>%
        #incidence with exactly 1 dose
    mutate(inc1e_cum = ifelse(cumulative_1d>0,100000*(cum_1-bl1)/(cumulative_1d),0)) %>%
    #incidence with 2 or more doses
    mutate(inc2_cum = ifelse(cumulative_2d>0 &!is.na(cumulative_2d),100000*(cum_2-bl2)/cumulative_2d,0))

#Pivot wide to long. Columns inc0_cum, inc1_cum, inc2_cum are now in a single column, and the column names are in a new column called vac.
death_data7c <- death_data7b %>%
  filter(fecha_fallecimiento>="2022-01-01" & fecha_fallecimiento<="2022-12-31") %>%
  #pivot_longer(cols = c(d0, d1, d2plus), names_to = "vac1", values_to = "daily_deaths") %>%
  #pivot_longer(cols = c(cum_0, cum_1, cum_2), names_to = "vac2", values_to = "cum_deaths") %>%
  #pivot_longer(cols = c(inc0, inc1, inc2), names_to = "vac3", values_to = "daily_incidence") %>%
  pivot_longer(cols = c(inc0_cum, inc0b_cum, inc1_cum, inc1e_cum, inc2_cum), names_to = "vac", values_to = "cumulative_incidence") %>%
  mutate(vac = factor(vac, levels = c("inc0_cum", "inc0b_cum", "inc1e_cum", "inc1_cum", "inc2_cum"), labels = c("0 dose", "0-1 dose","1 dose", "1+ doses", "2+ doses"))) %>%
  select( grupo_etario, fecha_fallecimiento,vac,cumulative_incidence) 




#Cumulative death plot by vaccination status
mort_vac_plot<-ggplot(death_data7c %>% filter(vac == "0 dose" | vac == "1 dose" | vac == "2+ doses"), aes(x = fecha_fallecimiento, y = cumulative_incidence, color = vac)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Cumulative deaths\nper 100k population", color = "COVID-19 Vaccine doses recieved") +
  #ggtitle("Cumulative deaths in 2022 by age group and vaccination status") +
  #facet for each grupo_etario
  facet_wrap(~grupo_etario) +
  scale_y_continuous(limits = c(0, 4),breaks = seq(0, 4, 0.5)) +
  scale_x_date(date_labels = "%b-%Y",date_breaks = "1 month",limits = c(as.Date("2022-01-01"), as.Date("2022-12-01"))) +
  theme_light(base_size=10)+
  #legend at bottom, rotate x axis marks 90 degrees
  theme(text = element_text(family = "Times New Roman"),strip.background = element_rect(fill="gray90",color="black",size=1),strip.text = element_text(face="bold", color="black"),plot.title = element_text(face="bold",hjust = 0.5),legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  labs(tag="B")+
  scale_color_manual(values = c("0 dose" = "#D55E00","1 dose" = "#E69F00", "2 doses" = "#009E73", "2+ doses" = "#009E73", "3+ doses" = "#56B4E9"))
  #Save png
#mort_vac_plot
#ggsave("cum_incidence_by_vac_status_0_1_2.png", width = 154, height = 77, units = "mm", dpi = 300)

#Join with poblacion7
final_df2<-merge(grouped_df_complete2,poblacion6,by="grupo_etario") %>% arrange(fecha_aplicacion)
#Calculate percentage of population vaccinated
final_df2<-final_df2 %>% mutate(pct_1=100*cumulative_1d /poblacion,
                                pct_2=100*cumulative_2d /poblacion,
                                pct_3=100*cumulative_3d /poblacion
                                )
#Change final_df2$grupo_etario factor level names and order to "0-2 years old", "3-11 years old","12-17 years old"
final_df2$grupo_etario<-factor(final_df2$grupo_etario,levels=c("0-2","3-11","12-17"))
levels(final_df2$grupo_etario)<-c("0-2 years old","3-11 years old","12-17 years old")
#pivot wide to long
final_df2<-final_df2 %>% pivot_longer(cols = c(pct_1, pct_2, pct_3), names_to = "nombre_dosis_generica", values_to = "pct_vac")
#Change factor level names to "1","2","3 or more"
final_df2$nombre_dosis_generica<-as.factor(final_df2$nombre_dosis_generica)
levels(final_df2$nombre_dosis_generica)<-c("1 dose","2 doses","3+ doses")

# Create the plot
plot_vac <- ggplot(final_df2, aes(x = fecha_aplicacion, y = pct_vac, color = nombre_dosis_generica)) +
  geom_line(size=1) +
  facet_wrap(~grupo_etario) +
  #y scale limits 0,100
  #limit x axis to 2023-01-01
  scale_x_date(date_labels = "%b-%Y",date_breaks = "1 month",limits = c(as.Date("2022-01-01"), as.Date("2022-12-01"))) +
  #set y scale breaks to 10
  scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, 20)) +
  labs(x = "Date", y = "Population vaccinated (%)", color = "COVID-19 Vaccine doses recieved") +
  #ggtitle("Pediatric COVID-19 vaccination coverage for each age group") +
  theme_light(base_size=10)+
  #legend at bottom, rotate x axis marks 90 degrees
  theme(text = element_text(family = "Times New Roman"),strip.background = element_rect(fill="gray90",color="black",size=1),strip.text = element_text(face="bold", color="black"),plot.title = element_text(hjust = 0.5),legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  labs(tag="A")+
  scale_color_manual(values = c("0 dose" = "#D55E00","1 dose" = "#E69F00", "2 doses" = "#009E73", "2+ doses" = "#009E73", "3+ doses" = "#56B4E9"))
#plot_vac
#ggsave("vaccination_plot.png", width = 154, height = 77, units = "mm", dpi = 300)

fig2<-plot_grid(plot_vac,mort_vac_plot, ncol=1, align="v")

ggsave("Fig2.png", width = 154, height = 75, units = "mm", dpi = 300, scale = 2)
ggsave("Fig2.svg", width = 154, height = 75, units = "mm", dpi = 300, scale = 2)

#Write Table 1
#Select columns output xlsx table
merged_data<-final_df2 %>% filter(fecha_aplicacion == as.Date("2022-12-01"))
#pivot long to wide
merged_data<-merged_data %>% pivot_wider(names_from = nombre_dosis_generica, values_from = pct_vac)
names(merged_data)[1]<-"Age Group (years)"
#Select columns output xslx table
table1<-merged_data %>% select('Age Group (years)',`1 dose`,`2 doses`,`3+ doses`)
#Round the values to percentages, add percentage sign 
table1[2:4]<-round(table1[2:4],0)
table1[2:4] <- lapply(table1[2:4], function(x) paste0(x, "%"))
#Change row order to 1,3,2
table1<-table1[c(1,3,2),]
#load library
library(openxlsx)
write.xlsx(table1,"Table1.xlsx")

#Write Table 3 of vaccinated and non vaccinated Deaths per year and age group
death_data8 <- death_data7 %>%
filter(fecha_fallecimiento<="2022-12-31") %>%
  mutate(year = year(fecha_fallecimiento)) %>%
  group_by(grupo_etario, year) %>%
  mutate(novac_annual = cumsum(d0) + cumsum(d1), vac_annual = cumsum(d2plus)) %>%
  summarise(novac_annual = max(novac_annual), vac_annual = max(vac_annual)) %>%
  pivot_longer(cols = c(novac_annual, vac_annual), names_to = "vac", values_to = "deaths") %>%
  mutate(vac = factor(vac, levels = c("novac_annual", "vac_annual"), labels = c("Non vaccinated", "Vaccinated")))

death_data8b<-death_data8 %>% pivot_wider(names_from = c(vac,grupo_etario), values_from = deaths)
#Create kable table with year values as rows and three groups of columns according to grupo_etario values
library(kableExtra)

colnames(death_data8b)<-c("Year",rep(c("Non Vaccinated Deaths","Vaccinated Deaths"),3))
# Create a table from death_data8b using Kable
kable_out <- kable(death_data8b, caption = "Deaths by year, age group, vaccination status") %>%
  add_header_above(c(" " = 1, "0-2 years old" = 2, "3-11 years old" = 2, "12-17 years old" = 2)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
readr::write_file(kable_out, "Table3.html")


###Create table 4, comorbilities
idfall1<-data1 %>% filter(FECHA_FALLECIMIENTO > as.Date("2020-01-01")) %>% dplyr::pull(IDEVENTOCASO)
data4<-data1 %>% 
filter(IDEVENTOCASO %in% idfall1) %>%
#If the sum of columns 13:31 equals 0, then SIN_COMORBILIDAD =1, else SIN_COMORBILIDAD = 0
mutate(NOREPORTADO = ifelse(rowSums(.[, c(13:31)] >= 1) == 0, 1, 0)) %>%
#create new column named COMORBILIDAD, which is the name of the column with value 1, 
mutate(COMORBILIDAD = names(.[, c(13:31)])[max.col(.[, c(13:31)])]) %>%
#in case of more than 1 column with value 1, then COMORBILIDAD = "MULTIPLE"
mutate(COMORBILIDAD = ifelse(rowSums(.[, c(13:27,29,30,31)] >= 1) > 1, "MULTIPLE", COMORBILIDAD)) %>%
#IF SIN_COMORBILIDAD = 1, then COMORBILIDAD = "NOREPORTADO", else COMORBILIDAD=COMORBILIDAD
mutate(COMORBILIDAD = ifelse(NOREPORTADO == 1, "NOREPORTADO", COMORBILIDAD)) 

data4m<-data4 %>% filter(IDEVENTOCASO %in% idfall1) %>% dplyr::select(IDEVENTOCASO,COMORBILIDAD)
data4m$COMORBILIDAD<-case_when(data4m$COMORBILIDAD=="NOREPORTADO" ~ "None reported",
                              data4m$COMORBILIDAD=="OTRA" ~ "Other",
                              data4m$COMORBILIDAD=="ENF_ONCO_PREVIA" ~ "Oncological disease",
                              data4m$COMORBILIDAD=="ENF_NEURO_PREVIA" ~ "Neurological disease",
                              data4m$COMORBILIDAD=="NAC_PREVIA" ~ "Previous community-acquired pneumonia",
                              data4m$COMORBILIDAD=="PREMATURO" ~ "Preterm birth",
                              data4m$COMORBILIDAD=="SIN_COMORB" ~ "No comorbidities",
                              data4m$COMORBILIDAD=="RESPIRATORIA_CRONICA" ~ "Chronic respiratory disease",
                              data4m$COMORBILIDAD=="HEPATO_CRONICA" ~ "Chronic liver disease",
                              data4m$COMORBILIDAD=="INMUNOS_CONGENITA" ~ "Congenital or acquired immunosuppression",
                              data4m$COMORBILIDAD=="INSF_CARDIACA" ~ "Cardiac insufficiency",
                              data4m$COMORBILIDAD=="INSF_RENAL" ~ "Chronic renal insufficiency",
                              data4m$COMORBILIDAD=="TBC" ~ "Tuberculosis",
                              data4m$COMORBILIDAD=="MULTIPLE" ~ "Multiple comorbidities",
                              data4m$COMORBILIDAD=="ASMA" ~ "Asthma",
                              data4m$COMORBILIDAD=="DIALISIS_AGUDA" ~ "Dialisis",
                              data4m$COMORBILIDAD=="DIALISIS_CRONICA" ~ "Dialisis",
                              data4m$COMORBILIDAD=="BAJO_PESO" ~ "Low body weight",
                              data4m$COMORBILIDAD=="OBESIDAD" ~ "Obesity",
                              data4m$COMORBILIDAD=="DBT" ~ "Diabetes",
                              data4m$COMORBILIDAD=="HTA" ~ "Arterial hypertension",
                              data4m$COMORBILIDAD=="EPOC" ~ "Chronic Obstructive Pulmonary Disease",
                              data4m$COMORBILIDAD=="BRONQUIO_PREVIA" ~ "Previous bronchiolitis"
                              )
#Make list of IDEVENTOCASO
idfall2<-data2 %>% filter(FECHA_FALLECIMIENTO > as.Date("2020-01-01")) %>% dplyr::pull(IDEVENTOCASO)
data2m<-data2 %>% filter(IDEVENTOCASO %in% idfall2) %>% dplyr::select(IDEVENTOCASO,COMORBILIDAD)
data2m$COMORBILIDAD[is.na(data2m$COMORBILIDAD)]<-"NOREPORTADO"
#If multiple rows have the same IDEVENTOCASO, but different COMORBILIDAD values, then COMORBILIDAD = "MULTIPLE"
data2m<-data2m %>% group_by(IDEVENTOCASO) %>% mutate(COMORBILIDAD=ifelse(n()>1,"MULTIPLE",COMORBILIDAD)) %>% dplyr::select(IDEVENTOCASO,COMORBILIDAD)
#Use case_when to translate COMORBILIDAD factor levels
data2m$COMORBILIDAD<-case_when(data2m$COMORBILIDAD=="NOREPORTADO" ~ "None reported",
                                data2m$COMORBILIDAD=="Otras (especificar en observaciones)" ~ "Other",
                                data2m$COMORBILIDAD=="Enfermedad oncológica" ~ "Oncological disease",
                                data2m$COMORBILIDAD=="Enfermedad neurológica crónica" ~ "Neurological disease",
                                data2m$COMORBILIDAD=="N.A.C. previa" ~ "Previous community-acquired pneumonia",
                                data2m$COMORBILIDAD=="Prematuridad" ~ "Preterm birth",
                                data2m$COMORBILIDAD=="Sin comorbilidades" ~ "No comorbidities",
                                data2m$COMORBILIDAD=="Enfermedad respiratoria crónica" ~ "Chronic respiratory disease",
                                data2m$COMORBILIDAD=="Hepatopatía crónica" ~ "Chronic liver disease",
                                data2m$COMORBILIDAD=="Inmunosupresión congénita o adquirida" ~ "Congenital or acquired immunosupression",
                                data2m$COMORBILIDAD=="Insuficiencia cardíaca" ~ "Cardiac insufficiency",
                                data2m$COMORBILIDAD=="Insuficiencia renal crónica" ~ "Chronic renal insufficiency",
                                data2m$COMORBILIDAD=="Tuberculosis" ~ "Tuberculosis",
                                data2m$COMORBILIDAD=="MULTIPLE" ~ "Multiple comorbidities"
                                )

#Merge data2m and data4m
datam<-rbind(data2m,data4m)
#View(datam %>% group_by(IDEVENTOCASO) %>% filter(n()>1) %>% arrange(IDEVENTOCASO))
datam <- datam %>% distinct(IDEVENTOCASO,COMORBILIDAD)
#Count occurrences of COMORBILIDAD in datam
counts2 <- datam %>%
  group_by(COMORBILIDAD) %>%
  summarise(count = n()) %>% 
  arrange(-count)

names(counts2)<-c("Comorbidity","Count")

library(openxlsx)
#write_csv(counts2,"Table4.csv")
write.xlsx(counts2,"Table4.xlsx")