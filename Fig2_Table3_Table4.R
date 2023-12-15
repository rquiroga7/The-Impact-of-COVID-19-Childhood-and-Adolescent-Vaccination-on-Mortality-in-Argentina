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
  mutate(grupo_etario= ifelse(EDAD_APERTURA<3 ,"0-2",ifelse(EDAD_APERTURA>2 & EDAD_APERTURA<12 ,"3-11","12-17") ) )
#Correct error in data
#CORRECT DATA ERRORS
#IDEVENTOCASO =  3616406, change FECHA_FALLECIMIENTO from 2020-10-19 to 2022-10-19
data$FECHA_FALLECIMIENTO[data$IDEVENTOCASO==3616406]<-as.Date("2022-10-19")
#IDEVENTOCASO =  3616406, change FECHA_FALLECIMIENTO from 2021-07-23 to 2022-07-23
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

# If grouped_df.gz does not exist:
if (!file.exists("./Data/grouped_df.gz")) {
  # Calculate number of vax and unvax for each day and age group
  datos <- read_csv("datos_nomivac_covid19_pediatrico.zip",
                    col_types = cols(
                      jurisdiccion_residencia = col_factor(),
                      grupo_etario = col_factor(),
                      fecha_aplicacion = col_date(format = "%Y-%m-%d"),
                      condicion_aplicacion = col_factor(),
                      orden_dosis = col_factor(),
                      nombre_dosis_generica = col_factor(),
                      id_persona_dw = col_factor()
                    ))

  # Change instances of nombre_dosis_generica which are "Adicional" to "Refuerzo"
  datos$nombre_dosis_generica[datos$nombre_dosis_generica == "Adicional"] <- "Refuerzo"
  datos$nombre_dosis_generica[datos$nombre_dosis_generica == "Unica"] <- "1ra"
  # Remove all instances of jurisdiccion_residencia which are "S.I." and the level from the factor
  datos <- datos %>% filter(jurisdiccion_residencia != "S.I.")
  datos$jurisdiccion_residencia <- droplevels(datos$jurisdiccion_residencia)

  # Create a new dataframe which only retains the earliest instance of the combination of id_persona_dw and nombre_dosis_generica
  datos <- datos %>%
    group_by(id_persona_dw, nombre_dosis_generica) %>%
    filter(fecha_aplicacion == min(fecha_aplicacion)) %>%
    ungroup()

  # Group the datos dataframe by grupo_etario, and nombre_dosis_generica
  grouped_df <- datos %>%
    group_by(grupo_etario, nombre_dosis_generica, fecha_aplicacion) %>%
    summarise(count = n()) %>%
    ungroup()

  write_csv(grouped_df, "./Data/grouped_df.gz")
}
grouped_df<-read_csv("./Data/grouped_df.gz",col_types = cols(
    grupo_etario = col_factor(),
    nombre_dosis_generica = col_factor(),
    fecha_aplicacion = col_date(format = "%Y-%m-%d"),
    count = col_integer()
  ))

# For each group, create a new column that contains the cumulative sum of the number of rows for each day
date_seq <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
grouped_df_complete <- grouped_df %>%
  complete(grupo_etario, nombre_dosis_generica, fecha_aplicacion = date_seq, fill = list(count = 0))

#Calculate number of vax for each dose, age_group and day  
vac_df_1ra <- grouped_df_complete %>%
  filter(nombre_dosis_generica == "1ra") %>%
  group_by(grupo_etario, nombre_dosis_generica) %>%
  arrange(fecha_aplicacion) %>%
  mutate(cumulative_1 = cumsum(count)) %>%
  ungroup()
vac_df_2da <- grouped_df_complete %>%
  filter(nombre_dosis_generica == "2da") %>%
  group_by(grupo_etario, nombre_dosis_generica) %>%
  arrange(fecha_aplicacion) %>%
  mutate(cumulative_2 = cumsum(count)) %>%
  ungroup()

#Join final_df_1ra and final_df_2da
vac_df3 <- left_join(vac_df_1ra, vac_df_2da, by = c("grupo_etario", "fecha_aplicacion")) %>% select(grupo_etario, fecha_aplicacion, count.x, cumulative_1, count.y, cumulative_2)
names(vac_df3)<-c("grupo_etario","fecha_aplicacion","1d_day","cumulative_1d","2d_day","cumulative_2d")
#Since almost no vaccination in 0-2 years, consider vaccinations in 0-11 to be in 3-11
#Change name of <12 age group to 3-11, remember these are factor levels
levels(vac_df3$grupo_etario)[1]<-"3-11"
#final_df2<-final_df %>% group_by(fecha_aplicacion, grupo_etario,nombre_dosis_generica) %>% summarize(cumulative_count = sum(cumulative_count))
#final_df3 <- final_df %>% filter(nombre_dosis_generica == "2da" | nombre_dosis_generica == "1ra") 


#Merge death_data5 and poblacion6
names(death_data5)<-c("grupo_etario","fecha_fallecimiento","d0","d1","d2plus","cum_0","cum_1","cum_2")
death_data6<-merge(death_data5,poblacion6,by="grupo_etario") %>% arrange(fecha_fallecimiento)
death_data7<-death_data6 %>% left_join(vac_df3,by = c("fecha_fallecimiento" = "fecha_aplicacion","grupo_etario"="grupo_etario")) %>% arrange(fecha_fallecimiento)
#sort death_data7c grupo_etario to "0-2","3-11","12-17"
death_data7$grupo_etario<-factor(death_data7$grupo_etario,levels=c("0-2","3-11","12-17"))
#Change grupo_etario levels to "0-2 years old","3-11 years old","12-17 years old"
levels(death_data7$grupo_etario)<-c("0-2 years old","3-11 years old","12-17 years old")

#death_data7b <- death_data7 %>% mutate(vax=ifelse(!is.na(cumulative_vax),cumulative_vax,0),unvax=ifelse(!is.na(poblacion) & !is.na(cumulative_vax),poblacion-cumulative_vax,poblacion))
death_data7b <- death_data7 %>% 
    filter(fecha_fallecimiento>="2022-01-01") %>%
    group_by(grupo_etario) %>%
    #incidence with 0 doses
    mutate(inc0=ifelse(cumulative_2d>0 &!is.na(cumulative_2d),100000*d0/(poblacion-cumulative_1d),100000*d0/poblacion)) %>%
    #incidence with 0 or 1 doses
    mutate(inc0b=ifelse(cumulative_2d>0 &!is.na(cumulative_2d),100000*(d0+d1)/(poblacion-cumulative_2d),100000*d0/poblacion)) %>%
    #incidence with 1 or more doses
    mutate(inc1 = ifelse(cumulative_1d>0 & !is.na(cumulative_1d),100000*(d1+d2plus)/(cumulative_1d),0)) %>%
        #incidence with exactly 1 dose
    mutate(inc1e = ifelse(cumulative_1d>0 & !is.na(cumulative_1d),100000*(d1)/(cumulative_1d),0)) %>%
    #incidence with 2 or more doses
    mutate(inc2 = ifelse(cumulative_2d>0 &!is.na(cumulative_2d),100000*d2plus/cumulative_2d,0)) %>%
    mutate(inc0_cum = cumsum(inc0)) %>%
    mutate(inc0b_cum = cumsum(inc0b)) %>%
    mutate(inc1_cum = cumsum(inc1)) %>%
    mutate(inc1e_cum = cumsum(inc1e)) %>%
    mutate(inc2_cum = cumsum(inc2)) 

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
mort_vac_plot<-ggplot(death_data7c %>% filter(vac =="0 dose" | (vac== "1 dose" & grupo_etario %in% c("12-17 years old","3-11 years old")) | (vac== "2+ doses" & grupo_etario %in% c("12-17 years old","3-11 years old")) ), aes(x = fecha_fallecimiento, y = cumulative_incidence, color = vac)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Cumulative deaths\nper 100k population", color = "COVID-19 Vaccine doses recieved") +
  #ggtitle("Cumulative deaths in 2022 by age group and vaccination status") +
  #facet for each grupo_etario
  facet_wrap(~grupo_etario) +
  theme_light(base_size=10)+
  #legend at bottom, rotate x axis marks 90 degrees
  theme(text = element_text(family = "Times New Roman"),strip.background = element_rect(fill="gray90",color="black",size=1),strip.text = element_text(face="bold", color="black"),plot.title = element_text(face="bold",hjust = 0.5),legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  labs(tag="B")+
    scale_color_manual(values = c("0 dose" = "#D55E00","1 dose" = "#E69F00", "2 doses" = "#009E73", "2+ doses" = "#009E73", "3+ doses" = "#56B4E9"))
  #Save png
#mort_vac_plot
#ggsave("cum_incidence_by_vac_status_0_1_2.png", width = 154, height = 77, units = "mm", dpi = 300)

#Create national vaccination plot

# Create a new dataframe that contains all possible combinations of jurisdiccion_residencia, grupo_etario, nombre_dosis_generica, and dates within the range of the fecha_aplicacion column in datos
date_range <- grouped_df %>%
  select(fecha_aplicacion) %>%
  summarize(start_date = min(fecha_aplicacion), end_date = max(fecha_aplicacion))

grupo_etario <- unique(grouped_df$grupo_etario)
nombre_dosis_generica <- unique(grouped_df$nombre_dosis_generica)

date_df <- expand.grid(grupo_etario, nombre_dosis_generica, fecha_aplicacion = seq(date_range$start_date, date_range$end_date, by = "day"))

# Rename columns in date_df to match grouped_df
colnames(date_df) <- c("grupo_etario", "nombre_dosis_generica", "fecha_aplicacion")



final_df <- date_df %>%
  left_join(grouped_df, by = c("grupo_etario", "nombre_dosis_generica", "fecha_aplicacion")) %>%
  replace_na(list(count = 0, cumulative_count = 0))

# For each group, create a new column that contains the cumulative sum of the number of rows for each day
final_df <- final_df %>%
  group_by(grupo_etario, nombre_dosis_generica) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()

#Join with poblacion7
poblacion7<-data.frame(grupo_etario=c("<12","12-17"),poblacion=c(7665412,4360513))
final_df2<-merge(final_df,poblacion7,by="grupo_etario") %>% arrange(fecha_aplicacion)
#Calculate percentage of population vaccinated
final_df2<-final_df2 %>% mutate(pct_vac=100*cumulative_count/poblacion)
#Change final_df2$grupo_etario factor level names to "3-11","12-17"
levels(final_df2$grupo_etario)<-c("0-11 years old","12-17 years old")
levels(final_df2$nombre_dosis_generica)<-c("2","3 or more","1","2","1")
#reorder nombre_dosis_generica factor levels to "1","2","3 or more"
final_df2$nombre_dosis_generica<-factor(final_df2$nombre_dosis_generica,levels=c("1","2","3 or more"))
levels(final_df2$nombre_dosis_generica) <- c("1 dose", "2 doses", "3+ doses")
# Create the plot
plot_vac <- ggplot(final_df2, aes(x = fecha_aplicacion, y = pct_vac, color = nombre_dosis_generica)) +
  geom_line(size=1) +
  facet_wrap(~grupo_etario) +
  #y scale limits 0,100
  scale_y_continuous(limits = c(0, 100)) +
  #limit x axis to 2023-01-01
    scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2023-01-01"))) +
  labs(x = "Date", y = "Percentage of population", color = "COVID-19 Vaccine doses recieved") +
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



#Write table of vaccinated and non vaccinated Deaths per year and age group

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
mutate(NOREPORTADO = ifelse(rowSums(.[, 13:31] == 1) == 0, 1, 0))

#Count number of rows with value 1 in each of columns 13-31
counts <- colSums(data4[, c(13:31,33)] == 1)
names<-colnames(data4[, c(13:31,33)])
counts_df <- data.frame(COMORBILIDAD = names, Count = counts,row.names = NULL) %>% arrange(-Count)

#Make list of IDEVENTOCASO
idfall2<-data2 %>% filter(FECHA_FALLECIMIENTO > as.Date("2020-01-01")) %>% dplyr::pull(IDEVENTOCASO)

#Count occurence of each value of COMORBILIDAD column in data2
counts2 <- data2 %>% filter(IDEVENTOCASO %in% idfall2)  %>% group_by(COMORBILIDAD) %>% summarise(count=n()) %>% arrange(-count)
#Modify NA with "NOREPORTADO"
counts2$COMORBILIDAD[is.na(counts2$COMORBILIDAD)]<-"NOREPORTADO"
counts2

counts_df$COMORBILIDAD<-c("None reported","Neurological disease","Oncological disease","No comorbidities","Congenital or acquired immunosupression","Preterm birth","Cardiac insufficiency","Obesity","Low body weight","Diabetes","Chronic Obstrucive Pulmonary Disease","Chronic renal insufficiency","Asthma","Previous bronchiolitis","Previous community-acquired pneumonia","Arterial hipertension","Acute dialisis","Chronic liver disease","Tuberculosis","Chronic dialisis")
counts2$COMORBILIDAD<-c("Other","None reported","No comorbidities","Previous community-acquired pneumonia","Chronic respiratory disease","Oncological disease","Neurological disease","Preterm birth","Chronic liver disease","Congenital or acquired immunosupression","Cardiac insufficiency","Chronic renal insufficiency","Tuberculosis")

#Merge and sum counts_df and counts2
counts3<-merge(counts_df,counts2,by="COMORBILIDAD",all=TRUE)
counts3[is.na(counts3)] <- 0 
counts3<- counts3%>% mutate(Count=Count+count) %>% arrange(-Count) %>% dplyr::select(COMORBILIDAD,Count) %>% filter(Count>0)
names(counts3)<-c("Comorbidity","Count")

library(openxlsx)
#write_csv(counts3,"Table4.csv")
write.xlsx(counts3,"Table4.xlsx")