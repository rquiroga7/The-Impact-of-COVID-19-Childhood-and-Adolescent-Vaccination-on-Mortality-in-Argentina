library(readxl)
library(extrafont)
library(ggplot2)
library(dplyr)
library(scales)
library(vistime)
library(patchwork)
library(highcharter)
library(cowplot)
#remotes::install_github("taylordunn/dunnr")
library(dunnr)
library(readr)
library(xlsx)
library(tidyverse)

#CARGO BASE

#Only run if data.zip is not in the working directory
if(!file.exists("./Data/data.gz")){
    data1 <- read_excel("covid_pediatricos_1.xlsx", col_types = c("text", "text", "numeric", "text", "text", "text", "date", "text", "text", "text", "text", rep("text", 20),"date"))
    data1$FECHA_FALLECIMIENTO<-as.Date(data1$FECHA_FALLECIMIENTO,format="%d/%m/%Y")
    data2 <- read_excel("covid_pediatricos_2.xlsx", col_types = c("text", "text", "numeric", "text", "text", "text", "date", "text", "text", "text", "date", "text","date","text"))
    data1b<-data1[,c(1,3,7,11,32)]
    data2b<-data2[,c(1,3,7,11,13)]
    names(data2b)<-names(data1b)
    data<-rbind(data1b,data2b)
    write_csv(data,"./Data/data.gz",col_names = TRUE)
}
data<-read_csv("./Data/data.gz")

#CORRECT TWO CASES WITH WRONG DEATH DATE, where death date was prior to vaccination date
#IDEVENTOCASO =  3616406, change FECHA_FALLECIMIENTO from 2020-10-19 to 2022-10-19
data$FECHA_FALLECIMIENTO[data$IDEVENTOCASO==3616406]<-as.Date("2022-10-19")
#IDEVENTOCASO =  16177350, change FECHA_FALLECIMIENTO from 2021-07-23 to 2022-07-23
data$FECHA_FALLECIMIENTO[data$IDEVENTOCASO==16177350]<-as.Date("2022-07-23")

#for each unique value of IDEVENTOCASO, get the row with a date in the column FECHA_FALLECIMIENTO, else get first row, do not break date format
datad <- data %>%
  group_by(IDEVENTOCASO) %>%
  mutate(FECHA_FALLECIMIENTO = ifelse(any(!is.na(FECHA_FALLECIMIENTO)), FECHA_FALLECIMIENTO[!is.na(FECHA_FALLECIMIENTO)][1], FECHA_FALLECIMIENTO[1])) %>%
  mutate(FECHA_FALLECIMIENTO = as.Date(FECHA_FALLECIMIENTO)) %>%
  ungroup() %>%
  distinct(IDEVENTOCASO, .keep_all = TRUE)
#create grupo_etario column using case_when function
datad<-datad %>% mutate(grupo_etario=case_when(EDAD_APERTURA<3 ~ "0-2",EDAD_APERTURA>=3 & EDAD_APERTURA<12 ~ "3-11",EDAD_APERTURA>=12 & EDAD_APERTURA<18 ~ "12-17"))

 #Count number of rows in datad by month, save to monthly_cases dataframe
monthly_deaths <- datad %>% 
 #filter only with deaths
   filter(!is.na(FECHA_FALLECIMIENTO)) %>%
   filter(FECHA_FALLECIMIENTO<="2022-12-31") %>%
   ungroup() %>% 
   group_by(grupo_etario) %>%
   mutate(Periodos = format(as.Date(FECHA_FALLECIMIENTO), "%Y-%m")) %>%
   distinct(IDEVENTOCASO, .keep_all = TRUE) %>%
   group_by(grupo_etario,Periodos) %>%
   #mutate FECHA_FALLECIMIENTO to the first day of the month
   count() %>%
   mutate(fecha=as.Date(paste0(Periodos,"-01")))  %>%
   ungroup() %>%
   #create all possible combinations of grupo_etario and fecha (first day of each month for 2020,2021 and 2022), using n=0
   complete(grupo_etario,fecha=seq.Date(as.Date("2020-03-01"),as.Date("2022-12-01"),by="month"),fill=list(n=0)) %>%
   #remove the Periodos column
   select(-Periodos)

  #yearly_deaths<-monthly_deaths %>% 
  #mutate(anio=format(fecha,"%Y")) %>%
  #group_by(grupo_etario,anio) %>%
  #summarise(n=sum(n))  

monthly_cases <- datad %>% 
  ungroup() %>% 
  filter(FECHA_APERTURA<="2022-12-31") %>%
  group_by(grupo_etario) %>%
  mutate(Periodos = format(as.Date(FECHA_APERTURA), "%Y-%m")) %>% 
  distinct(IDEVENTOCASO, .keep_all = TRUE) %>% 
  group_by(grupo_etario,Periodos) %>% 
  count() %>%
  mutate(fecha=as.Date(paste0(Periodos,"-01")))  




#Write xlsx file with monthly cases and deaths
monthly_cases_table<-monthly_cases[,1:3] %>% ungroup()
names(monthly_cases_table)<-c("Age group","Year-Month","Cases","Date")
write.xlsx(monthly_cases_table,"Supp_Table_3_monthly_cases.xlsx")
#SuppTable4 is now generated in the Fig2 script
#monthly_deaths_table<-monthly_deaths[,1:3] %>% ungroup()
#names(monthly_deaths_table)<-c("Age group","Year-Month","Deaths","Date")
#write.xlsx(monthly_deaths_table,"Supp_Table_4_monthly_deaths.xlsx")

#defino la paleta de colores para el grafico
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #toda la gama
#Use Okabe-Ito colorblind friendly palette https://jfly.uni-koeln.de/color/#pallet
#cbPalette <- c("#56B4E9", "#009E73", "#D55E00")
cbPalette <- c("#d55e00", "#009E73", "#F5C710")



#grafico barras apilado
deaths<-monthly_deaths %>%
  ggplot() +
  geom_col(aes(x=fecha, y=n, fill = factor(grupo_etario, levels= c("12-17","3-11","0-2")))) +
  geom_text(family="Times New Roman",aes(fecha, label_y, label = label_y), vjust = -1,
            data = . %>% group_by(fecha) %>% summarise(label_y = sum(n))) +
  scale_fill_manual(values=cbPalette) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", expand = c(0.01,0.01)) +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(xlim=c(as.Date("2020-03-01"),as.Date("2023-01-01")),ylim=c(0, 65))+
  #ggtitle("Deaths by month and age group") +
  theme_bw(base_size=10) +
  theme(text = element_text(family="Times New Roman",size = 14, face="bold"),axis.text=element_text(size=10),legend.position = c(0.90, 0.75),axis.text.x = element_blank()) +
  labs(x= "", y="Monthly deaths", fill="Age groups", tags="B")


cases<-monthly_cases %>%
  ggplot() +
  geom_col(aes(x=fecha, y=n, fill = factor(grupo_etario, levels= c("12-17","3-11","0-2")))) +
  geom_text(family="Times New Roman",aes(fecha, label_y, label = label_y), vjust = 0.5, hjust=0, angle=90, nudge_y = 2000,
            data = . %>% group_by(fecha) %>% summarise(label_y = sum(n))) +
  scale_fill_manual(values=cbPalette) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", expand = c(0.01,0.01)) +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(xlim=c(as.Date("2020-03-01"),as.Date("2023-01-01")),ylim=c(0, 250000))+
  #ggtitle("Deaths by month and age group") +
  theme_bw(base_size=10) + theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=1, size=10)) +
  theme(text = element_text(family="Times New Roman",size = 14, face="bold"),axis.text=element_text(size=10),legend.position = c(0.90, 0.75),axis.text.x = element_blank()) +  
  labs(x= "", y="Monthly cases", fill="Age groups",tag="A")


#linea de tiempo
timeline_data <- data.frame(event = c("Restricted access to testing",  "Widely Available Testing", "Limited Testing","Economic assistance to citizens","Economic assistance to companies","Remote Schooling", "On-site schooling with facemasks","On-site schooling",   "Partial School closure","Lockdown",  "Partial Lockdown", "Social distancing","12-17 year olds","3-11 year olds", "6 months - 2 year olds", "Gamma+Lambda", "Delta", "Omicron","BA.2","BA.4/BA.5","BQ.1*","Unknown"),
                            group = c("Testing",                      "Testing",                  "Testing",        "Social Assistance",              "Social Assistance",                "NPIs",             "NPIs",                            "NPIs",                "NPIs",                  "NPIs",      "NPIs",             "NPIs",             "Vaccination","Vaccination", "Vaccination", "Dominant Variants", "Dominant Variants", "Dominant Variants", "Dominant Variants", "Dominant Variants", "Dominant Variants","Dominant Variants"),
                            start = c("2022-04-01",                   "2020-11-01",               "2020-03-01",     "2020-03-23",                     "2020-04-01",                     "2020-03-16",       "2021-03-01",                      "2021-11-17",          "2021-04-19",            "2020-03-20","2020-06-08",       "2020-10-14",       "2021-07-08","2021-10-05", "2022-07-08", "2021-03-29", "2021-10-11", "2021-12-20","2022-04-11","2022-06-20","2022-11-07","2020-03-01"), 
                            end   = c("2022-12-31",                   "2022-03-31",               "2020-10-31",     "2020-08-30",                     "2020-12-31",                     "2021-02-28",       "2021-11-16",                      "2022-12-31",          "2021-04-30",            "2020-06-09","2020-10-15",       "2021-08-18",       "2023-01-01","2023-01-01", "2023-01-01", "2021-10-12", "2021-12-21","2022-04-12","2022-06-21","2022-11-08","2023-01-01","2021-03-30"),
                            #color = c("#CC79A7","#56B4E9", "#009E73", "#0072B2", "#D55E00", "#999999"), 
                            optimise_y = TRUE,
                            show_labels = FALSE,
                            linewidth = 5)
                            

p<-gg_vistime(timeline_data)+
    theme_bw(base_size=10) +
  theme(text = element_text(size = 10)) +
  scale_x_datetime(expand = c(0.01,0.01),date_breaks = "3 months", date_minor_breaks = "1 month", date_labels = "%Y-%m")+
  theme(text = element_text(family="Times New Roman",size = 14, face="bold"),axis.text = element_text(family="Times New Roman", face="bold", size=10))+
  labs(tag="C")

# Combine the plots using cowplot
set_geom_fonts(family="Times New Roman", ggrepel= TRUE)
fig1<-plot_grid(cases,deaths, p, ncol=1, align="v")
ggsave("Fig1.svg", width = 154, height = 154, units = "mm", dpi = 600, scale = 2)
ggsave("Fig1.png", width = 154, height = 154, units = "mm", dpi = 600, scale = 2)

