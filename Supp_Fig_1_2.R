library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(grid)

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
if (!file.exists("./Data/grouped_df2.gz")) {
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
    group_by(grupo_etario, nombre_dosis_generica, fecha_aplicacion,jurisdiccion_residencia) %>%
    summarise(count = n()) %>%
    ungroup()

  write_csv(grouped_df, "./Data/grouped_df2.gz")
}

#grouped_df<-read_csv("./Data/grouped_df2.gz")
grouped_df<-read_csv("./Data/vacnew.zip")
grouped_df <- grouped_df %>% ungroup() %>% filter(EDAD_ANIOS %in% c("0-2", "3-4", "5-11", "12-17"))
names(grouped_df)[1]<-"jurisdiccion_residencia"
names(grouped_df)[6]<-"EDAD"
names(grouped_df)[8]<-"nombre_dosis_generica"
grouped_df <- grouped_df %>%
mutate(MES = recode(MES,   Enero = 01,   Febrero = 02,   Marzo = 03,   Abril = 04,   Mayo = 05,   Junio = 06,    Julio = 07,   Agosto = 08,   Septiembre = 09,   Octubre = 10,   Noviembre = 11,  Diciembre = 12 ))
#Create date column from ANIO and MES
grouped_df$fecha_aplicacion<- ymd(paste0(grouped_df$ANIO,"-",grouped_df$MES,"-01"))
#Change age group to 0-2, 3-11, 12-17
grouped_df<-grouped_df %>% mutate(grupo_etario = case_when(EDAD == "0-2" ~ "0-2",
                                           EDAD == "3-4" | EDAD == "5-11" ~ "3-11",
                                           EDAD == "12-17" ~ "12-17")) 
grouped_df$grupo_etario<-as.factor(grouped_df$grupo_etario)
grouped_df$nombre_dosis_generica<-as.factor(grouped_df$nombre_dosis_generica)
grouped_df$jurisdiccion_residencia<-as.factor(grouped_df$jurisdiccion_residencia)
grouped_df <- grouped_df %>% group_by(jurisdiccion_residencia,grupo_etario,nombre_dosis_generica,fecha_aplicacion) %>% summarise(count = sum(CANTIDAD, na.rm = TRUE))
levels(grouped_df$nombre_dosis_generica)<-c("1ra","2da","3ra o más")


# Create a new dataframe that contains all possible combinations of jurisdiccion_residencia, grupo_etario, nombre_dosis_generica, and dates within the range of the fecha_aplicacion column in datos
jurisdiccion_residencia <- unique(grouped_df$jurisdiccion_residencia)
grupo_etario <- unique(grouped_df$grupo_etario)
nombre_dosis_generica <- unique(grouped_df$nombre_dosis_generica)
date_df <- expand.grid(jurisdiccion_residencia, grupo_etario, nombre_dosis_generica, fecha_aplicacion = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month"))

# Rename columns in date_df to match grouped_df
colnames(date_df) <- c("jurisdiccion_residencia", "grupo_etario", "nombre_dosis_generica", "fecha_aplicacion")

# Merge the new dataframe with the grouped and cumulative sum dataframe, filling in missing values with 0
final_df <- date_df %>%
  left_join(grouped_df, by = c("jurisdiccion_residencia", "grupo_etario", "nombre_dosis_generica", "fecha_aplicacion")) %>%
  replace_na(list(count = 0, cumulative_count = 0))

# For each group, create a new column that contains the cumulative sum of the number of rows for each day
final_df <- final_df %>%
  group_by(jurisdiccion_residencia, grupo_etario, nombre_dosis_generica) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  ungroup()

final_df$jurisdiccion_residencia<-as.factor(final_df$jurisdiccion_residencia)
poblacion5$jurisdiccion_residencia<-as.factor(poblacion5$jurisdiccion_residencia)

#Modify poblacion5$jurisdiccion_residencia levels to match final_df$jurisdiccion_residencia levels using factor
levels(poblacion5$jurisdiccion_residencia)<-c("Buenos Aires", "Catamarca","Chaco","Chubut",       "CABA",         "Córdoba",        "Corrientes",       "Entre Ríos",         "Formosa",            "Jujuy",               "La Pampa",            "La Rioja",           "Mendoza",             "Misiones",            "Neuquén",            "Río Negro",           "Salta",               "San Juan",           "San Luis",            "Santa Cruz",          "Santa Fe",           "Santiago del Estero", "Tierra del Fuego",    "Tucumán")

final_df2 <- final_df %>%
  left_join(poblacion5, by = c("jurisdiccion_residencia", "grupo_etario")) %>%
  mutate(cumulative_count_perc = cumulative_count / poblacion * 100)
#Change NA to 0 for poblacion and cumulative_count_perc



#Change factor levels of nombre_dosis_generica
final_df2b<-final_df2
final_df2b$nombre_dosis_generica <- as.factor(final_df2b$nombre_dosis_generica)
levels(final_df2b$nombre_dosis_generica)<- c("1+ doses", "2+ doses", "3+ doses")

nodose<-final_df2b %>% filter(nombre_dosis_generica=="1+ doses") %>% mutate(nombre_dosis_generica="0 dose")
final_df2b<-rbind(final_df2b,nodose) %>% arrange(fecha_aplicacion,grupo_etario)
#change the pct_vac value for nombre_dosis_generica== "0 dose" to 100-pct_vac
final_df2b<-final_df2b %>% mutate(cumulative_count_perc=ifelse(nombre_dosis_generica=="0 dose",100-cumulative_count_perc,cumulative_count_perc))
#Limit upper end of vaccinated to 100. Since we consider static population numbers, the total number of vaccinated per age group can exceed population
final_df2b<-final_df2b %>% mutate(cumulative_count_perc=ifelse(cumulative_count_perc>100,100,cumulative_count_perc))
final_df2b<-final_df2b %>% mutate(cumulative_count_perc=ifelse(cumulative_count_perc<0,0,cumulative_count_perc))
# Create the plots
plot <- ggplot(final_df2b %>% filter(grupo_etario=="0-2"), aes(x = fecha_aplicacion, y = cumulative_count_perc, color = nombre_dosis_generica)) +
  geom_line() +
  facet_wrap(~jurisdiccion_residencia, ncol = 4) +
  labs(x = "Date", y = "Cumulative % of population", color = "Doses recieved") +
  ggtitle("Pediatric vaccination coverage by province for 0-2 year old age group") +
  #scale_y limits the y axis to 0-100
  scale_y_continuous(limits = c(0, 100)) +
  #Adds x axis labels for every 6 months as %Y-%m, rotated 90 degrees
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "Times"), axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  #scale_color_manual(values = c("0 dose" = "#D55E00","1 dose" = "#E69F00", "2 doses" = "#009E73", "2+ doses" = "#009E73", "3+ doses" = "#56B4E9"))
    #Okabe-Ito colorblind friendly palette
  scale_color_manual(values = c("0 dose" = "#000000", "1+ doses" = "#e69f00", "2+ doses" = "#56b4e9", "3+ doses" = "#0071b2"))
plot
#save plot to file
ggsave("Supp_Fig1A.png", plot, width = 12, height = 8, units = "in", dpi = 300)

plot <- ggplot(final_df2b %>% filter(grupo_etario=="3-11"), aes(x = fecha_aplicacion, y = cumulative_count_perc, color = nombre_dosis_generica)) +
  geom_line() +
  facet_wrap(~jurisdiccion_residencia, ncol = 4) +
  labs(x = "Date", y = "Cumulative % of population", color = "Doses recieved") +
  ggtitle("Pediatric vaccination coverage by province for 3-11 year old age group") +
  #scale_y limits the y axis to 0-100
  scale_y_continuous(limits = c(0, 100)) +
  #Adds x axis labels for every 6 months as %Y-%m, rotated 90 degrees
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "Times"), axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  #scale_color_manual(values = c("0 dose" = "#D55E00","1 dose" = "#E69F00", "2 doses" = "#009E73", "2+ doses" = "#009E73", "3+ doses" = "#56B4E9"))
    #Okabe-Ito colorblind friendly palette
  scale_color_manual(values = c("0 dose" = "#000000", "1+ doses" = "#e69f00", "2+ doses" = "#56b4e9", "3+ doses" = "#0071b2"))
plot
#save plot to file
ggsave("Supp_Fig1B.png", plot, width = 12, height = 8, units = "in", dpi = 300)

plot <- ggplot(final_df2b %>% filter(grupo_etario=="12-17"), aes(x = fecha_aplicacion, y = cumulative_count_perc, color = nombre_dosis_generica)) +
  geom_line() +
  facet_wrap(~jurisdiccion_residencia, ncol = 4) +
  labs(x = "Date", y = "Cumulative % of population", color = "Doses recieved") +
  ggtitle("Pediatric vaccination coverage by province for 12-17 year old age group") +
  #scale_y limits the y axis to 0-100
  scale_y_continuous(limits = c(0, 100)) +
  #Adds x axis labels for every 6 months as %Y-%m, rotated 90 degrees
  scale_x_date(date_breaks = "3 months", minor_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
  theme_bw() +
  theme(text = element_text(family = "Times"), axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
  #scale_color_manual(values = c("0 dose" = "#D55E00","1 dose" = "#E69F00", "2 doses" = "#009E73", "2+ doses" = "#009E73", "3+ doses" = "#56B4E9"))
  #Okabe-Ito colorblind friendly palette
  scale_color_manual(values = c("0 dose" = "#000000", "1+ doses" = "#e69f00", "2+ doses" = "#56b4e9", "3+ doses" = "#0071b2"))
plot
ggsave("Supp_Fig1C.png", plot, width = 12, height = 8, units = "in", dpi = 300)
















#For each jurisdiccion_residencia, grupo_etario, and nombre_dosis_generica, find cumulative_count_perc for date "2022-12-01"
final_df4 <- final_df2 %>%
  group_by(jurisdiccion_residencia, grupo_etario, nombre_dosis_generica) %>%
  filter(fecha_aplicacion == as.Date("2022-12-01")) %>%
  ungroup()



#STATIC MAP

create_map <- function(argentina_data,titulo="",mins=20,maxs=90) {
  # Merge data frame with shapefile
  # Create choropleth map
  ss <- ggplot() +
    geom_sf(data = argentina_data, aes(fill = cumulative_count_perc)) +
    scale_fill_gradientn(colours = hcl.colors(9, "RdBu", alpha = 0.9), limits = c(mins, maxs)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "longitude", y = "latitude") +
    labs(title = titulo, fill = "Percentage of\npopulation") +
    xlim(c(-73, -52)) +
    ylim(c(-56, -22)) +
    theme_light() +
    ggrepel::geom_label_repel(min.segment.length = 0.1, data = (argentina_data %>% filter(NAM != "Ciudad Autónoma de Buenos Aires")) %>%
                               sf::st_set_geometry(NULL) %>%
                               bind_cols(argentina_data %>% filter(NAM != "Ciudad Autónoma de Buenos Aires") %>%
                                           sf::st_centroid() %>% 
                                           sf::st_coordinates() %>% 
                                           as_tibble() %>%
                                           mutate(X = replace(X, round(X, 5) == -58.19661, -65.00000)) %>%
                                           mutate(Y = replace(Y, round(Y, 5) == -24.29797, -25.1)) %>% 
                                           mutate(Y = replace(Y, round(Y, 5) == -77.49525, -55.60000))),
                             family = "Times",
                             aes(label = NAM, x = X, y = Y)) +
    ggrepel::geom_label_repel(force = 0, min.segment.length = 10, data = (argentina_data %>% filter(NAM == "Ciudad Autónoma de Buenos Aires")) %>%
                               sf::st_set_geometry(NULL) %>%
                               bind_cols(argentina_data %>% filter(NAM == "Ciudad Autónoma de Buenos Aires") %>%
                                           sf::st_centroid() %>% 
                                           sf::st_coordinates() %>% 
                                           as_tibble() %>%
                                           mutate(X = replace(X, round(X, 5) == -58.44591, -55.00000)) %>%
                                           mutate(Y = replace(Y, round(Y, 5) == -34.61448, -39.00000))),
                             family = "Times",
                             aes(label = "Ciudad Autónoma\nde Buenos Aires", x = X, y = Y)) +
    theme(text = element_text(family = "Times"), legend.position = "right", legend.direction = "vertical", legend.box = "vertical")
  
  # Print the choropleth map
  return(ss)
}


# Load shapefile of Argentina
unzip("./Data/provincias.zip")
argentina <- st_read("./provincias.shp")
# Load data frame with values to map
data <- final_df4 %>% arrange(jurisdiccion_residencia) %>% filter(grupo_etario=="0-2" & nombre_dosis_generica=="2da" & fecha_aplicacion <=as.Date('2023-01-01')) 
mins=0;maxs=20
levels(data$jurisdiccion_residencia)[2] <- "Ciudad Autónoma de Buenos Aires"
levels(data$jurisdiccion_residencia)[23] <- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
argentina_data <- argentina %>% left_join(data, by = c("NAM" = "jurisdiccion_residencia"))
  
  # Create inset map for Ciudad de Buenos Aires
  inset <- argentina_data %>% filter(NAM == "Ciudad Autónoma de Buenos Aires") %>%
    ggplot() + 
    geom_sf(aes(fill = cumulative_count_perc)) +
    scale_fill_gradientn(colours = hcl.colors(9, "RdBu", alpha = 0.9), limits = c(mins, maxs)) +
    labs(x = NULL, y = NULL) +
    theme_test() +
    theme(text = element_text(family = "Times New Roman"), legend.position = "none", axis.ticks = element_blank(), axis.text = element_blank()) +
    coord_sf(expand = FALSE)
# Create map for under 12 year olds
ss<-create_map(argentina_data,titulo="0-2 year olds vaccinated with at least 2 doses as of December 2022",mins=mins,maxs=maxs)
vp <- viewport()
sbvp <- viewport(0.57, 0.408, width = 0.15, height = 0.15)
pdf("Supp_Fig_2A.pdf", width = 15, height = 10); print(ss) ; print(inset, vp=sbvp); dev.off()

#Create map for 3-11 year olds
data <- final_df4 %>% arrange(jurisdiccion_residencia) %>% filter(grupo_etario=="3-11" & nombre_dosis_generica=="2da" & fecha_aplicacion <=as.Date('2023-01-01')) 
mins=0;maxs=90
levels(data$jurisdiccion_residencia)[2] <- "Ciudad Autónoma de Buenos Aires"
levels(data$jurisdiccion_residencia)[23] <- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
argentina_data <- argentina %>% left_join(data, by = c("NAM" = "jurisdiccion_residencia"))
  
  # Create inset map for Ciudad de Buenos Aires
  inset <- argentina_data %>% filter(NAM == "Ciudad Autónoma de Buenos Aires") %>%
    ggplot() + 
    geom_sf(aes(fill = cumulative_count_perc)) +
    scale_fill_gradientn(colours = hcl.colors(9, "RdBu", alpha = 0.9), limits = c(mins, maxs)) +
    labs(x = NULL, y = NULL) +
    theme_test() +
    theme(text = element_text(family = "Times New Roman"), legend.position = "none", axis.ticks = element_blank(), axis.text = element_blank()) +
    coord_sf(expand = FALSE)
# Create map for under 12 year olds
ss<-create_map(argentina_data,titulo="3-11 year olds vaccinated with at least 2 doses as of December 2022",mins=mins,maxs=maxs)
vp <- viewport()
sbvp <- viewport(0.57, 0.408, width = 0.15, height = 0.15)
pdf("Supp_Fig_2B.pdf", width = 15, height = 10); print(ss) ; print(inset, vp=sbvp); dev.off()

#Create map for 12-17 year olds
data <- final_df4 %>% arrange(jurisdiccion_residencia) %>% filter(grupo_etario=="12-17" & nombre_dosis_generica=="2da" & fecha_aplicacion <=as.Date('2023-01-01'))
levels(data$jurisdiccion_residencia)[2] <- "Ciudad Autónoma de Buenos Aires"
levels(data$jurisdiccion_residencia)[23] <- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"
argentina_data <- argentina %>% left_join(data, by = c("NAM" = "jurisdiccion_residencia"))
  # Create inset map for Ciudad de Buenos Aires
  inset <- argentina_data %>% filter(NAM == "Ciudad Autónoma de Buenos Aires") %>%
    ggplot() + 
    geom_sf(aes(fill = cumulative_count_perc)) +
    scale_fill_gradientn(colours = hcl.colors(9, "RdBu", alpha = 0.9), limits = c(mins, maxs)) +
    labs(x = NULL, y = NULL) +
    theme_test() +
    theme(text = element_text(family = "Times New Roman"), legend.position = "none", axis.ticks = element_blank(), axis.text = element_blank()) +
    coord_sf(expand = FALSE)
ss2<-create_map(argentina_data,titulo="12-17 year olds vaccinated with at least 2 doses as of December 2022",mins=mins,maxs=maxs)
vp <- viewport()
sbvp <- viewport(0.57, 0.408, width = 0.15, height = 0.15)
pdf("Supp_Fig_2C.pdf", width = 15, height = 10); print(ss2) ; print(inset, vp=sbvp); dev.off()


