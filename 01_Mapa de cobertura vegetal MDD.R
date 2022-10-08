Dis_MDD

library(raster)
library(tidyverse)
library(ggplot2)
library(sf)

Peru1  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Peru2  <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD  =  subset(Peru2 , NAME_1 == "Madre de Dios")

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Dis_MDD = st_read("SHP/Dis_MDD.shp")  %>% st_as_sf()
Dis_MD    <- st_transform(Dis_MDD ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Dis_Cober_MDD  = st_read("SHP/Dis_Cober_MDD.shp")  %>% st_as_sf()
Dis_Cober_MD  <- st_transform(Dis_Cober_MDD ,
                          crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))



library(ggspatial)

library(elevatr)
elev = get_elev_raster(Dis_MD, z=11)
plot(elev)
Poligo_alt    <- crop(elev, Dis_MD)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Dis_MD)


slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel


Geo_data       <-  rasterToPoints(elev)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

# load dependencies
library(biscale)


col=c("#E6A759","#D6842E", "#CD6F31",
      "#DA875B", "#89D79E", "#949956","#74A6A5", "#E0E080", "#689962", 
      "#7EDDD5", "#82E07F","#ACC38B", "#88BF6D", "#759E80", "#A1D9BE", 
      "#6AB9A2", "#B9DB85" ,"#E1E3A1", "#7AB388", "#85E2C1", "#569986",
      "#B8BA71", "#999973", "#C7C595", "#A4DADA", "#92C487", "#8BAD71")

scale_fill_viridis_d(name="Cobertura vegetal")
scale_fill_manual(values = col, name="Cobertura vegetal")+
  
bivariate_color_scale <- tibble(
    "3 - 3" = "#56106EFF", # high inequality, high income
    "2 - 3" = "#210C4AFF",
    "1 - 3" = "#000004FF", # low inequality, high income
    "3 - 2" = "#E35932FF",
    "2 - 2" = "#BB3754FF", # medium inequality, medium income
    "1 - 2" = "#89226AFF",
    "3 - 1" = "#FCFFA4FF", # high inequality, low income
    "2 - 1" = "#F9C932FF",
    "1 - 1" = "#F98C0AFF" # low inequality, low income
  ) %>%
  gather("group", "fill")

bivariate_color_scale %<>%
  separate(group, into = c("gini", "mean"), sep = " - ") %>%
  mutate(gini = as.integer(gini),
         mean = as.integer(mean))

legend <-ggplot() +
  geom_tile(data = bivariate_color_scale, mapping = aes(
    x = gini, y = mean, fill = fill)) +
  scale_fill_identity() +
  labs(x = "Colina Baja ⟶️",
       y = "Terraza alta ⟶️") +
  theme( axis.title = element_text(size = 6),
         axis.title.x=element_text(color="black"),
         axis.text = element_blank(),
         panel.background = element_rect(fill = "white"),
         axis.title.y=element_text(color="black")) +

  coord_fixed()

legend.grob <- ggplotGrob(legend)
  

MDDGG= ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data=Peru1, fill="white", color="black", size=0.1)+
  geom_sf(data=MDD, fill="gray", color="black", size=0.05)+
  geom_sf(data = Dis_MD, fill="black", size=0.1)+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  
  annotate(geom = "text", x = -71.5, y = -13.2, hjust = 0, vjust = 1, 
           label = "CUSCO",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -69.5, y = -13.5, hjust = 0, vjust = 1, 
           label = "PUNO",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -68.9, y = -11.5, hjust = 0, vjust = 1, angle=300,
           label = "BOLIVIA",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10.5, hjust = 0, vjust = 1, 
           label = "BRASIL",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -72, y = -10.5, hjust = 0, vjust = 1, 
           label = "UCAYALI",size = 3, family="serif", color = 
             "black",  fontface="italic")
MDDGG.grob <- ggplotGrob(MDDGG)

library(ggnewscale)
library(ggspatial)
Mapa = ggplot() +
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data= Dis_MD, fill=NA, color="black", size=0.8)+
  geom_sf(data = Dis_Cober_MD, aes(fill = CobVeg2013), color = "white", size = 0.1, alpha=0.4)+
  scale_fill_viridis_d(option = "inferno", name="Cobertura vegetal")+
  coord_sf(xlim = c(-72.27,-70.92067), ylim = c(-12.96288 ,-11.8114)) +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_classic()+
  theme(legend.position = c(0.1, 0.2),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, family="serif", face = "italic"),
        
        
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotation_custom(grob= legend.grob, xmin = -71.2, xmax = -71, ymin =-12, ymax=-11.8)+
    annotation_custom(grob= MDDGG.grob, xmin = -72.27, xmax = -71.9, ymin =-12.1, ymax=-11.78)+
  labs(color = '',  x = 'Longitud', y = 'Latitud',
       title = "Mapa de Cobertura Vegetal del",
       subtitle = "Distrito de Madre de Dios- Peru",
       caption = "Datos en ministerio del Ambiente \n@Ing. Gorky Florez")
Mapa
ggsave(plot = Mapa ,"Mapa de Cobertura.png", units = "cm", 
       width = 25,height = 25, dpi = 1200) 



























