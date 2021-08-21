library(classInt)
library(MLmetrics) 

#This code processes the final results and produces the visualisations presented in the report

#define carbon factors
gas_cf <- 0.18316
electricity_cf<-0.21233 

#read the buildings in westminster and get a count of properties
buildings <- st_read('Data/BuildingData/FinalBuildings.geojson') 
countofproperties <- buildings %>% dplyr::select(.,fid,NUMPOINTS) %>% st_drop_geometry()

#read westminster
westminster <-  st_read(here::here("Data/GeoData/westminster.shp")) %>% st_transform(., 27700)

#read domestic and non domestic geojson files
domestic <- st_read("Results/domestic_consumption.geojson")
domestic <- domestic[!is.na(domestic$electricity_consumption), ]#remove missing values
domestic <- domestic[!is.na(domestic$gas_consumption), ]#remove missing values

non_domestic <- st_read("Results/Non_Domestic_Consumption.geojson") 
non_domestic <- non_domestic[!is.na(non_domestic$electricity_consumption), ]#remove missing values
non_domestic <- non_domestic[!is.na(non_domestic$gas_consumption), ]#remove missing values

#get count of non-domestic properties from non domestic file
non_domestic <- non_domestic[!duplicated(non_domestic),]
no_double_uprn <- non_domestic[!duplicated(non_domestic$UPRN),]
non_domestic_count <- no_double_uprn %>% count(fid) %>% st_set_geometry(.,NULL) %>% rename(.,'non_domestic'='n')

domestic_counts <- domestic %>% count(fid) %>% st_set_geometry(.,NULL) %>% rename(.,'domestic'='n')

#merge with count of properties
countofproperties <- dplyr::left_join(countofproperties,non_domestic_count,by='fid')
countofproperties <- dplyr::left_join(countofproperties,domestic_counts,by='fid')
countofproperties[is.na(countofproperties)] <-  0
#remove rows where both doemstic and non domestic are 0
countofproperties <- dplyr::filter(countofproperties, domestic > 0|non_domestic>0)

#calculate difference
countofproperties$difference <- countofproperties$NUMPOINTS-countofproperties$domestic-countofproperties$non_domestic
#where the difference is negative, replace with 0, as these are most likely addresses slightly mismatched or non-domestic properties with EPCs
countofproperties[countofproperties < 0] <- 0 

#now allocate missing properties based on majority of known properties
countofproperties <- countofproperties %>% mutate(non_domestic_new=case_when(
  non_domestic>domestic~non_domestic+difference,
  non_domestic<domestic~non_domestic,
  non_domestic==domestic~non_domestic+difference
  
))

countofproperties <- countofproperties %>% mutate(domestic_new=case_when(
  domestic>non_domestic~domestic+difference,
  domestic<non_domestic~domestic,
  domestic==non_domestic~domestic
))

#now, we can allocate final counts to domestic and non_domestic
domestic_with_counts <- dplyr::left_join(domestic,countofproperties,by='fid') %>% dplyr::filter(., domestic_new !=0)

#merge domestic with counts and remove 0s as these are most likely addresses wrongly matched or non-domestic properties with EPCs
domestic_with_counts <- dplyr::left_join(domestic,countofproperties,by='fid') %>% dplyr::filter(., domestic !=0)

domestic_counts <-domestic_with_counts %>% as.data.frame() %>% group_by(fid) %>% tally() %>% rename(.,countofrecords=n)

DomesticEnergy_with_counts <- left_join(domestic_with_counts,domestic_counts, by='fid')

#separate out into two different dataframes, depending if count==countofproperties
match <- DomesticEnergy_with_counts %>% dplyr::filter(., countofrecords==domestic_new)
nomatch <- DomesticEnergy_with_counts %>% dplyr::filter(., countofrecords!=domestic_new)

#group by building and then take the sum
match <- match %>% group_by(fid) %>% summarise_at(vars("gas_consumption", "electricity_consumption",'tfa'), sum) 

#group by building and calculate median - then merge again with count of properties
nomatch <- nomatch %>% group_by(fid) %>% summarise_at(vars("gas_consumption","electricity_consumption",'tfa'),median)
nomatch <- dplyr::left_join(nomatch,countofproperties,by='fid')

#then multiply median with the number of properties in that building 
nomatch$electricity_consumption <- nomatch$electricity_consumption*nomatch$domestic_new
nomatch$gas_consumption <- nomatch$gas_consumption*nomatch$domestic_new
nomatch$tfa <- nomatch$tfa*nomatch$domestic_new
nomatch <- dplyr::select(nomatch,fid,gas_consumption,electricity_consumption,tfa,geometry)

#create new dataframe with total consumption for each building polygon 
domestic_total_consumption <- dplyr::bind_rows(match,nomatch)
domestic_total_consumption$gas_intensity <- domestic_total_consumption$gas_consumption/domestic_total_consumption$tfa
domestic_total_consumption$electricity_intensity <- domestic_total_consumption$electricity_consumption/domestic_total_consumption$tfa
domestic_total_consumption <- domestic_total_consumption %>% rename(.,'totalarea'='tfa') 

#----------------------------Non-domestic-------------------------------------------------------
nondomestic_with_counts <- dplyr::left_join(non_domestic,countofproperties,by='fid') %>% dplyr::filter(., non_domestic_new !=0)

non_domestic_counts <-nondomestic_with_counts %>% as.data.frame() %>% group_by(fid) %>% tally() %>% rename(.,countofrecords=n)

NonDomesticEnergy_with_counts <- left_join(nondomestic_with_counts,non_domestic_counts, by='fid')

#separate out into two different dataframes, depending if count==countofproperties
ndmatch <- NonDomesticEnergy_with_counts %>% dplyr::filter(., countofrecords==non_domestic_new)
ndnomatch <- NonDomesticEnergy_with_counts %>% dplyr::filter(., countofrecords!=non_domestic_new)

#group by building and then take the sum
ndmatch <- ndmatch %>% group_by(fid) %>% summarise_at(vars("gas_consumption", "electricity_consumption",'totalarea'), sum) 

#group by building and calculate median - then merge again with count of properties
ndnomatch <- ndnomatch %>% group_by(fid) %>% summarise_at(vars("gas_consumption","electricity_consumption",'totalarea'),median)
ndnomatch <- dplyr::left_join(ndnomatch,countofproperties,by='fid')

#then multiply median with the number of properties in that building 
ndnomatch$electricity_consumption <- ndnomatch$electricity_consumption*ndnomatch$non_domestic_new
ndnomatch$gas_consumption <- ndnomatch$gas_consumption*ndnomatch$non_domestic_new
ndnomatch$totalarea <- ndnomatch$totalarea*ndnomatch$non_domestic_new
ndnomatch <- dplyr::select(ndnomatch,fid,gas_consumption,electricity_consumption,totalarea,geometry)

#create new dataframe with total consumption for each building polygon 
non_domestic_total_consumption <- dplyr::bind_rows(ndmatch,ndnomatch)
non_domestic_total_consumption$gas_intensity <- non_domestic_total_consumption$gas_consumption/non_domestic_total_consumption$totalarea
non_domestic_total_consumption$electricity_intensity <- non_domestic_total_consumption$electricity_consumption/non_domestic_total_consumption$totalarea

#---------------------------Combined----------------------------------------------
total <- dplyr::bind_rows(domestic_total_consumption,non_domestic_total_consumption) %>% 
  group_by(fid) %>% 
  summarise_at(vars("gas_consumption", "electricity_consumption",'totalarea'), sum)


total$gas_intensity <- total$gas_consumption/total$totalarea
total$electricity_intensity <- total$electricity_consumption/total$totalarea

#--------------------First, let's create some choropleth maps---------------------------

#Domestic gas:
#classify
class <- classIntervals(domestic_total_consumption$gas_intensity, n = 5, style = "fisher")
class <- replace(class, c(1), 0)
domestic_total_consumption <- domestic_total_consumption %>%
  mutate(gasi_class = cut(gas_intensity, class$brks, include.lowest = T))


domestic_gas_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = domestic_total_consumption,
          aes(fill = gasi_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "YlOrRd",na.value = NA,
                    name = "Gas Use Intensity"~(kWh/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

domestic_gas_intensity

save_plot('plots/domestic_gas_intensity.png',domestic_gas_intensity)

#Plot histogram legend
histogram <- domestic_total_consumption %>%
  ggplot(aes(x=gas_intensity)) +
  geom_histogram(binwidth=1,aes(fill = as.factor(gasi_class)),color=NA)+
  scale_fill_brewer(name = "Average distance deciles",palette = "YlOrRd")+
  geom_vline(xintercept = class$brks-0.5, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Gas Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 250)) + 
  scale_x_continuous("Gas Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))


Final <- domestic_gas_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

save_plot('plots/domestic_gas_intensity_histogram.png',Final)

#Domestic electricity
#classify
class <- classIntervals(domestic_total_consumption$electricity_intensity, n = 5, style = "fisher")
class <- replace(class, c(1), 0)
domestic_total_consumption <- domestic_total_consumption %>%
  mutate(eleci_class = cut(electricity_intensity, class$brks, include.lowest = T))



domestic_electricity_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = domestic_total_consumption,
          aes(fill = eleci_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "YlOrRd",na.value = NA,
                    name = "Electricity Use Intensity"~(kWh/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

domestic_electricity_intensity

save_plot('plots/domestic_electricity_intensity.png',domestic_electricity_intensity)

#Plot histogram legend
histogram <- domestic_total_consumption %>%
  ggplot(aes(x=electricity_intensity)) +
  geom_histogram(binwidth=0.5,aes(fill = as.factor(eleci_class)),color=NA)+
  scale_fill_brewer(palette = "YlOrRd")+
  geom_vline(xintercept = class$brks, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Electricity Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 300)) + 
  scale_x_continuous("Electricity Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))

histogram


Final <- domestic_electricity_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

Final

save_plot('plots/domestic_electricity_intensity_histogram.png',Final)

#---------------------------------------Non-domestic------------------------------------------------------------
non_domestic_total_consumption <- non_domestic_total_consumption[!is.na(non_domestic_total_consumption$electricity_consumption), ]
non_domestic_total_consumption <- non_domestic_total_consumption[!is.na(non_domestic_total_consumption$gas_consumption), ]

#classify
class <- classIntervals(non_domestic_total_consumption$gas_intensity, n = 5, style = "fisher")
class <- replace(class, c(1), 0)
non_domestic_total_consumption <- non_domestic_total_consumption %>%
  mutate(gasi_class = cut(gas_intensity, class$brks, include.lowest = T))



non_domestic_gas_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = non_domestic_total_consumption,
          aes(fill = gasi_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "YlOrRd",na.value = NA,
                    name = "Gas Use Intensity"~(kWh/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

non_domestic_gas_intensity

save_plot('plots/nondomestic_gas_intensity.png',non_domestic_gas_intensity)

#Plot histogram legend
histogram <- non_domestic_total_consumption %>%
  ggplot(aes(x=gas_intensity)) +
  geom_histogram(binwidth=1,aes(fill = as.factor(gasi_class)),color=NA)+
  scale_fill_brewer(name = "Average distance deciles",palette = "YlOrRd")+
  geom_vline(xintercept = class$brks-0.5, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Gas Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 40)) + 
  scale_x_continuous("Gas Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))
histogram

Final <- non_domestic_gas_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

save_plot('plots/nondomestic_gas_intensity_histogram.png',Final)

#Electricity
#classify
class <- classIntervals(non_domestic_total_consumption$electricity_intensity, n = 5, style = "fisher")
class <- replace(class, c(1), 0)
non_domestic_total_consumption <- non_domestic_total_consumption %>%
  mutate(eleci_class = cut(electricity_intensity, class$brks, include.lowest = T))

non_domestic_electricity_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = non_domestic_total_consumption,
          aes(fill = eleci_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "YlOrRd",na.value = NA,
                    name = "Electricity Use Intensity"~(kWh/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

non_domestic_electricity_intensity

save_plot('plots/nondomestic_electricity_intensity.png',non_domestic_electricity_intensity)

#Plot histogram legend
histogram <- non_domestic_total_consumption %>%
  ggplot(aes(x=electricity_intensity)) +
  geom_histogram(binwidth=0.5,aes(fill = as.factor(eleci_class)),color=NA)+
  scale_fill_brewer(palette = "YlOrRd")+
  geom_vline(xintercept = class$brks, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Electricity Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 25)) + 
  scale_x_continuous("Electricity Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))

histogram


Final <- non_domestic_electricity_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

Final

save_plot('plots/nondomestic_electricity_intensity_histogram.png',Final)

#------------------------------------------total----------------------------------------------
total <- total[!is.na(total$electricity_consumption), ]
total <- total[!is.na(total$gas_consumption), ]

#classify
class <- classIntervals(total$gas_intensity, n = 5, style = "fisher")
class <- replace(class, c(1), 0)
total <- total %>%
  mutate(gasi_class = cut(gas_intensity, class$brks, include.lowest = T))



total_gas_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = total,
          aes(fill = gasi_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "YlOrRd",na.value = NA,
                    name = "Gas Use Intensity"~(kWh/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

total_gas_intensity

save_plot('plots/total_gas_intensity.png',total_gas_intensity)

#Plot histogram legend
histogram <- total %>%
  ggplot(aes(x=gas_intensity)) +
  geom_histogram(binwidth=1,aes(fill = as.factor(gasi_class)),color=NA)+
  scale_fill_brewer(name = "Average distance deciles",palette = "YlOrRd")+
  geom_vline(xintercept = class$brks-0.5, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Gas Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 300)) + 
  scale_x_continuous("Gas Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))
histogram

Final <- total_gas_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

save_plot('plots/total_gas_intensity_histogram.png',Final)


#classify
class <- classIntervals(total$electricity_intensity, n = 5, style = "fisher")
class <- replace(class, c(1), 0)
total <- total %>%
  mutate(eleci_class = cut(electricity_intensity, class$brks, include.lowest = T))



total_electricity_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = total,
          aes(fill = eleci_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "YlOrRd",na.value = NA,
                    name = "Electricity Use Intensity"~(kWh/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

total_electricity_intensity

save_plot('plots/total_electricity_intensity.png',total_electricity_intensity)

#Plot histogram legend
histogram <- total %>%
  ggplot(aes(x=electricity_intensity)) +
  geom_histogram(binwidth=1,aes(fill = as.factor(eleci_class)),color=NA)+
  scale_fill_brewer(palette = "YlOrRd")+
  geom_vline(xintercept = class$brks, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Electricity Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 900)) + 
  scale_x_continuous("Electricity Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))

histogram


Final <- total_electricity_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

Final

save_plot('plots/total_electricity_intensity_histogram.png',Final)


#Calculate total carbon intensity
total$carbon_intensity <- gas_cf*total$gas_intensity + electricity_cf*total$electricity_intensity
total$carbon_emissions <- total$carbon_intensity*total$totalarea

#get total carbon emissiosn for borough
sum(total$carbon_emissions)

#classify
class <- classIntervals(total$carbon_intensity, n = 7, style = "fisher")
class <- replace(class, c(1), 0)
total <- total %>%
  mutate(carbon_class = cut(carbon_intensity, class$brks, include.lowest = T))


#plot total carbon intensity
total_carbon_intensity <- ggplot() +
  geom_sf(data=westminster, color=NA, fill='gray68')+
  geom_sf(data = total,
          aes(fill = carbon_class),
          alpha = 0.8,
          colour = NA,
          size = 0.3) +
  scale_fill_brewer(palette = "Oranges",na.value = NA,
                    name = "Carbon Intensity"~~(kg~CO[2]~e/m^2)) +
  theme_map() +
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))+
  coord_sf(datum = NA)

total_carbon_intensity

save_plot('plots/total_carbon_intensity.png',total_carbon_intensity)

#Plot histogram legend
histogram <- total %>%
  ggplot(aes(x=electricity_intensity)) +
  geom_histogram(binwidth=1,aes(fill = as.factor(eleci_class)),color=NA)+
  scale_fill_brewer(palette = "YlOrRd")+
  geom_vline(xintercept = class$brks, linetype="dashed")+
  guides(colour = guide_legend(nrow =1))+
  xlab("Electricity Use Intensity"~(kWh/m^2))+
  ylab("Count")+
  theme_classic()+
  theme(legend.position="none")+
  scale_y_continuous("Count", expand = c(0, 0), 
                     limits = c(0, 900)) + 
  scale_x_continuous("Electricity Use Intensity"~(kWh/m^2), expand = c(0,0))+
  theme(axis.text=element_text(size=7))+
  theme(axis.title=element_text(size=9))

histogram


Final <- total_electricity_intensity + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")+ 
  coord_sf(xlim = c(523843.7, 534396.1), ylim = c(176847.3, 183993.8)) +
  #adding the histogram
  annotation_custom(grob = ggplotGrob(histogram), ymin=176897.3, ymax=179947.3,xmin=530350,xmax=534396.1)

Final

save_plot('plots/total_electricity_intensity_histogram.png',Final)

#---------------------------------Hexbin plot-------------------------------------------

points <- total %>% st_point_on_surface()

#create a grid
westminster_grid <- st_make_grid(westminster,
                                 n = c(50,50),
                                 what = 'polygons',
                                 square = FALSE,
                                 flat_topped = TRUE) %>%
  st_as_sf() %>%
  mutate(id=row_number())

#do a spatial join
test <- st_join(points,westminster_grid)

#group by grid id and take sum of emissions and area
test <- test %>% group_by(id) %>% summarise_at(vars("carbon_emissions", "totalarea"), sum)
test$carbon_intensity <- test$carbon_emissions/test$totalarea

#join to grid
westminster_grid <- test %>% st_drop_geometry() %>% dplyr::left_join(westminster_grid,., by='id')

#classify
class <- classIntervals(westminster_grid$carbon_intensity, n = 7, style = 'fisher')
westminster_grid <- westminster_grid %>%
  mutate(carbon_class = cut(carbon_intensity, class$brks, include.lowest = T))

#plot
hexplot <- ggplot() + 
  geom_sf(data = westminster_grid,
          aes(fill = carbon_class),color=NA,alpha=0.7) +
  scale_fill_brewer(palette = "Oranges",na.value='#e8e7e3',name = "Carbon Intensity"~(kg~CO[2]~e/m^2))+
  theme_map()+
  theme(legend.title = element_text(size = 8))+
  theme(legend.text = element_text(size = 6))

#view
hexplot

save_plot('plots/hexplot_total.png',p2)

#---------------------------------------------------------------------------------------------
#Read LSOA boundaries and get the ones in westminster - From London datastore https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
LSOAs <-  st_read(here::here("Data/GeoData/LSOA_2011_London_gen_MHW.shp")) %>% st_transform(., 27700) %>% dplyr::filter(str_detect(LAD11NM, "Westminster"))

#Remove all the columns we don't need
LSOAS <- dplyr::select(LSOAs,LSOA11CD,geometry)


##read in the LSOA gas data, from BEIS:
#Gas: https://www.gov.uk/government/collections/sub-national-gas-consumption-data
#Electricity: https://www.gov.uk/government/collections/sub-national-electricity-consumption-data
gas_consumptionLSOA <-read.csv("Data/EnergyData/LSOA_GAS_2019.csv") %>% clean_names() %>% 
  dplyr::rename(gasconsumption=consumption_k_wh)
electricity_consumptionLSOA <-read.csv("Data/EnergyData/LSOA_ELEC_2019.csv") %>% clean_names()%>% 
  dplyr::rename(electricityconsumption=total_domestic_electricity_consumption_k_wh)


#merge this with LSOA data
LSOAS_consumption <- dplyr::left_join(LSOAS,gas_consumptionLSOA, by=c('LSOA11CD'='lsoa11cd'))
LSOAS_consumption <- dplyr::left_join(LSOAS_consumption,electricity_consumptionLSOA, by=c('LSOA11CD'='lower_layer_super_output_area_lsoa_code'))

#assign LSOA
#Get total predicted consumption by LSOA
grouped_LSOA <- domestic %>% group_by(LSOA11CD) %>% summarise_at(vars("gas_consumption","electricity_consumption"),sum) %>% st_drop_geometry()
grouped_LSOA<- dplyr::left_join(LSOAS_consumption,grouped_LSOA,by='LSOA11CD')

#plot scatterplot of predicted vs. measured
#gas
domestic_gas_lsoa <- ggplot(grouped_LSOA, aes(x=gasconsumption/1000000, y=gas_consumption/1000000)) +
  geom_point(col='#526d9c')+geom_abline(intercept=0,slope=1,linetype="dashed")+
  theme_bw()+
  scale_x_continuous(label=comma,name = "Measured Value for LSOA Gas Consumption (GWh)") +
  scale_y_continuous(label=comma, name = "Predicted Value for LSOA Gas Consumption (GWh)")+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"))+
  theme(axis.title=element_text(size=9))


save_plot('plots/lsoa_gas.png',domestic_gas_lsoa)

#electricity
domestic_electricity_lsoa <- ggplot(grouped_LSOA, aes(x=electricityconsumption/1000000, y=electricity_consumption/1000000)) +
  geom_point(col='#a87b00')+geom_abline(intercept=0,slope=1,linetype="dashed")+
  theme_bw()+
  scale_x_continuous(label=comma,name = "Measured Value for LSOA Electricity Consumption (GWh)") +
  scale_y_continuous(label=comma, name = "Predicted Value for LSOA Electricity Consumption (GWh)")+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"))+
  theme(axis.title=element_text(size=9))

save_plot('plots/lsoa_electricity.png',domestic_electricity_lsoa)

#plot as choropleth on LSOA

#gas
#real
gasLSOAreal<- ggplot() +
  geom_sf(data =grouped_LSOA, aes(fill =gasconsumption/1000000),color='white')+theme_map()+labs(fill = "Measured Gas \nConsumption (GWh)")+scale_fill_continuous(low="slategray2", high="slateblue4", 
                                                                                                                                                                 guide="colorbar",na.value="#dbdbdb")+
  theme(legend.text = element_text(size = 6))+theme(legend.title = element_text(size = 8))

#predicted
gasLSOApredict<- ggplot() +
  geom_sf(data =grouped_LSOA, aes(fill =gas_consumption/1000000),color='white')+theme_map()+labs(fill = "Predicted Gas \nConsumption (GWh)")+scale_fill_continuous(low="slategray2", high="slateblue4", 
                                                                                                                                                                   guide="colorbar",na.value="#dbdbdb")+
  theme(legend.text = element_text(size = 6))+theme(legend.title = element_text(size = 8))

#save
save_plot('plots/gasLSOAreal.png',gasLSOAreal)
save_plot('plots/gasLSOApredict.png',gasLSOApredict)

#electricity
#real
elecLSOAreal<- ggplot() +
  geom_sf(data =grouped_LSOA, aes(fill =electricityconsumption/1000000),color='white')+theme_map()+labs(fill = "Measured Electricity \nConsumption (GWh)")+scale_fill_continuous(low="#ffdf9e", high="#966e1d", 
                                                                                                                                                                                 guide="colorbar",na.value="#dbdbdb")+
  theme(legend.text = element_text(size = 6))+theme(legend.title = element_text(size = 8))

#predicted
elecLSOApredict<- ggplot() +
  geom_sf(data =grouped_LSOA, aes(fill =electricity_consumption/1000000),color='white')+theme_map()+labs(fill = "Predicted Electricity \nConsumption (GWh)")+scale_fill_continuous(low="#ffdf9e", high="#966e1d", 
                                                                                                                                                                                   guide="colorbar",na.value="#dbdbdb")+
  theme(legend.text = element_text(size = 6))+theme(legend.title = element_text(size = 8))

save_plot('plots/elecLSOAreal.png',elecLSOAreal)
save_plot('plots/elecLSOApredict.png',elecLSOApredict)

plotLSOAelectricity

grid_map <- plot_grid(plotLSOAgas, plotLSOAelectricity, labels = c('A', 'B'),
                      label_x = 0, label_y = 0,
                      hjust = -0.5, vjust = -0.5)

#calculate performance
cor(grouped_LSOA$gasconsumption, grouped_LSOA$gas_consumption) ^ 2
cor(grouped_LSOA$electricityconsumption, grouped_LSOA$electricity_consumption) ^ 2
rmse(grouped_LSOA$gasconsumption/1000000, grouped_LSOA$gas_consumption/1000000)
rmse(grouped_LSOA$electricityconsumption/1000000, grouped_LSOA$electricity_consumption/1000000)
