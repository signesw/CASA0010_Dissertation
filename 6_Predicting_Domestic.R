library(stringr)

# This code uses the geographically referenced EPC data and Model 2 to predict gas and electricity use for domestic properties in London

#Read EPC matched building data
EPC_buildings <-st_read(here::here("Data/BuildingData/buildings_EPC.geojson"))

#Select only the columns we are interested in
EPC_buildings_small <- dplyr::select(EPC_buildings,fid,BUILDING_REFERENCE_NUMBER,POSTCODE,NUMPOINTS, CURRENT_ENERGY_RATING,PROPERTY_TYPE,BUILT_FORM,TOTAL_FLOOR_AREA, NUMBER_HABITABLE_ROOMS, geometry,CONSTRUCTION_AGE_BAND, LINE_ADDRESS)

#rename columns:
colnames(EPC_buildings_small) <- c('fid','ref','postcode','points','epc_rating','type','form','tfa','nroom','age_band','address','geometry')

#drop columns with both missing floor area data and number of rooms
#replace 0 tfa to NA values
EPC_buildings_small$tfa[EPC_buildings_small$tfa == 0] <- NA
EPC_buildings_small <- EPC_buildings_small[!with(EPC_buildings_small,is.na(tfa)& is.na(nroom)),]

#drop rows with tfa smaller than 20m2
EPC_buildings_small <- dplyr::filter(EPC_buildings_small,tfa>20)

#Map number of rooms based on floor area for those with missing values
areabins <- seq(0, 5000, 50)
arealabels <- seq(1:100)

EPC_buildings_small$nroomtfa <- cut(EPC_buildings_small$tfa, breaks = areabins, labels = arealabels)

#Where nrooms is NA, use mapped value
EPC_buildings_small <- EPC_buildings_small %>% 
  mutate(nroom= coalesce(nroom,as.numeric(levels(nroomtfa))[nroomtfa]))

#map exposed sides based on form and type
EPC_buildings_small <- EPC_buildings_small %>% mutate(propmap = case_when(
  type=="House"~ 0,
  type=='Flat'~-2,
  type=='Bungalow'~0.5,
  type=='Maisonette'~-2,
  type=='Park home'~0
))

EPC_buildings_small <- EPC_buildings_small %>% mutate(builtmap = case_when(
  form=="Detached"~ 0,
  form=='Semi-Detatched'~-1,
  form=='Semi-Detached'~-1,
  form=='End-Terrace'~-1,
  form=='Mid-Terrace'~-2,
  form=='Enclosed Mid-Terrace'~-2.5,
  form== 'Enclosed End-Terrace'~-1.5,
  form =='NO DATA!'~0
))

EPC_buildings_small <- EPC_buildings_small %>% mutate(exposedsides=6+propmap+builtmap)


##replace missing values with NA values
EPC_buildings_small$age_band[EPC_buildings_small$age_band == ''] <- NA
EPC_buildings_small$age_band[EPC_buildings_small$age_band == 'INVALID!'] <- NA
EPC_buildings_small$age_band[EPC_buildings_small$age_band == 'NO DATA!'] <- NA

EPC_buildings_small <- EPC_buildings_small %>% mutate(epc_age = case_when(
  age_band=="England and Wales: before 1900"~ 0,
  age_band=='England and Wales: 1900-1929'~1,
  age_band=='England and Wales: 1930-1949'~2,
  age_band=='England and Wales: 1950-1966'~3,
  age_band=='England and Wales: 1967-1975'~4,
  age_band=='England and Wales: 1976-1982'~5,
  age_band== 'England and Wales: 1983-1990'~6,
  age_band=='England and Wales: 1991-1995'~7,
  age_band=='England and Wales: 1996-2002'~8,
  age_band=='England and Wales: 2003-2006'~9,
  age_band=='England and Wales: 2007-2011'~10,
  age_band=='England and Wales: 2007 onwards'~10,
  age_band=='England and Wales: 2012 onwards'~11,
  age_band=='2014'~ 11,
  age_band=='2020'~11,
  age_band=='2021'~11
))

#deal with missing values
#find median age band in postcode
median_ages <- aggregate(EPC_buildings_small$epc_age, list(EPC_buildings_small$postcode), FUN=median,na.rm=TRUE) %>% 
  rename(.,postcode = Group.1, median_age = x)

EPC_buildings_small <- dplyr::left_join(EPC_buildings_small,median_ages,by="postcode")

#Where age is NA, use median value
EPC_buildings_small <- EPC_buildings_small %>% 
  mutate(age= coalesce(epc_age,median_age))

sum(is.na(EPC_buildings_small$age))

#Still 4374 properties with unknown age - use wider postcode median age for this
EPC_buildings_small <- EPC_buildings_small %>% dplyr::rowwise() %>% dplyr::mutate(firstpc = strsplit(postcode, split=" ")[[1]][1])

EPC_buildings_small <- st_as_sf(EPC_buildings_small)

median_ages_pc <- aggregate(EPC_buildings_small$epc_age, list(EPC_buildings_small$firstpc), FUN=median,na.rm=TRUE) %>% 
  rename(.,firstpc = Group.1, median_age_pc = x)

EPC_buildings_small <- dplyr::left_join(EPC_buildings_small,median_ages_pc,by="firstpc")

#Where age is NA, use median value
EPC_buildings_small <- EPC_buildings_small %>% 
  mutate(age= coalesce(age,median_age_pc))

sum(is.na(EPC_buildings_small$age))


#let's add LSOA to each building to know which LSOA it is in
#Read LSOA boundaries and get the ones in westminster - obtained from the london datastore: https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
LSOAs <-  st_read(here::here("Data/GeoData/LSOA_2011_London_gen_MHW.shp")) %>% st_transform(., 27700) %>% dplyr::filter(str_detect(LAD11NM, "Westminster"))

#Remove all the columns we don't need
LSOAS <- dplyr::select(LSOAs,LSOA11CD,geometry)

#join LSOA with EPC datset
EPC_buildings_small= st_join(EPC_buildings_small, LSOAS["LSOA11CD"])

#remove duplicate rows (where building is in more than 1 LSOA)
EPC_buildings_small <- EPC_buildings_small[!duplicated(EPC_buildings_small$ref),]

#read IMD csv 
imd <- read_csv('Data/GeoData/imd_lsoa.csv')
colnames(imd) <- c('lsoacd', 'lsoa', 'lacd', 'la', 'imdrank', 'imd')
imd <- dplyr::select(imd, lsoacd, imd)

EPC_buildings_small <- left_join(EPC_buildings_small, imd, by = c("LSOA11CD" = "lsoacd"))

#map EPC rating
EPC_buildings_small <- EPC_buildings_small %>% mutate(epc=case_when(
  epc_rating=='A'~1,
  epc_rating=='B'~2,
  epc_rating=='C'~3,
  epc_rating=='D'~4,
  epc_rating=='E'~5,
  epc_rating=='F'~6,
  epc_rating=='G'~7,
  epc_rating=='H'~8,
))

#now apply the models to predict gas and electricity consumption for both buildings 
EPC_buildings_small <- EPC_buildings_small %>% mutate(gas_consumption=predict(gas, EPC_buildings_small))
EPC_buildings_small <- EPC_buildings_small %>% mutate(electricity_consumption=predict(electricity, EPC_buildings_small))

#output results as a geojson
#write geojson file
st_write(EPC_buildings_small, "Results/domestic_consumption.geojson")

