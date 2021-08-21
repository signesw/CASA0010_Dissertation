#First, let's read the properties with known energy consumption data: 
DEC_properties <- st_read(here::here('Data/BuildingData/buildings_DEC.geojson'))

#calculate gas and electricity consumption from intensities and floor area
DEC_properties$gas_consumption <- DEC_properties$ANNUAL_THERMAL_FUEL_USAGE*DEC_properties$TOTAL_FLOOR_AREA
DEC_properties$electricity_consumption <- DEC_properties$ANNUAL_ELECTRICAL_FUEL_USAGE*DEC_properties$TOTAL_FLOOR_AREA

#Group by fid and take sum of consumption to get total consumption by building polygon
DEC_buildings <-DEC_properties %>% group_by(fid) %>% summarise_at(vars("gas_consumption", "electricity_consumption",'TOTAL_FLOOR_AREA'), sum)
DEC_buildings$gas_intensity <- DEC_buildings$gas_consumption/DEC_buildings$TOTAL_FLOOR_AREA
DEC_buildings$electricity_intensity <- DEC_buildings$electricity_consumption/DEC_buildings$TOTAL_FLOOR_AREA

#Now, let's read the VOA data and assign intensity
VOA_properties <- st_read(here::here('Data/BuildingData/buildings_VOA.geojson'))

#first, we want to make sure that the ones we have data for from DEC aren't included, so we filter those out 
'%ni%' <- Negate('%in%')
VOA_properties <- VOA_properties %>% dplyr::filter(UPRN %ni% unique(DEC_properties$UPRN))

#add energy intensities from calculated ND-NEED values (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1007426/nd-need-2021-data-tables.xlsx)
VOA_properties <- VOA_properties %>% mutate(gas_intensity= case_when(
  building_use=='Arts, Community and Leisure'~127,
  building_use=='Education'~172,
  building_use=='Emergency Services'~189,
  building_use=='Factories'~82,
  building_use=='Health'~184,
  building_use=='Hospitality'~284,
  building_use=='Offices'~164,
  building_use=='Shops'~204,
  building_use=='Warehouses'~61,
  building_use=='Other'~204))

VOA_properties <- VOA_properties %>% mutate(electricity_intensity= case_when(
  building_use=='Arts, Community and Leisure'~31,
  building_use=='Education'~59,
  building_use=='Emergency Services'~54,
  building_use=='Factories'~33,
  building_use=='Health'~188,
  building_use=='Hospitality'~187,
  building_use=='Offices'~75,
  building_use=='Shops'~125,
  building_use=='Warehouses'~30,
  building_use=='Other'~49))

#now, we want to calculate energy use for each record: 
VOA_properties$gas_consumption <- VOA_properties$gas_intensity*VOA_properties$totalarea
VOA_properties$electricity_consumption <- VOA_properties$electricity_intensity*VOA_properties$totalarea

#calculating for each fid
VOA_buildings <-VOA_properties %>% group_by(fid) %>% summarise_at(vars("gas_consumption", "electricity_consumption",'totalarea'), sum)
VOA_buildings$gas_intensity <- VOA_buildings$gas_consumption/VOA_buildings$totalarea
VOA_buildings$electricity_intensity <- VOA_buildings$electricity_consumption/VOA_buildings$totalarea


#combine the two datasets 
#select relevant rows:
VOA_properties <- dplyr::select(VOA_properties, UPRN,gas_consumption, gas_intensity, electricity_consumption,electricity_intensity,geometry,building_use,totalarea,fid)
DEC_properties <- dplyr::select(DEC_properties,UPRN, gas_consumption, ANNUAL_THERMAL_FUEL_USAGE, electricity_consumption,ANNUAL_ELECTRICAL_FUEL_USAGE,geometry,PROPERTY_TYPE,TOTAL_FLOOR_AREA,fid)
colnames(DEC_properties) <- c('UPRN','gas_consumption', 'gas_intensity', 'electricity_consumption','electricity_intensity','building_use','totalarea','fid','geometry')

#make sure datatypes match
DEC_properties$UPRN <-as.numeric(DEC_properties$UPRN)

combined_DEC_VOA <- dplyr::bind_rows(VOA_properties,DEC_properties)

#write as geojson
st_write(combined_DEC_VOA, "Results/Non_Domestic_Consumption.geojson")


