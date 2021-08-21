
#This code was used to compute Model 2 - Based on EPC records and postcode-level consumption


#First, read list of all postcodes in London, obtained from the london datastore: https://data.london.gov.uk/dataset/postcode-directory-for-london
postcodes <- read.csv("Data/GeoData/london_postcodes.csv") %>% dplyr::select(.,pcds)

#Then, read postcode level consumption data. This is published by BEIS here: 
#https://www.gov.uk/government/collections/sub-national-gas-consumption-data (Gas) and 
#https://www.gov.uk/government/collections/sub-national-electricity-consumption-data

#read in the postcode gas data
gas_postcode <-read.csv("Data/EnergyData/GasData.csv") %>% clean_names()
#select only the ones in london
gas_postcode <- dplyr::inner_join(postcodes, gas_postcode, by = c('pcds'='postcode'))
# assigning new names to the columns of the data frame
colnames(gas_postcode) <- c('pgas','gasmeters','gascons','gasavg','gasmid')


#read in the electricity data
elec_postcode <-read.csv("Data/EnergyData/ElectricityData.csv")%>% clean_names()
#select only the ones in london
elec_postcode <- dplyr::inner_join(postcodes, elec_postcode, by = c('pcds'='postcode'))
# assigning new names to the columns of the data frame
colnames(elec_postcode) <- c('pelec','elecmeters','eleccons','elecavg','elecmid')

#read epc cleaned data
epc <- read.csv("Data/EnergyData/cleaned_epc.csv")

#merge electricity and gas data
gas_and_elec <- dplyr::inner_join(gas_postcode,elec_postcode,by=c('pgas'='pelec'))

#merge epc data to gas and elec
epc_postcodecons <- dplyr::inner_join(epc,gas_and_elec,by=c('pcode'='pgas'))

#filter to only include readings where the number of meters is 6 or less 
epc_postcodecons <- dplyr::filter(epc_postcodecons, gasmeters < 7 &elecmeters<7)

#select the columns we need
dfepc <-  dplyr::select(epc_postcodecons,
                        imd, 
                        type,
                        epc_band,
                        form, 
                        exposedsides, 
                        tfa, 
                        age, 
                        nrooms,
                        nroom, 
                        gasmid,
                        elecmid,
                        gasmeters,
                        elecmeters)
#redefine columns
colnames(dfepc) <- c('imd', 
                     'type', 
                     'epc',
                     'form', 
                     'exposedsides', 
                     'tfa', 
                     'age', 
                     'nrooms',
                     'nroom',
                     'average_gcons',
                     'average_econs',
                     'gasmeters',
                     'elecmeters')


# select only entries with less than 10 rooms
dfepc <- dplyr::filter(dfepc,nroom<10)
#remove implausible floor areas
dfepc <- dplyr::filter(dfepc,tfa<500&tfa>20)
#remove implausible consumption
dfepc <- dplyr::filter(dfepc,average_econs<25000&average_econs>1000)
dfepc <- dplyr::filter(dfepc,average_gcons<50000&average_gcons>1000) %>% drop_na()

#---------------------------Regression Modelling-------------------------------------

#plot dataset into test and train
dt = sort(sample(nrow(dfepc), nrow(dfepc)*.8))
train<-dfepc[dt,] 
test<-dfepc[-dt,]


#define gas model
gas <- lm(average_gcons ~epc+ tfa +imd+ exposedsides+nroom+age,
          data = train)

# Stepwise regression model
gas <- stepAIC(gas, direction = "both", 
               trace = FALSE)

#model summary
summary(gas)

#check model performance
model_performance(gas)
check_model(gas)

#create datframe to be used to plot summary performance
#check with test model
model_test <- as.data.frame(predict(gas, test))
model_test$actual <- test$average_gcons
cor(model_test$`predict(gas, test)`, model_test$actual) ^ 2
model <- as.data.frame(fitted(gas))
model$actual <- train$average_gcons


#Predicted vs Actual plot
gas_fit <- ggplot() + 
  geom_point(data=model, aes(x = actual, y = fitted(gas),color='Train'), alpha=0.5) + 
  geom_point(data=model_test, aes(x=actual, y = predict(gas,test),color='Test'),shape=18, alpha=0.7)+
  geom_abline(slope=1,linetype='dashed',color='#2a5280')+
  theme_bw()+
  scale_x_continuous(label=comma,name = "Actual Value for Gas Consumption (kWh)") +
  scale_y_continuous(label=comma, name = "Predicted Value for Gas Consumption (kWh)")+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"))+
  theme(axis.title=element_text(size=9))+
  scale_color_manual(values=c("#6954c4","#7ba5c9"))+
  theme(
    legend.title = element_blank()
  )
gas_fit <- ggdraw(gas_fit)+draw_label(expression(paste("Train: ",  R^{2},'=','0.538')),size=9,x = 0.22  ,y = 0.92)+
  draw_label(expression(paste("Test: ",  R^{2},'=','0.498')),size=9,x = 0.22  ,y = 0.82)

gas_fit

save_plot('plots/Model2_gas_fit.png',gas_fit)


#----------------------ELECTRICITY--------------------------------------------------------

#define electricity model
electricity <- lm(average_econs ~epc+ tfa +imd+ exposedsides+nroom+age,
                  data = train)

# Stepwise regression model
electricity <- stepAIC(electricity, direction = "both", 
                       trace = FALSE)



#check model assumptions
check_model(electricity)
model_performance(electricity)

#check model performance
model_performance(electricity)

#create datframe to be used to plot summary performance
modele <- as.data.frame(fitted(electricity))

modele$actual <- train$average_econs


#check with test model 
modele_test <- as.data.frame(predict(electricity, test),col.names=prediction)
modele_test$actual <- test$average_econs
cor(modele_test$`predict(electricity, test)`, modele_test$actual) ^ 2



#Predicted vs Actual plot
elec_fit  <- ggplot() + 
  geom_point(data=modele, aes(x = actual, y = fitted(electricity),color='Train'), alpha=0.5) + 
  geom_point(data=modele_test, aes(x=actual, y = predict(electricity,test),color='Test'),shape=18, alpha=0.7)+
  geom_abline(slope=1,linetype='dashed',color='#c9a11a')+
  theme_bw()+
  scale_x_continuous(label=comma,name = "Actual Value for Electricity Consumption (kWh)") +
  scale_y_continuous(label=comma, name = "Predicted Value for Electricity Consumption (kWh)")+
  theme(axis.text.x = element_text(color="black"),axis.text.y = element_text(color="black"))+
  theme(axis.title=element_text(size=9))+
  scale_color_manual(values=c("#8f6503","#ebca86"))+
  theme(
    legend.title = element_blank()
  )
elec_fit <- ggdraw(elec_fit)+draw_label(expression(paste("Train: ",  R^{2},'=','0.350')),size=9,x = 0.22  ,y = 0.92)+
  draw_label(expression(paste("Test: ",  R^{2},'=','0.361')),size=9,x = 0.22  ,y = 0.82)
elec_fit
save_plot("plots/Model2_elec_fit.png", elec_fit)

