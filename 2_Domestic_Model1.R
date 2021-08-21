library(caret)
library(car)
library(MASS)
library(Metrics)
library(scales)
library(performance)

#This code was used to compute Model 1 - Based on the National Energy Efficiency Data Framework

#First, read the NEED dataset - this was downloaded from the government's publishing service: (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/857035/anon_set_50k_2019.csv/preview)
NEED <- read.csv(here::here('Data/EnergyData/anon_set_50k_2019.csv'))

#create an id column
NEED <- tibble::rowid_to_column(NEED, "ID")

#extract only records for London  --> will be more representative
NEED <- dplyr::filter(NEED, GOR_EW == "E12000007")
#drop region colum 
NEED <-  subset(NEED, select = -c(GOR_EW) )
#drop val flags column
NEED <- NEED %>% dplyr::select(-contains("ValFlag"))

#Select all the columns containing gas or elec or valid
colheads <- NEED %>% names()

gcons <- NEED %>% dplyr::select(contains("GCons")) %>% names()
econs <- NEED %>% dplyr::select(contains("Econs")) %>% names()
colheadkeep1 <- NEED %>% dplyr::select(-contains("Gcons")) %>% names()

#gather year columns into rows and filter out values outside of range
df1 <- tidyr::gather(NEED, key='column_name', value='gcons',-all_of(colheadkeep1))

#create new column called year
df1$year <- str_sub(df1$column_name,-4,-1)
df1 <- subset(df1, select = -c(column_name) )

#do the same for electricity
df_econs <- NEED %>% dplyr::select(contains("econs"), ID)
df_econs <- tidyr::gather(df_econs, key='column_name', value='econs',-ID)

#make sure it is within the plausible range defined by BEIS
df_econs <- dplyr::filter(df_econs, econs %in% (100:25000))
df_econs$year <- str_sub(df_econs$column_name,-4,-1)
df_econs <- subset(df_econs, select = -c(column_name) )

#drop econs columns in gas df
df1 <- df1 %>% dplyr::select(-contains("Econs"))

#merge df1 and df_econs on year and id
ldn <- inner_join(df1,df_econs,by=c('ID','year')) %>% distinct() 
ldn <- ldn[order(ldn$ID),]

#reset row index
row.names(ldn) <- NULL

#group properties by id and get an average consumption statistic
average_cons <- ldn %>% 
  group_by(ID) %>%                       
  summarise_at(vars(gcons,econs),list(average = mean),na.rm=TRUE)

#left join on ldn dataframe
ldn <- left_join(ldn,average_cons,by="ID")

# assigning new names to the columns of the data frame
colnames(ldn) <- c('id',
                   'proptype',
                   'age',
                   'imd',
                   'floorarea_band',
                   'counciltaxband',
                   'loftins',
                   'loftins_year',
                   'cwi',
                   'cwi_year',
                   'pv',
                   'pv_year',
                   'walls',
                   'main_heat',
                   'gcons',
                   'year',
                   'econs',
                   'average_gcons',
                   'average_econs')

#simplifying parameters for regression 
#convert age column to 1,2,3,4 (nominal encoding)
ldn$age <- ldn$age-100

#change main_heat to 1 and 0
ldn$main_heat[ldn$main_heat == 1] <- 1
ldn$main_heat[ldn$main_heat == 2] <- 0

#change council tax band to numbers
ldn$counciltaxband[ldn$counciltaxband == "A"] <- as.integer(1)
ldn$counciltaxband[ldn$counciltaxband == "B"] <- as.integer(2)
ldn$counciltaxband[ldn$counciltaxband == "C"] <- as.integer(3)
ldn$counciltaxband[ldn$counciltaxband == "D"] <- as.integer(4)
ldn$counciltaxband[ldn$counciltaxband == "E"] <- as.integer(5)
ldn$counciltaxband[ldn$counciltaxband == "F"] <- as.integer(6)
ldn$counciltaxband[ldn$counciltaxband == "G"] <- as.integer(7)
ldn$counciltaxband[ldn$counciltaxband == "H"] <- as.integer(8)

ldn <- transform(ldn, counciltaxband = as.numeric(counciltaxband))

##Function for one-hot encoding of variable
one_hot_encoding = function(df, columns="season"){
  # create a copy of the original data.frame for not modifying the original
  df = cbind(df)
  # convert the columns to vector in case it is a string
  columns = c(columns)
  # for each variable perform the One hot encoding
  for (column in columns){
    unique_values = sort(unique(df[column])[,column])
    non_reference_values  = unique_values[c(-1)] # the first element is going 
    # to be the reference by default
    for (value in non_reference_values){
      # the new dummy column name
      new_col_name = paste0(column,'.',value)
      # create new dummy column for each value of the non_reference_values
      df[new_col_name] <- with(df, ifelse(df[,column] == value, 1, 0))
    }
    # delete the one hot encoded column
    df[column] = NULL
    
  }
  return(df)
}

#select one year
ldn <- dplyr::filter(ldn, year == 2017)

#One hot encoding of property type
ldn_encoded <-  one_hot_encoding(ldn, c("proptype")) %>% clean_names()

#------------------------- RUNNING REGRESSION MODEL -------------------------------------------
##Run linear regression on data: 
#first, split dataset into a test and train dataset
#set random seed 
set.seed(42)

dt = sort(sample(nrow(ldn_encoded), nrow(ldn_encoded)*.8))
train<-ldn_encoded[dt,]
test<-ldn_encoded[-dt,]

#-------------------------GAS---------------------------------------------------------------------
#clean gas model
train_gas <- dplyr::filter(train, !is.na(average_gcons))
test_gas <- dplyr::filter(test, !is.na(average_gcons))

#define gas model
gas <- lm(average_gcons ~counciltaxband+ floorarea_band + main_heat + age + imd +proptype_detatched + proptype_mid_terrace + proptype_end_terrace + proptype_flat + proptype_semi_detached,
          data = train_gas)

#Run stepwise regression model
# Stepwise regression model
gas.model <- stepAIC(gas, direction = "both", 
                      trace = FALSE)
summary(gas.model)


# checking model assumptions
check_model(gas.model)

#check model performance
model_performance(gas.model)

#check VIF
car::vif(gas.model)

#check rmse
Metrics::rmse(train_gas$average_gcons,fitted(gas.model))

#create datframe to be used to plot summary performance
model <- as.data.frame(fitted(gas.model))
model$actual <- train_gas$average_gcons
#check with test model
model_test <- as.data.frame(predict(gas.model, test_gas))
model_test$actual <- test_gas$average_gcons
#check performance of test dataset
cor(model_test$`predict(gas.model, test_gas)`, model_test$actual) ^ 2


#Predicted vs Actual plot
gas_fit <- ggplot() + 
  geom_point(data=model, aes(x = actual, y = fitted(gas.model),color='Train'), alpha=0.5) + 
  geom_point(data=model_test, aes(x=actual, y = predict(gas.model,test_gas),color='Test'),shape=18, alpha=0.7)+
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
gas_fit <- ggdraw(gas_fit)+draw_label(expression(paste("Train: ",  R^{2},'=','0.499')),size=9,x = 0.22  ,y = 0.92)+
  draw_label(expression(paste("Test: ",  R^{2},'=','0.490')),size=9,x = 0.22  ,y = 0.82)


gas_fit
save_plot("plots/Model1_gas_fit.png", gas_fit)


#-------------------Electricity--------------------------

#define electricitymodel
electricity <- lm(average_econs ~ counciltaxband+ floorarea_band + main_heat + age + imd +proptype_detatched + proptype_mid_terrace + proptype_end_terrace + proptype_flat + proptype_semi_detached,
                  data = train)

# Stepwise regression model
electricity.model <- stepAIC(electricity, direction = "both", 
                        trace = FALSE)

summary(electricity.model)

#check VIF
car::vif(electrciity.model)

#check model assumption
check_model(electricity.model)
model_performance(electricity.model)

#create datframe to be used to plot summary performance
model <- as.data.frame(fitted(gas.model))
model$actual <- train_gas$average_gcons

#check with test model 
modele_test <- as.data.frame(predict(electricity.model, test),col.names=prediction)
modele_test$actual <- test$average_econs
cor(modele_test$`predict(electricity.model, test)`, modele_test$actual) ^ 2

#create datframe to be used to plot summary performance
modele <- as.data.frame(fitted(electricity.model))
modele$actual <- train$average_econs


#Predicted vs Actual plot
elec_fit  <- ggplot() + 
  geom_point(data=modele, aes(x = actual, y = fitted(electricity.model),color='Train'), alpha=0.5) + 
  geom_point(data=modele_test, aes(x=actual, y = predict(electricity.model,test),color='Test'),shape=18, alpha=0.7)+
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
elec_fit <- ggdraw(elec_fit)+draw_label(expression(paste("Train: ",  R^{2},'=','0.172')),size=9,x = 0.22  ,y = 0.92)+
  draw_label(expression(paste("Test: ",  R^{2},'=','0.190')),size=9,x = 0.22  ,y = 0.82)
elec_fit

save_plot("plots/Model1_elec_fit.png", elec_fit)



