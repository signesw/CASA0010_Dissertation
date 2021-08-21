# Exploring the distribution of urban building carbon emissions
## A spatial approach to Westminster's decarbonisation strategy

This repository includes the files needed to carry out the analysis for the CASA0010 Dissertation, set out to answer the following research question: 

*How can non-spatially referenced data be used to spatially investigate urban building CO2 emissions?*

To carry out the analysis, run the following files in order: 
***Note: Do not clear the R environment between each file!***

1. **1_Initial_cleaning.r** - This file carries out the initial data cleaning of the buildings file used in subsequent analysis.
2. **2_Domestic_Model1.r** - Builds Regression Model 1 
3. **3_Model2_EPC_DataCleaning.ipynb** - Precprocessing of data for model 2
4. **4_Domestic_Model2.r** - Builds Regression Model 2
5. **5_Data_Matching_EPC** - Matched EPC data with building polygons
6. **6_Predicting_Domestic.r** - Predicts domestic energy consumption
7. **7_Data_Matching_EPC.ipynb**-Matched DEC data with building polygons
8. **8_Data_Matching_VOA.ipynb**- Matches VOA data with building polygons
9. **9_Predicting_NonDomestic** - Predicts non domestic energy consumption
10. **10_Final_Results** - Produces final results presented in the report

#### Due to licensing, some data is not included in this repository, most notably data supplied by the Ordnance Survey. Where OS data is being read, this has been marked in the code by !!!, including instructions how to obtain this data. 


