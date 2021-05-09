# profor
R code for analysis and visualization of data in Cheng et al. 2019 - A systematic map of evidence on the contribution of forests to poverty alleviation published in Environmental Evidence

## Raw data processing and creation of relational datatables
The raw coded data is stored in Data_Final_PROFOR_7_2_18.csv. This data contains both categorical as well as free text data columns. In order to process the categorical data (particularly for row/column combinations with more than one categorical value), data types and categorical variables are defined in the questions_LU_cat.csv. These two files are then read into the evidence_based_PROFOR.R script which processes the raw data into a series of relational datatables. Datatables can be cross-related using the 'aid' variable (unique article ID assigned to each article in the dataset). The final relational datatables used in the analysis are stored in a RData file (PROFOR_Evidnece_Map_7_5_2018.RData)

## Data cleaning and flattening for analysis
Selected information from the datatables were compiled and flattened for use in the analysis and stored as an RDS file. The script map_data_generation.R draws selected columns from the RData file and cleans up any syntax issues for analysis. The final cleaned and processed data file for analysis is map_data_final_7_5_19.rds.

## Analysis and visualization
PROFOR_query_scripts.R contains the scripts used to generate summary statistics and visualizations from this dataset. 
