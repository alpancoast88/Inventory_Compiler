# Inventory_Compiler
Compile plot data into attribute table format 

This is an early attempt at compiling forest inventory data from plot records to a stand level summary table format. 

The population of interest is a large forest tract, stratified into 'stands'. Samples are taken within each strata. Variables of interest are summarized for each strata.  

Merchantable volume and log grade distributions are currently done in seperate script and combined with the inventory attribute table for complete analysis.

Eventually everything will be intergrated into a single tool 

## Note:
PP Site index is not working. Empty columns in attribute table are currently down outside and need work...

Inventory_Compiler.R reads Plot_Data.csv and Site_Trees.csv and outputs Inventory_Attribute_Table.csv
