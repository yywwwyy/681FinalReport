# 681FinalReport
This repository documents the data processing and analysis steps for BIS 681 iMAP1 Group. 

Below is a summary of file purposes and data flow.


## Code and Data Flow

| File                      | Input                     | Output                  |
|--------------------------|---------------------------|-------------------------|
| `Data Cleaning & processing.sas`    | `iMAP1_Data_20250411.xlsx`       | `cleaned_data.csv`,`imputed_data.csv`      |
### Note
> - `cleaned_data.csv` and `imputed_data.csv` preserve the original categorical variables. 
> - `cleaned_data2.csv` and `imputed_data2.csv` **recoded categorical variables into numeric values**.  


## Modeling and Analysis
| File                      | Input                     | Output                  |
|--------------------------|---------------------------|-------------------------|
| `model selection-681.Rmd` | `cleaned_data.csv`     |
| `Primary Analysis4-18.Rmd` | `cleaned_data.csv`     | 
| `secondary_analysis.R`  | `cleaned_data.csv`       | 

### Note
The input file names listed above are simplified for clarity.  
> **The actual names in the code may differ slightly** (e.g.,cleaned_data-4-11),  
> but they refer to the same data versions and purposes as described here.

## Statistical Analysis Plan
The file `IMAP1_FINAL_SAP.docx.pdf` outlines all planned analyses
