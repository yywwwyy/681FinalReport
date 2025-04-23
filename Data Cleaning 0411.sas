/***********************************
  Step 1: Import Excel Data
  - Import dataset from the specified Excel file and sheet.
************************************/
%let pathname=/home/u64197867/;
proc import datafile="&pathname.iMAP1_Data_20250411.xlsx"
    out=imap1_data
    dbms=xlsx
    replace;
    sheet="iMAP1_Data"; 
    getnames=yes;
run;

/***********************************
  Step 2: Data Cleaning
  - Convert character variables that should be numeric.
  - If value is 'NA' or blank, assign missing (.)
************************************/
data imap1_data_clean;
    set imap1_data;

    /* Convert character variables to numeric with missing handling */
    if strip(MRP_6) in ("NA", "") then MRP_6_num = .;
    else MRP_6_num = input(MRP_6, best.);

    if strip(MRP_12) in ("NA", "") then MRP_12_num = .;
    else MRP_12_num = input(MRP_12, best.);

    if strip(Sub_drug_6) in ("NA", "") then Sub_drug_6_num = .;
    else Sub_drug_6_num = input(Sub_drug_6, best.);

    if strip(Sub_drug_12) in ("NA", "") then Sub_drug_12_num = .;
    else Sub_drug_12_num = input(Sub_drug_12, best.);

    if strip(Undertreat_6) in ("NA", "") then Undertreat_6_num = .;
    else Undertreat_6_num = input(Undertreat_6, best.);

    if strip(Undertreat_12) in ("NA", "") then Undertreat_12_num = .;
    else Undertreat_12_num = input(Undertreat_12, best.);

    if strip(NonAd_6) in ("NA", "") then NonAd_6_num = .;
    else NonAd_6_num = input(NonAd_6, best.);

    if strip(NonAd_12) in ("NA", "") then NonAd_12_num = .;
    else NonAd_12_num = input(NonAd_12, best.);

    if strip(AE_6) in ("NA", "") then AE_6_num = .;
    else AE_6_num = input(AE_6, best.);

    if strip(AE_12) in ("NA", "") then AE_12_num = .;
    else AE_12_num = input(AE_12, best.);

    if strip(Sub_dose_6) in ("NA", "") then Sub_dose_6_num = .;
    else Sub_dose_6_num = input(Sub_dose_6, best.);

    if strip(Sub_dose_12) in ("NA", "") then Sub_dose_12_num = .;
    else Sub_dose_12_num = input(Sub_dose_12, best.);

    if strip(Med_Mon_6) in ("NA", "") then Med_Mon_6_num = .;
    else Med_Mon_6_num = input(Med_Mon_6, best.);

    if strip(Med_Mon_12) in ("NA", "") then Med_Mon_12_num = .;
    else Med_Mon_12_num = input(Med_Mon_12, best.);

    /* Drop original character variables */
    drop MRP_6 MRP_12 Sub_drug_6 Sub_drug_12 Undertreat_6 Undertreat_12
         NonAd_6 NonAd_12 AE_6 AE_12 Sub_dose_6 Sub_dose_12
         Med_Mon_6 Med_Mon_12;

    /* Rename newly created numeric variables back to original names */
    rename MRP_6_num = MRP_6
           MRP_12_num = MRP_12
           Sub_drug_6_num = Sub_drug_6
           Sub_drug_12_num = Sub_drug_12
           Undertreat_6_num = Undertreat_6
           Undertreat_12_num = Undertreat_12
           NonAd_6_num = NonAd_6
           NonAd_12_num = NonAd_12
           AE_6_num = AE_6
           AE_12_num = AE_12
           Sub_dose_6_num = Sub_dose_6
           Sub_dose_12_num = Sub_dose_12
           Med_Mon_6_num = Med_Mon_6
           Med_Mon_12_num = Med_Mon_12;
run;

/***********************************
  Step 2.1: Replace 'Unknown' in race with missing value
************************************/
data imap1_data_clean;
    set imap1_data_clean;
    /* Convert every non‐White race value (including Asian, Black, Pacific Islander, etc.) into 'Non-White' */
    if not missing(race) and upcase(strip(race)) ne "WHITE" then race = "Non-White";
run;

/***********************************
  Step 2.2: Convert negative MRP_6 values to positive
************************************/
data imap1_data_clean;
    set imap1_data_clean;
    if MRP_6 < 0 then MRP_6 = abs(MRP_6);
run;


/***********************************
  Step 2.3: Remove Rows with Negative Numeric Values and Duplicates
  - Delete any row that has a negative value in any numeric variable.
  - Remove duplicate rows (if identical, keep only one).
************************************/
/* Remove rows with negative numeric values first */
proc sort data=imap1_data_clean out=sorted_by_id;
    by ID;
run;

/* Dataset with all observations that have duplicate IDs.*/
data duplicates_by_id;
    set sorted_by_id;
    by ID;
    if first.ID and last.ID then delete;
run;

proc print data=duplicates_by_id;
    title "All Duplicate Observations Based on ID";
run;


proc sort data=imap1_data_clean out=imap1_clean_sorted;
    by ID descending race; 
run;

/* Remove duplicates by ID*/
proc sort data=imap1_clean_sorted nodupkey out=imap1_data_clean;
    by ID;
run;



/***********************************
  Step 2.4: Convert True/False (or count) variables to Binary (0/1)
  - For variables that are logically True/False or should be binary,
    recode any positive value to 1 and zero (or negative, if any) to 0.
  - This step ensures that during imputation and subsequent analysis, 
    these variables remain binary.
************************************/


data imap1_data_clean_binary;
    set imap1_data_clean;  
    
    /* Variables that need to be converted to binary */
    array bin_vars {*} Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0 
                              Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6 
                              Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12;
    
    /* Loop through and recode each variable */
    do i = 1 to dim(bin_vars);
        /* If the variable is missing, keep it missing */
        if missing(bin_vars[i]) then bin_vars[i] = .;
        /* If the value is greater than 0, recode to 1; otherwise, recode to 0 */
        else if bin_vars[i] > 0 then bin_vars[i] = 1;
        else bin_vars[i] = 0;
    end;
    drop i;
run;


/* Print the first 20 observations of the cleaned dataset */
proc print data=imap1_data_clean_binary(obs=20);
    title "First 20 Observations of imap1_data_clean_binary";
run;


/***********************************
  Step 2.5: Recode sex and race variable
************************************/

data imap1_data_clean;
    set imap1_data_clean;  
    if sex = 'F' then sex = 'Female';
    else if sex = 'M' then sex = 'Male';
run;

data imap1_data_clean;
    set imap1_data_clean;  
    if race = 'black' then race = 'Black';
run;

/***********************************
  Step 2.6: Delete rows where both MRP_6 and MRP_12 are missing
************************************/
data imap1_data_clean;
    set imap1_data_clean;
    if missing(MRP_6) and missing(MRP_12) then delete;
run;


/***********************************
  Step 3: Get Variable Names and Count
************************************/
proc contents data=imap1_data_clean; run;

proc freq data=imap1_data_clean;
    tables Treatment / nocum nopercent missing;
run;

proc freq data=imap1_data_clean;
    tables site / nocum nopercent missing;
run;

/***********************************
  Step 4: Convert character variables Treatment, race, sex, site to numeric codes
  - Create mapping tables (e.g., race_map) showing original values and numeric codes
  - Merge codes into the main dataset
************************************/

/* Macro to create and display mapping tables */
%macro code_map(var);
    /* Sort and remove duplicates */
    proc sort data=imap1_data_clean(keep=&var) out=&var._unique nodupkey;
        where not missing(&var);
        by &var;
    run;

    /* Assign numeric code */
    data &var._map;
        set &var._unique;
        &var._Code = _N_;
    run;

    /* Display the mapping table */
    title "Mapping of &var to Numeric Code";
    proc print data=&var._map noobs label;
        label &var = "Original &var"
              &var._Code = "Numeric Code";
    run;
%mend;

/* Run the macro for each variable */
%code_map(Treatment);
%code_map(race);
%code_map(sex);
%code_map(site);


/* Merge mappings into main dataset */
proc sql;
    create table imap1_data_clean2 as
    select a.*,
           b.Treatment_Code,
           c.race_Code,
           d.sex_Code,
           e.site_Code
    from imap1_data_clean as a
         left join Treatment_map as b on a.Treatment = b.Treatment
         left join race_map     as c on a.race     = c.race
         left join sex_map      as d on a.sex      = d.sex
         left join site_map     as e on a.site     = e.site;
quit;

/* Replace original variables with coded ones */
data imap1_data_clean2;
    set imap1_data_clean2;
    drop Treatment race sex site;
    rename Treatment_Code = Treatment
           race_Code = race
           sex_Code = sex
           site_Code = site;
run;

/***********************************
  Step 5: Calculate Missing Rates
  - Loop through all variables and compute percentage of missing values
  - Use fixed sample size as denominator
************************************/
/***********************************
  Step 5: Calculate Missing Rates Excluding Deaths
  - Loop through all variables and compute percentage of missing values,
    excluding observations where death equals 1.
  - Use fixed sample size (after excluding deaths) as denominator.
************************************/

/* 1. Initialize the base dataset with ALL variables and proper attributes */
data miss_summary;
    length Variable $32;
    total_SC = .;
    MissingCount_SC = .;
    MissingPercent_SC = .;
    total_iMAP = .;
    MissingCount_iMAP = .;
    MissingPercent_iMAP = .;
    format MissingPercent_SC MissingPercent_iMAP 6.2;
    stop; /* Creates structure without adding observations */
run;

/* 2. Modified macro: Note that here we assume that a macro variable nvars is defined
   and that the list of variable names is stored in macro variables &var1, &var2, ..., &var&nvars */
%macro missing_report;
    %do i = 1 %to &nvars;
        %let var = &&var&i;
        
        /* Calculate for SC group while excluding subjects with death=1 */
        proc means data=imap1_data_clean2(where=(treatment=1 and death ne 1)) n nmiss noprint;
            var &var;
            output out=temp_sc n=nobs_sc nmiss=nmiss_sc;
        run;
        
        /* Calculate for iMAP group while excluding subjects with death=1 */
        proc means data=imap1_data_clean2(where=(treatment=2 and death ne 1)) n nmiss noprint;
            var &var;
            output out=temp_imap n=nobs_imap nmiss=nmiss_imap;
        run;
        
        /* Combine results */
        data temp;
            length Variable $32;
            merge temp_sc temp_imap;
            Variable = "&var";
            total_SC = nobs_sc + nmiss_sc;
            MissingCount_SC = nmiss_sc;
            MissingPercent_SC = ifn(total_SC > 0, (nmiss_sc/total_SC)*100, .);
            
            total_iMAP = nobs_imap + nmiss_imap;
            MissingCount_iMAP = nmiss_imap;
            MissingPercent_iMAP = ifn(total_iMAP > 0, (nmiss_imap/total_iMAP)*100, .);
            
            format MissingPercent_SC MissingPercent_iMAP 6.2;
            keep Variable total_SC MissingCount_SC MissingPercent_SC
                 total_iMAP MissingCount_iMAP MissingPercent_iMAP;
        run;
        
        proc append base=miss_summary data=temp force;
        run;
    %end;
%mend;
%missing_report;

/* Sort the missing summary dataset */
proc sort data=miss_summary;
    by descending MissingPercent_SC descending MissingPercent_iMAP;
run;

/* Print the missing summary table */
proc print data=miss_summary noobs label;
    title "Missing Percentage for Each Variable by Treatment Group (Excluding Deaths)";
    label 
        Variable = "Variable Name"
        total_SC = "Total Observations (SC)"
        MissingCount_SC = "Missing Count (SC)"
        MissingPercent_SC = "Missing % (SC)"
        total_iMAP = "Total Observations (iMAP)"
        MissingCount_iMAP = "Missing Count (iMAP)"
        MissingPercent_iMAP = "Missing % (iMAP)";
run;


/***********************************
  Step 6: Look at the pattern of missing data to look for systematic issues
************************************/
data high_missing_vars;
    set miss_summary;
    if MissingPercent_SC >= 5 or MissingPercent_iMAP >= 5;
run;

proc print data=high_missing_vars noobs label;
    title "Variables with ≥5% Missing Data in Either Group";
run;

data missing_check;
    set imap1_data_clean2;
    miss_MRP_6  = missing(MRP_6);   /* 1 if MRP_6 is missing, 0 otherwise */
    miss_MRP_12 = missing(MRP_12);  /* 1 if MRP_12 is missing, 0 otherwise */
run;

/* Check if missingness in MRP_6 depends on observed variables */
proc logistic data=missing_check desc;
    model miss_MRP_6 = treatment age sex site MRP_0; 
    oddsratio age / at (age=5); 
    oddsratio treatment;     
    title "MAR Check: MRP_6 Missingness vs. Observed Variables (with ORs)";
    ods output ParameterEstimates=MAR_MRP_6;
run;

*Pr > ChiSq < 0.05 indicates missingness in MRP_6 is associated with treatment, age.;

/* Repeat for MRP_12 */
proc logistic data=missing_check desc;
    model miss_MRP_12 = treatment age sex site MRP_0 MRP_6; 
    oddsratio age / at (age=5);  
    oddsratio treatment;      
    title "MAR Check: MRP_12 Missingness vs. Observed Variables (with ORs)";
    ods output ParameterEstimates=MAR_MRP_12;
run;

*Pr > ChiSq < 0.05 indicates missingness in MRP_12 is associated with treatment, age;

proc freq data=missing_check;
    tables treatment*miss_MRP_6 / chisq nocol nopercent;
    title "Association Between Treatment and MRP_6 Missingness";
run;

proc freq data=missing_check;
    tables treatment*miss_MRP_12 / chisq nocol nopercent;
    title "Association Between Treatment and MRP_12 Missingness";
run;

proc freq data=missing_check;
    tables race*miss_MRP_6 / chisq nocol nopercent;
    title "Association Between race and MRP_6 Missingness";
run;

proc freq data=missing_check;
    tables race*miss_MRP_12 / chisq nocol nopercent;
    title "Association Between race and MRP_12 Missingness";
run;

proc freq data=missing_check;
    tables site*miss_MRP_6 / chisq nocol nopercent;
    title "Association Between site and MRP_6 Missingness";
run;

proc freq data=missing_check;
    tables site*miss_MRP_12 / chisq nocol nopercent;
    title "Association Between site and MRP_12 Missingness";
run;

proc freq data=missing_check;
    tables age*miss_MRP_6 / chisq nocol nopercent;
    title "Association Between age and MRP_6 Missingness";
run;

proc freq data=missing_check;
    tables age*miss_MRP_12 / chisq nocol nopercent;
    title "Association Between age and MRP_12 Missingness";
run;

*again looking at the chi-square p-value, p < 0.05 for treatment * MRP_12;



/***********************************
  Step 7: Sensitivity Analysis (MNAR Scenarios)
************************************/

/* MRP_6 */
data mnar_data;
    set missing_check;
    miss_MRP_6  = missing(MRP_6);  /* 1=missing, 0=observed */
    miss_MRP_12 = missing(MRP_12);
run;

/* Model for MRP_6 (adjusting for missingness in MRP_6) */
proc glimmix data=mnar_data;
    class treatment race site miss_MRP_6;
    model MRP_6 = treatment|race|site|miss_MRP_6 MRP_0 / dist=poisson link=log;
    random intercept / subject=ID;
    title "MNAR Sensitivity: MRP_6 Pattern-Mixture";
run;

/* Model for MRP_12 (adjusting for missingness in MRP_12) */
proc glimmix data=mnar_data;
    class treatment race site miss_MRP_12;
    model MRP_12 = treatment|race|site|miss_MRP_12 MRP_0 MRP_6 / dist=poisson link=log; 
    random intercept / subject=ID;
    title "MNAR Sensitivity: MRP_12 Pattern-Mixture";
run;

 *Pattern-Mixture Model shows not significant p-values for treatment*miss_group interaction, MNAR unlikely, MAR holds;


/***********************************
  Step 8 (Updated): Multiple Imputation and Force Integer Values
  - Use PROC MI to impute variables with missing data
  - Use FCS: regpmm for count variables and logistic for categorical ones
  - Then force count variables to be integer via rounding
************************************/

/* Imputation on imap1_data_clean */
proc mi data=imap1_data_clean nimpute=5 out=imputed_data seed=12345;
    /* Specify categorical (binary) variables in CLASS */
    class Treatment race site sex 
          Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0 
          Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6 
          Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12;
    
    /* List each variable to be imputed */
    var Treatment race site sex
        Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0
        Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6
        Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12
        MRP_12 MRP_6;
    
    /* Use FCS: regpmm for count variables and logistic for categorical ones */
    fcs regpmm(MRP_12 MRP_6)
        logistic(Treatment race site sex
                 Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0
                 Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6
                 Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12);
run;

/* Imputation on imap1_data_clean2 */
proc mi data=imap1_data_clean2 nimpute=5 out=imputed_data2 seed=12345;
    /* Specify categorical (binary) variables in CLASS */
    class Treatment race site sex 
          Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0 
          Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6 
          Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12;
    
    /* List each variable to be imputed */
    var Treatment race site sex
        Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0
        Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6
        Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12
        MRP_12 MRP_6;
    
    /* Use FCS: regpmm for count variables and logistic for categorical ones */
    fcs regpmm(MRP_12 MRP_6)
        logistic(Treatment race site sex
                 Undertreat_0 Sub_dose_0 Med_Mon_0 Sub_drug_0 AE_0 NonAd_0
                 Undertreat_6 Sub_dose_6 Med_Mon_6 Sub_drug_6 AE_6 NonAd_6
                 Undertreat_12 Sub_dose_12 Med_Mon_12 Sub_drug_12 AE_12 NonAd_12);
run;

/*-----------------------------------------------------------
  Post-Imputation: Force all count variables to be integers 
-----------------------------------------------------------*/

/* For the first imputed dataset */
data imputed_data;
    set imputed_data;
    /* Round count variables to nearest integer */
    MRP_6        = round(MRP_6, 1);
    MRP_12       = round(MRP_12, 1);
    AE_12        = round(AE_12, 1);
    Med_Mon_12   = round(Med_Mon_12, 1);
    NonAd_12     = round(NonAd_12, 1);
    Sub_dose_12  = round(Sub_dose_12, 1);
    Sub_drug_12  = round(Sub_drug_12, 1);
    Undertreat_12= round(Undertreat_12, 1);
    AE_6         = round(AE_6, 1);
    Med_Mon_6    = round(Med_Mon_6, 1);
    NonAd_6      = round(NonAd_6, 1);
    Sub_dose_6   = round(Sub_dose_6, 1);
    Sub_drug_6   = round(Sub_drug_6, 1);
    Undertreat_6 = round(Undertreat_6, 1);

    /* Ensure negative imputed values are set to 0 */
    if MRP_6 < 0 then MRP_6 = 0;
    if MRP_12 < 0 then MRP_12 = 0;
run;

/* For the second imputed dataset */
data imputed_data2;
    set imputed_data2;
    MRP_6        = round(MRP_6, 1);
    MRP_12       = round(MRP_12, 1);
    AE_12        = round(AE_12, 1);
    Med_Mon_12   = round(Med_Mon_12, 1);
    NonAd_12     = round(NonAd_12, 1);
    Sub_dose_12  = round(Sub_dose_12, 1);
    Sub_drug_12  = round(Sub_drug_12, 1);
    Undertreat_12= round(Undertreat_12, 1);
    AE_6         = round(AE_6, 1);
    Med_Mon_6    = round(Med_Mon_6, 1);
    NonAd_6      = round(NonAd_6, 1);
    Sub_dose_6   = round(Sub_dose_6, 1);
    Sub_drug_6   = round(Sub_drug_6, 1);
    Undertreat_6 = round(Undertreat_6, 1);

    if MRP_6 < 0 then MRP_6 = 0;
    if MRP_12 < 0 then MRP_12 = 0;
run;

/* Verify the results: Check missing values and summary statistics */
proc means data=imputed_data N Nmiss;
    var AE_12 MRP_12 Med_Mon_12 NonAd_12 Sub_dose_12 Sub_drug_12 Undertreat_12 
        AE_6 MRP_6 Med_Mon_6 NonAd_6 Sub_dose_6 Sub_drug_6 Undertreat_6;
    title "Check Missing Values After Imputation (cleaned_data)";
run;

proc means data=imputed_data2 N Nmiss;
    var AE_12 MRP_12 Med_Mon_12 NonAd_12 Sub_dose_12 Sub_drug_12 Undertreat_12 
        AE_6 MRP_6 Med_Mon_6 NonAd_6 Sub_dose_6 Sub_drug_6 Undertreat_6;
    title "Check Missing Values After Imputation (cleaned_data2)";
run;

/***********************************
  Step 9: Export Imputed Data
************************************/
proc export data=imap1_data_clean
    outfile="&pathname.cleaned_data.csv"
    dbms=csv
    replace;
run;

proc export data=imap1_data_clean2
    outfile="&pathname.cleaned_data2.csv"
    dbms=csv
    replace;
run;

proc export data=imputed_data
    outfile="&pathname.imputed_data.csv"
    dbms=csv
    replace;
run;

proc export data=imputed_data2
    outfile="&pathname.imputed_data2.csv"
    dbms=csv
    replace;
run;

