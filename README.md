# HC-shiny

This is a repository to store shiny app files including codes and dataframes. 

To-Do 160516:
Finish rest of the shiny app charts generations
Try run app

To-Do 160525:
Solve Drugs Tab's error
Find functions to apply function/count on data table: dplyr

To-do 160526:
1. Change all tab_functions to only filter by DRUGNAME (e.g. function(current_brand))
2. Change all reactive_function to only include current_brand & date range
3. Change current_search table to include only Brand Name & Date Range
4. In Drugs tab,  2 bar charts are presented: Top_25_Indications associated with searched durgname + Top_25_(most reported)Ingredients associated with  searched drugname

Note 160526:
- Tables needed with DrugTabOnly Reports_Tab: CV_Reports (REPORT_ID, DATINTRECEIVED_CLEAN, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, OTHER_MEDICALLY_IMP_COND)
                                 CV_Report_Drug (REPORT_ID, DRUGNAME)
- Tables needed with DrugTabOnly Patients_Tab:  CV_Reports (REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_GROUP_CLEAN)
                                                CV_Report_Drug (REPORT_ID, DRUGNAME)
- Tables needed with DrugTabOnly Drugs_Tab:

Note 160530:
- For all serious_reasons columns: "1" = "Yes", "2" = "No", NA = "missing"

