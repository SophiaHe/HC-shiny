# connect to CV database
hcopen <- dbConnect(PostgreSQL(), host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
dbListTables(hcopen)

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")



current_brand <- "RAMICADE"
current_rxn <- "Drug ineffective"
current_gender <- "All"
current_date_range <- c(ymd("19650101", ymd("20160527")))



####################################################################################################################
# RAMICADE vs. HUMIRA ([CV_Shiny_DO SHe.R#485], <-.data.frame: replacement has 1 row, data has 0) SOLVED!!!!
# DIGOXIN: [CV_Shiny_DO SHe.R#492], Error in if: missing value where TRUE/FALSE needed SOLVED!!!

####################################################################################################################
cv_drug_product <- tbl(hcopen, "cv_drug_product") %>% collect()
head(cv_drug_product)
cv_drug_product$DRUG_PRODUCT_ID[cv_drug_product$DRUGNAME == "VITAMIN C"]

####################################################################################################################
df <- cv_report_drug_indication %>% collect()
distinct(df[df$DRUGNAME== "ABILIFY",],INDICATION_NAME_ENG) %>% select(DRUGNAME, INDICATION_NAME_ENG)

a <- dplyr::summarise(group_by(cv_report_drug_indication,DRUGNAME ),count=n_distinct(INDICATION_NAME_ENG)) %>% dplyr::arrange(desc(count)) %>% collect()
dim(a)



b <- cv_reports %>% collect()
table(b$GENDER_ENG)

data <- patients_tab(current_brand=current_brand,current_rxn=current_rxn,current_gender =current_gender,current_date_range=current_date_range)


####################################################################################################################

observe({
  updateSelectizeInput(session, )
  })



####################################################################################################################
head(cv_reactions)
cv_reactions <-cv_reactions %>% as.data.frame(n=-1)
table(cv_reactions$SOC_NAME_ENG)

a <- cv_reactions %>% filter(SOC_NAME_ENG == "Eye disorders") %>% select(REPORT_ID, PT_NAME_ENG, SOC_NAME_ENG) %>% collect()
b <- dplyr::summarise(group_by(a,PT_NAME_ENG),count=n_distinct(REPORT_ID))
head(b)

date <- as.POSIXct("2015-12-01")
date1 <- floor_date(date,"year")
date2 <- format(date, "%Y")



current_ingd <- "infliximab"
current_year <- "2015"

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_reports <- tbl(hcopen, "cv_reports") 
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen,"cv_report_drug")
cv_reactions <- tbl(hcopen,"cv_reactions") 
####################################################################################################################

current_ingd <- "acetaminophen"
current_year <- "2015"



####################################################################################################################
################################################ Disproportionality analysis using BCPNN ###############################################
current_date_range <- c(ymd("20130331", ymd("20150331")))

library(dplyr)
part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% inner_join(cv_report_drug) %>% 
          dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
          inner_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table(n=-1)

part2 <- cv_reports  %>% 
          filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
          dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table(n=-1)

DISP_final <- dplyr::summarise(group_by(inner_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% collect()

library(PhViD)
bayes_table <- as.PhViD(DISP_final, MARGIN.THRES = 1) 
bayes_result <- BCPNN(bayes_table, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
str(bayes_result)
summary(bayes_result)

bayes_result$INPUT.PARAM
head(bayes_result$ALLSIGNALS)  
head(bayes_result$SIGNALS)     
bayes_result$NB.SIGNALS

str(bayes_table$data)
bayes_table$data[1:2,]
a <- as.data.frame(bayes_result$SIGNAL)
head(a)
####################################################################################################################
test <- DISP_final[DISP_final$ACTIVE_INGREDIENT_NAME == "&1-proteinase inhibitor (human)"]
sum(test$count)
test1 <- DISP_final[DISP_final$PT_NAME_ENG == "Chills"]
sum(test1$count)

N <- sum(DISP_final$count)

head(bayes_table)



bayes_table1 <-bayes_table %>% as.data.table(n=-1)
str(bayes_table1)
bayes_table1$data[2]


IC <- log((68/491363)/((145/491363)*(1742/491363)),exp(2))
IC


All_Sig <- bayes_result$ALLSIGNALS %>% as.data.table(n=-1)
tail(All_Sig)
test <- All_Sig[All_Sig$Q_0.025(log(IC)) == 0.04,]
str(All_Sig)


DISP_final[DISP_final$ACTIVE_INGREDIENT_NAME == "aminocaproic acid"]
any(DISP_final$ACTIVE_INGREDIENT_NAME == "(1s, 2s)-2-methylamino-1-phenylpropan-1-ol hydrochloride")
head(DISP_final)
write.csv(DISP_final, file = "test.csv")


(49*446)/491363

(145*1742)/491363

1742/491363

68/0.5140599

0.2922556 +0.6655419 












