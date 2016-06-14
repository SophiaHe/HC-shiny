# connect to CV database
hcopen <- dbConnect(PostgreSQL(), host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
dbListTables(hcopen)

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")



current_brand <- "REMICADE"
current_rxn <- "Drug ineffective"
current_gender <- "All"
current_date_range <- c(ymd("19650101", ymd("20160527")))



####################################################################################################################
# RAMICADE vs. HUMIRA ([CV_Shiny_DO SHe.R#485], <-.data.frame: replacement has 1 row, data has 0) SOLVED!!!!
# DIGOXIN: [CV_Shiny_DO SHe.R#492], Error in if: missing value where TRUE/FALSE needed SOLVED!!!


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



########################################################################################################################################
################################################ Disproportionality analysis using BCPNN ###############################################
#current_date_range <- c(ymd("20140101", ymd("20140331")))

library(dplyr)
library(PhViD)
BCPNN_signal <- function(current_date_range){
  part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% inner_join(cv_report_drug) %>% 
            dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
            inner_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table(n=-1)
  
  part2 <- cv_reports  %>% 
            filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
            dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table(n=-1)
  
  DISP_final <- dplyr::summarise(group_by(inner_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.table(n=-1)
  
  
  bayes_table_BCPNN <- as.PhViD(DISP_final, MARGIN.THRES = 1) 
  bayes_result <- BCPNN(bayes_table_BCPNN, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
  #signals <- as.data.table(bayes_result$SIGNAL)
  signals <-  bayes_result$SIGNAL %>% collect()
  signals_final <- signals %>% mutate(D_AR_Comb = paste(signals$`drug code`, " * ", signals$`event effect`)) %>% arrange(desc(`Q_0.025(log(IC))`)) %>% top_n(10,wt=`Q_0.025(log(IC))`)
  
  signals_plot <- gvisBarChart(signals_final,
                               xvar = "D_AR_Comb",
                               yvar = "Q_0.025(log(IC))",
                               options = list(
                                 #vAxes="[{title:'D*AR Combination'}",
                                 legend = "{position:'none'}",
                                 bars = 'horizontal',
                                 # axes= "x: {
                                 #   0: { side: 'top', label: 'Number of Reports'}}",
                                 bar = list(groupWidth =  '90%'),
                                 height=500,
                                 vAxis.textStyle = "{color:'black',fontName:'Courier',fontSize:5}",
                                 title = "Top 10 Signals Detected",
                                 hAxes="[{title:'Strength of Signal'}]"
                               )
                  )
  strength_plot <- plot(signals_plot)
  return(strength_plot)
}

BCPNN_signal(current_date_range=c(ymd("20140101", ymd("20140331"))))

########################################################### Explorative analysis on BCPNN function #####################################################

str(bayes_result)
summary(bayes_result)

bayes_result$INPUT.PARAM
head(bayes_result$ALLSIGNALS)  
head(bayes_result$SIGNALS)     
bayes_result$NB.SIGNALS

str(bayes_table$data)
bayes_table$data[1:2,]
a <- as.data.table(bayes_result$SIGNAL)


  b<- a %>% mutate(D_AR_Comb = paste(a$`drug code`, " * ", a$`event effect`))
  b$`Q_0.025(log(IC))`
head(a)

test <- DISP_final[DISP_final$ACTIVE_INGREDIENT_NAME == "&1-proteinase inhibitor (human)"]
sum(test$count)
test1 <- DISP_final[DISP_final$PT_NAME_ENG == "Chills"]
sum(test1$count)

N <- sum(DISP_final$count)

head(bayes_table)



bayes_table1 <-bayes_table %>% as.data.table(n=-1)
str(bayes_table1)
bayes_table1$data[2]

norms <- rnorm(100,mean=9,sd=2)
norms2 <- rnorm(100, mean=4, sd=2)

qnorm(0.025,mean=4,sd=2)


IC <- log((68/491363)/((145/491363)*(1742/491363)),exp(2))
IC


All_Sig <- bayes_result$ALLSIGNALS %>% as.data.table(n=-1)
tail(All_Sig)
test <- All_Sig[All_Sig$Q_0.025(log(IC)) == 0.04,]
str(All_Sig)


DISP_final[DISP_final$ACTIVE_INGREDIENT_NAME == "aminocaproic acid"]
any(DISP_final$ACTIVE_INGREDIENT_NAME == "(1s, 2s)-2-methylamino-1-phenylpropan-1-ol hydrochloride")
head(DISP_final)

DISP_final1 <- DISP_final[1:1000,]
write.csv(DISP_final, file = "test.csv")


(49*446)/491363

(145*1742)/491363

1742/491363

68/0.5140599

0.2922556 +0.6655419 

####################################################################################################################

################################################ Disproportionality analysis using PRR ###############################################
current_date_range <- c(ymd("20140101", ymd("20140331")))

part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% inner_join(cv_report_drug) %>% 
  dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
  inner_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table(n=-1)

part2 <- cv_reports  %>% 
  filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
  dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table(n=-1)

DISP_final <- dplyr::summarise(group_by(inner_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.table(n=-1)


bayes_table_PRR <- as.PhViD(DISP_final, MARGIN.THRES = 1) 

PRR_results <- PRR(bayes_table_PRR, RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2)

PRR_results$INPUT.PARAM
head(PRR_results$ALLSIGNALS)  # LB95(log(PRR)) might be Inf because C=0 in the contingency table
head(PRR_results$SIGNALS)     # FDR can only be estimated when the stats of interest is p-value (in this case, it's the LB95(log(PRR)))
PRR_results$NB.SIGNALS


# bar chart for PRR methid
signals <-  PRR_results$SIGNAL %>% collect()
signals_final <- signals %>% mutate(D_AR_Comb = paste(signals$`drug code`, " * ", signals$`event effect`)) %>% 
                  arrange(desc(`LB95(log(PRR))`)) %>% top_n(10,wt=`LB95(log(PRR))`)

signals_plot <- gvisBarChart(signals_final,
                             xvar = "D_AR_Comb",
                             yvar = "LB95(log(PRR))",
                             options = list(
                               #vAxes="[{title:'D*AR Combination'}",
                               legend = "{position:'none'}",
                               bars = 'horizontal',
                               # axes= "x: {
                               #   0: { side: 'top', label: 'Number of Reports'}}",
                               bar = list(groupWidth =  '90%'),
                               height=500,
                               vAxis.textStyle = "{color:'black',fontName:'Courier',fontSize:5}",
                               title = "Top 10 Signals Detected By PRR Method",
                               hAxes="[{title:'Strength of Signal'}]"
                             )
)
strength_plot <- plot(signals_plot)

# bubble chart with x-axis= PRR & y-axis = LB95(log(PRR)) & bubble is count/number of cases for D*AR pair
PRR_bubble_df <- PRR_results$SIGNALS %>% collect() 
PRR_bubble_df_final <- PRR_bubble_df %>% mutate(D_AR_Comb = paste(PRR_bubble_df$`drug code`, " * ", PRR_bubble_df$`event effect`))%>% 
                        arrange(desc(`LB95(log(PRR))`)) %>% top_n(200,wt=`LB95(log(PRR))`)


  
%>% dplyr::select(count,`LB95(log(PRR))`,PRR)

PRR_bubble_plot <- ggplot(PRR_bubble_df_final,aes(x=PRR,y=`LB95(log(PRR))`, size=count, label=`event effect`, colour= PRR))+
                    geom_point()+
                    geom_text(size=3, check_overlap=TRUE, nudge_y=-0.03)+
                    scale_size_area(max_size = 15)+
                    scale_x_continuous(name="PRR")+
                    scale_y_continuous(name="Lower Bound of 95% Confidence Interval of log(PRR)") +theme_bw()
PRR_bubble_plot


PRR_bubble_df_final1 <- PRR_bubble_df %>% mutate(D_AR_Comb = paste(PRR_bubble_df$`drug code`, " * ", PRR_bubble_df$`event effect`))%>% 
  arrange(desc(`LB95(log(PRR))`)) %>% top_n(20,wt=`LB95(log(PRR))`) %>% collect()

PRR_GoogBubbleChart <- gvisBubbleChart(data = PRR_bubble_df_final1, idvar="D_AR_Comb", xvar="PRR", yvar="LB95(log(PRR))", sizevar="PRR",
                                       options=list(
                                         height= 1000
                                       )
                                       )
plot(PRR_GoogBubbleChart)

, colorvar="PRR", sizevar="count"
