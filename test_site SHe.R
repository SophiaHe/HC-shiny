# connect to CV database
hcopen <- dbConnect(PostgreSQL(), host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
dbListTables(hcopen)

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")



current_brand <- NA
current_rxn <- NA
current_gender <- "All"
current_date_range <- c(ymd("19650101", ymd("20160527")))



####################################################################################################################

####################################################################################################################
current_ingd <- "infliximab"
current_year <- "2015"

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_reports <- tbl(hcopen, "cv_reports") 
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen,"cv_report_drug")
cv_reactions <- tbl(hcopen,"cv_reactions") 
cv_subtances <- tbl(hcopen, "cv_substances")
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")
####################################################################################################################

current_ingd <- "acetaminophen"
current_year <- "2015"

################################################ Disproportionality analysis (new) using BCPNN ###############################################
head(cv_subtances)
head(cv_drug_rxn)
str(cv_drug_rxn)

cv_drug_rxn <- cv_drug_rxn %>% as.data.table(n=-1)
# save(cv_drug_rxn, file = "cv_drug_rxn.RData")


DISP <- cv_drug_rxn %>% dplyr::group_by(cv_drug_rxn, quarter)
head(DISP)
tail(DISP)

DISP_final <- dplyr::summarise(group_by(DISP,quarter,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame(n=-1)
head(DISP_final)

# all levels of quarters
DISP$quarter <- as.factor(DISP$quarter)
quarters <- levels(DISP$quarter)

# create lists
DISP_table <- vector(mode = "list") # frequency table filtered by quarter
bayes_table <- vector(mode = "list") # as.PhVid_SHe for each quarter of frequency table
bayes_result <- vector(mode = "list") # BCPNN on each bayes_table
bayes_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_result


for(i in 1:length(quarters)){
  DISP_table[i]<-list(DISP_final %>% filter(quarter == quarters[i]))
  bayes_table[i] <- list(as.PhVid_SHe(data=as.data.frame(DISP_table[i]),MARGIN.THRES = 1))
  bayes_result[i] <- list(BCPNN(bayes_table[[i]], RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE))
  bayes_result_final[[paste("BCPNN - Quarter: ",quarters[i], sep="")]] <- list(bayes_result[[i]]$ALLSIGNALS)
}

#save(bayes_result_final, file = "bayes_result_BCPNN_final.RData")
str(DISP_table)
head(DISP_table[1])

str(bayes_table)
head(bayes_table[[1]]$data)

str(bayes_result)

str(bayes_result_final)

head(bayes_result_final[[200]])



quarters <- c("1965.1","1965.2")
DISP_table1 <- vector(mode = "list")
bayes_table1 <- vector(mode = "list")
bayes_result1 <- vector(mode = "list")
bayes_result_final1 <- vector(mode = "list")
for(i in 1:length(quarters)){
  DISP_table1[i]<-list(DISP_final %>% filter(quarter == quarters[i]))
  bayes_table1[i] <- list(as.PhVid_SHe(data=as.data.frame(DISP_table1[i]),MARGIN.THRES = 3))
  bayes_result1[i] <- list(BCPNN(bayes_table1[[i]], RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE))
  bayes_result_final1[i] <- list(bayes_result1[[i]]$ALLSIGNALS)
}

str(DISP_table1)
head(DISP_table1[[1]])

str(bayes_table1)
head(bayes_table1[[1]]$data)

str(bayes_result1)

str(bayes_result_final1)

# benchmark as.PhVid_SHe
op <- microbenchmark(as.PhVid_SHe(data=DISP_final, MARGIN.THRES = 1),times=10L)
boxplot(op)

# benchmark as.PhViD
DISP_final1 <- DISP_final %>% select(-quarter)
op1 <- microbenchmark(as.PhViD(DATA.FRAME=DISP_final1, MARGIN.THRES = 1),times=10L)
boxplot(op1)

op_all <- op %>% full_join(op1)

benchmark_boxplot <-ggplot(op_all, aes(x=expr, y=time, fill=expr))+geom_boxplot()
benchmark_boxplot

################################################ Disproportionality analysis (new) using PRR ###############################################
DISP <- cv_drug_rxn %>% dplyr::group_by(cv_drug_rxn, quarter)%>% as.data.table(n=-1)
head(DISP)
tail(DISP)

DISP_final <- dplyr::summarise(group_by(DISP,quarter,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame(n=-1)
head(DISP_final)
class(DISP_final$quarter)

# all levels of quarters
DISP$quarter <- as.factor(DISP$quarter)
quarters <- levels(DISP$quarter)
class(quarters)
quarters[1]

# create lists
DISP_PRR_table <- vector(mode = "list") # frequency table filtered by quarter
bayes_PRR_table <- vector(mode = "list") # as.PhVid_SHe for each quarter of frequency table
bayes_PRR_result <- vector(mode = "list") # PRR on each bayes_table
bayes_PRR_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_result


for(i in 1:length(quarters)){
  DISP_PRR_table[i]<-list(DISP_final %>% filter(quarter == quarters[i]))
  bayes_PRR_table[i] <- list(as.PhVid_SHe(data=as.data.frame(DISP_PRR_table[i]),MARGIN.THRES = 1))
  bayes_PRR_result[i] <- list(PRR(bayes_PRR_table[[i]], RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))
  bayes_PRR_result_final[[paste("PRR - Quarter: ",quarters[i], sep="")]] <- list(bayes_PRR_result[[i]]$ALLSIGNALS)
}



#save(bayes_PRR_result_final, file = "bayes_result_PRR_final.RData")
str(DISP_PRR_table)
head(DISP_PRR_table[1])

str(bayes_PRR_table)
head(bayes_PRR_table[[1]]$data)

str(bayes_PRR_result)

str(bayes_PRR_result_final)
names(bayes_PRR_result_final)

head(bayes_PRR_result_final[[200]])


######################################## as.PhVid_SHe function for margin.thres > 1 #################################################
MARGIN.THRES <- 5
data <- DISP_final
data <- as.data.frame(DISP_table[1])

as.PhVid_SHe <- function(data, MARGIN.THRES=1){
  RES2 <- vector(mode = "list")
  
  # margin.thres
  if(MARGIN.THRES>1){
    n1._df_name <- aggregate(count~ing,data,sum)  # 5143*2
    n1._df_name_final <- n1._df_name %>% filter(count >= MARGIN.THRES)%>% dplyr::select(ing)%>% as.data.table(n=-1) # 4273*2
    
    n.1_df_name <- aggregate(count~PT_NAME_ENG,data,sum) # 3116*2
    n.1_df_name_final <- n.1_df_name %>% filter(count >= MARGIN.THRES)%>% dplyr::select(PT_NAME_ENG)%>% as.data.table(n=-1) #2840*2
    
    df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter) # 300398
    df1 <- df[(df$ing %in% n1._df_name_final$ing),] # 299129
    data_final <- df1[(df1$PT_NAME_ENG %in% n.1_df_name_final$PT_NAME_ENG),] # 298698
    
    n1._df <- aggregate(n11~ing,data_final,sum) %>% dplyr::rename(n1. = n11) %>% as.data.table(n=-1) 
    n.1_df <- aggregate(n11~PT_NAME_ENG,data_final,sum)%>% dplyr::rename(n.1 = n11)%>% as.data.table(n=-1)
    
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
    
    RES2$data <- output %>% left_join(output1)  %>% filter(is.na(n.1) == FALSE)%>% select(n11, n1.,n.1)
    RES2$L <- data.frame(data_final %>% select(ing,PT_NAME_ENG))
    RES2$N <- sum(data_final$n11)
    
  } else {
    n1._df <- aggregate(count~ing,data,sum)  %>% dplyr::rename(n1. = count) %>% as.data.table(n=-1)#5143*2
    n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% dplyr::rename(n.1 = count)%>% as.data.table(n=-1) # 3116*2
    
    df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter)
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
    
    # RES$data as a dataframe
    RES2$data <- output %>% left_join(output1)  %>% filter(is.na(n.1) == FALSE) %>% dplyr::select(n11, n1.,n.1)
    
    # RES$N
    RES2$N <- sum(df$n11)
    
    RES2$L <- data.frame(output %>% left_join(output1)  %>% filter(is.na(n.1) == FALSE) %>% dplyr::select(ing,PT_NAME_ENG))
  }
  return(RES2)
}


####################################################################################################################

################### test #############################################################################################################
test <- as.PhVid_SHe1(data=DISP_final)
head(test$L)  
head(test$data)    
test$N

test1 <- as.PhVid_SHe(data=DISP_final,MARGIN.THRES = 3)
class(test1)
head(test1$L)  
head(test1$data)    
test1$N
str(test1)

DISP_final1 <- dplyr::summarise(group_by(DISP,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.table(n=-1)
test_table <- as.PhViD(DISP_final1, MARGIN.THRES = 3) 
bayes_result$INPUT.PARAM
head(bayes_table$L)  
head(bayes_table$data)     
bayes_table$N
str(test_table)


bayes_result <- BCPNN(test1, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
bayes_result$INPUT.PARAM
head(bayes_result$ALLSIGNALS)  
head(bayes_result$SIGNALS)     
bayes_result$NB.SIGNALS

bayes_result1 <- BCPNN(test_table, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
bayes_result1$INPUT.PARAM
head(bayes_result1$ALLSIGNALS)  
head(bayes_result1$SIGNALS)     
bayes_result1$NB.SIGNALS
########################################################################################################################################

################################################ Disproportionality analysis (old) using BCPNN ###############################################
current_date_range <- c(ymd("20140101", ymd("20140331")))

library(dplyr)
library(PhViD)
BCPNN_signal <- function(current_date_range){
  part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% left_join(cv_report_drug) %>% 
    dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
    left_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table(n=-1)
  
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

########################################################### Explorative analysis (old) on BCPNN function #####################################################

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

################################################ Disproportionality analysis (old) using PRR ###############################################
current_date_range <- c(ymd("20140101", ymd("20140601")))

part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% left_join(cv_report_drug) %>% 
  dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% filter(is.na(ACTIVE_INGREDIENT_NAME) == FALSE) %>% 
  left_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) %>% filter(is.na(PT_NAME_ENG) == FALSE) 
  #%>% as.data.table(n=-1)

part1[is.na(part1$PT_NAME_ENG) == TRUE,]

part2 <- cv_reports  %>% 
  filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
  dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table(n=-1)

DISP_final <- dplyr::summarise(group_by(inner_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.table(n=-1)


bayes_table_PRR <- as.PhViD(DISP_final, MARGIN.THRES = 1) 
head(bayes_table_PRR$L)


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
                               hAxes="[{title:'Strength of Signal'}]",
                               series="{
                                  0: {color: '#6495ED'},
                                  3: {color: 'red'}
                               }"
                             )
)
plot(signals_plot)

# bubble chart with x-axis= PRR & y-axis = LB95(log(PRR)) & bubble is count/number of cases for D*AR pair
PRR_bubble_df <- PRR_results$SIGNALS %>% collect() 
PRR_bubble_df_final <- PRR_bubble_df %>% mutate(D_AR_Comb = paste(PRR_bubble_df$`drug code`, " & ", PRR_bubble_df$`event effect`))%>% 
                        arrange(desc(`LB95(log(PRR))`)) %>% top_n(5,wt=`PRR`)


  

PRR_bubble_plot <- ggplot(PRR_bubble_df_final,aes(x=PRR/100000,y=`LB95(log(PRR))`, size=count, label=`D_AR_Comb`, colour= PRR))+
                    geom_point()+
                    geom_text(size=3, check_overlap=TRUE, nudge_y=-0.03)+
                    scale_size_area(max_size = 15)+
                    scale_x_continuous(name="PRR")+
                    scale_y_continuous(name="Lower Bound of 95% Confidence Interval of log(PRR)") +theme_bw()
PRR_bubble_plot


PRR_bubble_df_final1 <- PRR_bubble_df %>% mutate(D_AR_Comb = paste(PRR_bubble_df$`drug code`, "&", PRR_bubble_df$`event effect`))%>% 
  arrange(desc(`PRR`)) %>% top_n(10,wt=`PRR`) %>% collect()



#PRR_bubble_df_final1[1,3] <- 30


PRR_GoogBubbleChart <- gvisBubbleChart(data = PRR_bubble_df_final1, idvar="D_AR_Comb", xvar="PRR", yvar="LB95(log(PRR))", sizevar="count",
                                         options=list(
                                           height= 1000,
                                           sizeAxis = '{minSize:1}',
                                           hAxis=paste("{title: 'Proportional Reporting Ratio (PRR)'}"),
                                           vAxis=paste("{title: 'Lower Bound of 95% Confidence Interval of log(PRR)'}"),
                                           bubble="{textStyle:{fontSize: '13'}}",
                                           colorAxis = "{legend:{position: 'top'},colors: ['#90EE90', 'red']}"
                                         )
                                       )
plot(PRR_GoogBubbleChart)

######################################################################
a <- as.data.frame(c(1,2,3))
b<- as.data.frame(c("a","b","c"))
c <- as.data.frame()
list <- list(a,b)
a1 <- as.data.frame(list[1])

size <- as.numeric(object.size(DISP_final))
size *2

######################################################################

####################################################################################################################


D <- c("D1","D2","D2","D3","D3")
AR<- c("AR1","AR1","AR2", "AR3","AR4")
C <- c(3,4,5,6,7)
df<- data.frame(D,AR,C, stringsAsFactors = FALSE)
df

test_table <- as.PhVid_SHe(df,MARGIN.THRES = 1)
test_table$L
test_table$data
test_table$N

as.PhViD

as.PhViD<- function (DATA.FRAME, MARGIN.THRES = 1) {
  #test
  DATA.FRAME <- df
  MARGIN.THRES = 3
  
  data <- DISP_final

  # convert D, AR to FACTOR & convert count to DOUBLE 
  data[, 1] <- as.factor(data[, 1])
  data[, 2] <- as.factor(data[, 2])
  data[, 3] <- as.double(data[, 3])
  
  # change count column name of DATA.FRAME to n11
  coln <- names(data)
  names(data)[3] <- "n11"
  
  # build contingency table: D * AR & caculate row total & column total
  data_cont <- xtabs(n11 ~ ., data = data)
  n1._mat <- apply(data_cont, 1, sum) # row total
  n.1_mat <- apply(data_cont, 2, sum) # column total
  
  #used to eliminate the drugs and the adverse events for which the marginal counts are less than MARGIN.THRES.
  if (MARGIN.THRES > 1) {
    while (sum(n1._mat < MARGIN.THRES) > 0 | sum(n.1_mat < MARGIN.THRES) > 0) { # sum() Logical true values are regarded as one, false values as zero. 
      data_cont <- data_cont[n1._mat >= MARGIN.THRES, ]
      data_cont <- data_cont[, n.1_mat >= MARGIN.THRES]
      n1._mat <- apply(data_cont, 1, sum)
      n.1_mat <- apply(data_cont, 2, sum)
    }
  }
  coord <- which(data_cont != 0, arr.ind = TRUE) # Give the TRUE indices of a logical object
  coord <- coord[order(coord[, 1]), ]
  
  Nb_n1. <- length(n1._mat) # number of types of drug in DATA.FRAME
  Nb_n.1 <- length(n.1_mat) # number of types of AR in DATA.FRAME
  
  libel.medoc <- rownames(data_cont)[coord[, 1]]
  libel.effet <- colnames(data_cont)[coord[, 2]]
  n11 <- data_cont[coord]
  N <- sum(n11)
  n1. <- n1._mat[coord[, 1]]
  n.1 <- n.1_mat[coord[, 2]]
  
  RES <- vector(mode = "list")
  RES$L <- data.frame(libel.medoc, libel.effet)
  colnames(RES$L) <- coln[1:2]
  RES$data <- cbind(n11, n1., n.1)
  rownames(RES$data) <- paste(libel.medoc, libel.effet)
  RES$N <- N
  RES
}

# RES$data is a matrix 
# RES$L is a data frame
# RES$N is a numeric value

a<- RES$data
a

######################################### as.PhVid_SHe function for margin.thres = 1 #######################################################
as.PhVid_SHe1 <- function(data){
  RES1 <- vector(mode = "list")
  
  # compile RES1$L
  RES1$L<- data.frame(data %>% select(ACTIVE_INGREDIENT_NAME,PT_NAME_ENG ))
  
  # margin.thres =1
  n1._df <- aggregate(count~ACTIVE_INGREDIENT_NAME,data,sum) %>% rename(n1. = count) %>% as.data.table(n=-1) #5143*2
  n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% rename(n.1 = count)%>% as.data.table(n=-1) # 3116*2
  
  results0 <- data %>% rename(n11 = count)
  
  results <- results0 %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
  
  results1 <- results0 %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
  
  # RES$data as a dataframe
  RES1$data <- results %>% left_join(results1)  %>% filter(is.na(n.1) == FALSE)

  # RES$N
  RES1$N <- sum(data$count)
  
  return(RES1)
}

######################################## as.PhVid_SHe function for margin.thres > 1 #################################################
MARGIN.THRES <- 3
data <- DISP_final

as.PhVid_SHe <- function(data, MARGIN.THRES){
  RES2 <- vector(mode = "list")
  
  # margin.thres
  if(MARGIN.THRES>1){
    n1._df_name <- aggregate(count~ing,data,sum)  # 5143*2
    n1._df_name_final <- n1._df_name %>% filter(count >= MARGIN.THRES)%>% select(ing)%>% as.data.table(n=-1) # 4273*2
    
    n.1_df_name <- aggregate(count~PT_NAME_ENG,data,sum) # 3116*2
    n.1_df_name_final <- n.1_df_name %>% filter(count >= MARGIN.THRES)%>% select(PT_NAME_ENG)%>% as.data.table(n=-1) #2840*2
    
    df <- data %>% rename(n11 = count) # 300398
    df1 <- df[(df$ing %in% n1._df_name_final$ing),] # 299129
    data_final <- df1[(df1$PT_NAME_ENG %in% n.1_df_name_final$PT_NAME_ENG),] # 298698

    n1._df <- aggregate(n11~ing,data_final,sum) %>% rename(n1. = n11) %>% as.data.table(n=-1) 
    n.1_df <- aggregate(n11~PT_NAME_ENG,data_final,sum)%>% rename(n.1 = n11)%>% as.data.table(n=-1)
    
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
    
    RES2$data <- output %>% left_join(output1)  %>% filter(is.na(n.1) == FALSE)%>% select(n11, n1.,n.1)
    RES2$L <- data.frame(data_final %>% select(ing,PT_NAME_ENG))
    RES2$N <- sum(data_final$n11)
    
  } else {
    n1._df <- aggregate(count~ing,data,sum) %>% rename(n1. = count) %>% as.data.table(n=-1) #5143*2
    n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% rename(n.1 = count)%>% as.data.table(n=-1) # 3116*2
    
    df <- data %>% rename(n11 = count)
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    ouput1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
    
    # RES$data as a dataframe
    RES2$data <- output %>% left_join(output1)  %>% filter(is.na(n.1) == FALSE) %>% select(n11, n1.,n.1)
    
    # RES$N
    RES2$N <- sum(df$n11)
    
    RES2$L <- data.frame(output %>% left_join(output1)  %>% filter(is.na(n.1) == FALSE) %>% select(ing,PT_NAME_ENG))
  }
  return(RES2)
}


####################################################################################################################
bayes_table1 <- as.PhVid_SHe(DISP_final,MARGIN.THRES = 3)
bayes_table1$N
bayes_results <- BCPNN(RES1, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)


test <- as.PhVid_SHe1(data=DISP_final)
head(test$L)  
head(test$data)    
test$N

test1 <- as.PhVid_SHe(data=DISP_final,MARGIN.THRES = 1)
head(test1$L)  
head(test1$data)    
test1$N
str(test1)

DISP_final1 <- dplyr::summarise(group_by(DISP,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.table(n=-1)
test_table <- as.PhViD(DISP_final1, MARGIN.THRES = 1) 
bayes_result$INPUT.PARAM
head(bayes_table$L)  
head(bayes_table$data)     
bayes_table$N
str(test_table)


bayes_result <- BCPNN(test1, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
bayes_result$INPUT.PARAM
head(bayes_result$ALLSIGNALS)  
head(bayes_result$SIGNALS)     
bayes_result$NB.SIGNALS

bayes_result1 <- BCPNN(test1, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
bayes_result1$INPUT.PARAM
head(bayes_result1$ALLSIGNALS)  
head(bayes_result1$SIGNALS)     
bayes_result1$NB.SIGNALS


############### test for inconsistency with orginal as.PhVid ####################
data <- DISP_final
n1._df_name <- aggregate(count~ing,data,sum)  # 5143*2
n1._df_name_final <- n1._df_name %>% filter(count >= MARGIN.THRES)%>% select(ing, count)%>% as.data.table(n=-1) # 4273*2
dim(n1._df_name_final)
head(n1._df_name_final)
head(n1._df_name)
DISP_final1[DISP_final1$ing == "tenecteplase"]
DISP_final1[DISP_final1$PT_NAME_ENG == "Intention tremor"]


n.1_df_name <- aggregate(count~PT_NAME_ENG,data,sum) # 3116*2
n.1_df_name_final <- n.1_df_name %>% filter(count >= MARGIN.THRES)%>% select(PT_NAME_ENG)%>% as.data.table(n=-1) #2840*2
dim(n.1_df_name_final)


df <- data %>% rename(n11 = count) # 300398
dim(df)
df1 <- df[(df$ing %in% n1._df_name_final$ing),] # 299129
dim(df1)
head(df1)
data_final <- df1[(df1$PT_NAME_ENG %in% n.1_df_name_final$PT_NAME_ENG),] # 298698
dim(data_final)

L <- data.frame(test_table$L)
test_df <- data_final[!(data_final$ing %in% L$ing),] # tenecteplase
test_df1 <- data_final[!(data_final$PT_NAME_ENG %in% L$PT_NAME_ENG),]

