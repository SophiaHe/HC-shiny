############# check for orginal dataset #############
a <- as.numeric(levels(as.factor(cv_reactions$REPORT_ID))) %>% as.data.table(n=-1) # levels of REPORT_ID in cv_reactions
head(a)
a1 <- cv_drug_rxn[is.na(cv_drug_rxn$PT_NAME_ENG)==TRUE,]$REPORT_ID%>% as.data.table(n=-1) # levels of REPORT_ID in cv_drug_rxn
head(a1)

a2 <- a1$. %in% a$. 
table(a2) # 652 report_id in cv_drug_rxn are not in cv_reactions

cv_reactions <- cv_reactions %>% as.data.table(n=-1)
cv_reactions[cv_reactions$REPORT_ID == 89,]

cv_report_drug <-cv_report_drug %>% as.data.table(n=-1)
cv_reports <-cv_reports %>% as.data.table(n=-1)

b <- as.numeric(levels(as.factor(cv_report_drug$REPORT_ID))) %>% as.data.table(n=-1)# levels of REPORT_ID in cv_report_drug
b2 <- b$. %in% a$.
table(b2)  # 627 FALSE: 637 report_id in cv_report_drug are NOT in cv_reactions
b3 <- grep("FALSE",b2 )
b3
c <- as.numeric(levels(as.factor(cv_reports$REPORT_ID))) %>% as.data.table(n=-1)# levels of REPORT_ID in cv_reports
c2 <- c$. %in% a$.
table(c2) # 637 FALSE: 637 report_id in cv_reports are NOT in cv_reactions





################################################ Disproportionality analysis (new) using BCPNN ###############################################
head(cv_subtances)
head(cv_drug_rxn)
str(cv_drug_rxn)
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")
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
for(i in 1:length(quarters)){
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
str(bayes_result_final)

head(bayes_result_final$`BCPNN - Quarter: 1977.3`)

# add quarter column to each element in bayes_PRR_result_final list
for(i in 1:length(quarters)){
  bayes_result_final[[i]] <- Map(cbind, bayes_result_final[[i]], quarter = quarters[i])
}

# merge all elements in bayes_PRR_result_final list
bayes_result_all <-  ldply(bayes_result_final, data.frame)
head(bayes_PRR_result_all)
tail(bayes_PRR_result_all)
bayes_PRR_result_all[1:100,]
save(bayes_PRR_result_all, file="bayes_PRR_result_all.RData")

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


DISP_final <- dplyr::summarise(group_by(cv_drug_rxn,quarter,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame(n=-1)
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
  #bayes_PRR_result[i] <- list(PRR(bayes_PRR_table[[i]], RR0=1, MIN.n11 = 3, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))
  #bayes_PRR_result_final[[paste("PRR - Quarter: ",quarters[i], sep="")]] <- list(bayes_PRR_result[[i]]$ALLSIGNALS)
}

bayes_PRR_result[1] <- list(PRR(bayes_PRR_table[[1]], RR0=1, MIN.n11 = 3, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))

#save(bayes_PRR_result_final, file = "bayes_result_PRR_final.RData")
str(DISP_PRR_table)
head(DISP_PRR_table[1])

str(bayes_PRR_table)
head(bayes_PRR_table[[100]]$data)

str(bayes_PRR_result)

str(bayes_PRR_result_final$`PRR - Quarter: 2015.1`)
names(bayes_PRR_result_final)
str(bayes_PRR_result_final)
sum(lengths(bayes_PRR_result_final))



head(bayes_PRR_result_final[["PRR - Quarter: 1985.3"]])
head(bayes_PRR_result_final$`PRR - Quarter: 1966.2`, n=6L)
class(bayes_PRR_result_final)

# add quarter column to each element in bayes_PRR_result_final list
for(i in 1:length(quarters)){
  bayes_PRR_result_final[[i]] <- Map(cbind, bayes_PRR_result_final[[i]], quarter = quarters[i])
}

# merge all elements in bayes_PRR_result_final list
bayes_PRR_result_all <-  ldply(bayes_PRR_result_final, data.frame)
head(bayes_PRR_result_all)
tail(bayes_PRR_result_all)
bayes_PRR_result_all[1:100,]
save(bayes_PRR_result_all, file="bayes_PRR_result_all.RData")

str(bayes_PRR_result_final$`PRR - Quarter: 2000.4`)
sum(bayes_PRR_result_all$.id == "PRR - Quarter: 2000.4")

str(bayes_PRR_result_all)
length(levels(as.factor(DISP_final$ing)))
length(levels(as.factor(DISP_final$PT_NAME_ENG)))



prr <- cv_drug_rxn %>%
  dplyr::count(quarter, ing, PT_NAME_ENG) %>%
  ungroup() %>%
  group_by(quarter) %>%
  do(as.PhVid_SHe(data,MARGIN.THRES=1) %>%
       PRR(RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2) %>%
       magrittr::extract2("ALLSIGNALS"))

test <- as.PhVid_SHe(data=DISP_final,MARGIN.THRES=1)
test1 <- as.PhViD(DISP_final1,MARGIN.THRES=1)

type <- DISP_final %>% group_by(ing,PT_NAME_ENG) %>% tally()
head(type)
type1 <- DISP_final1 %>% group_by(ing,PT_NAME_ENG) %>% tally()
head(type1)

DISP_final1[DISP_final1$ing == "ABACAVIR" & DISP_final1$PT_NAME_ENG == "Abdominal discomfort",]
######################################## as.PhVid_SHe function for margin.thres > 1 #################################################
MARGIN.THRES <- 3
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
    
    RES2$data <- output %>% left_join(output1)  %>% distinct(ing, PT_NAME_ENG)%>% select(n11, n1.,n.1)
    
    RES2$L <- data.frame(data_final %>% select(ing,PT_NAME_ENG))
    RES2$N <- sum(data_final$n11)
    
  } else {
    n1._df <- aggregate(count~ing,data,sum)  %>% dplyr::rename(n1. = count) %>% as.data.table(n=-1)#9223*2
    n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% dplyr::rename(n.1 = count)%>% as.data.table(n=-1) # 3170*2
    
    DISP_final[is.na(DISP_final$PT_NAME_ENG)== TRUE, ]
    
    df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter)
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)

    
    # RES$data as a dataframe
    RES2$data <- output %>% left_join(output1) %>% distinct(ing, PT_NAME_ENG) %>% dplyr::select(n11, n1., n.1)

    RES2$N <- sum(df$n11)
    
    RES2$L <- data.frame(output %>% left_join(output1)  %>% distinct(ing, PT_NAME_ENG) %>% dplyr::select(ing,PT_NAME_ENG))
  }
  return(RES2)
}


####################################################################################################################
