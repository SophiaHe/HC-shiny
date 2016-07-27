############# check for orginal dataset #############
cv_reactions <- cv_reactions %>% as.data.table(n=-1)


cv_report_drug <-cv_report_drug %>% as.data.table(n=-1)
cv_reports <-cv_reports %>% as.data.table(n=-1)

a <- as.numeric(levels(as.factor(cv_reactions$REPORT_ID))) %>% as.data.table(n=-1) # levels of REPORT_ID in cv_reactions: 440722
head(a)
a1 <- cv_drug_rxn[is.na(cv_drug_rxn$PT_NAME_ENG)==TRUE,]$REPORT_ID%>% distinct() %>% as.data.table(n=-1) # number of na in PT_NAME_END in cv_drug_rxn: 652
a1 <- unique(a1) %>% as.data.table(n=-1) # 520 reports that are in cv_drug_rxn but not in cv_reactions
head(a1) 
a1 # REPORT_ID of reports that are in cv_drug_rxn but not in cv_reactions

a2 <- a1$. %in% a$. 
table(a2) # 520 report_id in cv_drug_rxn are not in cv_reactions


b <- as.numeric(levels(as.factor(cv_report_drug$REPORT_ID))) %>% as.data.table(n=-1)# levels of REPORT_ID in cv_report_drug: 441294
b2 <- b$. %in% a$.
table(b2)  # 627 FALSE: 627 report_id in cv_report_drug are NOT in cv_reactions
b3 <- grep("FALSE",b2 ) # index of the reports in cv_report_drug that missing in cv_reactions but presented in cv_report_drug
b3

ID_NA_b3 <- b[b3]
ID_NA_b3 # 627 REPORT_ID of reports that are in cv_report_drug but not in cv_reactions

c <- as.numeric(levels(as.factor(cv_reports$REPORT_ID))) %>% as.data.table(n=-1)# levels of REPORT_ID in cv_reports:441359
c2 <- c$. %in% a$.
table(c2) # 637 FALSE: 637 report_id in cv_reports are NOT in cv_reactions
c3 <- grep("FALSE", c2) # index of the reports in cv_reports that missing in cv_reactions but presented in cv_reports

ID_NA_c3 <- c[c3]
ID_NA_c3 # 637 REPORT_ID of reports that are in cv_reports but not in cv_reactions
cv_reactions[cv_reactions$REPORT_ID == 86,]
cv_reports[cv_reports$REPORT_ID == 86,]

################################################ Disproportionality analysis (new) using BCPNN ###############################################
head(cv_subtances)
head(cv_drug_rxn)
str(cv_drug_rxn)
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")
cv_drug_rxn <- cv_drug_rxn %>% as.data.table(n=-1)
# save(cv_drug_rxn, file = "cv_drug_rxn.RData")

DISP <- cv_drug_rxn %>% filter(is.na(cv_drug_rxn$PT_NAME_ENG)==FALSE) %>% dplyr::group_by(quarter)%>% as.data.table(n=-1)
head(DISP)
tail(DISP)

cv_drug_rxn[is.na(cv_drug_rxn$quarter)==TRUE,]

# need to get rid of NA in PT_NAME_ENG in cv_drug_rxn
DISP_final <- dplyr::summarise(group_by(DISP,quarter,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame(n=-1)
save(DISP_final, file="DISP_final.RData")

head(DISP_final)
class(DISP_final$quarter)
DISP_final[is.na(DISP_final$quarter)==TRUE,]

# all levels of quarters
DISP_final$quarter <- as.factor(DISP_final$quarter)
quarters <- levels(DISP_final$quarter)

# create lists
DISP_table <- vector(mode = "list") # frequency table filtered by quarter
bayes_table <- vector(mode = "list") # as.PhVid_SHe for each quarter of frequency table
bayes_result <- vector(mode = "list") # BCPNN on each bayes_table
bayes_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_result


for(i in 1:length(quarters)){
  DISP_table[i]<-list(DISP_final %>% filter(quarter == quarters[i]))
  bayes_table[i] <- list(as.PhVid_SHe(data=as.data.frame(DISP_table[i]),MARGIN.THRES = 1))
  #bayes_result[i] <- list(BCPNN(bayes_table[[i]], RR0 = 1, MIN.n11 = 1, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=TRUE))
  #bayes_result_final[[paste("BCPNN - Quarter: ",quarters[i], sep="")]] <- list(bayes_result[[i]]$ALLSIGNALS)
}

for(i in 184:length(quarters)){
  bayes_result[i] <- list(BCPNN(bayes_table[[i]], RR0 = 1, MIN.n11 = 1, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=TRUE))
}

for(i in 1:length(quarters)){
  bayes_result_final[[paste("BCPNN - Quarter: ",quarters[i], sep="")]] <- list(bayes_result[[i]]$ALLSIGNALS)
}

bayes_result[[1]]$SIGNALS
head(bayes_table[[2]])
str(bayes_result_final)
head(bayes_table[[1]]$data)
head(bayes_table[[1]]$L)
ex <- cbind(head(bayes_table[[1]]$L),head(bayes_table[[1]]$data))

save(ex, file = "ex.RData")

# add quarter column to each element in bayes_result_final list
for(i in 1:length(quarters)){
  bayes_result_final[[i]] <- Map(cbind, bayes_result_final[[i]], quarter = quarters[i])
}

str(bayes_table)

# merge all elements in bayes_PRR_result_final list
bayes_result_all <-  ldply(bayes_result_final, data.frame)

# change ID variable to just quarter and make event.effect to upper case
bayes_result_all$.id <- str_sub(bayes_result_all$.id,-6,-1)
bayes_result_all$event.effect <- toupper(bayes_result_all$event.effect)

head(bayes_result_all)
tail(bayes_result_all)
bayes_result_all[1:100,]
save(bayes_result_all, file="BCPNN_0712.RData")


# D*AR paris with vaccine
vaccine <- DISP_final[grep("VACCINE", DISP_final$ing), ]

# DISP_final_VF is DISP_final with vaccine-free pairs
DISP_final_VF <- anti_join(DISP_final, vaccine)

for(i in 1:length(quarters)){
  DISP_table[i]<-list(DISP_final_VF %>% filter(quarter == quarters[i]))
}




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
DISP <- cv_drug_rxn %>% filter(is.na(cv_drug_rxn$PT_NAME_ENG)==FALSE) %>% dplyr::group_by(quarter)%>% as.data.table(n=-1)
head(DISP)
tail(DISP)

cv_drug_rxn[is.na(cv_drug_rxn$quarter)==TRUE,]

DISP_final <- dplyr::summarise(group_by(DISP,quarter,ing,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame(n=-1)
head(DISP_final)
class(DISP_final$quarter)
DISP_final[is.na(DISP_final$quarter)==TRUE,]

# all levels of quarters
DISP_final$quarter <- as.factor(DISP_final$quarter)
quarters <- levels(DISP_final$quarter)
class(quarters)
quarters[1]

# create lists
DISP_PRR_table <- vector(mode = "list") # frequency table filtered by quarter
bayes_PRR_table <- vector(mode = "list") # as.PhVid_SHe for each quarter of frequency table
bayes_PRR_result <- vector(mode = "list") # PRR on each bayes_table
bayes_PRR_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_result


for(i in 1:length(quarters)){
  DISP_PRR_table[i]<-list(DISP_final %>% filter(quarter == quarters[i]))
  #bayes_PRR_table[i] <- list(as.PhVid_SHe(data=as.data.frame(DISP_PRR_table[i]),MARGIN.THRES = 1))
  #bayes_PRR_result[i] <- list(PRR(bayes_PRR_table[[i]], RR0=1, MIN.n11 = 3, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))
  #bayes_PRR_result_final[[paste("PRR - Quarter: ",quarters[i], sep="")]] <- list(bayes_PRR_result[[i]]$ALLSIGNALS)
}

bayes_PRR_result[1]<- list(PRR(bayes_PRR_table[[1]], RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))

for(i in 1:length(quarters)){
  bayes_PRR_table[i] <- list(as.PhVid_SHe(data=as.data.frame(DISP_PRR_table[i]),MARGIN.THRES = 1))
  #bayes_PRR_result[i] <- list(PRR(bayes_PRR_table[[i]], RR0=1, MIN.n11 = 3, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))
  #bayes_PRR_result_final[[paste("PRR - Quarter: ",quarters[i], sep="")]] <- list(bayes_PRR_result[[i]]$ALLSIGNALS)
}

for(i in 1:length(quarters)){
  bayes_PRR_result[i] <- list(PRR(bayes_PRR_table[[i]], RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2))
  bayes_PRR_result_final[[paste("PRR - Quarter: ",quarters[i], sep="")]] <- list(bayes_PRR_result[[i]]$ALLSIGNALS)
}

#save(bayes_PRR_result_final, file = "bayes_result_PRR_final.RData")
str(DISP_PRR_table)
head(DISP_PRR_table[1])

str(bayes_PRR_table)
head(bayes_PRR_table[[100]]$data)

str(bayes_PRR_result_final)

str(bayes_PRR_result_final$`PRR - Quarter: 2015.1`)
names(bayes_PRR_result_final)
str(bayes_PRR_result_final)
sum(lengths(bayes_PRR_result_final))

# PRR use LB95(log(PRR)) > 0 as signal detection criteria!!!!!!!!!
bayes_PRR_result[[3]]$SIGNALS

head(bayes_PRR_result_final[["PRR - Quarter: 1985.3"]])
head(bayes_PRR_result_final$`PRR - Quarter: 1966.2`, n=6L)
class(bayes_PRR_result_final)

# add quarter column to each element in bayes_PRR_result_final list
for(i in 1:length(quarters)){
  bayes_PRR_result_final[[i]] <- Map(cbind, bayes_PRR_result_final[[i]], quarter = quarters[i])
}

# merge all elements in bayes_PRR_result_final list
bayes_PRR_result_all <-  ldply(bayes_PRR_result_final, data.frame)

# change ID variable to just quarter and make event.effect to upper case
bayes_PRR_result_all$.id <- str_sub(bayes_PRR_result_all$.id,-6,-1)
bayes_PRR_result_all$event.effect <- toupper(bayes_PRR_result_all$event.effect)

head(bayes_PRR_result_all)
tail(bayes_PRR_result_all)
bayes_PRR_result_all[1:100,]
save(bayes_PRR_result_all, file="PRR_0713.RData")

str(bayes_PRR_result_final$`PRR - Quarter: 2000.4`)
sum(bayes_PRR_result_all$.id == "PRR - Quarter: 2000.4")

str(bayes_PRR_result_all)
length(levels(as.factor(DISP_final$ing)))
length(levels(as.factor(DISP_final$PT_NAME_ENG)))



test <- as.PhVid_SHe(data=DISP_final,MARGIN.THRES=1)
test1 <- as.PhViD(DISP_final1,MARGIN.THRES=1)

type <- DISP_final %>% group_by(ing,PT_NAME_ENG) %>% tally()
head(type)
type1 <- DISP_final1 %>% group_by(ing,PT_NAME_ENG) %>% tally()
head(type1)

DISP_final1[DISP_final1$ing == "ABACAVIR" & DISP_final1$PT_NAME_ENG == "Abdominal discomfort",]


################################################ Disproportionality analysis (new) using ROR ###############################################
bayes_ROR_result <- vector(mode = "list") # ROR on each bayes_table
bayes_ROR_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_ROR_result


for(i in 1:length(quarters)){
  bayes_ROR_result[i] <- list(ROR(bayes_table[[i]], OR0 = 1, MIN.n11 = 1, DECISION = 3,DECISION.THRES = 0, RANKSTAT = 2))
  bayes_ROR_result_final[[paste("ROR - Quarter: ",quarters[i], sep="")]] <- list(bayes_ROR_result[[i]]$ALLSIGNALS)
}

b <- bayes_ROR_result[[1]]$ALLSIGNALS
max(b$`LB95(log(ROR))`)

bayes_ROR_result_final[[3]]
str(bayes_ROR_result_final)

# add quarter column to each element in bayes_ROR_result_final list
for(i in 1:length(quarters)){
  bayes_ROR_result_final[[i]] <- Map(cbind, bayes_ROR_result_final[[i]], quarter = quarters[i])
}

# merge all elements in bayes_PRR_result_final list
bayes_ROR_result_all <-  ldply(bayes_ROR_result_final, data.frame)

# change ID variable to just quarter and make event.effect to upper case
bayes_ROR_result_all$.id <- str_sub(bayes_ROR_result_all$.id,-6,-1)
bayes_ROR_result_all$event.effect <- toupper(bayes_ROR_result_all$event.effect)
bayes_ROR_result_all <- bayes_ROR_result_all %>% select(-quarter)

head(bayes_ROR_result_all)
tail(bayes_ROR_result_all)

bayes_ROR_result_all[1:50,]

save(bayes_ROR_result_all, file="bayes_ROR_result_all0714.RData")


################################################ Disproportionality analysis (new) using Gamma Poisson Shrinkage(GPS) ###############################################
#	propensity score matching

# write table to database
testconnect <- dbConnect(PostgreSQL(),host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
dbWriteTable(testconnect, "cv_bcpnn_160712", bayes_result_all)
dbWriteTable(testconnect, "cv_prr_160713", bayes_PRR_result_all)
dbWriteTable(testconnect, "cv_ror_160714", bayes_ROR_result_all)

dbRemoveTable(testconnect, "test")

############################################## Cumulative BCPNN ##############################################################
# create lists
bayes_cumulative_table1 <- vector(mode = "list") # as.PhVid_SHe for each quarter of frequency table
bayes_cumulative_table <- vector(mode = "list") # calculate cumulative n11, n1. & n.1 & format it into a list fro BCPNN process
bayes_cumulative_result <- vector(mode = "list") # BCPNN on each bayes_table
bayes_cumulative_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_result

# Cumulative as.PhVid_CUM1_SHe function1 to generate ing+AR+n11+n1.+n.1 dataframes within a list:bayes_cumulative_table1
for(i in 1:length(quarters)){
  bayes_cumulative_table1[[i]] <- as.PhVid_CUM1_SHe(data=as.data.frame(DISP_table[i]),MARGIN.THRES = 1)
}
bayes_cumulative_table1[[4]]

# merge elements of bayes_cumulative_table1 list together
bayes_cumulative_table1_all <-  ldply(bayes_cumulative_table1, data.frame)

# calculate cumulative n11, n1. & n.1
bayes_cumulative_table_final <- bayes_cumulative_table1_all %>% group_by(ing,PT_NAME_ENG) %>% mutate(n11_cum = cumsum(n11), n1._cum = cumsum(n1.), n.1_cum = cumsum(n.1)) %>%
                                select(quarter, ing, PT_NAME_ENG,n11_cum,n1._cum,n.1_cum) %>% dplyr::rename(n11 = n11_cum, n1. = n1._cum, n.1 = n.1_cum)
bayes_cumulative_table_final <- bayes_cumulative_table_final  %>% ungroup(ing, PT_NAME_ENG)

# a <- bayes_cumulative_table_final[bayes_cumulative_table_final$ing == "INDINAVIR" & bayes_cumulative_table_final$PT_NAME_ENG == "Nausea",]
# a

# format bayes_cumulative_table_final into a list (bayes_cumulative_table) for BCPNN process
bayes_cumulative_table <- vector(mode = "list")

for (i in 1:length(quarters)){
  bayes_cumulative_table[i] <- list(as.PhVid_CUM2_SHe(data=bayes_cumulative_table_final, i=i))
}


str(bayes_cumulative_table)
a <- bayes_cumulative_table[[2]]$data
a
# Run BCPNN process
bayes_cumulative_result <- vector(mode = "list") # BCPNN on each bayes_table
bayes_cumulative_result_final <- vector(mode = "list") # ALLSIGNALS of each bayes_result
test_list <- vector(mode="list")

for(i in 189:201){
  bayes_cumulative_result[i] <- list(BCPNN_SHe(bayes_cumulative_table[[i]], RR0 = 1, MIN.n11 = 1, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=TRUE, NB.MC=10000))
}
test_list[1] <- list(BCPNN(bayes_cumulative_table[[1]], RR0 = 1, MIN.n11 = 1, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=TRUE, NB.MC=10000))

test_list[1]
bayes_cumulative_table[[1]]$data
head(bayes_table[[1]]$data)
# extract ALLSIGNALS from each quarter & put it into a list
for(i in 1:length(quarters)){
  bayes_cumulative_result_final[[quarters[i]]] <- list(bayes_cumulative_result[[i]]$ALLSIGNALS)
}

# merge all elements in bayes_PRR_result_final list
bayes_cumulative_result_all <-  ldply(bayes_cumulative_result_final, data.frame)
#############################################################################################################################################################

#############################################################################################################################################################
MARGIN.THRES <- 3
data <- as.data.frame(DISP_table[2])

# Final non-cumulative as.phvid function
as.PhVid_SHe <- function(data, MARGIN.THRES=1){
  RES2 <- vector(mode = "list")
  
  n1._df <- aggregate(count~ing,data,sum)  %>% dplyr::rename(n1. = count) %>% as.data.table(n=-1)#9223*2
  n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% dplyr::rename(n.1 = count)%>% as.data.table(n=-1) # 3170*2
  
  
  df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter)
  output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
  output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
  
  test <- output %>% left_join(output1)  
  RES2$data  <- test %>% dplyr::filter(is.na(test$n.1)== FALSE & test$n1.>= MARGIN.THRES & test$n.1>= MARGIN.THRES) %>% dplyr::select(n11, n1., n.1)
  
  
  RES2$N <- sum(as.data.frame(RES2$data)$n11) 
  
  RES2$L <- test %>% dplyr::filter(is.na(test$n.1)== FALSE & test$n1.>= MARGIN.THRES & test$n.1>= MARGIN.THRES) %>% dplyr::select(ing,PT_NAME_ENG) %>% as.data.frame(n=-1)
  return(RES2)
}

# Cumulative as.PhVid_CUM1_SHe function1 to generate ing+AR+n11+n1.+n.1 dataframe
DISP_final <- DISP_final %>% filter(PT_NAME_ENG != "")
data <- as.data.frame(DISP_table[2])
MARGIN.THRES <- 1
as.PhVid_CUM1_SHe <- function(data, MARGIN.THRES=1){
  n1._df <- aggregate(count~ing,data,sum)  %>% dplyr::rename(n1. = count) %>% as.data.table(n=-1)#9223*2
  n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% dplyr::rename(n.1 = count)%>% as.data.table(n=-1) # 3170*2
  
  
  df <- data %>% dplyr::rename(n11 = count) #%>% dplyr::select(-quarter)
  output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
  output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
  
  test <- output %>% left_join(output1)  
  
  final_data  <- test %>% dplyr::filter(is.na(test$n.1)== FALSE & test$n1.>= MARGIN.THRES & test$n.1>= MARGIN.THRES) # %>% dplyr::select(n11, n1., n.1)
  
  return(final_data)
}

# as.PhVid_CUM2_SHe function to format bayes_cumulative_table_final into a list for BCPNN process
as.PhVid_CUM2_SHe <- function(data, i){
  RES3 <- vector(mode = "list")
  
  RES3$data <- data %>% dplyr::filter(quarter == quarters[1]) %>% select(n11,n1.,n.1)
  RES3$N <- sum(as.data.frame(RES3$data)$n11)
  RES3$L <- data %>% dplyr::filter(quarter == quarters[1]) %>% select(ing, PT_NAME_ENG)
  return(RES3)
}
######################################## as.PhVid_SHe function for margin.thres > 1 #################################################
MARGIN.THRES <- 1
data <- as.data.frame(DISP_table[1])

as.PhVid_SHe <- function(data, MARGIN.THRES=1){
  RES2 <- vector(mode = "list")
  
  # margin.thres
  if(MARGIN.THRES>1){
    # n1._df_name <- aggregate(count~ing,data,sum)  # 5143*2
    # n1._df_name_final <- n1._df_name %>% filter(count >= MARGIN.THRES)%>% dplyr::select(ing)%>% as.data.table(n=-1) # 4273*2
    # 
    # n.1_df_name <- aggregate(count~PT_NAME_ENG,data,sum) # 3116*2
    # n.1_df_name_final <- n.1_df_name %>% filter(count >= MARGIN.THRES)%>% dplyr::select(PT_NAME_ENG)%>% as.data.table(n=-1) #2840*2
    # 
    # df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter) # 300398
    # df1 <- df[(df$ing %in% n1._df_name_final$ing),] # 299129
    # data_final <- df1[(df1$PT_NAME_ENG %in% n.1_df_name_final$PT_NAME_ENG),] # 298698
    # 
    # n1._df <- aggregate(n11~ing,data_final,sum) %>% dplyr::rename(n1. = n11) %>% as.data.table(n=-1) 
    # n.1_df <- aggregate(n11~PT_NAME_ENG,data_final,sum)%>% dplyr::rename(n.1 = n11)%>% as.data.table(n=-1)
    # 
    # output <- data_final%>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    # output1 <- data_final %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
    
    n1._df <- aggregate(count~ing,data,sum)  %>% dplyr::rename(n1. = count) %>% as.data.table(n=-1)#9223*2
    n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% dplyr::rename(n.1 = count)%>% as.data.table(n=-1) # 3170*2
    
    #DISP_final[is.na(DISP_final$PT_NAME_ENG)== TRUE, ]
    
    df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter)
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)
    
    test <- output %>% left_join(output1)  
    RES2$data  <- test %>% dplyr::filter(is.na(test$n.1)== FALSE & test$n1.> MARGIN.THRES & test$n.1> MARGIN.THRES) %>% dplyr::select(n11, n1., n.1)

    RES2$L <- data.frame(data_final %>% select(ing,PT_NAME_ENG))
    RES2$N <- sum(data_final$n11)
    
    RES2$N <- sum(df$n11)
    
    RES2$L <- test %>% dplyr::filter(is.na(test$n.1)== FALSE & test$n1.> MARGIN.THRES & test$n.1> MARGIN.THRES) %>% dplyr::select(ing,PT_NAME_ENG) %>% as.data.frame(n=-1)
    
  } else {
    n1._df <- aggregate(count~ing,data,sum)  %>% dplyr::rename(n1. = count) %>% as.data.table(n=-1)#9223*2
    n.1_df <- aggregate(count~PT_NAME_ENG,data,sum)%>% dplyr::rename(n.1 = count)%>% as.data.table(n=-1) # 3170*2
    
    #DISP_final[is.na(DISP_final$PT_NAME_ENG)== TRUE, ]
    
    df <- data %>% dplyr::rename(n11 = count) %>% dplyr::select(-quarter)
    output <- df %>% dplyr::left_join(n1._df) %>% filter(is.na(n1.) == FALSE)
    output1 <- df %>% left_join(n.1_df)%>% filter(is.na(n.1) == FALSE)

    
    # RES$data as a dataframe
    test <- output %>% left_join(output1)   
    RES2$data <- test %>% dplyr::filter(is.na(test$n.1)== FALSE)%>%dplyr::select(n11, n1., n.1)
    
    RES2$N <- sum(df$n11)
    
    RES2$L <- test %>% dplyr::filter(is.na(test$n.1)== FALSE) %>% dplyr::select(ing,PT_NAME_ENG) %>% as.data.frame(n=-1)
  }
  return(RES2)
}






####################################################################################################################
bayes_PRR_result[1]<- PRR(bayes_PRR_table[[1]], RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2)

bayes_PRR_table[[2]]$data[is.na(bayes_PRR_table[[2]]$data)== TRUE,]
tail(bayes_table[[2]]$data)

function (DATABASE, RR0 = 1, MIN.n11 = 1, DECISION = 1, DECISION.THRES = 0.05, 
          RANKSTAT = 1) 
{
  require("LBE")
  if (RANKSTAT == 2 & DECISION == 1) 
    stop("The FDR can't be used as decision rule with this ranking Statistic")
  DATA <- DATABASE$data
  N <- DATABASE$N
  L <- DATABASE$L
  n11 <- DATA[, 1]
  n1. <- DATA[, 2]
  n.1 <- DATA[, 3]
  n10 <- n1. - n11
  n01 <- n.1 - n11
  n00 <- N - (n11 + n10 + n01)
  E <- n1. * n.1/N
  # if (MIN.n11 > 1) {
  #   E <- E[n11 >= MIN.n11]
  #   n1. <- n1.[n11 >= MIN.n11]
  #   n.1 <- n.1[n11 >= MIN.n11]
  #   n10 <- n10[n11 >= MIN.n11]
  #   n01 <- n01[n11 >= MIN.n11]
  #   n00 <- n00[n11 >= MIN.n11]
  #   LL <- data.frame(drugs = L[, 1], events = L[, 2], n11)
  #   LL1 <- LL[, 1][n11 >= MIN.n11]
  #   LL2 <- LL[, 2][n11 >= MIN.n11]
  #   rm(list = "L")
  #   L <- data.frame(LL1, LL2)
  #   n11 <- n11[n11 >= MIN.n11]
  # }
  Nb.Cell <- length(n11)
  logPRR <- log((n11/(n11 + n10))/(n01/(n01 + n00)))
  var.logPRR <- 1/n11 - 1/(n11 + n10) + 1/n01 - 1/(n01 + n00)
  pval.logPRR.uni <- 1 - pnorm(logPRR, log(RR0), sqrt(var.logPRR))
  petit_rankstat <- (logPRR - log(RR0))/sqrt(var.logPRR)
  pval.uni <- pval.logPRR.uni
  pval.uni[pval.uni > 1] <- 1
  pval.uni[pval.uni < 0] <- 0
  PVAL.UNI <- pval.uni
  LBE.res <- LBE(2 * apply(cbind(pval.uni, 1 - pval.uni), 1, 
                           min), plot.type = "none")
  pi.c <- LBE.res$pi0
  fdr <- pi.c * sort(pval.uni[pval.uni <= 0.5])/(c(1:sum(pval.uni <= 
                                                           0.5))/Nb.Cell)
  fdr <- c(fdr, pi.c/(2 * ((sum(pval.uni <= 0.5) + 1):Nb.Cell)/Nb.Cell) + 
             1 - sum(pval.uni <= 0.5)/((sum(pval.uni <= 0.5) + 1):Nb.Cell))
  FDR <- apply(cbind(fdr, 1), 1, min)
  if (RANKSTAT == 2) {
    FDR <- rep(NaN, length(n11))
  }
  LB <- qnorm(0.025, logPRR, sqrt(var.logPRR))
  if (RANKSTAT == 1) 
    RankStat <- PVAL.UNI
  if (RANKSTAT == 2) 
    RankStat <- LB
  if (DECISION == 1 & RANKSTAT == 1) 
    Nb.signaux <- sum(FDR <= DECISION.THRES)
  if (DECISION == 2) 
    Nb.signaux <- min(DECISION.THRES, Nb.Cell)
  if (DECISION == 3) {
    if (RANKSTAT == 1) 
      Nb.signaux <- sum(RankStat <= DECISION.THRES, na.rm = TRUE)
    if (RANKSTAT == 2) 
      Nb.signaux <- sum(RankStat >= DECISION.THRES, na.rm = TRUE)
  }
  RES <- vector(mode = "list")
  RES$INPUT.PARAM <- data.frame(RR0, MIN.n11, DECISION, DECISION.THRES, 
                                RANKSTAT)
  RES$ALLSIGNALS <- data.frame(L[, 1][order(petit_rankstat, 
                                            decreasing = TRUE)], L[, 2][order(petit_rankstat, decreasing = TRUE)], 
                               n11[order(petit_rankstat, decreasing = TRUE)], E[order(petit_rankstat, 
                                                                                      decreasing = TRUE)], RankStat[order(petit_rankstat, 
                                                                                                                          decreasing = TRUE)], exp(logPRR)[order(petit_rankstat, 
                                                                                                                                                                 decreasing = TRUE)], n1.[order(petit_rankstat, decreasing = TRUE)], 
                               n.1[order(petit_rankstat, decreasing = TRUE)], FDR)
  colnames(RES$ALLSIGNALS) <- c("drug code", "event effect", 
                                "count", "expected count", "p-value", "PRR", "drug margin", 
                                "event margin", "FDR")
  if (RANKSTAT == 2) {
    colnames(RES$ALLSIGNALS)[5] <- "LB95(log(PRR))"
  }
  RES$SIGNALS <- RES$ALLSIGNALS[1:Nb.signaux, ]
  RES$NB.SIGNALS <- Nb.signaux
  RES
}