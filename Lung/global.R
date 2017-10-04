
library(shiny)
library(googlesheets)
library(gplots)
library(ggplot2)
library(CluMix)
library(Amelia)
library(reshape)
library(imputeTS)
library(pspline)
library(DT)
#library(CluMix)
library(dendextend)
#library(ggdendro)
library(plyr)
library(survival)
library(ggfortify)
library(survminer)
#library(ggplot2)
library(ggdendro)
#library(plyr)
library(zoo)



si = sessionInfo()

source('functions.R')
source('defaults.R')





############ UPLOAD DATA ####################
g_sheet = F # decided is data is to be read from google sheets or not
if(g_sheet == T){
  googlesheets::gs_auth(token = 'shiny_app_token.rds')
  
  key = "1Bvwyd_TRH6M5bB3y6gBDYKYHpQXhox9L9NQTGTiV1IA"
  gs = gs_url("https://docs.google.com/spreadsheets/d/1Bvwyd_TRH6M5bB3y6gBDYKYHpQXhox9L9NQTGTiV1IA/edit?usp=sharing_eil&ts=59b28497")
  sheet_list = gs_ws_ls(gs)
  sheet_list
  clustering = as.data.frame(gs_read(ss=gs, ws= "For clustering"))
  colnames(clustering)
  #saveRDS(file = 'clustering3.rds',object = clustering)
  #clustering2 = clustering
}else{
  clustering = readRDS('clustering3.rds')
}

colnames(clustering)
#colnames(clustering2)
#setdiff(colnames(clustering),colnames(clustering2))
#setdiff(colnames(clustering2),colnames(clustering))


#boss = read.csv('BOS.csv',header = F,sep=';') # read boss table, generate from scratch
#m_boss = melt(boss)

# colnames(m_boss)
# colnames(clustering)
# colnames(clustering2)
# setdiff(colnames(clustering),colnames(clustering2))
# setdiff(colnames(clustering2),colnames(clustering))
#ggplot(m_boss) + geom_line(aes(x = variable,y = value,col = V1,group = V1))

#saveRDS(clustering,'clustering.rds')

#### append duplicates  ########
dup = duplicated(clustering[,1])
dups = clustering[duplicated(clustering[,1]),1]
row_names = clustering[,1]
row_names[dup] = paste(row_names[dup],'a',sep='_') 
row_names
length(row_names)

clustering = clustering[!is.na(row_names),]
dim(clustering)
row_names = row_names[!is.na(row_names)]
rownames(clustering) = row_names
clustering$MRN = row_names
length(row_names)
dim(clustering)

########### SEPARATE COLUMS by name #########################
colnames(clustering)
numeric_columns = c("MonthsToEvent","YearsToDeath","CRP",
                    "FEV1Ratio","pFEV1_neg24","pFEV1_neg18", "pFEV1_neg12", "pFEV1_neg6", "pFEV1_neg5", "pFEV1_neg4", "pFEV1_neg3"     
                    ,"pFEV1_neg2", "pFEV1_neg1", "pFEV1_0", "pFEV1_pos1", "pFEV1_pos2", "pFEV1_pos3"     
                    ,"pFEV1_pos4", "pFEV1_pos5", "pFEV1_pos6", "pFEV1_pos12", "pFEV1_pos18", "pFEV1_pos24"    
                    ,"pFEV1_pos36", "pFEV1_pos48", 
                    "BOS1mnth", "BOS2mnth", "BOS3mnth", 
                    "ChangeFEV1_12mth_prior", "ChangeFEV1_6mth_prior",  "ChangeFEV1_3mth_prior",  "ChangeFEV1_1mth_prior",  "ChangeFEV1_1mth_post",   "ChangeFEV1_3mth_post","ChangeFEV1_6mth_post")

add_numeric_columns = c("MonthsToEvent","YearsToDeath","BOS 3 free survival","CRP",
                        "FEV1Ratio")

pFEV_cols = c("pFEV1_neg24","pFEV1_neg18", "pFEV1_neg12", "pFEV1_neg6", "pFEV1_neg5", "pFEV1_neg4", "pFEV1_neg3"     
              ,"pFEV1_neg2", "pFEV1_neg1", "pFEV1_0", "pFEV1_pos1", "pFEV1_pos2", "pFEV1_pos3"     
              ,"pFEV1_pos4", "pFEV1_pos5", "pFEV1_pos6", "pFEV1_pos12", "pFEV1_pos18", "pFEV1_pos24"    
              ,"pFEV1_pos36", "pFEV1_pos48")
bos_cols = c("BOS1mnth", "BOS2mnth", "BOS3mnth")
change_cols = c("ChangeFEV1_12mth_prior", "ChangeFEV1_6mth_prior",  "ChangeFEV1_3mth_prior",  "ChangeFEV1_1mth_prior",  "ChangeFEV1_1mth_post",   "ChangeFEV1_3mth_post","ChangeFEV1_6mth_post")

numeric_select_columns = c("pFEV1_neg24","pFEV1_neg18", "pFEV1_neg12", "pFEV1_neg6", "pFEV1_neg5", "pFEV1_neg4", "pFEV1_neg3"     
                           ,"pFEV1_neg2", "pFEV1_neg1", "pFEV1_0", "pFEV1_pos1", "pFEV1_pos2", "pFEV1_pos3"     
                           ,"pFEV1_pos4", "pFEV1_pos5", "pFEV1_pos6", "pFEV1_pos12", "pFEV1_pos18", "pFEV1_pos24"    
                           ,"pFEV1_pos36", "pFEV1_pos48")


factor_columns = c("AltDxScore","DSA_HLAScore","HLAType","HLAStrength","BiopsyScore",
                   "SignOfInfection","NewCTChange","AGrade","BGrade","PathScore","ViralPCR",
                   "BactCulture","Methylpred","IVAbx")
#factor_columns_con = c("Status" , "DaysSinceOnset", "Obstructive")
factor_columns_con = c("Status" , "DaysSinceOnset", "Obst_post","BOS3orDeath")

date_columns = c("RxDate","DeathDate")

full_factor_columns = c('MRN',factor_columns,factor_columns_con)
full_factor_columns
full_factor_columns = full_factor_columns[order(match(full_factor_columns,colnames(clustering)))]

######################################



#### correct DATE columns ##########
clust_date = clustering[,date_columns]
clust_date[,"RxDate"]
new_date = as.Date(clust_date[,"RxDate"],'%d-%b-%y')
clust_date[,"RxDate"] = new_date
clust_date[,"DeathDate"]
new_date = as.Date(clust_date[,"DeathDate"],'%d-%b-%Y')
#new_date
clust_date[,"DeathDate"] = new_date
#### Not sure if these are actully reintegrated, but are applied below

##################################

#### BOSS COLUMNS ####
#bos_cols = c("BOS1mnth", "BOS2mnth", "BOS3mnth")

boss_data = clustering[,bos_cols]
boss_data_num = apply(boss_data, 2, function(x) as.numeric(as.character(x)))
boss_data_num


####### PROCESS ###################



############ FACTOR COLUMNS ###################
cluster_factor = clustering[,factor_columns]
clust_fac_con = clustering[,factor_columns_con]

convert_factors = T # convert word columns to lowercase
if(convert_factors==T){
  cluster_factor_con = apply(clust_fac_con, 2, function(x) tolower(factor(x)))
}else{
  cluster_factor_con = clust_fac_con
}


colnames(cluster_factor_con)

full_fac = cbind(cluster_factor,cluster_factor_con)
full_fac$MRN = clustering$MRN
full_fac_colnames = colnames(full_fac)[order(match(colnames(full_fac),colnames(clustering)))]
full_fac = full_fac[,full_fac_colnames] ### original factor columns 


### remove missing values from factor columsn #####
num_fac = full_fac
factor_NA_value = -2
for(col in factor_columns_con){
  num_fac[,col] = as.numeric(factor(num_fac[,col]))
  #num_fac[,col][is.na(num_fac_0[,col])] <- factor_NA_value
}

for(col in factor_columns){
  num_fac[,col] = as.numeric(num_fac[,col])
  #num_fac[,col][is.na(num_fac_0[,col])] <- factor_NA_value
}
num_fac_0 = num_fac[,full_factor_columns]
factor_NA_value = -2
num_fac_0[is.na(num_fac_0)] <- factor_NA_value

# factor dataframe with missing values 02

full_fac_0 = as.data.frame(apply(num_fac_0,2,function(x) factor(x)))


##### CONSOLIDATE DATA #############

#full_fac = cbind(full_fac,clust_date,boss_data_num)
#full_fac_0 = cbind(full_fac_0,clust_date,boss_data_num)

###################################################

##### DATE COMPUTATIONS ####

#full_fac_0$survival_time = full_fac_0$DeathDate - full_fac_0$RxDate
#full_fac_0$survival_time =  round((full_fac_0$DeathDate - full_fac_0$RxDate)/(365.25/12))
#full_fac_0$survival_time
########### NUMERIC COLUMN ###############

## numric column ##
clust_num = clustering[,numeric_columns]
for(num_col in numeric_columns){
  clust_num[,num_col] = as.numeric(clust_num[,num_col])
}

add_num = clustering[,add_numeric_columns]
for(num_col in add_numeric_columns){
  add_num[,num_col] = as.numeric(add_num[,num_col])
}


##### CONSOLIDATE DATA #############

full_fac = cbind(full_fac,clust_date,boss_data_num,add_num)
colnames(full_fac)
full_fac_0 = cbind(full_fac_0,clust_date,boss_data_num,add_num)
colnames(full_fac_0)
###################################################



#############################################


full_num = cbind(full_fac,clust_num)


### order ###
patient_list = full_num$MRN
patient_list = patient_list[order(patient_list)]


 



####################### pFEV ##################################

########## ADD missing pFEV columns ###################
pFEV = full_num[,pFEV_cols]
full_fac_0$pFEV_na <- apply(pFEV, 1, function(x) round((1-(sum(is.na(x))/length(x)))*100,1))



pFEV_w = full_num[,pFEV_cols]
pFEV_numeric_colnames_n = c(-24,-18,-12,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,12,18,24,36,48)
pFEV_numeric_colnames_f = c('-24','-18','-12','-6','-5','-4','-3','-2','-1','0','1','2','3','4','5','6','12','18','24','36','48')
colnames(pFEV_w) = pFEV_numeric_colnames_f

for(col in seq(-24,48,1)){
  if(!col %in% pFEV_numeric_colnames_n){
    pFEV_w[,paste(col)] = NA
  }
}
p_cols = colnames(pFEV_w)[order(as.numeric(colnames(pFEV_w)))]
pFEV_w = pFEV_w[,p_cols]
pFEV_w_original = pFEV_w
full_fac_0_original = full_fac_0
excluded_patients_c = full_fac_0$MRN[full_fac_0$pFEV_na < completeness]
excluded_patients_c = c(names(excluded_patients_c),patient_custom_exclude)
r_list = patient_list[!patient_list %in% excluded_patients_c]
r_list
pFEV_wf = cbind(full_fac_0,pFEV_w)
pFEV_wf_original = cbind(full_fac_0_original,pFEV_w_original)
pFEV_lf_original = melt(pFEV_wf_original, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))

pFEV_lf = melt(pFEV_wf, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))
pFEV_lf[,full_factor_columns] = apply(pFEV_lf[,full_factor_columns],2,function(x) factor(x))


 
####################### IMPUTE pFEV ####################### 
  
pFEV_ts = as.ts(t(pFEV_w))
pFEV_ts

i_pFEV_ts = na.interpolation(pFEV_ts)

i_pFEV_ts = as.data.frame(t(i_pFEV_ts))
colnames(i_pFEV_ts) = seq(-24,48,1)
rownames(i_pFEV_ts)


i_pFEV_wf = cbind(full_fac_0,i_pFEV_ts)
i_pFEV_lf = melt(i_pFEV_wf, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))
pFEV_lf[,full_factor_columns] = apply(pFEV_lf[,full_factor_columns],2,function(x) factor(x))

i_pFEV_lf$i = pFEV_lf$value
i_pFEV_lf$data = pFEV_lf$value
i_pFEV_lf$i[is.na(i_pFEV_lf$data)] = '0'
i_pFEV_lf$i[!is.na(i_pFEV_lf$data)] = '1'



 ############## differential ###################




time = as.numeric(colnames(i_pFEV_ts))

i_pFEV_sm = as.data.frame(t(apply(i_pFEV_ts[full_fac_0$pFEV_na >= completeness,],1, function(x) predict(sm.spline(time, as.numeric(x)))$ysmth)))
colnames(i_pFEV_sm) = time

i_pFEV_smf = cbind(full_fac_0[rownames(i_pFEV_sm),],i_pFEV_sm)
i_pFEV_sm_lf = melt(i_pFEV_smf, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))



############ D1 ###############
i_pFEV_sm_ts = as.ts(t(i_pFEV_sm))

i_pFEV_sm_ts_d1 = diff(i_pFEV_sm_ts)
i_pFEV_sm_d1 = as.data.frame(t(i_pFEV_sm_ts_d1))

colnames(i_pFEV_sm_d1) = time[-2]


i_pFEV_sm_d1_m = i_pFEV_sm_d1[,pFEV_numeric_colnames_f]

i_pFEV_sm_d1_mf = cbind(full_fac_0[rownames(i_pFEV_sm_d1_m),],i_pFEV_sm_d1_m)



i_pFEV_sm_d1_f = cbind(full_fac_0[rownames(i_pFEV_sm_d1),],i_pFEV_sm_d1)
i_pFEV_sm_d1_fl = melt(i_pFEV_sm_d1_f, id.vars = colnames(full_fac_0), measure.vars = colnames(i_pFEV_sm_d1))


################## D2 #######################
i_pFEV_sm_ts_d2 = diff(i_pFEV_sm_ts_d1)
i_pFEV_sm_d2 = as.data.frame(t(i_pFEV_sm_ts_d2))


colnames(i_pFEV_sm_d2) = colnames(i_pFEV_sm_d1)[-2]

rownames(i_pFEV_sm_d2) = rownames(i_pFEV_sm)

i_pFEV_sm_d2_m = i_pFEV_sm_d2[,pFEV_numeric_colnames_f]

i_pFEV_sm_d2_mf = cbind(full_fac_0[rownames(i_pFEV_sm_d1_m),],i_pFEV_sm_d1_m)




i_pFEV_sm_d2_f = cbind(full_fac_0[rownames(i_pFEV_sm_d2),],i_pFEV_sm_d2)
i_pFEV_sm_d2_fl = melt(i_pFEV_sm_d2_f, id.vars = colnames(full_fac_0), measure.vars = colnames(i_pFEV_sm_d2))


before = colnames(pFEV_w)[c(1:25)]

after = colnames(pFEV_w)[c(25:73)]




########### LINEAR REGRESSION ###############

# 
#   df = data.frame(Factor = numeric(), Status = numeric(0))
# 
#   factor = 'MRN'
# 
#   cols = c(-6:6)
# 
#   full_data=pFEV_lf[pFEV_lf$variable %in% cols,]
# 
#   factor_levels = unique(full_data[,factor])
# 
#   entry = factor_levels[3]
# 
#   for(entry in factor_levels){
#     data = full_data[full_data[,factor] == entry,]
#     dim(data)
#     y = as.numeric(data$value)
#     x = as.numeric(as.character(data$variable))
#     x
#     y
#     if(!all(is.na(y))){
#       fit <- aov(y ~ x, data=data)
#       fit
#       summary(fit)
#       a = anova(fit)
#       a
#       
#       p = a$Pr[1]
#       print(p)
#       df[paste(factor,entry),'Factor'] = factor
#       df[paste(factor,entry),'Status'] = entry
#       df[paste(factor,entry),'ano_All'] = signif(p,3)
#       str(data$variable)
#       l = lm(y~x)
#       summary(l)
#       i = coef(l)['(Intercept)']
#       m = coef(l)['x']
#       df[paste(factor,entry),'Int_ALL'] = i
#       df[paste(factor,entry),'slope_ALL'] = m
#     }
#   }
#   df
#   
#   full_data=pFEV_lf[pFEV_lf$variable %in% before & pFEV_lf$variable %in% cols,]
# 
#   factor_levels = unique(full_data[,factor])
#   factor_levels
#   entry = factor_levels[2]
# 
#   for(entry in factor_levels){
#     data = full_data[full_data[,factor] == entry,]
#     dim(data)
#     y = as.numeric(data$value)
#     x = as.numeric(as.character(data$variable))
#     x
#     y
#     if(!all(is.na(y))){
#       fit <- aov(y ~ x, data=data)
#       fit
#       summary(fit)
#       a = anova(fit)
#       a
#       
#       p = a$Pr[1]
#       print(p)
#      #df[paste(factor,entry),'Factor'] = factor
#       #df[paste(factor,entry),'Status'] = entry
#       df[paste(factor,entry),'ano_Pre'] = signif(p,3)
#       str(data$variable)
#       l = lm(y~x)
#       summary(l)
#       i = coef(l)['(Intercept)']
#       m = coef(l)['x']
#       df[paste(factor,entry),'Int_Pre'] = i
#       df[paste(factor,entry),'slope_Pre'] = m
#     }
#   }
#   
#   
#   full_data=pFEV_lf[pFEV_lf$variable %in% after & pFEV_lf$variable %in% cols,]
# 
#   factor_levels = unique(full_data[,factor])
#   factor_levels
#   entry = factor_levels[2]
# 
#   for(entry in factor_levels){
#     data = full_data[full_data[,factor] == entry,]
#     dim(data)
#     y = as.numeric(data$value)
#     x = as.numeric(as.character(data$variable))
#     x
#     y
#     if(!all(is.na(y))){
#       fit <- aov(y ~ x, data=data)
#       fit
#       summary(fit)
#       a = anova(fit)
#       a
#       
#       p = a$Pr[1]
#       print(p)
#       #df[paste(factor,entry),'Factor'] = factor
#       #df[paste(factor,entry),'Status'] = entry
#       df[paste(factor,entry),'ano_Post'] = signif(p,3)
#       str(data$variable)
#       l = lm(y~x)
#       summary(l)
#       i = coef(l)['(Intercept)']
#       m = coef(l)['x']
#       df[paste(factor,entry),'Int_Post'] = i
#       df[paste(factor,entry),'slope_Post'] = m
#     }
#   }
#   df = df[order(df$Status),]
#   df
#   lm_MRN = df
# 
# as.numeric(full_fac$DeathDate)
# 
# full_fac$time_diff = full_fac$RxDate - full_fac$DeathDate
# 
# full_fac$SurvObj = with(full_fac, Surv(time_diff,Status == 'dead'))
# km.as.one <- survfit(SurvObj ~ 1, data = full_fac, conf.type = "log-log")
# km.as.one
# plot(km.as.one)
# 
# km.by.sex <- survfit(SurvObj ~ NewCTChange, data = full_fac, conf.type = "log-log")
# plot(km.by.sex)

#km.by.sex <- survfit(SurvObj ~ sex, data = lung, conf.type = "log-log")

## Show object
#km.as.one



########## NEW ##########



