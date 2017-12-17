
library(shiny)
library(shinythemes)
#library(shinydashboard)
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
library(dplyr)
library(survival)
library(ggfortify)
library(survminer)
#library(ggplot2)
library(ggdendro)
#library(plyr)
library(zoo)
library(broom)
library(ggsignif)






source('functions.R')
source('defaults.R')
library(devtools)
#source_url("https://raw.githubusercontent.com/naturofix/AMR/Phase_1/Lung/defaults.R")
g_sheet = T

info_tab = 'Session Info'

defaults = 'David'
#defaults = 'Shaun'


#post_exclude_list
if(defaults == "Shaun"){
  source('defaults.R')
  
  pre_values = c(-12,0)
  post_values = c(0,12)
  data_select = 'pFEV'
  
  cluster_cols = c('D1_-4','D1_-3','D1_-2','D1_-1','D1_0','D1_1','D1_2','D1_3','D1_4')
  d_weight_1 = 0
  d_weight_2 = 0
  c_weight_1 = 20
  c_weight_2 = 5
  num_clusters = 4
  g_sheet = F
  
  #default_tab = 'R Info'
  #info_tab = 'Testing'
  
}

patient_custom_exclude = readRDS('www/pre_exclude_list.rds')
post_exclude_list = readRDS('www/post_exclude_list.rds')

si = sessionInfo()
enableBookmarking(store = "url")
############ UPLOAD DATA ####################
#g_sheet = F # decided is data is to be read from google sheets or not
if(g_sheet == T){
  googlesheets::gs_auth(token = 'shiny_app_token.rds')
  
  key = "1Bvwyd_TRH6M5bB3y6gBDYKYHpQXhox9L9NQTGTiV1IA"
  gs = gs_url("https://docs.google.com/spreadsheets/d/1Bvwyd_TRH6M5bB3y6gBDYKYHpQXhox9L9NQTGTiV1IA/edit?usp=sharing_eil&ts=59b28497")
  sheet_list = gs_ws_ls(gs)
  #sheet_list
  #clustering = as.data.frame(gs_read(ss=gs, ws= "OldClustering"))
  clustering = as.data.frame(gs_read(ss=gs, ws= "NewClustering"))
  
  #colnames(clustering)
  saveRDS(file = 'clustering_7_new.rds',object = clustering)
  #clustering2 = clustering
}else{
  #clustering = readRDS('clustering4.rds') ## OLD CLUSTERING FILE
  clustering = readRDS('clustering_7_new.rds')
  
}

edit_colname_function = function(col_names){
  edit_list = c(' ','%')
  for(e in edit_list){
    col_names = gsub(e,'_',col_names)
  }
  return(col_names)
}

colnames(clustering)


rename_columns = F
if(rename_columns == T){
  colnames(clustering) =  edit_colname_function(colnames(clustering))
  
  custom_colnames_list = c("date_columns","continuous_date_columns","pFEV_cols","bos_cols","change_cols","continuous_columns","discrete_numeric_columns","discrete_term_columns")
  for(col_name in custom_colnames_list){
    cmd = paste('col_names = ',col_name)
    print(cmd)
    eval(parse(text=cmd))
    print(col_names)
    col_names = edit_colname_function(col_names)
    print(col_names)
    cmd = paste(col_name, ' = col_names')
    print(cmd)
    eval(parse(text=cmd))
  }
}






factor_list = c(discrete_numeric_columns,discrete_term_columns,'cluster') 

pFEV_numeric_colnames_f = c('-24','-18','-12','-6','-5','-4','-3','-2','-1','0','1','2','3','4','5','6','12','18','24')
d1_colnames = paste0('D1_',pFEV_numeric_colnames_f)
d1_colnames
comp_colnames = pFEV_numeric_colnames_f[!pFEV_numeric_colnames_f == '0']
#clustering_continuous_columns = c(pFEV_numeric_colnames_f,d1_colnames,continuous_columns,continuous_date_columns)
#clustering_continuous_columns
sym_times_cols = c(1,2,3,4,5,6,12,24)
ratio_colnames = paste0('log2(',sym_times_cols,')')
per_colnames = paste0('per_',sym_times_cols)

sym_ratio_colnames = paste0('ratio_',sym_times_cols)
sym_log_ratio_colnames = paste0('log2_',sym_times_cols)

sym_rel_ratio_colnames = paste0('ratio_rel_',sym_times_cols)
sym_rel_log_ratio_colnames = paste0('log2_rel_',sym_times_cols)

sym_per_colnames = paste0('per_',sym_times_cols)
sym_rel_per_colnames = paste0('per_rel_',sym_times_cols)

log2zero_ratio_colnames = paste0('log2zero_',comp_colnames)
log2zero_per_colnames = paste0('per2zero_',comp_colnames)


sym_list = c('sym_ratio_colnames' ,'sym_log_ratio_colnames','sym_per_colnames')
sym_prefix_list = c("ratio_",'log2_','per_')
rel_list = c('sym_rel_ratio_colnames','sym_rel_log_ratio_colnames','sym_rel_per_colnames')
rel_prefix_list = c('per_rel_')
clustering_continuous_columns = c(continuous_columns,continuous_date_columns,pFEV_numeric_colnames_f,d1_colnames,log2zero_ratio_colnames,sym_rel_per_colnames,sym_ratio_colnames)




### ALL COLUMNS #####
all_continuous_columns = c(continuous_columns,continuous_date_columns,pFEV_cols,bos_cols,change_cols)
all_discrete_columns = c('MRN',discrete_numeric_columns,discrete_term_columns) # full_factor_columns
all_columns = c(all_discrete_columns,all_continuous_columns,date_columns)
#Columns in program not in googlesheets
missing_columns = all_columns[!all_columns %in% colnames(clustering)]

#Columns in googlesheets not in program

additional_columns = setdiff(colnames(clustering),all_columns)

if(length(additional_columns) > 0){
  info_tab = 'Sanity Check'
  default_tab = 'R Info'
}

#### ORGANISING COLUMNS ############
continuous_columns = order_columns(continuous_columns,colnames(clustering))
continuous_date_columns = order_columns(continuous_date_columns,colnames(clustering))
pFEV_cols = order_columns(pFEV_cols,colnames(clustering))
bos_cols = order_columns(bos_cols,colnames(clustering))
change_cols = order_columns(change_cols,colnames(clustering))

discrete_numeric_columns = order_columns(discrete_numeric_columns,colnames(clustering))
discrete_term_columns = order_columns(discrete_term_columns,colnames(clustering))

all_continuous_columns = order_columns(all_continuous_columns,colnames(clustering))
all_discrete_columns = order_columns(all_discrete_columns,colnames(clustering)) 




#### GROUPING COLUMNS ####
discrete_columns_4_comparison = c(discrete_numeric_columns,discrete_term_columns) #factor_colums_4_comparisons
discrete_columns_4_comparison = order_columns(discrete_columns_4_comparison,colnames(clustering))

non_pFEV_continuous_columns = c(continuous_columns,bos_cols,change_cols,continuous_date_columns)
#discrete_columns_4_comparison



### REMOVE DUPLICATE ROWS ####
dup = duplicated(clustering[,1])
dups = clustering[duplicated(clustering[,1]),1]
#dups
row_names = clustering[,1]
row_names[dup] = paste(row_names[dup],'a',sep='_') 
clustering = clustering[!is.na(row_names),]
row_names = row_names[!is.na(row_names)]
rownames(clustering) = row_names
clustering$MRN = row_names




if(length(missing_columns) == 0){




######################################

#### correct DATE columns ##########
clust_date = clustering[,date_columns]
#clust_date
clust_date = apply(clust_date,2, function(x) as.character((as.Date(x, '%d-%b-%Y'))))
#head(clust_date)



####### PROCESS ###################



############ FACTOR COLUMNS ###################
cluster_factor = clustering[,discrete_numeric_columns]
clust_fac_con = clustering[,discrete_term_columns]
#View(clust_fac_con)
convert_factors = T # convert word columns to lowercase
if(convert_factors==T){
  cluster_factor_con = apply(clust_fac_con, 2, function(x) tolower(factor(x)))
}else{
  cluster_factor_con = clust_fac_con
}


cluster_factor_con = apply(cluster_factor_con, 2, function(x) as.numeric(factor(x)))




full_fac = cbind(cluster_factor,cluster_factor_con)
full_fac$MRN = clustering$MRN

full_fac = apply(full_fac,2, function(x) factor(x))




#num_fac = apply(full_fac,2, function(x) as.numeric(x))



########### NUMERIC COLUMN ###############

## numric column ##
clust_num = clustering[,non_pFEV_continuous_columns]
clust_num = as.data.frame(apply(clust_num, 2, function(x) as.numeric(sub('%','',x))))
#str(clust_num)
#clust_num



#as.numeric(clustering$MonthsToDeath)
####################### pFEV ##################################

########## ADD missing pFEV columns ###################
pFEV = clustering[,pFEV_cols]
pFEV = as.data.frame(apply(pFEV,2, function(x) as.numeric(x)))
rownames(pFEV) = rownames(clustering)
pFEV_na <- apply(pFEV, 1, function(x) round((1-(sum(is.na(x))/length(x)))*100,1))



pFEV_w = pFEV
pFEV_numeric_colnames_n = c(-24,-18,-12,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,12,18,24)
pFEV_numeric_colnames_f = c('-24','-18','-12','-6','-5','-4','-3','-2','-1','0','1','2','3','4','5','6','12','18','24')
colnames(pFEV_w) = pFEV_numeric_colnames_f


for(col in seq(-24,24,1)){
  if(!col %in% pFEV_numeric_colnames_n){
    pFEV_w[,paste(col)] = NA
  }
}

p_cols = colnames(pFEV_w)[order(as.numeric(colnames(pFEV_w)))]

pFEV_w = pFEV_w[,p_cols]


################## CONSOLIDATE DATA ##############

#full_num = cbind(full_fac,clust_num,clust_date)
patient_list = clustering$MRN
patient_list = patient_list[order(patient_list)]


processed_data = cbind(full_fac,clust_num,clust_date)


processed_data_pFEV = cbind(full_fac,clust_num,clust_date,pFEV)



processed_data = as.data.frame(processed_data[,order_columns(colnames(processed_data),colnames(clustering))])
processed_data$pFEV_na = as.numeric(pFEV_na)



full_factor_columns = colnames(processed_data)
processed_data = cbind(processed_data,pFEV_w) #extended withe missing columns



### order ###
excluded_patients_c = paste(processed_data$MRN[processed_data$pFEV_na < completeness])
r_list = patient_list[!patient_list %in% excluded_patients_c]



processed_data_r = processed_data # allowed for additional processing here

full_fac_0 = processed_data_r[,full_factor_columns]
pFEV_w = processed_data_r[,p_cols]
pFEV_wf = processed_data_r

pFEV_lf = melt(processed_data_r, id.vars = full_factor_columns, measure.vars = colnames(pFEV_w))
pFEV_lf$time = as.numeric(as.character(pFEV_lf$variable))
pFEV_lf[,all_discrete_columns] = apply(pFEV_lf[,all_discrete_columns],2,function(x) factor(x))

term_mapping_df = data.frame(Factor = numeric(0),Number = numeric(0),Name = numeric(0))
for(term in discrete_term_columns){
  reason = data.frame(Name = clustering[,term],Number = as.numeric(processed_data[,term]))
  #reason
  reason = reason[order(reason$Number),]
  reason = reason[!duplicated(reason),]
  reason$Factor = term
  reason$Number = factor(reason$Number)
  reason = reason[,c('Factor','Number','Name')]
  term_mapping_df = rbind(term_mapping_df,reason)
}

####################### IMPUTE pFEV ####################### 
  
pFEV_ts = as.ts(t(pFEV_w))
#pFEV_ts

i_pFEV_ts = na.interpolation(pFEV_ts)

i_pFEV_ts = as.data.frame(t(i_pFEV_ts))
colnames(i_pFEV_ts) = seq(-24,24,1)
#rownames(i_pFEV_ts) = rownames(full_fac_0)


i_pFEV_wf = cbind(full_fac_0,i_pFEV_ts)
i_pFEV_lf = melt(i_pFEV_wf, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))
i_pFEV_lf$time = as.numeric(as.character(i_pFEV_lf$variable))

pFEV_lf[,all_discrete_columns] = apply(pFEV_lf[,all_discrete_columns],2,function(x) factor(x))

i_pFEV_lf$i = pFEV_lf$value
i_pFEV_lf$data = pFEV_lf$value
i_pFEV_lf$i[is.na(i_pFEV_lf$data)] = '0'
i_pFEV_lf$i[!is.na(i_pFEV_lf$data)] = '1'



 ############## differential ###################




time = as.numeric(colnames(i_pFEV_ts))
#time
i_pFEV_sm = as.data.frame(t(apply(i_pFEV_ts[full_fac_0$pFEV_na >= completeness,],1, function(x) predict(sm.spline(time, as.numeric(x)))$ysmth)))
colnames(i_pFEV_sm) = time

i_pFEV_smf = cbind(full_fac_0[rownames(i_pFEV_sm),],i_pFEV_sm)
i_pFEV_sm_lf = melt(i_pFEV_smf, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))
i_pFEV_sm_lf$time = as.numeric(as.character(i_pFEV_sm_lf$variable))



############ D1 ###############
i_pFEV_sm_ts = as.ts(t(i_pFEV_sm))

i_pFEV_sm_ts_d1 = diff(i_pFEV_sm_ts)
i_pFEV_sm_d1 = as.data.frame(t(i_pFEV_sm_ts_d1))

colnames(i_pFEV_sm_d1) = time[-2]


i_pFEV_sm_d1_m = i_pFEV_sm_d1[,pFEV_numeric_colnames_f]

i_pFEV_sm_d1_mf = cbind(full_fac_0[rownames(i_pFEV_sm_d1_m),],i_pFEV_sm_d1_m)



i_pFEV_sm_d1_f = cbind(full_fac_0[rownames(i_pFEV_sm_d1),],i_pFEV_sm_d1)
i_pFEV_sm_d1_fl = melt(i_pFEV_sm_d1_f, id.vars = colnames(full_fac_0), measure.vars = colnames(i_pFEV_sm_d1))
i_pFEV_sm_d1_fl$time = as.numeric(as.character(i_pFEV_sm_d1_fl$variable))


################## D2 #######################
i_pFEV_sm_ts_d2 = diff(i_pFEV_sm_ts_d1)
i_pFEV_sm_d2 = as.data.frame(t(i_pFEV_sm_ts_d2))


colnames(i_pFEV_sm_d2) = colnames(i_pFEV_sm_d1)[-2]

rownames(i_pFEV_sm_d2) = rownames(i_pFEV_sm)

i_pFEV_sm_d2_m = i_pFEV_sm_d2[,pFEV_numeric_colnames_f]

i_pFEV_sm_d2_mf = cbind(full_fac_0[rownames(i_pFEV_sm_d1_m),],i_pFEV_sm_d1_m)




i_pFEV_sm_d2_f = cbind(full_fac_0[rownames(i_pFEV_sm_d2),],i_pFEV_sm_d2)
i_pFEV_sm_d2_fl = melt(i_pFEV_sm_d2_f, id.vars = colnames(full_fac_0), measure.vars = colnames(i_pFEV_sm_d2))
i_pFEV_sm_d2_fl$time = as.numeric(as.character(i_pFEV_sm_d2_fl$variable))


before = colnames(pFEV_w)[c(1:25)]

after = colnames(pFEV_w)[c(25:49)]


}else{
  patient_list = c()
  excluded_patients_c = c()
  full_factor_columns = c()
  info_tab = 'Sanity Check'
  default_tab = 'R Info'
  
}

