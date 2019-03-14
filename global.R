
## CRAN packages : install.packages('package_name')
#source("https://bioconductor.org/biocLite.R")
library(BiocManager)
options(repos = BiocManager::repositories())
getOption("repos")
install_packages = F
if(install_packages == T){
  source("https://bioconductor.org/biocLite.R")
  
  BiocManager::install(c("marray",'Biobase','RTCGA.clinical')) # required for CluMix

  install.packages(c("shiny","shinythemes", "googlesheets","gplots", "ggplot2","CluMix",
    "Amelia","reshape","imputeTS","pspline","dendextend","plyr","dplyr","survival",
    "ggfortify","survminer","ggdendro","zoo","broom","ggsignif",'devtools'))
  library(devtools)
  install_version("DT", version = "0.2", repos = "http://cran.us.r-project.org")
  install_github("vqv/ggbiplot")

}

library(shiny)
library(shinythemes) #
library(shinyFiles)
library(googlesheets)
library(gplots) #
library(ggplot2)
library(plot3D)
library(CluMix)
library(Amelia)#
library(reshape)


library(imputeTS)#
library(pspline)#
library(DT)
library(dendextend)
library(plyr)
library(dplyr)
library(tidyr)
library(survival)
library(ggfortify)
library(survminer)
library(ggdendro)
library(zoo)
library(broom)
library(ggsignif)
library(shinyBS)

#library(survival)
library(ggbiplot) # for plotting PCA's





original_theme = theme_get()

source('functions.R')
source('defaults.R')
default_file_name = 'current_default.rds'
default_file_name = 'Defaults Spirometry Clusters by Ratio Alone.rds'
#library(devtools)
#source_url("https://raw.githubusercontent.com/naturofix/AMR/Phase_1/Lung/defaults.R")
g_sheet = T

info_tab = 'Session Info'

defaults = 'David'
save_workspace = T
show_debug = T

read_workspace = T
if(read_workspace == F){
  show_debug = F
  save_workspace = F
}
save_data = F

#display_data_tables = F
#use_gs_defaults = T
#defaults = 'Shaun' 

if(read_workspace == T){
  load('workspace.RData')
  source('functions.R')
  default_df = default_gs[!is.na(default_gs$value),]
  last_updated = paste(' ### SAVED WORKSPACE from ###',last_updated)
  #View(default_df)
  use_gs_defaults = T
  for(variable in default_df$variable){
    #print(variable)
    cmd = paste(variable,'=',default_df$value[default_df$variable == variable])
    print(cmd)
    
    if(use_gs_defaults == T){
      eval(parse(text = cmd))
    }
  }
  #run_clustering = F
  read_workspace = T
  
}else{

  #post_exclude_list
  if(defaults == "Shaun"){
    source('defaults.R')
    use_gs_defaults = F
    #default_gs = as.data.frame(not_used = character(0))
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
    
    default_tab = 'Patients'
    info_tab = 'Testing'
    
  }
  
  #patient_custom_exclude = readRDS('www/pre_exclude_list.rds')
  #post_exclude_list = readRDS('www/post_exclude_list.rds')
  
  si = sessionInfo()
  #enableBookmarking(store = "url")
  ############ UPLOAD DATA ####################
  #g_sheet = F # decided is data is to be read from google sheets or not
  if(g_sheet == T){
    googlesheets::gs_auth(token = 'shiny_app_token.rds')
    
    key = "1Bvwyd_TRH6M5bB3y6gBDYKYHpQXhox9L9NQTGTiV1IA"
    gs_link = "https://docs.google.com/spreadsheets/d/1Bvwyd_TRH6M5bB3y6gBDYKYHpQXhox9L9NQTGTiV1IA/edit?usp=sharing_eil&ts=59b28497"

    gs = gs_url(gs_link)
    sheet_list = gs_ws_ls(gs)
    #sheet_list
    #clustering = as.data.frame(gs_read(ss=gs, ws= "OldClustering"))
    #clustering = as.data.frame(gs_read(ss=gs, ws= "NewClustering"))
    #clustering = as.data.frame(gs_read(ss=gs, ws= "Data for App 2"))
    #clustering = as.data.frame(gs_read(ss=gs, ws= "clustering_2018_04_04"))
    
    google_sheets_file = "Full cohort"
    clustering = as.data.frame(gs_read(ss=gs, ws= google_sheets_file))
    gs_updated = gs$updated
    
    default_gs = as.data.frame(gs_read(ss=gs, ws= "Defaults"))
    #colnames(clustering)
    #saveRDS(file = 'clustering_8_new.rds',object = clustering)
    saveRDS(file = 'www/full_cohort.rds',object = clustering)
    saveRDS(file = 'www/default_gs.rds', object = default_gs)
    
    #clustering2 = clustering
  }else{
    #clustering = readRDS('clustering4.rds') ## OLD CLUSTERING FILE
    #clustering = readRDS('clustering_8_new.rds')
    clustering = readRDS('www/full_cohort.rds')
    default_gs = readRDS('www/default_gs.rds')
  }
  
  colnames(clustering)
  last_updated = colnames(clustering)[2]
  last_updated
  
  #gs_updated = gs$updated
  #### process defaults #####
  default_df = default_gs[!is.na(default_gs$value),]
  #View(default_df)
  use_gs_defaults = T
  for(variable in default_df$variable){
    #print(variable)
    cmd = paste(variable,'=',default_df$value[default_df$variable == variable])
    print(cmd)
    
    if(use_gs_defaults == T){
      eval(parse(text = cmd))
    }
  }
  cluster_name_list = paste(cluster_patient_mapping)
  #display_data_tables = T
  default_df = c()
  edit_colname_function = function(col_names){
    edit_list = c(' ','%')
    for(e in edit_list){
      col_names = gsub(e,'_',col_names)
    }
    return(col_names)
  }
  
  colnames(clustering) = clustering[1,]
  colnames(clustering)
  variable_type = clustering[2,]
  variable_type
  unique(unlist(variable_type))
  #continuous_columns = names(variable_type[,variable_type == 'Continuous' | variable_type == 'pFVC' | variable_type == 'pRatio' ])
  
  continuous_columns = names(variable_type[,variable_type == 'Continuous'])# | variable_type == 'pFVC' | variable_type == 'pRatio' ])
  
  continuous_columns
  discrete_term_columns = names(variable_type[,variable_type == 'Discrete'])
  
  discrete_term_columns = discrete_term_columns[discrete_term_columns != "Name and notes"]
  discrete_term_columns
  continuous_date_columns = names(variable_type[,variable_type == 'Date'])
  continuous_date_columns                               
                                
  discrete_numeric_columns = names(variable_type[,variable_type == 'DiscreteNumeric'])
  discrete_numeric_columns
  
  other_columns = names(variable_type[,variable_type == 'other'])
  other_columns
  
  pFEV1_column_names = names(variable_type[,variable_type == 'pFEV1'])
  pFEV1_column_names
  
  pFVC_column_names = names(variable_type[,variable_type == 'pFVC'])
  
  
  pRatio_column_names = names(variable_type[,variable_type == 'pRatio'])
  
  
  
  
  clustering
  
  clustering = clustering[c(-1,-2),-1]
  clustering = clustering[!is.na(clustering$MRN),]
  clustering$MRN
  #View(clustering)
  clustering$MRN_original = clustering$MRN
  
  MRN_original = clustering$MRN_original
  MRN_original
  
  
  
  
  
  
  #factor_list = c(discrete_numeric_columns,discrete_term_columns,'cluster') 
  factor_list = c(discrete_numeric_columns,discrete_term_columns,'cluster') 
  factor_list
  
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
  all_continuous_columns = c(continuous_columns,continuous_date_columns,pFEV_cols)
  all_discrete_columns = c('MRN','MRN_original',discrete_numeric_columns,discrete_term_columns) # full_factor_columns
  all_columns = c(all_discrete_columns,all_continuous_columns,continuous_date_columns,pFVC_column_names,pRatio_column_names,other_columns)
  #Columns in program not in googlesheets
  missing_columns = all_columns[!all_columns %in% colnames(clustering)]
  missing_columns
  #Columns in googlesheets not in program
  
  additional_columns = setdiff(colnames(clustering),all_columns)
  additional_columns
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
  discrete_term_columns = discrete_term_columns[discrete_term_columns != "Name and notes"]
  all_continuous_columns = order_columns(all_continuous_columns,colnames(clustering))
  all_discrete_columns = order_columns(all_discrete_columns,colnames(clustering)) 
  
  
  
  
  #### GROUPING COLUMNS ####
  discrete_columns_4_comparison = c(discrete_numeric_columns,discrete_term_columns) #factor_colums_4_comparisons
  discrete_columns_4_comparison = order_columns(discrete_columns_4_comparison,colnames(clustering))
  
  non_pFEV_continuous_columns = c(continuous_columns,continuous_date_columns)
  #discrete_columns_4_comparison
  
  
  
  ### REMOVE DUPLICATE ROWS ####
  #dup = duplicated(clustering[,1])
  #dups = clustering[duplicated(clustering[,1]),1]
  #dups
  #row_names = clustering[,1]
  #row_names[dup] = paste(row_names[dup],'a',sep='_') 
  #dup = duplicated(row_names)
  #dup
  #row_names[dup]
  #clustering = clustering[!is.na(row_names),]
  
  
  
  row_names = clustering$MRN
  #clustering$MRN_original = clustering$MRN
  row_names
  dup = duplicated(row_names)
  dup
  duplicated_patients = unique(row_names[dup])
  duplicated_patients = duplicated_patients[order(duplicated_patients)]
  duplicated_patients
  length(dup[dup])
  if(length(dup[dup]) > 0){
    row_names[dup] = paste(row_names[dup],'a',sep='_') 
  }
  dup = duplicated(row_names)
  length(dup[dup])
  i = 1
  while(length(dup[dup]) > 0){
    row_names[dup] = gsub(paste0('_',letters[i]),paste0('_',letters[i+1]),row_names[dup])
    i = i + 1
    dup = duplicated(row_names)
  }
  dups = duplicated(row_names)
  #row_names
  
  #row_names = row_names[!is.na(row_names)]
  rownames(clustering) = row_names
  clustering$MRN = row_names
  
  
  
  ###### CREATE DATA MATRIXES ######
  pFEV_matrix = as.matrix(clustering[,pFEV1_column_names])
  pFEV_matrix[1,]
  pFEV_matrix = apply(pFEV_matrix,2, function(x) as.numeric(x))
  numeric_colnames_f = c('-24','-18','-12','-6','-5','-4','-3','-2','-1','0','1','2','3','4','5','6','12','18','24')
  colnames(pFEV_matrix) = numeric_colnames_f
  clustering$pFEV1_matrix = pFEV_matrix
  rownames(clustering$pFEV1_matrix) = rownames(clustering)
  clustering$pFEV1_matrix
  clustering$pFEV1_matrix[1,]
  
  pFVC_column_names
  pFVC_matrix = as.matrix(clustering[,pFVC_column_names])
  colnames(pFVC_matrix) = numeric_colnames_f
  clustering$pFVC_matrix = pFVC_matrix
  rownames(clustering$pFVC_matrix) = rownames(clustering)
  
  
  pRatio_column_names
  pRatio_matrix = as.matrix(clustering[,pRatio_column_names])
  colnames(pRatio_matrix) = numeric_colnames_f
  clustering
  clustering$pRatio_matrix = pRatio_matrix
  rownames(clustering$pRatio_matrix) = rownames(clustering)
  
  if(length(missing_columns) == 0){
  
    
    
    
    ######################################
    
    #### correct DATE columns ##########
    clust_date = clustering[,continuous_date_columns]
    clust_date
    clust_date = apply(clust_date,2, function(x) as.character((as.Date(x, '%d-%b-%Y'))))
    #head(clust_date)
    
    
    
    ####### PROCESS ###################
    
    
    
    ############ FACTOR COLUMNS ###################
    (cluster_factor = clustering[,discrete_numeric_columns])
    (cluster_factor = as.data.frame(apply(cluster_factor,2, function(x) as.factor(as.numeric(x))))) #had issues with clustering, conversion to numeric and then factor will hopefully help
    rownames(cluster_factor) = clustering$MRN
    colnames(cluster_factor)
    #View(cluster_factor)
    
    clust_fac_con = clustering[,discrete_term_columns]
    
    #View(clust_fac_con)
    
    term_mapping_df = data.frame(Factor = numeric(0),Number = numeric(0),Name = numeric(0))
    term = discrete_term_columns[3]
    term
    temp_df = clust_fac_con
    temp_df
    for(term in discrete_term_columns[discrete_term_columns != 'Coded MRN']){
      reason = data.frame(Name = temp_df[,term],Number = as.numeric(factor(temp_df[,term])))
      reason
      reason = reason[order(reason$Number),]
      reason = reason[!duplicated(reason),]
      reason$Factor = term
      reason$Number = trimws(factor(reason$Number))
      reason = reason[,c('Factor','Number','Name')]
      reason
      term_mapping_df = rbind(term_mapping_df,reason)
    }
    
    
    convert_factors = T # convert word columns to lowercase
    if(convert_factors==T){
      cluster_factor_con = apply(clust_fac_con, 2, function(x) tolower(factor(x)))
    }else{
      cluster_factor_con = clust_fac_con
    }
    
    
    cluster_factor_con = apply(cluster_factor_con, 2, function(x) as.numeric(factor(x)))
    
    
    
    
    full_fac = cbind(cluster_factor,cluster_factor_con)
    dim(full_fac)
    full_fac$MRN = clustering$MRN
    full_fac$MRN_original = clustering$MRN_original
    #View(full_fac)
    full_fac = apply(full_fac,2, function(x) trimws(factor(x)))
    
    
    
    
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
    
    make_numeric = function(m){
      n = apply(m,2,function(x) as.numeric(x))
      rownames(n) = rownames(m)
      return(n)
    }
    
    clustering$pFEV1_matrix = make_numeric(clustering$pFEV1_matrix)
    clustering$pFEV1_matrix[1,]
    clustering$pFVC_matrix = make_numeric(clustering$pFVC_matrix)
    clustering$pRatio_matrix = make_numeric(clustering$pRatio_matrix)
    
    count_na = function(m){
      n = apply(m, 1, function(x) round((1-(sum(is.na(x))/length(x)))*100,1))
      #rownames(n) = rownames(m)
      return(n)
    }
    clustering$pFEV1_na = count_na(clustering$pFEV1_matrix)
    clustering$pFEV1_na
    clustering$pFVC_na = count_na(clustering$pFVC_matrix)
    clustering$pRatio_na = count_na(clustering$pRatio_matrix)
    na_columns = c('pFEV1_na','pFVC_na','pRatio_na')
    na_cols = clustering[,na_columns]
    
    add_missing_columns = function(m,first_col = 0, last_col = 0){
      cols = colnames(m)
      numeric_cols = as.numeric(cols)
      #numeric_cols
      if(first_col == 0){
        first_col = numeric_cols[1]
      }
      if(last_col == 0){
        last_col = numeric_cols[length(numeric_cols)]
      }
      #last_col
      df = as.data.frame(m)
      for(col in seq(first_col,last_col,1)){
        #print(col)
        if(!col %in% numeric_cols){
          #print('hit')
          df[,paste(col)] = NA
        }
      }
      df = df[,(order(as.numeric(colnames(df))))]
      #colnames(df)
      n = as.matrix(df)
      rownames(n) = rownames(m)
      return(n)
    }
    
  
    numeric_cols = as.numeric(colnames(clustering$pFEV1_matrix))
    pFEV1_first_col = numeric_cols[1]
    pFEV1_last_col = numeric_cols[length(numeric_cols)]
    clustering$pFEV1_matrix = add_missing_columns(clustering$pFEV1_matrix,pFEV1_first_col,pFEV1_last_col)
    clustering$pFEV1_matrix[1,]
    clustering$pFVC_matrix = add_missing_columns(clustering$pFVC_matrix,pFEV1_first_col,pFEV1_last_col)
    clustering$pRatio_matrix = add_missing_columns(clustering$pRatio_matrix,pFEV1_first_col,pFEV1_last_col)
    
    matrix_columns = clustering[,c('pFEV1_matrix','pFVC_matrix','pRatio_matrix','pFEV1_na','pFVC_na','pRatio_matrix')]
    
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
    
    
    processed_data = cbind(full_fac,clust_num,clust_date,na_cols)
    dim(processed_data)
    
    processed_data_pFEV = cbind(full_fac,clust_num,clust_date,pFEV)
    
    
    
    processed_data = as.data.frame(processed_data[,order_columns(colnames(processed_data),colnames(clustering))])
    processed_data$pFEV_na = as.numeric(pFEV_na)
    
    
    
    full_factor_columns = colnames(processed_data)
    full_factor_columns = c('MRN_original',full_factor_columns[full_factor_columns != 'MRN_original'])
    full_factor_columns
    #full_factor_columns = c(full_factor_columns,na_columns)
    
    processed_data = processed_data[,full_factor_columns]
    processed_data = cbind(processed_data,pFEV_w) #extended with missing columns
    processed_data = cbind(processed_data,pFEV_w,matrix_columns) #extended with missing columns
    colnames(processed_data)
    
    #View(pFEV)
    pFEV_test = pFEV
    pFEV_test$MRN_original = clustering$MRN_original[clustering$MRN == rownames(pFEV_test)]
    colnames(pFEV_test)
    pFEV_dup = duplicated(pFEV_test)
    pFEV_dup
    duplicated_list = rownames(pFEV_test[pFEV_dup,])
    duplicated_list = duplicated_list[order(duplicated_list)]
    length(duplicated_list)
    dup_test = F
    if(dup_test == T){
      dup_result = pFEV_test[duplicated_list[1],]
      dup_result
      dup_result$MRN_original
      pFEV_test[pFEV_test$MRN_original == dup_result$MRN_original,]
    }
    
    processed_data_dup = processed_data[!(processed_data$MRN %in% duplicated_list),]
    dim(processed_data_dup)
    ### order ###
    excluded_patients_c = paste(processed_data_dup$MRN[processed_data_dup$pFEV_na < completeness])
    r_list = patient_list[!patient_list %in% excluded_patients_c]
    
    
    
    processed_data_r = processed_data # allowed for additional processing here
    
    full_fac_0 = processed_data_r[,full_factor_columns]
    pFEV_w = processed_data_r[,p_cols]
    pFEV_wf = processed_data_r
    
   
    #full_fc
    pFEV1_data_list = melt_processed_data(processed_data,full_factor_columns,'pFEV1_matrix')
    #str(pFEV1_data_list)
    pFVC_data_list = melt_processed_data(processed_data,full_factor_columns,'pFVC_matrix')
    pRatio_data_list = melt_processed_data(processed_data,full_factor_columns,'pRatio_matrix')
    
    
    pFEV_lf = melt(processed_data_r, id.vars = full_factor_columns, measure.vars = colnames(pFEV_w))
    pFEV_lf$time = as.numeric(as.character(pFEV_lf$variable))
    pFEV_lf[,all_discrete_columns] = apply(pFEV_lf[,all_discrete_columns],2,function(x) factor(x))
    

    
    ####################### IMPUTE pFEV ####################### 
    
    impute_data = function(m){
    
      m_ts = as.ts(t(m))
  
      i_m_ts = t(na.interpolation(m_ts))
    
      colnames(i_m_ts) = colnames(m)
  
      return(i_m_ts)
    }
    m = processed_data$pFEV1_matrix
    processed_data$i_pFEV1_matrix = impute_data(processed_data$pFEV1_matrix)
    #processed_data$i_pFEV1_matrix
    processed_data$i_pFVC_matrix = impute_data(processed_data$pFVC_matrix)
    processed_data$i_pRatio_matrix = impute_data(processed_data$pRatio_matrix)
    #o_data = processed_data$i
    #impute_NA_function = function(o_data,i_data){
    #  last_index = 
    #}
    #View(processed_data)
     
    pFEV_ts = as.ts(t(pFEV_w))
    head(pFEV_ts)
    
    i_pFEV_ts = na.interpolation(pFEV_ts)
    
    i_pFEV_ts = as.data.frame(t(i_pFEV_ts))
    colnames(i_pFEV_ts) = seq(-24,24,1)
    head(i_pFEV_ts)
    #rownames(i_pFEV_ts) = rownames(full_fac_0)
    
    
    i_pFEV_wf = cbind(full_fac_0,i_pFEV_ts)
    view = F
    
    run_BOS_calc = F
    if(run_BOS_calc == T){
      if(view == T){
        View(i_pFEV_wf)
        
        View(processed_data$i_pFEV1_matrix)
        
        a_i = i_pFEV_wf[1,colnames(i_pFEV_ts)]
        a_i
        b_i = processed_data$i_pFEV1_matrix[1,]
        b_i
        }
      
      i_pFEV_wf$BOS1 = apply(i_pFEV_wf[,colnames(i_pFEV_ts)],1,function(x) BOS_calc_function(0.8,x,colnames(i_pFEV_ts)))
      i_pFEV_wf$BOS2 = apply(i_pFEV_wf[,colnames(i_pFEV_ts)],1,function(x) BOS_calc_function(0.66,x,colnames(i_pFEV_ts)))
      i_pFEV_wf$BOS3 = apply(i_pFEV_wf[,colnames(i_pFEV_ts)],1,function(x) BOS_calc_function(0.5,x,colnames(i_pFEV_ts)))
      
      
      processed_data$BOS1 = apply(i_pFEV_wf[,colnames(i_pFEV_ts)],1,function(x) BOS_calc_function(0.8,x,colnames(i_pFEV_ts)))
      processed_data$BOS2 = apply(i_pFEV_wf[,colnames(i_pFEV_ts)],1,function(x) BOS_calc_function(0.66,x,colnames(i_pFEV_ts)))
      processed_data$BOS3 = apply(i_pFEV_wf[,colnames(i_pFEV_ts)],1,function(x) BOS_calc_function(0.5,x,colnames(i_pFEV_ts)))
      
      #BOS = 0.8
      #v = processed_data$pFEV1_matrix[1,]
      #BOS_calc_matrix_function(0.8,v)
      ##### NOT WORKING NEEDS AN UPGRADE ######
      pFEV1 = processed_data$i_pFEV1_matrix
      head(pFEV1)
      pRatio = processed_data$i_pRatio_matrix
      head(pRatio)
      BOS = 0.8
      RAS = 0.7
    
  
      
      BOS_list = c()
      
      test_MRN = '234078a'
      i = grep(test_MRN,processed_data$MRN)
      processed_data$MRN[i]
      i = 267
      for(i in c(1:dim(processed_data)[1])){
        BOS1 = NA
        print(i)
        x = processed_data[i,]
        v = x[,'i_pFEV1_matrix']
        y = x[,'i_pRatio_matrix']
        BOS1 = RAS_matrix_calc_function(0.8,v,y)
        BOS1
        print(BOS1)
        #print(length(BOS1))
        if(length(BOS1) == 0){
          BOS1 = NA
        }
        BOS_list = c(BOS_list,BOS1)
      }
      BOS_list
      processed_data$RAS = BOS_list
      
      BOS_list = c()
      for(i in c(1:dim(processed_data)[1])){
        BOS1 = NA
        #print(i)
        x = processed_data[i,]
        v = x[,'i_pFEV1_matrix']
        y = x[,'i_pRatio_matrix']
        BOS1 = BOS_RAS_matrix_calc_function(0.8,v,y)
        #print(BOS1)
        #print(length(BOS1))
        if(length(BOS1) == 0){
          BOS1 = NA
        }
        BOS_list = c(BOS_list,BOS1)
      }
      #BOS1_list
      processed_data$BOS1_RAS = BOS_list
      
      BOS_list = c()
      for(i in c(1:dim(processed_data)[1])){
        BOS1 = NA
        #print(i)
        x = processed_data[i,]
        v = x[,'i_pFEV1_matrix']
        y = x[,'i_pRatio_matrix']
        BOS1 = BOS_RAS_matrix_calc_function(0.66,v,y)
        #print(BOS1)
        #print(length(BOS1))
        if(length(BOS1) == 0){
          BOS1 = NA
        }
        BOS_list = c(BOS_list,BOS1)
      }
      #BOS1_list
      processed_data$BOS2_RAS = BOS_list
      
      BOS_list = c()
      for(i in c(1:dim(processed_data)[1])){
        BOS1 = NA
        #print(i)
        x = processed_data[i,]
        v = x[,'i_pFEV1_matrix']
        y = x[,'i_pRatio_matrix']
        BOS1 = BOS_RAS_matrix_calc_function(0.5,v,y)
        #print(BOS1)
        #print(length(BOS1))
        if(length(BOS1) == 0){
          BOS1 = NA
        }
        BOS_list = c(BOS_list,BOS1)
      }
      #BOS1_list
      processed_data$BOS3_RAS = BOS_list
      BOS_columns = c('BOS1','BOS2','BOS3','RAS','BOS1_RAS','BOS2_RAS','BOS3_RAS')
      full_factor_columns = c(full_factor_columns,BOS_columns)
      full_factor_columns
      
      
      #processed_data$BOS_1_RAS = apply(processed_data, 1 ,
                                       #function(x) BOS_RAS_matrix_calc_function(0.8,x[,'i_pFEV1_matrix'],x[,'i_pRatio_matrix']))
      #processed_data$BOS1_RAS
      #processed_data$BOS2 = apply(processed_data$i_pFEV1_matrix,1,function(x) BOS_calc_matrix_function(0.66,x))
      #processed_data$BOS3 = apply(processed_data$i_pFEV1_matrix,1,function(x) BOS_calc_matrix_function(0.5,x))
      
      
      #i_pFEV_wf[,c("BOS1mnth","BOS1","BOS2mnth",'BOS2',"BOS3mnth",'BOS3')]
      new_bos_cols = c("RAS","BOS1_RAS","BOS1_RAS","BOS3_RAS")
      i_pFEV_lf = melt(i_pFEV_wf, id.vars = c(colnames(full_fac_0),'BOS1','BOS2','BOS3'), measure.vars = colnames(pFEV_w))
      colnames(i_pFEV_lf)
      i_pFEV_lf$time = as.numeric(as.character(i_pFEV_lf$variable))
      
      pFEV_lf[,all_discrete_columns] = apply(pFEV_lf[,all_discrete_columns],2,function(x) factor(x))
      
      i_pFEV_lf$i = pFEV_lf$value
      i_pFEV_lf$data = pFEV_lf$value
      i_pFEV_lf$i[is.na(i_pFEV_lf$data)] = '0'
      i_pFEV_lf$i[!is.na(i_pFEV_lf$data)] = '1'
    }
  
    
    ############## differential ###################
    ###### ___SMOOTHING###########
    smoothing_function = function(m,m_na){
      time = as.numeric(colnames(m))
      m_sm = m
      x = m_sm
      m_sm[m_na >= completeness,] = t(apply(m[m_na >= completeness,],1, function(x) predict(sm.spline(time, as.numeric(x)))$ysmth))
      m_sm[m_na < completeness,] = NA
      if(view == T){
        View(m_sm)
      }
      return(m_sm)
    }
    
    
    m = processed_data$i_pFEV1_matrix
    m_na = processed_data$pFEV1_na
    
    processed_data$sm_i_pFEV1_matrix = smoothing_function(processed_data$i_pFEV1_matrix,processed_data$pFEV1_na)
    processed_data$sm_i_pFVC_matrix = smoothing_function(processed_data$i_pFVC_matrix,processed_data$pFVC_na)
    processed_data$sm_i_pRatio_matrix = smoothing_function(processed_data$i_pRatio_matrix,processed_data$pRatio_na)
    
    
    
    time = as.numeric(colnames(i_pFEV_ts))
    #time
    #x = i_pFEV_sm
    i_pFEV_sm = as.data.frame(t(apply(i_pFEV_ts[full_fac_0$pFEV_na >= completeness,],1, function(x) predict(sm.spline(time, as.numeric(x)))$ysmth)))
    if(view == T){
      ### slight differences here ####
      View(i_pFEV_sm)
      View(processed_data$sm_i_pFEV1_matrix)
      
      a = i_pFEV_sm[1,]
      a_sm
      b = processed_data$sm_i_pFEV1_matrix[1,] 
      b_sm
    }
    
    colnames(i_pFEV_sm) = time
    
    i_pFEV_smf = cbind(full_fac_0[rownames(i_pFEV_sm),],i_pFEV_sm)
    i_pFEV_sm_lf = melt(i_pFEV_smf, id.vars = colnames(full_fac_0), measure.vars = colnames(pFEV_w))
    i_pFEV_sm_lf$time = as.numeric(as.character(i_pFEV_sm_lf$variable))
    
    
    
    ############ ___D1 ###############
    
    differential_function = function(m,m_na){
      m_ts = as.ts(t(m))
      m_ts_d1 = m_ts[-1,]
      m_ts_d1[,m_na >= completeness] = (apply(m_ts[,m_na >= completeness],2, function(x) diff(x)))
      m_ts_d1[,m_na < completeness] = NA
      if(view == T){
        View(m_ts)
        View(m_ts_d1)
      }
      d1 = t(m_ts_d1)
      colnames(d1) = colnames(m)[-2]
      d1
    }
    m = processed_data$sm_i_pFEV1_matrix
    m_na = processed_data$pFEV1_na
    
    processed_data$d1_pFEV1_matrix = differential_function(processed_data$sm_i_pFEV1_matrix,processed_data$pFEV_na)
    processed_data$d1_pFVC_matrix = differential_function(processed_data$sm_i_pFVC_matrix,processed_data$pFVC_na)
    processed_data$d1_pRatio_matrix = differential_function(processed_data$sm_i_pRatio_matrix,processed_data$pRatio_na)
    processed_data$d2_pFEV1_matrix = differential_function(processed_data$d1_pFEV1_matrix,processed_data$pFEV_na)
    processed_data$d2_pFVC_matrix = differential_function(processed_data$d1_pFVC_matrix,processed_data$pFVC_na)
    processed_data$d2_pRatio_matrix = differential_function(processed_data$d1_pRatio_matrix,processed_data$pRatio_na)
    
    i_pFEV_sm_ts = as.ts(t(i_pFEV_sm))
    #i_pFEV_sm_ts[,1]
    #diff(i_pFEV_sm_ts[,1])
    i_pFEV_sm_ts_d1 = diff((i_pFEV_sm_ts))
    
    i_pFEV_sm_d1 = as.data.frame(t(i_pFEV_sm_ts_d1))
    
    colnames(i_pFEV_sm_d1) = time[-2]
    
    
    i_pFEV_sm_d1_m = i_pFEV_sm_d1[,pFEV_numeric_colnames_f]
    if(view == T){
      View(i_pFEV_sm_ts)
      View(i_pFEV_sm_ts_d1)
      View(i_pFEV_sm_d1)
      View(processed_data$d1_pFEV1_matrix)
    }
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
  
  ##### RATIO CALCULATIONS ##################
    #pFEV_2_zero = reactive({
    # full_data = comp_data()
      #full_data = pFEV_wf_r()
      matrix_column_list = c("i_pFEV1_matrix","pFEV1_matrix","pFVC_matrix","pRatio_matrix","i_pFEV1_matrix","i_pFVC_matrix","i_pRatio_matrix","sm_i_pFEV1_matrix","sm_i_pFVC_matrix","sm_i_pRatio_matrix","d1_pFEV1_matrix","d1_pFVC_matrix","d1_pRatio_matrix","d2_pFEV1_matrix","d2_pFVC_matrix","d2_pRatio_matrix")
      matrix_entry = matrix_column_list[1]
      for(matrix_entry in matrix_column_list){
        cmd = paste0("data = processed_data$",matrix_entry)
        print(cmd)
        eval(parse(text = cmd))
        entry_name = gsub('_matrix','',matrix_entry)
        entry_name
        # ZERO DATA #########
        pre_times = pFEV_numeric_colnames_f[pFEV_numeric_colnames_n < 0]
        pre_times
        post_times = pFEV_numeric_colnames_f[pFEV_numeric_colnames_n > 0]
        post_times
        zero_data = data[,'0']
        zero_data
        
        pre_data = data[,pre_times]
        post_data = data[,post_times]
        
        pre_ratio_data = log2(zero_data/pre_data)
        #colnames(pre_ratio_data) = paste0('log2zero_',pre_times)
        post_ratio_data = log2(post_data/zero_data)
        #colnames(post_ratio_data) = paste0('log2zero_',post_times)
        
        ratio_data = cbind(pre_ratio_data,post_ratio_data)
        colnames(ratio_data)
        cmd = paste0('processed_data$',entry_name,'_log2zero_matrix = as.matrix(ratio_data)')
        print(cmd)
        eval(parse(text = cmd))
        
        colnames(pre_ratio_data)
        colnames(post_ratio_data)
        
        #pre_ratio_data_temp = pre_ratio_data[,order(abs(as.numeric(colnames(pre_ratio_data))))]
        pre_ratio_data_temp = pre_ratio_data
        colnames(pre_ratio_data_temp) = as.character(abs(as.numeric(colnames(pre_ratio_data_temp))))
        #pre_ratio_data_temp = pre_ratio_data_temp[,order(as.numeric(colnames(pre_ratio_data_temp)))]
        #colnames(pre_ratio_data_temp)
        log_diff_cols = intersect(colnames(post_ratio_data),colnames(pre_ratio_data_temp))
        log_diff_cols
        log_diff = post_ratio_data[,log_diff_cols] - pre_ratio_data_temp[,log_diff_cols]
        cmd = paste0('processed_data$',entry_name,'_log2zero_diff_matrix = as.matrix(log_diff)')
        print(cmd)
        eval(parse(text = cmd))
          test_i = 15
          test_j = '3'
          pre_ratio_data_temp[test_i,test_j]
          post_ratio_data[test_i,test_j]

          post_ratio_data[test_i,test_j] - pre_ratio_data_temp[test_i,test_j]
          log_diff[test_i,test_j]
        
        
        pre_per_data =  (zero_data-pre_data)/pre_data*100
        #colnames(pre_per_data) = paste0('per2zero_',pre_times)
        post_per_data = (post_data-zero_data)/zero_data*100
        #colnames(post_per_data) = paste0('per2zero_',post_times)
        per_data = cbind(pre_per_data,post_per_data)
        cmd = paste0('processed_data$',entry_name,'_per2zero_matrix = as.matrix(per_data)')
        print(cmd)
        eval(parse(text = cmd))
        
        # SYM DATA ####
          ratio_df = data.frame(MRN = processed_data$MRN)
          log_df = data.frame(MRN = processed_data$MRN)
          per_df = data.frame(MRN = processed_data$MRN)
          i = 1
          for(i in sym_times_cols){
            #print(i)
            r1 = data[,as.character(-(i))]
            #r1
            r2 = data[,as.character((i))]
            #r2
            #log2(r2/r1)
            #as.character(i)
            ratio_df[,paste0(i)] = r2/r1
            log_df[,paste0(i)] = log2(r2/r1)
            per_df[,paste0(i)] = (r2-r1)/r1*100
            
            
          }
          ratio_df = ratio_df[,-1]
          log_df = log_df[,-1]
          per_df = per_df[,-1]
          
          ratio_df
          cmd = paste0('processed_data$',entry_name,'_ratio_matrix = as.matrix(ratio_df)')
          print(cmd)
          eval(parse(text = cmd))
          cmd = paste0('processed_data$',entry_name,'_log_matrix = as.matrix(log_df)')
          print(cmd)
          eval(parse(text = cmd))
          cmd = paste0('processed_data$',entry_name,'_per_matrix = as.matrix(per_df)')
          print(cmd)
          eval(parse(text = cmd))
          
        # PER SYM DATA #####
          # per_sym_data = reactive({ # calcuates the ratios and percentages of timepoints across zero, -12 compared to 12, -3 compared to 3

 
            ratio_df = data.frame(MRN = processed_data$MRN)
            i = 1
            for(i in sym_times_cols){
            
              pre = per_data[,paste0(as.character(-(i)))]
         
              post = per_data[,paste0(as.character((i)))]
      
              
              ratio = (post-pre)/abs(pre)*100
              ratio[is.nan(as.numeric(ratio))] = NA
              ratio[is.infinite(ratio)] = NA
              ratio_df[,paste0(i)] = ratio
            }
            ratio_df = ratio_df[,-1]
            #View(ratio_df)
            ratio_df
            cmd = paste0('processed_data$',entry_name,'_per_rel_matrix = as.matrix(ratio_df)')
            print(cmd)
            eval(parse(text = cmd))
            
        #LOG SYM DATA ######
            #log_sym_data = reactive({ # calcuates the ratios and percentages of timepoints across zero, -12 compared to 12, -3 compared to 3
              

            ratio_df = data.frame(MRN = processed_data$MRN)
            i = 1
            for(i in sym_times_cols){
              #print(i)
              pre = ratio_data[,paste0(as.character(-(i)))]
              #pre
              #print(r1)
              post = ratio_data[,paste0(as.character((i)))]
              #r2
              #print(r2)
              #print(log2(r2/r1))
              #as.character(i)
              ratio = pre-post
              #print(ratio)
              ratio[is.nan(as.numeric(ratio))] = NA
              ratio[is.infinite(ratio)] = NA
              #print(ratio)
              ratio_df[,paste0(i)] = ratio
         
            }
            ratio_df = ratio_df[,-1]
            #View(ratio_df)
            ratio_df
            cmd = paste0('processed_data$',entry_name,'_ratio_rel_matrix = as.matrix(ratio_df)')
            print(cmd)
            eval(parse(text = cmd))

        
        }

  colnames(processed_data)
    
  
  }else{
    patient_list = c()
    excluded_patients_c = c()
    full_factor_columns = c()
    info_tab = 'Sanity Check'
    default_tab = 'R Info'
    
  }
  
  
  # melt_processed_data_matrix = function(processed_data,matrix_column){
  #   #test = melt(processed_data, ,id.vars = full_factor_columns,measure_vars = processed_data$pFEV1_matrix)
  #   #processed_data = cbind(processed_data,processed_data$pFEV1_matrix)
  #   full_factor_columns
  #   #p_cols
  #   colnames(processed_data)
  #   wide_df = processed_data[,c(full_factor_columns)]
  #   wide_df = cbind(wide_df,processed_data[,matrix_column])
  #   #head(wide_df)
  #   long_df = melt(wide_df,id.vars = full_factor_columns,measure_vars = p_cols)
  #   #head(long_df)
  #   long_df$time = as.numeric(as.character(long_df$variable))
  #   long_df[,all_discrete_columns] = apply(long_df[,all_discrete_columns],2,function(x) factor(x))
  #   data_list = list(wide_df = wide_df,long_df = long_df)
  #   return(data_list)
  # }
  
  
  matrix_column_list = grep('matrix',colnames(processed_data),value = T)
  #matrix_column_list = c("i_pFEV1_matrix","pFEV1_matrix","pFVC_matrix","pRatio_matrix","i_pFEV1_matrix","i_pFVC_matrix","i_pRatio_matrix","sm_i_pFEV1_matrix","sm_i_pFVC_matrix","sm_i_pRatio_matrix","d1_pFEV1_matrix","d1_pFVC_matrix","d1_pRatio_matrix","d2_pFEV1_matrix","d2_pFVC_matrix","d2_pRatio_matrix")
  processed_long = data.frame(empty = character(0))
  dim(processed_long)
  colnames(processed_data)[!colnames(processed_data) %in% full_factor_columns]
  factor_columns = full_factor_columns[full_factor_columns %in% colnames(processed_data)]
  factor_columns
  #factor_columns = colnames(processed_data)[!colnames(processed_data) %in% matrix_column_list]
  #factor_columns
  for(entry in matrix_column_list){
    print(entry)
    matrix_column = entry
    
    temp = melt_processed_data(processed_data,factor_columns,entry)$long_df
    col_names = colnames(temp)
    entry_name = gsub('_matrix','',entry)
    entry_name
    #col_names = gsub('value',entry_name,col_names)
    #col_names
    #colnames(temp) = col_names
    
    if(dim(processed_long)[1] == 0){
      processed_long = temp[,!names(temp) == 'value']
    }
    #cmd = paste0('processed_long$',entry_name,' = temp$value')
    
    cmd = paste0('processed_long$',entry_name,' = temp$value[match(processed_long$time, temp$time) & match(processed_long$MRN, temp$MRN)]')
    
    print(cmd)
    eval(parse(text = cmd))
    
  }
  
  #View(processed_long)
  ### GGplot set theme ###
  
  if(save_workspace == T){
    print('save workspace')
    save.image('workspace.RData')
  }
  
}

