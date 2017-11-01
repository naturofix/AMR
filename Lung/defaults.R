completeness = 20 # percentage of datapoints required to automatically include patients
#patient_custom_exclude = c('4924475','5863880','5931655','5884461') # list of patients to exclude
patient_custom_exclude = c('4941516', '4964463', '5863880', '5881350', '5884461', '5900493', '5928232', '5931655', '5931655_a', '4854594', '4941516', '5895629', '4905274', '4977337', '5876473', '5916182')
#patient_custom_exclude = c()
default_cluster_list = c("HLAType","HLAStrongWeak","NewCTChange","SignOfInflammation")

default_continuous_cluster_list = factor(c(-6, -3, -2, -1, 0, 1, 2, 3))


### Columns  ###

date_columns = c("DeathDate","DoTPE","DoB","DoTx")
continuous_date_columns = c("YearsToDeath",'MonthSinceRx',"Yr since Tx","Age at Rx","MonthsToDeath")

#### continuous columns ####
pFEV_cols = c("pFEV1_neg24","pFEV1_neg18", "pFEV1_neg12", "pFEV1_neg6", "pFEV1_neg5", "pFEV1_neg4", "pFEV1_neg3"     
              ,"pFEV1_neg2", "pFEV1_neg1", "pFEV1_0", "pFEV1_pos1", "pFEV1_pos2", "pFEV1_pos3"     
              ,"pFEV1_pos4", "pFEV1_pos5", "pFEV1_pos6", "pFEV1_pos12", "pFEV1_pos18", "pFEV1_pos24")
bos_cols = c("BOS1mnth", "BOS2mnth", "BOS3mnth","BOS 3 free survival")

change_cols = c("ChangeFEV1_12mth_prior", "ChangeFEV1_6mth_prior",  "ChangeFEV1_3mth_prior",  "ChangeFEV1_1mth_prior",  "ChangeFEV1_1mth_post",   "ChangeFEV1_3mth_post","ChangeFEV1_6mth_post")

continuous_columns = c("MFI","Change DSA","CRP peak","Eosinophil peak","FEV1Ratio-3","FEV1Ratio0","FEV1Ratio+3","FinalFEV1Ratio","%Pred", "Best FEV1")


### Discrete Columns ####
discrete_numeric_columns = c("AltDxScore","DSA_HLAScore","HLAType","BiopsyScore",
                             "NewCTChange","AGrade","BGrade","ViralPCR",
                             "BactCulture","Methylpred","IVAbx","HLAStrongWeak","AMRClassification",
                             "C1Q","SignOfInflammation","CRPRaised","EosRaised","CTClassification"
                             ,"Ground glass","Consolidation","Pleural eff","CT3mnthPost",
                             "AMRPathScore","FOP","Airways","DAD","C4D","TPE","IVIG","Monoclonal")
#factor_columns_con = c("Status" , "DaysSinceOnset", "Obstructive")
discrete_term_columns = c("Status" , "DaysSinceOnset", "Obst_post","Symptoms","Type","Reason")

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
clustering_continuous_columns = c(pFEV_numeric_colnames_f,d1_colnames,continuous_columns,continuous_date_columns,sym_ratio_colnames,log2zero_ratio_colnames,sym_rel_per_colnames)







