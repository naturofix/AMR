#Patient Select Defaults
completeness = 20 # percentage of datapoints required to automatically include patients
patient_custom_exclude = c('4941516', '4964463', '5863880', '5881350', '5884461', '5900493', '5928232', '5931655', '5931655_a', '4854594', '4941516', '5895629', '4905274', '4977337', '5876473', '5916182')
subset_1 = 'All'
select_subset_1 = 'All'

subset_2 = 'All'
select_subset_2 = 'All'

subset_3 = 'All'
select_subset_3 = 'All'



#Header Defaults
pre_values = c(-3,0)
post_values = c(0,3)
## Data Select options : "pFEV", "imputed",'smoothed', 'd1','d1_ri','d2'
data_select = 'pFEV'


# Clustering Defaults
#discrete columns

discrete_list_1 = c("SignOfInflammation","BiopsyScore")
d_weight_1 = 10
discrete_list_2 = c("NewCTChange","Ground glass","HLAType","HLAStrongWeak")
d_weight_2 = 5

#continuous columns
continuous_list_1 = c('D1_-3','D1_-1','D1_0')
c_weight_1 = 4
continuous_list_2 = c("Eosinophil peak")
c_weight_2 = 10

num_clusters = 3
  
# Tabs : Data Tables, Patient pFEV, Plots, Statistics, Clustering, BOS, R Session Info
default_tab = 'Patient pFEV'


### Columns Extracted from the Google Sheet  ###

date_columns = c("DeathDate","DoTPE","DoB","DoTx")
continuous_date_columns = c("YearsToDeath",'MonthSinceRx',"Yr since Tx","Age at Rx","MonthsToDeath")

#### continuous columns ####
pFEV_cols = c("pFEV1_neg24","pFEV1_neg18", "pFEV1_neg12", "pFEV1_neg6", "pFEV1_neg5", "pFEV1_neg4", "pFEV1_neg3"     
              ,"pFEV1_neg2", "pFEV1_neg1", "pFEV1_0", "pFEV1_pos1", "pFEV1_pos2", "pFEV1_pos3"     
              ,"pFEV1_pos4", "pFEV1_pos5", "pFEV1_pos6", "pFEV1_pos12", "pFEV1_pos18", "pFEV1_pos24")
bos_cols = c("BOS1mnth", "BOS2mnth", "BOS3mnth","BOS 3 free survival")

change_cols = c("ChangeFEV1_12mth_prior", "ChangeFEV1_6mth_prior",  "ChangeFEV1_3mth_prior",  "ChangeFEV1_1mth_prior",  "ChangeFEV1_1mth_post",   "ChangeFEV1_3mth_post","ChangeFEV1_6mth_post")

continuous_columns = c("TotalMFI","Change DSA","CRP peak","Eosinophil peak","FEV1Ratio-3","FEV1Ratio0","FEV1Ratio+3","FinalFEV1Ratio","%Pred", "Best FEV1")


### Discrete Columns ####
discrete_numeric_columns = c("AltDxScore","DSA_HLAScore","HLAType","BiopsyScore",
                             "NewCTChange","AGrade","BGrade","ViralPCR",
                             "BactCulture","Methylpred","IVAbx","HLAStrongWeak","AMRClassification",
                             "C1Q","SignOfInflammation","CRPRaised","EosRaised","CTClassification"
                             ,"Ground glass","Consolidation","Pleural eff","CT3mnthPost",
                             "AMRPathScore","FOP","Airways","DAD","C4D","TPE","IVIG","Monoclonal")
#factor_columns_con = c("Status" , "DaysSinceOnset", "Obstructive")
discrete_term_columns = c("Status" , "DaysSinceOnset", "Obst_post","Symptoms","Type","Reason")







