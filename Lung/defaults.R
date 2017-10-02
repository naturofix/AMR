completeness = 20 # percentage of datapoints required to automatically include patients
patient_custom_exclude = c('4924475','5863880','5931655','5884461') # list of patients to exclude
patient_custom_exclude = c()
default_cluster_list = c("BiopsyScore","HLAStrength","NewCTChange","SignOfInfection")
default_continuous_cluster_list = factor(c(-6, -3, -2, -1, 0, 1, 2, 3))