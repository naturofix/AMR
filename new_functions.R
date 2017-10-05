
BOS_function = function(full_data,imputed = F){
  BOS1 = 0.8
  BOS2 = 0.66
  BOS3 = 0.5
  d_date = full_data$MonthsToEvent
  BOS1_date = full_data$BOS1mnth
  BOS2_date = full_data$BOS2mnth
  BOS3_date = full_data$BOS3mnth
  data = full_data[,pFEV_numeric_colnames_f]
  
  total_pFEV = unlist(apply(data,2,function(x) length(na.omit(x))))
  total_pFEV
  bos_df = data.frame(column = colnames(data),time = pFEV_numeric_colnames_n)
  bos_df$Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(d_date[d_date < x]))))
  bos_df$Dead_diff = c(bos_df$Dead[1],diff(bos_df$Dead))
  bos_df$total_pFEV = total_pFEV  
  
  bos_df$total = total_pFEV + bos_df$Dead
  
  bos_df$BOS1_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[BOS1_date < x]))))
  bos_df$BOS2_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[BOS2_date < x]))))
  bos_df$BOS3_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[BOS3_date < x]))))
  
  
  #bos_df$BOS1_num = apply(data,2,function(x) length(na.omit(x[x > BOS1])))
  #bos_df$BOS2_num = apply(data,2,function(x) length(na.omit(x[x > BOS2])))
  #bos_df$BOS3_num = apply(data,2,function(x) length(na.omit(x[x > BOS3])))
  
  #bos_df$BOS1_num_l = apply(data,2,function(x) length(na.omit(x[x < BOS1])))
  #bos_df$BOS2_num_l = apply(data,2,function(x) length(na.omit(x[x < BOS2])))
  #bos_df$BOS3_num_l = apply(data,2,function(x) length(na.omit(x[x < BOS3])))
  
  #bos_df = mutate(bos_df, BOS3_num = BOS2_num + Dead)
  
  bos_df = mutate(bos_df,
                  BOS1_per = BOS1_num/total*100,
                  BOS2_per = BOS2_num/total*100,
                  BOS3_per = BOS3_num/total*100,
                  Dead_per = Dead/total*100
                  #Surv_diff = c(0,diff(total_pFEV))
  )
  
  
  bos_df = mutate(bos_df,
                  BOS1_diff = c(BOS1_num[1],diff(BOS1_num)),
                  BOS2_diff = c(BOS2_num[1],diff(BOS2_num)),
                  BOS3_diff = c(BOS3_num[1],diff(BOS3_num)),
                  Dead_diff = c(Dead[1],diff(Dead))
                  
                  #Surv_diff = c(0,diff(total_pFEV))
  )
  
  
  bos_df = mutate(bos_df,
                  BOS1_prog = BOS1_diff,
                  BOS2_prog = BOS2_diff,
                  BOS3_prog = BOS3_diff
  )
  
  
  bos_df = mutate(bos_df,
                  BOS1_prog_per = BOS1_prog/total*100,
                  BOS2_prog_per = BOS2_prog/total*100,
                  BOS3_prog_per = BOS3_prog/total*100
  )
  
  b = c()
  t = 100
  for(entry in bos_df$BOS1_prog_per){
    t = t-entry
    b = c(b,t)
  }
  bos_df$BOS1_free = b
  
  b = c()
  t = 100
  for(entry in bos_df$BOS2_prog_per){
    t = t-entry
    b = c(b,t)
  }
  bos_df$BOS2_free = b
  
  b = c()
  t = 100
  for(entry in bos_df$BOS3_prog_per){
    t = t-entry
    b = c(b,t)
  }
  bos_df$BOS3_free = b
  
  return(bos_df)
}
