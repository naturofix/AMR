roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
} 

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
  
lm_function = function(function_data,factor,cols){
  df = data.frame(Factor = numeric(), Status = numeric(0))
  full_data=function_data[function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      a = anova(fit)
      p = a$Pr[1]
      df[paste(factor,entry),'Factor'] = factor
      df[paste(factor,entry),'Status'] = entry
      df[paste(factor,entry),'ano_All'] = signif(p,3)
      l = lm(y~x)
      i = coef(l)['(Intercept)']
      m = coef(l)['x']
      df[paste(factor,entry),'Int_ALL'] = signif(i,3)
      df[paste(factor,entry),'slope_ALL'] = signif(m,3)
    }
  }
  
  full_data=function_data[function_data$variable %in% before & function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      a = anova(fit)
      p = a$Pr[1]
      df[paste(factor,entry),'ano_Pre'] = signif(p,3)
      l = lm(y~x)
      i = coef(l)['(Intercept)']
      m = coef(l)['x']
      df[paste(factor,entry),'Int_Pre'] = signif(i,3)
      df[paste(factor,entry),'slope_Pre'] = signif(m,3)
    }
  }
  
  
  full_data=function_data[function_data$variable %in% after & function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  entry = factor_levels[2]
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      a = anova(fit)
      p = a$Pr[1]
      df[paste(factor,entry),'ano_Post'] = signif(p,3)
      l = lm(y~x)
      i = coef(l)['(Intercept)']
      m = coef(l)['x']
      df[paste(factor,entry),'Int_Post'] = signif(i,3)
      df[paste(factor,entry),'slope_Post'] = signif(m,3)
    }
  }
  df = df[order(df$Status),]
  df = df[,c('Factor','Status','ano_All','ano_Pre','ano_Post')]
  
  return(df)
}

lm_sample_function = function(function_data,factor,cols,df){ 
  full_data=function_data[function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      a = anova(fit)
      p = a$Pr[1]
      df[entry,'ano_All'] = signif(p,3)
      l = lm(y~x)
      i = coef(l)['(Intercept)']
      m = coef(l)['x']
      df[entry,'Int_ALL'] = i
      df[entry,'slope_ALL'] = m
    }
  }
  
  full_data=function_data[function_data$variable %in% before & function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      a = anova(fit)
      p = a$Pr[1]
      df[entry,'ano_Pre'] = signif(p,3)
      l = lm(y~x)
      i = coef(l)['(Intercept)']
      m = coef(l)['x']
      df[entry,'Int_Pre'] = i
      df[entry,'slope_Pre'] = m
    }
  }
  
  
  full_data=function_data[function_data$variable %in% after & function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      a = anova(fit)
      p = a$Pr[1]
      df[entry,'ano_Post'] = signif(p,3)
      l = lm(y~x)
      i = coef(l)['(Intercept)']
      m = coef(l)['x']
      df[entry,'Int_Post'] = i
      df[entry,'slope_Post'] = m
    }
  }
  df = df[order(df$Status),]
  #df = df[,c('Factor','Status','ano_All','ano_Pre','ano_Post')]
  return(df)
  }

slope_function = function(full_data,factor,cols){
  df = data.frame(Factor = numeric())
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]

    mean_all = mean(data$slope_ALL,na.rm=T)
    mean_pre = mean(data$slope_Pre,na.rm=T)
    mean_post = mean(data$slope_Post,na.rm=T)
    pre_num = length(data$slope_Pre[!is.na(data$slope_Pre)])
    post_num = length(data$slope_Post[!is.na(data$slope_Post)])
    pre_sd = sd(data$slope_Pre,na.rm=T)
    post_sd = sd(data$slope_Post,na.rm=T)
    paired = FALSE
    if(pre_num == post_num){
      if(!(FALSE %in% (is.na(data$slope_Pre) == is.na(data$slope_Pre)))){
        paired = TRUE
      }
    }
    t_all = t.test(as.numeric(data$slope_Pre),as.numeric(data$slope_Post),paired = paired)
    
    s = 0
    if(t_all$p.value < 0.05){
      s = 1
    }
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    #df[paste(factor,entry),'mean_slope_All'] = signif(mean_all,3)
    df[paste(factor,entry),'p value'] = signif(t_all$p.value,3)
    
    df[paste(factor,entry),'mean_slope_Pre'] = signif(mean_pre,3)
    df[paste(factor,entry),'mean_slope_Post'] = signif(mean_post,3)
    #df[paste(factor,entry),'conf_1'] = signif(t_all$conf.int[1],3)
    #df[paste(factor,entry),'conf_2'] = signif(t_all$conf.int[2],3)
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
    df[paste(factor,entry),'Paired'] = paired
    
    df[paste(factor,entry),'significant'] = s
    
    df[paste(factor,entry),'function'] = 'slope_function'
    df[paste(factor,entry),'pre_data'] = paste(data$slope_Post,collapse = ', ')
    df[paste(factor,entry),'post_data'] = paste(data$slope_Post,collapse = ', ')
  }
  df

  return(df)
}

slope_boxplot_data_function = function(data,df,global_factor){
  slope_cols = c("slope_Pre","slope_Post")
  data$significant = factor(df$significant[match(data[,global_factor],df$Status)])
  data_l = melt(data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1','significant'), measure.vars = slope_cols)
  return(data_l)
}

slope_boxplot_function = function(data,global_factor){
  sig_col = c("white", "blanchedalmond")
  if(!(0 %in% data$significant)){
    sig_col = c("blanchedalmond")
  }
  
  ggplot(data)+
    geom_boxplot(aes_string(col = 'variable',y='value',x = global_factor,fill='significant')) +
    scale_fill_manual(values = sig_col)
  
}


pp_t_test_function = function(full_data,factor,t1,t2){
  df = data.frame(Factor = numeric(0))
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col1]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col2]

    pre_num = length(pre_data[!is.na(pre_data)])
    post_num = length(post_data[!is.na(post_data)])
    pre_sd = sd(pre_data,na.rm=T)
    post_sd = sd(post_data,na.rm=T)
    
    paired = FALSE
    if(pre_num == post_num){
      if(!(FALSE %in% is.na(pre_data) == is.na(post_data))){
        paired = TRUE
      }
    }
    t = t.test(pre_data,post_data,paired = paired)
    p = t$p.value
    s = 0
    if(p < 0.05){
      s = 1
    }

    
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'p value'] = signif(p,3)
    df[paste(factor,entry),'pre mean'] = signif(mean(pre_data,na.rm=T),3)
    df[paste(factor,entry),'post mean'] = signif(mean(post_data,na.rm=T),3)
    df[paste(factor,entry),'pre num'] = signif(pre_num,3)
    df[paste(factor,entry),'post num'] = signif(post_num,3)
    df[paste(factor,entry),'pre sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post sd'] = signif(post_sd,3)
    df[paste(factor,entry),'paired'] = paired
    
    s = 0
    if(p < 0.05){
      s = 1
    }
    df[paste(factor,entry),'significant'] = s
    
    df[paste(factor,entry),'function'] = 'pp_t_test_function'
    df[paste(factor,entry),'pre_data'] = paste(pre_data,collapse = ', ')
    df[paste(factor,entry),'post_data'] = paste(post_data,collapse = ', ')
    
  }

  df = df[order(df$Status),]
  return(df)
}

pp_t_test_range_function = function(full_data,factor,pre1,pre2,post1,post2){
  df = data.frame(Factor = numeric(0))
  col1 = factor(c(pre1:pre2))
  col2 = factor(c(post1:post2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col1]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col2]
    pre_num = length(pre_data[!is.na(pre_data)])
    post_num = length(post_data[!is.na(post_data)])
    pre_sd = sd(pre_data,na.rm=T)
    post_sd = sd(post_data,na.rm=T)

    
    paired = FALSE
    if(pre_num == post_num){
      if(!(FALSE %in% is.na(pre_data) == is.na(post_data))){
        paired = TRUE
      }
    }
    t = t.test(pre_data,post_data,paired = paired)
    p = t$p.value
    s = 0
    if(p < 0.05){
      s = 1
    }

    
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'p value'] = signif(p,3)
    df[paste(factor,entry),'pre mean'] = signif(mean(pre_data,na.rm=T),3)
    df[paste(factor,entry),'post mean'] = signif(mean(post_data,na.rm=T),3)
    
    df[paste(factor,entry),'pre num'] = signif(pre_num,3)
    df[paste(factor,entry),'post num'] = signif(post_num,3)
    df[paste(factor,entry),'pre sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post sd'] = signif(post_sd,3)
    df[paste(factor,entry),'paired'] = paired
    
    s = 0
    if(p < 0.05){
      s = 1
    }
    df[paste(factor,entry),'significant'] = s
    
    df[paste(factor,entry),'function'] = 'pp_t_test_range_function'
    df[paste(factor,entry),'pre_data'] = paste(pre_data,collapse = ', ')
    df[paste(factor,entry),'post_data'] = paste(post_data,collapse = ', ')
    
    
  }
  
  df = df[order(df$Status),]
  return(df)
}

boxplot_pp_function = function(full_data,t1,t2,global_factor){
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  pre_data = full_data[full_data$variable %in% col1,]
  pre_data$treat = '0_pre'
  post_data = full_data[full_data$variable %in% col2,]
  post_data$treat = '1_post'
  
  pp_data = rbind(pre_data,post_data)
  
  p = ggplot(pp_data, aes_string(x = eval(global_factor),y = 'value',col='treat')) +
    geom_boxplot()
  return(p)
}

boxplot_pp_ranges_function = function(full_data,pre1,pre2,post1,post2,global_factor,df){
  col1 = factor(c(pre1:pre2))
  col2 = factor(c(post1:post2))
  pre_data = full_data[full_data$variable %in% col1,]
  pre_data$treat = '0_pre'
  post_data = full_data[full_data$variable %in% col2,]
  post_data$treat = '1_post'
  pp_data = rbind(pre_data,post_data)
  # sig_col = c("white", "blanchedalmond")
  # if(!(0 %in% pp_data$significant)){
  #   sig_col = c("blanchedalmond")
  # }
  pp_data$significant = factor(df$significant[match(pp_data[,global_factor],df$Status)])
  # p = ggplot(pp_data, aes_string(x = eval(global_factor),y = 'value',col='treat',fill='significant')) +
  #   geom_boxplot() + 
  #   scale_fill_manual(values = sig_col)
  
  return(pp_data)
}

t_boxplot_function = function(pp_data,global_factor){
  sig_col = c("white", "blanchedalmond")
  if(!(0 %in% pp_data$significant)){
    sig_col = c("blanchedalmond")
  }
  #pp_data$significant = factor(df$significant[match(pp_data[,global_factor],df$Status)])
  p = ggplot(pp_data, aes_string(x = eval(global_factor),y = 'value',col='treat',fill='significant')) +
    geom_boxplot() + 
    scale_fill_manual(values = sig_col)
  
  return(p)
}

  
pp_t_test_ratio_full_function = function(full_data,t1,t2){
  df = data.frame(Factor = numeric(0))
  factor = 'ALL'
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  entry = 'ALL'
  pre_data = full_data$value[full_data$variable == t1]
  zero = full_data$value[full_data$variable == 0]
  post_data = full_data$value[full_data$variable == t2]
  pre_ratio = log2(zero/pre_data)
  post_ratio = log2(post_data/zero)
  pre_num = length(pre_ratio[!is.na(pre_ratio)])
  post_num = length(post_ratio[!is.na(post_ratio)])

  
  paired = FALSE
  if(pre_num == post_num){
    if(!(FALSE %in% is.na(pre_ratio) == is.na(post_ratio))){
      paired = TRUE
    }
  }

  t = t.test(na.omit(pre_ratio),na.omit(post_ratio),alternative = c("two.sided"))

  p = t$p.value

  df[paste(factor,entry),'Factor'] = factor
  df[paste(factor,entry),'Status'] = entry
  df[paste(factor,entry),'T_p_value'] = signif(p,3)
  df[paste(factor,entry),'pre mean'] = signif(mean(pre_ratio,na.rm=T),3)
  df[paste(factor,entry),'post mean'] = signif(mean(post_ratio,na.rm=T),3)
  df[paste(factor,entry),'pre num'] = signif(pre_num,3)
  df[paste(factor,entry),'post num'] = signif(post_num,3)
  df[paste(factor,entry),'pre sd'] = signif(sd(post_ratio,na.rm=T),3)
  df[paste(factor,entry),'post sd'] = signif(sd(post_ratio,na.rm=T),3)
  df[paste(factor,entry),'paired'] = paired
  s = 0
  if(p < 0.05){
    s = 1
  }
  df[paste(factor,entry),'significant'] = s
  df[paste(factor,entry),'function'] = 'pp_t_test_ratio_full_function'
  df[paste(factor,entry),'pre_data'] = paste(pre_ratio,collapse = ', ')
  df[paste(factor,entry),'post_data'] = paste(post_ratio,collapse = ', ')
  
  
  

  df = df[order(df$Status),]
  return(df)
  
}
boxplot_pp_ratio_full_function = function(full_data,t1,t2){
  factor = 'All'
  entry = 1
  df = data.frame(Factor = numeric(0), Status = numeric(0),pre = numeric(0),post = numeric(0))
  pre_data = full_data$value[full_data$variable == t1]
  zero = full_data$value[full_data$variable == 0]
  post_data = full_data$value[full_data$variable == t2]
  pre_ratio = log2(zero/pre_data)
  post_ratio = log2(post_data/zero)
  df1 = data.frame(Factor = factor, Status = entry,pre = pre_ratio,post = post_ratio)
  df = rbind(df,df1)
  df_m = melt(df,id.vars = c('Factor','Status'))
  p =  ggplot(df_m, aes(x = Status,y=value,col=variable)) +
    geom_hline(yintercept=0)+
    geom_boxplot()
  return(p)
}

pp_t_test_zero_function = function(full_data,factor,t1,t2){
  df = data.frame(Factor = numeric(0))
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable == t1]
    zero = full_data$value[full_data[,factor] == entry & full_data$variable == 0]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable == t2]

    pre_num = length(pre_data[!is.na(pre_data)])
    post_num = length(post_data[!is.na(post_data)])
    zero_num = length(zero[!is.na(zero)])
    pre_sd = sd(pre_data,na.rm=T)
    post_sd = sd(post_data,na.rm=T)
    zero_sd = sd(zero,na.rm=T)
    
    pre_paired = FALSE
    if(pre_num == zero_num){
      if(!FALSE %in% (is.na(pre_data) == is.na(post_data))){
        pre_paired = TRUE
      }
    }
    post_paired = FALSE
    if(post_num == zero_num){
      if(!FALSE %in% (is.na(pre_data) == is.na(post_data))){
        post_paired = TRUE
      }
    }
    pre_t = t.test(na.omit(pre_data),na.omit(zero),paired = pre_paired)
    post_t = t.test(na.omit(post_data),na.omit(zero),paired = post_paired)
    p_pre = pre_t$p.value
    p_post = post_t$p.value

    
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'pre p value'] = signif(p_pre,3)
    df[paste(factor,entry),'post p value'] = signif(p_post,3)
    
    df[paste(factor,entry),'pre mean'] = signif(mean(pre_data,na.rm=T),3)
    df[paste(factor,entry),'zero mean'] = signif(mean(zero,na.rm=T),3)
    df[paste(factor,entry),'post mean'] = signif(mean(post_data,na.rm=T),3)
    
    df[paste(factor,entry),'pre num'] = signif(pre_num,3)
    df[paste(factor,entry),'zero num'] = signif(zero_num,3)
    df[paste(factor,entry),'post num'] = signif(post_num,3)
    
    df[paste(factor,entry),'pre sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'zero sd'] = signif(zero_sd,3)
    df[paste(factor,entry),'post sd'] = signif(post_sd,3)
    
    df[paste(factor,entry),'pre paired'] = pre_paired
    df[paste(factor,entry),'post paired'] = post_paired
    pre_s = 0
    if(p_pre < 0.05){
      pre_s = 1
    }
    post_s = 0
    if(p_post < 0.05){
      post_s = 1
    }
    df[paste(factor,entry),'pre_significant'] = pre_s
    df[paste(factor,entry),'post_significant'] = post_s
    
    
    
    df[paste(factor,entry),'function'] = 'pp_t_test_zero_function'
    df[paste(factor,entry),'pre_data'] = paste(pre_data,collapse = ', ')
    df[paste(factor,entry),'zero_data'] = paste(zero,collapse = ', ')
    df[paste(factor,entry),'post_data'] = paste(post_data,collapse = ', ')
    
    

  }
  df = df[order(df$Status),]
  return(df)
}
boxplot_pp_zero_function = function(full_data,factor,t1,t2){
  df = data.frame(Factor = numeric(0))
  df = data.frame(Factor = numeric(0), Status = numeric(0),pre = numeric(0),zero = numeric(),post = numeric(0))
  
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable == t1]
    zero = full_data$value[full_data[,factor] == entry & full_data$variable == 0]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable == t2]
    df1 = data.frame(Factor = factor, Status = entry,pre = pre_data,zero = zero,post = post_data)

    df = rbind(df,df1)
  }
  df_m = melt(df)
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  
  p = ggplot(df_m, aes(x = Status,y=value,col=variable)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    scale_x_discrete(limits = u)
  
  return(p)
}


boxplot_pp_zero_data_function = function(full_data,factor,t1,t2,df_s){
  df = data.frame(Factor = numeric(0))
  df = data.frame(Factor = numeric(0), Status = numeric(0),pre = numeric(0),zero = numeric(),post = numeric(0))
  
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable == t1]
    zero = full_data$value[full_data[,factor] == entry & full_data$variable == 0]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable == t2]
    df1 = data.frame(Factor = factor, Status = entry,pre = pre_data,zero = zero,post = post_data)
    
    df = rbind(df,df1)
  }
  df_m = melt(df)
  sig_list = c()
  for(i in c(1:dim(df_m)[1])){
    row_entry = df_m[i,]
    #print(row_entry)
    if(row_entry$variable == 'pre'){
      sig = df_s$pre_significant[df_s$Status == row_entry$Status]
    }
    if(row_entry$variable == 'post'){
      sig = df_s$post_significant[df_s$Status == row_entry$Status]
    }
    if(row_entry$variable == 'zero'){
      sig = '0'
    }
    sig_list = c(sig_list,sig)
  }
  #print(sig_list)
  df_m$significant = factor(sig_list)
  #df_m$significant[df$variable == 'pre'] = factor(df_s$pre_significant[match(df_m$Status[df$variable == 'pre'],df_s$Status)])
  
  return(df_m)
}

boxplot_pp_zero_plot_function = function(df_m){
  
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  sig_col = c("white", "blanchedalmond")
  if(!(0 %in% df_m$significant)){
    sig_col = c("blanchedalmond")
  }
  p = ggplot(df_m, aes(x = Status,y=value,col=variable,fill = significant)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    scale_x_discrete(limits = u) +
    scale_fill_manual(values = sig_col)
  
  return(p)
}



pp_t_test_ratio_function = function(full_data,factor,t1,t2){
  df = data.frame(Factor = numeric(0))
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable == t1]
    zero = full_data$value[full_data[,factor] == entry & full_data$variable == 0]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable == t2]
    pre_ratio = log2(zero/pre_data)
    post_ratio = log2(post_data/zero)
    
    pre_change_per = round(mean(((zero-pre_data)/pre_data)*100,na.rm=T),2)
    post_change_per = round(mean(((post_data-zero)/zero)*100,na.rm=T),2)


    
    pre_num = length(pre_data[!is.na(pre_ratio)])
    post_num = length(post_data[!is.na(post_ratio)])
    pre_mean = mean(pre_ratio,na.rm=T)
    post_mean = mean(post_ratio,na.rm=T)
    pre_sd = sd(pre_ratio,na.rm=T)
    post_sd = sd(post_ratio,na.rm=T)
    
    paired = FALSE
    if(pre_num == post_num){
      if(!(FALSE %in% is.na(pre_ratio) == is.na(post_ratio))){
        paired = TRUE
      }
    }
    
    t = t.test(na.omit(pre_ratio),na.omit(post_ratio),alternative = c("two.sided"),paired = paired)
    p = t$p.value
    
    s = 0
    if(p < 0.05){
      s = 1
    }

    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'p value'] = signif(p,3)
    df[paste(factor,entry),'pre_mean'] = signif(pre_mean,3)
    df[paste(factor,entry),'post_mean'] = signif(post_mean,3)
    df[paste(factor,entry),'Pre percentage change'] = paste(pre_change_per,'%')
    df[paste(factor,entry),'Post perentage change'] = paste(post_change_per,'%')
    
    
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
    df[paste(factor,entry),'Paired'] = paired
    
    df[paste(factor,entry),'significant'] = s
    
    df[paste(factor,entry),'function'] = 'pp_t_test_ratio_function'
    df[paste(factor,entry),'pre_data'] = paste(pre_ratio,collapse = ', ')
    df[paste(factor,entry),'post_data'] = paste(post_ratio,collapse = ', ')
    
    
  }
  df = df[order(df$Status),]
  return(df)
}


boxplot_pp_ratio_function = function(full_data,factor,t1,t2,df_s){
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  df = data.frame(Factor = numeric(0), Status = numeric(0),pre = numeric(0),post = numeric(0))
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable == t1]
    zero = full_data$value[full_data[,factor] == entry & full_data$variable == 0]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable == t2]
    pre_ratio = log2(zero/pre_data)
    post_ratio = log2(post_data/zero)
    df1 = data.frame(Factor = factor, Status = entry,pre = pre_ratio,post = post_ratio)
    df = rbind(df,df1)
  }
  df_m = melt(df)
  #df_m$significant = factor(df_s$significant[match(df_m[,factor],df_s$Status)])
  
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  #sig_col = c("white", "blanchedalmond")
  #if(!(0 %in% df_m$significant)){
  #  sig_col = c("blanchedalmond")
  #}
  p = ggplot(df_m, aes(x = Status,y=value,col=variable)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    scale_x_discrete(limits = u)
   # scale_fill_manual(values = sig_col)
  
  
  return(p)
}


boxplot_pp_ratio_data_function = function(full_data,global_factor,t1,t2,df_s){
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,global_factor])
  df = data.frame(Factor = numeric(0), Status = numeric(0),pre = numeric(0),post = numeric(0))
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,global_factor] == entry & full_data$variable == t1]
    zero = full_data$value[full_data[,global_factor] == entry & full_data$variable == 0]
    post_data = full_data$value[full_data[,global_factor] == entry & full_data$variable == t2]
    pre_ratio = log2(zero/pre_data)
    post_ratio = log2(post_data/zero)
    df1 = data.frame(Factor = global_factor, Status = entry,pre = pre_ratio,post = post_ratio)
    df = rbind(df,df1)
  }
  
  df_m = melt(df)
  df_m$significant = factor(df_s$significant[match(df_m$Status,df_s$Status)])
  
  df_m
}
  #df_m$significant = factor(df_s$significant[match(df_m[,factor],df_s$Status)])
boxplot_pp_ratio_plot_function = function(df_m){ 
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  #sig_col = c("white", "blanchedalmond")
  #if(!(0 %in% df_m$significant)){
  #  sig_col = c("blanchedalmond")
  #}
  df_m = df_m[!is.na(df_m$value),]
  sig_col = c("white", "blanchedalmond")
  if(!(0 %in% df_m$significant)){
    sig_col = c("blanchedalmond")
  }
  p = ggplot(df_m, aes(x = Status,y=value,col=variable, fill = significant)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    scale_x_discrete(limits = u) + 
    scale_fill_manual(values = sig_col)
  # scale_fill_manual(values = sig_col)
  
  
  return(p)
}




###### CLUSTERING #########
# clustering_function(data,retained_patients(),input$clutree_num,
#                     input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
#                     input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)



clustering_function = function(full_data,r_list,d_num,
                               fac_w_1,fac_col_list_1,fac_w_2,fac_col_list_2,
                               num_w_1,num_col_list_1,num_w_2,num_col_list_2){
  data = i_pFEV_wf[r_list,c(30:50)]
  rownames(data)
  data = data[complete.cases(data),]
  weights = rep(10,dim(data)[2])
  #d_num = 3
  clust_fac_col_list = c(fac_col_list_1,fac_col_list_2)
  clust_num_col_list = c(num_col_list_1,num_col_list_2)
  clust_num_col_list = clust_num_col_list[order(as.numeric(clust_num_col_list))]
  clust_col_list = c(clust_num_col_list,clust_fac_col_list)
  #print(clust_col_list)
  
  
  data = full_data[r_list,clust_col_list]
  o_data = data
  
  
  weights_2 = c()
  for(entry in colnames(data)){
    weight_v = 0
    if(entry %in% fac_col_list_2){
      weight_v = fac_w_2
    }
    if(entry %in% fac_col_list_1){
      weight_v = fac_w_1
    }
    if(entry %in% num_col_list_2){
      weight_v = num_w_2
    }
    if(entry %in% num_col_list_1){
      weight_v = num_w_1
    }
    #print(weight_v)
    weights_2 = c(weights_2,weight_v)
  }
  weights = weights_2
  data_dist = dist.subjects(data,weights = weights)
  #d_scale = cmdscale(data_dist)
  D = dendro.subjects(data_dist,weights = weights)
  dendr <- dendro_data(D, type = "rectangle") 
  x <- cutree(D, k = d_num)               # find 'cut' clusters
  x_cluster <- data.frame(label = names(x), cluster = x)
  
  #x = cutree(D, k = d_num)
  #x_cluster = data.frame(MRN = numeric(0))
  #for(entry in unique(x)){
  #  x_cluster[entry,'MRN'] = paste(list(names(x)[x == entry]),colapse=(", "))
  #}
  
  data$cluster = factor(x_cluster$cluster[match(rownames(data),x_cluster$label)])
  #data$cluster = factor(x) 
  result = list(data_dist = data_dist,D = D, o_data = o_data, data = data, x = x, x_cluster = x_cluster, weights = weights)
  return(result)
  #return(result)
}

clust_comparison_total = function(df,clust_col){
  num_clusters = unique(df[,clust_col])
  num_clusters = num_clusters[order(num_clusters)]
  df_c = data.frame(cluster = num_clusters)
  rownames(df_c) = df_c$num_clusters
  total = dim(df)[1]
  for(factor_name in colnames(full_fac_0)[-1]){
    for(i in num_clusters){
      status = unique(df[,factor_name])
      status = status[order(status)]
      for(j in status){
        status_total = length(df[,factor_name][df[,factor_name] == j])
        total = status_total
        num = length(df[,factor_name][df[,clust_col] == i & df[,factor_name] == j])
        per = round((num/total)*100,2)
        df_c["Factor",paste(factor_name,j,sep='_')] = factor_name
        df_c["Status",paste(factor_name,j,sep='_')] = j
        df_c["Data",paste(factor_name,j,sep='_')] = clust_col
        df_c[i,paste(factor_name,j,sep='_')] = per
      }
    }
  }
  df_tc = as.data.frame(t(df_c))
  df_tc = df_tc[,c('Factor','Status',num_clusters)]
  return(df_tc)
}

clust_comparison_within = function(df,clust_col){
  num_clusters = unique(df[,clust_col])
  num_clusters = num_clusters[(order(num_clusters))]
  #print(num_clusters)
  df_c = data.frame(cluster = num_clusters)
  rownames(df_c) = df_c$num_clusters

  for(i in num_clusters){
    df_clust = df[df[,clust_col] == i,]
    total = dim(df_clust)[1]
    for(factor_name in colnames(full_fac_0)[-1]){
      status = unique(df_clust[,factor_name])
      status = status[order(status)]
      for(j in status){
        num = length(df_clust[,factor_name][df_clust[,factor_name] == j])
        per = round((num/total)*100,2)
        df_c["Factor",paste(factor_name,j,sep='_')] = factor_name
        df_c["Status",paste(factor_name,j,sep='_')] = j
        df_c["Data",paste(factor_name,j,sep='_')] = clust_col
        df_c[i,paste(factor_name,j,sep='_')] = per
      }
    }
  }

  df_tc = as.data.frame(t(df_c))
  df_tc = df_tc[,c('Factor','Status',num_clusters)]
  df_tc
  return(df_tc)
}

dendrogram_plot_function = function(dendr,x_cluster,cut){
  dendr[["labels"]] <- merge(dendr[["labels"]],x_cluster, by="label")
  
  ## identify all the line above cluster to remove the colours
  height <- unique(dendr$segments$y)[order(unique(dendr$segments$y), decreasing = TRUE)]
  cut.height <- mean(c(height[cut], height[cut-1]))
  dendr$segments$line <- ifelse(dendr$segments$y == dendr$segments$yend &
                                  dendr$segments$y > cut.height, 1, 2)
  dendr$segments$line <- ifelse(dendr$segments$yend  > cut.height, 1, dendr$segments$line)
  
  
  dendr$segments$cluster = factor(dendr$labels$cluster[match(dendr$segments$x,dendr$labels$x)])
  dc = as.data.frame(dendr$segments)
  for(cl in unique(na.omit(dc$cluster))){
    maxx = max(dc$x[dc$cluster == cl],na.rm=T)
    minx = min(dc$x[dc$cluster == cl],na.rm=T)
    dc$cluster[dc$x <= maxx & dc$xend <= maxx & dc$x >= minx & dc$xend >= minx] = cl
  }
  
  dc$cluster[dc$line == 1] = NA
  clusters = factor(unique(na.omit(dc$cluster)))
  clusters = clusters[order(clusters)]
  
  p = ggplot() +
    geom_segment(data = dc, aes(x=x, y=y, xend=xend, yend=yend,colour = cluster),size = 1, show.legend = T) +
    geom_text(data = label(dendr), aes(x, y, label = label, colour = factor(cluster)), 
              hjust = 1,angle = 90, size = 3, show.legend = F) +
    scale_y_continuous(expand = c(.2, 0))+
    scale_color_discrete(breaks = clusters) +
    theme(axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank()
          
    )
  return(p)
}


BOS_function_pFEV = function(full_data,imputed = F){
  BOS1 = 0.8
  BOS2 = 0.66
  BOS3 = 0.5
  d_date = full_data$MonthsToEvent
  data = full_data[,pFEV_numeric_colnames_f]
  
  total_pFEV = unlist(apply(data,2,function(x) length(na.omit(x))))
  total_pFEV
  bos_df = data.frame(column = colnames(data),time = pFEV_numeric_colnames_n)
  bos_df$Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(d_date[d_date < x]))))
  bos_df$Dead_diff = c(bos_df$Dead[1],diff(bos_df$Dead))
  bos_df$total_pFEV = total_pFEV

  bos_df$total = total_pFEV


  
  bos_df$BOS1_num = apply(data,2,function(x) length(na.omit(x[x > BOS1])))
  bos_df$BOS2_num = apply(data,2,function(x) length(na.omit(x[x > BOS2])))
  bos_df$BOS3_num = apply(data,2,function(x) length(na.omit(x[x > BOS3])))
  
  bos_df$BOS1_num_l = apply(data,2,function(x) length(na.omit(x[x < BOS1])))
  bos_df$BOS2_num_l = apply(data,2,function(x) length(na.omit(x[x < BOS2])))
  bos_df$BOS3_num_l = apply(data,2,function(x) length(na.omit(x[x < BOS3])))
  
  #bos_df = mutate(bos_df, BOS3_num = BOS2_num + Dead)

    bos_df = mutate(bos_df,
                    BOS1_per = BOS1_num/total*100,
                    BOS2_per = BOS2_num/total*100,
                    BOS3_per = BOS3_num/total*100
                    #Surv_diff = c(0,diff(total_pFEV))
    )
  
  
  bos_df = mutate(bos_df,
                  BOS1_diff = c(BOS1_num_l[1],diff(BOS1_num_l)),
                  BOS2_diff = c(BOS2_num_l[1],diff(BOS2_num_l)),
                  BOS3_diff = c(BOS3_num_l[1],diff(BOS3_num_l))
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



BOS_function = function(full_data){
  BOS1 = 0.8
  BOS2 = 0.66
  BOS3 = 0.5
  end_time = 50
  MRN = full_data$MRN
  d_date = full_data$MonthsToEvent
  rx = full_data$MonthSinceRx
  status = full_data$Status
  BOS1_date = full_data$BOS1mnth
  BOS2_date = full_data$BOS2mnth
  BOS3_date = full_data$BOS3mnth
  BOS3_surv_date = full_data$`BOS 3 free survival`
  
  BOS1_date[is.na(BOS1_date)] = end_time
  BOS2_date[is.na(BOS2_date)] = end_time
  BOS3_date[is.na(BOS3_date)] = end_time
  BOS3_surv_date[is.na(BOS3_surv_date)] = end_time
  
  time = seq(-24,48,1)
  bos_df = data.frame(time = time)

  bos_df$BOS1_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[x >= BOS1_date]))))
  bos_df$BOS2_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS2_date[x >= BOS2_date]))))
  bos_df$BOS3_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_date[x >= BOS3_date]))))
  bos_df$BOS3_surv_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_surv_date[x >= BOS3_surv_date]))))
  
  bos_df$BOS1_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date < x & !is.na(d_date) & x < BOS1_date]))))
  bos_df$BOS2_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date < x & !is.na(d_date) & x < BOS2_date]))))
  bos_df$BOS3_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date < x & !is.na(d_date) & x < BOS3_date]))))
  bos_df$BOS3_surv_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date < x & !is.na(d_date) & x < BOS3_surv_date]))))
  
  
  bos_df$BOS1_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx < x & !is.na(rx) & x < BOS1_date]))))
  bos_df$BOS2_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx < x & !is.na(rx) & x < BOS2_date]))))
  bos_df$BOS3_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx < x & !is.na(rx) & x < BOS3_date]))))
  bos_df$BOS3_surv_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx < x & !is.na(rx) & x < BOS3_surv_date]))))
  

  bos_df = mutate(bos_df, 
                  BOS1_num_diff = c(0,diff(BOS1_num)),
                  BOS2_num_diff = c(0,diff(BOS2_num)),
                  BOS3_num_diff = c(0,diff(BOS3_num)),
                  BOS3_surv_num_diff = c(0,diff(BOS3_surv_num))
  )
  bos_df = mutate(bos_df, 
                  BOS1_Dead_diff = c(0,diff(BOS1_Dead)),
                  BOS2_Dead_diff = c(0,diff(BOS2_Dead)),
                  BOS3_Dead_diff = c(0,diff(BOS3_Dead)),
                  BOS3_surv_Dead_diff = c(0,diff(BOS3_surv_Dead))
  )
  bos_df = mutate(bos_df, 
                  BOS1_Censor_diff = c(0,diff(BOS1_Censor)),
                  BOS2_Censor_diff = c(0,diff(BOS2_Censor)),
                  BOS3_Censor_diff = c(0,diff(BOS3_Censor)),
                  BOS3_surv_Censor_diff = c(0,diff(BOS3_surv_Censor))
  )
  
  bos_df = mutate(bos_df, 
                    BOS1_risk = dim(full_data)[1] - c(0,BOS1_num[-length(BOS1_num)]) - c(0,BOS1_Dead[-length(BOS1_Dead)]) - c(0,BOS1_Censor[-length(BOS1_Censor)]),
                    BOS2_risk = dim(full_data)[1] - c(0,BOS2_num[-length(BOS2_num)]) - c(0,BOS2_Dead[-length(BOS2_Dead)]) - c(0,BOS2_Censor[-length(BOS2_Censor)]),
                    BOS3_risk = dim(full_data)[1] - c(0,BOS3_num[-length(BOS3_num)]) - c(0,BOS3_Dead[-length(BOS3_Dead)]) - c(0,BOS3_Censor[-length(BOS3_Censor)]),
                    BOS3_surv_risk = dim(full_data)[1] - c(0,BOS3_surv_num[-length(BOS3_surv_num)]) - c(0,BOS3_surv_Dead[-length(BOS3_surv_Dead)]) - c(0,BOS3_surv_Censor[-length(BOS3_surv_Censor)])
                  
                                    )
  
  bos_df = mutate(bos_df, 
                  BOS1_prog = c(BOS1_num[1] + BOS1_Dead[1], BOS1_num_diff[-1] + BOS1_Dead_diff[-1]),
                  BOS2_prog = c(BOS2_num[1] + BOS2_Dead[1], BOS2_num_diff[-1] + BOS2_Dead_diff[-1]),
                  BOS3_prog = c(BOS3_num[1] + BOS3_Dead[1], BOS3_num_diff[-1] + BOS3_Dead_diff[-1]),
                  BOS3_surv_prog = c(BOS3_surv_num[1] + BOS3_surv_Dead[1], BOS3_surv_num_diff[-1] + BOS3_surv_Dead_diff[-1])
  )
  
  bos_df = mutate(bos_df, 
                  BOS1_prog_per = BOS1_prog / BOS1_risk,
                  BOS2_prog_per = BOS2_prog / BOS2_risk,
                  BOS3_prog_per = BOS3_prog / BOS3_risk,
                  BOS3_surv_prog_per = BOS3_surv_prog / BOS3_surv_risk
  )
  

  t = 1 *(1 - bos_df$BOS1_prog_per[1])
  b = c(t)
  for(entry in bos_df$BOS1_prog_per[-1]){
    t = t*(1-entry)
    #print(t)
    b = c(b,t)
  }
  bos_df$BOS1_free = b*100
  
  t = 1 *(1 - bos_df$BOS2_prog_per[1])
  b = c(t)
  for(entry in bos_df$BOS2_prog_per[-1]){
    t = t*(1-entry)
    #print(t)
    b = c(b,t)
  }
  bos_df$BOS2_free = b*100
  
  t = 1 *(1 - bos_df$BOS3_prog_per[1])
  b = c(t)
  for(entry in bos_df$BOS3_prog_per[-1]){
    t = t*(1-entry)
    #print(t)
    b = c(b,t)
  }
  bos_df$BOS3_free = b*100
  
  t = 1 *(1 - bos_df$BOS3_surv_prog_per[1])
  b = c(t)
  for(entry in bos_df$BOS3_surv_prog_per[-1]){
    t = t*(1-entry)
    #print(t)
    b = c(b,t)
  }
  bos_df$BOS3_surv_free = b*100
  
  return(bos_df)
}


BOSS_plot = function(bos_df){
  bos_data = bos_df[,c('time',"BOS1_free","BOS2_free","BOS3_free")]
  bos_data
  m_bos = melt(bos_data,id.var = 'time')
  ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
    geom_line() +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)
}

BOSS_plot_per = function(bos_df){
  bos_data = bos_df[,c('time',"BOS1_per","BOS2_per","BOS3_per")]
  bos_data
  m_bos = melt(bos_data,id.var = 'time')
  ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
    geom_line() +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)
}


BOSS_plot_smooth = function(bos_df){
  bos_data = bos_df[,c('time',"BOS1_free","BOS2_free","BOS3_free")]
  bos_data
  m_bos = melt(bos_data,id.var = 'time')
  ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
    geom_smooth() +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)
}


BOSS_plot_smooth_per = function(bos_df){
  bos_data = bos_df[,c('time',"BOS1_per","BOS2_per","BOS3_per")]
  bos_data
  m_bos = melt(bos_data,id.var = 'time')
  ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
    geom_smooth() +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)
}


BOS_factor_plot = function(m_bos,col_name,global_factor,x1,x2){
  m_bos3 = m_bos[m_bos$variable == col_name,]
  p = ggplot(m_bos3, aes(x = time, y = value,col=Status)) +
    guides(col=guide_legend(title=global_factor)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_line() +
    ggtitle(paste(col_name,'by', global_factor)) +

    xlim(x1,x2)
  return(p)
}

BOS_factor_plot_smooth = function(m_bos,col_name,global_factor,x1,x2){
  m_bos3 = m_bos[m_bos$variable == col_name,]
  p = ggplot(m_bos3, aes(x = time, y = value,col=Status)) +
    guides(col=guide_legend(title=global_factor)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_smooth() +
    ggtitle(paste(col_name,'by', global_factor)) +

    xlim(x1,x2)
  return(p)
}

