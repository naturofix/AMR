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

  clust_fac_col_list = c(fac_col_list_1,fac_col_list_2)
  clust_num_col_list = c(num_col_list_1,num_col_list_2)
  clust_num_col_list = clust_num_col_list[order(as.numeric(clust_num_col_list))]
  clust_col_list = c(clust_num_col_list,clust_fac_col_list)
  print(clust_col_list)
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
    print(weight_v)
    weights_2 = c(weights_2,weight_v)
  }
  weights = weights_2
  data_dist = dist.subjects(data,weights = weights)
  D = dendro.subjects(data_dist,weights = weights)
  x = cutree(D, k = d_num)
  x_cluster = data.frame(MRN = numeric(0))
  for(entry in unique(x)){
    x_cluster[entry,'MRN'] = paste(list(names(x)[x == entry]),colapse=(", "))
  }
  data$cluster = factor(x) 
  return(list(D = D, o_data = o_data, data = data, x_cluster = x_cluster, weights = weights))
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
