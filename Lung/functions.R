roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
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
    t_all = t.test(as.numeric(data$slope_Pre),as.numeric(data$slope_Post),na.rm=T)
    pre_num = length(data$slope_Pre[!is.na(data$slope_Pre)])
    post_num = length(data$slope_Post[!is.na(data$slope_Post)])
    pre_sd = sd(data$slope_Pre,na.rm=T)
    post_sd = sd(data$slope_Post,na.rm=T)
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'mean_slope_All'] = signif(mean_all,3)
    df[paste(factor,entry),'mean_slope_Pre'] = signif(mean_pre,3)
    df[paste(factor,entry),'mean_slope_Post'] = signif(mean_post,3)
    df[paste(factor,entry),'p value'] = signif(t_all$p.value,3)
    df[paste(factor,entry),'conf_1'] = signif(t_all$conf.int[1],3)
    df[paste(factor,entry),'conf_2'] = signif(t_all$conf.int[2],3)
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
  }
  df
  return(df)
}


pp_t_test_function = function(full_data,factor,t1,t2){
  df = data.frame(Factor = numeric(0))
  col1 = factor(c(t1:-1))
  col2 = factor(c(1:t2))
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    pre_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col1]
    post_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col2]
    t = t.test(pre_data,post_data)
    p = t$p.value
    s = 0
    if(p < 0.05){
      s = 1
    }
    pre_num = length(pre_data[!is.na(pre_data)])
    post_num = length(post_data[!is.na(post_data)])
    pre_sd = sd(pre_data,na.rm=T)
    post_sd = sd(post_data,na.rm=T)
    
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'p value'] = signif(p,3)
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
    s = 0
    if(p < 0.05){
      s = 1
    }
    df[paste(factor,entry),'significant'] = s
    
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
    t = t.test(pre_data,post_data)
    p = t$p.value
    s = 0
    if(p < 0.05){
      s = 1
    }
    pre_num = length(pre_data[!is.na(pre_data)])
    post_num = length(post_data[!is.na(post_data)])
    pre_sd = sd(pre_data,na.rm=T)
    post_sd = sd(post_data,na.rm=T)
    
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'p value'] = signif(p,3)
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
    s = 0
    if(p < 0.05){
      s = 1
    }
    df[paste(factor,entry),'significant'] = s
    
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

boxplot_pp_ranges_function = function(full_data,pre1,pre2,post1,post2,global_factor){
  col1 = factor(c(pre1:pre2))
  col2 = factor(c(post1:post2))
  pre_data = full_data[full_data$variable %in% col1,]
  pre_data$treat = '0_pre'
  post_data = full_data[full_data$variable %in% col2,]
  post_data$treat = '1_post'
  
  pp_data = rbind(pre_data,post_data)
  
  p = ggplot(pp_data, aes_string(x = eval(global_factor),y = 'value',col='treat')) +
    geom_boxplot()
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

  t = t.test(na.omit(pre_ratio),na.omit(post_ratio),alternative = c("two.sided"))

  p = t$p.value

  df[paste(factor,entry),'Factor'] = factor
  df[paste(factor,entry),'Status'] = entry
  df[paste(factor,entry),'T_p_value'] = signif(p,3)
  s = 0
  if(p < 0.05){
    s = 1
  }
  df[paste(factor,entry),'significant'] = s
  

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

    pre_t = t.test(na.omit(pre_data),na.omit(zero))
    post_t = t.test(na.omit(post_data),na.omit(zero))
    p_pre = pre_t$p.value
    p_post = post_t$p.value
    pre_num = length(pre_data[!is.na(pre_data)])
    post_num = length(post_data[!is.na(post_data)])
    zero_num = length(zero[!is.na(zero)])
    pre_sd = sd(pre_data,na.rm=T)
    post_sd = sd(post_data,na.rm=T)
    zero_sd = sd(zero,na.rm=T)
    
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'pre p value'] = signif(p_pre,3)
    df[paste(factor,entry),'post p value'] = signif(p_post,3)
    
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'zero_num'] = signif(zero_num,3)
    
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
    df[paste(factor,entry),'zero_sd'] = signif(zero_sd,3)

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
    t = t.test(na.omit(pre_ratio),na.omit(post_ratio),alternative = c("two.sided"))
    p = t$p.value
    pre_num = length(pre_data[!is.na(pre_ratio)])
    post_num = length(post_data[!is.na(post_ratio)])
    pre_sd = sd(pre_ratio,na.rm=T)
    post_sd = sd(post_ratio,na.rm=T)
    df[paste(factor,entry),'Factor'] = factor
    df[paste(factor,entry),'Status'] = entry
    df[paste(factor,entry),'p value'] = signif(p,3)
    df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
    df[paste(factor,entry),'post_num'] = signif(post_num,3)
    df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
    df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
    s = 0
    if(p < 0.05){
      s = 1
    }
    df[paste(factor,entry),'significant'] = s
  }
  df = df[order(df$Status),]
  return(df)
}
boxplot_pp_ratio_function = function(full_data,factor,t1,t2){
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
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  
  p = ggplot(df_m, aes(x = Status,y=value,col=variable)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    scale_x_discrete(limits = u)
  
  return(p)
}

