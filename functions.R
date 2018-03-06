

#### save variable functions ####
save_variable_function = function(variable_list){
  cmd_list = c()
  for(entry in variable_list){
    cmd = paste0("saveRDS(",entry,",'temp/",entry,".rds')")
    print(cmd)
    cmd_list = c(cmd_list,cmd)
    #eval(parse(text = cmd))
  }
  ### TO GET DATA BACK OUT ###
  #cmd_list = save_variable_function(variable_list)
  #lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #save_input_function(input)
  return(cmd_list)
}

save_input_function = function(input){
  save_input = list()
  input_list = names(reactiveValuesToList(input))
  
  for(entry in input_list){
    cmd = paste0("save_input[['",entry,"']] = input$",entry)
    print(cmd)
    
    eval(parse(text = cmd))
  }
  #input = readRDS('temp/save_input.rds')
  saveRDS(save_input,'temp/save_input.rds')
}

read_variable_function = function(variable_list){
  cmd_list = c()
  for(entry in variable_list){
    cmd = paste0(entry, " = readRDS('temp/",entry,".rds')")
    print(cmd)
    cmd_list = c(cmd_list,cmd)
    #eval(parse(text = cmd))
  }
  #cmd_list = read_variable_function(variable_list)
  #for(cmd in cmd_list){
  #  eval(parse(text = cmd))
  #}
  return(cmd_list)
}


source_https_2 <- function(url, ...) {
  # load package
  require(RCurl)
  
  # parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
  })
}

source_https_1 <- function(url, ...) {
  ## Function for sourcing individual R scripts from GitHub
  ## Author: Tony Breyal
  ## Contributions: Kay Cichini
  ## Original URL: http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
  ## Load required package
  require(RCurl)
  ## Parse and evaluate each .R script
  sapply(c(url, ...), function(u) {
    eval(parse(text = getURL(u, followlocation = TRUE,
                             cainfo = system.file("CurlSSL", "cacert.pem",
                                                  package = "RCurl"))),
         envir = .GlobalEnv)
  })
}

source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

source_github <- function(u) {
  # load package
  require(RCurl)
  
  # read script lines from website
  script <- getURL(u, ssl.verifypeer = FALSE)
  print(script)
  
  # parase lines and evaluate in the global environment
  eval(parse(text = script))
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
} 

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

########

order_columns = function(column_list,ordering_list){
  column_list = column_list[order(match(column_list,ordering_list))]
  return(column_list)
}


##### PROCESSING DATA ####

melt_processed_data = function(processed_data,factor_columns,matrix_column){
  #test = melt(processed_data, ,id.vars = full_factor_columns,measure_vars = processed_data$pFEV1_matrix)
  #processed_data = cbind(processed_data,processed_data$pFEV1_matrix)
  #full_factor_columns
  #p_cols
  colnames(processed_data)
  wide_df = processed_data[,c(factor_columns)]
  wide_df = cbind(wide_df,processed_data[,matrix_column])
  #head(wide_df)
  long_df = melt(wide_df,id.vars = factor_columns,measure_vars = p_cols)
  #head(long_df)
  long_df$time = as.numeric(as.character(long_df$variable))
  long_df[,all_discrete_columns] = apply(long_df[,all_discrete_columns],2,function(x) factor(x))
  data_list = list(wide_df = wide_df,long_df = long_df)
  return(data_list)
}




########### PLOTS############

makePlotContainers <- function(plot_name_list, prefix="individual") {
  print('makePlotContainers')
  lst <- lapply(plot_name_list, function(plot_name) {
    output_name = paste(plot_name,prefix,sep = '_')
    print(output_name)
    plotOutput(paste(plot_name,prefix,sep = '_'),height = 500)
  })
  
  do.call(tagList, lst)
}


renderPlots = function(full_data, patient_list, input, output, prefix = 'individual'){
  save_test = F
  if(save_test == T){
    variable_list = c('full_data','patient_list','prefix')
    cmd_list = save_variable_function(variable_list)
    lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
    save_input_function(input)
  }
  read_test = F
  if(read_test == T){
    #variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
    
    cmd_list = read_variable_function(variable_list)
    
    for(cmd in cmd_list){
      eval(parse(text = cmd))
    }
    #sapply(cmd_list, function(cmd) eval(parse(text = cmd)))
    
    input = readRDS('temp/save_input.rds')
  }
  
  #boxplot_path = sample_path_line
  
  
  #print('gene_list')
  #print(gene_list)
  patient = patient_list[1]
  
  line_size = 2
  point_size = 3
  sm_size = 1
  
  for(patient in patient_list){
    local({
      
      
      #print('gene')
      #print(gene)
      
      output_name = paste(patient,prefix,sep = '_')
      name = paste(patient)
      #plot_path = paste0(sample_path_line,'/',gene,'.png')
      #print(plot_path)
      #print(file.exists(plot_path))
      
      print('running ')
      print(output_name)
      
      
      #full_data = processed_long
      
      data = full_data[full_data$MRN %in% patient,]
      #print(dim(data))

      cols = data$time
      scale_cols = pFEV_numeric_colnames_n[pFEV_numeric_colnames_n %in% cols]
      scale_cols
      
      p = ggplot(data)
      

      p = p + 
        geom_vline(xintercept = 0)
      if(input$retained_radiobutton == 'bos'){
        if(length(data$RAS[!is.na(data$RAS)]) != 0){
          p = p + geom_vline(xintercept = data$RAS, color = 'blue',lwd = 1, linetype = 'solid')
        }
        p = p + geom_vline(xintercept = input$bos_range[1]) + 
          geom_vline(xintercept = input$bos_range[2]) +
          
          
          geom_vline(xintercept = data$BOS3_RAS, color = 'red4',lwd = 4, linetype = 'dotdash') +
          geom_vline(xintercept = data$BOS2_RAS, color = 'red',lwd = 3, linetype = 'dashed') +
          geom_vline(xintercept = data$BOS1_RAS, color = 'orange',lwd = 2, linetype = 'dotted') +
          
          
          geom_hline(yintercept = c(0.8), color = 'orange',linetype = 'dotted') + 
          geom_hline(yintercept = c(0.66), color = 'red',linetype = 'dashed') + 
          geom_hline(yintercept = c(0.5), color = 'red4',linetype = 'dotdash') + 
          
          geom_hline(yintercept = 0.7, color = 'blue')
      }
      
      p = p + geom_line(aes(x = time, y = i_pFEV1, group = MRN),col='red',size = line_size)
      p = p + geom_point(aes(x = time, y = pFEV1,group = MRN),col='yellow',size = point_size)
      p = p + geom_line(aes(x = time, y = sm_i_pFEV1,group = MRN),col='black',size = sm_size)
      
      p = p + geom_line(aes(x = time, y = i_pFVC, group = MRN),col='green',size = line_size)
      p = p + geom_point(aes(x = time, y = pFVC,group = MRN),col='yellow',size = point_size)
      p = p + geom_line(aes(x = time, y = sm_i_pFVC,group = MRN),col='black',size = sm_size)
      
      p = p + geom_line(aes(x = time, y = i_pRatio, group = MRN),col='blue',size = line_size)
      p = p + geom_point(aes(x = time, y = pRatio,group = MRN),col='yellow',size = point_size)
      p = p + geom_line(aes(x = time, y = sm_i_pRatio,group = MRN),col='black',size = sm_size)
      
      #p = p +  scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
      #          scale_colour_manual(name = 'the colour', 
      #                      values =c('black'='black','red'='red'), labels = c('c2','c1'))
      p = p + theme(axis.text.x = element_text(size=8, angle=90)) +
        #ylim(0,1) + 
        scale_x_continuous(breaks = scale_cols) +
        
        #theme(legend.position="top", show.legend = T) + 
        ggtitle(patient)
      
      #p = p + scale_colour_manual(values = c('red','green','blue'), labels = c('1','2','3'))
  
      
      
      
      #print(p)
 
        
        
        
        output[[output_name]] = renderPlot({
         print(p)
        })
      
    })
  }
}

renderPlots_BOS = function(BOS_columns,m_bos,input,output,prefix = 'BOS'){
  x1 = as.numeric(input$bos_range[1])
  x2 = as.numeric(input$bos_range[2])
  global_factor = input$global_factor
  
  for(plot_name in BOS_columns){
    local({
      output_name = paste(plot_name,prefix,sep = '_')
      
      p = BOS_factor_plot(m_bos,plot_name,global_factor,x1,x2)
      output[[output_name]] = renderPlot({
        print(p)
      })
    })
  }
}

line_plot_function = function(plot_data,title,input){
  ggplot(plot_data, aes(x = time, y = value,group = MRN)) + 
    geom_vline(xintercept = 0) +
    geom_line(aes_string(col = input$global_factor)) +
    geom_point(aes_string(col = input$global_factor)) +
    stat_summary(data = plot_data, fun.y=mean,geom="line",lwd=3,aes_string(x = 'time', y = 'value',group=input$global_factor,col = input$global_factor)) +
    
    theme(axis.text.x = element_text(size=8, angle=90)) +
    xlim(input$pre_range[1],input$post_range[2]) +
    ggtitle(title)
}

smooth_line_plot_function = function(plot_data,title,input){
  global_factor = 'Status'
  global_factor = input$global_factor
  ggplot(plot_data, aes_string(x = 'time', y = 'value', col = global_factor )) + 
    #geom_vline(xintercept = 0) +
    #geom_line(aes(group = 'MRN')) + 
    geom_smooth() +
    #geom_point() +
    #stat_summary(data = plot_data, fun.y=mean,geom="line",lwd=3,aes_string(x = 'time', y = 'value',group=input$global_factor,col = input$global_factor)) +
    
    theme(axis.text.x = element_text(size=8, angle=90)) +
    xlim(input$pre_range[1],input$post_range[2]) +
    ggtitle(title)
}


mean_line_plot_function = function(plot_data,title,input){
  ggplot(plot_data, aes(x = time, y = value,group = MRN)) + 
    geom_vline(xintercept = 0) +
    #geom_line(aes_string(col = input$global_factor)) +
    #geom_point(aes_string(col = input$global_factor)) +
    stat_summary(data = plot_data, fun.y=mean,geom="line",lwd=3,aes_string(x = 'time', y = 'value',group=input$global_factor,col = input$global_factor)) +
    
    theme(axis.text.x = element_text(size=8, angle=90)) +
    xlim(input$pre_range[1],input$post_range[2]) +
    ggtitle(title)
}

D_line_plot_function = function(plot_data,title,input){
  ggplot(plot_data, aes(x = time, y = value,col = MRN)) +
    geom_line(aes(group = MRN)) +
    geom_vline(xintercept = 0) +
    theme(axis.text.x = element_text(size=8, angle=90)) +
    theme(legend.position="none") +
    xlim(input$pre_range[1],input$post_range[2]) +
    ggtitle(title)
}

boxplot_function = function(full_data,title,input){
  cols = factor(c(input$pre_range[1]:input$post_range[2]))
  plot_data = full_data[full_data$variable %in% cols,]
  scale_cols = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]
  ggplot(plot_data, aes(x = variable, y = value)) + 
    
    geom_boxplot(aes_string(col = input$global_factor)) +
    stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
    theme(axis.text.x = element_text(size=14, angle=90)) + 
    scale_x_discrete(breaks = scale_cols) +
    #geom_vline(aes(xintercept = which(levels(plot_data$variable) == '0'))) +
    
    ggtitle(title)
}

boxplot_4_cluster_function = function(full_data,title,global_factor,cols,input){
  plot_data = full_data[full_data$variable %in% cols,]
  scale_cols = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]
  ggplot(plot_data, aes(x = variable, y = value)) + 
    #vline(xinter)
    #geom_vline(xintercept = 0) +
    geom_vline(aes(xintercept = which(levels(plot_data$variable) == '0'))) +
    
    geom_boxplot(aes_string(col = global_factor)) +
    stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=global_factor,col = global_factor)) +
    theme(axis.text.x = element_text(size=14, angle=90)) + 
    scale_x_discrete(breaks = scale_cols) +
    ggtitle(title)
}

mean_bias_line_plot_function = function(full_data,title,global_factor,bias_col,cols,input){
  plot_data = full_data[full_data$variable %in% cols,]
  scale_cols = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]
  ggplot(plot_data, aes(x = variable, y = value)) + 
    
    #geom_boxplot(aes_string(col = global_factor)) +
    geom_line(aes(group=cluster,col = cluster))
    #stat_summary(fun.y=mean,geom="line",aes_string(group=global_factor,col = global_factor,size = bias_col)) +
    #theme(axis.text.x = element_text(size=14, angle=90)) + 
    #scale_x_discrete(breaks = scale_cols) +
    ggtitle(title)
}


boxplot_i_summary_function = function(full_data,title,input){
  cols = factor(c(input$pre_range[1]:input$post_range[2]))
  summary_data = full_data[full_data$variable %in% cols,]
  plot_data = summary_data[summary_data$variable %in% pFEV_numeric_colnames_f,]
  ggplot(NULL) +
    stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
    geom_vline(aes(xintercept = which(levels(summary_data$variable) %in% '0'))) +
    geom_boxplot(data = plot_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
    stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
    theme(axis.text.x = element_text(size=14, angle=90)) +
    scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
    ggtitle(title)
}


lm_function = function(function_data,factor,cols){
  df = data.frame(Factor = numeric(), Status = numeric(0))
  df_b = data.frame(term = numeric(0),df = numeric(0), sumsq = numeric(0),
                    meansq = numeric(0), statistic = numeric(0), p.value = numeric(0),
                    Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
  df = df_b
  full_data=function_data[function_data$variable %in% cols,]
  factor_levels = unique(full_data[,factor])
  for(entry in factor_levels){
    data = full_data[full_data[,factor] == entry,]
    y = as.numeric(data$value)
    x = as.numeric(as.character(data$variable))
    if(!all(is.na(y))){
      fit <- aov(y ~ x, data=data)
      df_n = tryCatch(tidy(anova(fit)), error = function(e) e = df_b)
      #print(df_n)
      if(dim(df_n)[1] > 0){
        df_n$Factor = factor
        df_n$Status = entry
        df_n$comparison = 'All'
        df = rbind(df,df_n)
      }
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
      df_n = tryCatch(tidy(anova(fit)), error = function(e) e = df_b)
      #print(df_n)
      if(dim(df_n)[1] > 0){
        df_n$Factor = factor
        df_n$Status = entry
        df_n$comparison = 'Pre Treatment'
        df = rbind(df,df_n)
      }
      #print(df)
      # a = anova(fit)
      # p = a$Pr[1]
      # df[paste(factor,entry),'ano_Pre'] = signif(p,3)
      # l = lm(y~x)
      # i = coef(l)['(Intercept)']
      # m = coef(l)['x']
      # df[paste(factor,entry),'Int_Pre'] = signif(i,3)
      # df[paste(factor,entry),'slope_Pre'] = signif(m,3)
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
      df_n = tryCatch(tidy(anova(fit)), error = function(e) e = df_b)
      #print(df_n)
      if(dim(df_n)[1] > 0){
        df_n$Factor = factor
        df_n$Status = entry
        df_n$comparison = 'Post Treatment'
        df = rbind(df,df_n)
      }
      # print(df)
      # a = anova(fit)
      # p = a$Pr[1]
      # df[paste(factor,entry),'ano_Post'] = signif(p,3)
      # l = lm(y~x)
      # i = coef(l)['(Intercept)']
      # m = coef(l)['x']
      # df[paste(factor,entry),'Int_Post'] = signif(i,3)
      # df[paste(factor,entry),'slope_Post'] = signif(m,3)
    }
  }
  #df = df[order(df$Status),]
  #df = df[,c('Factor','Status','ano_All','ano_Pre','ano_Post')]
  #col_rearrange_function(df,3)
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

slope_function_tidy = function(full_data,factor,input){
  #df = data.frame(Factor = numeric())
  df_b = data.frame(estimate = numeric(0),
                    statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                    conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                    alternative = numeric(0),
                    Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
  df = df_b
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
    df_n = tryCatch(tidy(t.test(as.numeric(data$slope_Pre),as.numeric(data$slope_Post),paired = paired)), error = function(e) e = df_b)
    if(dim(df_n)[1] > 0){
     #print(colnames(df_n))
      #s = 0
      #if(t_all$p.value < 0.05){
      #  s = 1
      #}
      df_n$Factor = factor
      df_n$Status = entry
      df_n$comparison = paste0("(",input$pre_range[1],":0) vs (0:",input$post_range[2],")")
      df = rbind(df,df_n[,colnames(df)])
      #df[paste(factor,entry),'mean_slope_All'] = signif(mean_all,3)
      #df[paste(factor,entry),'p value'] = signif(t_all$p.value,3)
      
      #df[paste(factor,entry),'mean_slope_Pre'] = signif(mean_pre,3)
      #df[paste(factor,entry),'mean_slope_Post'] = signif(mean_post,3)
      #df[paste(factor,entry),'conf_1'] = signif(t_all$conf.int[1],3)
      #df[paste(factor,entry),'conf_2'] = signif(t_all$conf.int[2],3)
      #df[paste(factor,entry),'pre_num'] = signif(pre_num,3)
      #df[paste(factor,entry),'post_num'] = signif(post_num,3)
      #df[paste(factor,entry),'pre_sd'] = signif(pre_sd,3)
      #df[paste(factor,entry),'post_sd'] = signif(post_sd,3)
      #df[paste(factor,entry),'Paired'] = paired
      
      #df[paste(factor,entry),'significant'] = s
      
      #df[paste(factor,entry),'function'] = 'slope_function'
      #df[paste(factor,entry),'pre_data'] = paste(data$slope_Post,collapse = ', ')
      #df[paste(factor,entry),'post_data'] = paste(data$slope_Post,collapse = ', ')
    }
    
  }
  
  
  return(df)
}


slope_fit_plot_function = function(data,cols,x_label,input){
  global_factor = input$global_factor
  plot_data = data[data$variable %in% cols,]
  ggplot(NULL, aes_string(x = 'variable', y = 'value', group = global_factor,col=global_factor)) + 
    
    stat_summary(data = plot_data, fun.data=mean_cl_normal) +
    geom_smooth(data = plot_data,  method = 'lm',se = F) + 
    labs(x = x_label)
  
}

slope_boxplot_data_function = function(data,df,global_factor){
  slope_cols = c("slope_Pre","slope_Post")
  #data$significant = factor(df$significant[match(data[,global_factor],df$Status)])
  data_l = melt(data, measure.vars = slope_cols)
  return(data_l)
}

slope_boxplot_function = function(data,global_factor){
  #sig_col = c("white", "blanchedalmond")
  #if(!(0 %in% data$significant)){
  #  sig_col = c("blanchedalmond")
  #}
  #sig_col = c('white','gray73')
  data$Status = data[,global_factor]
  ggplot(data)+
    geom_boxplot(aes_string(col = global_factor,y='value',x = 'variable')) +
    facet_grid(. ~ Status) + 
    labs(col = global_factor)
    
    
    #scale_fill_manual(values = sig_col)
  
}


pp_t_test_function = function(full_data,factor,t1,t2){
  #df = data.frame(Factor = numeric(0))
  df_b = data.frame(estimate = numeric(0),
                    statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                    conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                    alternative = numeric(0),
                    Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
  df = df_b
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
    df_n = tryCatch(tidy(t.test(pre_data,post_data,paired = paired)), error = function(e) e = df_b)
    if(dim(df_n)[1] > 0){
      df$Factor = Factor
      df$Status = entry
      df$comparison = 'test'
      df = rbind(df,df_n)
    }
  }
  #   t = t.test(pre_data,post_data,paired = paired)
  #   p = t$p.value
  #   s = 0
  #   if(p < 0.05){
  #     s = 1
  #   }
  # 
  #   
  #   df[paste(factor,entry),'Factor'] = factor
  #   df[paste(factor,entry),'Status'] = entry
  #   df[paste(factor,entry),'p value'] = signif(p,3)
  #   df[paste(factor,entry),'pre mean'] = signif(mean(pre_data,na.rm=T),3)
  #   df[paste(factor,entry),'post mean'] = signif(mean(post_data,na.rm=T),3)
  #   df[paste(factor,entry),'pre num'] = signif(pre_num,3)
  #   df[paste(factor,entry),'post num'] = signif(post_num,3)
  #   df[paste(factor,entry),'pre sd'] = signif(pre_sd,3)
  #   df[paste(factor,entry),'post sd'] = signif(post_sd,3)
  #   df[paste(factor,entry),'paired'] = paired
  #   
  #   s = 0
  #   if(p < 0.05){
  #     s = 1
  #   }
  #   df[paste(factor,entry),'significant'] = s
  #   
  #   df[paste(factor,entry),'function'] = 'pp_t_test_function'
  #   df[paste(factor,entry),'pre_data'] = paste(pre_data,collapse = ', ')
  #   df[paste(factor,entry),'post_data'] = paste(post_data,collapse = ', ')
  #   
  # }
  df = col_rearrange_function(df,3)
  df = df[order(df$Status),]
  return(df)
}

pp_t_test_range_function = function(full_data,factor,pre1,pre2,post1,post2,input){
  #df = data.frame(Factor = numeric(0))
  df_b = data.frame(estimate = numeric(0),
                    statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                    conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                    alternative = numeric(0),
                    Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
  df = df_b
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
    df_n = tryCatch(tidy(t.test(pre_data,post_data,paired = paired)), error = function(e) e = df_b)
    #print(df_n)
    if(dim(df_n)[1] > 0){
      df_n$Factor = factor
      df_n$Status = entry
      df_n$comparison = paste0('(',input$pre_range[1],':',input$pre_range[2],') vs (',input$post_range[1],':',input$post_range[2],')')
      df = rbind(df,df_n[,colnames(df)])
    }
  }
  #   t = t.test(pre_data,post_data,paired = paired)
  #   p = t$p.value
  #   s = 0
  #   if(p < 0.05){
  #     s = 1
  #   }
  # 
  #   
  #   df[paste(factor,entry),'Factor'] = factor
  #   df[paste(factor,entry),'Status'] = entry
  #   df[paste(factor,entry),'p value'] = signif(p,3)
  #   df[paste(factor,entry),'pre mean'] = signif(mean(pre_data,na.rm=T),3)
  #   df[paste(factor,entry),'post mean'] = signif(mean(post_data,na.rm=T),3)
  #   
  #   df[paste(factor,entry),'pre num'] = signif(pre_num,3)
  #   df[paste(factor,entry),'post num'] = signif(post_num,3)
  #   df[paste(factor,entry),'pre sd'] = signif(pre_sd,3)
  #   df[paste(factor,entry),'post sd'] = signif(post_sd,3)
  #   df[paste(factor,entry),'paired'] = paired
  #   
  #   s = 0
  #   if(p < 0.05){
  #     s = 1
  #   }
  #   df[paste(factor,entry),'significant'] = s
  #   
  #   df[paste(factor,entry),'function'] = 'pp_t_test_range_function'
  #   df[paste(factor,entry),'pre_data'] = paste(pre_data,collapse = ', ')
  #   df[paste(factor,entry),'post_data'] = paste(post_data,collapse = ', ')
  #   
  #   
  # }
  #df = df[order(df$Status),]
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
  # sig_col = c("white", "blanchedalmond")
  # if(!(0 %in% pp_data$significant)){
  #   sig_col = c("blanchedalmond")
  # }
  #pp_data$significant = factor(df$significant[match(pp_data[,global_factor],df$Status)])
  # p = ggplot(pp_data, aes_string(x = eval(global_factor),y = 'value',col='treat',fill='significant')) +
  #   geom_boxplot() + 
  #   scale_fill_manual(values = sig_col)
  
  return(pp_data)
}

t_boxplot_function = function(pp_data,global_factor){
  #sig_col = c("white", "blanchedalmond")
  #if(!(0 %in% pp_data$significant)){
  #  sig_col = c("blanchedalmond")
  #}
  #pp_data$significant = factor(df$significant[match(pp_data[,global_factor],df$Status)])
  #print(colnames(pp_data))
  #print(global_factor)

  pp_data$Status = pp_data[,global_factor]
  p = ggplot(pp_data, aes_string(x = 'treat',y = 'value',col=global_factor)) +
    facet_grid(. ~ Status) + 
    labs(col = global_factor) +
    geom_boxplot() 
    #scale_fill_manual(values = sig_col)
  
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
  df_m$Status = factor(df_m$Status)
  df_m$Factor = factor(df_m$Factor)
  #View(df_m)
  p =  ggplot(df_m, aes(x = Status,y=value,col=variable)) +
    geom_hline(yintercept=0)+
    geom_boxplot()
  return(p)
}

pp_t_test_zero_function = function(full_data,factor,t1,t2){
  df = data.frame(Factor = numeric(0))
  df_b = data.frame(estimate = numeric(0),
                    statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                    conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                    alternative = numeric(0),
                    Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
  df = df_b
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
    df_pre = tryCatch(tidy(t.test(na.omit(pre_data),na.omit(zero),paired = pre_paired)), error = function(e) e = df_b)
   #print(df_pre)
    if(dim(df_pre)[1] > 0){
      df_pre$Factor = factor
      df_pre$Status = entry
      df_pre$comparison = paste0('Pre treatment (',t2,' vs 0)')
      df = rbind(df,df_pre[,colnames(df)])
    }
    df_post = tryCatch(tidy(t.test(na.omit(post_data),na.omit(zero),paired = post_paired)), error = function(e) e = df_b)
   #print(df_post)
    if(dim(df_post)[1] > 0){
      df_post$Factor = factor
      df_post$Status = entry
      df_post$comparison = paste0('Post treatment (0 vs',t2,')')
      df = rbind(df,df_post[,colnames(df)])
    }
    
    # pre_t = t.test(na.omit(pre_data),na.omit(zero),paired = pre_paired)
    # post_t = t.test(na.omit(post_data),na.omit(zero),paired = post_paired)
    # p_pre = pre_t$p.value
    # p_post = post_t$p.value
    # 
    # 
    # df[paste(factor,entry),'Factor'] = factor
    # df[paste(factor,entry),'Status'] = entry
    # df[paste(factor,entry),'pre p value'] = signif(p_pre,3)
    # df[paste(factor,entry),'post p value'] = signif(p_post,3)
    # 
    # df[paste(factor,entry),'pre mean'] = signif(mean(pre_data,na.rm=T),3)
    # df[paste(factor,entry),'zero mean'] = signif(mean(zero,na.rm=T),3)
    # df[paste(factor,entry),'post mean'] = signif(mean(post_data,na.rm=T),3)
    # 
    # df[paste(factor,entry),'pre num'] = signif(pre_num,3)
    # df[paste(factor,entry),'zero num'] = signif(zero_num,3)
    # df[paste(factor,entry),'post num'] = signif(post_num,3)
    # 
    # df[paste(factor,entry),'pre sd'] = signif(pre_sd,3)
    # df[paste(factor,entry),'zero sd'] = signif(zero_sd,3)
    # df[paste(factor,entry),'post sd'] = signif(post_sd,3)
    # 
    # df[paste(factor,entry),'pre paired'] = pre_paired
    # df[paste(factor,entry),'post paired'] = post_paired
    # pre_s = 0
    # if(p_pre < 0.05){
    #   pre_s = 1
    # }
    # post_s = 0
    # if(p_post < 0.05){
    #   post_s = 1
    # }
    # df[paste(factor,entry),'pre_significant'] = pre_s
    # df[paste(factor,entry),'post_significant'] = post_s
    # 
    # 
    # 
    # df[paste(factor,entry),'function'] = 'pp_t_test_zero_function'
    # df[paste(factor,entry),'pre_data'] = paste(pre_data,collapse = ', ')
    # df[paste(factor,entry),'zero_data'] = paste(zero,collapse = ', ')
    # df[paste(factor,entry),'post_data'] = paste(post_data,collapse = ', ')
    # 
    # 

  }
  #df = col_rearrange_function(df,3)
  #df = df[order(df$Status),]
 #View(df)
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


boxplot_pp_zero_data_function = function(full_data,factor,t1,t2){
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
  # sig_list = c()
  # for(i in c(1:dim(df_m)[1])){
  #   row_entry = df_m[i,]
  #   #print(row_entry)
  #   if(row_entry$variable == 'pre'){
  #     sig = df_s$pre_significant[df_s$Status == row_entry$Status]
  #   }
  #   if(row_entry$variable == 'post'){
  #     sig = df_s$post_significant[df_s$Status == row_entry$Status]
  #   }
  #   if(row_entry$variable == 'zero'){
  #     sig = '0'
  #   }
  #   sig_list = c(sig_list,sig)
  # }
  # #print(sig_list)
  # df_m$significant = factor(sig_list)
  #df_m$significant[df$variable == 'pre'] = factor(df_s$pre_significant[match(df_m$Status[df$variable == 'pre'],df_s$Status)])
  
  return(df_m)
}

boxplot_pp_zero_plot_function = function(df_m){
  #View(df_m)
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  #sig_col = c("white", "blanchedalmond")
  #if(!(0 %in% df_m$significant)){
  #  sig_col = c("blanchedalmond")
  #}

  p = ggplot(df_m, aes(x = variable,y=value,col=Status)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    facet_grid(. ~ Status)
    #scale_x_discrete(limits = u)
    #scale_fill_manual(values = sig_col)
  
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
    #paired = FALSE
    
    
    p = tryCatch(t.test(na.omit(pre_ratio),na.omit(post_ratio),alternative = c("two.sided"),paired = paired)$p.value, error = function(e) e = 2)
    #p = t$p.value
    
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
boxplot_pp_ratio_plot_function = function(df_m,global_factor,title = 'T test of log2(ratio)'){ 
  u = as.numeric(as.character(unique(df_m$Status)))
  u = factor(u[(order(u))])
  #sig_col = c("white", "blanchedalmond")
  #if(!(0 %in% df_m$significant)){
  #  sig_col = c("blanchedalmond")
  #}
  df_m = df_m[!is.na(df_m$value),]
  #View(df_m)
  sig_col = c("white", "blanchedalmond")
  if(!(0 %in% df_m$significant)){
    sig_col = c("blanchedalmond")
  }
  df_m$Status = factor(df_m$Status, levels = u)
  #View(df_m)
  p = ggplot(df_m, aes(x = Status,y=value,col=variable, fill = significant)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    scale_x_discrete(limits = u) + 
    scale_fill_manual(values = sig_col)
  # scale_fill_manual(values = sig_col)
  #sig_col = c('white','azure1')
  p = ggplot(df_m, aes(x = variable,y=value,col=Status, fill = significant)) +
    geom_hline(yintercept=0)+
    geom_boxplot() +
    labs(col = global_factor) + 
    #scale_x_discrete(limits = u) + 
    scale_fill_manual(values = sig_col) + 
    facet_grid(. ~ Status)+
    ggtitle(title)
    #geom_signif(comparisons = c('pre','post'),textsize = 1)
  # scale_fill_manual(values = sig_col)
  
  
  return(p)
}



full_t_test_function = function(){
  for(i in cols){
   #print(paste(factor(-i)))
   #print(full_data[,factor(-i)])
    raw_data[,'0'] = full_data[,'0']
    raw_data[,paste(factor(-i))] = full_data[,paste(factor(-i))]
    
    raw_data[,paste(factor(i))] = full_data[,paste(factor(i))]
    
    col1 = paste0(prefix,i)
    col2 = paste0(prefix,-i)
    raw_data[,col1] = full_data[,col1]
    raw_data[,col2] = full_data[,col1]
    #View(raw_data)
    output$percentage_df = renderDataTable(raw_data)
    
    factor_levels = unique(full_data[,global_factor])
    for(entry in factor_levels){
      pre_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$variable %in% col1]
      post_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$variable %in% col2]
      df1 = data.frame(pre = pre_data,post = post_data)
      df1$Factor = global_factor
      df1$Status = entry
      df1$time = i
      plot_df = rbind(plot_df,df1)
      
      #print(pre_data)
      #print(post_data)
      df_n = tryCatch(tidy(t.test(pre_data,post_data)), error = function(e) e = df_b)
      if(dim(df_n)[1] > 0){
        df_n$Factor = global_factor
        df_n$Status = entry
        df_n$comparison = paste(-i,'vs',i)
        df = rbind(df,df_n)
      }
      
    }
  }
  df = col_rearrange_function(df,3)
  
}


###### CLUSTERING #########
# clustering_function(data,retained_patients(),input$clutree_num,
#                     input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
#                     input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)



clustering_function = function(full_data,r_list,d_num,
                               fac_w_1,fac_col_list_1,fac_w_2,fac_col_list_2,
                              num_w_1,num_col_list_1,num_w_2,num_col_list_2){
  #View(full_data)
  
  testing = 'F'
  if(testing == T){
    colnames(full_data)
    rownames(full_data)
    data = i_pFEV_wf[r_list,c(30:50)]
    data = full_data[,paste(unlist(factor(c(-6:6))))]
    colnames(data)
    rownames(data)
    #View(data)
    data = data[complete.cases(data),]
    weights = rep(10,dim(data)[2])
    weights
  }
  #d_num = 3
  clust_fac_col_list = c(fac_col_list_1,fac_col_list_2)
  clust_num_col_list = c(num_col_list_1,num_col_list_2)
  clust_num_col_list = clust_num_col_list[order(as.numeric(clust_num_col_list))]
  clust_col_list = c(clust_num_col_list,clust_fac_col_list)
  clust_col_list = clust_col_list[clust_col_list %in% colnames(full_data)]
 #print(colnames(full_data))
 #print(clust_col_list)
  
  
  #t_data = full_data[r_list,clust_col_list]
  #View(t_data)
  r_data = full_data[,clust_col_list]
  #View(r_data)
  data = r_data
  #View(data)
  o_data = r_data
  
  
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
  for(factor_name in factor_list){
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
        df_c[i,paste(factor_name,j,sep='_')] = as.numeric(per)
      }
    }
  }
  df_tc = as.data.frame(t(df_c))
  data = df_tc[,c('Factor','Status',num_clusters)] # colnames used in chisq function, do not change
  data = data[c(2:dim(data)[1]),]
  col_range = c(3:dim(data)[2])
  data[,col_range] = apply(data[,col_range], 2, function (x) as.numeric(as.character(x)))
  data$sum = apply(data[,col_range], 1, function(x) round(sum(x),0)) 
  data$p.value = apply(data[,col_range], 1, function(x) signif(chisq.test(x)$p.value,3)) 
  data = data[order(data$Factor,data$Status),]
  return(data)
}

clust_comparison_within = function(df,clust_col,input){
  num_clusters = unique(df[,clust_col])
  num_clusters = num_clusters[(order(num_clusters))]
  print(num_clusters)
  num_clusters = input$cluster_select_clusters
  print(num_clusters)
  #print(num_clusters_s)
  
  
  #print(num_clusters)
  df_c = data.frame(cluster = num_clusters)
  rownames(df_c) = df_c$num_clusters
  #print(dim(df))
  for(i in num_clusters){
    df_clust = df[df[,clust_col] == i,]
    
    #print(i)
    #print(dim(df_clust))
    total = dim(df_clust)[1]
    for(factor_name in factor_list[order(factor_list)]){
      status = paste(unique(df_clust[,factor_name]))
      #print(factor_name)
      #print(status)
      #status[is.na(as.numeric(status))] = -2
      status = status[order(status)]
      #print(status)
      #print(df_clust[,factor_name])
      for(j in status){
        #print(j)
        #print(df_clust[,factor_name][df_clust[,factor_name] == j])
        if(is.na(as.numeric(j))){
          num = length(df_clust[,factor_name][is.na(df_clust[,factor_name])])
        }else{
          num = length(df_clust[,factor_name][df_clust[,factor_name] == j & !is.na(df_clust[,factor_name])])
        }
        #print(num)
        #print(total)
        per = round((num/total)*100,2)
        df_c["Factor",paste(factor_name,j,sep='_')] = factor_name
        df_c["Status",paste(factor_name,j,sep='_')] = j
        df_c["Data",paste(factor_name,j,sep='_')] = clust_col
        df_c[i,paste(factor_name,j,sep='_')] = per
      }
    }
  }
  
  df_tc = as.data.frame(t(df_c))
  print(colnames(df_tc))
  data = df_tc[,c('Factor','Status',num_clusters)] # DBC --- colnames used in chisq functions
  data = data[c(2:dim(data)[1]),]
  data[,c(3:dim(data)[2])] = apply(data[,c(3:dim(data)[2])], 2, function (x) as.numeric(as.character(x)))
  data = data[order(data$Factor,data$Status),]
  
  return(data)
}


chisq_total = function(full_data,input){
    cluster_list = c(3:(2+input$clutree_num)) #set by the table in 
    full_test_data = full_data[,cluster_list]
    full_test_data[is.na(full_test_data)] = 0
    chisq_result = chisq.test(full_test_data)
    chi_df = tidy(chisq_result)
    chi_df$Factor = 'All'
    
    for(selected_factor in unique(full_data$Factor)){
      sub_data = full_data[full_data$Factor == selected_factor,]
      test_data = sub_data[,cluster_list]
      
      #print(test_data)
      
      chisq_result = tidy(chisq.test(test_data))
      chisq_result$Factor = selected_factor
      
      chi_df = rbind(chi_df,chisq_result)
    }
    chi_df$Cluster = paste(colnames(test_data),collapse=', ')
    chi_df = chi_df[,c('Factor','Cluster',colnames(chi_df[c(1:(length(colnames(chi_df))-2))]))]
    chi_df$p.value = signif(chi_df$p.value,3)
    chi_df$statistic = signif(chi_df$statistic,3)
    
    chi_df
    return(chi_df)
}


chisq_within = function(data,input){
  cluster_list = input$cluster_select_clusters
  factor_list = unique(data$Factor)

  
  full_test_data = data[,cluster_list]
  full_test_data[is.na(full_test_data)] = 0
  full_test_data = t(full_test_data)
  
  chisq_result = chisq.test(full_test_data)
  chi_df = tidy(chisq_result)
  chi_df$Cluster = 'All'
  chi_df$Factor = 'All'
 #print(chi_df)
  
  for(selected_factor in factor_list){
    sub_data = data[data$Factor == selected_factor,]
    test_data = t(sub_data[,cluster_list])
    test_data[is.na(test_data)] = 0
    chisq_result = tidy(chisq.test(test_data))
    chisq_result$Cluster = paste(cluster_list,collapse = ', ')
    chisq_result$Factor = selected_factor
    
    chi_df = rbind(chi_df,chisq_result)
  }
  
  chi_df = chi_df[,c('Cluster','Factor',colnames(chi_df[c(1:(length(colnames(chi_df))-2))]))]
  chi_df$p.value = signif(chi_df$p.value,3)
  chi_df$statistic = signif(chi_df$statistic,3)
  
  chi_df
  return(chi_df)
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
              hjust = 1,angle = 90, size = 5, show.legend = F) +
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



BOS_calc_function = function(BOS,v,cols){
  #print(cols)
  #print(v)
  vmin = tryCatch(min(which(v < BOS)), error = function(e) Inf)
  vmax = tryCatch(max(which(v > BOS)), error = function(e) Inf)
  #print(colnames(v))
  print(paste(vmin,vmax))
  print(length(v))
  if(vmin != length(v)){
    if(is.finite(vmax)){
      if(is.finite(vmin) ){
       
          if(v[vmin + 1] < BOS){
          #if(vmin > vmax){
            #return(vmin + 1)
            return(as.numeric(cols[vmin +1]))
          }else{
            new_start = vmax
            new_start = vmin + 1
            cols = cols[c(new_start:length(v))]
            v = v[c(new_start:length(v))]
            BOS_calc_function(BOS,v,cols)
          }
        
      }else{
        return(NA)
      }
    }else{
      if(is.finite(vmin)){
        return(as.numeric(cols[vmin +1]))
      }else{
        return(NA)
      }
    }
  }else{
    return(NA)
  }
}

BOS_RAS_matrix_calc_function = function(BOS, v, y){
  RAS = 0.7
  row = rownames(v)
  row

  cols = colnames(v)
  cols

  vmin = tryCatch(min(which(v < BOS & y < RAS)), error = function(e) Inf)
  vmin
  vmax = tryCatch(max(which(v > BOS & y < RAS)), error = function(e) Inf)
  vmax

  if(vmin != length(v)){
    if(is.finite(vmax)){
      if(is.finite(vmin) ){
        
        if(v[vmin + 1] < BOS & y[vmin + 1] < RAS){
          #if(vmin > vmax){
          #return(vmin + 1)
          return(as.numeric(cols[vmin +1]))
        }else{
          new_start = vmax
          new_start = vmin + 1
          cols = cols[c(new_start:length(v))]
          cols
          v = v[c(new_start:length(v))]
          v
          y = y[c(new_start:length(y))]
          a = BOS_RAS_matrix_calc_function(BOS,v,y)
          return(a)
        }
        
      }else{
        return(NA)
      }
    }else{
      if(is.finite(vmin)){
        return(as.numeric(cols[vmin +1]))
      }else{
        return(NA)
      }
    }
  }else{
    return(NA)
  }
}

RAS_matrix_calc_function = function(BOS, v, y){
  RAS = 0.7
  row = rownames(v)
  row
  
  cols = colnames(v)
  cols
  
  vmin = tryCatch(min(which(y > RAS & v < 0.8)), error = function(e) Inf)
  vmin
  vmax = tryCatch(max(which(y > RAS & v < 0.8)), error = function(e) Inf)
  vmax
  
  if(vmin != length(y)){
    if(is.finite(vmax)){
      if(is.finite(vmin)){
        
        if(y[vmin + 1] > RAS & v[vmin + 1] < 0.8){
          #if(vmin > vmax){
          #return(vmin + 1)
          return(as.numeric(cols[vmin + 1]))
        }else{
          new_start = vmax
          new_start = vmin + 1
          cols = cols[c(new_start:length(y))]
          cols
          v = v[c(new_start:length(y))]
          v
          y = y[c(new_start:length(y))]
          a = RAS_matrix_calc_function(BOS,v,y)
          return(a)
        }
        
      Ths }else{
        return(NA)
      }
    }else{
      if(is.finite(vmin)){
        return(as.numeric(cols[vmin]))
      }else{
        return(NA)
      }
    }
  }else{
    return(NA)
  }
}


BOS_calc_matrix_function = function(BOS,v){
  #print(v)
  cols = colnames(v)
  #print(cols)
  #print(v)
  vmin = tryCatch(min(which(v < BOS)), error = function(e) Inf)
  vmax = tryCatch(max(which(v > BOS)), error = function(e) Inf)
  #print(colnames(v))
  #print(paste(vmin,vmax))
  #print(length(v))
  if(vmin != length(v)){
    if(is.finite(vmax)){
      if(is.finite(vmin) ){
        
        if(v[vmin + 1] < BOS){
          #if(vmin > vmax){
          #return(vmin + 1)
          return(as.numeric(cols[vmin +1]))
        }else{
          new_start = vmax
          new_start = vmin + 1
          cols = cols[c(new_start:length(v))]
          v = v[c(new_start:length(v))]
          a = BOS_calc_matrix_function(BOS,v)
          return(a)
        }
        
      }else{
        return(NA)
      }
    }else{
      if(is.finite(vmin)){
        return(as.numeric(cols[vmin +1]))
      }else{
        return(NA)
      }
    }
  }else{
    return(NA)
  }
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


BOS_test_function = function(time,rx_date,bos_date,death_date,status){
  #bos_status = 0
  #print(rx_date)
  bos_date = as.numeric(bos_date)
  if(time >= bos_date){
    bos_status = 3
  }else{
    if(status == '2'){
      if(death_date > time & !is.na(death_date)){
        bos_status = 1
      }else{
        bos_status =  2
      }
    }
    if(status == '1'){
      
      if(rx_date > time & !is.na(rx_date)){
        bos_status = 1
      }else{
        bos_status = 0
      }
    }
  }
  return(bos_status)
}

#time = t
#rx_date = as.numeric(x[rx_i])
#bos_date = as.numeric(x[BOS3_date_i])
#death_date = as.numeric(x[d_date_i])
#status = as.numeric(x[status_i]))))


BOS_surv_test_function = function(time,rx_date,bos_date,death_date,status){
  #print(time)
  #print(rx_date)
  bos_date = min(bos_date,rx_date)
  bos_status = 0
  if(rx_date < time){
    bos_status = 0
  }else{
    bos_status = BOS_test_function(time,rx_date,bos_date,death_date,status)
  }
  #bos_status
  return(bos_status)
}




BOS_patient_function = function(full_data){
  #full_data = pFEV_wf
  
  BOS1 = 0.8
  BOS2 = 0.66
  BOS3 = 0.5
  end_time = 50
  MRN = full_data$MRN
  #print(str(MRN))
  d_date = full_data$MonthsToDeath
  #print(str(d_date))
  rx = full_data$MonthSinceRx
  #print(str(rx))
  status = full_data$Status
  #print(str(status))
   BOS1_date = full_data$BOS1mnth
   BOS2_date = full_data$BOS2mnth
   BOS3_date = full_data$BOS3mnth
   BOS3_surv_date = full_data$`BOS 3 free survival`

  BOS1_date[is.na(BOS1_date)] = end_time
  BOS2_date[is.na(BOS2_date)] = end_time
  BOS3_date[is.na(BOS3_date)] = end_time
  BOS3_surv_date[is.na(BOS3_surv_date)] = end_time

  full_data$BOS1_date = BOS1_date
  full_data$BOS2_date = BOS2_date
  full_data$BOS3_date = BOS3_date
  full_data$BOS3_surv_date = BOS3_surv_date
  full_data$MonthsToDeath[is.na(full_data$MonthsToDeath)] = end_time
  full_data$MonthSinceRx[is.na(full_data$MonthSinceRx)] = end_time
  
  time = seq(-24,48,1)
  #str(time)
  bos_df = data.frame(time = time)
  rx_i = grep('MonthSinceRx', colnames(full_data))
  BOS1_date_i = grep('BOS1_date', colnames(full_data))
  BOS2_date_i = grep('BOS2_date', colnames(full_data))
  BOS3_date_i = grep('BOS3_date', colnames(full_data))
  BOS3_surv_date_i = grep('BOS3_surv_date', colnames(full_data))
  d_date_i = grep('MonthsToDeath', colnames(full_data))
  status_i = grep('Status', colnames(full_data))
  patient_status_df = data.frame(time = numeric(0))
  #print(rx_i)
  for(t in time){
    #print(t)
    #row_data = full_data[1,]
    #print(row_data)
    #print(row_data[rx_i]) 
    status_1 = 0
    status_2 = 0
    status_3 = 0
    status_4 = 0
    patient_status_df[paste(t,MRN,sep='_'),'time'] = t
    patient_status_df[paste(t,MRN,sep='_'),'MRN'] = MRN
    status_1 = unlist(apply(full_data,1, function(x) BOS_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS1_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    status_2 = unlist(apply(full_data,1, function(x) BOS_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS2_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    status_3 = unlist(apply(full_data,1, function(x) BOS_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS3_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    status_4 = unlist(apply(full_data,1, function(x) BOS_surv_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS3_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    patient_status_df[paste(t,MRN,sep='_'),'BOS1_free'] = status_1
    patient_status_df[paste(t,MRN,sep='_'),'BOS2_free'] = status_2
    patient_status_df[paste(t,MRN,sep='_'),'BOS3_free'] = status_3
    patient_status_df[paste(t,MRN,sep='_'),'BOS3_surv_free'] = status_4
  }
  hit = 0
  no_hit = 1
  patient_status_df$BOS1[patient_status_df$BOS1_free == 3] = hit
  patient_status_df$BOS1[!patient_status_df$BOS1_free == 3] = no_hit
  patient_status_df$BOS2[patient_status_df$BOS2_free == 3] = hit
  patient_status_df$BOS2[!patient_status_df$BOS2_free == 3] = no_hit
  patient_status_df$BOS3[patient_status_df$BOS3_free == 3] = hit
  patient_status_df$BOS3[!patient_status_df$BOS3_free == 3] = no_hit
  return(patient_status_df)
}


BOS_patient_function_post_calc = function(full_data){
  #full_data = pFEV_wf
  
  BOS1 = 0.8
  BOS2 = 0.66
  BOS3 = 0.5
  end_time = 50
  MRN = full_data$MRN
  #print(str(MRN))
  d_date = full_data$MonthsToDeath
  #print(str(d_date))
  rx = full_data$MonthSinceRx
  #print(str(rx))
  status = full_data$Status
  #print(str(status))
  BOS1_date = full_data$BOS1
  BOS2_date = full_data$BOS2
  BOS3_date = full_data$BOS3
  BOS3_surv_date = full_data$BOS3
  
  BOS1_date[is.na(BOS1_date)] = end_time
  BOS2_date[is.na(BOS2_date)] = end_time
  BOS3_date[is.na(BOS3_date)] = end_time
  BOS3_surv_date[is.na(BOS3_surv_date)] = end_time
  
  full_data$BOS1_date = BOS1_date
  full_data$BOS2_date = BOS2_date
  full_data$BOS3_date = BOS3_date
  full_data$BOS3_surv_date = BOS3_surv_date
  full_data$MonthsToDeath[is.na(full_data$MonthsToDeath)] = end_time
  full_data$MonthSinceRx[is.na(full_data$MonthSinceRx)] = end_time
  
  time = seq(-24,48,1)
  #str(time)
  bos_df = data.frame(time = time)
  rx_i = grep('MonthSinceRx', colnames(full_data))
  BOS1_date_i = grep('BOS1_date', colnames(full_data))
  BOS2_date_i = grep('BOS2_date', colnames(full_data))
  BOS3_date_i = grep('BOS3_date', colnames(full_data))
  BOS3_surv_date_i = grep('BOS3_surv_date', colnames(full_data))
  d_date_i = grep('MonthsToDeath', colnames(full_data))
  status_i = grep('Status', colnames(full_data))
  patient_status_df = data.frame(time = numeric(0))
  #print(rx_i)
  for(t in time){
    #print(t)
    #row_data = full_data[1,]
    #print(row_data)
    #print(row_data[rx_i]) 
    status_1 = 0
    status_2 = 0
    status_3 = 0
    status_4 = 0
    patient_status_df[paste(t,MRN,sep='_'),'time'] = t
    patient_status_df[paste(t,MRN,sep='_'),'MRN'] = MRN
    status_1 = unlist(apply(full_data,1, function(x) BOS_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS1_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    status_2 = unlist(apply(full_data,1, function(x) BOS_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS2_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    status_3 = unlist(apply(full_data,1, function(x) BOS_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS3_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    status_4 = unlist(apply(full_data,1, function(x) BOS_surv_test_function(t,as.numeric(x[rx_i]),as.numeric(x[BOS3_date_i]),as.numeric(x[d_date_i]),as.numeric(x[status_i]))))
    patient_status_df[paste(t,MRN,sep='_'),'BOS1_free'] = status_1
    patient_status_df[paste(t,MRN,sep='_'),'BOS2_free'] = status_2
    patient_status_df[paste(t,MRN,sep='_'),'BOS3_free'] = status_3
    patient_status_df[paste(t,MRN,sep='_'),'BOS3_surv_free'] = status_4
  }
  hit = 0
  no_hit = 1
  patient_status_df$BOS1[patient_status_df$BOS1_free == 3] = hit
  patient_status_df$BOS1[!patient_status_df$BOS1_free == 3] = no_hit
  patient_status_df$BOS2[patient_status_df$BOS2_free == 3] = hit
  patient_status_df$BOS2[!patient_status_df$BOS2_free == 3] = no_hit
  patient_status_df$BOS3[patient_status_df$BOS3_free == 3] = hit
  patient_status_df$BOS3[!patient_status_df$BOS3_free == 3] = no_hit
  return(patient_status_df)
}

  
  #head(patient_status_df,50)  
    

  
  #colnames(patient_status_df)
  #head(patient_status_df,56)
  #patient_status_df[patient_status_df$MRN == 5931655,]
  #tail(patient_status_df)


BOS_function_post_calc = function(full_data){
  #full_data = pFEV_wf
  
  BOS1 = 0.8
  BOS2 = 0.66
  BOS3 = 0.5
  end_time = 50
  MRN = full_data$MRN
  d_date = full_data$MonthsToDeath
  rx = full_data$MonthSinceRx
  status = full_data$Status
  BOS1_date = full_data$BOS1
  BOS2_date = full_data$BOS2
  BOS3_date = full_data$BOS3
  Survival = full_data$MonthsToDeath
  
  BOS1_date[is.na(BOS1_date)] = end_time
  BOS2_date[is.na(BOS2_date)] = end_time
  BOS3_date[is.na(BOS3_date)] = end_time
  Survival[is.na(Survival)] = end_time
  
  full_data$BOS1_date = BOS1_date
  full_data$BOS2_date = BOS2_date
  full_data$BOS3_date = BOS3_date
  full_data$Survival = Survival
  
  
  time = seq(-24,48,1)
  bos_df = data.frame(time = time)
  
  bos_df$BOS1_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[x >= BOS1_date]))))
  bos_df$BOS2_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS2_date[x >= BOS2_date]))))
  bos_df$BOS3_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_date[x >= BOS3_date]))))
  bos_df$BOS3_surv_num = unlist(lapply(bos_df$time, function(x) length(na.omit(Survival[x >= Survival]))))
  
  bos_df$BOS1_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS1_date]))))
  bos_df$BOS2_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS2_date]))))
  bos_df$BOS3_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS3_date]))))
  bos_df$BOS3_surv_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < Survival]))))
  
  
  bos_df$BOS1_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS1_date]))))
  bos_df$BOS2_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS2_date]))))
  bos_df$BOS3_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS3_date]))))
  bos_df$BOS3_surv_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < Survival]))))
  
  
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
  bos_df$Survival = b*100
  
  return(bos_df)
}



bos_df_function = function(full_data){
  df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),RAS = numeric(0),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
  data_name_list = list(RAS = 'RAS', RAS_recovery = 'RAS_recovery',BOS1_RAS = 'BOS1',BOS2_RAS = 'BOS2',BOS3_RAS = 'BOS3',MonthsToDeath = 'Survival')
  for(data_name in names(data_name_list)){
    print(data_name)
    print(paste(data_name_list[data_name]))
    
    df_n = BOS_function_single(full_data,data_name,data_name_list[data_name])
    if(dim(df)[1] == 0){
      df = df_n
    }else{
      df = cbind(df,df_n[-1])
    }
  }
  return(df)
}

BOS_function_single = function(full_data,data_name, replacement_name,start_time = -24, end_time = 48, end_study = 50){

  test = F
  if(test == T){
    full_data = readRDS('temp/full_data.rds')
    start_time = -24
    end_time = 48
    end_study = 50
    data_name = 'RAS'
  }
  #end_time = 50
  MRN = full_data$MRN
  d_date = full_data$MonthsToDeath
  rx = full_data$MonthSinceRx
  status = full_data$Status
  
  data = full_data[,data_name]
  #RAS_date = full_data$RAS
  #BOS1_date = full_data$BOS1_RAS
  #BOS2_date = full_data$BOS2_RAS
  #BOS3_date = full_data$BOS3_RAS
  #Survival = full_data$MonthsToDeath
  
  
  data[is.na(data)] = end_time
  #BOS1_date[is.na(BOS1_date)] = end_time
  #BOS2_date[is.na(BOS2_date)] = end_time
  #BOS3_date[is.na(BOS3_date)] = end_time
  #Survival[is.na(Survival)] = end_time
  
  #full_data$RAS_date = RAS_date
  #full_data$BOS1_date = BOS1_date
  #full_data$BOS2_date = BOS2_date
  #full_data$BOS3_date = BOS3_date
  #full_data$Survival = Survival
  
  
  time = seq(start_time,end_time,1)
  bos_df = data.frame(time = time)
  
  bos_df$data_num = unlist(lapply(bos_df$time, function(x) length(na.omit(data[x >= data]))))
  #bos_df$BOS1_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[x >= BOS1_date]))))
  #bos_df$BOS2_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS2_date[x >= BOS2_date]))))
  #bos_df$BOS3_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_date[x >= BOS3_date]))))
  #os_df$BOS3_surv_num = unlist(lapply(bos_df$time, function(x) length(na.omit(Survival[x >= Survival]))))
  
  bos_df$data_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < data]))))
  #bos_df$BOS1_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS1_date]))))
  #bos_df$BOS2_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS2_date]))))
  #bos_df$BOS3_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS3_date]))))
  #bos_df$BOS3_surv_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < Survival]))))
  
  bos_df$data_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < data]))))
  #bos_df$BOS1_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS1_date]))))
  #bos_df$BOS2_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS2_date]))))
  #bos_df$BOS3_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS3_date]))))
  #bos_df$BOS3_surv_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < Survival]))))
  
  
  bos_df = mutate(bos_df,
                  data_num_diff = c(0,diff(data_num))
                  #BOS1_num_diff = c(0,diff(BOS1_num)),
                  #BOS2_num_diff = c(0,diff(BOS2_num)),
                  #BOS3_num_diff = c(0,diff(BOS3_num)),
                  #BOS3_surv_num_diff = c(0,diff(BOS3_surv_num))
  )
  bos_df = mutate(bos_df, 
                  data_Dead_diff = c(0,diff(data_Dead))
                  #BOS1_Dead_diff = c(0,diff(BOS1_Dead)),
                  #BOS2_Dead_diff = c(0,diff(BOS2_Dead)),
                  #BOS3_Dead_diff = c(0,diff(BOS3_Dead)),
                  #BOS3_surv_Dead_diff = c(0,diff(BOS3_surv_Dead))
  )
  bos_df = mutate(bos_df, 
                  data_Censor_diff = c(0,diff(data_Censor))
                  #BOS1_Censor_diff = c(0,diff(BOS1_Censor)),
                  #BOS2_Censor_diff = c(0,diff(BOS2_Censor)),
                  #BOS3_Censor_diff = c(0,diff(BOS3_Censor)),
                  #BOS3_surv_Censor_diff = c(0,diff(BOS3_surv_Censor))
  )
  
  bos_df = mutate(bos_df, 
                  data_risk = dim(full_data)[1] - c(0,data_num[-length(data_num)]) - c(0,data_Dead[-length(data_Dead)]) - c(0,data_Censor[-length(data_Censor)])
                  #BOS1_risk = dim(full_data)[1] - c(0,BOS1_num[-length(BOS1_num)]) - c(0,BOS1_Dead[-length(BOS1_Dead)]) - c(0,BOS1_Censor[-length(BOS1_Censor)]),
                  #BOS2_risk = dim(full_data)[1] - c(0,BOS2_num[-length(BOS2_num)]) - c(0,BOS2_Dead[-length(BOS2_Dead)]) - c(0,BOS2_Censor[-length(BOS2_Censor)]),
                  #BOS3_risk = dim(full_data)[1] - c(0,BOS3_num[-length(BOS3_num)]) - c(0,BOS3_Dead[-length(BOS3_Dead)]) - c(0,BOS3_Censor[-length(BOS3_Censor)]),
                  #BOS3_surv_risk = dim(full_data)[1] - c(0,BOS3_surv_num[-length(BOS3_surv_num)]) - c(0,BOS3_surv_Dead[-length(BOS3_surv_Dead)]) - c(0,BOS3_surv_Censor[-length(BOS3_surv_Censor)])
                  
  )
  
  bos_df = mutate(bos_df, 
                  data_prog = c(data_num[1] + data_Dead[1], data_num_diff[-1] + data_Dead_diff[-1])
                  #BOS1_prog = c(BOS1_num[1] + BOS1_Dead[1], BOS1_num_diff[-1] + BOS1_Dead_diff[-1]),
                  #BOS2_prog = c(BOS2_num[1] + BOS2_Dead[1], BOS2_num_diff[-1] + BOS2_Dead_diff[-1]),
                  #BOS3_prog = c(BOS3_num[1] + BOS3_Dead[1], BOS3_num_diff[-1] + BOS3_Dead_diff[-1]),
                  #BOS3_surv_prog = c(BOS3_surv_num[1] + BOS3_surv_Dead[1], BOS3_surv_num_diff[-1] + BOS3_surv_Dead_diff[-1])
  )
  
  bos_df = mutate(bos_df, 
                  data_prog_per = data_prog / data_risk
                  #BOS1_prog_per = BOS1_prog / BOS1_risk,
                  #BOS2_prog_per = BOS2_prog / BOS2_risk,
                  #BOS3_prog_per = BOS3_prog / BOS3_risk,
                  #BOS3_surv_prog_per = BOS3_surv_prog / BOS3_surv_risk
  )
  
  
  t = 1 *(1 - bos_df$data_prog_per[1])
  b = c(t)
  for(entry in bos_df$data_prog_per[-1]){
    t = t*(1-entry)
    #print(t)
    b = c(b,t)
  }
  bos_df$data = b*100
  
  # t = 1 *(1 - bos_df$BOS1_prog_per[1])
  # b = c(t)
  # for(entry in bos_df$BOS1_prog_per[-1]){
  #   t = t*(1-entry)
  #   #print(t)
  #   b = c(b,t)
  # }
  # bos_df$BOS1_free = b*100
  # 
  # t = 1 *(1 - bos_df$BOS2_prog_per[1])
  # b = c(t)
  # for(entry in bos_df$BOS2_prog_per[-1]){
  #   t = t*(1-entry)
  #   #print(t)
  #   b = c(b,t)
  # }
  # bos_df$BOS2_free = b*100
  # 
  # t = 1 *(1 - bos_df$BOS3_prog_per[1])
  # b = c(t)
  # for(entry in bos_df$BOS3_prog_per[-1]){
  #   t = t*(1-entry)
  #   #print(t)
  #   b = c(b,t)
  # }
  # bos_df$BOS3_free = b*100
  # 
  # t = 1 *(1 - bos_df$BOS3_surv_prog_per[1])
  # b = c(t)
  # for(entry in bos_df$BOS3_surv_prog_per[-1]){
  #   t = t*(1-entry)
  #   #print(t)
  #   b = c(b,t)
  # }
  # bos_df$Survival = b*100
  colnames(bos_df) = gsub('data',replacement_name,colnames(bos_df))
  return(bos_df)
}



BOS_RAS_function = function(full_data){
  #full_data = pFEV_wf
  
  #BOS1 = 0.8
  #BOS2 = 0.66
  #BOS3 = 0.5
  end_time = 50
  MRN = full_data$MRN
  d_date = full_data$MonthsToDeath
  rx = full_data$MonthSinceRx
  status = full_data$Status
  RAS_date = full_data$RAS
  BOS1_date = full_data$BOS1_RAS
  BOS2_date = full_data$BOS2_RAS
  BOS3_date = full_data$BOS3_RAS
  Survival = full_data$MonthsToDeath
  
  RAS_date[is.na(RAS_date)] = end_time
  BOS1_date[is.na(BOS1_date)] = end_time
  BOS2_date[is.na(BOS2_date)] = end_time
  BOS3_date[is.na(BOS3_date)] = end_time
  Survival[is.na(Survival)] = end_time
  
  full_data$RAS_date = RAS_date
  full_data$BOS1_date = BOS1_date
  full_data$BOS2_date = BOS2_date
  full_data$BOS3_date = BOS3_date
  full_data$Survival = Survival
  
  
  time = seq(-24,48,1)
  bos_df = data.frame(time = time)
  
  bos_df$RAS_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[x >= RAS_date]))))
  bos_df$BOS1_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[x >= BOS1_date]))))
  bos_df$BOS2_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS2_date[x >= BOS2_date]))))
  bos_df$BOS3_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_date[x >= BOS3_date]))))
  bos_df$BOS3_surv_num = unlist(lapply(bos_df$time, function(x) length(na.omit(Survival[x >= Survival]))))
  
  bos_df$RAS_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < RAS_date]))))
  bos_df$BOS1_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS1_date]))))
  bos_df$BOS2_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS2_date]))))
  bos_df$BOS3_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS3_date]))))
  bos_df$BOS3_surv_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < Survival]))))
  
  bos_df$RAS_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < RAS_date]))))
  bos_df$BOS1_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS1_date]))))
  bos_df$BOS2_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS2_date]))))
  bos_df$BOS3_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS3_date]))))
  bos_df$BOS3_surv_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < Survival]))))
  
  
  bos_df = mutate(bos_df,
                  RAS_num_diff = c(0,diff(RAS_num)),
                  BOS1_num_diff = c(0,diff(BOS1_num)),
                  BOS2_num_diff = c(0,diff(BOS2_num)),
                  BOS3_num_diff = c(0,diff(BOS3_num)),
                  BOS3_surv_num_diff = c(0,diff(BOS3_surv_num))
  )
  bos_df = mutate(bos_df, 
                  RAS_Dead_diff = c(0,diff(RAS_Dead)),
                  BOS1_Dead_diff = c(0,diff(BOS1_Dead)),
                  BOS2_Dead_diff = c(0,diff(BOS2_Dead)),
                  BOS3_Dead_diff = c(0,diff(BOS3_Dead)),
                  BOS3_surv_Dead_diff = c(0,diff(BOS3_surv_Dead))
  )
  bos_df = mutate(bos_df, 
                  RAS_Censor_diff = c(0,diff(RAS_Censor)),
                  BOS1_Censor_diff = c(0,diff(BOS1_Censor)),
                  BOS2_Censor_diff = c(0,diff(BOS2_Censor)),
                  BOS3_Censor_diff = c(0,diff(BOS3_Censor)),
                  BOS3_surv_Censor_diff = c(0,diff(BOS3_surv_Censor))
  )
  
  bos_df = mutate(bos_df, 
                  RAS_risk = dim(full_data)[1] - c(0,RAS_num[-length(BOS1_num)]) - c(0,RAS_Dead[-length(RAS_Dead)]) - c(0,RAS_Censor[-length(RAS_Censor)]),
                  BOS1_risk = dim(full_data)[1] - c(0,BOS1_num[-length(BOS1_num)]) - c(0,BOS1_Dead[-length(BOS1_Dead)]) - c(0,BOS1_Censor[-length(BOS1_Censor)]),
                  BOS2_risk = dim(full_data)[1] - c(0,BOS2_num[-length(BOS2_num)]) - c(0,BOS2_Dead[-length(BOS2_Dead)]) - c(0,BOS2_Censor[-length(BOS2_Censor)]),
                  BOS3_risk = dim(full_data)[1] - c(0,BOS3_num[-length(BOS3_num)]) - c(0,BOS3_Dead[-length(BOS3_Dead)]) - c(0,BOS3_Censor[-length(BOS3_Censor)]),
                  BOS3_surv_risk = dim(full_data)[1] - c(0,BOS3_surv_num[-length(BOS3_surv_num)]) - c(0,BOS3_surv_Dead[-length(BOS3_surv_Dead)]) - c(0,BOS3_surv_Censor[-length(BOS3_surv_Censor)])
                  
  )
  
  bos_df = mutate(bos_df, 
                  RAS_prog = c(RAS_num[1] + RAS_Dead[1], RAS_num_diff[-1] + RAS_Dead_diff[-1]),
                  BOS1_prog = c(BOS1_num[1] + BOS1_Dead[1], BOS1_num_diff[-1] + BOS1_Dead_diff[-1]),
                  BOS2_prog = c(BOS2_num[1] + BOS2_Dead[1], BOS2_num_diff[-1] + BOS2_Dead_diff[-1]),
                  BOS3_prog = c(BOS3_num[1] + BOS3_Dead[1], BOS3_num_diff[-1] + BOS3_Dead_diff[-1]),
                  BOS3_surv_prog = c(BOS3_surv_num[1] + BOS3_surv_Dead[1], BOS3_surv_num_diff[-1] + BOS3_surv_Dead_diff[-1])
  )
  
  bos_df = mutate(bos_df, 
                  RAS_prog_per = RAS_prog / RAS_risk,
                  BOS1_prog_per = BOS1_prog / BOS1_risk,
                  BOS2_prog_per = BOS2_prog / BOS2_risk,
                  BOS3_prog_per = BOS3_prog / BOS3_risk,
                  BOS3_surv_prog_per = BOS3_surv_prog / BOS3_surv_risk
  )
  
  
  t = 1 *(1 - bos_df$RAS_prog_per[1])
  b = c(t)
  for(entry in bos_df$RAS_prog_per[-1]){
    t = t*(1-entry)
    #print(t)
    b = c(b,t)
  }
  bos_df$RAS_free = b*100
  
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
  bos_df$Survival = b*100
  
  return(bos_df)
}


BOS_function = function(full_data){
  #full_data = pFEV_wf
  
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
  
  full_data$BOS1_date = BOS1_date
  full_data$BOS2_date = BOS2_date
  full_data$BOS3_date = BOS3_date
  full_data$BOS3_surv_date = BOS3_surv_date
  
  
  time = seq(-24,48,1)
  bos_df = data.frame(time = time)
  
  bos_df$BOS1_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS1_date[x >= BOS1_date]))))
  bos_df$BOS2_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS2_date[x >= BOS2_date]))))
  bos_df$BOS3_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_date[x >= BOS3_date]))))
  bos_df$BOS3_surv_num = unlist(lapply(bos_df$time, function(x) length(na.omit(BOS3_surv_date[x >= BOS3_surv_date]))))
  
  bos_df$BOS1_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS1_date]))))
  bos_df$BOS2_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS2_date]))))
  bos_df$BOS3_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS3_date]))))
  bos_df$BOS3_surv_Dead = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '2' & d_date <= x & !is.na(d_date) & x < BOS3_surv_date]))))
  
  
  bos_df$BOS1_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS1_date]))))
  bos_df$BOS2_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS2_date]))))
  bos_df$BOS3_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS3_date]))))
  bos_df$BOS3_surv_Censor = unlist(lapply(bos_df$time, function(x) length(na.omit(MRN[status == '1' & rx <= x & !is.na(rx) & x < BOS3_surv_date]))))
  

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
  plot_data = m_bos3[m_bos3$Status != 'All',]
  all_data = m_bos3[m_bos3$Status == 'All',]
  p = ggplot(plot_data, aes(x = time, y = value,col=Status)) +
    guides(col=guide_legend(title=global_factor)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_line(lwd = 2) +
    ggtitle(paste(col_name,'by', global_factor)) +

    xlim(x1,x2)
  if(dim(all_data)[1] > 0){
    p = p + geom_line(data = all_data, aes(x = time, y = value), col = 'black',linetype="dotted")
  }
  
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


pairwise_manova_function = function(data,m_factor,input){
  df_b = data.frame(term = numeric(0), df = numeric(0), pillai = numeric(0), statistic = numeric(0), num.df = numeric(0), den.df = numeric(0), p.value = numeric(0), Factor = numeric(0), comparison = numeric(0), range = numeric(0), numbers = numeric(0))
  if(dim(data)[1] > 1){
    
  factor_list = paste(unlist(unique(data[,m_factor])))
  factor_list = factor_list[order(factor_list)]
  factor_list
  num = length(factor_list)
  if(num > 1){
    cmd = paste('res.man = manova(cbind(variable,value) ~ ',m_factor,', data = data)')
    eval(parse(text = cmd))
    df = tidy(res.man)
    if(num > 2){
      df$Factor = m_factor
      df$comparison = 'All'
      df$range = paste0('(',input$pre_range[1],':',input$post_range[2],')')
      
      nums = lapply(factor_list, function(x) length(na.omit(data$value[data[,m_factor] == x])))
      df$numbers = paste(nums, collapse = ', ')
      for(i in c(1:num)){
        i_entry = factor_list[i]
        for(j in c(1:num)){
          j_entry = factor_list[j]
          if(i < j){
            m_data = data[data[,m_factor] %in% c(i_entry,j_entry),]
            df_n = df_b
            df_n = data.frame(term = m_factor, df = NA, pillai = NA, statistic = NA, num.df = NA, den.df = NA, p.value = NA, comparison = NA, numbers = NA)
            cmd = paste('df_n = tidy(manova(cbind(variable,value) ~ ',m_factor,', data = m_data))')
            
            try(eval(parse(text = cmd)))
              df_n$Factor = m_factor
              df_n$comparison = paste(i_entry,'vs',j_entry)
              df_n$range = paste0('(',input$pre_range[1],':',input$post_range[2],')')
              df_n$numbers = paste(length(m_data$value[m_data[,m_factor] == i_entry & !is.na(m_data$value)]),'vs',length(m_data$value[m_data[,m_factor] == j_entry  & !is.na(m_data$value)]))
              df = rbind(df,df_n)

            
          }
        }
      }
      
      
    }else{
      df$Factor = m_factor
      #df$comparison = 'All'
      df$comparison = paste(factor_list[1],'vs',factor_list[2])
      df$range = paste0('(',input$pre_range[1],':',input$post_range[2],')')
      df$numbers = paste(length(data$value[data[,m_factor] == factor_list[1]  & !is.na(data$value)]),'vs',length(data$value[data[,m_factor] == factor_list[2]  & !is.na(data$value)]))

    }
    #df$p.value = signif(df$p.value,3)
    df = col_rearrange_function(df,4)
    return(df)
  }
  }
}



binary_t_test_function = function(pre_data,post_data,factor_list,global_factor,input){
  p_pre = tidy(t.test(na.omit(pre_data$value[pre_data[,global_factor] == factor_list[1]]),na.omit(pre_data$value[pre_data[,global_factor] == factor_list[2]])))
  p_pre$comparison = 'Pre Treatment'
  p_pre$range = paste(input$pre_range[1],'to',input$pre_range[2])
  
  p_post = tidy(t.test(na.omit(post_data$value[post_data[,global_factor] == factor_list[1]]),na.omit(post_data$value[post_data[,global_factor] == factor_list[2]]))) 
  p_post$comparison = 'Post Treatment'
  p_post$range = paste(input$post_range[1],'to',input$post_range[2])
  
  t_df = rbind(p_pre,p_post)
  t_df$term = global_factor
  t_df$Status = paste(factor_list, collapse = ', ')
  return(t_df)
}

horizontal_stats_function = function(full_data,pre_cols,post_cols,global_factor,input) {
  t_df_b = data.frame(estimate = numeric(0), estimate1 = numeric(0), estimate2 = numeric(0), statistic = numeric(0), p.value = numeric(0), parameter = numeric(0), conf.low = numeric(0), conf.high = numeric(0), method = numeric(0), alternative = numeric(0), comparison = numeric(0), range = numeric(0), term = numeric(0), Status = numeric(0))
  t_df = t_df_b
  anova_df_b = data.frame(term = numeric(0), df = numeric(0), pillai = numeric(0), statistic = numeric(0), num.df = numeric(0), den.df = numeric(0), p.value = numeric(0), comparison = numeric(0), range = numeric(0), Status = numeric(0))
  anova_df = anova_df_b

  pre_data = full_data[full_data$variable %in% pre_cols,]
  
  post_data = full_data[full_data$variable %in% post_cols,]
  
  
  full_data[,global_factor][is.na(full_data[,global_factor])] = 'NA'
  factor_list = paste(unique(full_data[,global_factor]))

  factor_list = factor_list[order(factor_list)]

  if(length(factor_list) == 2){

    t_df = tryCatch(binary_t_test_function(pre_data,post_data,factor_list,global_factor,input), error = function(e) e = t_df_b)

  }else{
    if(length(factor_list > 2)){
 
      anova_df = tryCatch(anova_function(pre_data,post_data,global_factor,factor_list,input), error = function(e) e = anova_df_b)

      for(i in c(1:length(factor_list))){
        i_entry = factor_list[i]
        for(j in c(1:length(factor_list))){
          if(i < j){
            j_entry = factor_list[j]
            sub_factor_list = c(i_entry,j_entry)
            t_df_n = t_df_b
            #print('mulit t test')
            t_df_n = tryCatch(binary_t_test_function(pre_data,post_data,sub_factor_list,global_factor,input), error = function(e) e = t_df_b)
            if(dim(t_df_n)[1] > 0){
              t_df = rbind(t_df,t_df_n)
            }
            
          }
        }
      }
      
    }
  }
  #View(t_df)
  #View(anova_df)
  t_df = t_df[,c('term','Status','comparison','range',colnames(t_df)[c(0:(length(colnames(t_df))-4))])]
  anova_df = anova_df[,c('term','Status','comparison','range',colnames(anova_df)[c(1:(length(colnames(anova_df))-3))])]
  
  
  t_df$p.value = signif(t_df$p.value,3)
  anova_df$p.value = signif(anova_df$p.value,3)
  output = list(t_df = t_df, anova_df = anova_df, pre_data = pre_data, post_data = post_data)
  return(output)
}

anova_function = function(pre_data,post_data,global_factor,factor_list,input){
  cmd = paste("pre_anova = tidy(manova(cbind(time,value) ~ ",global_factor,", data = pre_data))")
  #print(cmd)
  eval(parse(text = cmd))
  pre_anova$comparison ='Pre Treatment'
  pre_anova$range = paste(input$pre_range[1],'to',input$pre_range[2])
  #pre_anova$Factor = global_factor
  #df_a$comparisong ='Pre Treatment'
  pre_anova$Status = paste(factor_list,collapse = ', ')
  cmd = paste("post_anova = tidy(manova(cbind(time,value) ~ ",global_factor,", data = post_data))")
  #print(cmd)
  eval(parse(text = cmd))
  post_anova$comparison ='Post Treatment'
  post_anova$range = paste(input$post_range[1],'to',input$post_range[2])
  
  #post_anova$Factor = global_factor
  #df_a$comparisong ='Pre Treatment'
  post_anova$Status = paste(factor_list,collapse = ', ')
  anova_df = rbind(pre_anova,post_anova)
  #anova_df
  return(anova_df)
}



manova_function = function(stat_data,global_factor,input){
  cmd = paste("anova_df = tidy(manova(cbind(variable,value) ~ ",global_factor,", data = stat_data))")
  #print(cmd)
  eval(parse(text = cmd))
  anova_df
}




######### FORMATTING ##############

proportion_table_formating_factor = function(df,col_range,colour,mtc = 'none'){
  df[,col_range] = apply(df[,col_range],2,function(x) as.numeric(x))
  df[is.na(df)] = 0
  #df$p.value.original = df$p.value
  df[,mtc] = p.adjust(df$p.value, method = mtc, n = length(df$p.value))
  
 
  df$significant = ifelse(df[,mtc] < 0.05,1,0)
  
  
  datatable(df, options = list(rownames = FALSE, pageLength = 500,
                                                 columnDefs = list(list(targets = grep('significant',colnames(df)),
                                                                        visible = FALSE)))) %>% 
    formatStyle(names(df[,col_range]),
                                                   background = styleColorBar(range(df[,col_range]), colour),
                                                   backgroundSize = '98% 88%',
                                                   backgroundRepeat = 'no-repeat',
                                                   backgroundPosition = 'center') %>% 
    formatStyle(
      mtc, 'significant',
      target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', 'lightyellow'))) %>% 
    
    formatStyle(
      mtc, 'significant',
      #target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', 'yellow'))) %>%
    formatSignif(colnames(select_if(df, is.numeric)),3)
  
}


proportion_table_formating_within = function(df,col_range,colour,mtc = 'none'){
  df[,col_range] = apply(df[,col_range],2,function(x) as.numeric(x))
  df[is.na(df)] = 0

  #df$p.value.original = df$p.value
  #df$p.value = p.adjust(df$p.value, method = mtc, n = length(df$p.value))
  datatable(df,options = list(rownames = FALSE, pageLength = 500)) %>% 
    formatStyle(names(df[,col_range]),
                                                   background = styleColorBar(range(df[,col_range]), colour),
                                                   backgroundSize = '98% 88%',
                                                   backgroundRepeat = 'no-repeat',
                                                   backgroundPosition = 'center') %>%
    formatSignif(colnames(select_if(df, is.numeric)),3)
  
}


significance_table_formatting_function = function(df,mtc = 'none',sort = T){
  if(sort == T){
    if('term' %in% colnames(df)){
      df = df[order(df$term),]
    }
    if("Status" %in% colnames(df) & 'Factor' %in% colnames(df)){
      df = df[order(df$Factor,df$Status),]
    }
  }
  #df$significant = ifelse(df$p.value < 0.05,1,0)
  #df$p.value.original = df$p.value
  df[,mtc] = p.adjust(df$p.value, method = mtc, n = length(na.omit(df$p.value)))
  df$significant = ifelse(df[,'p.value'] < 0.05,1,0)
  
  df$mtc_significant = ifelse(df[,mtc] < 0.05,1,0)
  df = col_rearrange_function(df,2)
  #df = apply(df, 2, function(x) ifelse(is.numeric(x), signif(x,3), x))
  datatable(df, options = list(pageLength = 500,
                               columnDefs = list(list(targets = c(grep('significant',colnames(df),grep('significant',colnames(df)))),
                                                      visible = FALSE)))) %>% 
    formatStyle(
      'p.value', 'significant',
      target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', 'lightyellow'))) %>% 
    formatStyle(
      mtc, 'mtc_significant',
      #target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', 'yellow'))) %>%
    formatSignif(colnames(select_if(df, is.numeric)),3)
}


significance_table_formatting_function_backup = function(df,mtc = 'none'){
  if('term' %in% colnames(df)){
    df = df[order(df$term),]
  }
  if("Status" %in% colnames(df) & 'Factor' %in% colnames(df)){
    df = df[order(df$Factor,df$Status),]
  }
  #df$significant = ifelse(df$p.value < 0.05,1,0)
  df$p.value.original = df$p.value
  df$p.value = p.adjust(df$p.value, method = mtc, n = length(df$p.value))
  df$significant = ifelse(df$p.value < 0.05,1,0)
  
  datatable(df, options = list(pageLength = 500,
                               columnDefs = list(list(targets = grep('significant',colnames(df)),
                                                      visible = FALSE)))) %>% 
    formatStyle(
    'p.value', 'significant',
    target = 'row',
    backgroundColor = styleEqual(c(0, 1), c('white', 'lightyellow'))) %>% 
    formatStyle(
      'p.value', 'significant',
      #target = 'row',
      backgroundColor = styleEqual(c(0, 1), c('white', 'yellow'))) %>%
    formatSignif(colnames(select_if(df, is.numeric)),3)
}

table_formatting_function = function(df){
  datatable(df, options = list(pageLength = 500)) %>% formatSignif(colnames(select_if(df, is.numeric)),3)
}

col_rearrange_function = function(df,col_num){
  l = length(colnames(df))
  df = df[,c((l-col_num+1):l,1:(l-col_num))]
  return(df)
}

processed_data_long_function = function(processed_data){
  matrix_column_list = c("i_pFEV1_matrix","pFEV1_matrix","pFVC_matrix","pRatio_matrix","i_pFEV1_matrix","i_pFVC_matrix","i_pRatio_matrix","sm_i_pFEV1_matrix","sm_i_pFVC_matrix","sm_i_pRatio_matrix","d1_pFEV1_matrix","d1_pFVC_matrix","d1_pRatio_matrix","d2_pFEV1_matrix","d2_pFVC_matrix","d2_pRatio_matrix")
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
  return(processed_long)
}
