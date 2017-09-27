



shinyServer(function(input, output) {
  

  

  ########## TEXT OUTPUTS ##################
  
  output$sessionInfo <- renderText({
    paste(capture.output(si$R.version$version.string), '"<br />"')
  })
  
  output$t1 = renderText(input$anova_range[1])
  output$t2 = renderText(input$anova_range[2])
  output$t_range_text = renderText(paste('(',input$pre_range[1],' to ',input$pre_range[2],') vs (',input$post_range[1],' to ',input$post_range[2],')'))
  
  output$t_ratio_text = renderText(paste('log2(pFEV at zero point / pFEV at ',input$anova_range[1],') vs log2(pFEV at ',input$anova_range[2],' / pFEV zero point)'))
  output$t_pFEV_text = renderText(paste('(',input$anova_range[1],' to -1 ) vs ( 1 to ',input$anova_range[2],')'))
  output$t_pFEV_zero_text = renderText(paste('pre p value (',input$anova_range[1],' vs 0 ), post p value ( 0 vs ',input$anova_range[2],')'))
  
  output$slope_pFEV_text = renderText(paste('(',input$anova_range[1],' to 0 ) vs ( 0 to ',input$anova_range[2],')'))
  
  output$t_d1_text = renderText(paste('(',input$d1_pre_range[1],' to ',input$d1_pre_range[2],') vs (',input$d1_post_range[1],' to ',input$d1_post_range[2],')'))
  output$slope_d1_text = renderText(paste('(',input$anova_range[1],' to 0 ) vs ( 0 to ',input$anova_range[2],')'))
  
  ########## DATA TABLES #####################
  
    output$clustering = renderDataTable(clustering)
    #output$clustering = renderDataTable(clustering)
    output$full_num = renderDataTable(full_num)
    output$pFEV_wf_r = renderDataTable(pFEV_wf_r)
    output$i_pFEV_wf_r = renderDataTable(i_pFEV_wf_r)
    output$i_pFEV_sm_d1_f_r = renderDataTable(i_pFEV_sm_d1_f_r)
    output$i_pFEV_sm_d2_f_r = renderDataTable(i_pFEV_sm_d2_f_r)
    
    output$na2z = renderDataTable(clust_0)

#### MISSINGNESS ############# 
    output$missmap_plot = renderPlot({
      par(mar=c(20,2,2,2),mgp=c(30,1,0))
      missmap(full_num)
    },height = 800, width = 800 )

    output$pFEV_na_hist = renderPlot({
      hist(full_fac_0$pFEV_na,breaks = 20, main = 'Percentage of Completeness for pFEV data')
    })
  
############## pFEV ########
  
  ############## pFEV line plots ######################
  ######### DISPLAY PATIENTS INDIVIDUALL AND CHOOSE WHICH ONES TO REMOVE ############
              excluded_patients = reactive(patient_list[patient_list %in% input$remove_list])
            
              retained_patients = reactive(patient_list[!(patient_list %in% input$remove_list)])
            
            line_size = 2
            point_size = 4
            sm_size = 1
              
            ####### retained ###########
              output$plots <- renderUI({
                plot_output_list <- lapply(1:length(retained_patients()), function(i) {
                  plotname <- paste("plot", i, sep="")
                  plotOutput(plotname, height = 280, width = 250)
                })
            
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
              })
              r_list = isolate(retained_patients())
              for (i in 1:length(r_list)) {
            
                # Need local so that each item gets its own number. Without it, the value
                # of i in the renderPlot() will be the same across all instances, because
                # of when the expression is evaluated.
                local({
                  my_i <- i
                  plotname <- paste("plot", my_i, sep="")
            
                  #my_i = 1
                  output[[plotname]] <- renderPlot({
                    #t_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% i_pFEV_lf_r$MRN[1],]
                    i_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% retained_patients()[my_i],]
                    sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% retained_patients()[my_i],]
                    #i_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% input$mrn_select,]
                    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$mrn_select,]
                    if(dim(sm_data)[1] > 0){
                      ggplot(NULL) +
                        geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
            
                        geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
                        geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
                        #geom_line(data = )
                        #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                        scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            
                        geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='green',size = sm_size)+
            
                        theme(axis.text.x = element_text(size=8, angle=90)) +
                        theme(legend.position="none") +
                        ggtitle(paste0(retained_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
                    }else{
                      ggplot(NULL) +
                        geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
                        geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
                        geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
                        #geom_line(data = )
                        #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                        scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            
                        #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
            
                        theme(axis.text.x = element_text(size=8, angle=90)) +
                        theme(legend.position="none") +
                        ggtitle(retained_patients()[my_i])
                    }
            
                  },width = 600)
                })
              }
            
            ########## excluded ###############
              output$excluded_plots <- renderUI({
                plot_output_list <- lapply(1:length(excluded_patients()), function(i) {
                  plotname <- paste("excluded_plot", i, sep="")
                  plotOutput(plotname, height = 280, width = 250)
                })
            
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
              })
            
              e_list = isolate(excluded_patients())
              for (i in 1:length(e_list)) {
            
                # Need local so that each item gets its own number. Without it, the value
                # of i in the renderPlot() will be the same across all instances, because
                # of when the expression is evaluated.
                local({
                  my_i <- i
                  plotname <- paste("excluded_plot", my_i, sep="")
            
                  #my_i = 2
                  #print(excluded_patients()[my_i])
                  output[[plotname]] <- renderPlot({
                    #t_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% i_pFEV_lf_r$MRN[1],]
                    print(excluded_patients()[my_i])
                    #o_data = pFEV_lf_r[pFEV_lf_r$MRN %in% patient_list[my_i],]
                    #i_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% patient_list[my_i],]
                    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% patient_list[my_i],]
                    i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% excluded_patients()[my_i],]
                    sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% excluded_patients()[my_i],]
                    o_data = pFEV_lf[pFEV_lf$MRN %in% excluded_patients()[my_i],]
                    
                    #i_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% input$mrn_select,]
                    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$mrn_select,]
                    if(dim(sm_data)[1]>0){
                      ggplot(NULL) +
                        geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
                        
                        geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
                        geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
                        
                        scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                        geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='green',size = sm_size)+
                        
                        
                        theme(axis.text.x = element_text(size=8, angle=90)) +
                        theme(legend.position="none") +
                        ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
                    }else{
                    
                    if(dim(i_data)[1]>0){
                      ggplot(NULL) +
                        geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
            
                        geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
                        geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
                        scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            
                        #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='blue',size = 0.7)+
            
                        theme(axis.text.x = element_text(size=8, angle=90)) +
                        theme(legend.position="none") +
                        ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
                      
                      }else{
                        ggplot(NULL) +
                          geom_vline(data = o_data,aes(xintercept = which(levels(o_data$variable) %in% '0'))) +
                          
                          geom_line(data = o_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
                          geom_point(data = o_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
                          #geom_line(data = )
                          #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                          scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                          
                          #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
                          
                          theme(axis.text.x = element_text(size=8, angle=90)) +
                          theme(legend.position="none") +
                          ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
                        
                      }
                    }
            
            
                  },width = 600)
                })
              }

  ############# ISOLATE DATA FRAME AFTER REMOVING INCORRECT DATA ###########
  
  
  pFEV_wf_r = isolate(pFEV_wf[pFEV_wf$MRN %in% retained_patients(),])
  pFEV_lf_r = isolate(pFEV_lf[pFEV_lf$MRN %in% retained_patients(),])        
  i_pFEV_wf_r = isolate(i_pFEV_wf[i_pFEV_wf$MRN %in% retained_patients(),])
  i_pFEV_lf_r = isolate(i_pFEV_lf[i_pFEV_lf$MRN %in% retained_patients(),])
  i_pFEV_sm_lf_r = isolate(i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% retained_patients(),])
  i_pFEV_sm_d1_f_r = isolate(i_pFEV_sm_d1_f[i_pFEV_sm_d1_f$MRN %in% retained_patients(),])
  i_pFEV_sm_d1_fl_r = isolate(i_pFEV_sm_d1_fl[i_pFEV_sm_d1_fl$MRN %in% retained_patients(),])
  i_pFEV_sm_d2_f_r = isolate(i_pFEV_sm_d2_f[i_pFEV_sm_d2_f$MRN %in% retained_patients(),])
  i_pFEV_sm_d2_fl_r = isolate(i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$MRN %in% retained_patients(),])
  

  ########### LINE PLOT ############
  
  output$line_pFEV = renderPlot({
    r_data = pFEV_lf_r[pFEV_lf_r$MRN %in% input$mrn_select,]
    
    ggplot(r_data, aes(x = variable, y = value,col = MRN,group = MRN)) + 
      geom_vline(aes(xintercept = which(levels(r_data$variable) %in% '0'))) +
      
      geom_line(aes(x = variable, y = value,col = MRN,group = MRN)) +
      geom_point() +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      theme(axis.text.x = element_text(size=8, angle=90)) +
      theme(legend.position="none") + 
      #scale_x_discrete(names = pFEV_numeric_colnames,breaks = pFEV_numeric_colnames) + 
      ggtitle(paste('pFEV values for ',length(unique(r_data$MRN))," Patients"))
  })
  
  output$line_i_pFEV = renderPlot({
    i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% i_pFEV_lf$MRN,]
    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% i_pFEV_sm_lf$MRN[1],]
    i_data = i_pFEV_lf_r[i_pFEV_lf_r$MRN %in% input$mrn_select,]
    #sm_data = i_pFEV_sm_lf_r[i_pFEV_sm_lf_r$MRN %in% input$mrn_select,]
    
    ggplot(NULL) + 
      #geom_vline(i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
      geom_line(data = i_data, aes(x = variable, y = value,col = MRN, group = MRN))+
      geom_point(data = i_data, aes(x = variable, y = data,col = MRN,group = MRN)) +
      #geom_line(data = sm_data, aes(x = variable, y = value,col = MRN, group = MRN))+
      
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      theme(axis.text.x = element_text(size=8, angle=90)) +
      theme(legend.position="none") +
      ggtitle(paste('Imputed pFEV values for ',length(unique(i_data$MRN))," Patients"))
  })
  
  ################ pFEV BOXPLOTS #######################
  output$boxplot_pFEV = renderPlot({
    plot_data = pFEV_lf_r
    ggplot(plot_data, aes(x = variable, y = value)) + 
      geom_vline(aes(xintercept = which(levels(plot_data$variable) %in% '0'))) +
      
      geom_boxplot(aes_string(col = input$global_factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ggtitle(paste('pFEV values for ',length(unique(plot_data$MRN))," Patients"))
  })
  output$boxplot_i_pFEV = renderPlot({
    plot_data = i_pFEV_lf_r[i_pFEV_lf_r$variable %in% pFEV_numeric_colnames_f,]
    summary_data = i_pFEV_lf_r
    ggplot(NULL) +
      
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(aes(xintercept = which(levels(i_pFEV_lf_r$variable) %in% '0'))) +
      
      geom_boxplot(data = plot_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle(paste('Imputed pFEV values for ',length(unique(plot_data$MRN))," Patients"))
  })
  
  output$boxplot_pFEV_mean = renderPlot({
    ggplot(pFEV_lf_r, aes(x = variable, y = value)) + 
      geom_vline(aes(xintercept = which(levels(pFEV_lf_r$variable) %in% '0'))) +
      
      #geom_boxplot(aes_string(col = input$global_factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Mean of Original Data")
  })
  
  output$boxplot_i_pFEV_mean = renderPlot({
    summary_data = i_pFEV_lf_r
    ggplot(summary_data, aes(x = variable, y = value)) + 
      #geom_boxplot(aes_string(col = input$global_factor)) +
      geom_vline(aes(xintercept = which(levels(i_pFEV_lf_r$variable) %in% '0'))) +
      
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Mean of Imputed Data")
  })
  
  ################ pFEV HEATMAP ########################
  

  ##### CLUSTERING ###############
  
  discrete_cluster_D = reactive({
    r_list = retained_patients()
    f_cols = eval(input$mix_clust_col_fac)
    n_cols = eval(input$mix_clust_col_num)
    d_num = input$clutree_num
    data = i_pFEV_wf[r_list,c(n_cols,f_cols)]
    o_data = data
    colnames(data)
    weights = c()
    fac_weight = input$fac_weight
    for(entry in colnames(data)){
      if(entry %in% pFEV_numeric_colnames_f){
        weights = c(weights,10)
      }else{
        weights = c(weights,fac_weight)
      }
    }
    data_dist = dist.subjects(data,weights = weights)
    D = dendro.subjects(data_dist,weights = weights)
    x = cutree(D, k = d_num)
    x_cluster = data.frame(MRN = numeric(0))
    for(entry in unique(x)){
      x_cluster[entry,'MRN'] = paste(list(names(x)[x == entry]),colapse=(", "))
    }
    print(unique(x))
    data$cluster = factor(x) 
    print(factor(x))
    list(D = D,o_data = o_data, data = data, x_cluster = x_cluster, weights = weights)
  })
  
  isolate_cluster = reactive({
    data = discrete_cluster_D()$data
    print(data$cluster)
    
    pFEV_wf_r$cluster = data$cluster
    pFEV_lf_r_c = melt(pFEV_wf_r, id.vars = c(colnames(full_fac_0),'cluster'), measure.vars = colnames(pFEV_w))
    
    i_pFEV_wf_r$cluster <- data$cluster
    
    i_pFEV_lf_r$cluster <- pFEV_lf_r_c$cluster[match(i_pFEV_lf_r$MRN,pFEV_lf_r_c$MRN)]
    
    i_pFEV_sm_d1_f_r$cluster <- pFEV_wf_r$cluster[match(i_pFEV_sm_d1_f_r$MRN,pFEV_wf_r$MRN)]
    i_pFEV_sm_d1_fl_r$cluster <- pFEV_lf_r_c$cluster[match(i_pFEV_sm_d1_fl_r$MRN,pFEV_lf_r_c$MRN)]
    
    i_pFEV_sm_d2_f_r$cluster <- pFEV_wf_r$cluster[match(i_pFEV_sm_d2_f_r$MRN,pFEV_wf_r$MRN)]
    i_pFEV_sm_d2_fl_r$cluster <- pFEV_lf_r_c$cluster[match(i_pFEV_sm_d2_fl_r$MRN,pFEV_lf_r_c$MRN)]
    
    list(i = i_pFEV_wf_r, il = i_pFEV_lf_r, 
         p = pFEV_wf_r, pl = pFEV_lf_r_c, 
         d1 = i_pFEV_sm_d1_f_r, d1l = i_pFEV_sm_d1_fl_r,
         d2 = i_pFEV_sm_d2_f_r, d2l = i_pFEV_sm_d2_fl_r)
  })
  
      #### ISOLATE cluster ##### 
          i_pFEV_wf_r = isolate(isolate_cluster()$i)
          i_pFEV_lf_r = isolate(isolate_cluster()$il)
          pFEV_wf_r = isolate(isolate_cluster()$p)
          pFEV_lf_r = isolate(isolate_cluster()$pl)
          i_pFEV_sm_d1_f_r = isolate(isolate_cluster()$d1)
          i_pFEV_sm_d1_fl_r = isolate(isolate_cluster()$d1l)
          i_pFEV_sm_d2_f_r = isolate(isolate_cluster()$d2)
          i_pFEV_sm_d2_fl_r = isolate(isolate_cluster()$d2l)
      ### PLOT CLUSTERS ####

          output$discrete_cluster_plot = renderPlot({
            D = discrete_cluster_D()$D
            plot(D,cex.lab = 0.5)
          })

          output$discrete_x_table = renderDataTable(discrete_cluster_D()$x_cluster)
          
          discrete_cutree_line_plots = reactive({
            data = discrete_cluster_D()$data
            cols = colnames(full_fac_0)[!(colnames(full_fac_0) %in% colnames(data))]
            data = cbind(data,full_fac_0[rownames(data),])
            n_cols = eval(input$mix_clust_col_num)
            max_n_cols = factor(max(as.numeric(n_cols)))
            min_n_cols = factor(min(as.numeric(n_cols)))
            m = which(colnames(i_pFEV_ts) == min_n_cols)
            n = which(colnames(i_pFEV_ts) == max_n_cols)
            sub_n_cols = colnames(i_pFEV_ts)[c(m:n)]
            plot_data = i_pFEV_wf[rownames(data),]
            plot_data$cluster = data$cluster
            data_l =  melt(plot_data, id.vars = c(colnames(full_fac_0),'cluster'), measure.vars = sub_n_cols)
            p = ggplot(data = data_l, aes(x=variable,y=value,group= MRN,col=cluster)) + 
              geom_vline(aes(xintercept = which(levels(data_l$variable) %in% '0'))) +
              
              geom_line() +
              #geom_point(aes(y = data),size = 3) +
              stat_summary(data = data_l,fun.y=mean,geom="line",lwd=2,
                           aes_string(x = 'variable', y = 'value',group='cluster',col = 'cluster')) +
              theme(axis.text.x = element_text(size=14, angle=90)) +
              scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              ggtitle("Clusters from imputer pFEV")
            
            
            s = ggplot(data = data_l, aes(x=variable,y=value,group= MRN,col=cluster)) + 
              #geom_line() +
              geom_vline(aes(xintercept = which(levels(data_l$variable) %in% '0'))) +
              
              stat_summary(data = data_l,fun.y=mean,geom="line",lwd=2,
                           aes_string(x = 'variable', y = 'value',group='cluster',col = 'cluster')) + 
              theme(axis.text.x = element_text(size=14, angle=90)) +
              scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              ggtitle("Mean of clusters from imputed pFEV values")
              
            list(p = p,s = s)
            })
          output$discrete_cutree_line = renderPlot(discrete_cutree_line_plots()$p)
          output$discrete_cutree_mean = renderPlot(discrete_cutree_line_plots()$s)
          
          
          output$mix_clu = renderPlot({
            data = discrete_cluster_D()$o_data
            weights = discrete_cluster_D()$weights
            D = discrete_cluster_D()$D
            mix.heatmap(data,dend.subjects = D,rowmar = 10,D.variables = NULL,legend.mat = T,varweights = weights)
          })
 
  

  
  
################# CHANGE #########################
    ############# line plot ###################
  
  i_mrn_sub_pFEV_sm = reactive({
    i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$change_mrn_select,]
  })
  
  output$i_pFEV_sm_line = renderPlot({
    r_data = i_mrn_sub_pFEV_sm()
    r_data = i_pFEV_sm_lf_r[i_pFEV_sm_lf_r$MRN %in% input$change_mrn_select,]

    #data = pFEV
    ggplot(r_data, aes(x = variable, y = value,col = MRN)) + 
      geom_line(aes(group = MRN)) +
      geom_vline(aes(xintercept = which(levels(r_data$variable) %in% '0'))) +
      
      #geom_point() +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      theme(axis.text.x = element_text(size=8, angle=90)) +
      theme(legend.position="none") + 
      #scale_x_discrete(names = pFEV_numeric_colnames,breaks = pFEV_numeric_colnames) + 
      ggtitle("IMPUTED SMOOTH")
  })
  
  ############# boxplot ####################
  output$boxplot_i_pFEV_sm = renderPlot({
    i_data = i_pFEV_sm_lf_r
    ggplot(NULL) +
      
      stat_summary(data = i_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(aes(xintercept = which(levels(i_pFEV_sm_lf$variable) %in% '0'))) +
      
      #geom_boxplot(data = i_pFEV_sm_lf[i_pFEV_lf$variable %in% pFEV_numeric_colnames_f & full_fac_0$pFEV_na > input$change_completeness,], aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_lf[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Imputed Smoothed Mean")
  })
  
  ############ D1 ##################
  
  
  
  i_mrn_sub_pFEV_sm_d1 = reactive({
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    
    i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$MRN %in% input$change_mrn_select & i_pFEV_sm_d1_fl_r$variable %in% cols,]
  })
  
  output$i_pFEV_sm_d1_line = renderPlot({
    r_data = i_mrn_sub_pFEV_sm_d1()
    #data = pFEV
    ggplot(r_data, aes(x = variable, y = value,col = MRN)) + 

      geom_line(aes(group = MRN)) +
      geom_vline(aes(xintercept = which(levels(r_data$variable) %in% '0'))) +
      
      #geom_point() +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      theme(axis.text.x = element_text(size=8, angle=90)) +
      theme(legend.position="none") + 
      #scale_x_discrete(names = pFEV_numeric_colnames,breaks = pFEV_numeric_colnames) + 
      ggtitle("IMPUTED SMOOTH D1")
  })
  
  output$boxplot_i_pFEV_sm_d1 = renderPlot({
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    summary_data =  i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% cols,]
    i_data = i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% pFEV_numeric_colnames_f,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(data = i_data, aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
      geom_boxplot(data = i_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d1_fl_r[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D1 boxplot")
  })
  
  output$boxplot_i_pFEV_sm_d1_mean = renderPlot({
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    summary_data =  i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% cols,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #geom_vline(aes(xintercept = which(levels(i_pFEV_sm_lf$variable) %in% '0'))) +
      
      #geom_boxplot(data = i_pFEV_sm_d1_fl_r[i_pFEV_lf$variable %in% pFEV_numeric_colnames_f & full_fac_0$pFEV_na > input$change_completeness,], aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d1_fl_r[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D1 Mean")
  })
  
  
  
  output$d1_mix_heatmap = renderPlot({
    #colnames(i_pFEV_sm_d1_mf)
    #mix.heatmap(i_pFEV_sm_d1_mf[full_fac_0$pFEV_na > input$completeness,c(eval(input$mix_clust_col),pFEV_numeric_colnames_f)],rowmar = 10,D.variables = NULL,legend.mat = T)
    mix.heatmap(i_pFEV_sm_d1_mf[i_pFEV_sm_d1_mf$pFEV_na > input$completeness,c(eval(input$change_mix_clust_col_fac),eval(input$change_mix_clust_col_num))],rowmar = 10,D.variables = NULL,legend.mat = T)
    
  })
  
  ####### D1 Clustering ############
  
  discrete_cluster_D_d1 = reactive({
    r_list = retained_patients()

    f_cols = eval(input$d1_mix_clust_col_fac)

    n_cols = eval(input$d1_mix_clust_col_num)

    d_num = input$d1_clutree_num
    data = i_pFEV_sm_d1_f[r_list,c(n_cols,f_cols)]
    o_data = data

    weights = c()
    fac_weight = input$d1_fac_weight
    for(entry in colnames(data)){
      if(entry %in% pFEV_numeric_colnames_f){
        weights = c(weights,10)
      }else{
        weights = c(weights,fac_weight)
      }
    }
    
    
    data_dist = dist.subjects(data,weights = weights)

    D = dendro.subjects(data_dist,weights = weights)

    x = cutree(D, k = d_num)
    x_cluster = data.frame(MRN = numeric(0))
    for(entry in unique(x)){
      x_cluster[entry,'MRN'] = paste(list(names(x)[x == entry]),colapse=(", "))
    }
    data$cluster = factor(x) 
    list(D = D,o_data = o_data, data = data, x_cluster = x_cluster, weights = weights)
  })
  
  isolate_cluster_d1 = reactive({
    data = discrete_cluster_D_d1()$data
    
    pFEV_wf_r$cluster_d1 = data$cluster
    pFEV_lf_r_c = melt(pFEV_wf_r, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = colnames(pFEV_w))
    
    
    #pFEV_lf_r$cluster_d1 <- data$cluster[match(pFEV_lf_r$MRN,data$MRN)]
  
    #i_pFEV_wf_r$cluster_d1 <- data$cluster[match(i_pFEV_wf_r$MRN,data$MRN)]
    i_pFEV_wf_r$cluster_d1 <- data$cluster
    
    i_pFEV_lf_r$cluster_d1 <- pFEV_lf_r_c$cluster_d1[match(i_pFEV_lf_r$MRN,pFEV_lf_r_c$MRN)]
    
    i_pFEV_sm_d1_f_r$cluster_d1 <- pFEV_wf_r$cluster_d1[match(i_pFEV_sm_d1_f_r$MRN,pFEV_wf_r$MRN)]
    i_pFEV_sm_d1_fl_r$cluster_d1 <- pFEV_lf_r_c$cluster_d1[match(i_pFEV_sm_d1_fl_r$MRN,pFEV_lf_r_c$MRN)]
    
    i_pFEV_sm_d2_f_r$cluster_d1 <- pFEV_wf_r$cluster_d1[match(i_pFEV_sm_d2_f_r$MRN,pFEV_wf_r$MRN)]
    i_pFEV_sm_d2_fl_r$cluster_d1 <- pFEV_lf_r_c$cluster_d1[match(i_pFEV_sm_d2_fl_r$MRN,pFEV_lf_r_c$MRN)]
    
    
    list(i = i_pFEV_wf_r, il = i_pFEV_lf_r, 
         p = pFEV_wf_r, pl = pFEV_lf_r_c, 
         d1 = i_pFEV_sm_d1_f_r, d1l = i_pFEV_sm_d1_fl_r, 
         d2 = i_pFEV_sm_d2_f_r, d2l = i_pFEV_sm_d2_fl_r)
  })
  
      #### ISOLATE cluster ##### 
        i_pFEV_wf_r = isolate(isolate_cluster_d1()$i)
        i_pFEV_lf_r = isolate(isolate_cluster_d1()$il)
        pFEV_wf_r = isolate(isolate_cluster_d1()$p)
        pFEV_lf_r = isolate(isolate_cluster_d1()$pl)
        i_pFEV_sm_d1_f_r = isolate(isolate_cluster_d1()$d1)
        i_pFEV_sm_d1_fl_r = isolate(isolate_cluster_d1()$d1l)
        i_pFEV_sm_d2_f_r = isolate(isolate_cluster_d1()$d2)
        i_pFEV_sm_d2_fl_r = isolate(isolate_cluster_d1()$d2l)
      ### PLOT CLUSTERS ####
      
        output$discrete_cluster_plot_d1 = renderPlot({
          D = discrete_cluster_D_d1()$D
          plot(D,cex.lab = 0.5)
        })
        
        
        output$discrete_x_table_d1 = renderDataTable(discrete_cluster_D_d1()$x_cluster)
        
      
        discrete_cutree_line_plots_d1 = reactive({
          data = discrete_cluster_D_d1()$data
          cols = colnames(full_fac_0)[!(colnames(full_fac_0) %in% colnames(data))]
          data = cbind(data,full_fac_0[rownames(data),])
          n_cols = eval(input$d1_mix_clust_col_num)
          max_n_cols = factor(max(as.numeric(n_cols)))
          min_n_cols = factor(min(as.numeric(n_cols)))
          m = which(colnames(i_pFEV_sm_d1) == min_n_cols)
          n = which(colnames(i_pFEV_sm_d1) == max_n_cols)
          sub_n_cols = colnames(i_pFEV_sm_d1)[c(m:n)]
          #sub_n_cols
          #print(sub_n_cols)
          plot_data = i_pFEV_sm_d1_f[rownames(data),]
          plot_data$cluster = data$cluster
          data_l =  melt(plot_data, id.vars = c(colnames(full_fac_0),'cluster'), measure.vars = sub_n_cols)
          p = ggplot(data = data_l, aes(x=variable,y=value,group= MRN,col=cluster)) + 
            geom_vline(aes(xintercept = which(levels(data_l$variable) %in% '0'))) +
            
            geom_line() +
            #geom_point(aes(y = data),size = 3) +
            stat_summary(data = data_l,fun.y=mean,geom="line",lwd=2,
                         aes_string(x = 'variable', y = 'value',group='cluster',col = 'cluster')) +
            theme(axis.text.x = element_text(size=14, angle=90)) +
            scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            ggtitle("Clusters from imputer pFEV")
          
          
          s = ggplot(data = data_l, aes(x=variable,y=value,group= MRN,col=cluster)) + 
            #geom_line() +
            geom_vline(aes(xintercept = which(levels(data_l$variable) %in% '0'))) +
            
            stat_summary(data = data_l,fun.y=mean,geom="line",lwd=2,
                         aes_string(x = 'variable', y = 'value',group='cluster',col = 'cluster')) + 
            theme(axis.text.x = element_text(size=14, angle=90)) +
            scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            ggtitle("Mean of clusters from imputed pFEV values")
          
          list(p = p,s = s)
        })
        output$discrete_cutree_line_d1 = renderPlot(discrete_cutree_line_plots_d1()$p)
        output$discrete_cutree_mean_d1 = renderPlot(discrete_cutree_line_plots_d1()$s)
        
        
        output$mix_clu_d1 = renderPlot({
          data = discrete_cluster_D_d1()$o_data
          weights = discrete_cluster_D_d1()$weights
          D = discrete_cluster_D_d1()$D
          mix.heatmap(data,dend.subjects = D,rowmar = 10,D.variables = NULL,legend.mat = T,varweights = weights)
        })
  
####################### D2 #########################
  
  i_mrn_sub_pFEV_sm_d2 = reactive({
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    i_pFEV_sm_d2_fl_r[i_pFEV_sm_d2_fl_r$MRN %in% input$change_mrn_select & i_pFEV_sm_d2_fl_r$variable %in% cols,]
  })
  
  output$i_pFEV_sm_d2_line = renderPlot({
    r_data = i_mrn_sub_pFEV_sm_d2()
    #data = pFEV
    ggplot(r_data, aes(x = variable, y = value,col = MRN)) + 
      geom_line(aes(group = MRN)) +
      #geom_point() +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      theme(axis.text.x = element_text(size=8, angle=90)) +
      theme(legend.position="none") + 
      #scale_x_discrete(names = pFEV_numeric_colnames,breaks = pFEV_numeric_colnames) + 
      ggtitle("IMPUTED SMOOTHED D2")
  })
  
  output$boxplot_i_pFEV_sm_d2 = renderPlot({
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    summary_data =  i_pFEV_sm_d2_fl_r[i_pFEV_sm_d2_fl_r$variable %in% cols,]
    i_data = i_pFEV_sm_d2_fl_r[i_pFEV_sm_d2_fl_r$variable %in% pFEV_numeric_colnames_f,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(data = i_data, aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
      geom_boxplot(data = i_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d2_fl_r[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D2 boxplot")
  })
  
  output$boxplot_i_pFEV_sm_d2_mean = renderPlot({

    cols = factor(c(input$anova_range[1]:input$anova_range[2]))

    summary_data =  i_pFEV_sm_d2_fl_r[i_pFEV_sm_d2_fl_r$variable %in% cols,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #geom_vline(aes(xintercept = which(levels(i_pFEV_sm_lf$variable) %in% '0'))) +
      
      #geom_boxplot(data = i_pFEV_sm_d2_fl_r[i_pFEV_lf$variable %in% pFEV_numeric_colnames_f & full_fac_0$pFEV_na > input$change_completeness,], aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d2_fl_r[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D2 Mean")
  })

  
############# STATS #####################
  
  
  ##### STATS by factor ggplot #############
  
        ####### LM ########
 
            df_lm = reactive({
                df = data.frame(Factor = numeric(), Status = numeric(0))
                factor = 'MRN'
                factor = input$global_factor
                cols = c(-6:6)
                cols = factor(c(input$anova_range[1]:input$anova_range[2]))
                cols
                #full_data=pFEV_lf[pFEV_lf$variable %in% cols,]
                function_data = pFEV_lf_r
                df = lm_function(function_data,factor,cols)
              })
            output$lm_table = renderDataTable(t(df_lm()))
            
            output$boxplot_anova_all_factor = renderPlot({
              factor = input$global_factor
              full_data = pFEV_lf_r
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=pFEV_lf_r[pFEV_lf_r$variable %in% cols,]
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Original Data")
            })
            output$boxplot_anova_before_factor = renderPlot({
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=pFEV_lf_r[pFEV_lf_r$variable %in% before & pFEV_lf_r$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Pre Treatment")
            })
            output$boxplot_anova_after_factor = renderPlot({
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=pFEV_lf_r[pFEV_lf_r$variable %in% after & pFEV_lf_r$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Post Treatment")
            })
            
            ## Imputed
            
            df_lm_i = reactive({
              df = data.frame(Factor = numeric(), Status = numeric(0))
              factor = 'MRN'
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              #full_data=pFEV_lf_r[pFEV_lf_r$variable %in% cols,]
              function_data = i_pFEV_lf_r
              df = lm_function(function_data,factor,cols)
            })
            output$lm_table_i = renderDataTable(t(df_lm_i()))
            
            output$boxplot_anova_all_factor_i = renderPlot({
              factor = input$global_factor
              function_data = i_pFEV_lf_r
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=function_data[function_data$variable %in% cols,]
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Original Data")
            })
            output$boxplot_anova_before_factor_i = renderPlot({
              function_data = i_pFEV_lf_r
              
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=function_data[function_data$variable %in% before & pFEV_lf_r$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Pre Treatment")
            })
            output$boxplot_anova_after_factor_i = renderPlot({
              function_data = i_pFEV_lf_r
              
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=function_data[function_data$variable %in% after & pFEV_lf_r$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Post Treatment")
            })

        ######## LM SAMPLE ######

          df_lm_sample = reactive({
            function_data = pFEV_lf_r
            df = pFEV_wf_r
            factor = 'MRN'
        
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            df = lm_sample_function(function_data,factor,cols,df)
          })
          output$df_lm_table = renderDataTable(t(df_lm_sample()))

          df_slope= reactive({
            factor = 'Status'
            factor = input$global_factor
            full_data = df_lm_sample()
            df = slope_function(full_data,factor,cols)
            
          })
          output$slope_table = renderDataTable(t(df_slope()))
          
          #output$slope_table = renderDataTable(df_slope()[,c("Factor","Status","T_p_value")])
          output$slope_boxplot = renderPlot({
            #data = df_sample
            data = df_lm_sample()
            slope_cols = c("slope_Pre","slope_Post")
            data_l = melt(data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = slope_cols)
            
            ggplot(data = data_l)+
              geom_boxplot(aes_string(col = 'variable',y='value',x = input$global_factor))
          })
          
          # imputed
          
          df_lm_sample_i = reactive({
            function_data = i_pFEV_lf_r
            df = i_pFEV_wf_r
            factor = 'MRN'
            
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            df = lm_sample_function(function_data,factor,cols,df)
          })
          output$df_lm_table_i = renderDataTable(df_lm_sample_i())
          df_slope_i = reactive({
            factor = 'Status'
            factor = input$global_factor
            full_data = df_lm_sample_i()
            df = slope_function(full_data,factor,cols)
            
          })
          output$slope_table_i = renderDataTable(df_slope_i())
          
          #output$slope_table_i = renderDataTable(df_slope_i()[,c("Factor","Status","T_p_value")])
          output$slope_boxplot_i = renderPlot({
            #data = df_sample
            data = df_lm_sample_i()
            slope_cols = c("slope_Pre","slope_Post")
            data_l = melt(data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = slope_cols)
            
            ggplot(data = data_l)+
              geom_boxplot(aes_string(col = 'variable',y='value',x = input$global_factor))
          })
        
        ######### T test ##############

  
          pp_t_test = reactive({
              full_data = pFEV_lf_r
              factor = input$global_factor
              t1 = input$anova_range[1]
              t2 = input$anova_range[2]
              df = pp_t_test_function(full_data,factor,t1,t2)
            })
          output$pp_t_table = renderDataTable(t(pp_t_test()))
          output$boxplot_pp = renderPlot({
            full_data = pFEV_lf_r
            t1 = input$anova_range[1]
            t2 = input$anova_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_function(full_data,t1,t2,global_factor)
            print(p)
          })
        
              ### Imputed ####
              
                  pp_t_test_i = reactive({
                    full_data = i_pFEV_lf_r
                    factor = input$global_factor
                    t1 = input$anova_range[1]
                    t2 = input$anova_range[2]
                    df = pp_t_test_function(full_data,factor,t1,t2)
                  })
                  
                  output$pp_t_table_i = renderDataTable(t(pp_t_test_i()))
                  
                  output$boxplot_pp_i = renderPlot({
                    full_data = i_pFEV_lf_r
                    t1 = input$anova_range[1]
                    t2 = input$anova_range[2]
                    global_factor = input$global_factor
                    p = boxplot_pp_function(full_data,t1,t2,global_factor)
                    print(p)
                  })
        
      
            ####t test by Range ####
                  pp_t_test_ranges = reactive({
                    full_data = pFEV_lf_r
                    factor = input$global_factor
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    df = pp_t_test_range_function(full_data,factor,pre1,pre1,post1,post2)
                  })
                  output$pp_t_table_ranges = renderDataTable(t(pp_t_test_ranges()))
                  output$boxplot_pp_ranges = renderPlot({
                    full_data = pFEV_lf_r
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    global_factor = input$global_factor
                    p = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor)
                    print(p)
                  })
                  
                  #### Imputed ###
                  
                  pp_t_test_ranges_i = reactive({
                    full_data = i_pFEV_lf_r
                    factor = input$global_factor
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    df = pp_t_test_range_function(full_data,factor,pre1,pre1,post1,post2)
                  })
                  output$pp_t_table_ranges_i = renderDataTable(t(pp_t_test_ranges_i()))
                  output$boxplot_pp_ranges_i = renderPlot({
                    full_data = i_pFEV_lf_r
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    global_factor = input$global_factor
                    p = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor)
                    print(p)
                  })
                  
      ### Ratio  FULL #### 
        pp_t_test_ratio_full = reactive({
          full_data = pFEV_lf_r
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df
        })
        output$pp_t_table_ratio_full = renderDataTable(t(pp_t_test_ratio_full()))
        output$boxplot_pp_ratio_full = renderPlot({
          full_data = pFEV_lf_r
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
        
        pp_t_test_ratio_full_i = reactive({
          full_data = i_pFEV_lf_r
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df
        })
        output$pp_t_table_ratio_full_i = renderDataTable(t(pp_t_test_ratio_full_i()))
        output$boxplot_pp_ratio_full_i = renderPlot({
          full_data = i_pFEV_lf_r
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
  
      ### Ratio ####
        pp_t_test_ratio = reactive({
          #df = data.frame(Factor = numeric(0))
          full_data = pFEV_lf_r
          #factor = 'Status'
          factor = input$global_factor
          #t1 = -6
          #t2 = 6
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_function(full_data,factor,t1,t2)
          
        })
        output$pp_t_table_ratio = DT::renderDataTable({
          datatable(t(pp_t_test_ratio()),rownames = TRUE)
          })
        output$boxplot_pp_ratio = renderPlot({
          full_data = pFEV_lf_r
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          global_factor = input$global_factor
          p = boxplot_pp_ratio_function(full_data,global_factor,t1,t2)
          print(p)
          
        })
  
            #### Imputed####
            
            pp_t_test_ratio_i = reactive({
              full_data = i_pFEV_lf_r
              factor = input$global_factor
              t1 = input$anova_range[1]
              t2 = input$anova_range[2]
              df = pp_t_test_ratio_function(full_data,factor,t1,t2)
              
            })
            output$pp_t_table_ratio_i = DT::renderDataTable({
              datatable(t(pp_t_test_ratio_i()),rownames = T)
              })
            output$boxplot_pp_ratio_i = renderPlot({
              full_data = i_pFEV_lf_r
              t1 = input$anova_range[1]
              t2 = input$anova_range[2]
              factor = input$global_factor
              p = boxplot_pp_ratio_function(full_data,factor,t1,t2)
              print(p)
              
            })
      
      ### ZERO ###
      pp_t_test_zero = reactive({
        #df = data.frame(Factor = numeric(0))
        full_data = pFEV_lf_r
        factor = 'Status'
        factor = input$global_factor
        t1 = -6
        t2 = 6
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        df = pp_t_test_zero_function(full_data,factor,t1,t2)
        
      })
      output$pp_t_table_zero = DT::renderDataTable({
        datatable(t(pp_t_test_zero()),rownames = TRUE)
      })
      output$boxplot_pp_zero = renderPlot({
        full_data = pFEV_lf_r
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        factor = input$global_factor
        p = boxplot_pp_zero_function(full_data,factor,t1,t2)
        print(p)
      })
      
      # Imputed
      
      pp_t_test_zero_i = reactive({
        full_data = i_pFEV_lf_r
        factor = input$global_factor
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        df = pp_t_test_zero_function(full_data,factor,t1,t2)
        
      })
      output$pp_t_table_zero_i = DT::renderDataTable({
        datatable(t(pp_t_test_zero_i()),rownames = T)
      })
      output$boxplot_pp_zero_i = renderPlot({
        full_data = i_pFEV_lf_r
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        factor = input$global_factor
        p = boxplot_pp_zero_function(full_data,factor,t1,t2)
        print(p)
      })
  #output$anova_table = renderDataTable(df_a())
  


################# CHANGE STAT ############################
  
  ########### D1 ########################
      
    #### LM ANOVA ####
          df_lm_d1 = reactive({
            factor = input$global_factor
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            function_data=i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% cols,]
            df = lm_function(function_data,factor,cols)
            df
          })
          output$lm_table_d1 = renderDataTable(t(df_lm_d1()))
          
          output$boxplot_anova_all_factor_d1 = renderPlot({
            factor = input$global_factor
            full_data = i_pFEV_sm_d1_fl_r
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            full_data=i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% cols,]
            ggplot(full_data, aes(x = variable, y = value)) + 
              geom_boxplot(aes_string(col = factor)) +
              stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
              theme(axis.text.x = element_text(size=14, angle=90)) + 
              scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              
              ggtitle("Original Data")
          })
          
          output$boxplot_anova_before_factor_d1 = renderPlot({
            factor = input$global_factor
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            full_data=i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% before & i_pFEV_sm_d1_fl_r$variable %in% cols,] 
            ggplot(full_data, aes(x = variable, y = value)) + 
              geom_boxplot(aes_string(col = factor)) +
              stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
              theme(axis.text.x = element_text(size=14, angle=90)) + 
              scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              
              ggtitle("Pre Treatment")
          })
          output$boxplot_anova_after_factor_d1 = renderPlot({
            factor = input$global_factor
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            full_data=i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% after & i_pFEV_sm_d1_fl_r$variable %in% cols,] 
            ggplot(full_data, aes(x = variable, y = value)) + 
              geom_boxplot(aes_string(col = factor)) +
              stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
              theme(axis.text.x = element_text(size=14, angle=90)) + 
              scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              
              ggtitle("Post Treatment")
          })
    #### SLOPE ####
          df_lm_sample_d1 = reactive({
            df = i_pFEV_sm_d1_f_r
            factor = 'MRN'
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            function_data=i_pFEV_sm_d1_fl_r[i_pFEV_sm_d1_fl_r$variable %in% cols,]
            df = lm_sample_function(function_data,factor,cols,df)
            df
          })
          df_slope_d1 = reactive({
            factor = 'Status'
            factor = input$global_factor
            full_data = df_lm_sample_d1()
            df = slope_function(full_data,factor,cols)
            df
            
          })
          output$slope_table_d1 = renderDataTable(t(df_slope_d1()))
          output$slope_boxplot_d1 = renderPlot({
            #data = df_sample
            data = df_lm_sample_d1()
            slope_cols = c("slope_Pre","slope_Post")
            data_l = melt(data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = slope_cols)
            
            ggplot(data = data_l)+
              geom_boxplot(aes_string(col = 'variable',y='value',x = input$global_factor))
          })
    ### T test #####
          pp_t_test_d1 = reactive({
            full_data = i_pFEV_sm_d1_fl_r
            factor = input$global_factor
            t1 = input$anova_range[1]
            t2 = input$anova_range[2]
            df = pp_t_test_function(full_data,factor,t1,t2)
            df
          })
          output$pp_t_table_d1 = renderDataTable(t(pp_t_test_d1()))
          output$boxplot_pp_d1 = renderPlot({
            full_data = i_pFEV_sm_d1_fl_r
            t1 = input$anova_range[1]
            t2 = input$anova_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_function(full_data,t1,t2,global_factor)
            p
          })
          
          pp_t_test_ranges_d1 = reactive({
            full_data = i_pFEV_sm_d1_fl_r
            factor = input$global_factor
            pre1 = input$d1_pre_range[1]
            pre2 = input$d1_pre_range[2]
            post1 = input$d1_post_range[1]
            post2 = input$d1_post_range[2]
            df = pp_t_test_range_function(full_data,factor,pre1,pre1,post1,post2)
          })
          output$pp_t_table_ranges_d1 = renderDataTable(t(pp_t_test_ranges_d1()))
          output$boxplot_pp_ranges_d1 = renderPlot({
            full_data = i_pFEV_sm_d1_fl_r
            pre1 = input$d1_pre_range[1]
            pre2 = input$d1_pre_range[2]
            post1 = input$d1_post_range[1]
            post2 = input$d1_post_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor)
            print(p)
          })
  
  ########## D2 ######################
  df_lm_d2 = reactive({
    df = data.frame(Factor = numeric(), Status = numeric(0))
    df
    factor = 'MRN'
    factor = input$global_factor
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% cols,]
    dim(full_data)
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[3]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      y = as.numeric(data$value)
      x = as.numeric(as.character(data$variable))
      x
      y
      if(!all(is.na(y))){
        fit <- aov(y ~ x, data=data)
        fit
        summary(fit)
        a = anova(fit)
        a
        
        p = a$Pr[1]
        print(p)
        df[paste(factor,entry),'Factor'] = factor
        df[paste(factor,entry),'Status'] = entry
        df[paste(factor,entry),'ano_All'] = signif(p,3)
        str(data$variable)
        l = lm(y~x)
        summary(l)
        i = coef(l)['(Intercept)']
        m = coef(l)['x']
        df[paste(factor,entry),'Int_ALL'] = i
        df[paste(factor,entry),'slope_ALL'] = m
      }
    }
    df
    
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% before & i_pFEV_sm_d2_fl$variable %in% cols,]
    #full_data=pFEV_lf_r
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[2]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      y = as.numeric(data$value)
      x = as.numeric(as.character(data$variable))
      x
      y
      if(!all(is.na(y))){
        fit <- aov(y ~ x, data=data)
        fit
        summary(fit)
        a = anova(fit)
        a
        
        p = a$Pr[1]
        print(p)
        #df[paste(factor,entry),'Factor'] = factor
        #df[paste(factor,entry),'Status'] = entry
        df[paste(factor,entry),'ano_Pre'] = signif(p,3)
        str(data$variable)
        l = lm(y~x)
        summary(l)
        i = coef(l)['(Intercept)']
        m = coef(l)['x']
        df[paste(factor,entry),'Int_Pre'] = i
        df[paste(factor,entry),'slope_Pre'] = m
      }
    }
    
    
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% after & i_pFEV_sm_d2_fl$variable %in% cols,]
    #full_data=pFEV_lf_r
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[2]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      y = as.numeric(data$value)
      x = as.numeric(as.character(data$variable))
      x
      y
      if(!all(is.na(y))){
        fit <- aov(y ~ x, data=data)
        fit
        summary(fit)
        a = anova(fit)
        a
        
        p = a$Pr[1]
        print(p)
        #df[paste(factor,entry),'Factor'] = factor
        #df[paste(factor,entry),'Status'] = entry
        df[paste(factor,entry),'ano_Post'] = signif(p,3)
        str(data$variable)
        l = lm(y~x)
        summary(l)
        i = coef(l)['(Intercept)']
        m = coef(l)['x']
        df[paste(factor,entry),'Int_Post'] = i
        df[paste(factor,entry),'slope_Post'] = m
      }
    }
    df = df[order(df$Status),]
    df
  })
  output$lm_table_d2 = renderDataTable(df_lm_d2()[,c("Factor","Status","ano_All","ano_Pre","ano_Post")])
  
  output$boxplot_anova_all_factor_d2 = renderPlot({
    factor = input$global_factor
    full_data = i_pFEV_sm_d2_fl
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% cols,]
    ggplot(full_data, aes(x = variable, y = value)) + 
      geom_boxplot(aes_string(col = factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Original Data")
  })
  
  output$boxplot_anova_before_factor_d2 = renderPlot({
    factor = input$global_factor
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% before & i_pFEV_sm_d2_fl$variable %in% cols,] 
    ggplot(full_data, aes(x = variable, y = value)) + 
      geom_boxplot(aes_string(col = factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Pre Treatment")
  })
  output$boxplot_anova_after_factor_d2 = renderPlot({
    factor = input$global_factor
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% after & i_pFEV_sm_d2_fl$variable %in% cols,] 
    ggplot(full_data, aes(x = variable, y = value)) + 
      geom_boxplot(aes_string(col = factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Post Treatment")
  })
  
  df_lm_sample_d2 = reactive({
    #df = data.frame(Factor = numeric(), Status = numeric(0))
    df = pFEV_wf
    factor = 'MRN'
    #factor = input$global_factor
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% cols,]
    #df = full_data
    #rownames(df) = full_data$MRN
    dim(full_data)
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[3]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      y = as.numeric(data$value)
      x = as.numeric(as.character(data$variable))
      x
      y
      if(!all(is.na(y))){
        fit <- aov(y ~ x, data=data)
        fit
        summary(fit)
        a = anova(fit)
        a
        
        p = a$Pr[1]
        print(p)
        entry
        
        #df[entry,'Factor'] = factor
        #df[paste(factor,entry),'Status'] = entry
        df[entry,'ano_All'] = signif(p,3)
        str(data$variable)
        l = lm(y~x)
        summary(l)
        i = coef(l)['(Intercept)']
        m = coef(l)['x']
        df[entry,'Int_ALL'] = i
        df[entry,'slope_ALL'] = m
      }
    }
    df
    
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% before & i_pFEV_sm_d2_fl$variable %in% cols,]
    #full_data=i_pFEV_sm_d2_fl
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[2]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      y = as.numeric(data$value)
      x = as.numeric(as.character(data$variable))
      x
      y
      if(!all(is.na(y))){
        fit <- aov(y ~ x, data=data)
        fit
        summary(fit)
        a = anova(fit)
        a
        
        p = a$Pr[1]
        print(p)
        #df[paste(factor,entry),'Factor'] = factor
        #df[paste(factor,entry),'Status'] = entry
        df[entry,'ano_Pre'] = signif(p,3)
        str(data$variable)
        l = lm(y~x)
        summary(l)
        i = coef(l)['(Intercept)']
        m = coef(l)['x']
        df[entry,'Int_Pre'] = i
        df[entry,'slope_Pre'] = m
      }
    }
    
    
    full_data=i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$variable %in% after & i_pFEV_sm_d2_fl$variable %in% cols,]
    #full_data=i_pFEV_sm_d2_fl
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[2]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      y = as.numeric(data$value)
      x = as.numeric(as.character(data$variable))
      x
      y
      if(!all(is.na(y))){
        fit <- aov(y ~ x, data=data)
        fit
        summary(fit)
        a = anova(fit)
        a
        
        p = a$Pr[1]
        print(p)
        #df[paste(factor,entry),'Factor'] = factor
        #df[paste(factor,entry),'Status'] = entry
        df[entry,'ano_Post'] = signif(p,3)
        str(data$variable)
        l = lm(y~x)
        summary(l)
        i = coef(l)['(Intercept)']
        m = coef(l)['x']
        df[entry,'Int_Post'] = i
        df[entry,'slope_Post'] = m
      }
    }
    df = df[order(df$Status),]
    df
    df_sample = df
  })
  df_slope_d2 = reactive({
    df = data.frame(Factor = numeric())
    factor = 'Status'
    factor = input$global_factor
    #full_data = df_sample
    dim(full_data)
    full_data = df_lm_sample_d2()
    #mean_pre = mean()
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[1]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      data = full_data[full_data[,factor] == entry,]
      dim(data)
      mean_all = mean(data$slope_ALL,na.rm=T)
      mean_all
      mean_pre = mean(data$slope_Pre,na.rm=T)
      mean_pre
      mean_post = mean(data$slope_Post,na.rm=T)
      mean_post
      t_all = t.test(as.numeric(data$slope_Pre),as.numeric(data$slope_Post),na.rm=T)
      #t_all
      str(t_all)
      df[paste(factor,entry),'Factor'] = factor
      df[paste(factor,entry),'Status'] = entry
      df[paste(factor,entry),'mean_slope_All'] = mean_all
      df[paste(factor,entry),'mean_slope_Pre'] = mean_pre
      df[paste(factor,entry),'mean_slope_Post'] = mean_post
      df[paste(factor,entry),'T_p_value'] = t_all$p.value
      df[paste(factor,entry),'T_conf_1'] = t_all$conf.int[1]
      df[paste(factor,entry),'T_conf_2'] = t_all$conf.int[2]
    }
    df
    
  })
  output$slope_table_d2 = renderDataTable(df_slope_d2()[,c("Factor","Status","T_p_value")])
  output$slope_boxplot_d2 = renderPlot({
    #data = df_sample
    data = df_lm_sample_d2()
    slope_cols = c("slope_Pre","slope_Post")
    data_l = melt(data, id.vars = colnames(full_fac_0), measure.vars = slope_cols)
    
    ggplot(data = data_l)+
      geom_boxplot(aes_string(col = 'variable',y='value',x = input$global_factor))
  })
  
  pp_t_test_d2 = reactive({
    df = data.frame(Factor = numeric(0))
    full_data = i_pFEV_sm_d2_fl
    factor = 'Status'
    factor = input$global_factor
    t1 = -6
    t2 = 6
    t1 = input$anova_range[1]
    t2 = input$anova_range[2]
    col1 = factor(c(t1:-1))
    col1
    col2 = factor(c(1:t2))
    col2
    factor_levels = unique(full_data[,factor])
    factor_levels
    entry = factor_levels[2]
    #par(mfrow = c(length(factor_levels),1))
    for(entry in factor_levels){
      pre_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col1]
      pre_data
      post_data = full_data$value[full_data[,factor] == entry & full_data$variable %in% col2]
      post_data
      
      t = t.test(pre_data,post_data)
      print(t)
      p = t$p.value
      p
      df[paste(factor,entry),'Factor'] = factor
      df[paste(factor,entry),'Status'] = entry
      df[paste(factor,entry),'T_p_value'] = signif(p,3)
    }
    df
    df = df[order(df$Status),]
    
  })
  output$pp_t_table_d2 = renderDataTable(pp_t_test_d2())
  output$boxplot_pp_d2 = renderPlot({
    full_data = i_pFEV_sm_d2_fl
    t1 = -6
    t2 = 6
    t1 = input$anova_range[1]
    t2 = input$anova_range[2]
    col1 = factor(c(t1:-1))
    col1
    col2 = factor(c(1:t2))
    col2
    pre_data = full_data[full_data$variable %in% col1,]
    pre_data$treat = '0_pre'
    post_data = full_data[full_data$variable %in% col2,]
    post_data$treat = '1_post'
    
    pp_data = rbind(pre_data,post_data)
    
    ggplot(pp_data, aes_string(x = eval(input$global_factor),y = 'value',col='treat')) +
      geom_boxplot()
  })
  
})