



shinyServer(function(input, output) {
  

  

  ########## TEXT OUTPUTS ##################
  
  output$sessionInfo <- renderText({
    paste(capture.output(si$R.version$version.string), '"<br />"')
  })
  
  output$citation1 = renderText({
    citations = c(paste(si$R.version$version.string),'"<br />"',paste0('<span style="font-weight:bold">Rcpp</span>'),toBibtex(citation('base')))
    
    for(i in c(1:length(si$otherPkgs))){
      pack = si$otherPkgs[[i]]$Package
      if(pack != 'base'){
        citations = c(citations,'<br/>',paste0('<span style="font-weight:bold">',pack,'</span>'),toBibtex(citation(pack)))
      }
    }
    paste(citations,collapse='<br/>')
    #toBibtex(citation('dendextend'))
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
    output$pFEV_wf_r = renderDataTable(pFEV_wf_r())
    output$i_pFEV_wf_r = renderDataTable(i_pFEV_wf_r())
    output$i_pFEV_sm_lf_r = renderDataTable(i_pFEV_smf_r())
    output$i_pFEV_sm_d1_f_r = renderDataTable(i_pFEV_sm_d1_f_r())
    output$i_pFEV_sm_d2_f_r = renderDataTable(i_pFEV_sm_d2_f_r())
    output$cluster_data = renderDataTable(discrete_cluster_D()$data)
    output$cluster_data_d1 = renderDataTable(discrete_cluster_D_d1()$data)
    
    
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
                    #t_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% i_pFEV_lf_r()$MRN[1],]
                    i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% retained_patients()[my_i],]
                    sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% retained_patients()[my_i],]
                    #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
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
                    #t_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% i_pFEV_lf_r()$MRN[1],]
                    print(excluded_patients()[my_i])
                    #o_data = pFEV_lf_r()[pFEV_lf_r()$MRN %in% patient_list[my_i],]
                    #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% patient_list[my_i],]
                    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% patient_list[my_i],]
                    i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% excluded_patients()[my_i],]
                    sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% excluded_patients()[my_i],]
                    o_data = pFEV_lf[pFEV_lf$MRN %in% excluded_patients()[my_i],]
                    
                    #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
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
  
  
  pFEV_wf_r = reactive({
    o_data = pFEV_wf
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    m_data_d1 = discrete_cluster_D_d1()$data
    m_data_d1$MRN = rownames(m_data_d1)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data_d1$cluster[match(data$MRN,m_data_d1$MRN)]
    
    #data$cluster = discrete_cluster_D()$data$cluster
    #data$cluster_d1 = discrete_cluster_D_d1()$data$cluster
    data
    })
  pFEV_lf_r = reactive({
    w_data = pFEV_wf_r()
    data = melt(w_data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = colnames(pFEV_w))
    data
  })
  
  i_pFEV_wf_r = reactive({
    o_data = i_pFEV_wf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    
    data
  })
  i_pFEV_lf_r = reactive({
    o_data = i_pFEV_lf
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data
  })
  
  i_pFEV_smf_r = reactive({
    o_data = i_pFEV_smf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    
    data
  })
  i_pFEV_sm_lf_r = reactive({
    o_data = i_pFEV_sm_lf
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data
  })

  
  i_pFEV_sm_d1_f_r = reactive({
    o_data = i_pFEV_sm_d1_f
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    
    data
  })
  i_pFEV_sm_d1_fl_r = reactive({
    o_data = i_pFEV_sm_d1_fl
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data
  })
  
  i_pFEV_sm_d2_f_r = reactive({
    o_data = i_pFEV_sm_d2_f
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    
    data
  })
  i_pFEV_sm_d2_fl_r = reactive({
    o_data = i_pFEV_sm_d2_fl
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data
  })

         
  #i_pFEV_wf_r = reactive(i_pFEV_wf[i_pFEV_wf$MRN %in% retained_patients(),])
  #i_pFEV_lf_r = reactive(i_pFEV_lf[i_pFEV_lf$MRN %in% retained_patients(),])
  #i_pFEV_sm_lf_r = reactive(i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% retained_patients(),])
  #i_pFEV_sm_d1_f_r = reactive(i_pFEV_sm_d1_f[i_pFEV_sm_d1_f$MRN %in% retained_patients(),])
  #i_pFEV_sm_d1_fl_r = reactive(i_pFEV_sm_d1_fl[i_pFEV_sm_d1_fl$MRN %in% retained_patients(),])
  #_pFEV_sm_d2_f_r = reactive(i_pFEV_sm_d2_f[i_pFEV_sm_d2_f$MRN %in% retained_patients(),])
  #i_pFEV_sm_d2_fl_r = isolate(i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$MRN %in% retained_patients(),])
  

  ########### LINE PLOT ############
  
  output$line_pFEV = renderPlot({
    r_data = pFEV_lf_r()[pFEV_lf_r()$MRN %in% input$mrn_select,]
    
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
    i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
    #sm_data = i_pFEV_sm_lf_r()[i_pFEV_sm_lf_r()$MRN %in% input$mrn_select,]
    
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
    plot_data = pFEV_lf_r()
    ggplot(plot_data, aes(x = variable, y = value)) + 
      geom_vline(aes(xintercept = which(levels(plot_data$variable) %in% '0'))) +
      
      geom_boxplot(aes_string(col = input$global_factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ggtitle(paste('pFEV values for ',length(unique(plot_data$MRN))," Patients"))
  })
  output$boxplot_i_pFEV = renderPlot({
    plot_data = i_pFEV_lf_r()[i_pFEV_lf_r()$variable %in% pFEV_numeric_colnames_f,]
    summary_data = i_pFEV_lf_r()
    ggplot(NULL) +
      
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(aes(xintercept = which(levels(i_pFEV_lf_r()$variable) %in% '0'))) +
      
      geom_boxplot(data = plot_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle(paste('Imputed pFEV values for ',length(unique(plot_data$MRN))," Patients"))
  })
  
  output$boxplot_pFEV_mean = renderPlot({
    ggplot(pFEV_lf_r(), aes(x = variable, y = value)) + 
      geom_vline(aes(xintercept = which(levels(pFEV_lf_r()$variable) %in% '0'))) +
      
      #geom_boxplot(aes_string(col = input$global_factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Mean of Original Data")
  })
  
  output$boxplot_i_pFEV_mean = renderPlot({
    summary_data = i_pFEV_lf_r()
    ggplot(summary_data, aes(x = variable, y = value)) + 
      #geom_boxplot(aes_string(col = input$global_factor)) +
      geom_vline(aes(xintercept = which(levels(i_pFEV_lf_r()$variable) %in% '0'))) +
      
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle("Mean of Imputed Data")
  })
  
  ################ pFEV HEATMAP ########################
  


  

  
  
################# CHANGE #########################
    ############# line plot ###################
  
  i_mrn_sub_pFEV_sm = reactive({
    i_pFEV_sm_lf_r()[i_pFEV_sm_lf_r()$MRN %in% input$change_mrn_select,]
  })
  
  output$i_pFEV_sm_line = renderPlot({
    r_data = i_mrn_sub_pFEV_sm()
    r_data = i_pFEV_sm_lf_r()[i_pFEV_sm_lf_r()$MRN %in% input$change_mrn_select,]

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
    i_data = i_pFEV_sm_lf_r()
    ggplot(NULL) +
      
      stat_summary(data = i_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
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
    
    i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$MRN %in% input$change_mrn_select & i_pFEV_sm_d1_fl_r()$variable %in% cols,]
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
    summary_data =  i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
    i_data = i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% pFEV_numeric_colnames_f,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(data = i_data, aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
      geom_boxplot(data = i_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d1_fl_r()[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D1 boxplot")
  })
  
  output$boxplot_i_pFEV_sm_d1_mean = renderPlot({
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    summary_data =  i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #geom_vline(aes(xintercept = which(levels(i_pFEV_sm_lf$variable) %in% '0'))) +
      
      #geom_boxplot(data = i_pFEV_sm_d1_fl_r()[i_pFEV_lf$variable %in% pFEV_numeric_colnames_f & full_fac_0$pFEV_na > input$change_completeness,], aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d1_fl_r()[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
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
  

  
####################### D2 #########################
  
  i_mrn_sub_pFEV_sm_d2 = reactive({
    cols = c(-6:6)
    cols = factor(c(input$anova_range[1]:input$anova_range[2]))
    cols
    i_pFEV_sm_d2_fl_r()[i_pFEV_sm_d2_fl_r()$MRN %in% input$change_mrn_select & i_pFEV_sm_d2_fl_r()$variable %in% cols,]
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
    summary_data =  i_pFEV_sm_d2_fl_r()[i_pFEV_sm_d2_fl_r()$variable %in% cols,]
    i_data = i_pFEV_sm_d2_fl_r()[i_pFEV_sm_d2_fl_r()$variable %in% pFEV_numeric_colnames_f,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      geom_vline(data = i_data, aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
      geom_boxplot(data = i_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d2_fl_r()[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D2 boxplot")
  })
  
  output$boxplot_i_pFEV_sm_d2_mean = renderPlot({

    cols = factor(c(input$anova_range[1]:input$anova_range[2]))

    summary_data =  i_pFEV_sm_d2_fl_r()[i_pFEV_sm_d2_fl_r()$variable %in% cols,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #geom_vline(aes(xintercept = which(levels(i_pFEV_sm_lf$variable) %in% '0'))) +
      
      #geom_boxplot(data = i_pFEV_sm_d2_fl_r()[i_pFEV_lf$variable %in% pFEV_numeric_colnames_f & full_fac_0$pFEV_na > input$change_completeness,], aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d2_fl_r()[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
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
                function_data = pFEV_lf_r()
                df = lm_function(function_data,factor,cols)
                df = df[order(df$Status),]
              })
            output$lm_table = renderDataTable(t(df_lm()))
            
            output$boxplot_anova_all_factor = renderPlot({
              factor = input$global_factor
              full_data = pFEV_lf_r()
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=pFEV_lf_r()[pFEV_lf_r()$variable %in% cols,]
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
              full_data=pFEV_lf_r()[pFEV_lf_r()$variable %in% before & pFEV_lf_r()$variable %in% cols,] 
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
              full_data=pFEV_lf_r()[pFEV_lf_r()$variable %in% after & pFEV_lf_r()$variable %in% cols,] 
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
              #full_data=pFEV_lf_r()[pFEV_lf_r()$variable %in% cols,]
              function_data = i_pFEV_lf_r()
              df = lm_function(function_data,factor,cols)
              df = df[order(df$Status),]
            })
            output$lm_table_i = renderDataTable(t(df_lm_i()))
            
            output$boxplot_anova_all_factor_i = renderPlot({
              factor = input$global_factor
              function_data = i_pFEV_lf_r()
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
              function_data = i_pFEV_lf_r()
              
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=function_data[function_data$variable %in% before & pFEV_lf_r()$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Pre Treatment")
            })
            output$boxplot_anova_after_factor_i = renderPlot({
              function_data = i_pFEV_lf_r()
              
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$anova_range[1]:input$anova_range[2]))
              cols
              full_data=function_data[function_data$variable %in% after & pFEV_lf_r()$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Post Treatment")
            })

        ######## LM SAMPLE ######

          df_lm_sample = reactive({
            function_data = pFEV_lf_r()
            df = pFEV_wf_r()
            factor = 'MRN'
        
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            df = lm_sample_function(function_data,factor,cols,df)
            
          })
          output$df_lm_table = renderDataTable((df_lm_sample()))

          df_slope= reactive({
            factor = 'Status'
            factor = input$global_factor
            full_data = df_lm_sample()
            df = slope_function(full_data,factor,cols)
            df = df[order(df$Status),]
            
          })
          
          slope_boxplot_data = reactive(slope_boxplot_data_function(df_lm_sample(),df_slope(),input$global_factor))
          output$slope_boxplot = renderPlot(slope_boxplot_function(slope_boxplot_data(),input$global_factor))
          #output$slope_table_i = renderDataTable((slope_boxplot_data_i()))
          output$slope_table = renderDataTable(t(df_slope()))
          
          # output$slope_table = renderDataTable({
          #   df = df_slope()
          #   t(df)
          #   })
          
          #output$slope_table = renderDataTable(df_slope()[,c("Factor","Status","T_p_value")])
          # output$slope_boxplot = renderPlot({
          #   #data = df_sample
          #   data = df_lm_sample()
          #   slope_cols = c("slope_Pre","slope_Post")
          #   data_l = melt(data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = slope_cols)
          #   
          #   ggplot(data = data_l)+
          #     geom_boxplot(aes_string(col = 'variable',y='value',x = input$global_factor))
          # })
          
          # imputed
          
          df_lm_sample_i = reactive({
            function_data = i_pFEV_lf_r()
            df = i_pFEV_wf_r()
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
            df = df[order(df$Status),]
          })
          
          slope_boxplot_data_i = reactive(slope_boxplot_data_function(df_lm_sample_i(),df_slope_i(),input$global_factor))
          output$slope_boxplot_i = renderPlot(slope_boxplot_function(slope_boxplot_data_i(),input$global_factor))
          #output$slope_table_i = renderDataTable((slope_boxplot_data_i()))
          output$slope_table_i = renderDataTable(t(df_slope_i()))
          
 
        
        ######### T test ##############

  
          pp_t_test = reactive({
              full_data = pFEV_lf_r()
              factor = input$global_factor
              t1 = input$anova_range[1]
              t2 = input$anova_range[2]
              df = pp_t_test_function(full_data,factor,t1,t2)
              df = df[order(df$Status),]
            })
          output$pp_t_table = renderDataTable(t(pp_t_test()))
          output$boxplot_pp = renderPlot({
            full_data = pFEV_lf_r()
            t1 = input$anova_range[1]
            t2 = input$anova_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_function(full_data,t1,t2,global_factor)
            print(p)
          })
        
              ### Imputed ####
              
                  pp_t_test_i = reactive({
                    full_data = i_pFEV_lf_r()
                    factor = input$global_factor
                    t1 = input$anova_range[1]
                    t2 = input$anova_range[2]
                    df = pp_t_test_function(full_data,factor,t1,t2)
                  })
                  
                  output$pp_t_table_i = renderDataTable(t(pp_t_test_i()))
                  
                  output$boxplot_pp_i = renderPlot({
                    full_data = i_pFEV_lf_r()
                    t1 = input$anova_range[1]
                    t2 = input$anova_range[2]
                    global_factor = input$global_factor
                    p = boxplot_pp_function(full_data,t1,t2,global_factor)
                    print(p)
                  })
        
      
            ####t test by Range ####
                  pp_t_test_ranges = reactive({
                    full_data = pFEV_lf_r()
                    factor = input$global_factor
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    df = pp_t_test_range_function(full_data,factor,pre1,pre2,post1,post2)
                    df = df[order(df$Status),]
                    })
                  output$pp_t_table_ranges = renderDataTable(t(pp_t_test_ranges()))
                  pp_ranges_data = reactive({
                    full_data = pFEV_lf_r()
                    df = pp_t_test_ranges()
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    global_factor = input$global_factor
                    pp_data = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor,df)
                    pp_data
                  })
                  
                  output$boxplot_pp_ranges = renderPlot({
                    t_boxplot_function(pp_ranges_data(),input$global_factor)
                  })
                  
                  #### Imputed ###
                  
                  pp_t_test_ranges_i = reactive({
                    full_data = i_pFEV_lf_r()
                    factor = input$global_factor
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    df = pp_t_test_range_function(full_data,factor,pre1,pre2,post1,post2)
                    df = df[order(df$Status),]
                    })
                  output$pp_t_table_ranges_i = renderDataTable(t(pp_t_test_ranges_i()))
                  pp_ranges_data_i = reactive({
    
                    full_data = i_pFEV_lf_r()
                    df = pp_t_test_ranges_i()
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    global_factor = input$global_factor
                    p = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor,df)
                    p
                    #print(p)
                  })
                  
                  output$boxplot_pp_ranges_i = renderPlot({
                    t_boxplot_function(pp_ranges_data_i(),input$global_factor)
                  })
                  
      ### Ratio  FULL #### 
        pp_t_test_ratio_full = reactive({
          full_data = pFEV_lf_r()
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df = df[order(df$Status),]
          df
          
        })
        output$pp_t_table_ratio_full = renderDataTable(t(pp_t_test_ratio_full()))
        output$boxplot_pp_ratio_full = renderPlot({
          full_data = pFEV_lf_r()
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
        
        pp_t_test_ratio_full_i = reactive({
          full_data = i_pFEV_lf_r()
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df = df[order(df$Status),]
          df
        })
        output$pp_t_table_ratio_full_i = renderDataTable(t(pp_t_test_ratio_full_i()))
        output$boxplot_pp_ratio_full_i = renderPlot({
          full_data = i_pFEV_lf_r()
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
  
      ### Ratio ####
        pp_t_test_ratio = reactive({
          full_data = pFEV_lf_r()
          factor = input$global_factor
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_function(full_data,factor,t1,t2)
          df_s = df[order(df$Status),]
          #View(df_s)
          df_s
          
          #print(df)
        })
        output$pp_t_table_ratio = DT::renderDataTable({
          datatable(t(pp_t_test_ratio()),rownames = TRUE)
          })
        boxplot_pp_ratio_data = reactive({
          full_data = pFEV_lf_r()
          df_s = pp_t_test_ratio()
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          global_factor = input$global_factor
          df = boxplot_pp_ratio_data_function(full_data,global_factor,t1,t2,df_s)
          #View(df)
          df
          
        })
        output$boxplot_pp_ratio = renderPlot({
          boxplot_pp_ratio_plot_function(boxplot_pp_ratio_data())
        })
            #### Imputed####
        pp_t_test_ratio_i = reactive({
          full_data = i_pFEV_lf_r()
          factor = input$global_factor
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          df = pp_t_test_ratio_function(full_data,factor,t1,t2)
          df_s = df[order(df$Status),]
          #View(df_s)
          df_s
          
          #print(df)
        })
        output$pp_t_table_ratio_i = DT::renderDataTable({
          datatable(t(pp_t_test_ratio_i()),rownames = TRUE)
        })
        boxplot_pp_ratio_data = reactive({
          full_data = i_pFEV_lf_r()
          df_s = pp_t_test_ratio_i()
          t1 = input$anova_range[1]
          t2 = input$anova_range[2]
          global_factor = input$global_factor
          df = boxplot_pp_ratio_data_function(full_data,global_factor,t1,t2,df_s)
          #View(df)
          df
          
        })
        output$boxplot_pp_ratio_i = renderPlot({
          boxplot_pp_ratio_plot_function(boxplot_pp_ratio_data())
        })
        
      
      ### ZERO ####
      pp_t_test_zero = reactive({
        #df = data.frame(Factor = numeric(0))
        full_data = pFEV_lf_r()
        factor = 'Status'
        factor = input$global_factor
        t1 = -6
        t2 = 6
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        df = pp_t_test_zero_function(full_data,factor,t1,t2)
        df_s = df[order(df$Status),]
        #View(df_s)
        df_s
        
      })
      output$pp_t_table_zero = DT::renderDataTable({
        datatable(t(pp_t_test_zero()),rownames = TRUE)
      })
      boxplot_pp_zero_data = reactive({
        full_data = pFEV_lf_r()
        df_s = pp_t_test_zero()
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        factor = input$global_factor
        df = boxplot_pp_zero_data_function(full_data,factor,t1,t2,df_s)
        #View(df)
        df
      })
      output$boxplot_pp_zero = renderPlot({
        boxplot_pp_zero_plot_function(boxplot_pp_zero_data())
      })
      
      # Imputed
      
      pp_t_test_zero_i = reactive({
        full_data = i_pFEV_lf_r()
        factor = input$global_factor
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        df = pp_t_test_zero_function(full_data,factor,t1,t2)
        df = df[order(df$Status),]
        
      })
      output$pp_t_table_zero_i = DT::renderDataTable({
        datatable(t(pp_t_test_zero_i()),rownames = T)
      })
      
      boxplot_pp_zero_data_i = reactive({
        full_data = i_pFEV_lf_r()
        df_s = pp_t_test_zero_i()
        t1 = input$anova_range[1]
        t2 = input$anova_range[2]
        factor = input$global_factor
        df = boxplot_pp_zero_data_function(full_data,factor,t1,t2,df_s)
        #View(df)
        df
      })
      output$boxplot_pp_zero = renderPlot({
        boxplot_pp_zero_plot_function(boxplot_pp_zero_data_i())
      })
      
      # output$boxplot_pp_zero_i = renderPlot({
      #   full_data = i_pFEV_lf_r()
      #   t1 = input$anova_range[1]
      #   t2 = input$anova_range[2]
      #   factor = input$global_factor
      #   p = boxplot_pp_zero_function(full_data,factor,t1,t2)
      #   print(p)
      # })
  #output$anova_table = renderDataTable(df_a())
  


################# CHANGE STAT ############################
  
  ########### D1 ########################
      
    #### LM ANOVA ####
          df_lm_d1 = reactive({
            factor = input$global_factor
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            function_data=i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
            df = lm_function(function_data,factor,cols)
            df
          })
          output$lm_table_d1 = renderDataTable(t(df_lm_d1()))
          
          output$boxplot_anova_all_factor_d1 = renderPlot({
            factor = input$global_factor
            full_data = i_pFEV_sm_d1_fl_r()
            cols = c(-6:6)
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            cols
            full_data=i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
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
            full_data=i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% before & i_pFEV_sm_d1_fl_r()$variable %in% cols,] 
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
            full_data=i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% after & i_pFEV_sm_d1_fl_r()$variable %in% cols,] 
            ggplot(full_data, aes(x = variable, y = value)) + 
              geom_boxplot(aes_string(col = factor)) +
              stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
              theme(axis.text.x = element_text(size=14, angle=90)) + 
              scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              
              ggtitle("Post Treatment")
          })
    #### SLOPE ####
          df_lm_sample_d1 = reactive({
            df = i_pFEV_sm_d1_f_r()
            factor = 'MRN'
            cols = factor(c(input$anova_range[1]:input$anova_range[2]))
            function_data=i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
            df = lm_sample_function(function_data,factor,cols,df)
            df
          })
          df_slope_d1 = reactive({
            factor = 'Status'
            factor = input$global_factor
            full_data = df_lm_sample_d1()
            df = slope_function(full_data,factor,cols)
            df = df[order(df$Status),]
            df
          })
          output$slope_table_d1 = renderDataTable(t(df_slope_d1()))
          output$slope_boxplot_d1 = renderPlot({
            #data = df_sample
            data = df_lm_sample_d1()
            df = df_slope_d1()
            data$significant = factor(df$significant[match(data[,input$global_factor],df$Status)])
            slope_cols = c("slope_Pre","slope_Post")
            data_l = melt(data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1','significant'), measure.vars = slope_cols)
            ggplot(data = data_l)+
              geom_boxplot(aes_string(col = 'variable',y='value',x = input$global_factor,fill = 'significant')) +
              #scale_fill_brewer(col = c('white','blanchedalmond'))
              scale_fill_manual(values = c("white", "blanchedalmond"))
          })
    ### T test #####
          pp_t_test_d1 = reactive({
            full_data = i_pFEV_sm_d1_fl_r()
            factor = input$global_factor
            t1 = input$anova_range[1]
            t2 = input$anova_range[2]
            df = pp_t_test_function(full_data,factor,t1,t2)
            df
          })
          output$pp_t_table_d1 = renderDataTable(t(pp_t_test_d1()))
          output$boxplot_pp_d1 = renderPlot({
            full_data = i_pFEV_sm_d1_fl_r()
            t1 = input$anova_range[1]
            t2 = input$anova_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_function(full_data,t1,t2,global_factor)
            p
          })
          
          pp_t_test_ranges_d1 = reactive({
            full_data = i_pFEV_sm_d1_fl_r()
            factor = input$global_factor
            pre1 = input$d1_pre_range[1]
            pre2 = input$d1_pre_range[2]
            post1 = input$d1_post_range[1]
            post2 = input$d1_post_range[2]
            df = pp_t_test_range_function(full_data,factor,pre1,pre2,post1,post2)
          })
          output$pp_t_table_ranges_d1 = renderDataTable(t(pp_t_test_ranges_d1()))
          # output$boxplot_pp_ranges_d1 = renderPlot({
          #   full_data = i_pFEV_sm_d1_fl_r()
          #   df = pp_t_test_ranges_d1()
          #   pre1 = input$d1_pre_range[1]
          #   pre2 = input$d1_pre_range[2]
          #   post1 = input$d1_post_range[1]
          #   post2 = input$d1_post_range[2]
          #   global_factor = input$global_factor
          #   p = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor,df)
          #   print(p)
          # })
          # 

          pp_ranges_data_d1 = reactive({
            
            full_data = i_pFEV_sm_d1_fl_r()
            df = pp_t_test_ranges_d1()
            pre1 = input$pre_range[1]
            pre2 = input$pre_range[2]
            post1 = input$post_range[1]
            post2 = input$post_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor,df)
            p
            #print(p)
          })
          
          output$boxplot_pp_ranges_d1 = renderPlot({
            t_boxplot_function(pp_ranges_data_d1(),input$global_factor)
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
    #full_data=pFEV_lf_r()
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
    #full_data=pFEV_lf_r()
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
  
  #### Cluster Analysis ####
  ##### CLUSTERING ###############
  
      #output$discrete_cluster_D_by_function = renderPrint({
      discrete_cluster_D = reactive({
        full_data = i_pFEV_wf
        cluster_data_list = clustering_function(full_data,retained_patients(),input$clutree_num,
                                                input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
                                                input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
        cluster_data_list
      })
  

  
  
        ### PLOT CLUSTERS ####
            output$D_text = renderPrint(str(discrete_cluster_D()$D,indent.str = '<br />'))
            
            #output$D = renderText(print(str(discrete_cluster_D()$D)))
            
            output$discrete_cluster_plot = renderPlot({
              D = discrete_cluster_D()$D
              #plot(D,cex.lab = 0.5)
              
              #ggdendrogram(D)
              #ggdend(D)
              d_num = input$clutree_num
              cols = (ggplotColours(d_num))
              par(lwd = 5)
              D %>% set("branches_k_color", value = cols, k = d_num) %>% 
                plot()
              
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
  
  
  
    ####### D1 Clustering ############
            discrete_cluster_D_d1 = reactive({
              full_data = i_pFEV_sm_d1_f
              cluster_data_list = clustering_function(full_data,retained_patients(),input$clutree_num,
                                                      input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
                                                      input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
              cluster_data_list
            })
  
 
            ### PLOT CLUSTERS ####
            
            output$discrete_cluster_plot_d1 = renderPlot({
              D = discrete_cluster_D_d1()$D
              #plot(D,cex.lab = 0.5)
              d_num = input$clutree_num
              cols = (ggplotColours(d_num))
              par(lwd = 5)
              D %>% set("branches_k_color", value = cols, k = d_num) %>% 
                plot()
            })
            
            output$D_d1_text = renderPrint(str(discrete_cluster_D_d1()$D,indent.str = '<br />'))
            
            output$discrete_x_table_d1 = renderDataTable({
              discrete_cluster_D_d1()$x_cluster
            })
            
            discrete_cutree_line_plots_d1 = reactive({
              data = discrete_cluster_D_d1()$data
              cols = colnames(full_fac_0)[!(colnames(full_fac_0) %in% colnames(data))]
              data = cbind(data,full_fac_0[rownames(data),])
              n_cols = eval(input$mix_clust_col_num)
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
            
            output$cluster_comparison = renderPlot({
              ggplot(NULL) + 
                stat_summary(data = pFEV_lf_r(), fun.y=mean,geom="line",lwd=4,aes(x = variable, y = value,group=cluster,col = cluster)) +
                stat_summary(data = pFEV_lf_r(), fun.y=mean,geom="line",lwd=2,aes(x = variable, y = value,group=cluster_d1,col= cluster_d1)) 
            })
        
    #### CLUSTER COMPOSITION TABLES #####
    
          cluster_analysis_total = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_total(df,'cluster')
          })
          output$cluster_analysis = renderDataTable(cluster_analysis_total())
          output$cluster_analyis_selected = renderDataTable({
            df = cluster_analysis_total()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })
          
          cluster_analysis_total_d1 = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_total(df,'cluster_d1')
            df_tc
          })
          output$cluster_analysis_d1 = renderDataTable(cluster_analysis_total_d1())
          output$cluster_analyis_selected_d1 = renderDataTable({
            df = cluster_analysis_total_d1()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })
          
          cluster_analysis_within = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_within(df,'cluster')
            df_tc
          })
          output$cluster_analysis_within_table = DT::renderDataTable({
            datatable(cluster_analysis_within(),rownames = FALSE)
            })
          output$cluster_analysis_within_table_selected = DT::renderDataTable({
            df = cluster_analysis_within()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
            datatable(df_selected,rownames = FALSE)
          })
          
          cluster_analysis_within_d1 = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_within(df,'cluster_d1')
            df_tc
          })
          output$cluster_analysis_within_d1_table = DT::renderDataTable({
            datatable(cluster_analysis_within_d1(),rownames = FALSE)
            })
          output$cluster_analysis_within_table_selected_d1 = DT::renderDataTable({
            df = cluster_analysis_within_d1()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
            datatable(df_selected,rownames = FALSE)
          })
          
          
  
})