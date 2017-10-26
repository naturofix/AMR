



shinyServer(function(input, output) {
  

  ########## TEXT OUTPUTS ##################
  
  output$missing_columns = renderPrint({
    paste('Columns in googlesheet not in app : ',paste(setdiff(colnames(clustering),all_columns),collapse = ', ')
)
  })
  
  output$additional_columns = renderPrint({
    paste('Columns in app not in the googlesheet : ',paste(all_columns[!all_columns %in% colnames(clustering)]),collapse = ', ')
  })

  output$duplicated_samples = renderPrint({
    paste('Duplicated samples : ',paste(unique(dups),collapse = ', '))
  })
  
  output$processed_data_str = renderPrint({
  })
  
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
  
  
  output$t1 = renderText(input$pre_range[1])
  output$t2 = renderText(input$post_range[2])
  output$t_range_text = renderText(paste('(',input$pre_range[1],' to ',input$pre_range[2],') vs (',input$post_range[1],' to ',input$post_range[2],')'))
  
  output$t_ratio_text = renderText(paste('log2(pFEV at zero point / pFEV at ',input$pre_range[1],') vs log2(pFEV at ',input$post_range[2],' / pFEV zero point)'))
  output$t_pFEV_text = renderText(paste('(',input$pre_range[1],' to -1 ) vs ( 1 to ',input$post_range[2],')'))
  output$t_pFEV_zero_text = renderText(paste('pre p value (',input$pre_range[1],' vs 0 ), post p value ( 0 vs ',input$post_range[2],')'))
  
  output$slope_pFEV_text = renderText(paste('(',input$pre_range[1],' to 0 ) vs ( 0 to ',input$post_range[2],')'))
  
  output$t_d1_text = renderText(paste('(',input$pre_range[1],' to ',input$pre_range[2],') vs (',input$post_range[1],' to ',input$post_range[2],')'))
  output$slope_d1_text = renderText(paste('(',input$pre_range[1],' to 0 ) vs ( 0 to ',input$post_range[2],')'))
  
  ########## DATA TABLES #####################
  
    output$clustering = renderDataTable(clustering)
    #output$clustering = renderDataTable(clustering)
    output$full_num = renderDataTable(processed_data)
    output$term_mapping = renderTable(term_mapping_df)
    
    
    output$pFEV_wf = renderDataTable(pFEV_wf)
    output$i_pFEV_wf = renderDataTable(i_pFEV_wf)
    output$i_pFEV_sm_lf= renderDataTable(i_pFEV_smf)
    output$i_pFEV_sm_d1_f = renderDataTable(i_pFEV_sm_d1_f)
    output$i_pFEV_sm_d2_f = renderDataTable(i_pFEV_sm_d2_f)

    output$pFEV_wf_r = renderDataTable(pFEV_wf_r())
    output$i_pFEV_wf_r = renderDataTable(i_pFEV_wf_r())
    output$i_pFEV_sm_lf_r = renderDataTable(i_pFEV_smf_r())
    output$i_pFEV_sm_d1_f_r = renderDataTable(i_pFEV_sm_d1_f_r())
    output$i_pFEV_sm_d2_f_r = renderDataTable(i_pFEV_sm_d2_f_r())
    output$cluster_data = renderDataTable(discrete_cluster_D()$data)
    output$cluster_data_d1 = renderDataTable(discrete_cluster_D_d1()$data)
    
    output$change_data = renderDataTable(change_data_w())
    
    
    output$na2z = renderDataTable(clust_0)

  #### PATIENT DATA ####
    #### _MISSINGNESS ############# 
    output$missmap_plot = renderPlot({
      par(mar=c(20,2,2,2),mgp=c(30,1,0))
      missmap(pFEV)
    },height = 800, width = 800 )

    output$pFEV_na_hist = renderPlot({
      hist(full_fac_0$pFEV_na,breaks = 20, main = 'Percentage of Completeness for pFEV data')
    })
  

  ######### _DISPLAY PATIENTS INDIVIDUALL AND CHOOSE WHICH ONES TO REMOVE ############
              output$auto_removed_patients = renderText(paste(excluded_patients_c,collapse = ', '))
              
              excluded_patients = reactive(patient_list[patient_list %in% input$remove_list])
            
              retained_patients = reactive({
                
                patient_list = patient_list[!(patient_list %in% input$remove_list)]
                if(input$status_radio == '2'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf$MonthsToDeath < as.numeric(input$post_range[2])])
                  #print('Dead MRN')
                  #print(length(MRN))
                  #print(MRN)
                  patient_list = patient_list[patient_list %in% MRN]
                }
                if(input$status_radio == '1'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf$MonthsToDeath > as.numeric(input$post_range[2]) | pFEV_wf$Status == '1'])
                  #print('Alive MRN')
                  #print(length(MRN))
                  #print(MRN)
                  patient_list = patient_list[patient_list %in% MRN]
                }
                
                patient_list
                
                })
            line_size = 2
            point_size = 4
            sm_size = 1
              
            ####### __retained ###########
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
            
            ########## __excluded ###############
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
                    #print(excluded_patients()[my_i])
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
  

  
  pFEV_wf_c = reactive({
    o_data = pFEV_wf
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$ratio_data)
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$per_data)
    #o_data = cbind(pFEV_wf, pFEV_sym())
    
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$ratio_data, pFEV_2_zero()$per_data, pFEV_sym())
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$ratio_data, pFEV_sym())
    
    data = o_data[o_data$MRN %in% retained_patients(),]
    #data = data[data$Status %in% status_r(),]
    ##View(data)
    data
  })  
              
  pFEV_wf_r = reactive({
    #print(retained_patients())
    o_data = pFEV_wf_c()
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    #data$m_data_d1 = discrete_cluster_D_d1()$data
    #data$m_data_d1$MRN = rownames(m_data_d1)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data_d1$cluster[match(data$MRN,m_data_d1$MRN)]
    
    #data$cluster = discrete_cluster_D()$data$cluster
    #data$cluster_d1 = discrete_cluster_D_d1()$data$cluster
    #data = data[data$Status %in% status_r(),]
    data = change_data_w()
    data
    
    })

  
  pFEV_lf_r = reactive({
    #w_data = pFEV_wf_r()
    #data = melt(w_data, measure.vars = colnames(pFEV_w))
    #data$time = as.numeric(as.character(data$variable))
    data = change_data_l()
    data
  })
  
  i_pFEV_wf_r = reactive({
    o_data = i_pFEV_wf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data = data[data$Status %in% status_r(),]
    data
  })
  
  i_pFEV_wf_c = reactive({
    o_data = i_pFEV_wf
    data = o_data[o_data$MRN %in% retained_patients(),]
    #data = data[data$Status %in% status_r(),]
    data
  })
  
  
  i_pFEV_lf_r = reactive({
    o_data = i_pFEV_lf
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    #data = data[data$Status %in% status_r(),]
    
    data
  })
  
  i_pFEV_smf_r = reactive({
    o_data = i_pFEV_smf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data = data[data$Status %in% status_r(),]
    
    data
  })
  i_pFEV_sm_lf_r = reactive({
    o_data = i_pFEV_sm_lf
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    #data = data[data$Status %in% status_r(),]
    
    data
  })

  
  i_pFEV_sm_d1_f_c = reactive({
    o_data = i_pFEV_sm_d1_f
    data = o_data[o_data$MRN %in% retained_patients(),]
    #data = data[data$Status %in% status_r(),]
    ##View(data)
    data
  })
  i_pFEV_sm_d1_f_r = reactive({
    o_data = i_pFEV_sm_d1_f
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data = data[data$Status %in% status_r(),]
    data
  })
  i_pFEV_sm_d1_fl_r = reactive({
    o_data = i_pFEV_sm_d1_fl
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    #data = data[data$Status %in% status_r(),]
    
    data
  })
  
  
  
  i_pFEV_sm_d1_f_c_ir = reactive({
    o_data = i_pFEV_sm_d1_f_c()
    m_data = pFEV_wf_c()
    d1_cols = p_cols[p_cols %in% colnames(o_data)]
    for(col_name in d1_cols){
      o_data[which(is.na(m_data[,col_name])),col_name] = NA
    }
    data = o_data
    #data = data[data$Status %in% status_r(),]
    data
    ##View(data)
    data
  })
  i_pFEV_sm_d1_f_c_ir_r = reactive({
    o_data = i_pFEV_sm_d1_f_c_ir()
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    #data$m_data_d1 = discrete_cluster_D_d1()$data
    #data$m_data_d1$MRN = rownames(m_data_d1)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data_d1$cluster[match(data$MRN,m_data_d1$MRN)]
    
    #data$cluster = discrete_cluster_D()$data$cluster
    #data$cluster_d1 = discrete_cluster_D_d1()$data$cluster
    #data = data[data$Status %in% status_r(),]
    
    data
    
  })
  i_pFEV_sm_d1_fl_c_ir_r = reactive({
    w_data = i_pFEV_sm_d1_f_c_ir_r()
    data = melt(w_data, measure.vars = colnames(pFEV_w)[-2])
    data$time = as.numeric(as.character(data$variable))
    data
  })
  
  
  
  
  i_pFEV_sm_d2_f_r = reactive({
    o_data = i_pFEV_sm_d2_f
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data = data[data$Status %in% status_r(),]
    
    data
  })
  i_pFEV_sm_d2_fl_r = reactive({
    o_data = i_pFEV_sm_d2_fl
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    #data = data[data$Status %in% status_r(),]
    
    data
  })
  
  comp_data = reactive({
    #print(input$data_select)
    data = pFEV_wf_c()
    
    if(input$data_select == 'pFEV'){
      data = pFEV_wf_c()
      
    }
    if(input$data_select == 'imputed'){
      data = i_pFEV_wf_c()
    }
    
    if(input$data_select == 'smoothed'){
      o_data = i_pFEV_smf
      #m_data = pFEV_wf_r()
      data = o_data[o_data$MRN %in% retained_patients(),]
    }
    
    if(input$data_select == 'd1'){
      data = i_pFEV_sm_d1_f_c()
    }
    if(input$data_select == 'd1_ri'){
      data = i_pFEV_sm_d1_f_c_ir()
    }
    
    if(input$data_select == 'd2'){
      o_data = i_pFEV_sm_d2_f
      #m_data = pFEV_wf_r()
      data = o_data[o_data$MRN %in% retained_patients(),]
    }
    ##View(data)
    
    data
    
  })
  
  
  select_cols = reactive({
    #print(input$data_select)
    #data = pFEV_wf_c()
    select_cols = colnames(pFEV_w)
    if(input$data_select == 'pFEV'){
      #data = pFEV_wf_c()
      select_cols = colnames(pFEV_w)
      
      
    }
    if(input$data_select == 'imputed'){
      #data = i_pFEV_wf_c()
      select_cols = colnames(pFEV_w)
      
    }
    if(input$data_select == 'd1'){
      #data = i_pFEV_sm_d1_f_c()
      select_cols = colnames(i_pFEV_sm_d1)
      
    }
    if(input$data_select == 'd2'){
      #data = i_pFEV_sm_d1_f_c()
      select_cols = colnames(i_pFEV_sm_d2)
      
    }
    if(input$data_select == 'd1_ri'){
      select_cols = colnames(i_pFEV_sm_d1)
      
    }
    ##View(data)
    
    select_cols
    
  })
  
  
  change_data = reactive({
    d1_data = i_pFEV_sm_d1_f_c()
    d1_data = d1_data[,colnames(d1_data) %in% pFEV_numeric_colnames_f]
    colnames(d1_data)
    colnames(d1_data) = paste0('D1_',colnames(d1_data))
    colnames(d1_data)
    data = cbind(comp_data(),d1_data,pFEV_2_zero()$ratio_data,pFEV_2_zero()$per_data, sym_data(),per_sym_data())
    #data$MRN = rownames(data)
    #data = cbind(comp_data(),pFEV_2_zero()$ratio_data)
    #data = cbind(comp_data(), sym_data())
    #print(colnames(data))
    ##View(data)
    data
    
  })
  
  change_data_w = reactive({
    #print(retained_patients())
    o_data = change_data()
    c_o_data = o_data
    ##View(c_o_data)
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    c_m_data = m_data
    ##View(c_m_data)
    #data$m_data_d1 = discrete_cluster_D_d1()$data
    #data$m_data_d1$MRN = rownames(m_data_d1)
    #c_m_data_d1 = m_data_d1
    ##View(c_m_data_d1)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data$cluster_d1 = m_data_d1$cluster[match(data$MRN,m_data_d1$MRN)]
    
    #data$cluster = discrete_cluster_D()$data$cluster
    #data$cluster_d1 = discrete_cluster_D_d1()$data$cluster
    #data = data[data$Status %in% status_r(),]
    data_l = data
    ##View(data_l)
    data
    
  })
  
  change_data_l = reactive({
    w_data = change_data_w()
    data = melt(w_data, measure.vars = select_cols())
    data$time = as.numeric(as.character(data$variable))
    data_w = data
    ##View(data_l)
    data
  })
  
  
  output$cluster_test = renderDataTable({
    test_list = list()
    #rownames(pFEV_wf_r())
    if(!identical(pFEV_wf_r()$MRN, i_pFEV_wf_r()$MRN)){
      test_list$i_pFEV_wf_r_row = FALSE
    }else{
      test_list$i_pFEV_wf_r_row = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster, i_pFEV_wf_r()$cluster)){
      test_list$i_pFEV_wf_r_cluster = FALSE
    }else{
      test_list$i_pFEV_wf_r_cluster = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster_d1, i_pFEV_wf_r()$cluster_d1)){
      test_list$i_pFEV_wf_r_cluster_d1 = FALSE
    }else{
      test_list$i_pFEV_wf_r_cluster_d1 = TRUE
    }
    if(!identical(pFEV_wf_r()$MRN, i_pFEV_smf_r()$MRN)){
      test_list$i_pFEV_smf_r_row = FALSE
    }else{
      test_list$i_pFEV_smf_r_row = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster, i_pFEV_smf_r()$cluster)){
      test_list$i_pFEV_smf_r_cluster = FALSE
    }else{
      test_list$i_pFEV_smf_r_cluster = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster_d1, i_pFEV_smf_r()$cluster_d1)){
      test_list$i_pFEV_smf_r_cluster_d1 = FALSE
    }else{
      test_list$i_pFEV_smf_r_cluster_d1 = TRUE
    }
    if(!identical(pFEV_wf_r()$MRN, i_pFEV_sm_d1_f_r()$MRN)){
      test_list$i_pFEV_sm_d1_f_r_row = FALSE
    }else{
      test_list$i_pFEV_sm_d1_f_r_row = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster, i_pFEV_sm_d1_f_r()$cluster)){
      test_list$i_pFEV_sm_d1_f_r_cluster = FALSE
    }else{
      test_list$i_pFEV_sm_d1_f_r_cluster = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster_d1, i_pFEV_sm_d1_f_r()$cluster_d1)){
      test_list$i_pFEV_sm_d1_f_r_cluster_d1 = FALSE
    }else{
      test_list$i_pFEV_sm_d1_f_r_cluster_d1 = TRUE
    }
    if(!identical(pFEV_wf_r()$MRN, i_pFEV_sm_d2_f_r()$MRN)){
      test_list$i_pFEV_sm_d2_f_r_row = FALSE
    }else{
      test_list$i_pFEV_sm_d2_f_r_row = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster, i_pFEV_sm_d2_f_r()$cluster)){
      test_list$i_pFEV_sm_d2_f_r_cluster = FALSE
    }else{
      test_list$i_pFEV_sm_d2_f_r_cluster = TRUE
    }
    if(!identical(pFEV_wf_r()$cluster_d1, i_pFEV_sm_d2_f_r()$cluster_d1)){
      test_list$i_pFEV_sm_d2_f_r_cluster_d1 = FALSE
    }else{
      test_list$i_pFEV_sm_d2_f_r_cluster_d1 = TRUE
    }
    paste(test_list)
    
    if(!identical(pFEV_lf_r()$MRN, i_pFEV_lf_r()$MRN)){
      test_list$i_pFEV_lf_r_row = FALSE
    }else{
      test_list$i_pFEV_lf_r_row = TRUE
    }
    if(!identical(pFEV_lf_r()$cluster, i_pFEV_lf_r()$cluster)){
      test_list$i_pFEV_lf_r_cluster = FALSE
    }else{
      test_list$i_pFEV_lf_r_cluster = TRUE
    }
    if(!identical(pFEV_lf_r()$cluster_d1, i_pFEV_lf_r()$cluster_d1)){
      test_list$i_pFEV_lf_r_cluster_d1 = FALSE
    }else{
      test_list$i_pFEV_lf_r_cluster_d1 = TRUE
    }
    if(!identical(pFEV_lf_r()$MRN, i_pFEV_sm_lf_r()$MRN)){
      test_list$i_pFEV_sm_lf_r_row = FALSE
    }else{
      test_list$i_pFEV_sm_lf_r_row = TRUE
    }
    if(!identical(pFEV_lf_r()$cluster, i_pFEV_sm_lf_r()$cluster)){
      test_list$i_pFEV_sm_lf_r_cluster = FALSE
    }else{
      test_list$i_pFEV_sm_lf_r_cluster = TRUE
    }
    if(!identical(pFEV_lf_r()$cluster_d1, i_pFEV_sm_lf_r()$cluster_d1)){
      test_list$i_pFEV_sm_lf_r_cluster_d1 = FALSE
    }else{
      test_list$i_pFEV_sm_lf_r_cluster_d1 = TRUE
    }

    if(identical(pFEV_lf_r()$MRN, i_pFEV_sm_d1_fl_r()$MRN)){
      test_list$i_pFEV_sm_d1_fl_r_row = TRUE
      if(!identical(pFEV_lf_r()$cluster, i_pFEV_sm_d1_fl_r()$cluster)){
        test_list$i_pFEV_sm_d1_fl_r_cluster = FALSE
      }else{
        test_list$i_pFEV_sm_d1_fl_r_cluster = TRUE
      }
      if(!identical(pFEV_lf_r()$cluster_d1, i_pFEV_sm_d1_fl_r()$cluster_d1)){
        test_list$i_pFEV_sm_d1_fl_r_cluster_d1 = FALSE
      }else{
        test_list$i_pFEV_sm_d1_fl_r_cluster_d1 = TRUE
      }
    }else{
      l1 = c()
      l2 = c()
      for(entry in unique(pFEV_lf_r()$MRN)){
        #print(entry)
        l1 = c(l1,unique(pFEV_lf_r()[,c('cluster')][pFEV_lf_r()$MRN == entry,]))
        l2 = c(l2,unique(i_pFEV_sm_d1_fl_r()[,c('cluster')][i_pFEV_sm_d1_fl_r()$MRN == entry,]))
      }
      #print(identical(l1,l2))
      #print('HIT')
      if(identical(l1,l2)){
        test_list$i_pFEV_sm_d1_fl_r_ENTRY_BY_ENTRY = TRUE
      }else{
        test_list$i_pFEV_sm_d1_fl_r_ENTRY_BY_ENTRY = FALSE
      }
      
    }
    if(identical(pFEV_lf_r()$MRN, i_pFEV_sm_d2_fl_r()$MRN)){
        test_list$i_pFEV_sm_d2_fl_r_row = TRUE
  
        if(!identical(pFEV_lf_r()$cluster, i_pFEV_sm_d2_fl_r()$cluster)){
          test_list$i_pFEV_sm_d2_fl_r_cluster = FALSE
        }else{
          test_list$i_pFEV_sm_d2_fl_r_cluster = TRUE
        }
        if(!identical(pFEV_lf_r()$cluster_d1, i_pFEV_sm_d2_fl_r()$cluster_d1)){
          test_list$i_pFEV_sm_d2_fl_r_cluster_d1 = FALSE
        }else{
          test_list$i_pFEV_sm_d2_fl_r_cluster_d1 = TRUE
        }
    }else{
      l1 = c()
      l2 = c()
      for(entry in unique(pFEV_lf_r()$MRN)){
        #print(entry)
        l1 = c(l1,unique(pFEV_lf_r()[,c('cluster')][pFEV_lf_r()$MRN == entry,]))
        l2 = c(l2,unique(i_pFEV_sm_d2_fl_r()[,c('cluster')][i_pFEV_sm_d2_fl_r()$MRN == entry,]))
      }
      #print(identical(l1,l2))
      #print('HIT')
      if(identical(l1,l2)){
        test_list$i_pFEV_sm_d2_fl_r_ENTRY_BY_ENTRY = TRUE
      }else{
        test_list$i_pFEV_sm_d2_fl_r_ENTRY_BY_ENTRY = FALSE
      }
    }
    
    if(!identical(pFEV_lf$MRN,i_pFEV_sm_d2_fl$Status)){
      #print('hit')
      test_list$error_test = FALSE
    }else{
      #print('no hit')
      test_list$error_test = TRUE
    }
    df = data.frame(df = names(test_list), test = paste(test_list))
    #print(df)
    df
    #paste(test_list,collapse = '<br>')
    #test_list
  })

         
  #i_pFEV_wf_r = reactive(i_pFEV_wf[i_pFEV_wf$MRN %in% retained_patients(),])
  #i_pFEV_lf_r = reactive(i_pFEV_lf[i_pFEV_lf$MRN %in% retained_patients(),])
  #i_pFEV_sm_lf_r = reactive(i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% retained_patients(),])
  #i_pFEV_sm_d1_f_r = reactive(i_pFEV_sm_d1_f[i_pFEV_sm_d1_f$MRN %in% retained_patients(),])
  #i_pFEV_sm_d1_fl_r = reactive(i_pFEV_sm_d1_fl[i_pFEV_sm_d1_fl$MRN %in% retained_patients(),])
  #_pFEV_sm_d2_f_r = reactive(i_pFEV_sm_d2_f[i_pFEV_sm_d2_f$MRN %in% retained_patients(),])
  #i_pFEV_sm_d2_fl_r = isolate(i_pFEV_sm_d2_fl[i_pFEV_sm_d2_fl$MRN %in% retained_patients(),])
  

  
  #### PLOTS ####
  ########### _LINE PLOT ############
  
  output$individual_patients = renderPlot({
    #mrn = c('5850700')
    #i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% mrn,]
    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% mrn,]
    
    i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% input$mrn_select_i,]
    sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN  %in% input$mrn_select_i,]

    if(dim(sm_data)[1] > 0){
      ggplot(NULL) +
        #geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
        geom_vline(xintercept = 0) +
        geom_line(data = i_data, aes(x = time, y = value, group = MRN),col='red',size = line_size)+
        geom_point(data = i_data, aes(x = time, y = data,group = MRN),col='blue',size = point_size) +
        #geom_line(data = )
        #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
        #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
        
        geom_line(data = sm_data, aes(x = time, y = value,group = MRN),col='green',size = sm_size)+
        
        theme(axis.text.x = element_text(size=8, angle=90)) +
        theme(legend.position="none") + 
        ggtitle('Imputed and Smoothed')
      
        #ggtitle(paste0(retained_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
    }else{
      ggplot(NULL) +
        geom_vline(x_intercept = 0) +
        #geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
        geom_line(data = i_data, aes(x = time, y = value, group = MRN),col='red',size = line_size)+
        geom_point(data = i_data, aes(x = time, y = data,group = MRN),col='blue',size = point_size) +
        #geom_line(data = )
        #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
        #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
        
        #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
        
        theme(axis.text.x = element_text(size=8, angle=90)) +
        theme(legend.position="none") + 
        ggtitle('Imputed and Smoothed')
    }
    
    
    
  })
  
  output$line_pFEV = renderPlot({
    r_data = pFEV_lf_r()
    title = paste(input$data_select,' values for ',length(unique(r_data$MRN))," Patients")
    line_plot_function(r_data,title,input)

  })
  
  output$smooth_line_pFEV = renderPlot({
    r_data = pFEV_lf_r()
    title = paste('SMOOTHED curve fitted to',input$data_select,' values for ',length(unique(r_data$MRN))," Patients")
    smooth_line_plot_function(r_data,title,input)

  })

  ################  _BOXPLOTS #######################
  output$boxplot_pFEV = renderPlot({
    full_data = pFEV_lf_r()
    title = paste(input$data_select,' values for ',length(unique(full_data$MRN))," Patients")
    boxplot_function(full_data,title,input)
  })
  

  
  output$boxplot_pFEV_cluster = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste(input$data_select,'values for ',length(unique(full_data$MRN))," Patients")
    cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
  
    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$boxplot_pFEV_cluster_full = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste(input$data_select,' values for ',length(unique(full_data$MRN))," Patients")
    cols = factor(c(input$pre_range[1]:input$post_range[2]))

    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$interaction_plot = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste(input$data_select,' values for ',length(unique(full_data$MRN))," Patients")
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    bias_col = 'Status'
    plot_data = full_data[full_data$variable %in% cols,]
    scale_cols = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]
    global_factor = input$global_factor
    interactors = input$interactors
    #print(interactors)
    interactor_list = c(global_factor,interactors)
    interactor_list = paste(c(global_factor,interactors),collapse = ', ')
    #print(interactor_list)
    #interactors = paste(c(global_factor,interactors),collapse=(','))
    #print(interactor_list)
    #plot_data = full_data[,cols]
    height_var = 600
    ggplot(plot_data, aes(x = variable, y = value)) + 
      #geom_boxplot(aes_string((col=paste0("interaction(", paste0(interactor_list, collapse =  ", "), ")"))))
      stat_summary(data = plot_data,fun.y=mean,geom="line",
                   aes_string(group=paste0("interaction(", paste0(interactor_list, collapse =  ", "), ")"),
                              col=paste0("interaction(", paste0(interactor_list,collapse =  ", "), ")"))) + 
      facet_grid(as.formula(paste(global_factor, '~','.')))

  },height = 1600)
  

  output$boxplot_pFEV_mean = renderPlot({
    full_data = pFEV_lf_r()
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    plot_data = full_data[full_data$variable %in% cols,]
    scale_cols = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]
    ggplot(plot_data, aes(x = time, y = value)) + 
      geom_vline(xintercept = 0) +
      
      #geom_boxplot(aes_string(col = input$global_factor)) +
      stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) + 
      #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
      
      ggtitle(paste("Mean of ",input$data_select,'Data'))
  })

  
  

  
############# STATS #####################
  
  
    ## _Linear Regression and ANOVA ####  
        ####### __LM ########
 
            df_lm = reactive({
                df = data.frame(Factor = numeric(), Status = numeric(0))

                factor = 'MRN'
                factor = input$global_factor
                cols = c(-6:6)
                cols = factor(c(input$pre_range[1]:input$post_range[2]))
                cols
                #full_data=pFEV_lf[pFEV_lf$variable %in% cols,]
                function_data = pFEV_lf_r()
                df = lm_function(function_data,factor,cols)
                df = df[order(df$Status),]
                col_rearrange_function(df,3)
                
              })
  
            output$df_lm_full = renderDataTable({
                df = data.frame(Factor = numeric(), Status = numeric(0))
                df_b = data.frame(term = numeric(0),df = numeric(0), sumsq = numeric(0),
                                  meansq = numeric(0), statistic = numeric(0), p.value = numeric(0),
                                  Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
                df = df_b
                factor = 'MRN'
                factor = input$global_factor
                for(factor in factor_list){
                  cols = c(-6:6)
                  cols = factor(c(input$pre_range[1]:input$post_range[2]))
                  cols
                  #full_data=pFEV_lf[pFEV_lf$variable %in% cols,]
                  function_data = pFEV_lf_r()
                  df_n = tryCatch(lm_function(function_data,factor,cols), error = function(e) e = df_b)
                  #print(df_n)
                  df = rbind(df,df_n)
                }
                #Veiw(df)
                df
                df = col_rearrange_function(df,3)
                significance_table_formatting_function(df,input$mtc)
                #df = df[order(df$Status),]
              })
              
              
            output$lm_table = renderDataTable({
              df = df_lm()
              significance_table_formatting_function(df,input$mtc)
              })
            
            output$boxplot_anova_all_factor = renderPlot({
              factor = input$global_factor
              full_data = pFEV_lf_r()
              cols = c(-6:6)
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
              cols
              full_data=pFEV_lf_r()[pFEV_lf_r()$variable %in% after & pFEV_lf_r()$variable %in% cols,] 
              ggplot(full_data, aes(x = variable, y = value)) + 
                geom_boxplot(aes_string(col = factor)) +
                stat_summary(fun.y=mean,geom="line",lwd=2,aes_string(group=factor,col = factor)) +
                theme(axis.text.x = element_text(size=14, angle=90)) + 
                scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
                
                ggtitle("Post Treatment")
            })

        ######## __LM SAMPLE ######

          df_lm_sample = reactive({
            function_data = pFEV_lf_r()
            df = pFEV_wf_r()
            factor = 'MRN'
        
            cols = c(-6:6)
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
            cols
            df = lm_sample_function(function_data,factor,cols,df)
            
          })
          output$df_lm_table = renderDataTable({
            df = df_lm_sample()
            df = df[,c(1,(grep('cluster',colnames(df))+1):length(colnames(df)))]
            df
            table_formatting_function(df)
            })

          df_slope= reactive({
            factor = 'Status'
            factor = input$global_factor
            full_data = df_lm_sample()
            df = slope_function_tidy(full_data,factor,input)
            #df = df[order(df$Status),]
            df = col_rearrange_function(df,3)
            
            df
          })
          
          output$df_slope_full = renderDataTable({
            factor = 'Status'
            factor = input$global_factor
            
            df_b = data.frame(estimate = numeric(0),
                              statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                              conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                              alternative = numeric(0),
                              Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
            df = df_b
            for(factor in factor_list){
              full_data = df_lm_sample()
              df_n = slope_function_tidy(full_data,factor,input)
              df = rbind(df,df_n)
            }
              #df = df[order(df$Status),]
            df
            df = col_rearrange_function(df,3)
            df
            significance_table_formatting_function(df,input$mtc)
            
          })
          
          output$slope_fit_pFEV_pre_plot = renderPlot({
            data = pFEV_lf_r()
            cols = factor(c(input$pre_range[1]:input$pre_range[2]))
            slope_fit_plot_function(data,cols,'Pre Treatment',input) 
          })
          output$slope_fit_pFEV_post_plot = renderPlot({
            data = pFEV_lf_r()
            cols = factor(c(input$post_range[1]:input$post_range[2]))
            slope_fit_plot_function(data,cols,'Post Treatment',input) 
          })
          
          slope_boxplot_data = reactive(slope_boxplot_data_function(df_lm_sample(),df_slope(),input$global_factor))
          
          
          output$slope_boxplot = renderPlot(slope_boxplot_function(slope_boxplot_data(),input$global_factor))
          output$slope_table = renderDataTable({
            df = df_slope()
            significance_table_formatting_function(df,input$mtc)})

        
        ######### _T test ##############

  
          pp_t_test = reactive({
              full_data = pFEV_lf_r()
              factor = input$global_factor
              t1 = input$pre_range[1]
              t2 = input$post_range[2]
              df = pp_t_test_function(full_data,factor,t1,t2)
              df = df[order(df$Status),]
            })
          output$pp_t_table = renderDataTable(t(pp_t_test()))
          output$boxplot_pp = renderPlot({
            full_data = pFEV_lf_r()
            t1 = input$pre_range[1]
            t2 = input$post_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_function(full_data,t1,t2,global_factor)
            print(p)
          })
        

      
            ####__t_test by Range ####
                  pp_t_test_ranges = reactive({
                    full_data = pFEV_lf_r()
                    factor = input$global_factor
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    df = pp_t_test_range_function(full_data,factor,pre1,pre2,post1,post2,input)
                    df = col_rearrange_function(df,3)
                    
                    #df = df[order(df$Status),]
                    #View(df)
                    df
                    })
                  output$pp_t_table_ranges = renderDataTable({
                    df = pp_t_test_ranges()
                    significance_table_formatting_function(df,input$mtc)
                    })
                  
                  output$pp_t_test_ranges_full = renderDataTable({
                    full_data = pFEV_lf_r()
                    factor = input$global_factor
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    df_b = data.frame(estimate = numeric(0),
                                      statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                                      conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                                      alternative = numeric(0),
                                      Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
                    df = df_b
                    for(factor in factor_list){
                      df_n = pp_t_test_range_function(full_data,factor,pre1,pre2,post1,post2,input)
                      df = rbind(df,df_n)
                      #df = df[order(df$Status),]
                      #View(df)
                      #df
                    }
                    df
                    df = col_rearrange_function(df,3)
                    significance_table_formatting_function(df,input$mtc)
                  })
                  
                  pp_ranges_data = reactive({
                    full_data = pFEV_lf_r()
                    df = pp_t_test_ranges()
                    pre1 = input$pre_range[1]
                    pre2 = input$pre_range[2]
                    post1 = input$post_range[1]
                    post2 = input$post_range[2]
                    global_factor = input$global_factor
                    pp_data = boxplot_pp_ranges_function(full_data,pre1,pre2,post1,post2,global_factor)
                    pp_data
                    print(colnames(pp_data))
                    #View(pp_data)
                    pp_data
                  })
                  
                  output$boxplot_pp_ranges = renderPlot({
                    t_boxplot_function(pp_ranges_data(),input$global_factor)
                  })
 
                  
      ### __Ratio  FULL #### 
        pp_t_test_ratio_full = reactive({
          full_data = pFEV_lf_r()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df = df[order(df$Status),]
          df
          
        })
        output$pp_t_table_ratio_full = renderDataTable({
          df = pp_t_test_ratio_full()
          significance_table_formatting_function(df,input$mtc)
          })
        output$boxplot_pp_ratio_full = renderPlot({
          full_data = pFEV_lf_r()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
 
      ### __Percentage ####
        output$percentage_change_t_test_full = renderDataTable({
          full_data = change_data_w()
          global_factor = input$global_factor
          cols_num = comp_colnames
          #cols_num = cols_num[as.numeric(cols_num) >= input$pre_range & as.numeric(cols_num) <= input$post_range]
          prefix = 'per2zero_'
          prefix = input$vs_zero_prefix
          
          #cols_num = cols_num[cols_num %in% colnames(full_data)]
          selected_columns = paste0(prefix,cols_num)
          selected_columns = selected_columns[selected_columns %in% colnames(full_data)]
          selected_w = melt(full_data, measure.vars = selected_columns)
          selected_w
          
          df_b = data.frame(estimate = numeric(0), estimate1  = numeric(0), estimate2 = numeric(0),
                            statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                            conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                            alternative = numeric(0),
                            Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
          df = df_b
          cols = unique(abs(as.numeric(cols_num)))
          for(i in cols){
            #print(paste(factor(-i)))
            #print(full_data[,factor(-i)])
            #raw_data[,'0'] = full_data[,'0']
            #raw_data[,paste(factor(-i))] = full_data[,paste(factor(-i))]
            
            #raw_data[,paste(factor(i))] = full_data[,paste(factor(i))]
            
            col1 = paste0(prefix,i)
            col2 = paste0(prefix,-i)
            #raw_data[,col1] = full_data[,col1]
            #raw_data[,col2] = full_data[,col1]
            #View(raw_data)
            #output$percentage_df = renderDataTable(raw_data)
            for(global_factor in factor_list){
              factor_levels = unique(full_data[,global_factor])
              for(entry in factor_levels){
                pre_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$variable %in% col1]
                post_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$variable %in% col2]
                #df1 = data.frame(pre = pre_data,post = post_data)
                #df1$Factor = global_factor
                #df1$Status = entry
                #df1$time = i
                #plot_df = rbind(plot_df,df1)
                
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
          }
          df = col_rearrange_function(df,3)
          df
          significance_table_formatting_function(df,input$mtc)
          
        })
        
        percentage_change_t_test = reactive({
          full_data = change_data_w()

          raw_data = data.frame(MRN = full_data$MRN)
         
          #print(raw_data$MRN)
          global_factor = input$global_factor
          prefix = 'per2zero_'
          prefix = input$vs_zero_prefix
          
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          cols_num = c(input$pre_range[1],input$post_range[2])
          cols_num = cols_num[!cols_num == 0]
          cols_num = cols_num[cols_num %in% colnames(full_data)]
          selected_columns = paste0(prefix,cols_num)
         
          selected_data = full_data[,selected_columns]
          selected_data
         
          selected_w = melt(full_data, measure.vars = selected_columns)
          selected_w
        
          df_b = data.frame(estimate = numeric(0), estimate1  = numeric(0), estimate2 = numeric(0),
                            statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                            conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                            alternative = numeric(0),
                            Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
          df = df_b
          plot_df = data.frame(pre = numeric(0),post = numeric(0),Factor = numeric(0), Status = numeric(0))
          
          cols = unique(abs(cols_num))
         #View(full_data)
          i = 1
          #for(i in cols){
           #print(paste(factor(t1)))
           #print(full_data[,factor(t2)])
            
            raw_data[,paste(factor(t1))] = full_data[,paste(factor(t1))]
            raw_data[,'0'] = full_data[,'0']
            raw_data[,paste(factor(t2))] = full_data[,paste(factor(t2))]
            
            col1 = paste0(prefix,t1)
            col2 = paste0(prefix,t2)
            raw_data[,col1] = full_data[,col1]
            raw_data[,col2] = full_data[,col2]
            #View(raw_data)
            
            factor_levels = unique(full_data[,global_factor])
            for(entry in factor_levels){
              pre_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$variable %in% col1]
              post_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$variable %in% col2]
              df1 = data.frame(pre = pre_data,post = post_data)
              df1$Factor = global_factor
              df1$Status = entry
              #df1$time = i
              plot_df = rbind(plot_df,df1)
              
              #print(pre_data)
              #print(post_data)
              df_n = tryCatch(tidy(t.test(pre_data,post_data)), error = function(e) e = df_b)
              if(dim(df_n)[1] > 0){
                df_n$Factor = global_factor
                df_n$Status = entry
                df_n$comparison = paste(t1,'vs',t2)
                df = rbind(df,df_n)
                }
              
              }
          #}
          df = col_rearrange_function(df,3)

         #View(plot_df)

          list(selected_w = selected_w, df = df, plot_df =  plot_df, raw_data = raw_data)
        })
        
   
        output$percentage_df = renderDataTable({
          raw_data = percentage_change_t_test()$raw_data
          table_formatting_function(raw_data)
          })
        
        output$percentage_boxplot = renderPlot({
          plot_df = percentage_change_t_test()$plot_df
          global_factor = input$global_factor
          df_m = melt(plot_df, measure.vars = c('pre','post'))
         #View(df_m)
          ggplot(df_m, aes(x = variable,y = value,col = Status)) +
            facet_grid(. ~ Status) +
            labs(col = global_factor) + 
            geom_boxplot()
        })
        
        output$percentage_table = renderDataTable({
          df = percentage_change_t_test()$df
          significance_table_formatting_function(df,input$mtc)
          #df
          
          })
      ### __Ratio ####
        pp_t_test_ratio = reactive({
          full_data = pFEV_lf_r()
          factor = input$global_factor
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          df = pp_t_test_ratio_function(full_data,factor,t1,t2)
          df_s = df[order(df$Status),]
          ##View(df_s)
          df_s
          
          #print(df)
        })
        
        
        
        output$pp_t_table_ratio = DT::renderDataTable({
          datatable(t(pp_t_test_ratio()),rownames = TRUE)
          })
        boxplot_pp_ratio_data = reactive({
          full_data = pFEV_lf_r()
          df_s = pp_t_test_ratio()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          global_factor = input$global_factor
          df = boxplot_pp_ratio_data_function(full_data,global_factor,t1,t2,df_s)
          ##View(df)
          df
          
        })
        output$boxplot_pp_ratio = renderPlot({
          title = paste0('T test of log2( 0/',input$pre_range[1],' )  vs log2( ',input$post_range[2],'/0 )')
          
          boxplot_pp_ratio_plot_function(boxplot_pp_ratio_data(),input$global_factor,title)
        })
            ### ___for clustering ####
        pp_t_test_ratio_cluster = reactive({
          full_data = pFEV_lf_r()
          plot_data = full_data
          ##View(plot_data)
          factor = 'cluster'
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          df = pp_t_test_ratio_function(full_data,factor,t1,t2)
          df_s = df[order(df$Status),]
          ##View(df_s)
          df_s
          
          #print(df)
        })
        boxplot_pp_ratio_data_cluster = reactive({
          full_data = pFEV_lf_r()
          df_s = pp_t_test_ratio_cluster()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          global_factor = 'cluster'
          df = boxplot_pp_ratio_data_function(full_data,global_factor,t1,t2,df_s)
          ##View(df)
          df
          
        })
        output$boxplot_pp_ratio_cluster = renderPlot({
          title = paste0('T test of log2( 0/',input$pre_range[1],' )  vs log2( ',input$post_range[2],'/0 )')
          
          boxplot_pp_ratio_plot_function(boxplot_pp_ratio_data_cluster(),'cluster',title)
        })
        
        boxplot_pp_ratio_data = reactive({
          full_data = i_pFEV_lf_r()
          df_s = pp_t_test_ratio_i()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          global_factor = input$global_factor
          df = boxplot_pp_ratio_data_function(full_data,global_factor,t1,t2,df_s)
          ##View(df)
          df
          
        })
        
      
      ### __ZERO ####
      pp_t_test_zero = reactive({
        #df = data.frame(Factor = numeric(0))
        full_data = pFEV_lf_r()
        factor = 'Status'
        factor = input$global_factor
        t1 = -6
        t2 = 6
        t1 = input$pre_range[1]
        t2 = input$post_range[2]
        df = pp_t_test_zero_function(full_data,factor,t1,t2)
        df = col_rearrange_function(df,3)
        
        #df_s = df[order(df$Status),]
        ##View(df_s)
        df
        
      })
        
        output$pp_t_test_zero_full = renderDataTable({
          #df = data.frame(Factor = numeric(0))
          
          df_b = data.frame(estimate = numeric(0),
                            statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                            conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                            alternative = numeric(0),
                            Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
          df = df_b
          full_data = pFEV_lf_r()
          factor = 'Status'
          factor = input$global_factor
          t1 = -6
          t2 = 6
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          for(factor in factor_list){
            df_n = pp_t_test_zero_function(full_data,factor,t1,t2)
            df = rbind(df,df_n)
          }
          #df_s = df[order(df$Status),]
          ##View(df_s)
          #df_s
          df = col_rearrange_function(df,3)
          significance_table_formatting_function(df,input$mtc)
        })
      output$pp_t_table_zero = renderDataTable({
        df = pp_t_test_zero()
        significance_table_formatting_function(df,input$mtc)
      })
      boxplot_pp_zero_data = reactive({
        full_data = pFEV_lf_r()
        #df_s = pp_t_test_zero()
        t1 = input$pre_range[1]
        t2 = input$post_range[2]
        factor = input$global_factor
        df = boxplot_pp_zero_data_function(full_data,factor,t1,t2)
        ##View(df)
        df
      })
      output$boxplot_pp_zero = renderPlot({
        boxplot_pp_zero_plot_function(boxplot_pp_zero_data())
      })
      
      ##### _VERTIVAL T TESTS ####
      horizontal_stats = reactive({
        
        #global_factor = 'HLAType'
        pre_cols = factor(c(-6:0))
        post_cols = factor(c(0:6))
        full_data = pFEV_lf_r()
        
        global_factor = 'Status'
        global_factor = input$global_factor
        pre_cols = paste(factor(c(input$pre_range[1]:input$pre_range[2])))
        post_cols = paste(factor(c(input$post_range[1]:input$post_range[2])))
        
        df_output = horizontal_stats_function(full_data,pre_cols,post_cols,global_factor,input)
        
        df_output
        
        
      })
      output$horizontal_t_test = renderDataTable({
        df = horizontal_stats()$t_df
        significance_table_formatting_function(df,input$mtc)
      })
      output$horizontal_anova = renderDataTable({
        df = horizontal_stats()$anova_df
        significance_table_formatting_function(df,input$mtc)
      })
      output$hor_box_pre = renderPlot({
        data = horizontal_stats()$pre_data
        ggplot(data, aes_string(x = input$global_factor,y = 'value', col = input$global_factor)) + 
          geom_boxplot()
      })
      output$hor_box_post = renderPlot({
        data = horizontal_stats()$post_data
        ggplot(data, aes_string(x = input$global_factor,y = 'value', col = input$global_factor)) + 
          geom_boxplot()
      })
      
      horizontal_stats_d1 = reactive({
        
        #global_factor = 'HLAType'
        pre_cols = factor(c(-6:0))
        post_cols = factor(c(0:6))
        full_data = i_pFEV_sm_d1_fl_r()
        
        global_factor = 'Status'
        global_factor = input$global_factor
        pre_cols = factor(c(input$pre_range[1]:input$pre_range[2]))
        post_cols = factor(c(input$post_range[1]:input$post_range[2]))
        df_output = horizontal_stats_function(full_data,pre_cols,post_cols,global_factor,input)
        
        
        
      })
      output$horizontal_t_test_d1 = renderDataTable({
        df = horizontal_stats_d1()$t_df
        significance_table_formatting_function(df,input$mtc)
      })
      output$horizontal_anova_d1 = renderDataTable({
        df = horizontal_stats_d1()$anova_df
        significance_table_formatting_function(df,input$mtc)
      })
      output$hor_box_pre_d1 = renderPlot({
        data = horizontal_stats_d1()$pre_data
        ggplot(data, aes_string(x = input$global_factor,y = 'value', col = input$global_factor)) + 
          geom_boxplot()
      })
      output$hor_box_post_d1 = renderPlot({
        data = horizontal_stats_d1()$post_data
        ggplot(data, aes_string(x = input$global_factor,y = 'value', col = input$global_factor)) + 
          geom_boxplot()
      })
      
      #output$test_text_1 = renderPrint({
      horizontal_stats_full = reactive({
        #print('test')
        #global_factor = 'HLAType'
        pre_cols = factor(c(-6:0))
        post_cols = factor(c(0:6))
        full_data = pFEV_lf_r()
        #print(dim(full_data))
        global_factor = 'Status'
        global_factor = input$global_factor
        pre_cols = paste(factor(c(input$pre_range[1]:input$pre_range[2])))
        post_cols = paste(factor(c(input$post_range[1]:input$post_range[2])))
        #print(pre_cols)
        #print(post_cols)
        global_factor_list = discrete_columns_4_comparison
        #print(global_factor_list[1])
        df_output = horizontal_stats_function(full_data,pre_cols,post_cols,global_factor_list[1],input)
        # #output = list(t_df = t_df_b, anova_df = anova_df, pre_data = pre_data, post_data = post_data)
        #print(df_output)
        t_df = df_output$t_df
        anova_df = df_output$anova_df
        for(global_factor in discrete_columns_4_comparison[-1]){
          #print(global_factor)
          df_output = horizontal_stats_function(full_data,pre_cols,post_cols,global_factor,input)
          if(dim(df_output$t_df)[1] > 0){
            t_df = rbind(t_df,df_output$t_df)
          }
          if(dim(df_output$t_df)[1] > 0){
            anova_df = rbind(anova_df,df_output$anova_df)
          }
        }
        
        list(t_df = t_df,anova_df = anova_df)
        
        
      })
      
      output$horizontal_t_test_full = renderDataTable({
        df = horizontal_stats_full()$t_df
        significance_table_formatting_function(df,input$mtc)
      })
      output$horizontal_anova_full = renderDataTable({
        df = horizontal_stats_full()$anova_df
        significance_table_formatting_function(df,input$mtc)
      })
      
      horizontal_stats_full_d1 = reactive({
        
        pre_cols = factor(c(-6:0))
        post_cols = factor(c(0:6))
        full_data = i_pFEV_sm_d1_fl_r()
        
        global_factor = 'Status'
        global_factor = input$global_factor
        pre_cols = paste(factor(c(input$pre_range[1]:input$pre_range[2])))
        post_cols = paste(factor(c(input$post_range[1]:input$post_range[2])))
        
        global_factor_list = discrete_columns_4_comparison
        
        df_output = horizontal_stats_function(full_data,pre_cols,post_cols,global_factor_list[1],input)
        
        t_df = df_output$t_df
        anova_df = df_output$anova_df
        for(global_factor in discrete_columns_4_comparison[-1]){
          
          df_output = horizontal_stats_function(full_data,pre_cols,post_cols,global_factor,input)
          if(dim(df_output$t_df)[1] > 0){
            t_df = rbind(t_df,df_output$t_df)
          }
          if(dim(df_output$t_df)[1] > 0){
            anova_df = rbind(anova_df,df_output$anova_df)
          }
        }
        
        list(t_df = t_df,anova_df = anova_df)
        
        
      })
      
      output$horizontal_t_test_full_d1 = renderDataTable({
        df = horizontal_stats_full_d1()$t_df
        significance_table_formatting_function(df,input$mtc)
      })
      output$horizontal_anova_full_d1 = renderDataTable({
        df = horizontal_stats_full_d1()$anova_df
        significance_table_formatting_function(df,input$mtc)
      })
      
      ###### _RATIOS AND PERCENTAGES #######
      output$pFEV_mean_table = renderTable({
        full_data = pFEV_wf
        full_data = pFEV_wf_r()
        global_factor = 'Status'
        global_factor = input$global_factor
        
        #data = full_data[,pFEV_numeric_colnames_f]
        factor_list = unique(full_data[,global_factor])
        #print(factor_list)
        data = full_data[,pFEV_numeric_colnames_f]
        mean_df = data.frame('pFEV' = pFEV_numeric_colnames_f)
        mean_list = as.data.frame(apply(data,2,function(x) mean(x,na.rm=T)))
        colnames(mean_list) = 'Mean for all'
        mean_df = cbind(mean_df,mean_list)
        for(entry in factor_list){
          sub_data = full_data[full_data[,global_factor] == entry,pFEV_numeric_colnames_f]
          sub_mean_list = data.frame(apply(sub_data,2,function(x) mean(x,na.rm=T)))
          colnames(sub_mean_list) = paste('Mean for',global_factor, entry)
          #print(sub_mean_list)
          mean_df = cbind(mean_df,sub_mean_list)
        }
        mean_df
        
        #(mean_df['0',2] - mean_df[,2])/ mean_df['0',2]* 100
      })
      
      pFEV_2_zero = reactive({
        full_data = comp_data()
        #full_data = pFEV_wf_r()
        
        pre_times = pFEV_numeric_colnames_f[pFEV_numeric_colnames_n < 0]
        pre_times
        post_times = pFEV_numeric_colnames_f[pFEV_numeric_colnames_n > 0]
        
        zero_data = full_data[,'0']
        zero_data
        
        pre_data = full_data[,pre_times]
        post_data = full_data[,post_times]
        
        pre_ratio_data = log2(zero_data/pre_data)
        colnames(pre_ratio_data) = paste0('log2zero_',pre_times)
        post_ratio_data = log2(post_data/zero_data)
        colnames(post_ratio_data) = paste0('log2zero_',post_times)
        
        ratio_data = cbind(pre_ratio_data,post_ratio_data)
        
        
        pre_per_data =  (zero_data-pre_data)/pre_data*100
        colnames(pre_per_data) = paste0('per2zero_',pre_times)
        
        post_per_data = (post_data-zero_data)/zero_data*100
        colnames(post_per_data) = paste0('per2zero_',post_times)
        per_data = cbind(pre_per_data,post_per_data)
        per_data
        class(per_data)
        list(ratio_data = ratio_data, per_data = per_data)
        
      })
      
      output$pFEV_ratio2zero = renderDataTable(table_formatting_function(pFEV_2_zero()$ratio_data))
      
      output$pFEV_per2zero = renderDataTable(table_formatting_function(pFEV_2_zero()$per_data))
      
      output$raw_sym_data = renderDataTable({
        
        data_name = input$col_select_prefix
        
        if(data_name %in% sym_prefix_list){
          per_data = sym_data()[,sym_cols()]
          full_data = comp_data()
          ratio_df = data.frame(MRN = full_data$MRN)
          
          #full_data = full_data[,sym_times_cols]
          #full_data
          for(i in as.numeric(input$ratio_num)){
            #print(i)
            #per_col = grep(i, colnames(per_data),value = T)
            per_col = paste0(input$col_select_prefix,i)
            
            ratio_df[,as.character(-(i))] = full_data[,as.character(-(i))]
            ratio_df[,as.character((i))] = full_data[,as.character((i))]
            ratio_df[,per_col] = per_data[,per_col]
            
          }
        }else{
          per_data = per_sym_data()[, sym_cols()]
          full_data = pFEV_2_zero()$per_data
          ratio_df = data.frame(MRN = comp_data()$MRN)
          i = 1
          for(i in as.numeric(input$ratio_num)){
            #per_col = grep(i, colnames(per_data),value = T)
            per_col = paste0(input$col_select_prefix,i)
            
            ratio_df[,paste0('per2zero_',as.character(-(i)))] = full_data[,paste0('per2zero_',as.character(-(i)))]
            ratio_df[,paste0('per2zero_',as.character((i)))] = full_data[,paste0('per2zero_',as.character((i)))]
            ratio_df[,per_col] = per_data[,per_col]
            
          }
        }
        ratio_df
        table_formatting_function(ratio_df)
      })
      sym_data = reactive({ # calcuates the ratios and percentages of timepoints across zero, -12 compared to 12, -3 compared to 3
        full_data = comp_data()
        #full_data = pFEV_wf_r()
        colnames(full_data)
        ratio_df = data.frame(MRN = full_data$MRN)
        i = 1
        for(i in sym_times_cols){
          #print(i)
          r1 = full_data[,as.character(-(i))]
          r1
          r2 = full_data[,as.character((i))]
          r2
          log2(r2/r1)
          as.character(i)
          ratio_df[,paste0('ratio_',i)] = r2/r1
          ratio_df[,paste0('log2_',i)] = log2(r2/r1)
          ratio_df[,paste0('per_',i,'')] = (r2-r1)/r1*100
          
          
        }
        ratio_df = ratio_df[,-1]
        ratio_df
        #View(ratio_df)
      })
      
      per_sym_data = reactive({ # calcuates the ratios and percentages of timepoints across zero, -12 compared to 12, -3 compared to 3
        full_data = pFEV_2_zero()$per_data
        #full_data = pFEV_wf_r()
        #View(full_data)
        colnames(full_data)
        ratio_df = data.frame(MRN = comp_data()$MRN)
        i = 1
        for(i in sym_times_cols){
          #print(i)
          pre = full_data[,paste0('per2zero_',as.character(-(i)))]
          #pre
          #print(r1)
          post = full_data[,paste0('per2zero_',as.character((i)))]
          #r2
          #print(r2)
          #print(log2(r2/r1))
          as.character(i)
          #ratio = pre/post
          #print(ratio)
          #ratio[is.nan(as.numeric(ratio))] = NA
          #ratio[is.infinite(ratio)] = NA
          #print(ratio)
          #ratio_df[,paste0('ratio_rel_',i)] = ratio
          
          #ratio = log2(pre/post)
          #ratio[is.nan(as.numeric(ratio))] = NA
          #ratio[is.infinite(ratio)] = NA
          #ratio_df[,paste0('log2_rel_',i)] = ratio
          
          ratio = (post-pre)/abs(post)*100
          ratio[is.nan(as.numeric(ratio))] = NA
          ratio[is.infinite(ratio)] = NA
          ratio_df[,paste0('per_rel_',i,'')] = ratio
        }
        ratio_df = ratio_df[,-1]
        #View(ratio_df)
        ratio_df
        
      })
      
      log_sym_data = reactive({ # calcuates the ratios and percentages of timepoints across zero, -12 compared to 12, -3 compared to 3
        full_data = pFEV_2_zero()$ratio_data
        #full_data = pFEV_wf_r()
        #View(full_data)
        colnames(full_data)
        ratio_df = data.frame(MRN = comp_data()$MRN)
        i = 1
        for(i in sym_times_cols){
          #print(i)
          pre = full_data[,paste0('log2zero_',as.character(-(i)))]
          #pre
          #print(r1)
          post = full_data[,paste0('log2zero_',as.character((i)))]
          #r2
          #print(r2)
          #print(log2(r2/r1))
          as.character(i)
          ratio = pre-post
          #print(ratio)
          ratio[is.nan(as.numeric(ratio))] = NA
          ratio[is.infinite(ratio)] = NA
          #print(ratio)
          ratio_df[,paste0('ratio_rel_',i)] = ratio
          # 
          # ratio = log2(pre/post)
          # ratio[is.nan(as.numeric(ratio))] = NA
          # ratio[is.infinite(ratio)] = NA
          # ratio_df[,paste0('log2_rel_',i)] = ratio
          # 
          # ratio = (pre-post)/post*100
          # ratio[is.nan(as.numeric(ratio))] = NA
          # ratio[is.infinite(ratio)] = NA
          # ratio_df[,paste0('per_rel_',i,'')] = ratio
        }
        ratio_df = ratio_df[,-1]
        #View(ratio_df)
        ratio_df
        
      })
      
      
      
      select_sym_cols = reactive({
        col_selected = eval(parse(text = input$col_select))
        col_selected
      })
      
      output$sym_equation = renderText({
        data_name = input$col_select_prefix
        #"ratio_",'log2_','per_', 'per_rel_'
        out_text = 'test'
        eq_text = 'Equation : '
        if(data_name == "ratio_"){
          out_text = paste(eq_text, 'post_x / pre_x')
        }
        if(data_name == "log2_"){
          out_text = paste(eq_text, 'log2(post_x / pre_x)')
        }
        if(data_name == "per_"){
          out_text = paste(eq_text, 'post_x - pre_x / pre_x * 100')
        }
        if(data_name == "sym_rel_ratio_colnames"){
          out_text = paste(eq_text, '(post_x / 0) / (0 / pre_x)')
        }
        if(data_name == "sym_rel_log_ratio_colnames"){
          out_text = paste(eq_text, 'log2(post_x / pre_x)')
        }
        if(data_name == "per_rel_"){
          out_text = paste(eq_text, '((post_x - 0 / 0 * 100) - (0 - pre_x / pre_x * 100)) / abs(0 - post_x / post_x * 100) ')
        }
        out_text
      })
      
      output$pFEV_ratio_df = renderDataTable({
        df = change_data_w()
        
        df = df[,sym_cols()]
        table_formatting_function(df)
      })
      
      output$scale_slide = renderUI({
        full_data = change_data_w()
        plot_data = melt(full_data, measure.vars = sym_cols())
        #print(plot_data$value)
        plot_min = round(min(plot_data$value,na.rm = T),3)
        plot_max = round(max(plot_data$value,na.rm = T),3)
        sliderInput('sym_y_lim','y axis limits', min = plot_min,max=plot_max, step = 0.01, value = c(plot_min,plot_max))
        
      })
      
      sym_cols = reactive({
        select_cols = paste0(input$col_select_prefix,input$ratio_num)
        select_cols
        
      })
      
      output$sym_ratio_boxplot = renderPlot({
        full_data = change_data_w()
        
        plot_data = melt(full_data, measure.vars = sym_cols())
        global_factor = 'Status'
        global_factor = input$global_factor
        ggplot(plot_data, aes_string(x = 'variable', y = 'value', col = global_factor)) +
          coord_cartesian(ylim = c(as.numeric(input$sym_y_lim[1]),as.numeric(input$sym_y_lim[2]))) + 
          geom_hline(yintercept = 0) + 
          
          geom_boxplot()
      })
      
      
      mean_sym = reactive({
        #full_data = sym_df
        global_factor = 'Status'
        #ratio_colnames = paste0('log2(',sym_times_cols,')')
        #per_colnames = paste0('per_',sym_times_cols)
        
        full_data = change_data_w()
        global_factor = input$global_factor
        factor_list = unique(full_data[,global_factor])
        factor_list
        entry = factor_list[1]
        sub_data = full_data[full_data[,global_factor] == entry,sym_cols()]
        mean_ratio_df = data.frame(t(apply(sub_data,2, function(x) mean(x,na.rm=T))))
        mean_ratio_df$Factor = global_factor
        mean_ratio_df$Status = entry
        for(entry in factor_list[-1]){
          sub_data = full_data[full_data[,global_factor] == entry,sym_cols()]
          mean_ratio_df_n = data.frame(t(apply(sub_data,2, function(x) mean(x,na.rm=T))))
          mean_ratio_df_n$Factor = global_factor
          mean_ratio_df_n$Status = entry
          mean_ratio_df = rbind(mean_ratio_df,mean_ratio_df_n)
        }
        mean_ratio_df
        
        mean_ratio_df = mean_ratio_df[,c('Factor','Status',colnames(mean_ratio_df[c(1:(length(colnames(mean_ratio_df))-2))]))]
        
        mean_ratio_df
      })
      
      
      
      output$sym_ratio_mean_df = renderDataTable({
        df = mean_sym()
        table_formatting_function(df)
      })
      #output$sym_per_mean_df = renderDataTable(table_formatting_function(mean_sym()$mean_per_df))
      
      anova_sym = reactive({
        anova_df_b = data.frame(term = numeric(0), df = numeric(0), sumsq = numeric(0), meansq = numeric(0), statistic = numeric(0),
                                p.value = numeric(0), Factor = numeric(0), time  = numeric(0))
        anova_df = anova_df_b
        #full_data = sym_df
        global_factor = 'HLAType'
        full_data = change_data_w()
        global_factor = input$global_factor
        #ratio_colnames = paste0('log2(',sym_times_cols,')')
        #per_colnames = paste0('per_',sym_times_cols)
        
        ratio_stat_data = melt(full_data, measure.vars = sym_cols())
        #View(ratio_stat_data)
        #print(colnames(ratio_stat_data))
        #print(ratio_stat_data$value)
        #per_stat_data = melt(full_data, measure.vars = sym_per_colnames)
        time = sym_ratio_colnames[7]
        for(time in sym_cols()){
          time_data = ratio_stat_data[ratio_stat_data$variable == time,]
          #cmd = paste("anova_df = tidy(manova(cbind(variable,value) ~ ",global_factor,", data = stat_data))")
          anova_df_n = tryCatch(tidy(anova(lm(time_data$value ~ time_data[,global_factor]))), error = function(e) e = anova_df_b)
          if(dim(anova_df_n)[1] > 0){
            anova_df_n$Factor = global_factor
            anova_df_n$time = time
            anova_df = rbind(anova_df,anova_df_n)
          }
        }
        anova_df
        df = col_rearrange_function(anova_df,2)
        df
        
      })
      
      manova_sym = reactive({
        
        #full_data = sym_df
        global_factor = 'HLAType'
        full_data = change_data_w()
        ##View(full_data)
        global_factor = input$global_factor
        sym_times_cols_selected = sym_cols()
        #print(sym_times_cols_selected)
        #ratio_colnames = paste0('log2(',sym_times_cols_selected,')')
        #per_colnames = paste0('per_',sym_times_cols)
        
        ratio_stat_data = melt(full_data, measure.vars = sym_times_cols_selected)
        
        manova_df = pairwise_manova_function(ratio_stat_data,global_factor)
        manova_df$range = paste(sym_times_cols_selected,collapse = ', ')
        manova_df
        
      })
      
      output$manova_sym_table = renderDataTable({
        df = manova_sym()
        significance_table_formatting_function(df,input$mtc)
      })
      
      
      output$anova_pw_sym_ratio = renderDataTable({
        df =  anova_sym()
        df
        significance_table_formatting_function(df,input$mtc)
      })
      
      
      sym_t_test = reactive({
        t_df_b = data.frame(estimate = numeric(0), estimate1 = numeric(0), estimate2 = numeric(0),
                            statistic = numeric(0), p.value = numeric(0), parameter = numeric(0),
                            conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                            alternative = numeric(0),
                            Factor = numeric(0), comparison = numeric(0), time = numeric(0))
        t_df = t_df_b
        #full_data = sym_df
        global_factor = "HLAType"
        full_data = change_data_w()
        
        #ratio_colnames = paste0('log2(',sym_times_cols,')')
        #per_colnames = paste0('per_',sym_times_cols)
        
        global_factor = input$global_factor
        factor_list = unique(full_data[,global_factor])
        factor_list = paste(factor_list[order(factor_list)])
        #factor_list
        l = length(factor_list)
        entry = factor_list[1]
        #entry
        time = sym_ratio_colnames[7]
        #time
        i = 1
        j = 2
        ratio_data = melt(full_data,measure.vars = sym_cols())
        for(time in sym_cols()){
          time_data = ratio_data[ratio_data$variable == time,]
          for(i in c(1:l)){
            for(j in c(1:l)){
              if(i < j){
                i_entry = factor_list[i]
                #i_entry
                j_entry = factor_list[j]
                #j_entry
                i_data = time_data$value[time_data[,global_factor] == i_entry]
                #i_data
                j_data = time_data$value[time_data[,global_factor] == j_entry]
                #j_data
                t_df_n = tryCatch(tidy(t.test(i_data,j_data)), error = function(e) e = t_df_b)
                t_df_n
                if(dim(t_df_n)[1] > 0){
                  
                  #colnames(t_df_n)
                  t_df_n$Factor = global_factor
                  t_df_n$comparison = paste(i_entry,'vs',j_entry)
                  t_df_n$time = time
                  #t_df_n
                  t_df = rbind(t_df,t_df_n)
                  #t_df
                }
              }
            }
            
          }
        }
        t_df
        #colnames(t_df)
        df = col_rearrange_function(t_df,3)
        df
        
      })
      
      sym_t_test_0 = reactive({
        t_df_b = data.frame(estimate = numeric(0), estimate1 = numeric(0), estimate2 = numeric(0),
                            statistic = numeric(0), p.value = numeric(0), parameter = numeric(0),
                            conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                            alternative = numeric(0),
                            Factor = numeric(0), comparison = numeric(0), time = numeric(0))
        t_df = t_df_b
        #full_data = sym_df
        global_factor = "HLAType"
        full_data = change_data_w()
        
        #ratio_colnames = paste0('log2(',sym_times_cols,')')
        #per_colnames = paste0('per_',sym_times_cols)
        
        global_factor = input$global_factor
        factor_list = unique(full_data[,global_factor])
        factor_list = paste(factor_list[order(factor_list)])
        #factor_list
        l = length(factor_list)
        entry = factor_list[1]
        #entry
        time = sym_ratio_colnames[7]
        #time
        i = 1
        j = 2
        ratio_data = melt(full_data,measure.vars = sym_cols())
        for(time in sym_cols()){
          time_data = ratio_data[ratio_data$variable == time,]
          for(i in c(1:l)){
            #for(j in c(1:l)){
            #  if(i < j){
            i_entry = factor_list[i]
            #i_entry
            #j_entry = factor_list[j]
            #j_entry
            i_data = time_data$value[time_data[,global_factor] == i_entry]
            #i_data
            #j_data = time_data$value[time_data[,global_factor] == j_entry]
            #j_data
            t_df_n = tryCatch(tidy(t.test(i_data,mu= 0)), error = function(e) e = t_df_b)
            t_df_n
            if(dim(t_df_n)[1] > 0){
              
              #colnames(t_df_n)
              t_df_n$Factor = global_factor
              t_df_n$comparison = paste(i_entry,'vs mean of zero')
              t_df_n$time = time
              #t_df_n
              t_df = rbind(t_df,t_df_n)
              #t_df
              # }
              #}
            }
            
          }
        }
        t_df
        #colnames(t_df)
        df = col_rearrange_function(t_df,3)
        df
        
      })
      
      output$sym_t_test_table = renderDataTable({
        df = sym_t_test()
        significance_table_formatting_function(df,input$mtc)
      })
      
      output$sym_t_test_table_0 = renderDataTable({
        df = sym_t_test_0()
        significance_table_formatting_function(df,input$mtc)
      })
      
      

  
  ##### CLUSTERING ###############
  
      discrete_cluster_D = reactive({
        full_data = change_data()
        print(colnames(full_data))
        cluster_data_list = clustering_function(full_data,retained_patients(),input$clutree_num,
                                                input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
                                                input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
        #return(list(data_dist = data_dist, D = D, o_data = o_data, data = data, x_cluster = x_cluster, weights = weights))
        cluster_data_list
      })
  
    

  
  
        ### _plot clusters ####
            output$D_text = renderPrint(str(discrete_cluster_D()$D,indent.str = '<br />'))

            
          output$discrete_cluster_plot = renderPlot({
              
              D = discrete_cluster_D()$D
              dendr <- dendro_data(D, type = "rectangle") 
              x_cluster = discrete_cluster_D()$x_cluster
              cut = input$clutree_num
              p = dendrogram_plot_function(dendr,x_cluster,cut)
              print(p)
              

            })  
          
            
            output$discrete_x_table = renderDataTable({
              x_cluster = discrete_cluster_D()$x_cluster
              x_cluster_table = data.frame(num = numeric(0),MRN = numeric(0))
              for(entry in unique(x_cluster$cluster)){
                line_list  = x_cluster[x_cluster$cluster == entry,'label']
                line = paste(line_list,collapse = (', '))
                line
                x_cluster_table[entry,'num'] = length(line_list)
                x_cluster_table[entry,'MRN'] = line
              }
              tx = as.data.frame(t(x_cluster_table))
              tx

              })
            
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
  
            #### __DISTANCE SCATTER PLOTS ####
            

     
            distance_model = reactive({
              data_dist = discrete_cluster_D()$data_dist
              x = discrete_cluster_D()$x
              cm = cmdscale(data_dist)
              xy <- data.frame(cm, factor(x))
              names(xy) <- c("x", "y", "cluster")
              xy$model <- rownames(xy)
              xy
            })
            output$distance_scatter = renderPlot({
              xy = distance_model()
              ggplot(xy, aes(x, y, colour=cluster)) + 
                geom_point(size=3)
            })
            output$distance_density = renderPlot({
              xy = distance_model()
              ggplot(xy, aes(x, y, colour=cluster)) + 
                geom_point( size=3) +
                geom_density2d(alpha=0.5)
            })
            output$distance_polygon = renderPlot({
              xy = distance_model()
              ggplot(xy, aes(x, y, colour=cluster,fill = cluster)) + 
                geom_point(size=3) +
                geom_polygon(alpha = 0.5)
            })
            output$distance_polygon_neat = renderPlot({
              mydata = distance_model()
              nomissing <- na.omit(mydata) #chull function does not work with missing data
              
              #getting the convex hull of each unique point set
              df <- nomissing
              find_hull <- function(df) df[chull(df$x, df$y), ]
              hulls <- ddply(df, "cluster", find_hull)
              
              plot <- ggplot(data = nomissing, aes(x = x, y = y, colour=cluster, fill = cluster)) +
                geom_point() + 
                geom_polygon(data = hulls, alpha = 0.5)
              print(plot)
            })
            
            output$distance_table = renderDataTable(as.matrix(discrete_cluster_D()$data_dist))
            output$distance_model_table = renderDataTable(distance_model())
            
          
            

            
            
    #### _CLUSTER COMPOSITION TABLES #####
    
          #### __TOTAL ####
          cluster_analysis_total = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_total(df,'cluster')
            df_tc
          })
          
          output$cluster_analysis = DT::renderDataTable({
            df = cluster_analysis_total()
            colour = 'lightgreen'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          cluster_analyis_selected_df = reactive({
            df = cluster_analysis_total()
            data = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            data = data[order(data$Factor,data$Status),]
            data
            
          })
          
          
          output$cluster_analyis_selected_table = DT::renderDataTable({
            df = cluster_analyis_selected_df()

            colour = 'lightgreen'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour) 
          })
          
          ### __WITHIN ####
          cluster_analysis_within = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_within(df,'cluster')
            df_tc
          })

          
          output$cluster_analysis_within_table = DT::renderDataTable({
            df = cluster_analysis_within()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          
          
          cluster_analysis_within_table_selected_df = reactive({
            df = cluster_analysis_within()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })

          
          output$cluster_analysis_within_table_selected_table = DT::renderDataTable({
            df = cluster_analysis_within_table_selected_df()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })


          ### __CHISQ ####
          
          
          output$cluster_select_clusters <- renderUI({
            data = cluster_analysis_within_table_selected_df()
            test_data = data[,c(3:dim(data)[2])]
            clusters <- colnames(test_data)
            selectInput("cluster_select_clusters", "Choose Clusters to Test using ChiSQ", choices = clusters, selected = clusters,multiple = T)
          })
          output$chisq_cluster = renderDataTable({
            full_data = cluster_analyis_selected_df()
            
            chi_df = chisq_total(full_data,input)
            chi_df
            significance_table_formatting_function(chi_df,input$mtc)
            
            
            
          })
          output$chisq_cluster_within = renderDataTable({
            data = cluster_analysis_within_table_selected_df()
            
            chi_df = chisq_within(data,input)
            chi_df
            significance_table_formatting_function(chi_df,input$mtc)
            
            
          })
        
 
########## BOS PLOTS ###########

  
          
          bos_df = reactive({
            full_data = pFEV_wf
            full_data = pFEV_wf_r()
            bos_data = BOS_function(full_data)
            #return(list(bos_df = bos_df,patient_status_df = patient_status_df))
            bos_data
          })
          
          patient_status_df= reactive({
            full_data = pFEV_wf
            full_data = pFEV_wf_r()
            patient_status = BOS_patient_function(full_data)
            #return(list(bos_df = bos_df,patient_status_df = patient_status_df))
            patient_status
          })
          
          output$bos_df = renderDataTable(bos_df())
          output$bos_patient_status = renderDataTable(patient_status_df())
          
          output$bos_plots = renderPlot({
            x1 = -12
            x2 = 12
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            #bos_df_data = bos_df$bos_df
            bos_df_data = bos_df()
            bos_data = bos_df_data[,c('time',"BOS1_free","BOS2_free","BOS3_free",'BOS3_surv_free')]
            bos_data
            m_bos = melt(bos_data,id.var = 'time')
            ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
              geom_line() +
              geom_vline(xintercept = 0) +
              geom_hline(yintercept = 0)+
              xlim(x1,x2)
            
          })
          
          output$bos_plots_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            bos_df_data = bos_df()
            bos_data = bos_df_data[,c('time',"BOS1_free","BOS2_free","BOS3_free")]
            bos_data
            m_bos = melt(bos_data,id.var = 'time')
            ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
              geom_smooth() +
              geom_vline(xintercept = 0) +
              geom_hline(yintercept = 0)+
              xlim(x1,x2)
            
          })
          
          bos_factor = reactive({
            full_data = pFEV_wf_r()
            global_factor = 'Status'
            global_factor = input$global_factor
            factor_entry = unique(na.omit(full_data[,global_factor]))
            entry = factor_entry[1]
            #factor_entry
            df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1_free = numeric(0),BOS2_free = numeric(0),BOS3_free = numeric())
            patient_df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            for(entry in factor_entry){
              function_data = full_data[full_data[,global_factor] == entry,]
              bos_df = BOS_function(function_data)
              patient_status = BOS_patient_function(function_data)
              str(patient_status)
              head(patient_status)
              bos_df$Factor = global_factor
              bos_df$Status = entry
              df = rbind(df,bos_df[,c("Factor",'Status','time','BOS1_free','BOS2_free','BOS3_free',"BOS3_surv_free")])
              
              
              patient_status$Factor = global_factor
              patient_status$Status = entry
              head(patient_df)
              head(patient_status)
              patient_df = rbind(patient_df,patient_status[,c("Factor",'Status','time','BOS1','BOS2','BOS3')])
            }
            ##View(df)
            m_bos = melt(df,id.vars= c('Factor','Status','time'))
            m_bos
          })
          
          output$boss_factor_table = renderDataTable(bos_factor())
          
          output$bos1_factor_plot = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS1_free'
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)
            #p


          })
          output$bos2_factor_plot = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS2_free'
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)

          })
          output$bos3_factor_plot = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS3_free'
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)

          })
          output$bos3_surv_factor_plot = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS3_surv_free'
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
          output$bos3_surv_factor_plot_cluster = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = 'cluster'
            m_bos = bos_factor()
            col_name = 'BOS3_surv_free'
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
          # output$bos3_surv_factor_plot_cluster = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = 'cluster_d1'
          #   m_bos = bos_factor()
          #   col_name = 'BOS3_surv_free'
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          #   
          # })
          
          output$bos1_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS1_free'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
            
          })
          output$bos2_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS2_free'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          output$bos3_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS3_free'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          output$bos3_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS3_surv_free'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
          bos_factor_cluster = reactive({
            full_data = pFEV_wf_r()
            #global_factor = 'Status'
            global_factor = 'cluster'
            factor_entry = unique(na.omit(full_data[,global_factor]))
            factor_entry
            df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1_free = numeric(0),BOS2_free = numeric(0),BOS3_free = numeric())
            for(entry in factor_entry){
              function_data = full_data[full_data[,global_factor] == entry,]
              bos_df = BOS_function(function_data)
              #bos_df = bos_data$bos_df
              bos_df$Factor = global_factor
              bos_df$Status = entry
              df = rbind(df,bos_df[,c("Factor",'Status','time','BOS1_free','BOS2_free','BOS3_free',"BOS3_surv_free")])
            }
            ##View(df)
            m_bos = melt(df,id.vars= c('Factor','Status','time'))
            m_bos
            ##View(m_bos)
          })
          output$bos3_surv_factor_plot_cluster = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = 'cluster'
            m_bos = bos_factor_cluster()
            col_name = 'BOS3_surv_free'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
          # bos_factor_cluster_d1 = reactive({
          #   full_data = pFEV_wf_r()
          #   #global_factor = 'Status'
          #   global_factor = 'cluster_d1'
          #   factor_entry = unique(na.omit(full_data[,global_factor]))
          #   factor_entry
          #   df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1_free = numeric(0),BOS2_free = numeric(0),BOS3_free = numeric())
          #   for(entry in factor_entry){
          #     function_data = full_data[full_data[,global_factor] == entry,]
          #     bos_df = BOS_function(function_data)
          #     #bos_df = bos_data$bos_df
          #     bos_df$Factor = global_factor
          #     bos_df$Status = entry
          #     df = rbind(df,bos_df[,c("Factor",'Status','time','BOS1_free','BOS2_free','BOS3_free',"BOS3_surv_free")])
          #   }
          #   ##View(df)
          #   m_bos = melt(df,id.vars= c('Factor','Status','time'))
          #   m_bos
          # })
          # 
          # output$bos3_surv_factor_plot_cluster_d1 = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = 'cluster_d1'
          #   m_bos = bos_factor_cluster_d1()
          #   col_name = 'BOS3_surv_free'
          #   p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          #   
          # })
          
          


          


})


