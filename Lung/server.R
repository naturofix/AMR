



shinyServer(function(input, output) {
  
  output$cover_plot = renderPlot({
    xy = distance_model_d1()
    ggplot(xy, aes(x, y, colour=cluster)) +
      geom_point( size=3) +
      geom_density2d(alpha=0.5)
  })
  

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
  
  status_r = reactive({
    if(input$status_radio == '0'){
      s = c('1','2')
    }else{
      s = input$status_radio
    }
    s
    #print(s)
    
  })
  
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
    data = data[data$Status %in% status_r(),]

    data
    
    })
  pFEV_wf_c = reactive({
    o_data = pFEV_wf
    data = o_data[o_data$MRN %in% retained_patients(),]
    data
  })
  
  pFEV_lf_r = reactive({
    w_data = pFEV_wf_r()
    data = melt(w_data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = colnames(pFEV_w))
    data$time = as.numeric(as.character(data$variable))
    
    data
  })
  
  i_pFEV_wf_r = reactive({
    o_data = i_pFEV_wf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data = data[data$Status %in% status_r(),]
    
    data
  })
  
  i_pFEV_wf_c = reactive({
    o_data = i_pFEV_wf
    data = o_data[o_data$MRN %in% retained_patients(),]
  })
  
  
  i_pFEV_lf_r = reactive({
    o_data = i_pFEV_lf
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    data = data[data$Status %in% status_r(),]
    
    data
  })
  
  i_pFEV_smf_r = reactive({
    o_data = i_pFEV_smf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data = data[data$Status %in% status_r(),]
    
    data
  })
  i_pFEV_sm_lf_r = reactive({
    o_data = i_pFEV_sm_lf
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    data = data[data$Status %in% status_r(),]
    
    data
  })

  
  i_pFEV_sm_d1_f_c = reactive({
    o_data = i_pFEV_sm_d1_f
    data = o_data[o_data$MRN %in% retained_patients(),]
    data
  })
  i_pFEV_sm_d1_f_r = reactive({
    o_data = i_pFEV_sm_d1_f
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data = data[data$Status %in% status_r(),]
    data
  })
  i_pFEV_sm_d1_fl_r = reactive({
    o_data = i_pFEV_sm_d1_fl
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    data = data[data$Status %in% status_r(),]
    
    data
  })
  
  
  
  i_pFEV_sm_d1_f_c_ir = reactive({
    o_data = i_pFEV_sm_d1_f_c()
    m_data = pFEV_wf_c()
    d1_cols = p_cols[p_cols %in% colnames(o_data)]
    for(col_name in d1_cols){
      o_data[which(is.na(m_data[,col_name])),col_name] = NA
    }
    o_data
  })
  i_pFEV_sm_d1_f_c_ir_r = reactive({
    o_data = i_pFEV_sm_d1_f_c_ir()
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    m_data_d1 = discrete_cluster_D_d1()$data
    m_data_d1$MRN = rownames(m_data_d1)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data_d1$cluster[match(data$MRN,m_data_d1$MRN)]
    
    #data$cluster = discrete_cluster_D()$data$cluster
    #data$cluster_d1 = discrete_cluster_D_d1()$data$cluster
    data = data[data$Status %in% status_r(),]
    
    data
    
  })
  i_pFEV_sm_d1_fl_c_ir_r = reactive({
    w_data = i_pFEV_sm_d1_f_c_ir_r()
    data = melt(w_data, id.vars = c(colnames(full_fac_0),'cluster','cluster_d1'), measure.vars = colnames(pFEV_w)[-2])
    data$time = as.numeric(as.character(data$variable))
    data
  })
  
  
  
  
  i_pFEV_sm_d2_f_r = reactive({
    o_data = i_pFEV_sm_d2_f
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    data = data[data$Status %in% status_r(),]
    
    data
  })
  i_pFEV_sm_d2_fl_r = reactive({
    o_data = i_pFEV_sm_d2_fl
    m_data = pFEV_lf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data$cluster_d1 = m_data$cluster_d1[match(data$MRN,m_data$MRN)]
    #data$time = as.numeric(as.character(data$variable))
    data = data[data$Status %in% status_r(),]
    
    data
  })
  
  # output$test_table_1 = renderDataTable(pFEV_lf_r())
  # output$test_table_2 = renderDataTable(i_pFEV_sm_d1_fl_r())
  
  output$cluster_test = renderDataTable({
    test_list = list()
    #rownames(pFEV_wf_r())
    ###  pFEV_wf_r #####
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
    ##### i_pFEV_smf_r ####
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
    #### i_pFEV_sm_d1_f_r #####
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
    ###  i_pFEV_sm_d2_f_r #####
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
    
    ###  i_pFEV_lf_r #####
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
    ##### i_pFEV_sm_lf_r ####
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
    #### i_pFEV_sm_d1_fl_r #####

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
        l1 = c(l1,unique(pFEV_lf_r()[,c('cluster','cluster_d1')][pFEV_lf_r()$MRN == entry,]))
        l2 = c(l2,unique(i_pFEV_sm_d1_fl_r()[,c('cluster','cluster_d1')][i_pFEV_sm_d1_fl_r()$MRN == entry,]))
      }
      #print(identical(l1,l2))
      #print('HIT')
      if(identical(l1,l2)){
        test_list$i_pFEV_sm_d1_fl_r_ENTRY_BY_ENTRY = TRUE
      }else{
        test_list$i_pFEV_sm_d1_fl_r_ENTRY_BY_ENTRY = FALSE
      }
      
    }
    ###  i_pFEV_sm_d2_fl_r #####
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
        l1 = c(l1,unique(pFEV_lf_r()[,c('cluster','cluster_d1')][pFEV_lf_r()$MRN == entry,]))
        l2 = c(l2,unique(i_pFEV_sm_d2_fl_r()[,c('cluster','cluster_d1')][i_pFEV_sm_d2_fl_r()$MRN == entry,]))
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
    print(df)
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
  

  ########### LINE PLOT ############
  
  output$line_pFEV = renderPlot({
    r_data = pFEV_lf_r()
    title = paste('pFEV values for ',length(unique(r_data$MRN))," Patients")
    line_plot_function(r_data,title,input)

  })
  
  output$smooth_line_pFEV = renderPlot({
    r_data = pFEV_lf_r()
    title = paste('pFEV values for ',length(unique(r_data$MRN))," Patients")
    smooth_line_plot_function(r_data,title,input)
    
  })
  
  output$line_i_pFEV = renderPlot({
    r_data = i_pFEV_lf_r()
    title = paste('Imputed pFEV values for ',length(unique(r_data$MRN))," Patients")
    line_plot_function(r_data,title,input)
  })
  
  ################ pFEV BOXPLOTS #######################
  output$boxplot_pFEV = renderPlot({
    full_data = pFEV_lf_r()
    title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
    boxplot_function(full_data,title,input)
  })
  

  
  output$boxplot_pFEV_cluster = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
    cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
  
    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$boxplot_pFEV_cluster_full = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
    cols = factor(c(input$pre_range[1]:input$post_range[2]))

    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$interaction_plot = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    bias_col = 'Status'
    plot_data = full_data[full_data$variable %in% cols,]
    scale_cols = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]
    global_factor = input$global_factor
    interactors = input$interactors
    print(interactors)
    interactor_list = c(global_factor,interactors)
    interactor_list = paste(c(global_factor,interactors),collapse = ', ')
    print(interactor_list)
    #interactors = paste(c(global_factor,interactors),collapse=(','))
    print(interactor_list)
    #plot_data = full_data[,cols]
    height_var = 600
    ggplot(plot_data, aes(x = variable, y = value)) + 
      #geom_boxplot(aes_string((col=paste0("interaction(", paste0(interactor_list, collapse =  ", "), ")"))))
      stat_summary(data = plot_data,fun.y=mean,geom="line",
                   aes_string(group=paste0("interaction(", paste0(interactor_list, collapse =  ", "), ")"),
                              col=paste0("interaction(", paste0(interactor_list,collapse =  ", "), ")"))) + 
      facet_grid(as.formula(paste(global_factor, '~','.')))

  },height = 1600)
  
  output$boxplot_pFEV_cluster_d1 = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster_d1'
    title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
    cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
    
    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
    
  })
  
  output$boxplot_pFEV_cluster_d1_full = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster_d1'
    title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    
    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$boxplot_i_pFEV = renderPlot({
    
    full_data = i_pFEV_lf_r()
    title = paste('Imputed pFEV values for ',length(unique(full_data$MRN))," Patients")
    boxplot_i_summary_function(full_data,title,input)

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
  
  output$i_pFEV_sm_line = renderPlot({
    r_data = i_pFEV_sm_lf_r()
    title = paste("IMPUTED SMOOTH")
    line_plot_function(r_data,title,input)

  })

  
  ############# boxplot ####################
  output$boxplot_i_pFEV_sm = renderPlot({
    r_data = i_pFEV_sm_lf_r()
    title = paste("IMPUTED SMOOTH MEAN")
    mean_line_plot_function(r_data,title,input)
  })

  
  ############ D1 ##################
  
  output$i_pFEV_sm_d1_line = renderPlot({
    r_data = i_pFEV_sm_d1_fl_r()
    title = "IMPUTED SMOOTH D1"
    D_line_plot_function(r_data,title,input)
    
  })
  
  output$boxplot_i_pFEV_sm_d1 = renderPlot({
    cols = c(-6:6)
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    cols
    summary_data =  i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
    i_data = i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% pFEV_numeric_colnames_f,]
    ggplot(NULL) +
      #stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
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
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
  
  output$i_pFEV_sm_d1_line_ri = renderPlot({
    r_data = i_pFEV_sm_d1_fl_c_ir_r()
    title = "IMPUTED SMOOTH D1 REMOVED MISSING"
    D_line_plot_function(r_data,title,input)
    
  })
  
  output$boxplot_i_pFEV_sm_d1_ri = renderPlot({
    cols = c(-6:6)
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    cols
    
    summary_data =  i_pFEV_sm_d1_fl_c_ir_r()[i_pFEV_sm_d1_fl_c_ir_r()$variable %in% cols,]
    i_data = i_pFEV_sm_d1_fl_c_ir_r()[i_pFEV_sm_d1_fl_c_ir_r()$variable %in% pFEV_numeric_colnames_f,]
    col_breaks = unlist(pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols])
    print(col_breaks)
    col_break_order = unlist(col_breaks[order(as.numeric(as.character(col_breaks)))])
    print(col_break_order)

    ggplot(NULL) +
      #stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #geom_vline(data = i_data, aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
      
      geom_boxplot(data = i_data, aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #theme(axis.text.x = element_text(size=14, angle=90)) +
      #scale_x_discrete(breaks = col_break_order) +
      #ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D1 boxplot REMOVED MISSING")
  })
  
  output$boxplot_i_pFEV_sm_d1_mean_ri = renderPlot({
    cols = c(-6:6)
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    cols
    print(cols)
    summary_data =  i_pFEV_sm_d1_fl_c_ir_r()[i_pFEV_sm_d1_fl_c_ir_r()$variable %in% cols,]
    ggplot(NULL) +
      stat_summary(data = summary_data, fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      #geom_vline(aes(xintercept = which(levels(i_pFEV_sm_lf$variable) %in% '0'))) +
      
      #geom_boxplot(data = i_pFEV_sm_d1_fl_r()[i_pFEV_lf$variable %in% pFEV_numeric_colnames_f & full_fac_0$pFEV_na > input$change_completeness,], aes_string(x = 'variable', y = 'value',col = input$global_factor)) +
      #stat_summary(data = i_pFEV_sm_d1_fl_r()[full_fac_0$pFEV_na > input$change_completeness,], fun.y=mean,geom="line",lwd=2,aes_string(x = 'variable', y = 'value',group=input$global_factor,col = input$global_factor)) +
      theme(axis.text.x = element_text(size=14, angle=90)) +
      scale_x_discrete(breaks = pFEV_numeric_colnames_f[pFEV_numeric_colnames_f %in% cols]) +
      ylim(-0.1,0.1)+
      
      ggtitle("Imputed Smoothed D1 Mean REMOVED MISSING")
  })
  
  
  

  

  
####################### D2 #########################
  
  i_mrn_sub_pFEV_sm_d2 = reactive({
    cols = c(-6:6)
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
    cols
    i_pFEV_sm_d2_fl_r()[i_pFEV_sm_d2_fl_r()$MRN %in% input$change_mrn_select & i_pFEV_sm_d2_fl_r()$variable %in% cols,]
  })
  
  output$i_pFEV_sm_d2_line = renderPlot({
    r_data = i_pFEV_sm_d2_fl_r()
    title = "IMPUTED SMOOTH D2"
    D_line_plot_function(r_data,title,input)
    # r_data = i_mrn_sub_pFEV_sm_d2()
    # #data = pFEV
    # ggplot(r_data, aes(x = variable, y = value,col = MRN)) + 
    #   geom_line(aes(group = MRN)) +
    #   #geom_point() +
    #   scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
    #   
    #   theme(axis.text.x = element_text(size=8, angle=90)) +
    #   theme(legend.position="none") + 
    #   #scale_x_discrete(names = pFEV_numeric_colnames,breaks = pFEV_numeric_colnames) + 
    #   ggtitle("IMPUTED SMOOTHED D2")
  })
  
  output$boxplot_i_pFEV_sm_d2 = renderPlot({
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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

    cols = factor(c(input$pre_range[1]:input$post_range[2]))

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
                cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
            
            ## Imputed
            
            df_lm_i = reactive({
              df = data.frame(Factor = numeric(), Status = numeric(0))
              factor = 'MRN'
              factor = input$global_factor
              cols = c(-6:6)
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
              cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
          
          output$test_plot_1 = renderPlot({
            data = slope_boxplot_data()
            global_factor = input$global_factor
            sig_col = c("white", "blanchedalmond")
            if(!(0 %in% data$significant)){
              sig_col = c("blanchedalmond")
            }
            sig_col = c('white','gray73')
            
            ggplot(data)+
              geom_boxplot(aes_string(col = global_factor,y='value',x = global_factor,fill='variable')) +
              scale_fill_manual(values = sig_col)
              #geom_line(annotate("text", x = 2, y = 42, label = "*", size = 8)
              
              #geom_signif(comparisons = list(c(global_factor, 'value', 'variable')), 
              #map_signif_level=TRUE)
          })
          
          output$slope_boxplot = renderPlot(slope_boxplot_function(slope_boxplot_data(),input$global_factor))
          output$slope_table = renderDataTable(t(df_slope()))
          
          df_lm_sample_i = reactive({
            function_data = i_pFEV_lf_r()
            df = i_pFEV_wf_r()
            factor = 'MRN'
            
            cols = c(-6:6)
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
          
          output$slope_fit_pFEV_pre_plot_i = renderPlot({
            data = i_pFEV_lf_r()
            cols = factor(c(input$pre_range[1]:input$pre_range[2]))
            slope_fit_plot_function(data,cols,'Pre Treatment',input) 
          })
          output$slope_fit_pFEV_post_plot_i = renderPlot({
            data = i_pFEV_lf_r()
            cols = factor(c(input$post_range[1]:input$post_range[2]))
            slope_fit_plot_function(data,cols,'Post Treatment',input) 
          })
          
          slope_boxplot_data_i = reactive(slope_boxplot_data_function(df_lm_sample_i(),df_slope_i(),input$global_factor))
          output$slope_boxplot_i = renderPlot(slope_boxplot_function(slope_boxplot_data_i(),input$global_factor))
          #output$slope_table_i = renderDataTable((slope_boxplot_data_i()))
          output$slope_table_i = renderDataTable(t(df_slope_i()))
          
 
        
        ######### T test ##############

  
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
        
              ### Imputed ####
              
                  pp_t_test_i = reactive({
                    full_data = i_pFEV_lf_r()
                    factor = input$global_factor
                    t1 = input$pre_range[1]
                    t2 = input$post_range[2]
                    df = pp_t_test_function(full_data,factor,t1,t2)
                  })
                  
                  output$pp_t_table_i = renderDataTable(t(pp_t_test_i()))
                  
                  output$boxplot_pp_i = renderPlot({
                    full_data = i_pFEV_lf_r()
                    t1 = input$pre_range[1]
                    t2 = input$post_range[2]
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
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df = df[order(df$Status),]
          df
          
        })
        output$pp_t_table_ratio_full = renderDataTable(t(pp_t_test_ratio_full()))
        output$boxplot_pp_ratio_full = renderPlot({
          full_data = pFEV_lf_r()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
        
        pp_t_test_ratio_full_i = reactive({
          full_data = i_pFEV_lf_r()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          df = pp_t_test_ratio_full_function(full_data,t1,t2)
          df = df[order(df$Status),]
          df
        })
        output$pp_t_table_ratio_full_i = renderDataTable(t(pp_t_test_ratio_full_i()))
        output$boxplot_pp_ratio_full_i = renderPlot({
          full_data = i_pFEV_lf_r()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          p = boxplot_pp_ratio_full_function(full_data,t1,t2)
          print(p)
        })
  
      ### Ratio ####
        pp_t_test_ratio = reactive({
          full_data = pFEV_lf_r()
          factor = input$global_factor
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
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
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
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
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
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
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
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
        t1 = input$pre_range[1]
        t2 = input$post_range[2]
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
        t1 = input$pre_range[1]
        t2 = input$post_range[2]
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
        t1 = input$pre_range[1]
        t2 = input$post_range[2]
        df = pp_t_test_zero_function(full_data,factor,t1,t2)
        df = df[order(df$Status),]
        
      })
      output$pp_t_table_zero_i = DT::renderDataTable({
        datatable(t(pp_t_test_zero_i()),rownames = T)
      })
      
      boxplot_pp_zero_data_i = reactive({
        full_data = i_pFEV_lf_r()
        df_s = pp_t_test_zero_i()
        t1 = input$pre_range[1]
        t2 = input$post_range[2]
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
      #   t1 = input$pre_range[1]
      #   t2 = input$post_range[2]
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
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
            function_data=i_pFEV_sm_d1_fl_r()[i_pFEV_sm_d1_fl_r()$variable %in% cols,]
            df = lm_function(function_data,factor,cols)
            df
          })
          output$lm_table_d1 = renderDataTable(t(df_lm_d1()))
          
          output$boxplot_anova_all_factor_d1 = renderPlot({
            factor = input$global_factor
            full_data = i_pFEV_sm_d1_fl_r()
            cols = c(-6:6)
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
            cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
          
          
          output$slope_fit_pFEV_pre_plot_d1 = renderPlot({
            data = i_pFEV_sm_d1_fl_r()
            cols = factor(c(input$pre_range[1]:input$pre_range[2]))
            slope_fit_plot_function(data,cols,'Pre Treatment',input) 
          })
          output$slope_fit_pFEV_post_plot_d1 = renderPlot({
            data = i_pFEV_sm_d1_fl_r()
            cols = factor(c(input$post_range[1]:input$post_range[2]))
            slope_fit_plot_function(data,cols,'Post Treatment',input) 
          })
          

          
          
          slope_boxplot_data_d1 = reactive(slope_boxplot_data_function(df_lm_sample_d1(),df_slope_d1(),input$global_factor))
          output$slope_boxplot_d1 = renderPlot(slope_boxplot_function(slope_boxplot_data_d1(),input$global_factor))
    ### T test #####
          pp_t_test_d1 = reactive({
            full_data = i_pFEV_sm_d1_fl_r()
            factor = input$global_factor
            t1 = input$pre_range[1]
            t2 = input$post_range[2]
            df = pp_t_test_function(full_data,factor,t1,t2)
            df
          })
          output$pp_t_table_d1 = renderDataTable(t(pp_t_test_d1()))
          output$boxplot_pp_d1 = renderPlot({
            full_data = i_pFEV_sm_d1_fl_r()
            t1 = input$pre_range[1]
            t2 = input$post_range[2]
            global_factor = input$global_factor
            p = boxplot_pp_function(full_data,t1,t2,global_factor)
            p
          })
          
          pp_t_test_ranges_d1 = reactive({
            full_data = i_pFEV_sm_d1_fl_r()
            factor = input$global_factor
            pre1 = input$pre_range[1]
            pre2 = input$pre_range[2]
            post1 = input$post_range[1]
            post2 = input$post_range[2]
            df = pp_t_test_range_function(full_data,factor,pre1,pre2,post1,post2)
          })
          output$pp_t_table_ranges_d1 = renderDataTable(t(pp_t_test_ranges_d1()))
          # output$boxplot_pp_ranges_d1 = renderPlot({
          #   full_data = i_pFEV_sm_d1_fl_r()
          #   df = pp_t_test_ranges_d1()
          #   pre1 = input$pre_range[1]
          #   pre2 = input$pre_range[2]
          #   post1 = input$post_range[1]
          #   post2 = input$post_range[2]
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
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
    cols = factor(c(input$pre_range[1]:input$post_range[2]))
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
    t1 = input$pre_range[1]
    t2 = input$post_range[2]
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
    t1 = input$pre_range[1]
    t2 = input$post_range[2]
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
  
      # cluster_weight = reactive({
      #   full_data = i_pFEV_wf_r()
      #   weights = clustering_weight_function(full_data,retained_patients(),input$clutree_num,
      #                                           input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
      #                                           input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
      #   
      #   test = 'broken'
      #   return(test)
      #   })
      # 
      # 
      discrete_cluster_D = reactive({
        if(input$cluster_imputed == 'original'){
          full_data = pFEV_wf_c()
        }else{
          full_data = i_pFEV_wf_c()
        }
        cluster_data_list = clustering_function(full_data,retained_patients(),input$clutree_num,
                                                input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
                                                input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
        #return(list(data_dist = data_dist, D = D, o_data = o_data, data = data, x_cluster = x_cluster, weights = weights))
        cluster_data_list
      })
  
    

  
  
        ### PLOT CLUSTERS ####
            output$D_text = renderPrint(str(discrete_cluster_D()$D,indent.str = '<br />'))
            
            #output$D = renderText(print(str(discrete_cluster_D()$D)))
            
            output$discrete_cluster_plot_2 = renderPlot({
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
            
            output$discrete_cluster_plot_d1_2 = renderPlot({
              D = discrete_cluster_D_d1()$D
              #plot(D,cex.lab = 0.5)
              
              #ggdendrogram(D)
              #ggdend(D)
              d_num = input$clutree_num
              cols = (ggplotColours(d_num))
              par(lwd = 5)
              D %>% set("branches_k_color", value = cols, k = d_num) %>% 
                plot()
              
              # 
              # D = discrete_cluster_D_d1()$D
              # #plot(D,cex.lab = 0.5)
              # # d_num = input$clutree_num
              # # cols = (ggplotColours(d_num))
              # # par(lwd = 5)
              # # #D %>% set("branches_k_color", value = cols, k = d_num) %>% 
              # # plot(D)
              # 
              # d_num = input$clutree_num
              # dend = D %>% set("branches_k_color", k=d_num) %>% set("branches_lwd", 1.2)
              # 
              # 
              # #plot()
              # ggd1 = as.ggdend(dend) 
              # #ggd1 %>% set("branches_k_color", k=3)
              # ggplot(ggd1) + ylim(-1, max(get_branches_heights(dend)) )
              # 
            })
            
            output$discrete_cluster_plot_3 = renderPlot({
              D = discrete_cluster_D()$D
              #hc <- hclust(data_dist)
              #plot(hc)
              #gg<-rect.hclust(hc,k=5)
              dendr <- dendro_data(D, type="rectangle") 
              
              plot(D)
              #gg<-rect.hclust(hc,k=d)
              d_num = input$clutree_num
              #gg<-rect.hclust(hc,k=d_num)
              #g

              
              #df <- USArrests                       # really bad idea to muck up internal datasets
              #labs <- paste("sta_", 1:50, sep = "") # new labels
              #rownames(df) <- labs                  # set new row names
              
              cut <- d_num    # Number of clusters
              hc <- D              # hierarchical clustering
              dendr <- dendro_data(D, type = "rectangle") 
              clust <- cutree(hc, k = cut)               # find 'cut' clusters
              clust.df <- data.frame(label = names(clust), cluster = clust)
              
              # Split dendrogram into upper grey section and lower coloured section
              height <- unique(dendr$segments$y)[order(unique(dendr$segments$y), decreasing = TRUE)]
              cut.height <- mean(c(height[cut], height[cut-1]))
              dendr$segments$line <- ifelse(dendr$segments$y == dendr$segments$yend &
                                              dendr$segments$y > cut.height, 1, 2)
              dendr$segments$line <- ifelse(dendr$segments$yend  > cut.height, 1, dendr$segments$line)
              
              # Number the clusters
              dendr$segments$cluster <- c(-1, diff(dendr$segments$line))
              change <- which(dendr$segments$cluster == 1)
              for (i in 1:cut) dendr$segments$cluster[change[i]] = i + 1
              dendr$segments$cluster <-  ifelse(dendr$segments$line == 1, 1, 
                                                ifelse(dendr$segments$cluster == 0, NA, dendr$segments$cluster))
              dendr$segments$cluster <- na.locf(dendr$segments$cluster) 
              
              # Consistent numbering between segment$cluster and label$cluster
              clust.df$label <- factor(clust.df$label, levels = levels(dendr$labels$label))
              clust.df <- arrange(clust.df, label)
              clust.df$cluster <- factor((clust.df$cluster), levels = unique(clust.df$cluster), labels = (1:cut) + 1)
              dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")
              
              # Positions for cluster labels
              n.rle <- rle(dendr$segments$cluster)
              N <- cumsum(n.rle$lengths)
              N <- N[seq(1, length(N), 2)] + 1
              N.df <- dendr$segments[N, ]
              N.df$cluster <- N.df$cluster - 1
              
              # Plot the dendrogram
              ggplot() + 
                geom_segment(data = segment(dendr), 
                             aes(x=x, y=y, xend=xend, yend=yend, size=factor(line), colour=factor(cluster)), 
                             lineend = "square", show.legend = FALSE) + 
                scale_colour_manual(values = c("grey60", rainbow(cut))) +
                scale_size_manual(values = c(.1, 1)) +
                geom_text(data = N.df, aes(x = x, y = y, label = factor(cluster),  colour = factor(cluster + 1)), 
                          hjust = 1.5, show.legend = FALSE) +
                geom_text(data = label(dendr), aes(x, y, label = label, colour = factor(cluster)), 
                          hjust = -0.2, size = 3, show.legend = FALSE) +
                #scale_y_reverse(expand = c(0.2, 0)) + 
                labs(x = NULL, y = NULL) +
                #coord_flip() +
                #coord_flip() +
                ylim(-2, 5 )+
                theme(axis.line.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank(),
                      panel.background = element_rect(fill = "white"),
                      panel.grid = element_blank())
            
              })
            
            clust_org_d1 = reactive({
              D = discrete_cluster_D_d1()$D
              dendr = discrete_cluster_D_d1()$dendr
              clust = discrete_cluster_D_d1()$x
              cut = 3
              hc = D
              #hc <- hclust(data_dist)
              #plot(hc)
              #gg<-rect.hclust(hc,k=5)
              dendr <- dendro_data(D, type="rectangle") 
              
              #plot(D)
              #gg<-rect.hclust(hc,k=d)
              cut = input$clutree_num
              #gg<-rect.hclust(hc,k=d_num)
              #g
              
              
              #df <- USArrests                       # really bad idea to muck up internal datasets
              #labs <- paste("sta_", 1:50, sep = "") # new labels
              #rownames(df) <- labs                  # set new row names
              
              #cut <- d_num    # Number of clusters
              #hc <- D              # hierarchical clustering
              #dendr <- dendro_data(D, type = "rectangle") 
              clust <- cutree(hc, k = cut)               # find 'cut' clusters
              clust.df <- data.frame(label = names(clust), cluster = clust)
              
              # Split dendrogram into upper grey section and lower coloured section
              height <- unique(dendr$segments$y)[order(unique(dendr$segments$y), decreasing = TRUE)]
              cut.height <- mean(c(height[cut], height[cut-1]))
              dendr$segments$line <- ifelse(dendr$segments$y == dendr$segments$yend &
                                              dendr$segments$y > cut.height, 1, 2)
              dendr$segments$line <- ifelse(dendr$segments$yend  > cut.height, 1, dendr$segments$line)
              
              # Number the clusters
              dendr$segments$cluster <- c(-1, diff(dendr$segments$line))
              change <- which(dendr$segments$cluster == 1)
              for (i in 1:cut) dendr$segments$cluster[change[i]] = i + 1
              dendr$segments$cluster <-  ifelse(dendr$segments$line == 1, 1, 
                                                ifelse(dendr$segments$cluster == 0, NA, dendr$segments$cluster))
              dendr$segments$cluster <- na.locf(dendr$segments$cluster) 
              
              # Consistent numbering between segment$cluster and label$cluster
              clust.df$label <- factor(clust.df$label, levels = levels(dendr$labels$label))
              clust.df <- arrange(clust.df, label)
              clust.df$cluster <- factor((clust.df$cluster), levels = unique(clust.df$cluster), labels = (1:cut) + 1)
              dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")
              
              # Positions for cluster labels
              n.rle <- rle(dendr$segments$cluster)
              N <- cumsum(n.rle$lengths)
              N <- N[seq(1, length(N), 2)] + 1
              N.df <- dendr$segments[N, ]
              N.df$cluster <- N.df$cluster - 1
              list(dendr = dendr, N.df = N.df)
            })
            
          output$discrete_cluster_plot = renderPlot({
              
              D = discrete_cluster_D()$D
              dendr <- dendro_data(D, type = "rectangle") 
              x_cluster = discrete_cluster_D()$x_cluster
              cut = input$clutree_num
              p = dendrogram_plot_function(dendr,x_cluster,cut)
              print(p)
              

            })  
          
          output$discrete_cluster_plot_d1 = renderPlot({
            
            D = discrete_cluster_D_d1()$D
            dendr <- dendro_data(D, type = "rectangle") 
            x_cluster = discrete_cluster_D_d1()$x_cluster
            cut = input$clutree_num
            p = dendrogram_plot_function(dendr,x_cluster,cut)
            print(p)
               })
          

          
          output$discrete_cluster_plot_d1_2 = renderPlot({
            dendr = clust_org_d1()$dendr
            N.df = clust_org_d1()$N.df
              # Plot the dendrogram
            
              #ggplot() +
              #  geom_segment(data = segment(dendr), aes(x=x, y=y, xend=xend, yend=yend, size=factor(line), colour=factor(cluster)))
            
              ggplot() + 
                geom_segment(data = segment(dendr), 
                             aes(x=x, y=y, xend=xend, yend=yend, size=factor(line), colour=factor(cluster)), 
                             lineend = "square", show.legend = FALSE) + 
                scale_colour_manual(values = c("grey60", rainbow(cut))) +
                scale_size_manual(values = c(.1, 1)) +
                geom_text(data = N.df, aes(x = x, y = y, label = factor(cluster),  colour = factor(cluster + 1)), 
                          hjust = 1.5, show.legend = FALSE) +
                geom_text(data = label(dendr), aes(x, y, label = label, colour = factor(cluster)), 
                          hjust = -0.2, size = 3, show.legend = FALSE) +
                #scale_y_reverse(expand = c(0.2, 0)) + 
                labs(x = NULL, y = NULL)
                #coord_flip() +
                #coord_flip() +
                #ylim(-2, 5 )+
                
                # theme(axis.line.y = element_blank(),
                #       axis.ticks.y = element_blank(),
                #       axis.text.y = element_blank(),
                #       axis.title.y = element_blank(),
                #       panel.background = element_rect(fill = "white"),
                #       panel.grid = element_blank())
              
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
  
  
  
    ####### D1 Clustering ############
            
            discrete_cluster_D_d1 = reactive({
              if(input$cluster_change_imputed == 'imputed'){
                full_data = i_pFEV_sm_d1_f_c()
              }else{
                full_data = i_pFEV_sm_d1_f_c_ir()
              }
              
              
            #discrete_cluster_D_d1 = reactive({
             # full_data = i_pFEV_sm_d1_f
              cluster_data_list = clustering_function(full_data,retained_patients(),input$clutree_num,
                                                      input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
                                                      input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
              cluster_data_list
            })
  
 
            ### PLOT CLUSTERS ####
            


            
            output$D_d1_text = renderPrint(str(discrete_cluster_D_d1()$D,indent.str = '<br />'))
            
            output$discrete_x_table_d1 = renderDataTable({
              #discrete_cluster_D_d1()$x_cluster
              x_cluster = discrete_cluster_D_d1()$x_cluster
              #x_cluster = clust_org_d1()$dendr$label
              x_cluster_table = data.frame(num = numeric(0),MRN = numeric(0))
              for(entry in unique(x_cluster$cluster)){
                line_list  = x_cluster[x_cluster$cluster == entry,'label']
                line = paste(line_list,collapse = (', '))
                line
                x_cluster_table[entry,'num'] = length(line_list)
                x_cluster_table[entry,'MRN'] = line
              }
             t(x_cluster_table)
            #})
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
        
            
            #### DISTANCE SCATTER PLOTS ####
            

     
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
            
            
            ## D1 ##
            distance_model_d1 = reactive({
              data_dist = discrete_cluster_D_d1()$data_dist
              x = discrete_cluster_D_d1()$x
              cm = cmdscale(data_dist)
              xy <- data.frame(cm, factor(x))
              names(xy) <- c("x", "y", "cluster")
              xy$model <- rownames(xy)
              xy
            })
            output$distance_scatter_d1 = renderPlot({
              xy = distance_model_d1()
              ggplot(xy, aes(x, y, colour=cluster)) + 
                geom_point(size=3)
            })
            output$distance_density_d1 = renderPlot({
              xy = distance_model_d1()
              ggplot(xy, aes(x, y, colour=cluster)) + 
                geom_point( size=3) +
                geom_density2d(alpha=0.5)
            })
            output$distance_polygon_d1 = renderPlot({
              xy = distance_model_d1()
              ggplot(xy, aes(x, y, colour=cluster,fill = cluster)) + 
                geom_point(size=3) +
                geom_polygon(alpha = 0.5)
            })
            output$distance_polygon_neat_d1 = renderPlot({
              mydata = distance_model_d1()
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
            
            output$distance_table_d1 = renderDataTable(as.matrix(discrete_cluster_D_d1()$data_dist))
            output$distance_model_table_d1 = renderDataTable(distance_model_d1())
            
            

            
            
    #### CLUSTER COMPOSITION TABLES #####

            
            #output$test_text_2 = renderPrint(cluster_analysis_within_table_selected_df())
            
            #output$test_text_3 = renderPrint(input$cluster_select_factors)
    
          #### TOTAL ----
            # cluster ####
          cluster_analysis_total = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_total(df,'cluster')
            df_tc
          })
          #output$cluster_analysis = renderDataTable(cluster_analysis_total())
          
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
          
          #output$cluster_analyis_selected_table = renderDataTable({cluster_analyis_selected_df()})
          
          output$cluster_analyis_selected_table = DT::renderDataTable({
            df = cluster_analyis_selected_df()
            colour = 'lightgreen'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          
          

          

            # cluster d1 ####
          
          cluster_analysis_total_d1 = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_total(df,'cluster_d1')
            #p = apply(df_tc,1, function(x) chisq.test(x[c(3:length(x))])$p.value)
            #print(p)
            df_tc
          })
          
          
          #output$cluster_analysis_d1 = renderDataTable(cluster_analysis_total_d1())
          
          output$cluster_analysis_d1 = DT::renderDataTable({
            df = cluster_analysis_total_d1()
            colour = 'lightgreen'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          
          cluster_analyis_selected_df_d1 = reactive({
            df = cluster_analysis_total_d1()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })
          

          output$cluster_analyis_selected_table_d1 = DT::renderDataTable({
            df = cluster_analyis_selected_df_d1()
            colour = 'lightgreen'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          output$chisq_cluster_d1 = renderPrint({
            data = cluster_analyis_selected_df_d1()
            test_data = data[,c(3:dim(data)[2])]
            test_data[is.na(test_data)] = 0
            
            chisq.test(test_data)
          })
          
          
          ### WITHIN ####
            ## cluster ----
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
          
            ### D1 ----
          cluster_analysis_within_d1 = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_within(df,'cluster_d1')
            df_tc
            #df_tc$p
          })
          
          output$cluster_analysis_within_d1_table = DT::renderDataTable({
            df = cluster_analysis_within_d1()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          
          cluster_analysis_within_table_selected_df_d1 = reactive({
            df = cluster_analysis_within_d1()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })
          
          output$cluster_analysis_within_table_selected_table_d1 = DT::renderDataTable({
            df = cluster_analysis_within_table_selected_df_d1()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            proportion_table_formating(df,col_range,colour)
          })
          

          output$chisq_cluster_within_d1 = renderPrint({
            data = cluster_analysis_within_table_selected_df_d1()
            test_data = data[,c(3:dim(data)[2])]
            test_data[is.na(test_data)] = 0
            
            chisq.test(test_data)
          })
          
          
          ### CHISQ ####
          
          # inputs =====
          
          output$cluster_select_clusters <- renderUI({
            data = cluster_analysis_within_table_selected_df()
            test_data = data[,c(3:dim(data)[2])]
            clusters <- colnames(test_data)
            selectInput("cluster_select_clusters", "Choose Clusters to Test using ChiSQ", choices = clusters, selected = clusters,multiple = T)
          })
          
          output$cluster_select_clusters_d1 <- renderUI({
            data = cluster_analysis_within_table_selected_df()
            test_data = data[,c(3:dim(data)[2])]
            clusters <- colnames(test_data)
            selectInput("cluster_select_clusters", "Choose Clusters to Test using ChiSQ", choices = clusters, selected = clusters,multiple = T)
          })

          output$chisq_cluster = renderDataTable({
            full_data = cluster_analyis_selected_df()
            
            chi_df = chisq_total(full_data,input)
            chi_df
            
            
            
          })
          output$chisq_cluster_within = renderDataTable({
            data = cluster_analysis_within_table_selected_df()
            
            chi_df = chisq_within(data,input)
            chi_df
            
            
          })
        
          output$chisq_cluster_d1 = renderDataTable({
            full_data = cluster_analyis_selected_df_d1()
            
            chi_df = chisq_total(full_data,input)
            chi_df
            
            
            
          })
          output$chisq_cluster_within_d1 = renderDataTable({
            data = cluster_analysis_within_table_selected_df_d1()
            
            chi_df = chisq_within(data,input)
            chi_df
            
            
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
            #View(df)
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
            #print(p)
            p


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
          
          output$bos3_surv_factor_plot_cluster = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = 'cluster_d1'
            m_bos = bos_factor()
            col_name = 'BOS3_surv_free'
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
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
            #View(df)
            m_bos = melt(df,id.vars= c('Factor','Status','time'))
            m_bos
            #View(m_bos)
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
          
          bos_factor_cluster_d1 = reactive({
            full_data = pFEV_wf_r()
            #global_factor = 'Status'
            global_factor = 'cluster_d1'
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
            #View(df)
            m_bos = melt(df,id.vars= c('Factor','Status','time'))
            m_bos
          })
          
          output$bos3_surv_factor_plot_cluster_d1 = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = 'cluster_d1'
            m_bos = bos_factor_cluster_d1()
            col_name = 'BOS3_surv_free'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
          


          
############ AUTOPLOT ################## MESS #########          
          
          output$BOS_plot = renderPlot({
            plot_data = pFEV_wf
            colnames(plot_data)
            plot_data = pFEV_wf_r()
            boss_data = plot_data[c('MonthsToEvent','BOS1mnth','BOS2mnth','BOS3mnth')]
            m_bos = melt(boss_data)
            m_bos
            
            f1 <- survfit(Surv(value) ~ variable, data = m_bos)
            #ggkm(f1)
            autoplot(f1,surv.geom = "line", surv.connect = FALSE) + 
              geom_vline(aes(xintercept =  0)) +
              xlim(-25,50)
              
        
              #ggsurvplot_list(f1,data = m_bos)
          })
          
          #### LINE ###
          
          BOS_all = reactive({
            plot_data = pFEV_wf
            global_factor = 'Status'
            x1 = -24
            x2 = 24
            colnames(plot_data)
            plot_data = pFEV_wf_r()
            boss_data = plot_data[c('MonthsToEvent', "BOS 3 free survival", 'BOS1mnth','BOS2mnth','BOS3mnth')]
            m_bos = melt(boss_data)
            m_bos
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            sfit <- survfit(Surv(value) ~ variable, data = m_bos, type="kaplan-meier", conf.type="log")
            sdiff = survdiff(Surv(value) ~ variable, data = m_bos)
            list(sfit = sfit,sdiff = sdiff)
          })
          
          output$BOS_all_summary = renderText({
            print('test')
            paste(summary(BOS_all()$sfit[1]))
          })
          
          output$BOS_plot_l = renderPlot({
            sfit = BOS_all()$sfit
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            
            autoplot(sfit,surv.geom = "line", surv.connect = FALSE,conf.int = FALSE) + 
              geom_vline(aes(xintercept =  0)) +
              xlim(x1,x2)
          })
          
          output$survival_factor_l = renderPlot({
            plot_data = pFEV_wf_r()
            global_factor = input$global_factor
            #print(global_factor)
            colnames(plot_data)
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            fit <- survfit(Surv(plot_data$MonthsToEvent) ~ plot_data[,global_factor], data = plot_data, type="kaplan-meier", conf.type="log")
            
            #fit <- survfit(Surv(BOS1mnth) ~ as.character(global_factor), data = plot_data)
            autoplot(fit,surv.geom = "line",surv.connect = FALSE,conf.int = FALSE) + 
              ggtitle(paste('Survival by ',global_factor)) +
              geom_vline(aes(xintercept =  0)) +
              xlim(x1,x2)
          })
          
          output$boss_3_free_factor_l = renderPlot({
            plot_data = pFEV_wf_r()
            global_factor = input$global_factor
            #print(global_factor)
            colnames(plot_data)
            x1 = input$bos_range[1]
            x2 = input$bos_range[2]
            fit <- survfit(Surv(plot_data[,"BOS 3 free survival"]) ~ plot_data[,global_factor], data = plot_data, type="kaplan-meier", conf.type="log")
            
            #fit <- survfit(Surv(BOS1mnth) ~ as.character(global_factor), data = plot_data)
            autoplot(fit,surv.geom = "line", surv.connect = FALSE,conf.int = FALSE) + 
              ggtitle(paste('BOS 3 free survival by',global_factor)) +
              geom_vline(aes(xintercept =  0)) +
              xlim(x1,x2)
          })
          
          output$boss_2_factor_l = renderPlot({
            plot_data = pFEV_wf_r()
            global_factor = input$global_factor
            #print(global_factor)
            colnames(plot_data)           
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            fit <- survfit(Surv(plot_data$BOS2mnth) ~ plot_data[,global_factor], data = plot_data, type="kaplan-meier", conf.type="log")
            
            #fit <- survfit(Surv(BOS1mnth) ~ as.character(global_factor), data = plot_data)
            autoplot(fit,surv.geom = "line", surv.connect = FALSE,conf.int = FALSE) + 
              ggtitle(paste('BOSS 2 by',global_factor)) +
              geom_vline(aes(xintercept =  0)) +
              xlim(x1,x2)
          })
          
          output$boss_3_factor_l = renderPlot({
            plot_data = pFEV_wf_r()
            global_factor = input$global_factor
            #print(global_factor)
            colnames(plot_data)
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            fit <- survfit(Surv(plot_data$BOS3mnth) ~ plot_data[,global_factor], data = plot_data, type="kaplan-meier", conf.type="log")
            
            #fit <- survfit(Surv(BOS1mnth) ~ as.character(global_factor), data = plot_data)
            autoplot(fit,surv.geom = "line", surv.connect = FALSE,conf.int = FALSE) + 
              ggtitle(paste('BOSS 3 by',global_factor)) +
              geom_vline(aes(xintercept =  0)) +
              xlim(x1,x2)
          })
          
##### MORE STATS #########
    ###### LOGISTIC REGRESSION ##########
      output$lg_scatter = renderPlot({
        data = pFEV_wf
        colnames(data)
        binary_factor = 'Status'
        add_factor = '`BOS 3 free survival`'
        
        data = pFEV_wf_r()
        binary_factor = input$binary_factor
        add_factor = input$add_factor[1]
        
        ggplot(data = data, aes_string(y = binary_factor,x=add_factor)) + geom_jitter()
        #data$Status[data$Status == '1'] = '0'
        #data$Status[data$Status == 2] = 1
        #data$Status
        #fit = glm(Status~BOS2mnth, data = data,family=binomial)
        #summary(fit)
      })    
      
      lg = reactive({
        data = pFEV_lf
    
        data = pFEV_wf_r()
        binary_factor = input$binary_factor
        add_factor = input$add_factor
        add_factor_add = paste(add_factor,collapse = '+')
        add_factor_add
        cmd = paste('fit = glm(',binary_factor,'~',add_factor_add,', data = data,family=binomial)')
        cmd
        eval(parse(text = cmd))
        fit
        #str(tidy(fit))
      })
          
      output$logistic_regression_text = renderPrint(summary(lg()))
      output$logistic_regression_table = renderDataTable(tidy(lg()))
      
      
      
      output$logistic_regression_p_hist = renderPlot({
        tidy(lg()) %>% 
          filter(term!="(Intercept)") %>% 
          mutate(logp=-1*log10(p.value)) %>% 
          ggplot(aes(term, logp)) + geom_bar(stat="identity") + coord_flip()
      })
      
      xt = reactive({
        data = pFEV_wf
        fac1 = "Status"
        fac2 = "HLAType"
        data = pFEV_wf_r()
        fac1 = input$binary_factor
        fac2 = input$add_factor[1]
        
        cmd = paste('xt = xtabs(~',fac1,'+',fac2,', data = data)')
        #print(cmd)
        eval(parse(text = cmd))
        xt
      })
      
      output$chisq_xt = renderPrint(chisq.test(xt()))
      output$fisher_xt = renderPrint(fisher.test(xt()))
      output$mosaic_xt = renderPlot(mosaicplot(xt()))
      
      output$test_text_2 = renderPrint({
        global_factor = 'Status'
        global_factor = input$global_factor
        plot_data = pFEV_lf_r()
        qplot(plot_data, aes_string(x = 'time', y = 'value',group = global_factor, col = global_factor )) + 
          #geom_vline(xintercept = 0) +
          stat_smooth(outfit=fit<<-..y..)
          #geom_point() +
          #stat_summary(data = plot_data, fun.y=mean,geom="line",lwd=3,aes_string(x = 'time', y = 'value',group=input$global_factor,col = input$global_factor)) +
          
          #theme(axis.text.x = element_text(size=8, angle=90)) +
          #xlim(input$pre_range[1],input$post_range[2]) +
          #ggtitle(title)
        print(fit)
      })
      
      #### MANOVA ####
        ##cluster  -------

      
      
      output$boxplot_pFEV_manova_cluster = renderPlot({
        full_data = pFEV_lf_r()
        global_factor = 'cluster'
        title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
        cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
        
        boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
        
      })
      output$cluster_pairwise_manova_table= renderDataTable({
        data = pFEV_lf_r()
        m_factor = 'cluster'
        function_data = data[data$variable %in% c(input$mix_clust_col_num,input$mix_clust_col_num_2),]
        df = pairwise_manova_function(function_data,m_factor)
        df
      })

      
      output$boxplot_change_manova_cluster_d1 = renderPlot({
        full_data = i_pFEV_sm_d1_fl_r()
        global_factor = 'cluster_d1'
        title = paste('Change D1 values for ',length(unique(full_data$MRN))," Patients")
        cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
        
        boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
        
      })
      output$cluster_change_pairwise_manova_table_d1= renderDataTable({
        data = i_pFEV_sm_d1_fl_r()
        function_data = data[data$variable %in% c(input$mix_clust_col_num,input$mix_clust_col_num_2),]
        m_factor = 'cluster_d1'
        df = pairwise_manova_function(function_data,m_factor)
        df
      })
      
      output$boxplot_pFEV_manova_cluster_d1 = renderPlot({
        full_data = pFEV_lf_r()
        global_factor = 'cluster_d1'
        title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
        cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
        
        boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
        
      })
      output$cluster_pFEV_pairwise_manova_table_d1= renderDataTable({
        data = pFEV_lf_r()
        function_data = data[data$variable %in% c(input$mix_clust_col_num,input$mix_clust_col_num_2),]
        m_factor = 'cluster_d1'
        df = pairwise_manova_function(function_data,m_factor)
        df
      })
      
      # pFEV --------
      
      output$full_manova_table = renderDataTable({
        data = pFEV_lf
        cols = factor(c(-6,6))
        data = pFEV_lf_r()
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,factor_colums_4_comparisons[1])
        #df
        for(m_factor in factor_colums_4_comparisons[-1]){
          df_n = tryCatch(pairwise_manova_function(function_data,m_factor), error = function(e) e = data.frame(d = numeric(0)))
          #print(df_n)

          if(dim(df_n)[1] > 0){
            df = rbind(df,df_n)
          }
        }
        df
      })
      output$full_manova_table_i = renderDataTable({
        data = pFEV_lf
        cols = factor(c(-6,6))
        data = i_pFEV_lf_r()
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,factor_colums_4_comparisons[1])

        for(m_factor in factor_colums_4_comparisons[-1]){

          df_n = tryCatch(pairwise_manova_function(function_data,m_factor), error = function(e) e = data.frame(d = numeric(0)))

          if(dim(df_n)[1] > 0){
            df = rbind(df,df_n)
          }
        }

        df
      })

      
      output$boxplot_pFEV_manova = renderPlot({
        full_data = pFEV_lf_r()
        title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
        boxplot_function(full_data,title,input)
      })
      output$boxplot_pFEV_manova_i = renderPlot({
        full_data = i_pFEV_lf_r()
        title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
        boxplot_function(full_data,title,input)
      })

      output$selected_manova_table = renderDataTable({
        data = pFEV_lf
        data = pFEV_lf_r()
        global_factor = 'HLAStrength'
        global_factor = input$global_factor
        cols = factor(c(-6,6))
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,global_factor)
      })
      output$selected_manova_table_i = renderDataTable({
        data = pFEV_lf
        data = i_pFEV_lf_r()
        global_factor = 'HLAStrength'
        global_factor = input$global_factor
        cols = factor(c(-6,6))
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,global_factor)
      })
      
      
      # D1 ----
      output$boxplot_pFEV_manova_d1 = renderPlot({
        full_data = i_pFEV_sm_d1_fl_r()
        title = paste('pFEV values for ',length(unique(full_data$MRN))," Patients")
        boxplot_function(full_data,title,input)
      })
      
      output$selected_manova_table_d1 = renderDataTable({
        data = pFEV_lf
        data = i_pFEV_sm_d1_fl_r()
        global_factor = 'HLAStrength'
        global_factor = input$global_factor
        cols = factor(c(-6,6))
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,global_factor)
      })
      
      
      output$full_manova_table_d1 = renderDataTable({
        data = pFEV_lf
        cols = factor(c(-6,6))
        data = i_pFEV_sm_d1_fl_r()
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,factor_colums_4_comparisons[1])
        for(m_factor in factor_colums_4_comparisons[-1]){
          df_n = tryCatch(pairwise_manova_function(function_data,m_factor), error = function(e) e = data.frame(d = numeric(0)))

          if(dim(df_n)[1] > 0){
            df = rbind(df,df_n)
          }
        }
        df
      })
      
      
#         
#         ggplot(data, aes(x = time, y = value, group = Status, col = Status)) + 
#           geom_smooth(method = 'loess', aes(outfit=fit<<-..y..))
#         
#         
#         colnames(data)
#         model_data = data[data$Status == 1,]
#         model = loess(value ~ time, data = model_data)
#         modelFit <- data.frame(predict(model, se = TRUE))
#         df <- data.frame(cbind(time = model_data$time
#                                , value = model_data$value
#                                , fit = modelFit$fit
#                                , upperBound = modelFit$fit + 2 * modelFit$se.fit
#                                , lowerBound = modelFit$fit - 2 * modelFit$se.fit
#         ))
#         df
#         g <- ggplot(df, aes(time,value))
#         g <- g + geom_point()
#         g <- g + geom_linerange(aes(ymin = lowerBound, ymax = upperBound))
#         g <- g + geom_point(aes(time, fit, size = 1))
#         g <- g + geom_smooth(method = "loess")
#         g
#         print(model)
#         newdata = data[data$Status == 2,]
#         predict(model, newdata, se = TRUE)
#         
# 
#         fit = lm(y~x)
#         #fit first degree polynomial equation:
#         fit  <- lm(y~x)
#         #second degree
#         fit2 <- lm(y~poly(x,2,raw=TRUE))
#         #third degree
#         fit3 <- lm(y~poly(x,3,raw=TRUE))
#         #fourth degree
#         fit4 <- lm(y~poly(x,4,raw=TRUE))
#         #generate range of 50 numbers starting from 30 and ending at 160
#         xx <- seq(-24,48,1)
#         plot(x,y,pch=19,ylim=c(0,1))
#         lines(xx, predict(fit, data.frame(x=xx)), col="red")
#         lines(xx, predict(fit2, data.frame(x=xx)), col="green")
#         lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
#         lines(xx, predict(fit4, data.frame(x=xx)), col="purple")
#         
# 
#         
#         x = model_2_data$time
#         y = model_2_data$value
#         fit = lm(y~x)
#         #fit first degree polynomial equation:
#         fit  <- lm(y~x)
#         #second degree
#         fit2 <- lm(y~poly(x,2,raw=TRUE))
#         #third degree
#         fit3 <- lm(y~poly(x,3,raw=TRUE))
#         #fourth degree
#         fit4 <- lm(y~poly(x,4,raw=TRUE))
#         
#         model_data_1 = data[data$Status == 1,]
#         x1 = model_data$time
#         y1 = model_data$value
#         glm_1 = glm(y1~x1)
#         lines(xx, predict(glm_1, data.frame(x=xx)), col="green")
#         
#         goal_1 = goal(y~x)
#         ?gl
#         library(mgcv)
#         gam_1 = gam(x1~y1)
#         summary(gam_1)
#         plot(gam1(X))
#         lines(xx, predict(gam_1, data.frame(x=xx)), col="orange")
#         plot.gam(gam_1,pages = 1, scale = 0)
#         
#         gam_2 = gam(y2~x2)
#         
#         plot(gam_2)
#         anova(gam_1,gam_2)
#         
#         s1 = s(y1~x1)
#         s1
#         plot(s1)
#         lo_1 = loess(y1~x1)
#         summary(lo_1)
#         model_data_2 = data[data$Status == 2,]
#         x2 = model_data_2$time
#         y2 = model_data_2$value
#         
#         lo_2 = loess(y2~x2)
#         summary(lo_2)
#         plot(x1,y1,col = 'red')
#         points(x2,y2,col = 'blue')
#         xl_1 <- seq(min(x),max(x), (max(x) - min(x))/1000)
#         lines(xl_1, predict(lo_1,xl_1), col='red', lwd=2)
#         
#         xl_2 <- seq(min(x),max(x), (max(x) - min(x))/1000)
#         lines(xl_2, predict(lo_2,xl_2), col='blue', lwd=2)
#         
#         anova(lo_1,lo_2)
#         
#         gllines(predict(lo))
#         
#         summary(lo)
#         
#         library(splines)
#         fit_1 <- lm(y1~bs(x1,5))
#         fit_2 <- lm(y2~bs(x2,5))
#         anova(fit_1,fit_2)
#         
#         summary(fit2)
#         #lines(fit2)
#         plot(x1,y1,col = 'red')
#         points(x2,y2,col = 'blue')
#         xx1 <- seq(-24,48,length.out = 311*5)
#         length(xx1)
#         yy1 <- predict(fit_1, data.frame(x=xx1))
#         length(yy1)
#         lines(xx1,yy1, col='red')  
#         xx2 <- seq(-24,48,length.out = 2701)
#         
#         yy2 <- predict(fit_2, data.frame(x=xx))
#         length(yy2)
#         lines(xx2,yy2, col='blue') 
#         
#         
#         
#        
#         length(xx1)
#         yy1 <- predict(fit_1)
#         xx1 <- seq(-24,48,length.out = length(yy1))
#         length(yy1)
#         lines(xx1,yy1, col='red') 
#         
#         xx2 <- seq(-24,48,length.out = 363)
#         
#         yy2 <- predict(fit_2)
#         length(yy2)
#         lines(xx2,yy2, col='blue') 
#         anova(fit_1,fit_2)
#         
#         predict(fit_1, model_data_2)
#         
#         poly(x, 4, raw = TRUE)
#         
#         data = i_pFEV_lf
#         
#         model_data_1 = data[data$Status == 1,]
#         x1 = model_data$time
#         y1 = model_data$value
#         
#         model_data_2 = data[data$Status == 2,]
#         x2 = model_data_2$time
#         y2 = model_data_2$value
#         
#         qfit_1 = qda(variable ~ value, data = model_data_1)
#         summary(qfit_1)
#         qfit_2 = qda(variable ~ value, data = model_data_2)
#         summary(qfit_1)
#         
#         library(klaR)
#         partimat(variable~value+Status,data=data,method="qda") 
#         
#         anova(qfit_1, qfit_2)
#         
#         qfit_1
#         plot(qfit_1)
#         
#         st_fit = stl(data$time, data$value)
#         })
#       
#       
#           
# # 
# # ######## TEST ###########
# #           output$test_plot = renderPlot({
# #             data = t(discrete_cluster_D_d1()$o_data)
# #             library(pvclust)
# #             result <- pvclust(data, method.dist="euclidian", method.hclust="average", nboot=1000)
# #             plot(result)
# #             #pvrect(result, alpha=0.95)
# #             #seplot(result)
# #             #seplot(result, identify=TRUE)
# #           })
# #   
# #         ################## END ##########          
# # })
#       
      
})


on.exit(rm(list= ls()))