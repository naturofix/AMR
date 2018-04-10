


library(shiny)
shinyServer(function(input, output) {
  
  r_values = reactiveValues(init = 0)
  
  output$live_ui = renderUI({
    if(read_workspace == T){
      tags$h6('Data loaded from saved workspace')
    }else{
      tags$h6(paste(google_sheets_file,last_updated,gs_updated,sep = ' : '))
    }
  })
  
  output$moreControls <- renderUI({
    tagList(
      sliderInput("n", "N", 1, 1000, 500),
      textInput("label", "Label")
    )
  })

  ########## TEXT OUTPUTS ##################
  
  output$missing_columns = renderPrint({
    paste(all_columns[!all_columns %in% colnames(clustering)],collapse = ', ')
    
  })
  
  output$additional_columns = renderPrint({
    paste(setdiff(colnames(clustering),all_columns),collapse = ', ')
    
  })

  output$duplicated_samples = renderPrint({
    paste(unique(dups),collapse = ', ')
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
  output$manova_clustering_text = renderText(paste('MANOVA for ',input$data_select_clust,'data'))
  
  
  ########## DATA TABLES #####################
   #### DATA TABLES ####
    output$data_table_ui = renderUI({
     
      #tabPanel("Data Tables",
            #display_data_tables = T
            if(display_data_tables == T){ 
               tabsetPanel(
                 tabPanel('Original',
                          tags$h5('Original Data downloaded from googlesheet'),
                          dataTableOutput('clustering')),
                 tabPanel('Defaults',dataTableOutput('gs_defaults')),
                 tabPanel('Processed',
                          tags$h5('Data after processing into R data types'),
                          dataTableOutput('full_num')),
                 tabPanel('Term Mapping',
                          tags$h5('Mapping of discrete character factors to discrete numeric factors'),
                          tableOutput('term_mapping')),
                 
                 
                 # tabPanel('pFEV',dataTableOutput('pFEV_wf')),
                 # tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf')),
                 # tabPanel('Smoothed',dataTableOutput('i_pFEV_sm_lf')),
                 # #i_pFEV_sm_lf_r
                 # #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
                 # tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f")),
                 # tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f")),
                 
                 tabPanel('Selected Data',
                          tags$h5('Data after generation ratio and percentage calculations as well as clustering'),
                          tags$h6('D1 : first differential of imputed smoothed data'),
                          tags$h6('log2zero: log2(treatment/-x) or log2(x/treatment)'), 
                          tags$h6('per2zero : percentage change to or from treatment'), 
                          tags$h6('log2 : log(x/-x)'),
                          tags$h6('per : precentage change from -x to x'),
                          tags$h6('per_rel : change between percentage change before treamtment and percentage change after treatment'),
                          dataTableOutput('pFEV_wf_r')),
                 #tabPanel('pFEV_l',dataTableOutput('pFEV_lf_r')),
                 
                 #tabPanel('Change Data', dataTableOutput('change_data')),
                 #tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf_r')),
                 #tabPanel('Smoothed',dataTableOutput('i_pFEV_sm_lf_r')),
                 #i_pFEV_sm_lf_r
                 #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
                 #tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r")),
                 #tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f_r")),
                 tabPanel('cluster',
                          tags$h5('Data used to the generate the clusters'),
                          dataTableOutput("cluster_data")),
                 #tabPanel('cluster_d1',dataTableOutput("cluster_data_d1"))
                 
                 #tabPanel('lm',dataTableOutput("df_lm_table")),
                 #tabPanel('lm imputed',dataTableOutput("df_lm_table_i"))
                 #tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r"))
                 #i_pFEV_sm_d1_f
                 
                 tabPanel('log 2 ratio vs zero',
                          dataTableOutput("pFEV_ratio2zero")),
                 tabPanel('precentage change vs zero',
                          dataTableOutput("pFEV_per2zero")),
                 tabPanel('Summary Table',
                          sliderInput('summary_slider','Select Month',min = -24,max=24,step = 1,value = c(-6,6), width = 800),
                          dataTableOutput('summary_table'))
               )
            }else{
              tags$h3('Data Tables have been hidden to protect the privacy of the data')
            }
      #)
    })
  
    output$clustering = renderDataTable(clustering)
    output$gs_defaults = renderDataTable(default_df)
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
    
    output$data_matrix = renderDataTable({
      df = processed_data
      colnames(processed_data)
      df = change_data_w()
      print(select_matrix())
      print(colnames(df))
      df_m = df[,select_matrix()]
      df_m
    })

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
              
              output$status_text = renderText(paste0('Select patients by Status, if Dead is selected only those dead at the post-treatment day ',input$post_range[2],' are removed. This timepoint is adjusted using the post-treatment range slider.'))
            
              output$out_select_factor_1 = renderUI({
                if(input$subset_1 != 'All'){
                  select_factor_list = unique(pFEV_wf[,input$subset_1])
                  select_factor_list = select_factor_list[order(select_factor_list)]
                  if(select_subset_1 != 'All'){
                    selected_factor_selected = select_subset_1
                  }else{
                    selected_factor_selected = select_factor_list
                  }
                  selectInput('select_subset_1','Subset Selection 1',choices = select_factor_list,multiple=T,selected = selected_factor_selected)
                }
              })
    
              output$out_select_factor_2 = renderUI({
                if(input$subset_2 != 'All'){
                  select_factor_list = unique(pFEV_wf[,input$subset_2])
                  select_factor_list = select_factor_list[order(select_factor_list)]
                  if(select_subset_2 != 'All'){
                    selected_factor_selected = select_subset_2
                  }else{
                    selected_factor_selected = select_factor_list
                  }
                  
                  selectInput('select_subset_2','Subset Selection 2',choices = select_factor_list,multiple=T,selected = selected_factor_selected)
                }
              })
              
              output$out_select_factor_3 = renderUI({
                if(input$subset_3 != 'All'){
                  select_factor_list = unique(pFEV_wf[,input$subset_3])
                  select_factor_list = select_factor_list[order(select_factor_list)]
                  if(select_subset_3 != 'All'){
                    selected_factor_selected = select_subset_3
                  }else{
                    selected_factor_selected = select_factor_list
                  }
                  selectInput('select_subset_3','Subset Select 1',choices = select_factor_list,multiple=T,selected = selected_factor_selected)
                }
              })
              output$auto_removed_duplicates = renderText(paste(duplicated_list,collapse = ', '))
              
              output$auto_removed_patients = renderText(paste(excluded_patients_c,collapse = ', '))
              
              excluded_patients = reactive({
                patient_list = patient_list[patient_list %in% input$remove_list]
                #saveRDS(patient_list,'www/pre_exclude_list.rds')
                patient_list
                })
              
              pre_removed_list = reactive({ # can potentially be used to re-incorporate sample automatically removed, but not implimented yet
                # pre_removed_list = c()
                # if('completeness' %in% input$select_remove){
                #   pre_removed_list = c(pre_removed_list,excluded_patients_c)
                #   }
                # if('duplicates' %in% input$select_remove){
                #   pre_removed_list = c(pre_removed_list,duplicated_list)
                # }
                pre_removed_list = c(excluded_patients_c,duplicated_list)
                pre_removed_list
              })
              
              pre_dead_patients = reactive({
                retained_patients =  patient_list[!patient_list %in% pre_removed_list()]
                death_list = death_list()
                dead_list = names(death_list[death_list < input$pre_death_cutoff])
                retained_list = retained_patients[retained_patients %in% dead_list]
                retained_list
              })
              
              output$pre_remove_ui = renderUI({
                #if(file.exists('www/pre_exclude_list.rds')){
                #  print('read pre RDS')
                #  patient_custom_exclude = readRDS('www/pre_exclude_list.rds')
                #}
                
                patient_list = patient_list[!patient_list %in% pre_removed_list()]
                
                selected_list = unique(c(patient_custom_exclude,pre_dead_patients()))
                selected_list = selected_list[order(selected_list)]
                #patient_custom_exclude = patient_custom_exclude[!patient_custom_exclude %in% excluded_patients_c]
                #selectInput('remove_list','Select additional patients to removed',patient_list,multiple = T,selected = unique(c(excluded_patients_c,patient_custom_exclude)), width = 800)
                selectInput('remove_list','Select additional patients to removed',patient_list,multiple = T,selected = selected_list, width = 800)
               
               })
              
              pre_retained_patients = reactive({
                patient_list = patient_list[!patient_list %in% pre_removed_list()]
                
                #if(r_values$init == 0){
                #  remove_list = patient_custom_exclude
                #  r_values$init = 1
                #}else{
                #  remove_list = patient_list[patient_list %in% remove_list()]
                #}
                #print(remove_list)
                patient_list = patient_list[!(patient_list %in% input$remove_list)]
                
                if(input$status_radio == '2'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf$MonthsToDeath < as.numeric(input$post_range[2])])
                  patient_list = patient_list[patient_list %in% MRN]
                }
                if(input$status_radio == '1'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf$MonthsToDeath > as.numeric(input$post_range[2]) | pFEV_wf$Status == '1'])
                  patient_list = patient_list[patient_list %in% MRN]
                }
                
                if(input$subset_1 != 'All'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf[,input$subset_1] %in% input$select_subset_1])
                  patient_list = patient_list[patient_list %in% MRN]
                }
                
                if(input$subset_2 != 'All'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf[,input$subset_2] %in% input$select_subset_2])
                  patient_list = patient_list[patient_list %in% MRN]
                }
                
                if(input$subset_3 != 'All'){
                  MRN = na.omit(pFEV_wf$MRN[pFEV_wf[,input$subset_3] %in% input$select_subset_3])
                  patient_list = patient_list[patient_list %in% MRN]
                }
                
                patient_list                
                })
              
              remove_list = reactive(input$remove_list)
              
              observeEvent(remove_list(), { 
                
                remove_list <- remove_list()
                str(remove_list)
                
              })
              
            output$pre_num_patients = renderText(print(paste('Number of retained patients : ',length(pre_retained_patients()))))
            output$clustered_patients_text = renderText(print(paste(length(pre_retained_patients()), 'patients clustered')))
            
            #observeEvent(input$pre_save,{
            #  remove_list = input$remove_list
            #  saveRDS(remove_list,'www/pre_exclude_list.rds')
            #})
            
            output$pre_patients_text = renderText({
              #remove_list = input$remove_list
              #saveRDS(remove_list,'www/pre_exclude_list.rds')
              print(pre_retained_patients())
              })
            

            
            output$post_num_patients = renderText(print(paste('Number of retained patients : ',length(post_retained_patients()))))
            
           # observeEvent(input$post_save,{
          #    remove_list = input$post_cluster_select
          #    saveRDS(remove_list, 'www/post_exclude_list.rds')
          #  })
            
            output$post_patients_text = renderText({
              #remove_list = input$post_cluster_select
              #saveRDS(remove_list, 'www/post_exclude_list.rds')
              print(post_retained_patients())
              })
            
            line_size = 2
            point_size = 4
            sm_size = 1
            
            
    
            
            output$pre_patients_table = renderDataTable({
              pFEV_wf[pFEV_wf$MRN %in% excluded_patients_c,pFEV_numeric_colnames_f]
            })
            
            output$post_patients_table = renderDataTable({
              pFEV_wf[pFEV_wf$MRN %in% input$post_cluster_select,pFEV_numeric_colnames_f]
            })
            
            output$retained_patients_table = renderDataTable({
              pFEV_wf[pFEV_wf$MRN %in% post_retained_patients(),pFEV_numeric_colnames_f]
            })
            
            output$patients_text = renderText({
              paste(dim(clustering)[1],'entries, ', length(unique(clustering$MRN_original)), 'individual patients,', length(post_retained_patients()), 'clustered entries')
            })
            output$completeness_text = renderText({
              paste(length(excluded_patients_c), 'patients with less than ',completeness,'% of the pFEV datapoints were automatically removed from the analysis')
            })
            
            ####### __retained ###########
              output$plots <- renderUI({
                plot_output_list <- lapply(retained_patients(), function(i) {
                  plotname <- paste("plot", i, sep="_")
                  plotOutput(plotname, height = 280, width = 250)
                })
                
              
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
              })
              # r_list = isolate(pre_retained_patients())
              # for (i in 1:length(r_list)) {
              # 
              #   # Need local so that each item gets its own number. Without it, the value
              #   # of i in the renderPlot() will be the same across all instances, because
              #   # of when the expression is evaluated.
              #   local({
              #     my_i <- i
              #     plotname <- paste("plot", my_i, sep="")
              # 
              #     #my_i = 1
              #     output[[plotname]] <- renderPlot({
              #       #t_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% i_pFEV_lf_r()$MRN[1],]
              #       i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% retained_patients()[my_i],]
              #       sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% retained_patients()[my_i],]
              #       #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
              #       #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$mrn_select,]
              #       if(dim(sm_data)[1] > 0){
              #         ggplot(NULL) +
              #           geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
              # 
              #           geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #           geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #           #geom_line(data = )
              #           #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #           scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              # 
              #           geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='green',size = sm_size)+
              # 
              #           theme(axis.text.x = element_text(size=8, angle=90)) +
              #           theme(legend.position="none") +
              #           ggtitle(paste0(retained_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #       }else{
              #         ggplot(NULL) +
              #           geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
              #           geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #           geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #           #geom_line(data = )
              #           #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #           scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              # 
              #           #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
              # 
              #           theme(axis.text.x = element_text(size=8, angle=90)) +
              #           theme(legend.position="none") +
              #           ggtitle(retained_patients()[my_i])
              #       }
              # 
              #     },width = 600)
              #   })
              # }
            
            ########## __excluded ###############
              output$excluded_plots <- renderUI({
                #renderPlots()
                plot_output_list <- lapply(excluded_patients(), function(i) {
                  plotname <- paste("plot", i, sep="_")
                  plotOutput(plotname, height = 280, width = 250)
                })
            
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
              })
            
              # pre_list = isolate(excluded_patients())
              # for (i in 1:length(pre_list)) {
              # 
              #   # Need local so that each item gets its own number. Without it, the value
              #   # of i in the renderPlot() will be the same across all instances, because
              #   # of when the expression is evaluated.
              #   local({
              #     my_i <- i
              #     plotname <- paste("excluded_plot", my_i, sep="")
              # 
              #     #my_i = 2
              #     #print(excluded_patients()[my_i])
              #     output[[plotname]] <- renderPlot({
              #       #t_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% i_pFEV_lf_r()$MRN[1],]
              #       #print(excluded_patients()[my_i])
              #       #o_data = pFEV_lf_r()[pFEV_lf_r()$MRN %in% patient_list[my_i],]
              #       #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% patient_list[my_i],]
              #       #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% patient_list[my_i],]
              #       i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% excluded_patients()[my_i],]
              #       sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% excluded_patients()[my_i],]
              #       o_data = pFEV_lf[pFEV_lf$MRN %in% excluded_patients()[my_i],]
              #       
              #       #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
              #       #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$mrn_select,]
              #       if(dim(sm_data)[1]>0){
              #         ggplot(NULL) +
              #           geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
              #           
              #           geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #           geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #           
              #           scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #           geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='green',size = sm_size)+
              #           
              #           
              #           theme(axis.text.x = element_text(size=8, angle=90)) +
              #           theme(legend.position="none") +
              #           ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #       }else{
              #       
              #       if(dim(i_data)[1]>0){
              #         ggplot(NULL) +
              #           geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
              # 
              #           geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #           geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #           scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              # 
              #           #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='blue',size = 0.7)+
              # 
              #           theme(axis.text.x = element_text(size=8, angle=90)) +
              #           theme(legend.position="none") +
              #           ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #         
              #         }else{
              #           ggplot(NULL) +
              #             geom_vline(data = o_data,aes(xintercept = which(levels(o_data$variable) %in% '0'))) +
              #             
              #             geom_line(data = o_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #             geom_point(data = o_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #             #geom_line(data = )
              #             #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #             scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #             
              #             #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
              #             
              #             theme(axis.text.x = element_text(size=8, angle=90)) +
              #             theme(legend.position="none") +
              #             ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #           
              #         }
              #       }
              # 
              # 
              #     },width = 600)
              #   })
              # }
              # 
              excluded_patients_post = reactive({ 
                patient_list = input$post_cluster_select
                patient_list
                })
              
              output$excluded_plots_post <- renderUI({
                plot_output_list <- lapply(excluded_patients_post(), function(i) {
                  plotname <- paste("plot", i, sep="_")
                  plotOutput(plotname, height = 280, width = 250)
                })
                
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
              })
              
        
              
              # post_list = isolate(excluded_patients_post())
              # for (i in 1:length(post_list)) {
              #   
              #   # Need local so that each item gets its own number. Without it, the value
              #   # of i in the renderPlot() will be the same across all instances, because
              #   # of when the expression is evaluated.
              #   local({
              #     my_i <- i
              #     plotname <- paste("plot", my_i, sep="")
              #     
              #     #my_i = 2
              #     #print(excluded_patients()[my_i])
              #     output[[plotname]] <- renderPlot({
              #       #t_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% i_pFEV_lf_r()$MRN[1],]
              #       #print(excluded_patients()[my_i])
              #       #o_data = pFEV_lf_r()[pFEV_lf_r()$MRN %in% patient_list[my_i],]
              #       #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% patient_list[my_i],]
              #       #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% patient_list[my_i],]
              #       i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% excluded_patients_post()[my_i],]
              #       sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% excluded_patients_post()[my_i],]
              #       o_data = pFEV_lf[pFEV_lf$MRN %in% excluded_patients_post()[my_i],]
              #       
              #       #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
              #       #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$mrn_select,]
              #       if(dim(sm_data)[1]>0){
              #         ggplot(NULL) +
              #           geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
              #           
              #           geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #           geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #           
              #           scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #           geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='green',size = sm_size)+
              #           
              #           
              #           theme(axis.text.x = element_text(size=8, angle=90)) +
              #           theme(legend.position="none") +
              #           ggtitle(paste0(excluded_patients()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #       }else{
              #         
              #         if(dim(i_data)[1]>0){
              #           ggplot(NULL) +
              #             geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
              #             
              #             geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #             geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #             scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #             
              #             #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='blue',size = 0.7)+
              #             
              #             theme(axis.text.x = element_text(size=8, angle=90)) +
              #             theme(legend.position="none") +
              #             ggtitle(paste0(excluded_patients_post()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #           
              #         }else{
              #           ggplot(NULL) +
              #             geom_vline(data = o_data,aes(xintercept = which(levels(o_data$variable) %in% '0'))) +
              #             
              #             geom_line(data = o_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
              #             geom_point(data = o_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
              #             #geom_line(data = )
              #             #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #             scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
              #             
              #             #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
              #             
              #             theme(axis.text.x = element_text(size=8, angle=90)) +
              #             theme(legend.position="none") +
              #             ggtitle(paste0(excluded_patients_post()[my_i],' (',i_data$pFEV_na[my_i],'%)'))
              #           
              #         }
              #       }
              #       
              #       
              #     },width = 600)
              #   })
              # }
              
              ####### PLOT ALL ########
              
              #post_list = isolate(excluded_patients_post())
            # renderPlots = reactive({
            #   patient_list = excluded_patients()
            #   for (i in 1:length(patient_list)) {
            #     
            #     # Need local so that each item gets its own number. Without it, the value
            #     # of i in the renderPlot() will be the same across all instances, because
            #     # of when the expression is evaluated.
            #     local({
            #       my_i <- patient_list[i]
            #       plotname <- paste("plot", my_i, sep="_")
            #       
            #       #my_i = 2
            #       #print(excluded_patients()[my_i])
            #       output[[plotname]] <- renderPlot({
            #         #t_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% i_pFEV_lf_r()$MRN[1],]
            #         #print(excluded_patients()[my_i])
            #         #o_data = pFEV_lf_r()[pFEV_lf_r()$MRN %in% patient_list[my_i],]
            #         #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% patient_list[my_i],]
            #         #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% patient_list[my_i],]
            #         i_data = i_pFEV_lf[i_pFEV_lf$MRN == my_i,]
            #         sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN == my_i,]
            #         o_data = pFEV_lf[pFEV_lf$MRN == my_i,]
            #         
            #         #i_data = i_pFEV_lf_r()[i_pFEV_lf_r()$MRN %in% input$mrn_select,]
            #         #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN %in% input$mrn_select,]
            #         #if(dim(sm_data)[1]>0){
            #         p = ggplot(NULL) +
            #           geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0')))
            #         p = p + geom_hline(yintercept = 0.8,color = 'yellow') +
            #           geom_hline(yintercept = 0.66, color = 'orange') + 
            #           geom_hline(yintercept = 0.5, color = 'red')
            #         if(!is.na(as.numeric(i_data$MonthsToDeath))){
            #            mortality = which(levels(i_data$variable) %in% factor(round(as.numeric(i_data$MonthsToDeath),0))[1])
            #            print(as.numeric(i_data$MonthsToDeath))
            #            print(mortality)
            #            if(length(mortality) > 0){
            #             p = p + geom_segment(data = i_data,aes(x = mortality, xend = mortality,y = 0, yend = 1),col = 'grey',lwd = 5)
            #            }
            #           }
            #         
            #         if(!is.na(i_data$BOS1)){
            #           #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% factor(i_data$BOS1))),col = 'yellow',lwd = 1.5)
            #           BOS1_x = which(levels(i_data$variable) %in% factor(i_data$BOS1))
            #           p = p + geom_segment(data = i_data,aes(x = BOS1_x, xend = BOS1_x,y = 0, yend = 0.8),col = 'yellow',lwd = 3.5)
            #         }
            #         if(!is.na(i_data$BOS2)){
            #           #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% factor(i_data$BOS2))),col = 'orange',lwd = 3)
            #           BOS2_x = which(levels(i_data$variable) %in% factor(i_data$BOS2))
            #           p = p + geom_segment(data = i_data,aes(x = BOS2_x, xend = BOS2_x,y = 0, yend = 0.66),col = 'orange',lwd = 3.5)
            #         }
            #         if(!is.na(i_data$BOS3)){
            #           #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% factor(i_data$BOS3))),col = 'red',lwd = 4.5)
            #           BOS3_x = which(levels(i_data$variable) %in% factor(i_data$BOS3))
            #           p = p + geom_segment(data = i_data,aes(x = BOS3_x, xend = BOS3_x,y = 0, yend = 0.5),col = 'red',lwd = 3.5)
            #           
            #         }
            # 
            # 
            #       
            #         if(!is.na(as.numeric(i_data$BOS1mnth))){
            #           #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% factor(i_data$BOS1mnth))),col = 'blue',lwd = 1)
            #           BOS1_x = which(levels(i_data$variable) %in% factor(i_data$BOS1mnth))
            #           p = p + geom_segment(data = i_data,aes(x = BOS1_x, xend = BOS1_x,y = 0, yend = 0.8),col = 'blue',lwd = 1)
            #           } 
            #         if(!is.na(as.numeric(i_data$BOS2mnth))){
            #           #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% factor(i_data$BOS2mnth))),col = 'blue',lwd = 1)
            #           BOS2_x = which(levels(i_data$variable) %in% factor(i_data$BOS2mnth))
            #           p = p + geom_segment(data = i_data,aes(x = BOS2_x, xend = BOS2_x,y = 0, yend = 0.66),col = 'blue',lwd = 1)
            #           }
            #         if(!is.na(as.numeric(i_data$BOS3mnth))){
            #           #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% factor(i_data$BOS3mnth))),col = 'blue',lwd = 1)
            #           BOS3_x = which(levels(i_data$variable) %in% factor(i_data$BOS3mnth))
            #           p = p + geom_segment(data = i_data,aes(x = BOS3_x, xend = BOS3_x,y = 0, yend = 0.5),col = 'blue',lwd = 1)
            #           }
            #             
            #           if(dim(sm_data)[1]>0){  
            #             p = p + geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
            #             geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
            #             
            #             scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            #             geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='green',size = sm_size)+
            #             
            #             
            #             theme(axis.text.x = element_text(size=8, angle=90)) +
            #             theme(legend.position="none") +
            #             ggtitle(my_i)
            #         }else{
            #           
            #           if(dim(i_data)[1]>0){
            #             #p = ggplot(NULL) +
            #             #p = p + geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
            #               
            #             p = p + geom_line(data = i_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
            #               geom_point(data = i_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
            #               scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            #               
            #               #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN),col='blue',size = 0.7)+
            #               
            #               theme(axis.text.x = element_text(size=8, angle=90)) +
            #               theme(legend.position="none") +
            #               ggtitle(my_i)
            #             
            #           }else{
            #             #p = ggplot(NULL) +
            #             #  geom_vline(data = o_data,aes(xintercept = which(levels(o_data$variable) %in% '0'))) +
            #               
            #              p = p + geom_line(data = o_data, aes(x = variable, y = value, group = MRN),col='red',size = line_size)+
            #               geom_point(data = o_data, aes(x = variable, y = data,group = MRN),col='blue',size = point_size) +
            #               #geom_line(data = )
            #               #scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            #               scale_x_discrete(breaks = pFEV_numeric_colnames_f) +
            #               
            #               #geom_line(data = sm_data, aes(x = variable, y = value,group = MRN,col='green'))+
            #               
            #               theme(axis.text.x = element_text(size=8, angle=90)) +
            #               theme(legend.position="none") +
            #               ggtitle(my_i)
            #             
            #           }
            #         }
            #         
            #       print(p) 
            #       },width = 600)
            #     })
            #   }
            # })

  ############# ISOLATE DATA FRAME AFTER REMOVING INCORRECT DATA ###########
  

  
  pFEV_wf_c = reactive({
    o_data = pFEV_wf
    o_data = processed_data
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$ratio_data)
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$per_data)
    #o_data = cbind(pFEV_wf, pFEV_sym())
    
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$ratio_data, pFEV_2_zero()$per_data, pFEV_sym())
    #o_data = cbind(pFEV_wf, pFEV_2_zero()$ratio_data, pFEV_sym())
    #death_list = death_list()
    #o_data$drop_out = death_list[match(o_data$MNR,names(death_list))]
    o_data$drop_out = death_list()
    data = o_data[o_data$MRN %in% pre_retained_patients(),]
    colnames(o_data)
    #View(o_data[,c("DeathDate","MonthsToDeath","YearsToDeath", "MonthSinceRx" , "AltDxScore","BOS1mnth","BOS2mnth" , "BOS3mnth","BOS 3 free survival", 'drop_out')])
    #data = data[data$Status %in% status_r(),]
    ##View(data)
    data
  })  
              
  pFEV_wf_r_post_clustering = reactive({

    o_data = pFEV_wf_c()
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)

    data = o_data[o_data$MRN %in% pre_retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]

    data = change_data_w()
    
    data
    
  })
  
  output$post_hist = renderPlot({
    death_list = death_list()
    death_list = death_list[post_retained_patients()]
    breaks = (max(death_list) - min(death_list))
    h = hist(death_list, breaks = breaks, main = 'Frequency at which patients stop having pFEV values', xlab = '', xaxt = 'n', xlim = c(0,24))
    labels = c(0:24)
    at = labels - 0.5
    axis(1, at =  at, labels = labels, tick = FALSE, padj= -1.5)
  },height = 200)
  
  output$pre_hist = renderPlot({
    death_list = death_list()
    #death_list
    #print(dim(death_list))
    death_list = death_list[pre_retained_patients()]
    death_list
    breaks = (max(death_list) - min(death_list))
    
    
    hist(death_list,breaks = breaks, main = 'Frequency at which patients stop having pFEV values', xlab = '', xaxt = 'n')
    labels = c(-24:24)
    at = labels - 0.5
    axis(1, at =  at, labels = labels, tick = FALSE, padj= -1.5)
    
  },height = 200)
  
  post_dead_patients = reactive({
    retained_patients = pre_retained_patients()
    death_list = death_list()
    dead_list = names(death_list[death_list < input$post_death_cutoff])
    retained_list = retained_patients[retained_patients %in% dead_list]
    retained_list
  })
  
  output$post_select_ui = renderUI({

    selected_list = unique(c(post_exclude_list,post_dead_patients()))
    selected_list = selected_list[order(selected_list)]
    selectInput('post_cluster_select','Select Additional Sample to Exclude or remove those added by the slider',pre_retained_patients(),multiple = T,selected = selected_list)
  })
  
  post_retained_patients = reactive({
    retained_patients = pre_retained_patients()
    remove_list = input$post_cluster_select
    retained_patients = retained_patients[! retained_patients %in% remove_list]
    retained_patients
  })
  
  retained_patients = reactive(post_retained_patients())
  
  processed_data_w_r = reactive({
    print('processed_data_w_r')
    o_data = processed_data
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    
    data
  })
  
  processed_data_l_r = reactive({
    o_data = processed_long
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data
  }) 
  
  pFEV_wf_r = reactive({
    #o_data = pFEV_wf_c()
    #m_data = discrete_cluster_D()$data
    #m_data$MRN = rownames(m_data)
    #data = o_data[o_data$MRN %in% retained_patients(),]
    #data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]

    data = change_data_w()
    data
    
    }) #### USED THROUGHT APP, actually change_data_w()

  
  pFEV_lf_r = reactive({ 

    data = change_data_l()
    data
  }) #### USED THROUGHT APP, actually change_data_l()
  

  
  
  
  i_pFEV_wf_r = reactive({
    o_data = i_pFEV_wf
    m_data = pFEV_wf_r()
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]

    data
  })
  
  i_pFEV_wf_c = reactive({
    o_data = i_pFEV_wf
    o_data = processed_data
    data = o_data[o_data$MRN %in% pre_retained_patients(),]
    #data = data[data$Status %in% status_r(),]
    data
  })
  
  last_index = reactive({
    o_data = pFEV_wf
    pFEV_data = o_data[,p_cols]
    last_index = apply(pFEV_data,1, function(x) max(which(!is.na(x))))
    last_index
  })
  
  death_list = reactive({
    last_index = last_index()
    death_list = as.numeric(p_cols[last_index])
    names(death_list) = names(last_index)
    death_list[is.na(death_list)] = min(as.numeric(p_cols))
    death_list
  })
  
  
  
  i_pFEV_wf_c_NA = reactive({
    o_data = pFEV_wf
    pFEV_data = o_data[,p_cols]
    #last_index = apply(pFEV_data,1, function(x) max(which(!is.na(x))))
    last_index = last_index()
    full_index = dim(pFEV_data)[2]
    sample_list = c(1:dim(pFEV_data)[1])


    
    
    i_data = i_pFEV_wf

    i_pFEV_data = i_data[,p_cols]

    for(x in sample_list){

      if(last_index[x] != full_index){
        if(is.finite(last_index[x])){
          i_pFEV_data[x,c(last_index[x]:full_index)] = NA
        }
      }
      
    }
    

    o_data[,p_cols] = i_pFEV_data
    data = o_data[o_data$MRN %in% pre_retained_patients(),]
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
    data = o_data[o_data$MRN %in% pre_retained_patients(),]
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

    data
  })
  i_pFEV_sm_d1_f_c_ir_r = reactive({
    o_data = i_pFEV_sm_d1_f_c_ir()
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)

    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    
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
    data = pFEV_wf_c()
    
    if(input$data_select == 'pFEV'){
      data = pFEV_wf_c()
    }
    if(input$data_select == 'imputed'){
      data = i_pFEV_wf_c()
    }
    
    if(input$data_select == 'imputed_NA'){
      data = i_pFEV_wf_c_NA()
    }
    
    if(input$data_select == 'smoothed'){
      o_data = i_pFEV_smf
      #m_data = pFEV_wf_r()
      data = o_data[o_data$MRN %in% pre_retained_patients(),]
    }
    
    if(input$data_select == 'd1'){
      data = i_pFEV_sm_d1_f_c()
    }
    if(input$data_select == 'd1_ri'){
      data = i_pFEV_sm_d1_f_c_ir()
    }
    
    if(input$data_select == 'd2'){
      o_data = i_pFEV_sm_d2_f
      data = o_data[o_data$MRN %in% pre_retained_patients(),]
    }

    
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

  change_data_list = reactive({
    print('change_data_list')
    #d1_data = i_pFEV_sm_d1_f_c()
    #print()
    #print(dim(d1_data))
    #d1_data = d1_data[,colnames(d1_data) %in% pFEV_numeric_colnames_f]
    #colnames(d1_data) = paste0('D1_',colnames(d1_data))
  
    
    #data = cbind(comp_data(),d1_data,pFEV_2_zero()$ratio_data,pFEV_2_zero()$per_data, sym_data(),per_sym_data())
    #data = comp_data()
    data = pFEV_wf_c()
    ccc = colnames(data)[colnames(data) %in% continuous_columns]
    data_list = c('pFEV1','i_pFEV1','i_pFVC','i_pRatio','d1_pFEV1','d1_pFVC','d1_pRatio')
    data_list = input$data_set
    #print(data_list)
    data_entry = data_list[1]
    for(data_entry in data_list){
      cmd = paste0("o_data = data$",data_entry,"_matrix")
      eval(parse(text = cmd))
      head(o_data)
      o_data = as.data.frame(o_data)
      o_data
      colnames(o_data) = gsub('-','neg_',paste(data_entry,colnames(o_data),sep='_'))
      ccc = c(ccc,colnames(o_data))
      #print(head(o_data))
      #print(rownames(o_data))
      #o_data$MRN = rownames(o_data)
      #new_data = o_data[o_data$MRN %in% pre_retained_patients(),]
      #print()
      #dim(new_data)
      #new_data = within(new_data, rm(MRN))
      data = cbind(data,o_data)
    }
    
    save_test = F
    if(save_test == T){
      variable_list = c('data')
      cmd_list = save_variable_function(variable_list)
      lapply(cmd_list, function(x) eval(parse(text = x)))
      try(save_input_function(input))
      read_test = F
      if(read_test == T){
        variable_list = c(variable_list,'input')
        cmd_list = read_variable_function(variable_list)
        for(cmd in cmd_list){
          print(cmd)
          try(eval(parse(text = cmd)))
        }
      }
    }
    data
    list(data = data, ccc = ccc)
  })
  
  change_data = reactive(change_data_list()$data) # used for clustering
  
  output$ccc_1 = renderUI({
    selectInput('mix_clust_col_num','Continuous Variable List 1',change_data_list()$ccc,multiple = T,selected = continuous_list_1,width = 600)
  })
  
  output$ccc_2 = renderUI({
    selectInput('mix_clust_col_num_2','Continuous Variable List 2', change_data_list()$ccc, multiple = T,selected = continuous_list_2,width = 600)
  })
  
  select_matrix = reactive({
    matrix_name_list = c(input$data_select,input$data_source,input$calc_select,'matrix')
    matrix_name_list = matrix_name_list[matrix_name_list != 'none']
    
    matrix_name = paste(matrix_name_list,collapse = '_')
    matrix_name
  })
  
  change_data_full_w = reactive({
    o_data = change_data()
    c_o_data = o_data
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    c_m_data = m_data
    data = o_data[o_data$MRN %in% retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    #data = data[data$cluster %in% input$cluster_select_clusters,]
    dim(data)
    data_l = data
    data
    
  })
  
  change_data_w = reactive({
    full_data = change_data_full_w()
    cmd = paste0("data = full_data[full_data$",input$global_factor," %in% input$cluster_select_clusters,]")
    eval(parse(text = cmd))
    data
    
  })
  change_data_l = reactive({
    w_data = change_data_w()
    print('')
    print(dim(w_data))
    entry = "pFEV1_matrix"
    entry = select_matrix()
    entry
    melt_columns = c(colnames(w_data)[colnames(w_data) %in% factor_columns],'cluster')
    melt_columns
    #data = melt_processed_data(processed_data,c(melt_columns),entry)$long_df
    temp_melt = melt(processed_data[,entry], id.vars = MRN)
    colnames(temp_melt) = c('MRN','variable','value')
    temp_melt = temp_melt[temp_melt$MRN %in% w_data$MRN,]
    #head(temp_melt)
    #data = processed_data[]
    temp_data = w_data[,melt_columns][match(w_data$MRN,temp_melt$MRN),]
    data = cbind(temp_data,temp_melt)
    
    #data$variable = temp_melt$X2
    #data = data[data$MRN %in% w_data$MRN,]
    dim(data)
    
    #data = melt(w_data, measure.vars = select_cols())
    data$time = as.numeric(as.character(data$variable))
    data_w = data
    ##View(data_l)
    data$variable = as.factor(data$variable)
    data
  })
  
  comp_data_clust = reactive({
    data = pFEV_wf_c()
    
    if(input$data_select_clust == 'pFEV'){
      data = pFEV_wf_c()
      
    }
    if(input$data_select_clust == 'imputed'){
      data = i_pFEV_wf_c()
    }
    
    if(input$data_select_clust == 'imputed_NA'){
      data = i_pFEV_wf_c_NA()
    }
    
    if(input$data_select_clust == 'smoothed'){
      o_data = i_pFEV_smf
      data = o_data[o_data$MRN %in% pre_retained_patients(),]
    }
    
    if(input$data_select_clust == 'd1'){
      data = i_pFEV_sm_d1_f_c()
    }
    if(input$data_select_clust == 'd1_ri'){
      data = i_pFEV_sm_d1_f_c_ir()
    }
    
    if(input$data_select_clust == 'd2'){
      o_data = i_pFEV_sm_d2_f
      #m_data = pFEV_wf_r()
      data = o_data[o_data$MRN %in% pre_retained_patients(),]
    }
    ##View(data)
    
    data
    
  })
  select_cols_clust = reactive({

    select_cols = colnames(pFEV_w)
    if(input$data_select_clust == 'pFEV'){

      select_cols = colnames(pFEV_w)
      
      
    }
    if(input$data_select_clust == 'imputed'){
      #data = i_pFEV_wf_c()
      select_cols = colnames(pFEV_w)
      
    }
    if(input$data_select_clust == 'd1'){
      #data = i_pFEV_sm_d1_f_c()
      select_cols = colnames(i_pFEV_sm_d1)
      
    }
    if(input$data_select_clust == 'd2'){
      #data = i_pFEV_sm_d1_f_c()
      select_cols = colnames(i_pFEV_sm_d2)
      
    }
    if(input$data_select_clust == 'd1_ri'){
      select_cols = colnames(i_pFEV_sm_d1)
      
    }
    ##View(data)
    
    select_cols
    
  })
  change_data_clust = reactive({
    d1_data = i_pFEV_sm_d1_f_c()
    d1_data = d1_data[,colnames(d1_data) %in% pFEV_numeric_colnames_f]
    colnames(d1_data) = paste0('D1_',colnames(d1_data))
    data = cbind(comp_data_clust(),d1_data,pFEV_2_zero()$ratio_data,pFEV_2_zero()$per_data, sym_data(),per_sym_data())
    data
    
  })
  change_data_w_clust = reactive({
    o_data = change_data_clust()
    c_o_data = o_data
    m_data = discrete_cluster_D()$data
    m_data$MRN = rownames(m_data)
    c_m_data = m_data
    data = o_data[o_data$MRN %in% pre_retained_patients(),]
    data$cluster = m_data$cluster[match(data$MRN,m_data$MRN)]
    data_l = data
    data
    
  })
  change_data_l_clust = reactive({
    w_data = change_data_w_clust()
    data = melt(w_data, measure.vars = select_cols_clust())
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
    #paste(test_list)
    
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
  
  output$plot_mrn_select = renderUI({
    selected_patient_list = patient_list
    selected_patient_list = paste(unlist(processed_data$MRN[!is.na(processed_data$RAS)]))
    selected_patient_list
    selectInput('mrn_select_i','MRN',selected_patient_list,multiple = F, width = 800)
    
    #selectInput('mrn_select_i','MRN',selected_patient_list,multiple = F, selected = selected_patient_list[1], width = 800)
  })
  
  output$individual_patients_pd = renderPlot({

    full_data = processed_long

    data = full_data[full_data$MRN %in% input$mrn_select_i,]
    print(dim(data))
    test = F
    if(test == T){
      save_input_function(input)
      
      saveRDS(full_data,'temp/full_data.rds')
      saveRDS(data,'temp/data.rds')
    }
    read_test = F
    if(read_test == T){
      data = readRDS('temp/data.rds')
      full_data = readRDS('temp/full_data.rds')
      
      input = readRDS('temp/save_input.rds')
      
    }
    
    #sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN  %in% input$mrn_select_i,]
    
   
      p = ggplot(data)
      
      if(length(data$RAS[!is.na(data$RAS)]) != 0){
        p = p + geom_vline(xintercept = data$RAS, color = 'orange',lwd = 1, linetype = 'solid')
      }
      p = p + 
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = data$BOS1_RAS, color = 'orange',lwd = 2, linetype = 'dashed') +
        geom_vline(xintercept = data$BOS2_RAS, color = 'orange',lwd = 3, linetype = 'dotted') +
        geom_vline(xintercept = data$BOS3_RAS, color = 'orange',lwd = 4, linetype = 'dotdash') + 
        geom_hline(yintercept = c(0.8,0.66,0.5), color = 'grey') + 
        geom_hline(yintercept = 0.7, color = 'darkgrey')
        
      p = p + geom_line(aes(x = time, y = i_pFEV1, group = MRN),col='red',size = line_size)
      p = p + geom_point(aes(x = time, y = pFEV1,group = MRN),col='grey',size = point_size)
      p = p + geom_line(aes(x = time, y = sm_i_pFEV1,group = MRN),col='black',size = sm_size)
      
      p = p + geom_line(aes(x = time, y = i_pFVC, group = MRN),col='green',size = line_size)
      p = p + geom_point(aes(x = time, y = pFVC,group = MRN),col='grey',size = point_size)
      p = p + geom_line(aes(x = time, y = sm_i_pFVC,group = MRN),col='black',size = sm_size)
      
      p = p + geom_line(aes(x = time, y = i_pRatio, group = MRN),col='blue',size = line_size)
      p = p + geom_point(aes(x = time, y = pRatio,group = MRN),col='grey',size = point_size)
      p = p + geom_line(aes(x = time, y = sm_i_pRatio,group = MRN),col='black',size = sm_size)
      
      #p = p +  scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
      #          scale_colour_manual(name = 'the colour', 
      #                      values =c('black'='black','red'='red'), labels = c('c2','c1'))
      p = p + theme(axis.text.x = element_text(size=8, angle=90)) +
        ylim(0,1) + 
        theme(legend.position="top") + 
        ggtitle('Imputed and Smoothed')
 

    
    
    print(p)
  })
  
  
  output$individual_patients = renderPlot({
 
    
    i_data = i_pFEV_lf[i_pFEV_lf$MRN %in% input$mrn_select_i,]
    sm_data = i_pFEV_sm_lf[i_pFEV_sm_lf$MRN  %in% input$mrn_select_i,]

    if(dim(sm_data)[1] > 0){
      ggplot(NULL) +
        #geom_vline(data = i_data,aes(xintercept = which(levels(i_data$variable) %in% '0'))) +
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = i_data$BOS1, color = 'red',lwd = 3) +
        geom_vline(xintercept = i_data$BOS2, color = 'green',lwd = 2) +
        geom_vline(xintercept = i_data$BOS3, color = 'blue',lwd = 1) +
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
  
  output$individual_patient_table = renderDataTable({
    data = i_pFEV_wf[i_pFEV_wf$MRN %in% input$mrn_select_i,]
    data
  })
  
  output$individual_patient_table_pd = renderDataTable({
    data = processed_long[processed_long$MRN %in% input$mrn_select_i,]
    data
  })
  
  patient_data = reactive({
    patient_data = processed_data
    if(input$retained_radiobutton == 'bos'){
      patient_data = BOS_processed_data_w_r()
    }
    patient_data
  })
  
  output$multi_plot_column_select_ui = renderUI({
    if(input$retained_radiobutton == 'bos'){
      entry = 'RAS'
    }else{
      entry = 'MRN'
    }
    selectInput('multi_plot_columns','select column',full_factor_columns,multiple = F,selected = 'RAS', width = 800)
    
  })
  
  multi_patient_list = reactive({
    
    patient_list = paste(unlist(patient_data()$MRN))
    
    if(input$retained_radiobutton == 'retained'){
      patient_list = retained_patients()
    }
    if(input$retained_radiobutton == 'pre'){
      patient_list = patient_list[!(patient_list %in% pre_retained_patients())]
    }
    if(input$retained_radiobutton == 'post'){
      patient_list = input$post_cluster_select
    }
    patient_list
    
    df = patient_data()[patient_data()$MRN %in% patient_list,]
    print(dim(df))
    print(dim(df))
    
    selected_patient_list = paste(unlist(df$MRN[!is.na(df[,input$multi_plot_columns])]))
    selected_patient_list
  })
  
  output$multi_patient_number = renderText(length(multi_patient_list()))
  
  multi_selected_subset = reactive({
    selected_patient_list = multi_patient_list()
    if(input$multi_subset == 'all'){
      selected_subset = selected_patient_list
    }else{
      selected_subset = selected_patient_list[c(1:5)]
    }
    selected_subset
  })
  
  output$multi_patient_table_wide = renderDataTable({
    patient_data()[patient_data()$MRN %in% input$multi_plot_patients,]
  })
  multi_patient_long_data = reactive({
    data = processed_long
    if(input$retained_radiobutton == 'bos'){
      data = BOS_processed_data_l_r()  
    }
    data[data$MRN %in% input$multi_plot_patients,]
    data
    
  })
  
  output$multi_patient_table_long = renderDataTable({multi_patient_long_data})
  
  output$multi_plot_select_patient_ui = renderUI({

    selectInput('multi_plot_patients','MRN',multi_patient_list(),multiple = T,selected = multi_selected_subset(), width = 800)
    
    #selectInput('mrn_select_i','MRN',selected_patient_list,multiple = F, selected = selected_patient_list[1], width = 800)
  })
  
  output$individual_patients_ui = renderUI({
    print('individual_patients_ui')
    full_data = multi_patient_long_data()
    patient_list = input$multi_plot_patients
    dim(full_data)
    length(patient_list)
    renderPlots(full_data, patient_list, input, output, prefix = 'individual')
    makePlotContainers(patient_list, prefix="individual") 

  })
  output$individual_patients_dup = renderPlot({

    data = i_pFEV_lf[pFEV_lf$MRN_original %in% input$mrn_select_i_dup,]

      ggplot(NULL) +
        geom_vline(x_intercept = 0) +
        geom_line(data = data, aes(x = time, y = value, group = MRN,col = MRN),size = line_size)+
        geom_point(data = data, aes(x = time, y = data,group = MRN,col = MRN),size = point_size, position = position_jitter(w = 0.5, h = 0)) +
        theme(axis.text.x = element_text(size=8, angle=90)) +
        ggtitle('Imputed and Smoothed')
    
    
    
    
  })
  
  output$individual_patient_table_dup = renderDataTable({
    data = pFEV_wf[pFEV_wf$MRN_original %in% input$mrn_select_i_dup,]
    data
  })
  
  output$line_pFEV_title_ui = renderUI(textInput('line_pFEV_title','Title',paste(select_matrix(), 'line plot')))
  output$line_pFEV_y_ui = renderUI(textInput('line_pFEV_y','y title',gsub('_matrix','',select_matrix())))
  
  output$line_pFEV_ui = renderUI({
    tabsetPanel(
    tabPanel('Line Plot',
             
             column(12,tags$hr()),
             column(6,textInput('line_pFEV_title','Title',paste(select_matrix(), 'line plot'))),
             column(3,textInput('line_pFEV_x','x title','months')),
             column(3,textInput('line_pFEV_y','y title',gsub('_matrix','',select_matrix()))),

             column(11,plotOutput('line_pFEV')),
             column(1,downloadButton('line_pFEV_download','')),
             column(12,tags$hr()),
             
            column(12,tags$hr()),
            column(6,textInput('smooth_line_pFEV_title','Title',paste(select_matrix(), 'smoothed'))),
            column(3,textInput('smooth_line_pFEV_x','x title','months')),
            column(3,textInput('smooth_line_pFEV_y','y title',gsub('_matrix','',select_matrix()))),
            
            column(11,plotOutput('smooth_line_pFEV')),
            column(1,downloadButton('smooth_line_pFEV_download','')),
            column(12,tags$hr())
             
             
             
    ),
    tabPanel('Boxplot',
             #column(12,
                    column(6,textInput('boxplot_pFEV_title','Title',paste(select_matrix(), 'boxplot'))),
                    column(3,textInput('boxplot_pFEV_x','x title','months')),
                    column(3,textInput('boxplot_pFEV_y','y title',gsub('_matrix','',select_matrix()))),
                    
                    column(11,plotOutput('boxplot_pFEV')),
                    column(1,downloadButton('boxplot_pFEV_download','')),
                    column(12,tags$hr()),
                    
                    
                    column(6,textInput('boxplot_pFEV_mean_title','Title',paste(select_matrix(), 'boxplot mean'))),
                    column(3,textInput('boxplot_pFEV_mean_x','x title','months')),
                    column(3,textInput('boxplot_pFEV_mean_y','y title',gsub('_matrix','',select_matrix()))),
                    
                    column(11,plotOutput('boxplot_pFEV_mean')),
                    column(1,downloadButton('boxplot_pFEV_mean_download','')),
                    column(12,tags$hr())
                    
             
    ))
    # tabPanel('Interaction',
    #          HTML(paste("The primary separation factor is separated into different plots. The Interactors Selector box is used to choose additional factors. Mean line plots are then generated illustrating the how these factors separate the data within the main factor.")),
    #          column(3,selectInput('interactors','Ineteractors',c(full_factor_columns,'cluster','cluster_d1'),multiple = T,selected = 'Status')),
    #          
    #          plotOutput('interaction_plot')
    # )))),
    
  })
  
  line_pFEV = reactive({
    r_data = pFEV_lf_r()
    title = paste(select_matrix(),' values for ',length(unique(r_data$MRN))," Patients")
    p = line_plot_function(r_data,input$line_pFEV_title,input,input$line_pFEV_x,input$line_pFEV_y)
    p
  })
  output$line_pFEV = renderPlot(line_pFEV())
  output$distance_density = renderPlot(distance_density())
  output$line_pFEV_download <- downloadHandler(
    filename = paste0(select_matrix(),'_line.png'),
    content = function(file) {
      ggsave(file,line_pFEV(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
    }
  )

  output$smooth_line_pFEV_title_ui = renderUI(textInput('smooth_line_pFEV_title','Title',paste(select_matrix(), 'smoothed')))
  #column(3,textInput('smooth_line_pFEV_x','x title','months')),
  output$smooth_line_pFEV_y_ui = renderUI(textInput('smooth_line_pFEV_y','y title',gsub('_matrix','',select_matrix())))
  smooth_line_pFEV = reactive({
    r_data = pFEV_lf_r()
    title = paste('SMOOTHED curve fitted to',select_matrix(),' values for ',length(unique(r_data$MRN))," Patients")
    p = smooth_line_plot_function(r_data,input$smooth_line_pFEV_title,input,input$smooth_line_pFEV_x,input$smooth_line_pFEV_y)
    p 
  })
  output$smooth_line_pFEV = renderPlot(smooth_line_pFEV())
  
  output$smooth_line_pFEV_download <- downloadHandler(
    filename = paste0(select_matrix(),'smooth_line.png'),
    content = function(file) {
      ggsave(file,smooth_line_pFEV(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
    }
  )

  ################  _BOXPLOTS #######################
  
  output$boxplot_pFEV_title_ui = renderUI(textInput('boxplot_pFEV_title','Title',paste(select_matrix(), 'boxplot')))
  #column(3,textInput('boxplot_pFEV_x','x title','months')),
  output$boxplot_pFEV_y_ui = renderUI(textInput('boxplot_pFEV_y','y title',gsub('_matrix','',select_matrix())))
  
  boxplot_pFEV = reactive({
    full_data = pFEV_lf_r()
    title = paste(select_matrix(),' values for ',length(unique(full_data$MRN))," Patients")
    boxplot_function(full_data,input$boxplot_pFEV_title,input,input$boxplot_pFEV_x,input$boxplot_pFEV_y)
  })
  
  output$boxplot_pFEV = renderPlot(boxplot_pFEV())
    
  output$boxplot_pFEV_download <- downloadHandler(
    filename = paste0(select_matrix(),'boxplot.png'),
    content = function(file) {
      ggsave(file,boxplot_pFEV(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
    }
  )

  
  output$boxplot_pFEV_cluster = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste(select_matrix(),'values for ',length(unique(full_data$MRN))," Patients")
    cols = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
  
    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$boxplot_pFEV_cluster_full = renderPlot({
    #full_data = pFEV_lf_r()
    full_data = change_data_l_clust()
    global_factor = 'cluster'
    title = paste(input$data_select_clust,' values for ',length(unique(full_data$MRN))," Patients")
    cols = factor(c(input$pre_range[1]:input$post_range[2]))

    boxplot_4_cluster_function(full_data,title,global_factor,cols,input)
  })
  
  output$interaction_plot = renderPlot({
    full_data = pFEV_lf_r()
    global_factor = 'cluster'
    title = paste(select_matrix(),' values for ',length(unique(full_data$MRN))," Patients")
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
  

  output$boxplot_pFEV_mean_title_ui = renderUI(textInput('boxplot_pFEV_mean_title','Title',paste(select_matrix(), 'boxplot mean')))
  #column(3,textInput('boxplot_pFEV_mean_x','x title','months')),
  output$boxplot_pFEV_mean_y_ui = renderUI(textInput('boxplot_pFEV_mean_y','y title',gsub('_matrix','',select_matrix())))
  
  
  boxplot_pFEV_mean = reactive({
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
      
      ggtitle(input$boxplot_pFEV_mean_title) + 
      xlab(input$boxplot_pFEV_mean_x)+
      ylab(input$boxplot_pFEV_mean_y)
  })
  
  output$boxplot_pFEV_mean = renderPlot(boxplot_pFEV_mean())

  output$boxplot_pFEV_mean_download <- downloadHandler(
    filename = paste0(select_matrix(),'boxplot_mean.png'),
    content = function(file) {
      ggsave(file,boxplot_pFEV_mean(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
    }
  )
  
  

  
############# STATS #####################
  
  
    ## _Linear Regression and ANOVA ####  
        ####### __LM ########
 
            df_lm = reactive({
                df = data.frame(Factor = numeric(), Status = numeric(0))

                factor = 'MRN'
                factor = input$global_factor
                cols = c(-6:6)
                cols = factor(c(input$pre_range[1]:input$post_range[2]))
                #cols
                #full_data=pFEV_lf[pFEV_lf$variable %in% cols,]
                function_data = pFEV_lf_r()
                df = lm_function(function_data,factor,cols)
                #df = df[order(df$Status),]
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
                #df
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
              #cols
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
              #cols
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
              #cols
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
            #df
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
                  
                  pp_t_test_ranges_full = reactive({
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
                    df
                    #significance_table_formatting_function(df,input$mtc)
                  })
                  
                  output$pp_t_test_ranges_full = renderDataTable({
                    df = pp_t_test_ranges_full()
                    significance_table_formatting_function(df,input$mtc)
                  })
                    
                  
                  
                  output$pp_t_test_ranges_full_download <- downloadHandler(
                    filename = paste0(select_matrix(),'_t_test_',input$pre_range[1],'_',input$post_range[2],'_ranges.csv'),
                    content = function(file) {
                      write.csv(pp_t_test_ranges_full(), file)
                    }
                  )
                  
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
                    #print(colnames(pp_data))
                    #View(pp_data)
                    pp_data
                  })
                  
                  output$boxplot_pp_ranges_title_ui = renderUI({
                    textInput('boxplot_pp_ranges_title','Title',paste(select_matrix(), 'boxplot (', input$pre_range[1],' vs ',input$post_range[2],')'))
                  })
                  output$boxplot_pp_ranges_x_ui = renderUI({
                    textInput('boxplot_pp_ranges_x','x title','')
                  })
                  output$boxplot_pp_ranges_y_ui = renderUI({
                    textInput('boxplot_pp_ranges_y','y title',gsub('_matrix','',select_matrix()))
                  })
                  
                  boxplot_pp_ranges = reactive({
                    t_boxplot_function(pp_ranges_data(),input$global_factor,input$boxplot_pp_ranges_title,input$boxplot_pp_ranges_x,input$boxplot_pp_ranges_y)
                  })
                  
                  output$boxplot_pp_ranges = renderPlot({
                    boxplot_pp_ranges()
                  })
                  
                  output$boxplot_pp_ranges_download <- downloadHandler(
                    filename = paste0(select_matrix(),'boxplot_t_test_ranges.png'),
                    content = function(file) {
                      ggsave(file,boxplot_pp_ranges(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
                    }
                  )
 
                  
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
        percentage_change_t_test_full = reactive({
          
          
          #full_data = change_data_w()
          #print(dim(full_data))
          #global_factor = input$global_factor
          #cols_num = comp_colnames
          #cols_num = cols_num[as.numeric(cols_num) >= input$pre_range & as.numeric(cols_num) <= input$post_range]
          #prefix = 'per2zero_'
          #prefix = input$vs_zero_prefix
          
          #cols_num = cols_num[cols_num %in% colnames(full_data)]
          #selected_columns = paste0(prefix,cols_num)
          #selected_columns = selected_columns[selected_columns %in% colnames(full_data)]
          #selected_w = melt(full_data, measure.vars = selected_columns)
          #selected_w
          #full_data = change_data_l()
          selected_w = change_data_l()
          print(dim(selected_w))
          global_factor = input$global_factor
          
          
          
          df_b = data.frame(estimate = numeric(0), estimate1  = numeric(0), estimate2 = numeric(0),
                            statistic = numeric(0),   p.value = numeric(0), parameter = numeric(0),
                            conf.low = numeric(0), conf.high = numeric(0), method = numeric(0),
                            alternative = numeric(0),
                            Factor = numeric(0), Status = numeric(0), comparison = numeric(0))
          df = df_b
          #cols = unique(abs(as.numeric(cols_num)))
          cols_num = unique(selected_w$time)
          #cols_num
          #cols_num = cols_num[cols_num != 0]
          cols = cols_num[cols_num > 0]
          cols
          #cols = unique()
          i = cols[2]
          i
          for(i in cols){
            #print(paste(factor(-i)))
            #print(full_data[,factor(-i)])
            #raw_data[,'0'] = full_data[,'0']
            #raw_data[,paste(factor(-i))] = full_data[,paste(factor(-i))]
            
            #raw_data[,paste(factor(i))] = full_data[,paste(factor(i))]
            
            col1 = paste0(i)
            col1
            col2 = paste0(-i)
            col2
            #raw_data[,col1] = full_data[,col1]
            #raw_data[,col2] = full_data[,col1]
            #View(raw_data)
            #output$percentage_df = renderDataTable(raw_data)
            global_factor = factor_list[1]
            global_factor
            for(global_factor in factor_list){
              factor_levels = unique(selected_w[,global_factor])
              factor_levels
              
              entry = factor_levels[1]
              for(entry in factor_levels){
                pre_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$time %in% col1]
                #pre_data = selected_w$value[selected_w[,global_factor] == entry]
                
                length(pre_data)
                post_data = selected_w$value[selected_w[,global_factor] == entry & selected_w$time %in% col2]
                length(post_data)
                #df1 = data.frame(pre = pre_data,post = post_data)
                #df1$Factor = global_factor
                #df1$Status = entry
                #df1$time = i
                #plot_df = rbind(plot_df,df1)
                
                #print(pre_data)
                #print(post_data)
                df_n = tryCatch(tidy(t.test(pre_data,post_data)), error = function(e) e = df_b)
                dim(df_n)
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
          
        })
        
        output$percentage_change_t_test_full = renderDataTable({
          df = percentage_change_t_test_full()
          significance_table_formatting_function(df,input$mtc)
          
          })
        
        percentage_change_t_test = reactive({
          selected_w = change_data_l()
          print(dim(selected_w))
          raw_data = data.frame(MRN = unique(selected_w$MRN))
         
          #print(raw_data$MRN)
          global_factor = input$global_factor
          #prefix = 'per2zero_'
          #prefix = input$vs_zero_prefix
          
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          cols_num = c(input$pre_range[1],input$post_range[2])
          cols_num = cols_num[!cols_num == 0]
          cols_num = cols_num[cols_num %in% selected_w$time]
          #selected_columns = paste0(prefix,cols_num)
         
          #selected_data = full_data[,selected_columns]
          #selected_data
         
          #selected_w = melt(full_data, measure.vars = selected_columns)
          #selected_w
        
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
            
            raw_data[,paste(factor(t1))] = selected_w$value[selected_w$time == t1]
            raw_data[,'0'] = selected_w$value[selected_w$time == 0]
            raw_data[,paste(factor(t2))] = selected_w$value[selected_w$time == t2]
            
            col1 = paste0(t1)
            col2 = paste0(t2)
            #raw_data[,col1] = full_data[,col1]
            #raw_data[,col2] = full_data[,col2]
            #View(raw_data)
            
            factor_levels = unique(selected_w[,global_factor])
            entry = factor_levels[1]
            entry
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
          
          boxplot_pp_ratio_plot_function(boxplot_pp_ratio_data(),input$global_factor,title,input)
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
          #full_data = pFEV_lf_r()
          full_data = change_data_l_clust()
          
          df_s = pp_t_test_ratio_cluster()
          t1 = input$pre_range[1]
          t2 = input$post_range[2]
          print(dim(df_s))
          global_factor = 'cluster'
          
          df = boxplot_pp_ratio_data_function(full_data,global_factor,t1,t2,df_s)
          ##View(df)
          df
          
        })
        output$boxplot_pp_ratio_cluster = renderPlot({
          print('boxplot_pp_ratio_cluster')
          title = paste0('T test of ',input$data_select_clust,' log2( 0/',input$pre_range[1],' )  vs log2( ',input$post_range[2],'/0 )')
          
          boxplot_pp_ratio_plot_function(boxplot_pp_ratio_data_cluster(),'cluster',title,input)
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
        
      pp_t_test_zero_full = reactive({
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
          df
          #significance_table_formatting_function(df,input$mtc)
        })
      
      output$pp_t_test_zero_full = renderDataTable({
        df = pp_t_test_zero_full()
        significance_table_formatting_function(df,input$mtc)
        
      })
      
      output$pp_t_test_zero_full_download <- downloadHandler(
        filename = paste0(select_matrix(),'_t_test_',input$pre_range[1],'_0_',input$post_range[2],'.csv'),
        content = function(file) {
          write.csv(pp_t_test_zero_full(), file)
        }
      )
        
      
      output$pp_t_table_zero = renderDataTable({
        df = pp_t_test_zero()
        significance_table_formatting_function(df,input$mtc)
      })

      
      output$boxplot_pp_zero_title_ui = renderUI({
        textInput('boxplot_pp_zero_title','Title',paste(select_matrix(), 'boxplot (', input$pre_range[1],' vs 0 vs ',input$post_range[2],')'))
      })
      output$boxplot_pp_zero_x_ui = renderUI({
        textInput('boxplot_pp_zero_x','x title','')
      })
      output$boxplot_pp_zero_y_ui = renderUI({
        textInput('boxplot_pp_zero_y','y title',gsub('_matrix','',select_matrix()))
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
      boxplot_pp_zero = reactive({
        boxplot_pp_zero_plot_function(boxplot_pp_zero_data(),input$boxplot_pp_zero_title,input$boxplot_pp_zero_x,input$boxplot_pp_zero_y)
      })
      
      output$boxplot_pp_zero = renderPlot({boxplot_pp_zero()})
      
      output$boxplot_pp_zero_download <- downloadHandler(
        filename = paste0(select_matrix(),'boxplot',input$pre_range[1],'_0_',input$post_range[2],'.png'),
        #filename = gsub('-','neg_',filename),
        content = function(file) {
          ggsave(file,boxplot_pp_zero(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
        }
      )
      
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
      # mean_df ----------------------------
      mean_df = reactive({
        full_data = pFEV_wf
        full_data = pFEV_wf_r()
        full_data = change_data_l()
        full_data = full_data[!duplicated(full_data$MRN),]
        #full_data = full_data[full_data$cluster %in% input$cluster_select_clusters,]
        cmd = paste0("full_data = full_data[full_data$",input$global_factor," %in% input$cluster_select_clusters,]")
        eval(parse(text = cmd))
        print(dim(full_data))
        #long_data = pFEV_lf_r()
        #long_data = change_data_l()
        #cluster_data = change_data_w()[change_data_w()$cluster %in% input$cluster_select_clusters,]
        
        
        
        save_test = F
        if(save_test == T){
          variable_list = c('full_data')
          cmd_list = save_variable_function(variable_list)
          lapply(cmd_list, function(x) eval(parse(text = x)))
          save_input_function(input)
          read_test = F
          if(read_test == T){
            variable_list = c(variable_list,'input')
            cmd_list = read_variable_function(variable_list)
            for(cmd in cmd_list){
              print(cmd)
              eval(parse(text = cmd))
            }
          }
        }

        
        global_factor = 'Status'
        global_factor = input$global_factor
 
        factor_list = unique(full_data[,global_factor])
        factor_list
        #print(factor_list)
        data = full_data[,continuous_columns]
        mean_df = data.frame('pFEV' = continuous_columns)
        mean_list = as.data.frame(apply(data,2,function(x) mean(x,na.rm=T)))
        colnames(mean_list) = 'Mean for all'
        mean_df = cbind(mean_df,mean_list)
        mean_df
        entry = factor_list[1]
        entry
        
        for(entry in factor_list){
          sub_data = full_data[full_data[,global_factor] == entry,continuous_columns]
          sub_mean_list = data.frame(apply(sub_data,2,function(x) as.numeric(mean(x,na.rm=T))))
          colnames(sub_mean_list) = paste(global_factor, entry, 'avg')
          #print(sub_mean_list)
          mean_df = cbind(mean_df,sub_mean_list)
        }
        
  
        numbers = as.data.frame(t(c('Number_of_Patients',dim(full_data)[1],as.numeric(table(full_data$cluster)))))
        numbers = numbers[,c(1:dim(mean_df)[2])]
        numbers
        colnames(numbers) = colnames(mean_df)
        rownames(numbers) = 'Number_of_Patients'
        mean_df = rbind(mean_df,numbers)
    
        mean_df
        
 
        test_list = rownames(mean_df)
        test_list
        test_entry = test_list[18]
        test_entry
        #long_data[,test_entry]
        p_value_list = c()
        p_value = NA
        print(p_value)
        #long_data = full_data
        for(test_entry in test_list){
          p_value = NA
          if(test_entry %in% colnames(full_data)){
            #x = cluster_data[,test_entry]
            #x
            x = full_data[,test_entry]
            x
            if(length(x[!is.na(x)]) > 0){
              y = full_data[,'cluster']
              #p_value = try(anova(lm(long_data[,test_entry] ~ long_data[,'time'] + long_data[,'cluster']))$"Pr(>F)"[2])
              #p_value = try(anova(lm(long_data[,test_entry] ~ long_data[,'cluster']))$"Pr(>F)")
              #p_value
              #anova(x ~ y)
              #fit = aov(x ~ y)
              #fit
              #summary(fit)
              #unlist(summary(fit))
              #p_value = unlist(summary(fit))["Pr(>F)1"]
              #p_value
              #p_value = fit$"Pr(>F)"[2]
              df_n = tidy(aov(x ~ y))
              p_value = df_n$p.value[1]
              p_value
              
              }
          }
          p_value_list = c(p_value_list,p_value)
        }
        
        mean_df$p.value = signif(as.numeric(p_value_list),3)
        mean_df
        #mean_df = mean_df[-1]
        mean_df[,c(2:dim(mean_df)[2])] = apply(mean_df[,c(2:dim(mean_df)[2])], 2, function(x) signif(as.numeric(x),3))
        mean_df = mean_df[-1]
        
        str(mean_df)
        dim(mean_df)
        mean_df
        
        # for(global_factor in clustering_continuous_columns[c(2:length(clustering_continuous_columns))]){
        #   print(global_factor)
        #   x = cluster_data[,global_factor]
        #   if(length(x[!is.na(x)]) > 0){
        #     y = cluster_data[,'cluster']
        #     y
        #     df_n = tidy(aov(x ~ y))
        #     df_n$term[1] = global_factor
        #     df = rbind(df,df_n[1,])
        #   }
        #   
        # }

      })
      
      #output$pFEV_mean_table = renderDataTable({
      #  mean_df()
      #})
      
      output$pFEV_mean_table = renderDataTable({
        df = mean_df()
        #dim(df)
        #df = apply(df, 2, function(x) ifelse(is.numeric(x), signif(x,3), x))
        
        significance_table_formatting_function(df,input$mtc)
      })
      
      
      pFEV_2_zero = reactive({
        full_data = comp_data()
        #full_data = pFEV_wf_r()
        
        pre_times = pFEV_numeric_colnames_f[pFEV_numeric_colnames_n < 0]
        #pre_times
        post_times = pFEV_numeric_colnames_f[pFEV_numeric_colnames_n > 0]
        
        zero_data = full_data[,'0']
        #zero_data
        
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
        #per_data
        #class(per_data)
        list(ratio_data = ratio_data, per_data = per_data)
        
      })
      
      output$pFEV_ratio2zero = renderDataTable(table_formatting_function(pFEV_2_zero()$ratio_data))
      
      output$pFEV_per2zero = renderDataTable(table_formatting_function(pFEV_2_zero()$per_data))
      
      output$ratio_input_ui = renderUI({
        selectInput('ratio_num','timepoints',unique(change_data_l()$variable),multiple = T,selected = seq(input$pre_range[1],input$post_range[2],by = 1),width = 600)
      })

      
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
        #colnames(full_data)
        ratio_df = data.frame(MRN = full_data$MRN)
        i = 1
        for(i in sym_times_cols){
          #print(i)
          r1 = full_data[,as.character(-(i))]
          #r1
          r2 = full_data[,as.character((i))]
          #r2
          #log2(r2/r1)
          #as.character(i)
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
        #colnames(full_data)
        ratio_df = data.frame(MRN = comp_data()$MRN)
        #i = 1
        for(i in sym_times_cols){
          #print(i)
          pre = full_data[,paste0('per2zero_',as.character(-(i)))]
          #pre
          #print(r1)
          post = full_data[,paste0('per2zero_',as.character((i)))]
          #r2
          #print(r2)
          #print(log2(r2/r1))
          #as.character(i)
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
        #colnames(full_data)
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
          #as.character(i)
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
        full_data = change_data_l()
        #plot_data = melt(full_data, measure.vars = sym_cols())
        plot_data = full_data[full_data$variable %in% input$ratio_num,]
        #print(dim())
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
        # full_data = change_data_w()
        # print(dim(full_data))
        # plot_data = melt(full_data, measure.vars = sym_cols())
        # global_factor = 'Status'
        # global_factor = input$global_factor
        # ggplot(plot_data, aes_string(x = 'variable', y = 'value', col = global_factor)) +
        #   coord_cartesian(ylim = c(as.numeric(input$sym_y_lim[1]),as.numeric(input$sym_y_lim[2]))) + 
        #   geom_hline(yintercept = 0) + 
        #   
        #   geom_boxplot()
        
        full_data = change_data_l()
        plot_data = full_data[full_data$variable %in% input$ratio_num,]
        global_factor = 'Status'
        global_factor = input$global_factor
        
        save_test = F
        if(save_test == T){
          variable_list = c('full_data','plot_data')
          cmd_list = save_variable_function(variable_list)
          lapply(cmd_list, function(x) eval(parse(text = x)))
          try(save_input_function(input))
          read_test = F
          if(read_test == T){
            variable_list = c(variable_list,'input')
            cmd_list = read_variable_function(variable_list)
            for(cmd in cmd_list){
              print(cmd)
              try(eval(parse(text = cmd)))
            }
          }
        }
        
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
        
        full_data = change_data_l()
        global_factor = input$global_factor
        factor_list = unique(full_data[,global_factor])
        #factor_list
        entry = factor_list[1]
        full_data = cbind(change_data_w()[,c('MRN','cluster')],change_data_w()[,select_matrix()])
        #View(sub_data)
        #sub_data$MRN = rownames(sub_data)
        #sub_data$cluster = change_data_w()$cluster[match(change_data_w()$MRN,sub_data)]
        #sub_data = full_data[full_data[,global_factor] == entry,sym_cols()]
        sub_data = full_data[full_data[,global_factor] == entry,input$ratio_num]
        #View(sub_data)
        mean_ratio_df = data.frame(t(apply(sub_data,2, function(x) mean(x,na.rm=T))))
        #View(mean_ratio_df)
        colnames(mean_ratio_df) = colnames(sub_data)
        mean_ratio_df$Factor = global_factor
        mean_ratio_df$Status = entry
        entry = factor_list[2]
        for(entry in factor_list[-1]){
          sub_data = full_data[full_data[,global_factor] == entry,input$ratio_num]
          mean_ratio_df_n = data.frame(t(apply(sub_data,2, function(x) mean(x,na.rm=T))))
          colnames(mean_ratio_df_n) = colnames(sub_data)
          
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
        #full_data = change_data_w()
        global_factor = input$global_factor
        #ratio_colnames = paste0('log2(',sym_times_cols,')')
        #per_colnames = paste0('per_',sym_times_cols)
        full_data = change_data_l()
        ratio_stat_data = full_data[full_data$variable %in% input$ratio_num,]
        #ratio_stat_data = melt(full_data, measure.vars = sym_cols())
        #View(ratio_stat_data)
        #print(colnames(ratio_stat_data))
        #print(ratio_stat_data$value)
        #per_stat_data = melt(full_data, measure.vars = sym_per_colnames)
        time = sym_ratio_colnames[7]
        for(time in input$ratio_num){
          time_data = ratio_stat_data[ratio_stat_data$variable == time,]
          #cmd = paste("anova_df = tidy(manova(cbind(variable,value) ~ ",global_factor,", data = stat_data))")
          anova_df_n = tryCatch(tidy(anova(lm(time_data$value ~ time_data[,global_factor]))), error = function(e) e = anova_df_b)
          if(dim(anova_df_n)[1] > 0){
            anova_df_n$Factor = global_factor
            anova_df_n$time = time
            anova_df = rbind(anova_df,anova_df_n)
          }
        }
        #anova_df
        df = col_rearrange_function(anova_df,2)
        df
        
      })
      
      manova_sym = reactive({
        
        #full_data = sym_df
        global_factor = 'HLAType'
        #full_data = change_data_w()
        ##View(full_data)
        global_factor = input$global_factor
        #sym_times_cols_selected = sym_cols()
        #print(sym_times_cols_selected)
        #ratio_colnames = paste0('log2(',sym_times_cols_selected,')')
        #per_colnames = paste0('per_',sym_times_cols)
        
        #ratio_stat_data = melt(full_data, measure.vars = sym_times_cols_selected)
        full_data = change_data_l()
        ratio_stat_data = full_data[full_data$variable %in% input$ratio_num,]
        manova_df = pairwise_manova_function(ratio_stat_data,global_factor,input)
        #manova_df$range = paste(sym_times_cols_selected,collapse = ', ')
        manova_df$range = paste(input$ratio_num,collapse = ', ')
        
        manova_df
        
      })
      
      output$manova_sym_table = renderDataTable({
        df = manova_sym()
        significance_table_formatting_function(df,input$mtc)
      })
      
      
      output$anova_pw_sym_ratio = renderDataTable({
        df =  anova_sym()
        #df
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
        full_data = change_data_l()
        
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
        #ratio_data = melt(full_data,measure.vars = sym_cols())
        ratio_data = full_data[full_data$variable %in% input$ratio_num,]
        for(time in input$ratio_num){
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
                #t_df_n
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
        #t_df
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
        full_data = change_data_l()
        
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
        #ratio_data = melt(full_data,measure.vars = sym_cols())
        ratio_data = full_data[full_data$variable %in% input$ratio_num,]
        for(time in input$ratio_num){
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
        #t_df
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
      
      
  ### MANOVA ###
      output$full_manova_table = renderDataTable({
        data = pFEV_lf
        cols = factor(c(-6,6))
        data = pFEV_lf_r()
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,discrete_columns_4_comparison[1],input)
        #print(discrete_columns_4_comparison[1])
        #df
        for(m_factor in discrete_columns_4_comparison[-1]){
          #print(m_factor)
          #print(unique(data[,m_factor]))
          data[,m_factor][is.na(data[,m_factor])] = 'NA'
          if(length(na.omit(data[,m_factor])) > 50){
            if(length(na.omit(unique(data[,m_factor]))) > 1){
              df_n = tryCatch(pairwise_manova_function(function_data,m_factor,input), error = function(e) e = data.frame(d = numeric(0)))
              #print(df_n)
              
              if(dim(df_n)[1] > 0){
                df = rbind(df,df_n)
              }
            }}}
        #df
        significance_table_formatting_function(df,input$mtc)
        
      })

      output$boxplot_pFEV_manova = renderPlot({
        full_data = pFEV_lf_r()
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
        df = pairwise_manova_function(function_data,global_factor,input)
        significance_table_formatting_function(df,input$mtc)
        
      })
      selected_manova_table_cluster = reactive({
        #data = pFEV_lf
        #data = pFEV_lf_r()
        data = change_data_l_clust()
        global_factor = 'cluster'
        #global_factor = input$global_factor
        cols = factor(c(-6,6))
        cols = factor(c(input$pre_range[1]:input$post_range[2]))
        function_data = data[data$variable %in% cols,]
        df = pairwise_manova_function(function_data,global_factor,input)
        df
        #df = significance_table_formatting_function(df,input$mtc)
        #df

      })
      
      output$selected_manova_table_cluster = renderDataTable({
        df = selected_manova_table_cluster()
        significance_table_formatting_function(df,input$mtc)
        })
      
      output$selected_manova_table_cluster_download <- downloadHandler(
        filename = 'manova_cluster.csv',
        content = function(file) {
          write.csv(selected_manova_table_cluster(), file)
        }
      )
        
      
  
  # CLUSTERING ---------------------------------
  
      discrete_cluster = reactive({
        full_data = change_data()
        
        #print(colnames(full_data))
        if(input$run_clustering == T){
          cluster_data_list = clustering_function(full_data,pre_retained_patients(),input$clutree_num,
                                                  input$fac_weight,input$mix_clust_col_fac,input$fac_weight_2,input$mix_clust_col_fac_2,
                                                  input$num_weight,input$mix_clust_col_num,input$num_weight_2,input$mix_clust_col_num_2)
          #return(list(data_dist = data_dist, D = D, o_data = o_data, data = data, x_cluster = x_cluster, weights = weights))
          #save_cluster_data = save_data
          #save_cluster_data = T
 
          #cluster_data_list
        }else{
          cluster_data_list = readRDS('www/cluster_data_list.rds')
        }
        cluster_data_list
        
      })
  
    
      discrete_cluster_D = reactive({
        cluster_data_list = discrete_cluster()
        data = cluster_data_list$data
        #rename_clusters = F
        if(input$rename_clusters == T){
          #print(dim(cluster_data_list))
          #dim(cluster_data_list)
          
          data$cluster = as.character(data$cluster)
          i = 4
          for(i in c(1:input$clutree_num)){
            cmd = paste0("data$cluster[data$cluster == '",i,"'] = input$cluster_", i)
            print(cmd)
            eval(parse(text = cmd))
            cmd = paste0("cluster_data_list$x_cluster[2][cluster_data_list$x_cluster[2] == '",i,"'] = input$cluster_", i)
            print(cmd)
          }
          data$cluster
          cluster_data_list$data = data
        }
        if(save_data == T){
          saveRDS(cluster_data_list,'www/cluster_data_list.rds')
        }
        cluster_data_list
        
        
      })
  
  
        # plot clusters ==================================================
            output$D_text = renderPrint(str(discrete_cluster_D()$D,indent.str = '<br />'))

            
          discrete_cluster_plot = reactive({
              print('discrete_cluster_plot')
              D = discrete_cluster_D()$D
              dendr <- dendro_data(D, type = "rectangle") 
              x_cluster = discrete_cluster_D()$x_cluster
              cut = input$clutree_num
              cut
              p = dendrogram_plot_function(dendr,x_cluster,cut,input)
              p
            })  
          output$discrete_cluster_plot = renderPlot({discrete_cluster_plot()})
          
          output$discrete_cluster_plot_download <- downloadHandler(
            filename = 'dendogram.png',
            content = function(file) {
              ggsave(file,discrete_cluster_plot(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
            }
          )
            
            output$discrete_x_table = renderDataTable({
              x_cluster = discrete_cluster_D()$x_cluster
              x_cluster_table = data.frame(num = numeric(0),MRN = numeric(0))
              for(entry in unique(x_cluster$cluster)){
                line_list  = x_cluster[x_cluster$cluster == entry,'label']
                line = paste(line_list,collapse = (', '))
                #line
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
            
            mix_clu = reactive({
              data = discrete_cluster_D()$o_data
              weights = discrete_cluster_D()$weights
              D = discrete_cluster_D()$D
              mix.heatmap(data,dend.subjects = D,rowmar = 15,D.variables = NULL,legend.mat = T,varweights = weights)
              
              })
            
            output$mix_clu = renderPlot(mix_clu())
            
            
            
            output$mix_clu_download <- downloadHandler(
              filename = 'clumix.png',
              content = function(file) {
                png(file, height = input$ggsave_height, width = input$ggsave_width, units = 'in', res = 300)
                data = discrete_cluster_D()$o_data
                weights = discrete_cluster_D()$weights
                D = discrete_cluster_D()$D
                mix.heatmap(data,dend.subjects = D,rowmar = 15,D.variables = NULL,legend.mat = T,varweights = weights)
                
                dev.off()
                #ggsave(file,mix_clu(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
              }
            )
  
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
            
            
            distance_density = reactive({
              xy = distance_model()
              #saveRDS(xy, 'temp/xy.rds')
              #xy = readRDS('temp/xy.rds')
              data = xy
              
              if(input$rename_clusters == T){
                data$cluster = as.character(data$cluster)
                i = 4
                clutree_num = 4
                clutree_num = input$clutree_num
                for(i in c(1:clutree_num)){
                  #cmd = paste0("data$cluster[data$cluster == '",i,"'] = 'cluster_", i,"'")
                  cmd = paste0("data$cluster[data$cluster == '",i,"'] = input$cluster_", i)
                  print(cmd)
                  eval(parse(text = cmd))
     
                }
              }
              
              p = ggplot(data, aes(x, y, colour=cluster)) + 
                geom_point( size=3) +
                geom_density2d(alpha=0.5)
              p = p + ggtitle(input$distance_density_title)
              p = p + xlab(input$distance_density_x)
              p = p + ylab(input$distance_density_y)
              p
            })
            output$distance_density = renderPlot(distance_density())
            output$distance_density_download <- downloadHandler(
              filename = 'distance_density.png',
              content = function(file) {
                ggsave(file,distance_density(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
              }
            )
            
            
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
            proportion_table_formating_factor(df,col_range,colour,input$mtc)
          })
          
          output$cluster_analysis_download <- downloadHandler(
            filename = 'chi_squared_factor_proportions.csv',
            content = function(file) {
              write.csv(cluster_analysis_total(), file)
            }
          )
          
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
            proportion_table_formating_factor(df,col_range,colour,input$mtc) 
          })
          
          ### __WITHIN ####
          cluster_analysis_within = reactive({
            df = pFEV_wf_r()
            df_tc = clust_comparison_within(df,'cluster',input)
            df_tc
          })
        

          
          output$cluster_analysis_within_table = DT::renderDataTable({
            df = cluster_analysis_within()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            col_range = input$cluster_select_clusters
            print(col_range)
            proportion_table_formating_within(df,col_range,colour,input$mtc)
          })
          
          output$cluster_analysis_within_download <- downloadHandler(
            filename = 'cluster_analysis_within_proportions.csv',
            content = function(file) {
              write.csv(cluster_analysis_within(), file)
            }
          )
          
          cluster_analysis_within_table_selected_df = reactive({
            df = cluster_analysis_within()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })
          
          output$cluster_analysis_within_table_selected_table = DT::renderDataTable({
            df = cluster_analysis_within_table_selected_df()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            print(col_range)
            print(colnames(df))
            col_range = input$cluster_select_clusters
            print(col_range)
            proportion_table_formating_within(df,col_range,colour,input$mtc)
          })
          
          
          chisq_within_p = reactive({
            data = cluster_analysis_within()
            factors = unique(data$Factor)
            factors
            factor = factors[2]
            factor
            clusters = input$cluster_select_clusters
            clusters
            cluster = clusters[1]
            df_p = data.frame(NULL)
            df_p
            for(factor in factors){
              print(factor)
              for(cluster in clusters){
                print(cluster)
                values = data[data$Factor == factor, input$clus]
                status = data[data$Factor == factor,'Status']
                print(values)
                values = values[!is.na(values)]
                if(length(values) > 1){
                  result = tidy(chisq.test(values))
                  result_cols = colnames(result)
                  result
                  result$Factor = factor
                  result$cluster = cluster
                  result$proportions = paste(values,collapse = ', ')
                  result$Status = paste(status,collapse = ', ')
                  result
                  if(dim(df_p)[1] == 0){
                    df_p = result
                    
                  }else{
                    df_p = rbind(df_p,result)
                  }
                }
              }  
            }
            df_p = df_p[,c('cluster','Factor','Status','proportions',result_cols)]
            colnames(df_p)
            df_p
          })
          
          output$cluster_analysis_within_p_table = DT::renderDataTable({
            df = chisq_within_p()
            significance_table_formatting_function(df,input$mtc)

          })
          
          output$cluster_analysis_within_p_download <- downloadHandler(
            filename = 'cluster_analysis_within_proportions_stat.csv',
            content = function(file) {
              write.csv(chisq_within_p(), file)
            }
          )
          
          cluster_analysis_within_p_table_selected_df = reactive({
            df = chisq_within_p()
            df_selected = df[df$Factor %in% c(input$mix_clust_col_fac,input$mix_clust_col_fac_2),]
            df_selected
          })
          
          output$cluster_analysis_within_p_table_selected_table = DT::renderDataTable({
            df = cluster_analysis_within_p_table_selected_df()
            significance_table_formatting_function(df,input$mtc)
            
          })
     

          
          output$cluster_analysis_within_table_selected_table = DT::renderDataTable({
            df = cluster_analysis_within_table_selected_df()
            colour = 'lightblue'
            col_range = c(3:(2+input$clutree_num)) # find a better way to do this
            print(col_range)
            print(colnames(df))
            col_range = input$cluster_select_clusters
            print(col_range)
            proportion_table_formating_within(df,col_range,colour,input$mtc)
          })


          ### __CHISQ ####
          
          
          output$cluster_select_clusters_old = renderUI({
            if(input$run_clustering == T){
              clusters <- unique(discrete_cluster_D()$data$cluster)
            }else{
              cluster_data_list = readRDS('www/cluster_data_list.rds')
              clusters = unique(cluster_data_list$data$cluster)
            }
            selectInput("cluster_select_clusters", "Choose Clusters to Use in Analysis", choices = clusters, selected = clusters,multiple = T,width = 1500)
          })
          
          output$global_factor_ui = renderUI({
            print('global_factor_ui')
            factor_list = c(all_discrete_columns,'cluster')
            factor_list = factor_list[order(factor_list)]
            selectInput('global_factor','Factor to Separate by',factor_list,multiple = F,selected = 'cluster')
            
          })
          
          output$cluster_select_clusters = renderUI({
            print('cluster_select_clusters')
            factors = tryCatch(unique(change_data_full_w()[,input$global_factor]),error = function(e) {'error'})
            factors
            if(factors == 'error'){
              HTML(paste(tags$span(style="color:red", 'Run Clustering before continuing ...')))
              
              #tags$h5('Run Clustering before continuing ....')
            }else{
              factors = factors[order(factors)]
              
              #factor_list = list(factors)
              #factor_list
              factor_list = lapply(factors, function(x) x)
              names(factor_list) = factor(factors)
              
              factor_list
              #if(input$run_clustering == T){
              #  clusters <- unique(discrete_cluster_D()$data$cluster)
              #}else{
              #  cluster_data_list = readRDS('www/cluster_data_list.rds')
              #  clusters = unique(cluster_data_list$data$cluster)
              #}
              selectInput("cluster_select_clusters", "Choose Elements to Use in Analysis", choices = factor_list,selected = factors,multiple = T,width = 1500)
            }
            })
          
          output$cluster_select_clusters_anova <- renderUI({
            #data = cluster_analysis_within_table_selected_df()
            #test_data = data[,c(3:dim(data)[2])]
            clusters <- seq(1,input$clutree_num,1)
            selectInput("cluster_select_clusters_anova", "Choose Clusters to Test", choices = clusters, selected = clusters,multiple = T)
          })
          output$chisq_cluster = renderDataTable({
            full_data = cluster_analyis_selected_df()
            chi_df = chisq_total(full_data,input)
            chi_df
            significance_table_formatting_function(chi_df,input$mtc)
          })
          
         chisq_cluster_full = reactive({
            full_data = cluster_analysis_total()
            chi_df = chisq_total(full_data,input)
            chi_df
            #significance_table_formatting_function(chi_df,input$mtc)
          })
          
          output$chisq_cluster_full = renderDataTable({
            chi_df = chisq_cluster_full()
            significance_table_formatting_function(chi_df,input$mtc)
          })
          
          output$chisq_cluster_full_download <- downloadHandler(
            filename = 'chi_squared_factor_stat.csv',
            content = function(file) {
              write.csv(chisq_cluster_full(), file)
            }
          )
          
          
          output$chisq_cluster_within = renderDataTable({
            print('chisq_cluster_within')
            data = cluster_analysis_within_table_selected_df()
            chi_df = chisq_within(data,input)
            chi_df
            significance_table_formatting_function(chi_df,input$mtc)
          })
          output$chisq_cluster_within_full = renderDataTable({
            data = cluster_analysis_within()
            chi_df = chisq_within(data,input)
            chi_df
            significance_table_formatting_function(chi_df,input$mtc)
          })
          
          output$chisq_cluster_within_full_download <- downloadHandler(
            filename = 'chi_squared_within_stat.csv',
            content = function(file) {
              write.csv(cluster_analysis_within(), file)
            }
          )
          
          ##### MANOVA #####
          
          output$anova_cluster_plot = renderPlot({
            global_factor = input$continuous_variable
            full_data = change_data_w()
            cluster_data = full_data
            #cluster_data = full_data[full_data$cluster %in% input$cluster_select_clusters,]
            #cmd = paste0("cluster_data = full_data[full_data$",input$global_factor," %in% input$cluster_select_clusters,]")
            #eval(parse(text = cmd))
            plot_data = melt(cluster_data,measure.vars = global_factor)
            ggplot(plot_data, aes(x = cluster,y = value,col=cluster)) +
              geom_boxplot()
          })
          
          output$continuous_manova_single = renderDataTable({
            full_data = change_data_w()
            cluster_data = full_data
            #cluster_data = full_data[full_data$cluster %in% input$cluster_select_clusters,]
            global_factor = input$continuous_variable
            x = cluster_data[,input$continuous_variable]
            y = cluster_data[,'cluster']
            df = tidy(aov(x ~ y))
            df$term[1] = global_factor
            significance_table_formatting_function(df,input$mtc)
          })
          
          output$continuous_manova_full = renderDataTable({

            full_data = change_data_l()
            full_data = full_data[!duplicated(full_data$MRN),]
            cluster_data = full_data
            #cluster_data = full_data[full_data$cluster %in% input$cluster_select_clusters,]
            print(dim(cluster_data))
            
            variable_list = c('cluster_data','clustering_continuous_columns')
            save_test = F
            if(save_test == T){
              cmd_list = save_variable_function(variable_list)
              lapply(cmd_list, function(x) eval(parse(text = x)))
              save_input_function(input)
            }
            read_test = F
            if(read_test == T){
              variable_list = c(variable_list,'save_input')
              cmd_list = read_variable_function(variable_list)
              for(cmd in cmd_list){
                print(cmd)
                eval(parse(text = cmd))
              }
            }
            x = cluster_data[,continuous_columns[1]]
            y = cluster_data[,'cluster']
            df = tidy(aov(x ~ y))
            df$term[1] = clustering_continuous_columns[1]
            df = df[1,]
            
            for(global_factor in clustering_continuous_columns[c(2:length(continuous_columns))]){
              print(global_factor)
              x = cluster_data[,global_factor]
              if(length(x[!is.na(x)]) > 0){
                y = cluster_data[,'cluster']
                y
                df_n = tidy(aov(x ~ y))
                df_n$term[1] = global_factor
                df = rbind(df,df_n[1,])
              }
              
            }
            significance_table_formatting_function(df,input$mtc,F)

          })
          
          continuous_manova_cluster = reactive({
            full_data = change_data_w()
            cluster_data = full_data
            
            selected_columns = c(input$mix_clust_col_num,input$mix_clust_col_num_2)
            print(selected_columns)
            
            x = cluster_data[,selected_columns[1]]
            y = cluster_data[,'cluster']
            df = tidy(aov(x ~ y))
            df$term[1] = selected_columns[1]
            df = df[1,]
            
            for(global_factor in selected_columns[c(2:length(selected_columns))]){
              x = cluster_data[,global_factor]
              y = cluster_data[,'cluster']
              df_n = tidy(aov(x ~ y))
              df_n$term[1] = global_factor
              df = rbind(df,df_n[1,])
              
            }
            df
            #significance_table_formatting_function(df,input$mtc)
            
          })
        
          output$continuous_manova_cluster = renderDataTable({
            df = continuous_manova_cluster()
            significance_table_formatting_function(df,input$mtc)
          })
          
          output$continuous_manova_cluster_download <- downloadHandler(
            filename = 'anova_cluster.csv',
            content = function(file) {
              write.csv(continuous_manova_cluster(), file)
            }
          )
 
########## BOS PLOTS ###########

  
          
          bos_df = reactive({ # full bos_df
            #full_data = pFEV_wf
            #full_data = i_pFEV_wf_r()
            full_data = processed_data
            full_data = BOS_processed_data_w_r() # need to change to retained
            print(dim(full_data))
            
            #bos_data = BOS_function_post_calc(full_data)
            bos_data = bos_df_function(full_data)
            #return(list(bos_df = bos_df,patient_status_df = patient_status_df))
            bos_data
          }) # FULL bos_df
          
          # patient_status_df= reactive({
          #   full_data = pFEV_wf
          #   full_data = pFEV_wf_r()
          #   patient_status = BOS_patient_function(full_data)
          #   #return(list(bos_df = bos_df,patient_status_df = patient_status_df))
          #   patient_status
          # })
          
          output$bos_df = renderDataTable(bos_df())
          #output$bos_patient_status = renderDataTable(patient_status_df())
          
          output$bos_plots = renderPlot({
            x1 = -12
            x2 = 12
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            #bos_df_data = bos_df$bos_df
            bos_df_data = bos_df()
            print(dim(bos_df_data))
            bos_data = bos_df_data[,c('time','RAS', 'RAS_recovery', "BOS1","BOS2","BOS3",'Survival')]
            bos_data
            m_bos = melt(bos_data,id.var = 'time')
            ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
              geom_line(lwd = 2) +
              geom_vline(xintercept = 0) +
              geom_hline(yintercept = 0)+
              xlim(x1,x2) + 
              ggtitle('BOS plot for All patients')
              
            
          })
          
          output$bos_plots_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            bos_df_data = bos_df()
            bos_data = bos_df_data[,c('time','RAS',"BOS1","BOS2","BOS3")]
            bos_data
            m_bos = melt(bos_data,id.var = 'time')
            ggplot(m_bos,aes(x = time,y=value,col=variable)) + 
              geom_smooth() +
              geom_vline(xintercept = 0) +
              geom_hline(yintercept = 0)+
              xlim(x1,x2)
            
          })
          
          bos_factor_original = reactive({
            full_data = pFEV_wf_r()
            global_factor = 'Status'
            global_factor = input$global_factor
            factor_entry = unique(na.omit(full_data[,global_factor]))
            entry = factor_entry[1]
            #factor_entry
            df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            patient_df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            for(entry in factor_entry){
              function_data = full_data[full_data[,global_factor] == entry,]
              bos_df = BOS_function_single(function_data)
              patient_status = BOS_patient_function(function_data)
              str(patient_status)
              head(patient_status)
              bos_df$Factor = global_factor
              bos_df$Status = entry
              df = rbind(df,bos_df[,c("Factor",'Status','time','BOS1','BOS2','BOS3',"Survival")])
              
              
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
          
          bos_factor = reactive({
            #full_data = i_pFEV_wf_r()
            BOS_colnames = BOS_calc_list()$BOS_colnames
            full_data = BOS_processed_data_w_r()
            print(dim(full_data))
            global_factor = 'Status'
            global_factor = input$global_factor
            factor_entry = unique(na.omit(full_data[,global_factor]))
            entry = factor_entry[1]
            #factor_entry
            
            #df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            #patient_df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            #df = BOS_function_post_calc(full_data)
            df = bos_df_function(full_data)
            # df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),RAS = numeric(0),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            # data_name_list = list(RAS = 'RAS',BOS1_RAS = 'BOS1',BOS2_RAS = 'BOS2',BOS3_RAS = 'BOS3',MonthsToDeath = 'Survival')
            # for(data_name in names(data_name_list)){
            #   print(data_name)
            #   print(paste(data_name_list[data_name]))
            #   
            #   df_n = BOS_function_single(full_data,data_name,data_name_list[data_name])
            #   if(dim(df)[1] == 0){
            #     df = df_n
            #   }else{
            #     df = cbind(df,df_n[-1])
            #   }
            # }
            
            #df_o = BOS_RAS_function(full_data)
            
            
            df$Factor = global_factor
            df$Status = 'All'
            df =  df[,c("Factor",'Status','time',BOS_colnames)]
            dim(df)
            colnames(df)
            for(entry in factor_entry){
              function_data = full_data[full_data[,global_factor] == entry,]
              bos_df = bos_df_function(function_data)
              head(bos_df)
              #patient_status = BOS_patient_function_post_calc(function_data)
              #str(patient_status)
              #head(patient_status)
              bos_df$Factor = global_factor
              bos_df$Status = entry
              df = rbind(df,bos_df[,c("Factor",'Status','time',BOS_colnames)])
              
              
              #patient_status$Factor = global_factor
              #patient_status$Status = entry
              #head(patient_df)
              #head(patient_status)
              #patient_df = rbind(patient_df,patient_status[,c("Factor",'Status','time','BOS1','BOS2','BOS3')])
            }
            ##View(df)
            m_bos = melt(df,id.vars= c('Factor','Status','time'))
            #View(m_bos)
            m_bos
          })
          
          
          output$boss_factor_table = renderDataTable(bos_factor())
          
          
          output$boss_factor_table_select = renderDataTable({
            df = bos_factor()
            df = df[df$time == input$bos_time_select & df$variable == input$boss_select,]
            table_formatting_function(df)
            })
          
          
          output$BOS_plot_ui = renderUI({
            print('BOS_plot_ui')
            BOS_colnames = BOS_calc_list()$BOS_colnames
            renderPlots_BOS(BOS_colnames, bos_factor(),input,output,prefix = 'BOS')
            makePlotContainers(BOS_colnames, prefix="BOS") 
            
          })
          #"RAS"          "RAS_recovery" "BOS1"         "BOS2"         "BOS3"         "Survival"
          #output$RAS_plot = renderPlots_BOS = function(BOS_columns,m_bos,input,output,prefix = 'BOS'){
          RAS_factor_plot = reactive({
            print('RAS_plot')
            
            #BOS_colnames = BOS_calc_list()$BOS_colnames
            #BOS_colnames
            
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            plot_name = 'RAS'
            plot_name
            p = BOS_factor_plot(bos_factor(),plot_name,global_factor,x1,x2,input$RAS_title,input$BOS_x,input$BOS_y,input)
            p
          })
          output$RAS_factor_plot = renderPlot({RAS_factor_plot()})
          
          output$RAS_factor_plot_download <- downloadHandler(
            filename = 'RAS_by_cluster.png',
            content = function(file) {
              ggsave(file,RAS_factor_plot(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
            }
          )
          
          BOS1_factor_plot = reactive({
            print('BOS1_plot')
            
            #BOS_colnames = BOS_calc_list()$BOS_colnames
            #BOS_colnames
            
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            plot_name = 'BOS1'
            plot_name
            p = BOS_factor_plot(bos_factor(),plot_name,global_factor,x1,x2,input$BOS1_title,input$BOS_x,input$BOS_y,input)
            p
          })
          output$BOS1_factor_plot = renderPlot({BOS1_factor_plot()})
          
          output$BOS1_factor_plot_download <- downloadHandler(
            filename = 'BOS1_by_cluster.png',
            content = function(file) {
              ggsave(file,BOS1_factor_plot(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
            }
          )
          
          BOS2_factor_plot = reactive({
            print('BOS2_plot')
            
            #BOS_colnames = BOS_calc_list()$BOS_colnames
            #BOS_colnames
            
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            plot_name = 'BOS2'
            plot_name
            p = BOS_factor_plot(bos_factor(),plot_name,global_factor,x1,x2,input$BOS2_title,input$BOS_x,input$BOS_y,input)
            p
          })
          output$BOS2_factor_plot = renderPlot({BOS2_factor_plot()})
          
          output$BOS2_factor_plot_download <- downloadHandler(
            filename = 'BOS2_by_cluster.png',
            content = function(file) {
              ggsave(file,BOS2_factor_plot(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
            }
          )
          
          BOS3_factor_plot = reactive({
            print('BOS3_plot')
            
            #BOS_colnames = BOS_calc_list()$BOS_colnames
            #BOS_colnames
            
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            plot_name = 'BOS3'
            plot_name
            p = BOS_factor_plot(bos_factor(),plot_name,global_factor,x1,x2,input$BOS3_title,input$BOS_x,input$BOS_y,input)
            p
          })
          output$BOS3_factor_plot = renderPlot({BOS3_factor_plot()})
          
          output$BOS3_factor_plot_download <- downloadHandler(
            filename = 'BOS3_by_cluster.png',
            content = function(file) {
              ggsave(file,BOS3_factor_plot(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
            }
          )
          
          Survival_factor_plot = reactive({
            print('Survival_plot')
            
            #BOS_colnames = BOS_calc_list()$BOS_colnames
            #BOS_colnames
            
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            plot_name = 'Survival'
            plot_name
            p = BOS_factor_plot(bos_factor(),plot_name,global_factor,x1,x2,input$Survival_title,input$BOS_x,input$BOS_y,input)
            p
          })
          output$Survival_factor_plot = renderPlot({Survival_factor_plot()})
          
          output$Survival_factor_plot_download <- downloadHandler(
            filename = 'Survival_by_cluster.png',
            content = function(file) {
              ggsave(file,Survival_factor_plot(),width = input$ggsave_width, height = input$ggsave_height,dpi = 300)
            }
          )

          
          output$RAS_recovery_factor_plot = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            #head(m_bos)
            col_name = 'RAS_recovery'
            #m_bos = m_bos[m_bos$Status != 'All',]
            p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
            print(p)
            #p
            
          })
          
          # output$RAS_factor_plot = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = input$global_factor
          #   m_bos = bos_factor()
          #   #head(m_bos)
          #   col_name = 'RAS'
          #   #m_bos = m_bos[m_bos$Status != 'All',]
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          #   #p
          # 
          # })
          # output$bos1_factor_plot = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = input$global_factor
          #   m_bos = bos_factor()
          #   #head(m_bos)
          #   col_name = 'BOS1'
          #   #m_bos = m_bos[m_bos$Status != 'All',]
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          #   #p
          # 
          # 
          # })
          # output$bos2_factor_plot = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = input$global_factor
          #   m_bos = bos_factor()
          #   col_name = 'BOS2'
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          # 
          # })
          # output$bos3_factor_plot = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = input$global_factor
          #   m_bos = bos_factor()
          #   col_name = 'BOS3'
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          # 
          # })
          # output$bos3_surv_factor_plot = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = input$global_factor
          #   m_bos = bos_factor()
          #   #head(m_bos)
          #   col_name = 'Survival'
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          #   
          # })
          # 
          # output$bos3_surv_factor_plot_cluster = renderPlot({
          #   x1 = as.numeric(input$bos_range[1])
          #   x2 = as.numeric(input$bos_range[2])
          #   global_factor = 'cluster'
          #   m_bos = bos_factor()
          #   col_name = 'Survival'
          #   p = BOS_factor_plot(m_bos,col_name,global_factor,x1,x2)
          #   print(p)
          #   
          # })
          # 
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
            #m_bos = m_bos[m_bos$Status != 'All',]
            
            col_name = 'BOS1'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
            
          })
          output$bos2_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS2'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          output$bos3_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'BOS3'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          output$bos3_factor_plot_smooth = renderPlot({
            x1 = as.numeric(input$bos_range[1])
            x2 = as.numeric(input$bos_range[2])
            global_factor = input$global_factor
            m_bos = bos_factor()
            col_name = 'Survival'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          
          bos_factor_cluster = reactive({
            full_data = pFEV_wf_r()
            #global_factor = 'Status'
            global_factor = 'cluster'
            factor_entry = unique(na.omit(full_data[,global_factor]))
            factor_entry
            df = data.frame(Factor = numeric(),Status = numeric(),time = numeric(),BOS1 = numeric(0),BOS2 = numeric(0),BOS3 = numeric())
            for(entry in factor_entry){
              function_data = full_data[full_data[,global_factor] == entry,]
              bos_df = bos_df_function(function_data)
              #bos_df = bos_data$bos_df
              bos_df$Factor = global_factor
              bos_df$Status = entry
              df = rbind(df,bos_df[,c("Factor",'Status','time','BOS1','BOS2','BOS3',"Survival")])
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
            col_name = 'Survival'
            p = BOS_factor_plot_smooth(m_bos,col_name,global_factor,x1,x2)
            print(p)
            
          })
          

          
    output$summary_table = renderDataTable({
      selected_month = input$summary_slider[2]
      selected_pre_month = input$summary_slider[1]
      df = bos_factor()
      bos_df = df[df$time == selected_month & df$variable == 'BOS3',]
      m_df = df[df$time == selected_month & df$variable == 'Survival',]
      #table_formatting_function(df)
      #bos_df = bos_factor()
      head(bos_df)
      #df = data.frame(`BOS3 @ 6 months` = numeric(0), `Mortality @ 6 months` = numeric(0), `pFEV -6 months` = numeric(0), `pFEV 6 month relative` = numeric(0))
      #df
      df = bos_df[,c('Status','value')]
      colnames(df) = c('Cluster','BOS3')
      mean_df = mean_df()
      
      df[,'Number of Patients'] = as.numeric(mean_df[mean_df$pFEV == 'Number_of_Patients',c(2:length(mean_df))])
      df[,paste('BOS3 at',selected_month,'months post treatment')] = paste(signif((100-df$BOS3),3),'%')
      df[,paste('Survival at',selected_month,'months post treatment')] = paste(signif(m_df$value,3),'%')
      #View(mean_df)
      mean_pre_6_months = as.numeric(mean_df[mean_df$pFEV == selected_pre_month,c(2:length(mean_df))])
      mean_pre_6_months
      mean_post_6_months = as.numeric(mean_df[mean_df$pFEV == selected_month,c(2:length(mean_df))])
      mean_post_6_months
      
      per_change = (mean_post_6_months-mean_pre_6_months)/mean_pre_6_months*100
      df[,paste('pFEV1',selected_month,'months pre-treatment')] = signif(mean_pre_6_months,3)
      df[,paste('pFEV1 percentage change',selected_pre_month,'months pre and',selected_month,' post treatment')] = paste(signif(per_change,3),'%')
      rownames(df) = df$Cluster
      df = df[,c(-1,-2)]
      t_df = as.data.frame(t(df))
      table_formatting_function(t_df)
    })
          
     BOS_calc_list = reactive({
        #print('BOS_calc_list')
        t1 = -6
        t2 = 6
        RAS_sequence_correction = F
        sequence_correction = T
        first_and_last = T
        measured_columns = T
        data_type = 'i'
        data = processed_data
        data_type = input$bos_data_select
        
        pFEV1_name = 'pFEV1_matrix'
        pRatio_name = 'i_pRatio_matrix'
        if(data_type != ''){
          pFEV1_name = paste(data_type,pFEV1_name,sep='_')
          #pRatio_name = paste(data_type,pRatio_name,sep='_')
        }
        
        pFEV1_name
        pRatio_name
        
        
        #t1 = input$bos_range[1]
        #t2 = input$bos_range[2]
        sequence_correction = input$sequence_correction
        first_and_last = input$first_and_last
        measured_columns = input$measured_columns
        data = processed_data_w_r()
        
        if(input$bos_slider_data == T){
          t1 = input$bos_range[1]
          t2 = input$bos_range[2]
        }else{
          df_l = processed_data_l_r()
          t1 = min(df_l$time)
          t2 = max(df_l$time)
        }
        
        #print(dim(data))
        BOS = 0.8
        RAS = 0.7
        RAS_list = c()
        RAS_recovery_list = c()
        BOS1_RAS_list = c()
        BOS2_RAS_list = c()
        BOS3_RAS_list = c()
        i = 1
        for(i in c(1:dim(data)[1])){
          #BOS1 = NA
          #print(i)
          x = data[i,]
          v_o = (x[,pFEV1_name])
          v_o
          y_o = x[,pRatio_name]
          y_o
          
          if(measured_columns == T & data_type == 'i'){
            v_o = t(data.frame(v_o[,pFEV_numeric_colnames_f]))
            v_o
          }
          
          if(first_and_last == T & data_type == 'i'){
            original = (x[,'pFEV1_matrix'])
            original
            entry_colnames = colnames(original)[!is.na(original)]
            entry_colnames
            #min_v = min(c(t1,grep(paste0('^',entry_colnames[1]),colnames(original))))
            #min_v
            #max_v = min(c(t2,grep(paste0('^',entry_colnames[length(entry_colnames)]),colnames(original))))
            #max_v
            min_v = max(c(t1,as.numeric(entry_colnames[1])))
            min_v
            max_v = min(c(t2,as.numeric(entry_colnames[length(entry_colnames)])))
            max_v
            
            col_list = colnames(v_o)[colnames(v_o) %in% as.character(seq(min_v,max_v))]
            col_list
            v_o = v_o[,col_list]
            v_o
            #y_o = y_o[,seq(min_v,max_v)]
            
          }else{
            v_o = v_o[,as.character(seq(t1,t2))]
          }
          
          
          
          v_o
          v = t(as.matrix(v_o[!is.na(v_o)]))
          v
          #colnames(v) = colnames(v_o)[!is.na(v_o)]
          y = t(as.matrix(y_o[,colnames(v)]))
          colnames(y) = colnames(v)
          y
          #v = v_o
          #y = y_o
          
          RAS = RAS_matrix_calc_function(0.8,v,y)
          RAS
          BOS1 = BOS_RAS_matrix_calc_function(0.8,v,y)
          BOS1
          BOS2 = BOS_RAS_matrix_calc_function(0.66,v,y)
          BOS2
          BOS3 = BOS_RAS_matrix_calc_function(0.5,v,y)
          BOS3
          
          RAS = ifelse(length(RAS) == 0,NA,RAS)
          BOS1 = ifelse(length(BOS1) == 0,NA,BOS1)
          BOS2 = ifelse(length(BOS2) == 0,NA,BOS2)
          BOS3 = ifelse(length(BOS3) == 0,NA,BOS3)
          RAS
          BOS1
          BOS2
          BOS3
          if(sequence_correction == T){
            if(is.finite(RAS) & is.finite(BOS1)){
              if(RAS > BOS1){
                BOS1 = NA
              }
            }
            if(is.finite(RAS) & is.finite(BOS2)){
              if(RAS > BOS2){
                BOS2 = NA
              }
            }
            if(is.finite(RAS) & is.finite(BOS3)){
              if(RAS > BOS3){
                BOS3 = NA
              }
            }
            if(is.finite(BOS1) & is.finite(BOS2)){
              if(BOS1 > BOS2){
                BOS2 = NA
              }
              if(BOS1 == BOS2){
                BOS1 = NA
              }
            }
            if(is.finite(BOS1) & is.finite(BOS3)){
              if(BOS1 > BOS3){
                BOS3 = NA
              }
              if(BOS1 == BOS3){
                BOS1 = NA
              }
            }
            if(is.finite(BOS2) & is.finite(BOS3)){
              if(BOS2 > BOS3){
                BOS3 = NA
              }
              if(BOS2 == BOS3){
                BOS2 = NA
              }
            }
          }
          if(is.finite(RAS) & is.finite(BOS1)){
            RAS_recovery = ifelse(RAS > BOS1,RAS,NA)
          }else{
            RAS_recovery = NA
          }
          
          RAS_list = c(RAS_list,RAS)
          RAS_recovery_list = c(RAS_recovery_list,RAS_recovery)
          BOS1_RAS_list = c(BOS1_RAS_list,BOS1)
          BOS2_RAS_list = c(BOS2_RAS_list,BOS2)
          BOS3_RAS_list = c(BOS3_RAS_list,BOS3)
          
        }
  
        data$RAS = RAS_list
        data$RAS_recovery = RAS_recovery_list
        data$BOS1_RAS = BOS1_RAS_list
        data$BOS2_RAS = BOS2_RAS_list
        data$BOS3_RAS = BOS3_RAS_list
        #print('test')
        #print('test')
        data$RAS_50 = data$RAS
        data$RAS_50[is.na(data$RAS_50)] = 50
        data$BOS1_RAS_50 = data$BOS1_RAS
        data$BOS1_RAS_50[is.na(data$BOS1_RAS_50)] = 50
        data$BOS2_RAS_50 = data$BOS2_RAS
        data$BOS2_RAS_50[is.na(data$BOS2_RAS_50)] = 50
        data$BOS3_RAS_50 = data$BOS3_RAS
        data$BOS3_RAS_50[is.na(data$BOS3_RAS_50)] = 50
  
        BOS_colnames = c('RAS','RAS_recovery','BOS1','BOS2','BOS3',"Survival")
        
        data
        
        print('BOS_processed_data_l_r')
        save_test = F
        if(save_test == T){
          saveRDS(data,'temp/BOS_data_w.rds')
        }
        #data = data[data$cluster %in% input$cluster_select_clusters,]
        cmd = paste0("data = data[data$",input$global_factor," %in% input$cluster_select_clusters,]")
        eval(parse(text = cmd))
        list(data = data, BOS_colnames = BOS_colnames)
        
      })
    
    
    BOS_processed_data_w_r = reactive({BOS_calc_list()$data})
    
    BOS_processed_data_l_r = reactive({
      print('BOS_processed_data_l_r')
      saveRDS(BOS_processed_data_w_r(),'temp/BOS_data_w.rds')
      df = processed_data_long_function(BOS_processed_data_w_r())
      saveRDS(df,'temp/BOS_data_l.rds')
      colnames(df)
      df
      
    })
    

    
    output$KM_cluster_RAS = renderPlot({
      df_w = BOS_processed_data_w_r()
      p = KM_cluster_function(df_w,'RAS_50','RAS','Months','',input)
      print(p)
    })
    output$KM_cluster_BOS1_RAS = renderPlot({
      df_w = BOS_processed_data_w_r()
      p = KM_cluster_function(df_w,'BOS1_RAS_50','BOS1','Months','',input)
      print(p)
    })
    output$KM_cluster_BOS2_RAS = renderPlot({
      df_w = BOS_processed_data_w_r()
      p = KM_cluster_function(df_w,'BOS2_RAS_50','BOS2','Months','',input)
      print(p)
    })
    output$KM_cluster_BOS3_RAS = renderPlot({
      df_w = BOS_processed_data_w_r()
      p = KM_cluster_function(df_w,'BOS3_RAS_50','BOS3','Months','',input)
      print(p)
    })
    output$KM_cluster_Survival = renderPlot({
      df_w = BOS_processed_data_w_r()
      p = KM_cluster_function(df_w,"MonthsToDeath",'Survival','Months','',input)
      print(p)
    })
    
    
    output$KM_cluster_RAS_text = renderPrint({
      df_w = BOS_processed_data_w_r()
      a = KM_cluster_diff_function(df_w,'RAS_50',input)
      print(a)
    })
    output$KM_cluster_BOS1_RAS_text = renderPrint({
      df_w = BOS_processed_data_w_r()
      a = KM_cluster_diff_function(df_w,'BOS1_RAS_50',input)
      print(a)
    })
    output$KM_cluster_BOS2_RAS_text = renderPrint({
      df_w = BOS_processed_data_w_r()
      a = KM_cluster_diff_function(df_w,'BOS2_RAS_50',input)
      print(a)
    })
    output$KM_cluster_BOS3_RAS_text = renderPrint({
      df_w = BOS_processed_data_w_r()
      a = KM_cluster_diff_function(df_w,'BOS3_RAS_50',input)
      print(a)
    })
    output$KM_cluster_Survival_text = renderPrint({
      df_w = BOS_processed_data_w_r()
      a = KM_cluster_diff_function(df_w,"MonthsToDeath",input)
      print(a)
    })
    
      
    
    
    
    
    #calculate BOS columns on the fly
    output$BOS_data_recalc_table = renderDataTable({
      BOS_columns = c('RAS','BOS1_RAS','BOS2_RAS','BOS3_RAS')
      
      BOS_processed_data_w_r()[,BOS_columns]
      })
    
    
    
    
  output$name_clusters_ui = renderUI({
    if(input$rename_clusters == T){
      cluster_name_list_full = c(cluster_name_list, sapply(length(cluster_name_list):input$clutree_num, function(i) paste('Cluster', i)))
      lapply(1:input$clutree_num, function(i) { 
        if(input$rename_menu_test == 'menu'){
          column(4,selectInput(paste0('cluster_', i), paste('Cluster', i),
                    choices = cluster_name_list_full,selected = cluster_name_list_full[i]))
        }else{
          column(4,textInput(paste0('cluster_', i), paste('Cluster', i),
                             value = cluster_name_list_full[i]))
        }
      })
    }

  })

  custom_theme = reactive({
    theme_replace(axis.title.x = element_text(size=input$axis_title_x,face = 'bold', margin = margin(t = input$x_title_margin_t, b = input$x_title_margin_b)))
    theme_replace(axis.title.y = element_text(size=input$axis_title_y,angle = 90,face = "bold", margin = margin(r = input$y_title_margin_r, l = input$y_title_margin_l)))
    theme_replace(axis.text.x = element_text(size=input$axis_text_x, angle = input$axis_text_angle_x))
    theme_replace(axis.text.y = element_text(size = input$axis_text_y, angle = input$axis_text_angle_y))
    theme_replace(plot.title = element_text(size = input$plot_title_size,hjust = input$plot_title_hjust,face = 'bold',margin = margin(b= input$plot_title_margin_b, t = input$plot_title_margin_t)))
    theme_replace(legend.title = element_text(size = input$legend_title_size, face = 'bold'))
    theme_replace(legend.text=element_text(size=input$legend_text_size))
    theme_replace(plot.margin = margin(t=20))
    #theme_replace(plot.margin = margin(t=20, r = 20, b=40, l=20))
    
    th = theme_get()
    th
  })
  
  output$custom_theme = renderPrint(custom_theme())
  
  

  #})
})


