library(shiny)
shinyUI(fluidPage(
  titlePanel("Retrospective Review of AMR Diagnosis and Outcomes"),
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  fluidRow(
  
   
    column(6,uiOutput('live_ui')),
    
    column(6,textOutput('default_file_name')),
    
    #column(4,selectInput('global_factor','Factor to Separate by',c(all_discrete_columns,'cluster'),multiple = F,selected = 'cluster')),
    column(12,
    column(4,uiOutput('global_factor_ui')),
    #column(4,sliderInput('pre_range','Pre Treatment Range',min = -24,max=0,step = 1,value = pre_values, width = 800)),
    #column(4,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = post_values,width = 800)),
    column(4,uiOutput('pre_range_ui')),
    column(4,uiOutput('post_range_ui')),

    
    tabsetPanel(id = 'Main', selected = default_tab,
 
    #### DATA TABLES ####
      #uiOutput('data_table_ui'),
      ####_Defaults ###
      tabPanel('Defaults',
               column(6,
                      #shinyFilesButton('file_2', 'Load Default', 'Please select a dataset', FALSE),
                      fileInput("file1", "Load Default File")
               ),
               column(6,downloadButton('downloadData', 'Save Default File')),
               column(12,dataTableOutput('d_list_table'))
                      #verbatimTextOutput('r_values_text')
                      
                      #verbatimTextOutput('d_list_text'),
               
                      #verbatimTextOutput('d_list_new_text'))
               
               
               ),
      tabPanel("Data Tables",
        uiOutput('data_table_ui')     
        # tabsetPanel(
        #   tabPanel('Original',
        #            tags$h5('Original Data downloaded from googlesheet'),
        #            dataTableOutput('clustering')),
        #   tabPanel('Defaults',dataTableOutput('gs_defaults')),
        #   tabPanel('Processed',
        #            tags$h5('Data after processing into R data types'),
        #            dataTableOutput('full_num')),
        #   tabPanel('Term Mapping',
        #            tags$h5('Mapping of discrete character factors to discrete numeric factors'),
        #            tableOutput('term_mapping')),
        #   
        #   
        #   # tabPanel('pFEV',dataTableOutput('pFEV_wf')),
        #   # tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf')),
        #   # tabPanel('Smoothed',dataTableOutput('i_pFEV_sm_lf')),
        #   # #i_pFEV_sm_lf_r
        #   # #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
        #   # tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f")),
        #   # tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f")),
        #   
        #   tabPanel('Selected Data',
        #            tags$h5('Data after generation ratio and percentage calculations as well as clustering'),
        #            tags$h6('D1 : first differential of imputed smoothed data'),
        #           tags$h6('log2zero: log2(treatment/-x) or log2(x/treatment)'), 
        #           tags$h6('per2zero : percentage change to or from treatment'), 
        #           tags$h6('log2 : log(x/-x)'),
        #           tags$h6('per : precentage change from -x to x'),
        #           tags$h6('per_rel : change between percentage change before treamtment and percentage change after treatment'),
        #            dataTableOutput('pFEV_wf_r')),
        #   #tabPanel('pFEV_l',dataTableOutput('pFEV_lf_r')),
        #   
        #   #tabPanel('Change Data', dataTableOutput('change_data')),
        #   #tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf_r')),
        #   #tabPanel('Smoothed',dataTableOutput('i_pFEV_sm_lf_r')),
        #   #i_pFEV_sm_lf_r
        #   #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
        #   #tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r")),
        #   #tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f_r")),
        #   tabPanel('cluster',
        #            tags$h5('Data used the generate the clusters'),
        #            dataTableOutput("cluster_data")),
        #   #tabPanel('cluster_d1',dataTableOutput("cluster_data_d1"))
        #   
        #   #tabPanel('lm',dataTableOutput("df_lm_table")),
        #   #tabPanel('lm imputed',dataTableOutput("df_lm_table_i"))
        #   #tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r"))
        #   #i_pFEV_sm_d1_f
        # 
        #   tabPanel('log 2 ratio vs zero',
        #            dataTableOutput("pFEV_ratio2zero")),
        #   tabPanel('precentage change vs zero',
        #            dataTableOutput("pFEV_per2zero")),
        #   tabPanel('Summary Table',
        #            sliderInput('summary_slider','Select Month',min = -24,max=24,step = 1,value = c(-6,6), width = 800),
        #            dataTableOutput('summary_table'))
        # )
      
    ),

    #### PATIENT DATA ####
    
    tabPanel('Patients',
        
             
        textOutput('patients_text'),
        

        #textOutput('default_file_name'),
        #tabPanel('Pre Clustering Selection',
             #uiOutput('scale_slide_1'),
             
             tabsetPanel(id = "Patients", selected = 'Select Patients for Clustering',
                ####_missingness####
               tabPanel('Missingness Plot',
                        tags$h5('Missing values in the pFEV data'),
                        plotOutput('missmap_plot'),
                        plotOutput('pFEV_na_hist')),
               
                ####_selection####
               tabPanel('Select Patients for Clustering',
                  uiOutput('duplicate_remove_text_ui'),
                  #tags$h4(paste(length(duplicated_list), 'entries for individual patients, had duplicated pFEV values and were automatically removed')),
                  #HTML("<br>"),
                  #tags$h4(paste(length(excluded_patients_c), 'patients with less than ',completeness,'% of the pFEV datapoints were automatically removed from the analysis')),
                  #textOutput('auto_removed_patients'),
                  #HTML("<br><br>"),
                  #selectInput('select_remove','remove',list(`< 20% of pFEV values` = 'completeness' ,duplicates = 'duplicates'),multiple = T, selected = c('completeness','duplicates')),
                  
          
                  tags$h5('Select the column used to subset the data, then select the the categories within that column to retain'),
                  column(4,uiOutput('subset_1_ui')),
                  #column(4,selectInput('subset_1','Subset by 1',c('All',all_discrete_columns),multiple = F,selected = subset_1)),
                  column(4,uiOutput('out_select_factor_1')),
                  column(4,uiOutput('subset_1_re_include_ui')),
                  #column(4,verbatimTextOutput('out_select_factor_1_options_text')),
                  column(12),
                  column(4,uiOutput('subset_2_ui')),
                  #column(4,selectInput('subset_2','Subset by 2',c('All',all_discrete_columns),multiple = F,selected = subset_2)),
                  column(4,uiOutput('out_select_factor_2')),
                  column(4,uiOutput('subset_2_re_include_ui')),
                  
                  #column(4,verbatimTextOutput('out_select_factor_2_options_text')),
                  column(12),
                  column(4,uiOutput('subset_3_ui')),
                  #column(4,selectInput('subset_3','Subset by 3',c('All',all_discrete_columns),multiple = F,selected = subset_3)),
                  column(4,uiOutput('out_select_factor_3')),
                  column(4,uiOutput('subset_3_re_include_ui')),
                  
                  #column(4,verbatimTextOutput('out_select_factor_3_options_text')),
                  
                  column(9,sliderInput('pre_death_cutoff','Exclude Patients that no longer have pFEV measures after timepoint ',min = -24,max=24,step = 1,value = -24,width = 800)),
                  column(3,uiOutput('dead_re_include_ui')),
                  
                  column(9,numericInput('missing_pFEV','Minimum percentage of pFEV values',2)),
                  column(3,uiOutput('missing_re_include_ui')),
                  uiOutput('pre_remove_ui'),
                  #actionButton('pre_save','Save List'),
                  
                  plotOutput('pre_hist',height = 200),
                  
                  tags$h5(textOutput('status_text')),
                  column(12,radioButtons("status_radio", 'Status',choiceNames = list('Alive',"Dead",'All'),
                                         choiceValues = list("1", "2","0"
                                         ),inline = T,selected = '0')),
                  column(12,
                         tags$h5('Retained patient information'),
                         textOutput('pre_num_patients')),
                  column(12,
                         textOutput('pre_patients_text')),
               
                  #uiOutput('scale_slide_1')
                  #column(6,selectInput('subset_list_0','Select',factor_list,multiple = T,selected = factor_list))
                  
                  #column(3,selectInput('subset_2','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
                  #column(3,selectInput('subset_3','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All'))
                  
                  #tabsetPanel(
                  # CUSTOMISE PLOTS ####
                  textOutput("default_file_name_2"),
                  tabsetPanel(tabPanel('Customize Plots',
                           column(12,radioButtons('mtc','Multiple Testing Correction', choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),selected = 'BH', inline = T)),
                           
                           column(6,numericInput('ggsave_height','Save Height','9')),
                           column(6,numericInput('ggsave_width','Save_Width Size','12')),
                           
                           column(3,numericInput('plot_title_size','Plot Title Size','20')),
                           column(3,numericInput('plot_title_hjust','hjust','0.5')),
                           column(3,numericInput('plot_title_margin_t','top margin','5')),
                           column(3,numericInput('plot_title_margin_b','bottom margin','10')),
                           
                           #column(4,numericInput('plot_title_vjust','Plot Title vjust','4')),
                           column(6,numericInput('legend_title_size','Legend Title Size','15')),
                           column(6,numericInput('legend_text_size','Legend Text Size','12')),
                           
                           column(4,numericInput('axis_title_x','X axis title size','15')),
                           column(4,numericInput('x_title_margin_t','title top margin','5')),
                           column(4,numericInput('x_title_margin_b','title bottom margin','10')),
                           column(6,numericInput('axis_text_x','X axis text size','12')),
                           column(6,numericInput('axis_text_angle_x','X axis text angle','0')),
                           
                           column(4,numericInput('axis_title_y','Y axis title size','15')),
                           column(4,numericInput('y_title_margin_l','title left margin','5')),
                           column(4,numericInput('y_title_margin_r','title right margin','10')),
                           column(6,numericInput('axis_text_y','Y axis text size','12')),
                           column(6,numericInput('axis_text_angle_y','Y axis text angle','90')),
                           column(12,
                           
                                  actionButton('show_tabs_button','Show All Tabs'),
                                  actionButton('hide_tabs_button','Hide Tabs'))
                  
                           #column(12,verbatimTextOutput('custom_theme'))
                           
                  )
              )),
              ### _Post Selection ####
              tabPanel('Post Clustering Selection',
                       sliderInput('post_death_cutoff','Exclude Patients that no longer have pFEV measures at ... ',min = -24,max=24,step = 1,value = -24,width = 800),
                       
                       uiOutput('post_select_ui'),
                       #actionButton('post_save','Save List'),
                       
                       
                       plotOutput('post_hist',height = 200),
                       
                       #textOutput('post_retained_text'),
                       
                      textOutput('post_num_patients'),
              
                      textOutput('post_patients_text')
                       #tags$h5(paste('Patients with less than ',completeness,'% of the pFEV datapoints were automatically removed from the analysis')),
                       #textOutput('auto_removed_patients'),
                       #selectInput('post_remove_list','Select additional patients to removed',patient_list,multiple = T,selected = unique(c(excluded_patients_c,patient_custom_exclude)), width = 800),
                       #tags$h5(textOutput('status_text')),
                       #column(12,radioButtons("status_radio", 'Status',choiceNames = list('Alive',"Dead",'All'),
                      #                        choiceValues = list("1", "2","0"
                      #                        ),inline = T,selected = '0')),
                      # tags$h5('Select the column used to to subset the data, then select the the categories within that column to retain'),
                       
                      # column(6,selectInput('subset_1','Subset by',c('All',all_discrete_columns),multiple = F,selected = subset_1)),
                      # column(6,uiOutput('out_select_factor_1')),
                      # column(12),
                      # column(6,selectInput('subset_2','Subset by',c('All',all_discrete_columns),multiple = F,selected = subset_2)),
                      # column(6,uiOutput('out_select_factor_2')),
                      # column(12),
                      # column(6,selectInput('subset_3','Subset by',c('All',all_discrete_columns),multiple = F,selected = subset_3)),
                      # column(6,uiOutput('out_select_factor_3')),
                       
                       
                      # column(12,
                      #        tags$h5('Retained patient information'),
                      #        textOutput('num_patients')),
                      # column(12,
                      #        textOutput('patients_text'))
                       #uiOutput('scale_slide_1')
                       #column(6,selectInput('subset_list_0','Select',factor_list,multiple = T,selected = factor_list))
                       
                       #column(3,selectInput('subset_2','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
                       #column(3,selectInput('subset_3','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All'))
                       #tags$h5('test')
              ),
              ####_plot####
              #tabPanel('Select Patients',
           
                       
                     
                       #radioButtons('retained_radiobutton','Select Patient Lists', list(All = 'all',`Excluded Pre-clustering` = 'pre',`Excluded Post-clustering` = 'post',`Retained Patients` = 'retained', `BOS data` = 'bos'),selected = 'retained',inline = T),
                       #uiOutput('multi_plot_column_select_ui'),
              ##### _View Individual Patients ######
              tabPanel('View Individual Patients',
                       uiOutput('retained_radionButton_ui'),
                       
                       textOutput('multi_patient_number'),
                       column(9,uiOutput('multi_plot_select_patient_ui')),
                       column(3,radioButtons('multi_subset','',c('subset','all'))),
                       tabsetPanel(
                         
                         tabPanel('Plots',
                                  tags$h5('Red : pFEV1, Green : pFVC, Blue : pRatio, Yellow : original data points, Black : Smoothed Data'),
                                  tags$h5('solid : RAS, dotted : BOS1, dashed : BOS2, dotted-dashed : BOS3'),
                                  
                                  
                                  column(12,uiOutput('individual_patients_ui'))),
                         tabPanel('Table', tabsetPanel(
                           tabPanel('Wide', dataTableOutput('multi_patient_table_wide')),
                           tabPanel('Long', dataTableOutput('multi_patient_table_long'))
                         ))
                       #)
                       
                       
              ))
              
             # tabPanel('Plots',
             #          tabsetPanel(
             #          tabPanel('Individual',
             #                   tags$h5('Red : pFEV1, Green : pFVC, Blue : pRatio, Grey : original data points, Black : Smoothed Data'),
             #                   uiOutput('plot_mrn_select'),
             #                   
             #                   plotOutput('individual_patients'),
             #                   dataTableOutput('individual_patient_table'),
             #                   
             #                   plotOutput('individual_patients_pd'),
             #                   dataTableOutput('individual_patient_table_pd')),
             # 
             #          tabPanel('Duplicated',
             #                  
             #                   selectInput('mrn_select_i_dup','Duplicated MRN',duplicated_patients,multiple = F, selected = duplicated_patients[1], width = 800),
             #                   plotOutput('individual_patients_dup'),
             #                   dataTableOutput('individual_patient_table_dup')
             #                   
             #          ),
             #          
             #          
             # 
             #          
             # 
             #   
             #   
             #   #   #HTML(paste('Plots take some time to render, please wait ...')),
             #   tabPanel('Excluded - pre cluster',
             #            column(12,
             #       HTML(paste('Plots take some time to render, please wait ...')),
             #       tags$h5('Blue : original data points, Red : Imputed Data, Green : Smoothed Data'),
             #       
             #       
             #       dataTableOutput('pre_patients_table'),
             #       uiOutput('excluded_plots'))),
             #   #   
             #   tabPanel('Excluded - post clustering',column(12,
             #        HTML(paste('Plots take some time to render, please wait ...')),
             #        tags$h5('Blue : original data points, Red : Imputed Data, Green : Smoothed Data'),
             #        
             #        
             #        dataTableOutput('post_patients_table'),
             #        uiOutput('excluded_plots_post'))),
             #   
             #   tabPanel('Retained',column(12,
             #                              dataTableOutput('retained_patients_table'),
             #                              uiOutput("plots")))
             #   #tabPanel('Excluded',column(12,uiOutput('excluded_plots')))
             # ))
             )),
    
    #tabPanel('Post Clustering Selection')
    
    
    #### CLUSTERING #####         
    tabPanel('Clustering',
             #tabsetPanel(id = 'Clustering',
            #   tabPanel('Parameters',
                        column(12,textOutput('clustered_patients_text')),
                        column(10,uiOutput('mix_clust_col_fac_ui')),
                        column(2,uiOutput("fac_weight_ui")),
                        column(10,uiOutput('mix_clust_col_fac_2_ui')),
                        column(2,uiOutput("fac_weight_2_ui")),
                        column(6,uiOutput('data_set_ui')),
                        column(6,uiOutput('clust_range_ui')),
            column(10,uiOutput('ccc_1')),
            column(2,uiOutput('num_weight_ui')),
            #numericInput('num_weight', "Weight of Continuous Variable for List 1 *", d_list()$values$c_weight_1, min = 0, max = 100, step = 1),
            
            #selectInput('mix_clust_col_num_2','Continuous Variable List 2',clustering_continuous_columns,multiple = T,,selected = continuous_list_2,width = 600),
            column(10,uiOutput('ccc_2')),
            column(2,uiOutput('num_weight_2_ui')),
            
                        #column(6,
                               #uiOutput('mix_clust_col_fac_ui'),
                               #uiOutput("fac_weight_ui"),
                         #      uiOutput('mix_clust_col_fac_2_ui'),
                        #       uiOutput('fac_weight_2_ui')
                               #selectInput('mix_clust_col_fac','Discrete Factors List 1',discrete_columns_4_comparison,multiple = T,selected = discrete_list_1,width = 600),
                               #numericInput('fac_weight', "Weight of Discrete Factors for List 1", d_weight_1, min = 0, max = 100, step = 1),
                               #selectInput('mix_clust_col_fac_2','Discrete Factors List 2',discrete_columns_4_comparison,multiple = T,selected = discrete_list_2,width = 600),
                               #numericInput('fac_weight_2', "Weight of Discrete Factors for List 2", d_weight_2, min = 0, max = 100, step = 1)
                        
                               
                        #),
                        #column(6,
                               #selectInput('mix_clust_col_num','Continuous Variable List 1',clustering_continuous_columns,multiple = T,selected = continuous_list_1,width = 600),
                               # uiOutput('ccc_1'),
                               # uiOutput('num_weight_ui'),
                               # #numericInput('num_weight', "Weight of Continuous Variable for List 1 *", d_list()$values$c_weight_1, min = 0, max = 100, step = 1),
                               # 
                               # #selectInput('mix_clust_col_num_2','Continuous Variable List 2',clustering_continuous_columns,multiple = T,,selected = continuous_list_2,width = 600),
                               # uiOutput('ccc_2'),
                               # uiOutput('num_weight_2_ui'),
                               
                               #numericInput('num_weight_2', "Weight of Continuous Variable for List 2", c_weight_2, min = 0, max = 100, step = 1),
                               #uiOutput('data_set_ui'),
                               #selectInput('data_set','Datasets used for clustering',gsub('_matrix','',grep('matrix',colnames(processed_data),value = T)),selected = clustering_data_sets,multiple = T),
                               
                               
                               
                        #),
                  
                        
                        #),
                        #tabPanel('Dendogram',
                                #column(12,numericInput('clutree_num', "Number of Clusters", num_clusters, min = 1, max = 50, step = 1)),
                                #column(12,),
           
            
                              column(12,
                              #column(12,tags$hr()),
                              #column(6,radioButtons('rename_clusters','Rename Clusters',c(T,F),inline = TRUE)),
                              
                              #column(6,uiOutput('rename_menu_test_ui')),
                              column(12,tags$hr()),
                              column(3,
                                     uiOutput('clutree_num_ui'),
                                     uiOutput('cluster_names_list_ui'),
                                     uiOutput('add_cluster_name'),
                                     actionButton('add_cluster_button','Click to Add')),
                              
                               
                              column(9,uiOutput('cluster_naming_list')
                                     #uiOutput('name_clusters_ui')
                                     ),
                              
                      
                               column(12,tags$hr()),
                              
                              #column(12,dataTableOutput('cluster_mapping_2'),
                              #       textOutput('cluster_levels_text')),
                              column(12,
                                     #actionButton('run_clustering_button','Re-run Clustering')
                              #       uiOutput('run_clustering_rb_ui')
                                     radioButtons('run_clustering_rb','Run Clustering',c(F,T),selected = T,inline = T),
                                    textOutput('run_cluster_text_ui')
                                     
                              ),
                              tabsetPanel(id = 'dendo', 
                                          
                              #tabPanel('Heatmap',
                              #         plotOutput('mix_clu_1')
                              #         ),
                              tabPanel('Dendogram',
                                                                 
                               column(12,
                                      
                                      column(6,textInput('discrete_cluster_title','Title','Patient Dendogram')),
                                      column(3,textInput('discrete_cluster_x','x title','Patients')),
                                      column(3,textInput('discrete_cluster_y','y title','Distance')),
                                      column(9),
                                      column(3,selectInput('patient_labels','Patient_label',c('mapped','simple','none'),selected = 'mapped')),
                                    
                                      column(11,plotOutput('discrete_cluster_plot')),
                                      column(1,downloadButton('discrete_cluster_plot_download','')),
                                      
                                      
                                      
                                      column(12,tags$hr()),
                                      column(11,plotOutput('mix_clu')),
                                      column(1,downloadButton('mix_clu_download','')),
                                      column(12,tags$hr()),
                                      
                                      column(6,textInput('distance_density_title','Title','Distance Density Plot')),
                                      column(3,textInput('distance_density_x','x title','x')),
                                      column(3,textInput('distance_density_y','y title','y')),
                                  
                                      column(11,plotOutput('distance_density')),
                                      column(1,downloadButton('distance_density_download',''))
                                      #column(12,tags$hr())
                                      #radioButtons('view_summary_stats_rb','View Summary Stats',c(F,T),selected = F,inline = T),
                                      #textOutput('summary_stat_show')
                                      #)
                              
                              )),
                              #tabsetPanel(id = 'clust_stat',
                                  tabPanel('Summary Statistics',
                              #tabPanel('Stats',
                                      radioButtons("data_select_clust", 'Select Data applied to plots below',
                                                   choiceNames = list('pFEV',"imputed", 'imputed to last pFEV value', 'smoothed','D1', "D1 remove imputed", 'D2'),
                                                   choiceValues = list("pFEV", "imputed",'imputed_NA', 'smoothed', 'd1','d1_ri','d2'),inline = T,selected = 'd1'),
                                      
                                      tags$h5('log2 ratio t test'),
                                      plotOutput('boxplot_pp_ratio_cluster'),
                                      
                                      tags$h5('Full Range'),
                                      plotOutput('boxplot_pFEV_cluster_full'),
                                      column(12,tags$hr()),
                                      column(12,tags$h5(textOutput('manova_clustering_text'))),
                                      column(11,dataTableOutput('selected_manova_table_cluster')),
                                      column(1,downloadButton('selected_manova_table_cluster_download','')),
                                      
                                      column(12,tags$hr()),
                                      column(12,tags$h5('ANOVA of continuous variables across clusters')),
                                      column(11,dataTableOutput('continuous_manova_cluster')),
                                      column(1,downloadButton('continuous_manova_cluster_download',''))
                               #)   
                              
                               # column(12)
                               
                        #) # Dendogram
               #### _chi-squared ####
             ))
    )), #Clustering
    
    ##### ANALYSIS ######
    tabPanel('Analysis',
             column(12,
                    uiOutput("cluster_select_clusters")),
    tabsetPanel(id = 'Analysis', selected = '',
               tabPanel('Discrete Variables',
                        tabsetPanel(id = "Discrete_Variables", selected = 'Cluster',
                          tabPanel('Factor',
                                   tags$h5('Factor proportions across clusters by factor'),
                                
                                   tabsetPanel(
                                     tabPanel('Proportions',
                                              
                                              tags$h6('Chi-squared used to determines if the proportions of a single factor criteria is randomly distributed between clusters. Proportions of each row sums to 100. Random would equate to equal propotions across clusters. Chi-squared tests if the deviation from random is significant.'),
                                              tabsetPanel(
                                                tabPanel('Selected',dataTableOutput('cluster_analyis_selected_table')),
                                                tabPanel('Full',
                                                         column(11,dataTableOutput('cluster_analysis')),
                                                         column(1,downloadButton('cluster_analysis_download',''))
                                                         
                                                         ))),
                                     tabPanel('Matrix',
                                              tags$h6('Chi-squared used to determines if the matrix of proportions between factors within a variable across clusters are significantly different from each other.'),
                                              
                                              tabsetPanel(
                                                tabPanel('Selected',dataTableOutput('chisq_cluster')),
                                                tabPanel('Full',
                                                                  
                                                    column(11,dataTableOutput('chisq_cluster_full')),
                                                    column(1,downloadButton('chisq_cluster_full_download',''))
                                                       
                                                )))
                                   )),
                          tabPanel('Cluster',
                                   column(9,tags$h5('Factor proportions within clusters')),
                                   column(3,radioButtons('rm_na_1',"Remove NA's",c(T,F),inline = T)),
                                   
                                   tabsetPanel(
                                     tabPanel('Proportions',
                                              tags$h6('Illustrates the proportions of factor criteria within a cluster, Proportions within a cluster column for each factor sum to 100.'),
                                              #tags$div(HTML(paste(tags$span(style="color:red", 'Not sure the p values here are valid, the proportions are calculated for each factor within a cluster (down). These p values can be see in Stat -> Proportions. Here the p values are calculated across the clusters')))),
                                              tabsetPanel(
                                                tabPanel('Selected',
                                                         dataTableOutput('cluster_analysis_within_table_selected_table')),
                                                tabPanel('Full',
                                                         column(11,dataTableOutput('cluster_analysis_within_table')),
                                                         column(1,downloadButton('cluster_analysis_within_table_download',''))
                                                         
                                                         )
                                              )),
                                     tabPanel("Chi-square Statistic details",
                                              tabsetPanel(
                                              tabPanel('Proportions',
                                                       tabsetPanel(
                                                         tabPanel('Selected', dataTableOutput('cluster_analysis_within_p_table_selected_table')),
                                                         tabPanel('Full',
                                                            column(11,dataTableOutput('cluster_analysis_within_p_table')),
                                                            column(1,downloadButton('cluster_analysis_within_p_download'))
                                                         )
                                                       )),
                                              tabPanel('Matrix',
                                                tags$h6('Chi-squared used to determines if the matrix of factor criteria proportions between clusters are signficantly different from each factor. The clusters can be selected to determine if there are specific differences between clusters.'),
                                                
                                                tabsetPanel(
                                                  tabPanel('Selected',dataTableOutput('chisq_cluster_within')),
                                                  tabPanel('Full',
                                                           column(11,dataTableOutput('chisq_cluster_within_full')),
                                                           column(1,downloadButton('chisq_cluster_within_full_download',''))
                                                           
                                                           )
                                                ))
                                              ))
                                   ))
                        )),# chi-squared
               
               # _Continuous Variable #####
               tabPanel('Continuous Variables', 
                 # __Mean Table ##############
                 #tabPanel('Mean Table',
                       h1("Mean Summary Table"),   
                          dataTableOutput('pFEV_mean_table'),
                 #),
                 # __ANOVA ###################
                 tabsetPanel(
                 
                 tabPanel('ANOVA statistic details',
                          #column(6,selectInput('continuous_variable','Select Continuous Variable',clustering_continuous_columns,multiple = F)),
                          
                          #column(12,uiOutput("cluster_select_clusters_anova")),
                          tabsetPanel(
                            tabPanel('Individual Plots',
                                     selectInput('continuous_variable','Select Continuous Variable',clustering_continuous_columns,multiple = F),
                                     
                                     plotOutput('anova_cluster_plot'),
                                     dataTableOutput('continuous_manova_single')
                            ),
                            tabPanel('Full Statistic Details',dataTableOutput('continuous_manova_full'))
                          )
                 )
               )

             ), 
    
    
  ##### POST CLUSTERING ANALSYSI ######
    tabPanel('Spirometry Patterns',
             radioButtons('data_source','Select Data Type',c('pFEV1','pFVC','pRatio'),inline = T),
             uiOutput('data_select_ui'),
  
             
             #column(3,
            #        radioButtons("radioSelection", label = "So many options!", choices = c("A", "B", "C"))
             #),
            # radioTooltip(id = "radioSelection", choice = "A", title = "Button 1 Explanation", placement = "right", trigger = "hover"),
            # radioTooltip(id = "radioSelection", choice = "B", title = "Button 2 Explanation", placement = "right", trigger = "hover"),
            # radioTooltip(id = "radioSelection", choice = "C", title = "Button 3 Explanation", placement = "right", trigger = "hover"),
             
             
             
             radioButtons('calc_select','Select Calculations',
                          choiceNames = list('none','log2zero','per2zero','ratio(post/pre)','percentage(post/pre)','log2(ratio)','relative percentage','relative log2(ratio)'),
                          choiceValues = list('none','log2zero','per2zero','ratio','per','log','per_rel','ratio_rel'),
                          inline = T),
                radioTooltip(id = "calc_select", choice = "none", title = "no manipulation of data", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "log2zero", title = "log2(treatment (zero) timepoint / pre treatment timepoint)     --or--    log2(post treatment timepoint / treatment (zero) timepoint) ", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "per2zero", title = "(treatment (zero) timepoint - pre treatment timepoint)/pre treatment timepoint * 100   --or--    (post treatment timepoint - treatment (zero) timepoint) / treatment (zero) timepoint * 100 ", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "ratio", title = "post treatment timepoint / pre treatment timepoint", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "log", title = "log2(post treatment timepoint / pre treatment timepoint)", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "per", title = "(post treatment timepoint / pre treatment timepoint) / pre treatment timepoint * 100", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "per_rel", title = "(per post timepoint / per pre timepoint) / abs(per pre timepoint) * 100", placement = "right", trigger = "hover"),
                radioTooltip(id = "calc_select", choice = "ratio_rel", title = "pre log2zero - post log2zero", placement = "right", trigger = "hover"),
            
            #textOutput('statistice_tab_text'),
             tabsetPanel(id = 'Spirometry', selected = 'Statistics',
                         
    ##### _PLOTS####                     
    tabPanel('Data Matrix', dataTableOutput('data_matrix')),
    tabPanel('Plots',
             tabsetPanel(
               tabPanel('Line Plot',
                        
                        column(12,tags$hr()),
                        column(6,uiOutput('line_pFEV_title_ui')),
                        column(3,textInput('line_pFEV_x','x title','months')),
                        column(3,uiOutput('line_pFEV_y_ui')),
                        
                        column(11,plotOutput('line_pFEV')),
                        column(1,downloadButton('line_pFEV_download','')),
            
                        
                        column(12,tags$hr()),
                        column(12,h4('Plot does not display if there are too few datapoints.')),
                        column(6,uiOutput('smooth_line_pFEV_title_ui')),
                        column(3,textInput('smooth_line_pFEV_x','x title','months')),
                        column(3,uiOutput('smooth_line_pFEV_y_ui')),
                        
                        column(11,plotOutput('smooth_line_pFEV')),
                        column(1,downloadButton('smooth_line_pFEV_download','')),
                        column(12,tags$hr())
                        
                        
                        
               ),
               tabPanel('Boxplot',
                        #column(12,
                        
                        column(6,uiOutput('boxplot_pFEV_title_ui')),
                        column(3,textInput('boxplot_pFEV_x','x title','months')),
                        column(3,uiOutput('boxplot_pFEV_y_ui')),
                        
                        #column(6,textInput('boxplot_pFEV_title','Title',paste(select_matrix(), 'boxplot'))),
                        #column(3,textInput('boxplot_pFEV_x','x title','months')),
                        #column(3,textInput('boxplot_pFEV_y','y title',gsub('_matrix','',select_matrix()))),

                        column(11,plotOutput('boxplot_pFEV')),
                        column(1,downloadButton('boxplot_pFEV_download','')),
                        column(12,tags$hr()),


                        column(6,uiOutput('boxplot_pFEV_mean_title_ui')),
                        column(3,textInput('boxplot_pFEV_mean_x','x title','months')),
                        column(3,uiOutput('boxplot_pFEV_mean_y_ui')),

                        #column(6,textInput('boxplot_pFEV_mean_title','Title',paste(select_matrix(), 'boxplot mean'))),
                        #column(3,textInput('boxplot_pFEV_mean_x','x title','months')),
                        #column(3,textInput('boxplot_pFEV_mean_y','y title',gsub('_matrix','',select_matrix()))),
                        
                        
                        column(11,plotOutput('boxplot_pFEV_mean')),
                        column(1,downloadButton('boxplot_pFEV_mean_download','')),
                        column(12,tags$hr())


               ),
               tabPanel('Histogram',
                        column(8,uiOutput('hist3D_cluster_select')),
                        column(2,radioButtons('hist3D_rb','',c('2D','3D'))),
                        
                        column(2,numericInput('hist3D_breaks','Breaks',10,step = 1)),
                        column(12,plotOutput('hist_3D')),
                                    
                        column(6,uiOutput('histogrma_time_ui')),
                        column(6,radioButtons('hist_all','Add All',c(T,F), inline = T)),
                        column(12,plotOutput('plot_FEV_histogram'),
                        plotOutput('plot_FEV_histogram_facets'),
                        plotOutput('plot_FEV_density'))
               )
               )),
             
             
             #uiOutput('line_pFEV_ui')),

              # column(12,
              #        tabsetPanel(
              #          tabPanel('plot',uiOutput('line_pFEV_ui')),

                    ######## _LINE PLOTS ###########
                    

                    
                    # tabPanel('Line Plot',
                    #          
                    #          column(12,tags$hr()),
                    #          
                    #          #column(6,textInput('line_pFEV_title','Title','Distance Density Plot')),
                    #          #column(3,textInput('line_pFEV_x','x title','x')),
                    #          #column(3,textInput('line_pFEV_y','y title','y')),
                    #          uiOutput('line_pFEV_ui'),
                    #          column(11,plotOutput('line_pFEV')),
                    #          column(1,downloadButton('line_pFEV_download','')),
                    #          column(12,tags$hr()),
                    #          
                    #          column(12,plotOutput('smooth_line_pFEV'))
                    #          
                    #          ),
                    ######## _BOXPLOTS ##########
                    # tabPanel('Boxplot',
                    #          column(12,
                    #                 plotOutput('boxplot_pFEV'),
                    #                 plotOutput('boxplot_pFEV_mean'))
                    #          
                    #          ),
                    # tabPanel('Interaction',
                    #          HTML(paste("The primary separation factor is separated into different plots. The Interactors Selector box is used to choose additional factors. Mean line plots are then generated illustrating the how these factors separate the data within the main factor.")),
                    #          column(3,selectInput('interactors','Ineteractors',c(full_factor_columns,'cluster','cluster_d1'),multiple = T,selected = 'Status')),
                    #          
                    #          plotOutput('interaction_plot')
                    #          )))),
                    

      ############## STAT ################
                    tabPanel('Statistics',
                             #tabsetPanel(tabPanel('Lung Measurements',
                             column(12,

                                  tabsetPanel(id = 'Statistics',selected = 'Pre Treatment vs Post Treatment',
                                    ### _pre vs post ####
                                    tabPanel('Pre Treatment vs Post Treatment',
                                             tags$h5('Statistical assesment of values between pre treatment ranges and post treatment ranges. The ranges are adjusted using the range slides.'),
                                             #HTML(paste(input$pre_range[1])),
                                             tabsetPanel(selected = 'T test',
                                                  tabsetPanel(id = 'PTP',
                   
                                          

                                          
                                                      tabPanel('Pre vs Treatment vs Post',
                                                              tags$h5('T test calculated on values from a pre-treatment or post-treatment timepoint vs the treatment timepoint'),
                                                              HTML(paste(textOutput('t_pFEV_zero_text'))),
                                                               
                                                               tabsetPanel(
                                                                   tabPanel('Selected',
                                                                            column(12,tags$hr()),
                                                                            column(6,uiOutput('boxplot_pp_zero_title_ui')),
                                                                            column(3,uiOutput('boxplot_pp_zero_x_ui')),
                                                                            column(3,uiOutput('boxplot_pp_zero_y_ui')),
                                                                            #uiOutput('boxplot_pp_zero_x_ui'),
                                                                            #column(3,textInput('line_pFEV_x','x title','months')),
                                                                            #column(3,textInput('line_pFEV_y','y title',gsub('_matrix','',select_matrix()))),
                                                                            
                                                                            column(11,plotOutput('boxplot_pp_zero')),
                                                                            column(1,downloadButton('boxplot_pp_zero_download','')),
                                                                            column(12,tags$hr()),
                                                                          
                                                                          #plotOutput('boxplot_pp_zero'),
                                                                          dataTableOutput('pp_t_table_zero')
                                                                   ),
                                                                   tabPanel('Full',
                                                                            column(11,dataTableOutput("pp_t_test_zero_full")),
                                                                            column(1,downloadButton('pp_t_test_zero_full_download',''))
                                                                            
                                                                   )
                                                               )
                                                      ),
                                                      tabPanel('Post / Treatment vs Treatment / Pre',
                                                               tags$h5('T test calculated on ratios or percentages of  (treatment / pre-treatment )  vs (post-treatment timepoint / treatment timepoint)'),
                                                               #radioButtons('vs_zero_prefix', label = 'T test calculated on ratios or percentages of  (treatment timepoint / pre-treatment timepoint)  vs (post-treatment timepoint / treatment timepoint)', choiceNames = c('log2(ratio)','Percentage'), choiceValues = c('log2zero_','per2zero_'),inline = T),
                                                               tabsetPanel(
                                    
                                                                 tabPanel('Selected',
                                                                          plotOutput('percentage_boxplot'),
                                                                          dataTableOutput('percentage_table')
                                                                          ),
                                                                 #tabPanel('Data', dataTableOutput('percentage_df')),
                                                                 tabPanel('Full', dataTableOutput('percentage_change_t_test_full'))
                                                               )
                                                               ),
                                                        tabPanel('Pre vs Post',
                                                                 tags$h5('Pre-treatment timepoint vs Post-treatment Timepoint'),
                                                                 tabsetPanel(
                                                                       tabPanel('Timepoint Range',
                                                                                tags$h5('T test are calculated on a range of timepoints before treatment vs a range of timepoints post treatment. '),
                                                                                HTML(paste(textOutput('t_range_text'))),
                                                                                
                                                                                tabsetPanel(
                                                                                  tabPanel('Selected',
                                                                                           column(12,tags$hr()),
                                                                                           column(6,uiOutput('boxplot_pp_ranges_title_ui')),
                                                                                           column(3,uiOutput('boxplot_pp_ranges_x_ui')),
                                                                                           column(3,uiOutput('boxplot_pp_ranges_y_ui')),
                              
                                                                                           column(11,plotOutput('boxplot_pp_ranges')),
                                                                                           column(1,downloadButton('boxplot_pp_ranges_download','')),
                                                                                           column(12,tags$hr()),
                                                                                           
                                                                                         
                                                                                           dataTableOutput('pp_t_table_ranges')
                                                                                  ),
                                                                                  tabPanel('Full',
                                                                                           column(11,dataTableOutput('pp_t_test_ranges_full')),
                                                                                           column(1,downloadButton('pp_t_test_ranges_full_download','')))
                                                                                  
                                                                                )
                                                                                
                                                                       ),
                                                                       tabPanel('Slope',
                                                                                tags$h5('T test calculated on slopes pre treatment vs slopes post treatment. The slopes are calculated from a linear regression of all points between two timepoints.'),
                                                                                tags$h6("lm(time_range ~ pFEV)"),
                                                                                HTML(paste0(textOutput('slope_pFEV_text'))),
                                                                                tabsetPanel(
                                                                                  tabPanel('Selected',
                                                                                           column(6,plotOutput('slope_fit_pFEV_pre_plot')),
                                                                                           column(6,plotOutput('slope_fit_pFEV_post_plot')),
                                                                                           column(12,
                                                                                                  plotOutput('slope_boxplot'),
                                                                                                  dataTableOutput('slope_table')
                                                                                           )),
                                                                                  tabPanel('Data',
                                                                                           tags$h5('One way anova, intercepts and slopes per patient for the pFEV data'),
                                                                                           dataTableOutput('df_lm_table')),
                                                                                  tabPanel('Full',dataTableOutput('df_slope_full'))
                                                                                )
                                                                          ),
                                                                          tabPanel('Ratios',
                                                                                   radioButtons("col_select", 'Ratio generated by equation described below',
                                                                                                choiceNames = list('Ratio','log2(ratio)', 'Percentage',  'Relative Percentage'),
                                                                                                choiceValues = list("sym_ratio_colnames",'sym_log_ratio_colnames','sym_per_colnames', 'sym_rel_per_colnames'), select ='sym_rel_per_colnames', inline = T),
                                                                                   # 
                                                                                   #radioButtons("col_select_prefix", 'Ratio generated by equation described below',
                                                                                  #              choiceNames = list('Ratio','log2(ratio)', 'Percentage',  'Relative Percentage'),
                                                                                  #              choiceValues = list("ratio_",'log2_','per_', 'per_rel_'), select ='log2_', inline = T),
                                                                                  uiOutput('ratio_input_ui'),
                                                                                  #selectInput('ratio_num','timepoints',sym_times_cols,multiple = T,selected = sym_times_cols,width = 600),
                                                                                   
                                                                                   #tags$h4('Ratios generates symmetrically across treatment i.e(-3/3) and then statistics performed on these ratios'),
                                                                                   #tags$h5(textOutput('sym_equation')),
                                                                                   uiOutput('scale_slide'),
                                                                                   
                                                                                   plotOutput('sym_ratio_boxplot'),
                                                                                   
                                                                                   tabsetPanel(
                                
                                                                                         tabPanel('Statistics',
                                      
                                                                                                  #plotOutput('sym_per_boxplot'),
                                                                                                  tabsetPanel(id = 'Ratio_Statistics',
                                                                                                    tabPanel('MANOVA',
                                                                                                             tags$h5('Determines if there is a significant difference between the factors (ie clusters), when log2 ratios are generated between two timepoints across 0. ie(log2(-12/12), MANOVA compares all the ratio together across the time range which can be changed using the post range slider'),
                                                                                                             
                                                                                                             dataTableOutput('manova_sym_table')
                                                                                                    ),
                                                                                                    tabPanel('ANOVA',
                                                                                                             tags$h5('Determines if there is a significant difference between the factors (ie clusters) at a specific timepoints for which log2 ratio were generated. ie(log2(-12/12)'),
                                                                                                             dataTableOutput('anova_pw_sym_ratio')
                                                                                                    ),
                                                                                                    tabPanel('Two Sample t tests',
                                                                                                             dataTableOutput('sym_t_test_table')
                                                                                                             
                                                                                                    ),
                                                                                                    tabPanel('T test vs mean of zero',
                                                                                                             dataTableOutput('sym_t_test_table_0')
                                                                                                             
                                                                                                    )
                                                                                                  )),
                                                                                             tabPanel('Tables',
                                                                                                      tabsetPanel(
                                                                                                        tabPanel('Data',
                                                                                                                 dataTableOutput('raw_sym_data')),
                                                                                                        #tabPanel('Processed Data',
                                                                                                        #tags$h3('log2 ratio of timepoints on either side of treatment, ie log2(-12/12) or log2(-6/6)'),
                                                                                                        #          dataTableOutput('pFEV_ratio_df')
                                                                                                        #),
                                                                                                        tabPanel('Means',
                                                                                                                 #tags$h4('Mean log2(ratio)  (-12/12)'),
                                                                                                                 dataTableOutput("sym_ratio_mean_df")
                                                                                                                 #tags$h4('Mean percentage change (-12 to 12)'),
                                                                                                                 #dataTableOutput("sym_per_mean_df")
                                                                                                        )))
                                                                                     ))
                                                                 ))
                             ))),
                             
                             ### _timeseries #####
                             tabPanel("Time-series comparisons",
                                      tags$h5("Assesses if the time series are significantly different from each other"),
                                      tabsetPanel(
                                          tabPanel('ANOVA',
                                                   tags$h5('Asseses if there is a significant change in the values pre treatment or post treatment.'),
                                                   tabsetPanel(
                                                     tabPanel('Selected',
                                                              
                                                              plotOutput('boxplot_anova_all_factor'),
                                                              column(6,plotOutput('boxplot_anova_before_factor')),
                                                              column(6,plotOutput('boxplot_anova_after_factor')),
                                                              dataTableOutput('lm_table')
                                                     ),
                                                     tabPanel('Full', dataTableOutput('df_lm_full'))
                                                   )
                                            ),
                                      
                                            tabPanel('MANOVA',
                                                     tags$h5('Compares the difference between values at each timepoint for different discrete factors, and determines if the whole timeseries is significantly different between these discrete factors'),
                                                     #tabsetPanel(
                                                       #tabPanel('Original Data',
                                                                tabsetPanel(
                                                                  tabPanel('Selected',
                                                                           plotOutput('boxplot_pFEV_manova'),
                                                                           dataTableOutput('selected_manova_table')
                                                                  ),
                                                                  tabPanel('Full',
                                                                           dataTableOutput('full_manova_table'))
                                                                )
            
                                                     ))),
                                        #### _pre vs post ####       
                                        tabPanel('Pre vs Pre and Post vs Post',
                                                 tags$h5('Vertical comparison, asseses is the pre treatment values are different between different discrete factors. The same is done for the post treatment values. Post is not compared to Pre.'),
                                                 tabsetPanel(
                                                     tabPanel('Selected',
                                                       column(6,plotOutput("hor_box_pre")),
                                                       column(6,plotOutput("hor_box_post")),
                                                       column(12,
                                                        tags$h4('MANOVA'),
                                                       dataTableOutput('horizontal_anova'),
                                                       tags$h4('T test'),
                                                       dataTableOutput('horizontal_t_test'))
                                                     ),
                                                     tabPanel('Full MANOVA',
                                                              dataTableOutput('horizontal_anova_full')),
                                                      tabPanel('Full T test',
                                                              dataTableOutput('horizontal_t_test_full')
    
                                                              )
                                                 ))
                                      ))
                             #))
                             )
                             
                    )),#Stats
                    
                    



    ####### BOSS #######
    tabPanel('BOS', id = 'BOS',
             column(6,radioButtons('ras_new','RAS NEW DEFINITION',c(F,T),selected = T,inline = T)),
             column(6,radioButtons('re_run_bos','Run BOS calculations',c(F,T), inline = T)),
             
             column(12,radioButtons('bos_dataset_select','Select Dataset',list('Full Dataset'= 'full', 'Data used for Clustering' = 'cluster','After post clustring selection' = 'post'),selected = 'cluster', inline = T),
             textOutput('bos_data_length_text')),
             
             tabsetPanel(id = 'BOS_settings', tabPanel('Parameters',
              radioButtons('bos_pFEV1','pFEV1 data',list('pFEV1' = 'pFEV1_matrix','i_pFEV1' = 'i_pFEV1_matrix','sm_i_pFEV1' = "sm_i_pFEV1_matrix"),selected ='i_pFEV1_matrix',inline = T),
              radioButtons('bos_pRatio','pRatio data',list('pRatio' = 'pRatio_matrix','i_pRatio' = 'i_pRatio_matrix','sm_i_pRatio' = "sm_i_pRatio_matrix"),selected ='i_pRatio_matrix',inline = T),
                                                       
             column(3,numericInput('bos1_limit','BOS1 pFEV',0.8)),
             column(3,numericInput('bos2_limit','BOS3 pFEV',0.66)),
             column(3,numericInput('bos3_limit','BOS3 pFEV',0.5)),
             column(3,numericInput('concurrent','Concurrent Measurements',2)),
             column(3,numericInput('ras_upper','pRatio Upper Limit',0.85)),
             column(3,numericInput('ras_lower','pRatio Lower Limit',0.7)),
             column(3,numericInput('fall','pRatio percentage change',-10)),
             column(3,numericInput('history','pRatio change duration (months)',6))
             
             ),
             tabPanel('Description',
                      tags$h4('RAS'),
                      tags$h6("RAS is assigned when pFEV is less than 'BOS1 pFEV' (0.8) but while pRatio is above 'pRatio Lower Limit' (0.7) and only if the 'pRatio percentage change' is less than -10% over the 'pRatio change duration' of 6 months. The change duration means that RAS can only be assigned after 6 months of measurements"),
                      tags$h4('BOS1'),
                      tags$h6("BOS1 is assigned when pFEV is less than 'BOS1 pFEV' (0.8) and pRatio is below 'pRatio upper Limit' (0.85). This only applies if the 'pRatio percentage change' is more than -10%, over the 'pRatio change duration' of 6 months."),
                      tags$h6("If the pRatio is below the 'pRatio Lower Limit' then the percentage change is ignored. Like RAS, BOS1 can only be assigned after 6 months of measurements"),      
                          
                      tags$h4('BOS2 and BOS3'),
                      tags$h6('BOS2 and BOS3 are calculated exactly as BOS1 except that pFEV limits of 0.66 and 0.5 are used respectively')
                      
                      ),
             tabPanel('Old parameters',
                      radioButtons('bos_data_select','Select pFEV Data to use for BOS calculation, pRatio uses imputed',list(original = '', imputed = 'i'),selected = 'i',inline = T)
 
                      )
             ),
             radioButtons('first_and_last','Only do BOS calculation between first and last pFEV1 measurements',c(T,F),selected = F,inline = T),
             radioButtons('measured_columns','Only do BOS calculation where measurements were taked in any patient, impute missing only for these timepoints',c(T,F),selected = F,inline = T),
             radioButtons('sequence_correction','Correct BOS assignments so they are always in sequence',c(T,F),selected = F,inline = T),
             
             
             #radioButtons('bos_dataset_select','Select Dataset',list('Data used for Clustering' = 'cluster','After post clustring selection' = 'post'),'cluster', inline = T),
            tabsetPanel(id = 'bos_scale', tabPanel('Set plot scale',
             column(12,sliderInput('bos_range','Timecourse Range',min = -48,max=48,step = 1,value = c(-24,24),width = 800)),
             column(4,radioButtons('bos_slider','Select Slider Effect',c('plot_lim','plot_scale','none'),inline = T)),
             column(4,radioButtons('bos_slider_data','Limit data using slider',c(T,F),inline = T)),
             column(4,radioButtons('bos_slider_KM','Limit KM data using slider',c(T,F),selected = F, inline = T)),
             column(12)
            )),
            tabsetPanel(id = 'bos_plots', selected = 'Line Plots',
            #tabsetPanel(selected = 'Line Plots',
                                     
               tabPanel('Line Plots',
                        column(6,textInput('BOS_All_title','Title','BOS plot for all patients')),
                        column(3,textInput('BOS_x','x  title','Months')),
                        column(3,textInput('BOS_y','x  title','')),
                        column(12,
                        
                        plotOutput('bos_plots'),
                        
                        textInput('RAS_title','Title','RAS free'),               
                        column(11,plotOutput('RAS_factor_plot')),
                        column(1,downloadButton('RAS_factor_plot_download','')),
                        
                        textInput('BOS1_title','Title','BOS1 free'),               
                        column(11,plotOutput('BOS1_factor_plot')),
                        column(1,downloadButton('BOS1_factor_plot_download','')),
                        
                        textInput('BOS2_title','Title','BOS2 free'),               
                        column(11,plotOutput('BOS2_factor_plot')),
                        column(1,downloadButton('BOS2_factor_plot_download','')),
                        
                        textInput('BOS3_title','Title','BOS3 free'),               
                        column(11,plotOutput('BOS3_factor_plot')),
                        column(1,downloadButton('BOS3_factor_plot_download','')),
                        
                        
                        textInput('Survival_title','Title','Survival free'),               
                        column(11,plotOutput('Survival_factor_plot')),
                        column(1,downloadButton('Survival_factor_plot_download',''))
                        #download
                        
                        #uiOutput('BOS_plot_ui')
                        #plotOutput('RAS_factor_plot'),
                        #plotOutput('bos1_factor_plot'),
                        #plotOutput('bos2_factor_plot'),
                        #plotOutput('bos3_factor_plot'),
                        #plotOutput('bos3_surv_factor_plot')
                        #plotOutput('boss_3_factor_l')
                        )
               ),
               tabPanel('Kaplan-Meier Survival Curves and the Log - Rank Test',
                        plotOutput('KM_cluster_RAS'),
                        verbatimTextOutput('KM_cluster_RAS_text'),
                        plotOutput('KM_cluster_BOS1_RAS'),
                        verbatimTextOutput('KM_cluster_BOS1_RAS_text'),
                        
                        plotOutput('KM_cluster_BOS2_RAS'),
                        verbatimTextOutput('KM_cluster_BOS2_RAS_text'),
                        
                        plotOutput('KM_cluster_BOS3_RAS'),
                        verbatimTextOutput('KM_cluster_BOS3_RAS_text'),
                        
                        plotOutput('KM_cluster_Survival'),
                        verbatimTextOutput('KM_cluster_Survival_text')
                        
                        #plotOutput('KM_RAS_plot'),
                        #plotOutput('KM_RAS_BOS1_plot'),
                        #plotOutput('KM_RAS_BOS2_plot'),
                        #plotOutput('KM_RAS_BOS3_plot'),
                        #plotOutput('KM_RAS_cluster_plot'),
                        #verbatimTextOutput('KM_RAS_cluster_text'),
                        
                        #plotOutput('KM_BO1_RAS_cluster_plot'),
                        #verbatimTextOutput('KM_BO1_RAS_cluster_text'),
                        
                        #plotOutput('KM_BO2_RAS_cluster_plot'),
                        #verbatimTextOutput('KM_BO2_RAS_cluster_text'),
                        
                        #plotOutput('KM_BO3_RAS_cluster_plot'),
                        #verbatimTextOutput('KM_BO3_RAS_cluster_text')
                        
                        ),
               #tabPanel('Patient Plots'),
               tabPanel('Tables', tabsetPanel(
                    tabPanel('Original',
                             
                             dataTableOutput("BOS_data_recalc_table")
                      ),
                    tabPanel('Selected',
                             column(6,selectInput('boss_select','Select',c('BOS1_free','BOS2_free','BOS3_free','BOS3_surv_free'),selected = 'BOS3_surv_free')),
                             column(6,sliderInput('bos_time_select','Time',min = -24,max=48,step = 1,value = 6,width = 800)),
                             dataTableOutput('boss_factor_table_select')
                             
                             ),
                    tabPanel('Full',
                        dataTableOutput('bos_df'),
                        dataTableOutput('boss_factor_table'),
                        dataTableOutput('bos_patient_status')
                        ))),
               tabPanel('Smooth',
                        plotOutput('bos_plots_smooth'),
                        plotOutput('bos1_factor_plot_smooth'),
                        plotOutput('bos2_factor_plot_smooth'),
                        plotOutput('bos3_factor_plot_smooth'),
                        plotOutput('bos3_surv_factor_plot_smooth')
               )
             )
             )
  
  )), # BOSS

          

    ######### R SESSION INFO #########
          tabPanel('R Info',
          tabsetPanel(selected = info_tab,

          tabPanel('Session Info',
                              htmlOutput('citation1')
                              
                              ),
          tabPanel('Sanity Check',
                   tags$h5('Columns in the googlesheet but not in the app, will not be included in the calculations'),
                   verbatimTextOutput('additional_columns'),
                   tags$h5('Columns removed from the googlesheet but assigned in the app, will break the app !'),
                   verbatimTextOutput('missing_columns'),
                   tags$h5('Samples with multiple rows in the data were appended with _a'),
                   verbatimTextOutput('duplicated_samples')
                   #verbatimTextOutput("processed_data_str")
          ),
          
          tabPanel('Testing',
            tabsetPanel(
              tabPanel('Multivariant_Analysis',
                       plotOutput('normality_plot'))
              
            ))
          #          
          #          
          # 
          # 
          #                       tabPanel('text',
          #                                verbatimTextOutput('test_text_1'),
          #                                verbatimTextOutput('test_text_2'),
          #                                verbatimTextOutput('test_text_3')
          #                       ),
          #                       tabPanel('More Stats',
          #                                column(3,selectInput('binary_factor','Binary Factor',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'Status')),
          #                                column(3,selectInput('add_factor','Additional Factors',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'HLAType')),
          #                                column(12,
          #                                tabsetPanel(
          #                                  tabPanel('Discrete Variables',
          #                                           plotOutput('mosaic_xt'),
          #                                           verbatimTextOutput('chisq_xt'),
          #                                           verbatimTextOutput('fishe_xt')
          # 
          # 
          #                                            ),
          #                       tabPanel('Logistic Regression',
          #                                         column(12,
          #                                                plotOutput('lg_scatter'),
          #                                                verbatimTextOutput('logistic_regression_text'),
          #                                                dataTableOutput('logistic_regression_table'),
          #                                                plotOutput('logistic_regression_p_hist')
          #                                         )
          #                       )
          #                                )
          # 
          #                                )
          # 
          #                       ),
          #                       tabPanel('Cover Plot',
          #                                
          #                                plotOutput('cover_plot')
          #                                ),
          #                       tabPanel('Plot',
          # 
          #                                HTML(paste('takes time to generate, please wait ...')),
          #                                
          #                                plotOutput('test_plot_1'),
          #                                plotOutput('test_plot_2')
          #                                ),
          # 
          #                      tabPanel('table',
          #                              dataTableOutput('test_table_1'),
          #                              dataTableOutput('test_table_2'),
          #                              dataTableOutput('test_table_3')
          #                               ),
          #                      tabPanel('Cluster Column Test',
          #                               dataTableOutput('cluster_test')
          #                               )
          #                     ))
                     
                     
                   
          
          ))

  #### END #########
)
  ),#tabset
column(10),
column(2,uiOutput('debug_ui'))
  )#fluidRow
  

)#fluidPage
  
)#shiny

#closeAllConnections()
#rm(list=ls())


