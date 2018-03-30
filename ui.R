library(shiny)
shinyUI(fluidPage(
  titlePanel("Retrospective Review of AMR Diagnosis and Outcomes"),
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  fluidRow(
    column(4,selectInput('global_factor','Factor to Separate by',c(all_discrete_columns,'cluster'),multiple = F,selected = 'cluster')),
    
    
    

    column(4,sliderInput('pre_range','Pre Treatment Range',min = -24,max=0,step = 1,value = pre_values, width = 800)),
    column(4,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = post_values,width = 800)),
    column(12,
           uiOutput("cluster_select_clusters"),
           radioButtons('data_source','Select Data Type',c('pFEV1','pFVC','pRatio'),inline = T),
           radioButtons("data_select", 'Select Data',
                        choiceNames = list('pFEV',"imputed", 'imputed to last pFEV value','smoothed','D1', "D1 remove imputed", 'D2'),
                        choiceValues = list("pFEV", "imputed",'imputed_NA', 'smoothed', 'd1','d1_ri','d2'),inline = T,selected = data_select),
    
    tabsetPanel(selected = default_tab,
 
    #### DATA TABLES ####
      #uiOutput('data_table_ui'),
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
        #tabPanel('Pre Clustering Selection',
             #uiOutput('scale_slide_1'),
             
             tabsetPanel(selected = 'Pre Clustering Selection',
                ####_missingness####
               tabPanel('Missingness Plot',
                        tags$h5('Missing values in the pFEV data'),
                        plotOutput('missmap_plot'),
                        plotOutput('pFEV_na_hist')),
               
                ####_selection####
               tabPanel('Pre Clustering Selection',
                  tags$h4(paste(length(duplicated_list), 'entries for individual patients, had duplicated pFEV values and were automatically removed')),
                  textOutput('auto_removed_duplicates'),  
                  HTML("<br>"),
                  tags$h4(paste(length(excluded_patients_c), 'patients with less than ',completeness,'% of the pFEV datapoints were automatically removed from the analysis')),
                  textOutput('auto_removed_patients'),
                  HTML("<br><br>"),
                  #selectInput('select_remove','remove',list(`< 20% of pFEV values` = 'completeness' ,duplicates = 'duplicates'),multiple = T, selected = c('completeness','duplicates')),
                  
                  sliderInput('pre_death_cutoff','Exclude Patients that no longer have pFEV measures after timepoint ',min = -24,max=24,step = 1,value = -24,width = 800),
                  
                  uiOutput('pre_remove_ui'),
                  #actionButton('pre_save','Save List'),
                  
                  plotOutput('pre_hist',height = 200),
                  
                  tags$h5(textOutput('status_text')),
                  column(12,radioButtons("status_radio", 'Status',choiceNames = list('Alive',"Dead",'All'),
                                        choiceValues = list("1", "2","0"
                                        ),inline = T,selected = '0')),
                  tags$h5('Select the column used to to subset the data, then select the the categories within that column to retain'),
                  
                  column(6,selectInput('subset_1','Subset by 1',c('All',all_discrete_columns),multiple = F,selected = subset_1)),
                  column(6,uiOutput('out_select_factor_1')),
                  column(12),
                  column(6,selectInput('subset_2','Subset by 2',c('All',all_discrete_columns),multiple = F,selected = subset_2)),
                  column(6,uiOutput('out_select_factor_2')),
                  column(12),
                  column(6,selectInput('subset_3','Subset by 3',c('All',all_discrete_columns),multiple = F,selected = subset_3)),
                  column(6,uiOutput('out_select_factor_3')),
                  
                  
                  column(12,
                         tags$h5('Retained patient information'),
                  textOutput('pre_num_patients')),
                  column(12,
                  textOutput('pre_patients_text'))
                  #uiOutput('scale_slide_1')
                  #column(6,selectInput('subset_list_0','Select',factor_list,multiple = T,selected = factor_list))
                  
                  #column(3,selectInput('subset_2','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
                  #column(3,selectInput('subset_3','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All'))
                  
              ),
              
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
              
              tabPanel('View Selected Patients',
                       radioButtons('retained_radiobutton','Select Patient Lists', list(All = 'all',`Excluded Pre-clustering` = 'pre',`Excluded Post-clustering` = 'post',`Retained Patients` = 'retained', `BOS data` = 'bos'),selected = 'bos',inline = T),
                       uiOutput('multi_plot_column_select_ui'),
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
                       )
                       
                       
              )
              
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
             )
    
    #tabPanel('Post Clustering Selection')
    ),
    
    #### CLUSTERING #####         
    tabPanel('Clustering',
             tabsetPanel(
               tabPanel('Dendograms',
                        
                        column(6,
                               selectInput('mix_clust_col_fac','Discrete Factors List 1',discrete_columns_4_comparison,multiple = T,selected = discrete_list_1,width = 600),
                               numericInput('fac_weight', "Weight of Discrete Factors for List 1", d_weight_1, min = 0, max = 100, step = 1),
                               selectInput('mix_clust_col_fac_2','Discrete Factors List 2',discrete_columns_4_comparison,multiple = T,selected = discrete_list_2,width = 600),
                               numericInput('fac_weight_2', "Weight of Discrete Factors for List 2", d_weight_2, min = 0, max = 100, step = 1),
                               numericInput('clutree_num', "Number of Clusters", num_clusters, min = 1, max = 50, step = 1),
                               radioButtons('rename_clusters','Rename Clusters',c(T,F),inline = TRUE),
                               radioButtons('run_clustering','Run Clustering',c(F,T),selected = run_clustering,inline = T)
                        
                               
                        ),
                        column(6,
                               #selectInput('mix_clust_col_num','Continuous Variable List 1',clustering_continuous_columns,multiple = T,selected = continuous_list_1,width = 600),
                               uiOutput('ccc_1'),
                               
                               numericInput('num_weight', "Weight of Continuous Variable for List 1", c_weight_1, min = 0, max = 100, step = 1),
                               
                               #selectInput('mix_clust_col_num_2','Continuous Variable List 2',clustering_continuous_columns,multiple = T,,selected = continuous_list_2,width = 600),
                               uiOutput('ccc_2'),
                               numericInput('num_weight_2', "Weight of Continuous Variable for List 2", c_weight_2, min = 0, max = 100, step = 1),
                               selectInput('data_set','Datasets used for clustering',gsub('_matrix','',grep('matrix',colnames(processed_data),value = T)),selected = clustering_data_sets,multiple = T),
                               
                               textOutput('clustered_patients_text')
                               
                               
                        ),
                        column(12,
                               
                               
                               column(12,uiOutput('name_clusters_ui')),
                               column(12,
                                      plotOutput('discrete_cluster_plot'),  
                                      plotOutput('mix_clu'),
                                      
                                      
                                      plotOutput('distance_density'),
                                      
                                      radioButtons("data_select_clust", 'Select Data applied to plots below',
                                                   choiceNames = list('pFEV',"imputed", 'imputed to last pFEV value', 'smoothed','D1', "D1 remove imputed", 'D2'),
                                                   choiceValues = list("pFEV", "imputed",'imputed_NA', 'smoothed', 'd1','d1_ri','d2'),inline = T,selected = 'd1'),
                                      
                                      tags$h5('log2 ratio t test'),
                                      plotOutput('boxplot_pp_ratio_cluster'),
                                      
                                      tags$h5('Full Range'),
                                      plotOutput('boxplot_pFEV_cluster_full'),
                                      tags$h5(textOutput('manova_clustering_text')),
                                      dataTableOutput('selected_manova_table_cluster'),
                                      
                                      tags$h5('ANOVA of continuous variables across clusters'),
                                      dataTableOutput('continuous_manova_cluster')
                               ))# column(12)
                               
                        ), # Dendogram
               #### _chi-squared ####
               tabPanel('Chi-squared',
                        tabsetPanel(
                          tabPanel('Factor',
                                   tags$h5('Factor proportions across clusters by factor'),
                                   
                                   tabsetPanel(
                                     tabPanel('Proportions',
                                              
                                              tags$h6('Chi-squared used to determines if the proportions of a specific factor criteria is randomly distributed between clusters. Proportions of each row sums to 100. Random would equate to equal propotions across clusters. Chi-squared tests if the deviation from random is significant.'),
                                              tabsetPanel(
                                                tabPanel('Selected',dataTableOutput('cluster_analyis_selected_table')),
                                                tabPanel('Full',dataTableOutput('cluster_analysis')))),
                                     tabPanel('Stat',
                                              tags$h6('Chi-squared used to determines if the proportions between factor criteria are significantly different from each other.'),
                                              
                                              tabsetPanel(
                                                tabPanel('Selected',dataTableOutput('chisq_cluster')),
                                                tabPanel('Full',dataTableOutput('chisq_cluster_full')     
                                                )))
                                   )),
                          tabPanel('Cluster',
                                   tags$h5('Factor proportions within clusters'),
                                   
                                   tabsetPanel(
                                     tabPanel('Proportions',
                                              tags$h6('Illustrates the proportions of factor criteria within a cluster, Proportions within a cluster column for each factor sum to 100.'),
                                              tabsetPanel(
                                                tabPanel('Selected',dataTableOutput('cluster_analysis_within_table_selected_table')),
                                                tabPanel('Full',dataTableOutput('cluster_analysis_within_table'))
                                              )),
                                     tabPanel("Stat",
                                              tags$h6('Chi-squared used to determines if the factor criteria proportions between clusters are signficantly different from each factor. The clusters can be selected to determine if there are specific differences between clusters.'),
                                              
                                              tabsetPanel(
                                                tabPanel('Selected',dataTableOutput('chisq_cluster_within')),
                                                tabPanel('Full',dataTableOutput('chisq_cluster_within_full'))
                                              ))
                                   ))
                        ))# chi-squared

             ) 
    ), #Clustering
    
  ##### PLOTS ######
    tabPanel('Plots',

              column(12,
                     tabsetPanel(

                    ######## _LINE PLOTS ###########
                    

                    
                    tabPanel('Line Plot',
                             
                             column(12,plotOutput('line_pFEV')),
                             column(12,plotOutput('smooth_line_pFEV'))
                             
                             ),
                    ######## _BOXPLOTS ##########
                    tabPanel('Boxplot',
                             column(12,
                                    plotOutput('boxplot_pFEV'),
                                    plotOutput('boxplot_pFEV_mean'))
                             
                             ),
                    tabPanel('Interaction',
                             HTML(paste("The primary separation factor is separated into different plots. The Interactors Selector box is used to choose additional factors. Mean line plots are then generated illustrating the how these factors separate the data within the main factor.")),
                             column(3,selectInput('interactors','Ineteractors',c(full_factor_columns,'cluster','cluster_d1'),multiple = T,selected = 'Status')),
                             
                             plotOutput('interaction_plot')
                             )))),
                    

      ############## STAT ################
                    tabPanel('Statistics',tabsetPanel(
                             tabPanel('Lung Measurements',
                             column(12,

                                  radioButtons('mtc','Multiple Testing Correction', choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none"),selected = 'BH', inline = T),
                                  tabsetPanel(selected = 'Pre Treatment vs Post Treatment',
                                    ### _pre vs post ####
                                    tabPanel('Pre Treatment vs Post Treatment',
                                             tags$h5('Statistical assesment of values between pre treatment ranges and post treatment ranges. The ranges are adjusted using the range slides.'),
                                             #HTML(paste(input$pre_range[1])),
                                             tabsetPanel(selected = 'T test',
                                                  tabsetPanel(
                   
                                          

                                          
                                                      tabPanel('Pre vs Treatment vs Post',
                                                              tags$h5('T test calculated on values from a pre-treatment or post-treatment timepoint vs the treatment timepoint'),
                                                              HTML(paste(textOutput('t_pFEV_zero_text'))),
                                                               
                                                               tabsetPanel(
                                                                   tabPanel('Selected',
                                                                          plotOutput('boxplot_pp_zero'),
                                                                          dataTableOutput('pp_t_table_zero')
                                                                   ),
                                                                   tabPanel('Full',dataTableOutput("pp_t_test_zero_full"))
                                                               )
                                                      ),
                                                      tabPanel('Post / Treatment vs Treatment / Pre',
                                                               tags$h5('T test calculated on ratios or percentages of  (treatment / pre-treatment )  vs (post-treatment timepoint / treatment timepoint)'),
                                                               radioButtons('vs_zero_prefix', label = 'T test calculated on ratios or percentages of  (treatment timepoint / pre-treatment timepoint)  vs (post-treatment timepoint / treatment timepoint)', choiceNames = c('log2(ratio)','Percentage'), choiceValues = c('log2zero_','per2zero_'),inline = T),
                                                               tabsetPanel(
                                    
                                                                 tabPanel('Selected',
                                                                          plotOutput('percentage_boxplot'),
                                                                          dataTableOutput('percentage_table')
                                                                          ),
                                                                 tabPanel('Data', dataTableOutput('percentage_df')),
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
                                                                                           plotOutput('boxplot_pp_ranges'),
                                                                                           dataTableOutput('pp_t_table_ranges')
                                                                                  ),
                                                                                  tabPanel('Full',dataTableOutput('pp_t_test_ranges_full'))
                                                                                  
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
                                                                                   # radioButtons("col_select", 'Ratio generated by equation described below',
                                                                                   #              choiceNames = list('Ratio','log2(ratio)', 'Percentage',  'Relative Percentage'),
                                                                                   #              choiceValues = list("sym_ratio_colnames",'sym_log_ratio_colnames','sym_per_colnames', 'sym_rel_per_colnames'), select ='sym_rel_per_colnames', inline = T),
                                                                                   # 
                                                                                   radioButtons("col_select_prefix", 'Ratio generated by equation described below',
                                                                                                choiceNames = list('Ratio','log2(ratio)', 'Percentage',  'Relative Percentage'),
                                                                                                choiceValues = list("ratio_",'log2_','per_', 'per_rel_'), select ='log2_', inline = T),
                                                                                   selectInput('ratio_num','timepoints',sym_times_cols,multiple = T,selected = sym_times_cols,width = 600),
                                                                                   
                                                                                   #tags$h4('Ratios generates symmetrically across treatment i.e(-3/3) and then statistics performed on these ratios'),
                                                                                   tags$h5(textOutput('sym_equation')),
                                                                                   uiOutput('scale_slide'),
                                                                                   
                                                                                   plotOutput('sym_ratio_boxplot'),
                                                                                   
                                                                                   tabsetPanel(
                                
                                                                                         tabPanel('Statistics',
                                      
                                                                                                  #plotOutput('sym_per_boxplot'),
                                                                                                  tabsetPanel(
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
                             ),
                             # Continuous Variable #####
                             tabPanel('Continuous Variables', tabsetPanel(
                               # _Mean Table ##############
                               tabPanel('Mean Table',
                                        
                                        dataTableOutput('pFEV_mean_table')
                               ),
                               # _ANOVA ###################
                               tabPanel('ANOVA',
                                        #column(6,selectInput('continuous_variable','Select Continuous Variable',clustering_continuous_columns,multiple = F)),
                                        
                                        #column(12,uiOutput("cluster_select_clusters_anova")),
                                        tabsetPanel(
                                          tabPanel('Individual',
                                                   selectInput('continuous_variable','Select Continuous Variable',clustering_continuous_columns,multiple = F),
                                                   
                                                   plotOutput('anova_cluster_plot'),
                                                   dataTableOutput('continuous_manova_single')
                                          ),
                                          tabPanel('Full',dataTableOutput('continuous_manova_full'))
                                        )
                               )
                             ))
                             )
                             
                    ),#Stats
                    
                    



    ####### BOSS #######
    tabPanel('BOS',
             radioButtons('bos_data_select','Select pFEV Data to use for BOS calculation, pRatio uses imputed',list(original = '', imputed = 'i'),selected = 'i',inline = T),
             radioButtons('sequence_correction','Correct BOS assignments so they are always in sequence',c(T,F),selected = F,inline = T),
             radioButtons('first_and_last','Only do BOS calculation between first and last pFEV1 measurements',c(T,F),selected = T,inline = T),
             radioButtons('measured_columns','Only do BOS calculation where measurements were taked in any patient, impute missing only for these timepoints',c(T,F),selected = T,inline = T),
             
             column(12,sliderInput('bos_range','Timecourse Range',min = -48,max=48,step = 1,value = c(-24,24),width = 800)),
             
             tabsetPanel(
               
               tabPanel('Line Plots',
                        plotOutput('bos_plots'),
                        uiOutput('BOS_plot_ui')
                        #plotOutput('RAS_factor_plot'),
                        #plotOutput('bos1_factor_plot'),
                        #plotOutput('bos2_factor_plot'),
                        #plotOutput('bos3_factor_plot'),
                        #plotOutput('bos3_surv_factor_plot')
                        #plotOutput('boss_3_factor_l')
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
             ), # BOSS

          

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
  )#tabset
  )#fluidRow
  

)#fluidPage
  
)#shiny

#closeAllConnections()
#rm(list=ls())


