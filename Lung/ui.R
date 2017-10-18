library(shiny)
shinyUI(fluidPage(
  titlePanel("Retrospective Review of AMR Diagnosis and Outcomes"),
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  fluidRow(
    column(3,selectInput('global_factor','Factor to Separate by',c(all_discrete_columns,'cluster','cluster_d1'),multiple = F,selected = 'cluster_d1')),
    column(1,radioButtons("status_radio", 'Status',choiceNames = list('Alive',"Dead",'Both'),
                              choiceValues = list("1", "2","0"
                              ),inline = F,selected = '0')),
    column(4,sliderInput('pre_range','Pre Treatment Range',min = -24,max=0,step = 1,value = c(-3,0),width = 800)),
    column(4,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(0,3),width = 800)),
    #column(4,sliderInput('pre_range','Pre Treatment Range',min = -24,max=0,step = 1,value = c(-12,0),width = 800)),
    #column(4,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(0,12),width = 800)),
    #column(6,sliderInput('anova_range','Timecourse Range',min = -48,max=24,step = 1,value = c(-6,6),width = 800)),
    
    column(12,
    
    tabsetPanel(selected = 'Testing',
    ############# TESTING ##############
      tabPanel('Testing',
 
               tabsetPanel(selected = 'Sym Data',
                           tabPanel('Sym Data',
                                    tabsetPanel(
                                      tabPanel('Data Table',
                                               tags$h3('log2 ratio of timepoints on either side of treatment, ie log2(-12/12) or log2(-6/6)'),
                                               dataTableOutput('pFEV_ratio_df')
                                      ),
                                      tabPanel('Plots',
                                               plotOutput('sym_ratio_boxplot'),
                                               plotOutput('sym_per_boxplot')),
                                      tabPanel('Means',
                                        tags$h4('Mean log2(ratio)  (-12/12)'),
                                        dataTableOutput("sym_ratio_mean_df"),
                                        tags$h4('Mean percentage change (-12 to 12)'),
                                        dataTableOutput("sym_per_mean_df")
                                               ),
                                      tabPanel('MANOVA',
                                               tags$h5('Determines if there is a significant difference between the factors (ie clusters), when log2 ratios are generated between two timepoints across 0. ie(log2(-12/12), MANOVA compares all the ratio together across the time range which can be changed using the post range slider'),
                                               
                                               dataTableOutput('manova_sym_table')
                                               ),
                                      tabPanel('ANOVA',
                                        tags$h5('Determines if there is a significant difference between the factors (ie clusters) at a specific timepoints for which log2 ratio were generated. ie(log2(-12/12)'),
                                        dataTableOutput('anova_pw_sym_ratio')
                                               ),
                                      tabPanel('T tests',
                                               dataTableOutput('sym_t_test_table')
                                               
                                    )
                           )),
                           
                 tabPanel('Sanity Check',
                          verbatimTextOutput('additional_columns'),
                          verbatimTextOutput('missing_columns'),
                          verbatimTextOutput('duplicated_samples'),
                          verbatimTextOutput("processed_data_str")
                          ),
                 tabPanel('text',
                          verbatimTextOutput('test_text_1'),
                          verbatimTextOutput('test_text_2'),
                          verbatimTextOutput('test_text_3')
                 ),
                 tabPanel('More Stats',
                          column(3,selectInput('binary_factor','Binary Factor',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'Status')),
                          column(3,selectInput('add_factor','Additional Factors',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'HLAType')),
                          column(12,
                          tabsetPanel(
                            tabPanel('Discrete Variables',
                                     plotOutput('mosaic_xt'),
                                     verbatimTextOutput('chisq_xt'),
                                     verbatimTextOutput('fishe_xt')
                                      
                                      
                                      ),
                 tabPanel('Logistic Regression',
                                   column(12,
                                          plotOutput('lg_scatter'),
                                          verbatimTextOutput('logistic_regression_text'),
                                          dataTableOutput('logistic_regression_table'),
                                          plotOutput('logistic_regression_p_hist')
                                   )
                 )
                          )
                                   
                          )         
                          
                 ),
                 tabPanel('Cover Plot',
                          plotOutput('cover_plot')
                          ),
                 tabPanel('Plot',
                          HTML(paste('takes time to generate, please wait ...')),
                          plotOutput('test_plot_1'),
                          plotOutput('test_plot_2')
                          ),
  
                tabPanel('table',
                        dataTableOutput('test_table_1'),
                        dataTableOutput('test_table_2'),
                        dataTableOutput('test_table_3')
                         ),
                tabPanel('Cluster Column Test',
                         dataTableOutput('cluster_test')
                         )
               )),
    
    #### DATA TABLES ####
      tabPanel("Data",
               
        tabsetPanel(
          tabPanel('Original',dataTableOutput('clustering')),
          tabPanel('Processed',dataTableOutput('full_num')),
          tabPanel('Term Mapping',
                   tableOutput('term_mapping')),
          
          
          # tabPanel('pFEV',dataTableOutput('pFEV_wf')),
          # tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf')),
          # tabPanel('Smoothed',dataTableOutput('i_pFEV_sm_lf')),
          # #i_pFEV_sm_lf_r
          # #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
          # tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f")),
          # tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f")),
          
          tabPanel('pFEV',dataTableOutput('pFEV_wf_r')),
          tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf_r')),
          tabPanel('Smoothed',dataTableOutput('i_pFEV_sm_lf_r')),
          #i_pFEV_sm_lf_r
          #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
          tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r")),
          tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f_r")),
          tabPanel('cluster',dataTableOutput("cluster_data")),
          tabPanel('cluster_d1',dataTableOutput("cluster_data_d1")),
          
          tabPanel('lm',dataTableOutput("df_lm_table")),
          tabPanel('lm imputed',dataTableOutput("df_lm_table_i"))
          #tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r"))
          #i_pFEV_sm_d1_f
        )
      
    ),
    ######### MISSINGNESS PLOT and SELECT DATA #############
    tabPanel('Missingness Plot', 
             plotOutput('missmap_plot'),
             plotOutput('pFEV_na_hist')),
    
    
    tabPanel('Select Data',
             tabsetPanel(
               tabPanel('Select',
            tags$h5(paste('Patients with less than ',completeness,'% of the pFEV datapoints were automatically removed from the analysis')),
            textOutput('auto_removed_patients'),
             selectInput('remove_list','Additional Patients Removed',patient_list,multiple = T,selected = unique(c(excluded_patients_c,patient_custom_exclude)), width = 800)
               ),
             tabPanel('Plot',
                      HTML(paste('Plots take some time to render, please wait ...')),
             tabsetPanel(
               
               
               #   #HTML(paste('Plots take some time to render, please wait ...')),
               tabPanel('Excluded',column(12,uiOutput('excluded_plots'))),
               #   
               tabPanel('Retained',column(12,uiOutput("plots")))
               #tabPanel('Excluded',column(12,uiOutput('excluded_plots')))
             )))
    ),
    
    
##################### pFEV #####################
    tabPanel('pFEV',
             #column(6,numericInput('completeness', "Completeness", 30, min = 0, max = 100, step = 1,
             #             width = NULL)),
              column(12,
                     tabsetPanel(
                       tabPanel('Tables',
                                tabsetPanel(
                                    tabPanel('Mean Table',
                                    tableOutput('pFEV_mean_table')
                             ),
                             tabPanel('log 2 ratio vs zero',
                                         dataTableOutput("pFEV_ratio2zero")),
                             tabPanel('precentage change vs zero',
                                      dataTableOutput("pFEV_per2zero"))

                             )),
                    ######## LINE PLOTS ###########
                    tabPanel('Line Plot',
                             
                             #selectInput('mrn_select','MRN',patient_list,multiple = T,selected = patient_list, width = 800),
                             column(12,plotOutput('line_pFEV')),
                             column(12,plotOutput('smooth_line_pFEV')),
                             
                             column(12,plotOutput('line_i_pFEV'))
                             ),
                    ######## BOXPLOTS ##########
                    tabPanel('Boxplot',
                             column(12,
                                    plotOutput('boxplot_pFEV'),
                                    plotOutput('boxplot_i_pFEV')),
                             column(6,
                                    plotOutput('boxplot_pFEV_mean')),
                             column(6,
                                    plotOutput('boxplot_i_pFEV_mean'))
                             
                             ),
                    tabPanel('Interaction',
                             HTML(paste("The primary separation factor is separated into different plots. The Interactors Selector box is used to choose additional factors. Mean line plots are then generated illustrating the how these factors separate the data within the main factor.")),
                             column(3,selectInput('interactors','Ineteractors',c(full_factor_columns,'cluster','cluster_d1'),multiple = T,selected = 'Status')),
                             
                             plotOutput('interaction_plot')
                             ),
                    

                    ############## STAT ################
                    tabPanel('Stats',
                             column(12,
                                    #selectInput('anova_factor','Factor to Separate by',full_factor_columns,multiple = F,selected = 'Status'),                        tabsetPan
                                  tabsetPanel(
                                    ### PRE POST ####
                                    tabPanel('Pre Treatment vs Post Treatment',
                                             tabsetPanel(
  
                                  
                                      
                                          ########### ANOVA #############
      
                                          tabPanel('ANOVA',
                                                tabsetPanel(
                                                   tabPanel('Original Data',
                                                       dataTableOutput('lm_table'),
                                                       plotOutput('boxplot_anova_all_factor'),
                                                       column(6,plotOutput('boxplot_anova_before_factor')),
                                                       column(6,plotOutput('boxplot_anova_after_factor'))
                                              ), #ANOVA G
                                              tabPanel('Imputed',
                                                       dataTableOutput('lm_table_i'),
                                                       plotOutput('boxplot_anova_all_factor_i'),
                                                       column(6,plotOutput('boxplot_anova_before_factor_i')),
                                                       column(6,plotOutput('boxplot_anova_after_factor_i'))
                                                       )
                                                )),

                                              ############ SLOPE #################
                                              tabPanel('Slope',
                                                       HTML(paste0('T test calculated on pFEV slopes for each patient. Slopes are calculated from a linear regression of all points between two timepoints. The timepoints can be adjusted using the Timecourse Range slider ',textOutput('slope_pFEV_text'))),
                                                       tabsetPanel(
                                                         tabPanel('Original Data',
                                                          column(6,plotOutput('slope_fit_pFEV_pre_plot')),
                                                          column(6,plotOutput('slope_fit_pFEV_post_plot')),
                                                          column(12,
                                                          tags$span(style="color:red", "Shading no longer indicates significance here"),
                                                          plotOutput('slope_boxplot'),
                                                           dataTableOutput('slope_table')
                                                         )), 
                                                         tabPanel('Imputed',
                                                                  column(6,plotOutput('slope_fit_pFEV_pre_plot_i')),
                                                                  column(6,plotOutput('slope_fit_pFEV_post_plot_i')),
                                                                  column(12,
                                                                         tags$span(style="color:red", "Shading no longer indicates significance here"),
                                                                         
                                                               plotOutput('slope_boxplot_i'),
                                                               dataTableOutput('slope_table_i')
                                                               #plotOutput('slope_boxplot_i')
                                                         ))
                                                       )
                                              ),
                                              ############## T test ################
                                              # tabPanel('t test (pFEV)',
                                              #          HTML(paste0('T test calculated on pFEV values, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_pFEV_text'))),
                                              #          tabsetPanel(
                                              #            tabPanel('Original Data',
                                              #               dataTableOutput('pp_t_table'),
                                              #               plotOutput('boxplot_pp')
                                              #                ),
                                              #            tabPanel('Imputed',
                                              #               dataTableOutput('pp_t_table_i'),
                                              #               plotOutput('boxplot_pp_i')
                                              #             )
                                              #          )
                                              #       ),
                                          
                                          tabPanel('t test',
                                                   HTML(paste0('T test are calculated on all pFEV values between the selected timepoints.')),
                                                   HTML(paste0('The timepoints can be adjusted using the Pre and Post Treatment Range Sliders ',textOutput('t_range_text'))),
                                                   
                                                   #column(6,sliderInput('pre_range','Pre Treatment Range',min = -48,max=0,step = 1,value = c(-6,0),width = 800)),
                                                   #column(6,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(0,6),width = 800)),
                                                   tabsetPanel(
                                                     tabPanel('Original Data',
                                                              plotOutput('boxplot_pp_ranges'),
                                                              dataTableOutput('pp_t_table_ranges')
                                                              
                                                     ),
                                                     tabPanel('Imputed',
                                                              plotOutput('boxplot_pp_ranges_i'),
                                                              dataTableOutput('pp_t_table_ranges_i')
                                                              
                                                     )
                                                   )

                                                ),
                                          
                                              tabPanel('t test zero',
                                                       HTML(paste0('T test calculated on pFEV values from a pre or post timepoint vs the zero timepoint, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_pFEV_zero_text'))),
                                                       
                                                       tabsetPanel(
                                                         tabPanel('Original Data',
                                                                  #HTML(paste0('T test calculated on log2 ratio, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_ratio_text'))),
                                                                  plotOutput('boxplot_pp_zero'),
                                                                  dataTableOutput('pp_t_table_zero')
                                                                  #plotOutput('boxplot_pp_zero')
                                                         ),
                                                         tabPanel('Imputed',
                                                                  plotOutput('boxplot_pp_zero_i'),
                                                                  dataTableOutput('pp_t_table_zero_i')
                                                                  
                                                         )
                                                       )
                                              ),
                                              tabPanel('t test log2(ratio)',
                                                       tabsetPanel(
                                                         tabPanel('Original Data',
                                                             #HTML(paste0('T test calculated on log2 ratio, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_ratio_text'))),
                                                             dataTableOutput('pp_t_table_ratio_full'),
                                                             plotOutput('boxplot_pp_ratio_full')
                                                         ),
                                                         tabPanel('Imputed',
                                                                  dataTableOutput('pp_t_table_ratio_full_i'),
                                                                  plotOutput('boxplot_pp_ratio_full_i')
                                                          )
                                                       )
                                              ),
                                              tabPanel('t test log2(ratio) by factor',
                                                       HTML(paste0('T test calculated on log2 ratio, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_ratio_text'))),
                                                       tabsetPanel(
                                                         tabPanel('Original Data',
                                                           plotOutput('boxplot_pp_ratio'),
                                                           dataTableOutput('pp_t_table_ratio')
                                                            ),
                                                         tabPanel('Imputed',
                                                                  #HTML(paste0('T test calculated on log2 ratio, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_ratio_text'))),
                                                                  plotOutput('boxplot_pp_ratio_i'),
                                                                  dataTableOutput('pp_t_table_ratio_i')
                                                                  
                                                               )
                                                         
                                                         )
                                                       )
                                    ) # tabset
                             ),
                             
                             ### TIME SERIES #####
                             tabPanel("Time-series comparisons",
                                      tabsetPanel(
                                        tabPanel('MANOVA',
                                                 tabsetPanel(
                                                   tabPanel('Original Data',
                                                            tabsetPanel(
                                                              tabPanel('Selected',
                                                                       plotOutput('boxplot_pFEV_manova'),
                                                                       dataTableOutput('selected_manova_table')
                                                              ),
                                                              tabPanel('Full',
                                                                       dataTableOutput('full_manova_table'))
                                                            )
                                                   ),
                                                   tabPanel('Imputed',
                                                            tabsetPanel(
                                                              tabPanel('Selected',
                                                                       plotOutput('boxplot_pFEV_manova_i'),
                                                                       dataTableOutput('selected_manova_table_i')
                                                              ),
                                                              tabPanel('Full',
                                                                       dataTableOutput('full_manova_table_i'))
                                                            )
                                                   ))))),
                                                 
                                        tabPanel('Pre vs Pre and Post vs Post',
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
                                                 tabPanel('MANOVA',
                                                          dataTableOutput('horizontal_anova_full')),
                                                  tabPanel('T test',
                                                          dataTableOutput('horizontal_t_test_full')

                                                          )
                                                 ))
                                                 
                                        
                                        
                                        
                                      ))
                             # column
                             
                    )#Stats
                    
                    
                        )) #tabsetPanel
             ), #pFEV

##################### Change #########################
    tabPanel('Change',

             column(12,
                    tabsetPanel(

                        ########## LINE ##########
                         tabPanel('smooth',
                                  #selectInput('change_mrn_select','MRN',patient_list,multiple = T,selected = patient_list,width = 800),
                                  plotOutput('i_pFEV_sm_line'),
                                  plotOutput('boxplot_i_pFEV_sm')
                                ),
                        ########## D1 ##############
                             tabPanel('D1',
                                      tabsetPanel(
                                        ### LINE PLOT ####
                                        tabPanel('Line Plot',
              
                                            column(12,
                                                   plotOutput('i_pFEV_sm_d1_line'),
                                                   plotOutput('boxplot_i_pFEV_sm_d1'),
                                                   plotOutput('boxplot_i_pFEV_sm_d1_mean'),
                                                   plotOutput('i_pFEV_sm_d1_line_ri'),
                                                   plotOutput('boxplot_i_pFEV_sm_d1_ri'),
                                                   plotOutput('boxplot_i_pFEV_sm_d1_mean_ri')
                                                   )
                                            #column(6,plotOutput('boxplot_i_change'))
                                            ),

                                        ##### STATS ###########
                                        tabPanel('Stats',
                                                 tabsetPanel(
                                                   tabPanel('Pre Treatment vs Post Treatment',
                                                            tabsetPanel(
                                                   tabPanel('ANOVA',
                                                            dataTableOutput('lm_table_d1'),
                                                            plotOutput('boxplot_anova_all_factor_d1'),
                                                            column(6,plotOutput('boxplot_anova_before_factor_d1')),
                                                            column(6,plotOutput('boxplot_anova_after_factor_d1'))
                                                   ),

                                                   tabPanel('Slope',
                                                            HTML(paste0('T test calculated on imputed smoothed first differential (D1) slopes for each patient. The slopes are calculated as a linear regression of all the values between the timepoints. The timepoints can be adjusted using the Timecourse Range slider ',textOutput('slope_d1_text'))),
                                                            column(6,plotOutput('slope_fit_pFEV_pre_plot_d1')),
                                                            column(6,plotOutput('slope_fit_pFEV_post_plot_d1')),
                                                            column(12,
                                                                   tags$span(style="color:red", "Shading no longer indicates significance here"),
                                                            plotOutput('slope_boxplot_d1'),
                                                            dataTableOutput('slope_table_d1')
                                                   )),
                                                   # tabPanel('t test',
                                                   #          HTML(paste0('T test calculated on imputed smoothed first differential values (D1), (use with caution)')),
                                                   #          HTML(paste0('the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_d1_text'))),
                                                   #          
                                                   #          dataTableOutput('pp_t_table_d1'),
                                                   #          plotOutput('boxplot_pp_d1')
                                                   # ),
                                                   tabPanel('t test',
                                                            HTML(paste0('T test calculated on imputed smoothed first differential values (D1), (use with caution)')),
                                                            HTML(paste0('the timepoints can be adjusted using the Pre and Post Treatment Range Sliders ',textOutput('t_d1_text'))),
                                                            HTML(paste(textOutput('paired_t_d1'))),
                                                            #column(6,sliderInput('d1_pre_range','Pre Treatment Range',min = -48,max=0,step = 1,value = c(-6,0),width = 800)),
                                                            #column(6,sliderInput('d1_post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(0,6),width = 800)),
                                                            column(12,
                                                                   plotOutput('boxplot_pp_ranges_d1'),

                                                              dataTableOutput('pp_t_table_ranges_d1')
                                                            )
                                                            
                                                            
                                                            
                                                            )
                                                   
                                                 )),
                                                 tabPanel('Time-series Comparison',
                                                          tabsetPanel(
                                                          tabPanel('MANOVA',
                                                                   tabsetPanel(
                                                                     tabPanel('Original Data',
                                                                              tabsetPanel(
                                                                                tabPanel('Selected',
                                                                                         plotOutput('boxplot_pFEV_manova_d1'),
                                                                                         dataTableOutput('selected_manova_table_d1')
                                                                                ),
                                                                                tabPanel('Full',
                                                                                         dataTableOutput('full_manova_table_d1'))
                                                                              )
                                                                     )
                                                                   ))
                                                          
                                                )
                                                          
                                                          ),
                                                tabPanel('Pre vs Pre and Post vs Post',
                                                         tabsetPanel(
                                                           tabPanel('Selected',
                                                                    column(6,plotOutput("hor_box_pre_d1")),
                                                                    column(6,plotOutput("hor_box_post_d1")),
                                                                    column(12,
                                                                           tags$h4('MANOVA'),
                                                                           dataTableOutput('horizontal_anova_d1'),
                                                                           tags$h4('T test'),
                                                                           dataTableOutput('horizontal_t_test_d1'))
                                                           ),
                                                           tabPanel('MANOVA',
                                                                    dataTableOutput('horizontal_anova_full_d1')),
                                                           tabPanel('T test',
                                                                    dataTableOutput('horizontal_t_test_full_d1')
                                                                    
                                                           )
                                                         ))
                                                
                                                
                                                ) #tabsetPanel#######
                                                 )
                                      )
                                      
                             ), 
                        ###### D2 ##########
                             tabPanel('D2',
                                      tabsetPanel(
                                        tabPanel('Line Plot',
                                          column(12,
                                                 plotOutput('i_pFEV_sm_d2_line'),
                                                 plotOutput('boxplot_i_pFEV_sm_d2'),
                                                 plotOutput('boxplot_i_pFEV_sm_d2_mean')
                                          )
                                        )
                                      )
              
                             )
                        ########

             ))
    ),#change
    #### CLUSTERING ####
    tabPanel('Clustering',
             
             column(6,
                    selectInput('mix_clust_col_fac','Discrete Factors List 1',discrete_columns_4_comparison,multiple = T,selected = c("SignOfInflammation","BiopsyScore"),width = 600),
                    numericInput('fac_weight', "Weight of Discrete Factors for List 1", 10, min = 0, max = 20, step = 1),
                    selectInput('mix_clust_col_fac_2','Discrete Factors List 2',discrete_columns_4_comparison,multiple = T,selected = c("NewCTChange","Ground glass","HLAType","HLAStrongWeak"),width = 600),
                    numericInput('fac_weight_2', "Weight of Discrete Factors for List 2", 5, min = 0, max = 20, step = 1)
                    ),
              column(6,
                    selectInput('mix_clust_col_num','Continuous Variable List 1',clustering_continuous_columns,multiple = T,selected = c('-3','-1','0'),width = 600),
                    numericInput('num_weight', "Weight of Continuous Variable for List 1", 4, min = 0, max = 20, step = 1),

                    selectInput('mix_clust_col_num_2','Continuous Variable List 2',clustering_continuous_columns,multiple = T,,selected = c("Eosinophil peak"),width = 600),
                    numericInput('num_weight_2', "Weight of Continuous Variable for List 2", 10, min = 10, max = 20, step = 1)
                    ),
             column(12,
                #HTML('Weight of factors changes how the factors influence the clustering. The pFEV values are set at a weight of 10'),
                numericInput('clutree_num', "Number of Clusters", 3, min = 1, max = 50, step = 1),
                
                tabsetPanel(selected = 'pFEV Data',
                  ###### CLUSTERING pFEV #########
                  tabPanel('pFEV Data',
                           radioButtons("cluster_imputed", 'Select Data',
                                        choiceNames = list('Original',"Imputed"),
                                        choiceValues = list("original", "imputed"),inline = T,selected = 'original'),
                           tabsetPanel(
                             tabPanel('Dendograms',
                                plotOutput('discrete_cluster_plot'),  
                                plotOutput('mix_clu'),
                                

                                plotOutput('distance_density'),
                                tags$h4('log2 ratio t test'),
                                plotOutput('boxplot_pp_ratio_cluster'),
                                
                                #tags$h4('Clustering Range'),
                                # plotOutput('boxplot_pFEV_cluster'),
                                #tags$h4('Full Range'),
                                 plotOutput('boxplot_pFEV_cluster_full')
                                 #plotOutput('bos3_surv_factor_plot_cluster'),
                                  
                                
                                #plotOutput('bos3_surv_factor_plot_cluster'),
                                
                                 #plotOutput('discrete_cutree_line'),
                                 #plotOutput('discrete_cutree_mean'),
                                #dataTableOutput('discrete_x_table'),
                                # htmlOutput('D_text')
                             ),
           
                             # tabPanel('ScatterPlots',
                             #          plotOutput('distance_scatter'),
                             #          plotOutput('distance_density'),
                             #          plotOutput('distance_polygon'),
                             #          plotOutput('distance_polygon_neat'),
                             #          dataTableOutput('distance_table'),
                             #          dataTableOutput('distance_model_table')
                             # ),
                             tabPanel('MANOVA',
                                      plotOutput('boxplot_pFEV_manova_cluster'),
                                      dataTableOutput('cluster_pairwise_manova_table')),
                             
                             tabPanel('Chi-squared',
                                      tabsetPanel(
                                        tabPanel('Selected',
                                              tags$h3('Factor proportions across clusters by factor status'),
                                              dataTableOutput('cluster_analyis_selected_table'),
                                              dataTableOutput('chisq_cluster'),
                                              
                                              tags$h3('Factor proportions within clusters'),
                                              dataTableOutput('cluster_analysis_within_table_selected_table'),
                                              #uiOutput("cluster_select_factors"),
                                              uiOutput("cluster_select_clusters"),
                                              
                                              verbatimTextOutput('test_text_c'),
                                              dataTableOutput('chisq_cluster_within')
                                     ),
                             tabPanel('Full',
                                      tags$h3('Factor proportions across clusters by factor status'),
                                      dataTableOutput('cluster_analysis'),
                                      tags$h3('Factor proportions within clusters'),
                                      dataTableOutput('cluster_analysis_within_table')
                             ))
                             
                             
                             ))), # Imputed pFEV Data
           ##### CLUSTERING CHANGE ####
                  tabPanel('Change Data (D1)',
                           tabsetPanel(
                             tabPanel('Dendograms',
                                      radioButtons("cluster_change_imputed", 'Select Data',
                                                   choiceNames = list('Imputed',"Remove pFEV missing values"),
                                                   choiceValues = list("imputed", "removed"),inline = T,selected = 'imputed'),
                                        plotOutput('discrete_cluster_plot_d1'),
                                        plotOutput('mix_clu_d1'),
                                        plotOutput('distance_density_d1'),
                                      tags$h4('log2 ratio t test'),
                                      plotOutput('boxplot_pp_ratio_cluster_d1'),
                                      
                                        #dataTableOutput('cluster_analysis_within_table_selected_d1'),
                                        #uiOutput("clusters_d1"),
                                        #textOutput('chisq_cluster_within_d1'),
                                      
                                        #plotOutput("bos3_factor_plot_cluster_d1"),
                                        #plotOutput('boxplot_pFEV_cluster_d1'),
                                        plotOutput('boxplot_pFEV_cluster_d1_full')
                                        
                                      #plotOutput('bos3_surv_factor_plot_cluster_d1'),
                                      
                                         #plotOutput('discrete_cutree_line_d1'),
                                         #plotOutput('discrete_cutree_mean_d1'),
                                       # dataTableOutput('discrete_x_table_d1'),
                                      
                                       #  htmlOutput('D_d1_text')), #Dendrogram

                             # tabPanel('ScatterPlot',
                             #          plotOutput('distance_scatter_d1'),
                             #          plotOutput('distance_polygon_d1'),
                             #          plotOutput('distance_polygon_neat_d1'),
                             #          dataTableOutput('distance_table_d1'),
                             #          dataTableOutput('distance_model_table_d1')
                                       ),
                             tabPanel('MANOVA',
                                      plotOutput('boxplot_change_manova_cluster_d1'),
                                      dataTableOutput('cluster_change_pairwise_manova_table_d1'),
                                      plotOutput('boxplot_pFEV_manova_cluster_d1'),
                                      dataTableOutput('cluster_pFEV_pairwise_manova_table_d1')),
                             tabPanel('Chi-squared',
                                      tabsetPanel(
                                        tabPanel('Selected',
                                    tags$h3('Factor proportions across clusters by factor status'),
                                    dataTableOutput('cluster_analyis_selected_table_d1'),
                                    dataTableOutput('chisq_cluster_d1'),
                                    
                                    tags$h3('Factor proportions within clusters'),
                                    dataTableOutput('cluster_analysis_within_table_selected_table_d1'),
                                    uiOutput("cluster_select_clusters_d1"),
                                    dataTableOutput('chisq_cluster_within_d1')
                           ),
                           tabPanel('Full',
                                    tags$h3('Factor proportions across clusters by factor status'),
                                    dataTableOutput('cluster_analysis_d1'),
                                    tags$h3('Factor proportions within clusters'),
                                    dataTableOutput('cluster_analysis_within_table_d1')
                           )))
                           
                           )),# change Data
                  tabPanel('Comparison',
                           HTML('Comparison of clusters generated from pFEV and Change D1 data. The line plots are from the means for each cluster. The pFEV clusters are the thicker lines'),
                           plotOutput('cluster_comparison'))
                #   tabPanel('Composition',
                #            tabsetPanel(
                #              tabPanel('Total',
                #                       HTML(paste('Percentage of total number of discrete factor occuring within each clusterm. The sum of every row is equal to 100%')),
                #                       tabsetPanel(
                #                         tabPanel('pFEV clusters',
                #                             dataTableOutput('cluster_analysis')),
                #                         tabPanel('D1 clusters',
                #                                  dataTableOutput('cluster_analysis_d1')   
                #                                   )
                #                         
                #                         
                #                                 )
                #                       ),
                #             tabPanel('Within Clusters',
                #                      HTML(paste('Percentage of total number of discrete factor occuring within each clusterm. The sum of every row is equal to 100%')),
                #                      tabsetPanel(
                #                        tabPanel('pFEV clusters',
                #                                 dataTableOutput('cluster_analysis_within_table')),
                #                        tabPanel('D1 clusters',
                #                                 dataTableOutput('cluster_analysis_within_d1_table')   
                #                        )
                #                        
                #                        
                #                      ))
                #         
                # )) # Composition
                )
                
             )
    ),

    ####### BOSS #######
    tabPanel('BOS',
             column(12,sliderInput('bos_range','Timecourse Range',min = -24,max=48,step = 1,value = c(-24,24),width = 800)),
             
             tabsetPanel(
               
               tabPanel('Line Plots',
                        plotOutput('bos_plots'),
                        plotOutput('bos1_factor_plot'),
                        plotOutput('bos2_factor_plot'),
                        plotOutput('bos3_factor_plot'),
                        plotOutput('bos3_surv_factor_plot')
                        #plotOutput('boss_3_factor_l')
               ),
               tabPanel('Tables',
                        dataTableOutput('bos_df'),
                        dataTableOutput('boss_factor_table'),
                        dataTableOutput('bos_patient_status')
                        ),
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
          tabPanel('R Session Info',
                              htmlOutput('citation1')
                              
                              )

  #### END #########
)
  )#tabset
  )#fluidRow
  

)#fluidPage
  
)#shiny

#closeAllConnections()
#rm(list=ls())


