library(shiny)
shinyUI(fluidPage(
  titlePanel("Retrospective Review of AMR Diagnosis and Outcomes"),
  fluidRow(
    column(6,selectInput('global_factor','Factor to Separate by',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'Status')),
    column(6,sliderInput('anova_range','Timecourse Range',min = -48,max=24,step = 1,value = c(-6,6),width = 800)),
    tabsetPanel(
    ############# DATA ##############
      tabPanel('Test Stuff',
               tabsetPanel(
                 tabPanel('first',
                          textOutput('test_text_1'),
                          dataTableOutput('test_table_1')),
                 tabPanel('New',
                    plotOutput('plot_test')
               ))),
      tabPanel("Data",
               
        tabsetPanel(
          tabPanel('Original',dataTableOutput('clustering')),
          tabPanel('Processed',dataTableOutput('full_num')),
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
             HTML(paste('Plots take some time to render, please wait ...')),
             selectInput('remove_list','Patients Removed',patient_list,multiple = T,selected = excluded_patients_c, width = 800),
             
             tabsetPanel(
               #   #HTML(paste('Plots take some time to render, please wait ...')),
               tabPanel('Excluded',column(12,uiOutput('excluded_plots'))),
               #   
               tabPanel('Retained',column(12,uiOutput("plots")))
               #tabPanel('Excluded',column(12,uiOutput('excluded_plots')))
             )
    ),
    
    
##################### pFEV #####################
    tabPanel('pFEV',
             #column(6,numericInput('completeness', "Completeness", 30, min = 0, max = 100, step = 1,
             #             width = NULL)),
              column(12,
                     tabsetPanel(
                    ######## LINE PLOTS ###########
                    tabPanel('Line Plot',
                             
                             selectInput('mrn_select','MRN',patient_list,multiple = T,selected = patient_list, width = 800),
                             column(12,plotOutput('line_pFEV')),
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
                    

                    # ##### BOSS #######
                    # tabPanel('BOSS',
                    #          tabsetPanel(
                    #            tabPanel('BOSS temp',
                    #                     plotOutput('boss_test'))
                    #          )
                    #          
                    # ),#BOSS
                    ############## STAT ################
                    tabPanel('Stats',
                             column(12,
                                    #selectInput('anova_factor','Factor to Separate by',full_factor_columns,multiple = F,selected = 'Status'),
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
                                                          plotOutput('slope_boxplot'),
                                                           dataTableOutput('slope_table')
                                                         ), 
                                                         tabPanel('Imputed',
                                                               plotOutput('slope_boxplot_i'),
                                                               dataTableOutput('slope_table_i')
                                                               #plotOutput('slope_boxplot_i')
                                                         )
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
                                                   
                                                   column(6,sliderInput('pre_range','Pre Treatment Range',min = -48,max=0,step = 1,value = c(-6,0),width = 800)),
                                                   column(6,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(0,6),width = 800)),
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
                             )# column
                             
                    )#Stats
                    
                    
                        )) #tabsetPanel
             ), #pFEV

##################### Change #########################
    tabPanel('Change',

             column(12,
                    tabsetPanel(
                        ########## LINE ##########
                         tabPanel('smooth',
                                  selectInput('change_mrn_select','MRN',patient_list,multiple = T,selected = patient_list,width = 800),
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
                                                   plotOutput('boxplot_i_pFEV_sm_d1_mean')
                                                   )
                                            #column(6,plotOutput('boxplot_i_change'))
                                            ),

                                        ##### STATS ###########
                                        tabPanel('Stats',
                                                 tabsetPanel(
                                                   tabPanel('ANOVA',
                                                            dataTableOutput('lm_table_d1'),
                                                            plotOutput('boxplot_anova_all_factor_d1'),
                                                            column(6,plotOutput('boxplot_anova_before_factor_d1')),
                                                            column(6,plotOutput('boxplot_anova_after_factor_d1'))
                                                   ),
                                                   tabPanel('Slope',
                                                            HTML(paste0('T test calculated on imputed smoothed first differential (D1) slopes for each patient. The slopes are calculated as a linear regression of all the values between the timepoints. The timepoints can be adjusted using the Timecourse Range slider ',textOutput('slope_d1_text'))),
                                                            plotOutput('slope_boxplot_d1'),
                                                            dataTableOutput('slope_table_d1')
                                                   ),
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
                                                            column(6,sliderInput('d1_pre_range','Pre Treatment Range',min = -48,max=0,step = 1,value = c(-6,0),width = 800)),
                                                            column(6,sliderInput('d1_post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(0,6),width = 800)),
                                                            column(12,
                                                                   plotOutput('boxplot_pp_ranges_d1'),

                                                              dataTableOutput('pp_t_table_ranges_d1')
                                                            )
                                                            
                                                            
                                                            
                                                            )
                                                   
                                                 )
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

             )) #tabsetPanel
    ),#change
    #### CLUSTERING ####
    tabPanel('Clustering',
             
             column(6,
                    selectInput('mix_clust_col_fac','Discrete Factors List 1',colnames(full_fac_0),multiple = T,selected = default_cluster_list,width = 600),
                    numericInput('fac_weight', "Weight of Discrete Factors for List 1", 10, min = 10, max = 20, step = 1),
                    selectInput('mix_clust_col_fac_2','Discrete Factors List 2',colnames(full_fac_0),multiple = T,width = 600),
                    numericInput('fac_weight_2', "Weight of Discrete Factors for List 2", 0, min = 0, max = 20, step = 1)
                    ),
              column(6,
                    selectInput('mix_clust_col_num','Continuous Variable List 1',colnames(i_pFEV_ts),multiple = T,selected = default_continuous_cluster_list,width = 600),
                    numericInput('num_weight', "Weight of Continuous Variable for List 1", 10, min = 10, max = 20, step = 1),

                    selectInput('mix_clust_col_num_2','Continuous Variable List 2',colnames(i_pFEV_ts),multiple = T,width = 600),
                    numericInput('num_weight_2', "Weight of Continuous Variable for List 2", 0, min = 10, max = 20, step = 1)
                    ),
             column(12,
                #HTML('Weight of factors changes how the factors influence the clustering. The pFEV values are set at a weight of 10'),
                numericInput('clutree_num', "Number of Clusters", 5, min = 1, max = dim(full_num)[1], step = 1),
                
                tabsetPanel(
                  ###### CLUSTERING #########
                  tabPanel('Imputed pFEV Data',
                           #selectInput('mix_clust_col_fac','Clustering Columns',colnames(full_fac_0),multiple = T,selected = default_cluster_list ,width = 600),
                           #selectInput('mix_clust_col_num','Clustering Columns',colnames(i_pFEV_ts),multiple = T,selected = c(pFEV_numeric_colnames_f),width = 600),
                           #HTML('Weight of factors changes how the factors influence the clustering. The pFEV values are set at a weight of 10'),
                           #numericInput('fac_weight', "Weight of Factors", 0, min = 0, max = 20, step = 1),
                           plotOutput('mix_clu'),
                           plotOutput('discrete_cluster_plot'),
                           dataTableOutput('cluster_analysis_within_table_selected'),
                           
                           #numericInput('clutree_num', "Number of Clusters", 5, min = 1, max = dim(full_num)[1], step = 1),
                           dataTableOutput('discrete_x_table'),
                           plotOutput('discrete_cutree_line'),
                           plotOutput('discrete_cutree_mean'),
                           htmlOutput('D_text')
                           
                           
                  ),
                  tabPanel('Change Data (D1)',
                   plotOutput('mix_clu_d1'),
                   plotOutput('discrete_cluster_plot_d1'),
                   dataTableOutput('cluster_analysis_within_table_selected_d1'),
                   dataTableOutput('discrete_x_table_d1'),
                   plotOutput('discrete_cutree_line_d1'),
                   plotOutput('discrete_cutree_mean_d1'),
                   htmlOutput('D_d1_text')
                  ),
                  tabPanel('Comparison',
                           HTML('Comparison of clusters generated from pFEV and Change D1 data. The line plots are from the means for each cluster. The pFEV clusters are the thicker lines'),
                           plotOutput('cluster_comparison')),
                  tabPanel('Composition',
                           tabsetPanel(
                             tabPanel('Total',
                                      HTML(paste('Percentage of total number of discrete factor occuring within each clusterm. The sum of every row is equal to 100%')),
                                      tabsetPanel(
                                        tabPanel('pFEV clusters',
                                            dataTableOutput('cluster_analysis')),
                                        tabPanel('D1 clusters',
                                                 dataTableOutput('cluster_analysis_d1')   
                                                  )
                                        
                                        
                                                )
                                      ),
                            tabPanel('Within Clusters',
                                     HTML(paste('Percentage of total number of discrete factor occuring within each clusterm. The sum of every row is equal to 100%')),
                                     tabsetPanel(
                                       tabPanel('pFEV clusters',
                                                dataTableOutput('cluster_analysis_within_table')),
                                       tabPanel('D1 clusters',
                                                dataTableOutput('cluster_analysis_within_d1_table')   
                                       )
                                       
                                       
                                     ))
                        
                )) # Composition
                )
                
             )
    ),

    ######### R SESSION INFO #########
          tabPanel('R Session Info',
                              htmlOutput('citation1')
                              
                              )

  #### END #########

  )#tabset
  )#fluidRow
  

)#fluidPage
  
)#shiny

