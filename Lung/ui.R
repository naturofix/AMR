library(shiny)
shinyUI(fluidPage(
  titlePanel("Retrospective Review of AMR Diagnosis and Outcomes"),
  fluidRow(
    column(6,selectInput('global_factor','Factor to Separate by',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'Status')),
    column(6,sliderInput('anova_range','Timecourse Range',min = -48,max=24,step = 1,value = c(-6,6),width = 800)),
    tabsetPanel(
    ############# DATA ##############
      tabPanel("Data",
        tabsetPanel(
          tabPanel('Original',dataTableOutput('clustering')),
          tabPanel('Processed',dataTableOutput('full_num')),
          tabPanel('pFEV',dataTableOutput('pFEV_wf_r')),
          tabPanel('Imputed pFEV',dataTableOutput('i_pFEV_wf_r')),
          #tabPanel('Imputed pFEV clustering',dataTableOutput('i_pFEV_wf_r_c')),
          tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r")),
          tabPanel('D2',dataTableOutput("i_pFEV_sm_d2_f_r"))
          
          #tabPanel('lm',dataTableOutput("df_lm_table")),
          #tabPanel('lm imputed',dataTableOutput("df_lm_table_i")),
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
                    
                    ###### CLUSTERING #########
                    tabPanel('Clustering',
                             selectInput('mix_clust_col_fac','Clustering Columns',colnames(full_fac_0),multiple = T,selected = default_cluster_list ,width = 600),
                             selectInput('mix_clust_col_num','Clustering Columns',colnames(i_pFEV_ts),multiple = T,selected = c(pFEV_numeric_colnames_f),width = 600),
                             HTML('Weight of factors changes how the factors influence the clustering. The pFEV values are set at a weight of 10'),
                             numericInput('fac_weight', "Weight of Factors", 0, min = 0, max = 20, step = 1),
                             plotOutput('mix_clu'),
                             plotOutput('discrete_cluster_plot'),
                             numericInput('clutree_num', "Number of Clusters", 5, min = 1, max = dim(full_num)[1], step = 1),
         
                             dataTableOutput('discrete_x_table'),
                             plotOutput('discrete_cutree_line'),
                             plotOutput('discrete_cutree_mean')
                             

                    ),
                    ##### BOSS #######
                    tabPanel('BOSS',
                             tabsetPanel(
                               tabPanel('BOSS temp',
                                        plotOutput('boss_test'))
                             )
                             
                    ),#BOSS
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
                                                       HTML(paste0('T test calculated on pFEV slopes for each patient, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('slope_pFEV_text'))),
                                                       tabsetPanel(
                                                         tabPanel('Original Data',
                                                           dataTableOutput('slope_table'),
                                                           plotOutput('slope_boxplot')
                                                         ), 
                                                         tabPanel('Imputed',
                                                               dataTableOutput('slope_table_i'),
                                                               plotOutput('slope_boxplot_i')
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
                                                   HTML(paste0('T test calculated on imputed smoothed first differential values (D1), (use with caution)')),
                                                   HTML(paste0('the timepoints can be adjusted using the Pre and Post Treatment Range Sliders ',textOutput('t_range_text'))),
                                                   
                                                   column(6,sliderInput('pre_range','Pre Treatment Range',min = -48,max=0,step = 1,value = c(-6,-2),width = 800)),
                                                   column(6,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(2,6),width = 800)),
                                                   tabsetPanel(
                                                     tabPanel('Original Data',
                                                              dataTableOutput('pp_t_table_ranges'),
                                                              plotOutput('boxplot_pp_ranges')
                                                     ),
                                                     tabPanel('Imputed',
                                                              dataTableOutput('pp_t_table_ranges_i'),
                                                              plotOutput('boxplot_pp_ranges_i')
                                                     )
                                                   )

                                                ),
                                          
                                              tabPanel('t test zero',
                                                       HTML(paste0('T test calculated on pFEV values from a pre or post timepoint vs the zero timepoint, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_pFEV_zero_text'))),
                                                       
                                                       tabsetPanel(
                                                         tabPanel('Original Data',
                                                                  #HTML(paste0('T test calculated on log2 ratio, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_ratio_text'))),
                                                                  dataTableOutput('pp_t_table_zero'),
                                                                  plotOutput('boxplot_pp_zero')
                                                         ),
                                                         tabPanel('Imputed',
                                                                  dataTableOutput('pp_t_table_zero_i'),
                                                                  plotOutput('boxplot_pp_zero_i')
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
                                                           dataTableOutput('pp_t_table_ratio'),
                                                           plotOutput('boxplot_pp_ratio')
                                                            ),
                                                         tabPanel('Imputed',
                                                                  #HTML(paste0('T test calculated on log2 ratio, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('t_ratio_text'))),
                                                                  dataTableOutput('pp_t_table_ratio_i'),
                                                                  plotOutput('boxplot_pp_ratio_i')
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
                                        #### CLUSTERING ####
                                        tabPanel('Clustering',
                            
                                                 selectInput('d1_mix_clust_col_fac','Clustering Columns',colnames(full_fac_0),multiple = T,selected = c("Status","SignOfInfection","NewCTChange","HLAType","HLAStrength"),width = 600),
                                                 selectInput('d1_mix_clust_col_num','Clustering Columns',colnames(i_pFEV_ts),multiple = T,selected = c(pFEV_numeric_colnames_f),width = 600),
                                                 numericInput('d1_fac_weight', "Weight of Factors", 0, min = 0, max = 10, step = 1),
                                                 plotOutput('mix_clu_d1'),
                                                 plotOutput('discrete_cluster_plot_d1'),
                                                 numericInput('d1_clutree_num', "Number of Clusters", 5, min = 1, max = dim(full_num)[1], step = 1),
                                                 
                                                 dataTableOutput('discrete_x_table_d1'),
                                                 plotOutput('discrete_cutree_line_d1'),
                                                 plotOutput('discrete_cutree_mean_d1')
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
                                                            HTML(paste0('T test calculated on imputed smoothed first differential slopes for each patient, the timepoints can be adjusted using the Timecourse Range slider ',textOutput('slope_d1_text'))),
                                                            
                                                            dataTableOutput('slope_table_d1'),
                                                            plotOutput('slope_boxplot_d1')
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
                                                            
                                                            column(6,sliderInput('d1_pre_range','Pre Treatment Range',min = -48,max=0,step = 1,value = c(-6,-2),width = 800)),
                                                            column(6,sliderInput('d1_post_range','Post Treatment Range',min = 0,max=24,step = 1,value = c(2,6),width = 800)),
                                                            dataTableOutput('pp_t_table_ranges_d1'),
                                                            plotOutput('boxplot_pp_ranges_d1')
                                                            
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

    ######### R SESSION INFO #########
          tabPanel('R Session Info',
                   tabsetPanel(
                     tabPanel('SessionInfo',HTML(paste(htmlOutput("sessionInfo")))),
                     tabPanel('Package Citations',HTML(paste(htmlOutput('cite'))))
                   )
          )
  #### END #########

  )#tabset
  )#fluidRow
  

)#fluidPage
  
)#shiny

