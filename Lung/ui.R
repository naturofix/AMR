library(shiny)
shinyUI(fluidPage(
  titlePanel("Retrospective Review of AMR Diagnosis and Outcomes"),
  #shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  fluidRow(
    column(3,selectInput('global_factor','Factor to Separate by',c(all_discrete_columns,'cluster'),multiple = F,selected = 'cluster')),
 
    
    column(1,radioButtons("status_radio", 'Status',choiceNames = list('Alive',"Dead",'All'),
                              choiceValues = list("1", "2","0"
                              ),inline = F,selected = '0')),
    column(4,sliderInput('pre_range','Pre Treatment Range',min = -24,max=0,step = 1,value = pre_values, width = 800)),
    column(4,sliderInput('post_range','Post Treatment Range',min = 0,max=24,step = 1,value = post_values,width = 800)),
    column(12,
           radioButtons("data_select", 'Select Data',
                        choiceNames = list('pFEV',"imputed", 'smoothed','D1', "D1 remove imputed", 'D2'),
                        choiceValues = list("pFEV", "imputed",'smoothed', 'd1','d1_ri','d2'),inline = T,selected = 'pFEV')
           
    ),
    
    column(12,
    
    tabsetPanel(selected = 'Patient pFEV',
    ############ TESTING ##############
    #tabPanel('Testing',
             

      # 
      #          tabsetPanel(
      # 
      #                      
      #            tabPanel('Sanity Check',
      #                     verbatimTextOutput('additional_columns'),
      #                     verbatimTextOutput('missing_columns'),
      #                     verbatimTextOutput('duplicated_samples'),
      #                     verbatimTextOutput("processed_data_str")
      #                     ),
      #            tabPanel('text',
      #                     verbatimTextOutput('test_text_1'),
      #                     verbatimTextOutput('test_text_2'),
      #                     verbatimTextOutput('test_text_3')
      #            ),
      #            tabPanel('More Stats',
      #                     column(3,selectInput('binary_factor','Binary Factor',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'Status')),
      #                     column(3,selectInput('add_factor','Additional Factors',c(full_factor_columns,'cluster','cluster_d1'),multiple = F,selected = 'HLAType')),
      #                     column(12,
      #                     tabsetPanel(
      #                       tabPanel('Discrete Variables',
      #                                plotOutput('mosaic_xt'),
      #                                verbatimTextOutput('chisq_xt'),
      #                                verbatimTextOutput('fishe_xt')
      #                                 
      #                                 
      #                                 ),
      #            tabPanel('Logistic Regression',
      #                              column(12,
      #                                     plotOutput('lg_scatter'),
      #                                     verbatimTextOutput('logistic_regression_text'),
      #                                     dataTableOutput('logistic_regression_table'),
      #                                     plotOutput('logistic_regression_p_hist')
      #                              )
      #            )
      #                     )
      #                              
      #                     )         
      #                     
      #            ),
      #            tabPanel('Cover Plot',
      #                     plotOutput('cover_plot')
      #                     ),
      #            tabPanel('Plot',
      #                     HTML(paste('takes time to generate, please wait ...')),
      #                     plotOutput('test_plot_1'),
      #                     plotOutput('test_plot_2')
      #                     ),
      # 
      #           tabPanel('table',
      #                   dataTableOutput('test_table_1'),
      #                   dataTableOutput('test_table_2'),
      #                   dataTableOutput('test_table_3')
      #                    ),
      #           tabPanel('Cluster Column Test',
      #                    dataTableOutput('cluster_test')
      #                    )
      #          )),
    
    
    
    #### DATA TABLES ####
      tabPanel("Data Tables",
               
        tabsetPanel(
          tabPanel('Original',
                   tags$h5('Original Data downloaded from googlesheet'),
                   dataTableOutput('clustering')),
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
                   tags$h5('Data used the generate the clusters'),
                   dataTableOutput("cluster_data")),
          #tabPanel('cluster_d1',dataTableOutput("cluster_data_d1"))
          
          #tabPanel('lm',dataTableOutput("df_lm_table")),
          #tabPanel('lm imputed',dataTableOutput("df_lm_table_i"))
          #tabPanel('D1',dataTableOutput("i_pFEV_sm_d1_f_r"))
          #i_pFEV_sm_d1_f
          tabPanel('Mean Table',
                   tableOutput('pFEV_mean_table')
          ),
          tabPanel('log 2 ratio vs zero',
                   dataTableOutput("pFEV_ratio2zero")),
          tabPanel('precentage change vs zero',
                   dataTableOutput("pFEV_per2zero"))
        )
      
    ),

    #### PATIENT DATA ####
    
    tabPanel('Patient pFEV',
             #uiOutput('scale_slide_1'),
             
             tabsetPanel(selected = 'Exclude Patients',
               tabPanel('Missingness Plot',
                        tags$h5('Missing values in the pFEV data'),
                        plotOutput('missmap_plot'),
                        plotOutput('pFEV_na_hist')),
               tabPanel('Individual Patient Plots',
                        tags$h5('Blue : original data points, Red : Imputed Data, Green : Smoothed Data'),
                        selectInput('mrn_select_i','MRN',patient_list,multiple = T, selected = patient_list[1], width = 800),
                        plotOutput('individual_patients')
               ),
               tabPanel('Exclude Patients',
            tags$h5(paste('Patients with less than ',completeness,'% of the pFEV datapoints were automatically removed from the analysis')),
            textOutput('auto_removed_patients'),
            selectInput('remove_list','Select additional patients to removed',patient_list,multiple = T,selected = unique(c(excluded_patients_c,patient_custom_exclude)), width = 800),

            column(6,selectInput('subset_1','Subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
            column(6,uiOutput('out_select_factor_1')),
            column(12),
            column(6,selectInput('subset_2','Subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
            column(6,uiOutput('out_select_factor_2')),
            column(12),
            column(6,selectInput('subset_3','Subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
            column(6,uiOutput('out_select_factor_3')),
            column(12,
            textOutput('num_patients')),
            column(12,
            textOutput('patients_text'))
            #uiOutput('scale_slide_1')
            #column(6,selectInput('subset_list_0','Select',factor_list,multiple = T,selected = factor_list))
            
            #column(3,selectInput('subset_2','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All')),
            #column(3,selectInput('subset_3','subset by',c('All',all_discrete_columns),multiple = F,selected = 'All'))
            
            ),
            
             tabPanel('Plot all Patients',
                      HTML(paste('Plots take some time to render, please wait ...')),
             tabsetPanel(
               
               
               #   #HTML(paste('Plots take some time to render, please wait ...')),
               tabPanel('Excluded',column(12,uiOutput('excluded_plots'))),
               #   
               tabPanel('Retained',column(12,uiOutput("plots")))
               #tabPanel('Excluded',column(12,uiOutput('excluded_plots')))
             )))
    ),
    
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
                    tabPanel('Statistics',
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
                             
                    ),#Stats
                    
                    

    #### CLUSTERING #####         
    tabPanel('Clustering',
             tabsetPanel(
               tabPanel('Dendograms',
             
             column(6,
                    selectInput('mix_clust_col_fac','Discrete Factors List 1',discrete_columns_4_comparison,multiple = T,selected = c("SignOfInflammation","BiopsyScore"),width = 600),
                    numericInput('fac_weight', "Weight of Discrete Factors for List 1", d_weight_1, min = 0, max = 20, step = 1),
                    selectInput('mix_clust_col_fac_2','Discrete Factors List 2',discrete_columns_4_comparison,multiple = T,selected = c("NewCTChange","Ground glass","HLAType","HLAStrongWeak"),width = 600),
                    numericInput('fac_weight_2', "Weight of Discrete Factors for List 2", d_weight_2, min = 0, max = 20, step = 1)
                    ),
              column(6,
                    selectInput('mix_clust_col_num','Continuous Variable List 1',clustering_continuous_columns,multiple = T,selected = cluster_cols,width = 600),
                    numericInput('num_weight', "Weight of Continuous Variable for List 1", c_weight_1, min = 0, max = 20, step = 1),

                    selectInput('mix_clust_col_num_2','Continuous Variable List 2',clustering_continuous_columns,multiple = T,,selected = c("Eosinophil peak"),width = 600),
                    numericInput('num_weight_2', "Weight of Continuous Variable for List 2", c_weight_2, min = 0, max = 20, step = 1)
                    ),
             column(12,
                    numericInput('clutree_num', "Number of Clusters", num_clusters, min = 1, max = 50, step = 1),
                

                                plotOutput('discrete_cluster_plot'),  
                                plotOutput('mix_clu'),
                                

                                plotOutput('distance_density'),
                                tags$h5('log2 ratio t test'),
                                plotOutput('boxplot_pp_ratio_cluster'),
                                
                                tags$h5('Full Range'),
                                plotOutput('boxplot_pFEV_cluster_full'),
                                tags$h5('MANOVA'),
                                dataTableOutput('selected_manova_table_cluster')
                         )), # Dendogram
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
                                             
                                             uiOutput("cluster_select_clusters"),
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


