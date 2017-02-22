source('init.R',local = T)

# UI ----------------------------------------------------------------------

ui <- bootstrapPage(useShinyjs(),
                    # Add custom CSS & Javascript;
                    tagList(tags$head(
                      tags$link(rel="stylesheet", type="text/css",href="style.css"),
                      tags$script(type="text/javascript", src = "busy.js"),
                      lapply(1:length(mdls),function(i) modelCSS(mdls[i],pal[i]))
                      
                    )),
                    
                    dashboardPage(#skin = 'red',
                      dashboardHeader(title = HTML(paste(icon('cubes'),'machLearn'))
                      ),
                      dashboardSidebar(
                        sidebarMenu(
                          id = "tabs",
                          menuItem("Step 1: Input Data", tabName = "setup", icon = icon("cog")),
                          menuItem("Step 2: Training & CV",tabName = "model", icon = icon("sitemap"),selected = T),
                          menuItem("Step 3: Model Performance",tabName = "test", icon = icon("bar-chart")),
                          menuItem("Exploration", icon = icon(">>"),
                                   menuSubItem("Feature Importance",tabName = "imp"))
                        ),
                        hr(),
                        fluidRow(
                          column(width=1),
                          column(width=10,
                                 h5(textOutput('txt_dataset')),
                                 h5(textOutput('txt_n')),
                                 h5(textOutput('txt_Yvar')),
                                 h5(textOutput('txt_testSet'))
                          ),
                          column(width=1)
                        ),
                        absolutePanel(
                          bottom = 10,
                          left = 10,
                          draggable = F,
                          width='100%',
                          height='auto',
                          a(icon('github fa-2x'),href='https://github.com/davesteps/machLearn',target='_blank')
                        )
                      ),
                      dashboardBody(
                        tabItems(
                          tabItem("setup",
                                  box(width = 4,title = 'Input Dataset',solidHeader = T,status = 'primary',
                                      selectInput('dataset',label = 'Choose Dataset',
                                                  choices = names(datasets),selected='iris'),
                                      fileInput('fileIn',label = 'Upload data') %>% disabled(),
                                      actionButton('btn_viewData',label = 'View Data',icon=icon('table')),
                                      hr(),
                                      sliderInput('sld_testsplit',label = label.help('Test set %','lbl_testsplit'),min = 33,max = 90,step = 1,value = 33),
                                      bsTooltip(id = "lbl_testsplit", title = "% of data to set aside for test data", 
                                                placement = "right", trigger = "hover")
                                  ),
                                  box(width=4,title = 'y variable',solidHeader = T,status = 'primary',
                                      helpText('Select the variable we would like to predict'),
                                      selectizeInput('yvar',label=label.help('y var','lbl_yvar'),choices = character(0)),
                                      helpText(HTML(paste('data type:', textOutput('Ytype')))),
                                      bsTooltip(id = "lbl_yvar", title = "Variable to predict", 
                                                placement = "right", trigger = "hover"),
                                      hr(),
                                      plotOutput('Yplot',height=260),
                                      conditionalPanel("output.Ytype == 'numeric'|output.Ytype == 'integer'",
                                                       checkboxInput('chk_logY',label = 'log transform')
                                      ),
                                      verbatimTextOutput('Ystats')
                                  ),
                                  box(width=4,title = 'X vars',solidHeader = T,status = 'primary',
                                      selectizeInput('xvar',label=label.help('X (Predict Y as function of):','lbl_xvar'),choices = character(0),multiple = T),
                                      bsTooltip(id = "lbl_xvar", title = "Try and predict Y as function of these variables", 
                                                placement = "right", trigger = "hover")
                                  ),
                                  bsModal('data',title = 'Dataset',trigger = 'btn_viewData',size = 'large',
                                          dataTableOutput('rawdata')
                                  )
                          ),
                          tabItem("model",
                                  column(width=3,
                                         box(width = 12,title = 'Model Options',solidHeader = T,status = 'primary',
                                             selectInput('slt_algo',label = 'Algorithm:'%>%label.help('lbl_algo'),
                                                         choices = reg.mdls,selected = reg.mdls,multiple=T),
                                             selectizeInput('slt_Tune','Parameter Tuning'%>%label.help('lbl_Tune'),
                                                            choices = c('Coarse auto-tune (fast)','Fine auto-tune (slow)','manual')),
                                             radioButtons('rdo_CVtype',label = 'Cross-validation folds'%>%label.help('lbl_CV'),
                                                          choices = c('3-fold'=3,'5-fold'=5,'10-fold'=10),inline = T),
                                             
                                             actionButton('btn_train',label = 'Train Models',
                                                          icon = icon('cogs'),#'bullseye','rocket'
                                                          class='btn-danger fa-lg',
                                                          width='100%'),
                                             bsTooltip(id = "lbl_algo", title = "Which algorithms to test", 
                                                       placement = "right", trigger = "hover"),
                                             bsTooltip(id = "lbl_Tune", title = "Type of tuning which is performed to optimize model parameters", 
                                                       placement = "right", trigger = "hover"),
                                             bsTooltip(id = "lbl_CV", title = "Number of splits of training data used to tune parameters", 
                                                       placement = "right", trigger = "hover")
                                         ),
                                         box(width = 12,title = 'Summary',solidHeader = F,
                                             status = 'primary',
                                             helpText(textOutput('txt_bestModel')),
                                             helpText(textOutput('txt_bestModelStat1')),
                                             helpText(textOutput('txt_bestModelStat2')),
                                             hr(),
                                             helpText(textOutput('txt_Type')),
                                             helpText(textOutput('txt_CV')),
                                             helpText(textOutput('txt_nModels'))
                                         )
                                  ),
                                  tabBox(width = 9,
                                         tabPanel(title = 'CV Model Rank',#icon = icon('sort-amount-asc'),
                                                  h4('Cross-validation results'),
                                                  plotOutput('CVplot1',height=600)
                                         ),
                                         tabPanel(title = 'CV Pred vs Obs',
                                                  h4('Observed vs Predicted (best candidate for algorithm)'),
                                                  plotOutput('CVplot2',height=600)
                                         ),
                                         tabPanel(title = 'CV Stats',
                                                  h4('Performance statiscs from cross-validation'),
                                                  
                                                  dataTableOutput('model_info')
                                         )
                                  )
                          ),
                          tabItem("test",
                                  column(width=3,
                                         box(width = 12,title = 'Test Set Predictions',solidHeader = F,status = 'primary',
                                             selectInput('slt_Finalalgo',label = 'Final Model:'%>%label.help('lbl_Finalalgo'),
                                                         choices=mdls,multiple=T),
                                             helpText('The best cross-validated model is selected by default. 
                                                      Multiple models can be selected to make ensemble predictions'),
                                             bsTooltip(id = "lbl_Finalalgo", title = "Which algorithms to use to predict test", 
                                                       placement = "right", trigger = "hover")
                                             
                                             ),
                                         valueBoxOutput('testsetS1',width=12),
                                         valueBoxOutput('testsetS2',width=12)
                          ),
                          box(width = 6,title = 'Test Set observed vs Predicted',
                              solidHeader = T,status = 'primary',
                              plotOutput('testsetPlot')
                          )
                                  ),
                          tabItem("imp",
                                  box(width = 6,title = 'Feature importance',
                                      helpText('Relative feature importance indicated from randomForest'),
                                      
                                      plotOutput('featImp')
                                  )
                          )
                        )
                        )
                    ),
                    div(class = "busy", 
                        h4("working..."),
                        h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                    )
)