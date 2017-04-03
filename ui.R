#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(DT)
library(shinydashboard)
library(shinyBS)
library(markdown)
library(curl)
library(plotly)


# from https://github.com/davesteps/machLearn/blob/master/init.R
label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}


dashboardPage(
  dashboardHeader(title = "P2M2 - Analyses ciblées",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Admin",
                                 message = "The app layout is under construction",
                                 icon = icon("info-circle"),
                                 time = "2017-02-16"
                               ),
                               messageItem(
                                 from = "Admin",
                                 message = "Import tab v1.0 is working",
                                 icon = icon("info-circle"),
                                 time = "2017-02-21"
                               ),
                               messageItem(
                                 from = "Admin",
                                 message = "First working version with SI correction",
                                 icon = icon("info-circle"),
                                 time = "2017-03-28"
                               )
                  )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("1 - Données",     tabName = "data",       icon = icon('database'),       badgeLabel = "In test",     badgeColor = "blue", selected = T),
      menuItem("2 - Calculs",     tabName = "calculs",    icon = icon('tasks'),          badgeLabel = "In test", badgeColor = "blue"),
      menuItem("3 - Corrections", tabName = "correction", icon = icon('check-square-o'), badgeLabel = "In dev",  badgeColor = "orange"),
      menuItem("4 - Analyses",    tabName = "analysis",   icon = icon('bar-chart'),      badgeLabel = "In project",  badgeColor = "red")
    ),
    hr(),
    fluidRow(
      column(width = 1),
      column(width = 10, uiOutput('sidebar_info'))
    ),
    absolutePanel(
      bottom = 10,
      left = 10,
      draggable = F,
      width='100%',
      height='auto',
      p(icon('github fa-2x'), "Sylvain Dechaumet")
    )
  ),
  
  dashboardBody(useShinyjs(),
                tags$head(tags$style("#dataset_check{font-size: 20px;
                                     font-style: bold;
                                     }")),
                tabItems(
                  #################################################
                  tabItem(tabName = "data",
                          fluidRow(
                            box(width = 7,
                                title = "Description", solidHeader = T, status = 'info',
                                includeMarkdown('text/description.md')
                            ),
                            column(width = 5,
                                   box(width = 12, title = 'Exemple feuillet (1)', solidHeader = T, collapsible = T, collapsed = T, status = 'info',
                                       img(src="table_1.png", height = 120)),
                                   box(width = 12, title = 'Exemple feuillet (2)', solidHeader = T, collapsible = T, collapsed = T, status = 'info',
                                       img(src="table_2.png", height = 120)),
                                   box(width = 12, title = 'Exemple feuillet (3)', solidHeader = T, collapsible = T, collapsed = T, status = 'info',
                                       img(src="table_3.png", height = 120))
                                   )
                          ),
                          fluidRow(
                            box(width = 7,
                                title = 'Choix des données', solidHeader = T, status = 'primary',
                                selectInput('dataset', label = 'Choisir un jeu de données', choices = c('Aucun', 'Glucides (GC-FID)', 'Acides aminés (UPLC-DAD)', 'Importer un fichier')),
                                conditionalPanel("input.dataset == 'Glucides (GC-FID)'",
                                                 downloadButton('download_exemple1', label = 'Télécharger')
                                ),
                                conditionalPanel("input.dataset == 'Acides aminés (UPLC-DAD)'",
                                                 downloadButton('download_exemple2', label = 'Télécharger')
                                ),
                                conditionalPanel("input.dataset == 'Importer un fichier'",
                                                 fileInput('file_input', NULL, accept = c(".xls", ".xlsx")),
                                                 conditionalPanel("file_input_sheets > 0",
                                                                  htmlOutput("select_sheets"))
                                ),
                                hr(),
                                actionButton('submit_data', 'Valider', icon = icon('check'))
                            ),
                            box(width = 5, title = 'Vérification des données', solidHeader = T, status = 'primary',
                                column(width = 12, uiOutput('dataset_check'))
                            )
                          )
                  ),
                  
                  #################################################
                  tabItem(tabName = "calculs",
                            box(width = 4,
                                title = 'Choix des données', solidHeader = T, status = 'primary',
                                fluidRow(
                                  column(6, shiny::textInput('vol_extraction', label = "Volume d'extraction", value = 600)),
                                  column(6, shiny::textInput('dilution_fac', label = "Facteur de dilution", placeholder = '1 = pas de dilution', value = 1))
                                ),
                                hr(),
                                fluidRow(
                                  column(6, shiny::textInput('conc_SI', label = label.help('[SI] échantillons', 'unit_SI_help'), value = 100),
                                  bsTooltip('unit_SI_help', title = 'Utiliser la même unité que pour les standards externes (feuillet (3))', placement = 'right', trigger = 'hover')),
                                  column(6, shiny::textInput('unit_SI', label = 'Unité', value = "µM"))
                                ),
                                hr(),
                                fluidRow(
                                  column(6, selectizeInput('Mass_col', label = label.help('Masses Extraite', 'Mass_col_help'), choices = '', options = list(
                                    maxItems = 1, placeholder = 'Colonne des masses')),  
                                    bsTooltip('Mass_col_help', title = 'Choisir la colonne qui contient les masses extraites du feuillet (2)', placement = 'right', trigger = 'hover')),
                                  column(6, shiny::textInput('unit_Mass', label = 'Unité', value = "mg de MS"))
                                ),
                                hr(),
                                fluidRow(
                                  column(width = 4, actionButton('submit_data_calc', 'Valider', icon = icon('check')))
                                )
                          ),
                          
                          column(width = 8,
                                 box(width = 6,
                                     title= "Graphique", solidHeader = T, status = 'primary',
                                     selectizeInput('sample_choice_1', label = 'Choisissez un ou plusieurs échantillons :', choices = '', multiple = T, width = '75%'),
                                     plotlyOutput('data_calc', width = "auto", height = "auto")
                                 ),
                                 box(width = 6,
                                     title = "Rendements d'extractions", solidHeader = T, status = 'primary',
                                     selectizeInput('batch_choice_1', label = label.help('Choisissez un ou plusieurs batchs :', 'outliers_help'), choices = '', multiple = T, width = '75%'),
                                     bsTooltip('outliers_help', title = "Les outliers sont définis par un écart de 2.5 * la MAD du batch", placement = 'left', trigger = 'hover'),
                                     plotlyOutput("data_calc_Rmdt_plot", width = "auto", height = "auto")
                                 )
                          )
                  ),
                  #################################################
                  tabItem(tabName = "correction",
                          box(width = 3, title = "Appliquer une correction des données", solidHeader = T, status = 'primary',
                              h4("Normaliser par rapport à un standard interne (SI)"),
                                column(12,
                                       column(width = 4, actionButton('submit_data_correction', 'Valider', icon = icon('check'))),
                                       column(width = 4, downloadButton('download_data_norm', 'Télécharger', icon = icon('check')))
                              ),
                              hr(),
                              h4("Corriger la déviation analytique"),
                              hr()
                          ),
                          box(width = 9, title = "Teneurs corrigées", solidHeader = T, status = 'primary',
                              selectizeInput('sample_choice_2', label = 'Choisissez un ou plusieurs échantillons :', choices = '', multiple = T, width = '75%'),
                              plotlyOutput('data_calc_norm_plot', width = "auto", height = "auto")
                              )
                  ),
                  #################################################
                  tabItem(tabName = "analysis",
                          h3("Analyses")
                  )
                  #################################################
                )
  )
)
