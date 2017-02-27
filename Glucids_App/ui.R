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
                               )
                  )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("1 - Données",     tabName = "data",       icon = icon('database'),       badgeLabel = "In test",     badgeColor = "blue", selected = T),
      menuItem("2 - Calculs",     tabName = "calculs",    icon = icon('tasks'),          badgeLabel = "In progress", badgeColor = "orange"),
      menuItem("3 - Corrections", tabName = "correction", icon = icon('check-square-o'), badgeLabel = "In project",  badgeColor = "red"),
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
                            box(width = 6,
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
                                                                  uiOutput("select_sheets"))
                                ),
                                hr(),
                                fluidRow(
                                  column(width = 6,
                                         actionButton('submit_data', 'Valider', icon = icon('check'))
                                         ),
                                  column(width = 6,
                                         uiOutput("dataset_check")
                                         )
                                )
                            ),
                            box(width = 6, title = 'Vérification des données', solidHeader = T, status = 'primary',
                                column(width = 12, uiOutput('progress_box'))
                            )
                          )
                  ),
                  
                  #################################################
                  tabItem(tabName = "calculs",
                          box(width = 3, solidHeader = T, status = 'primary',
                              shiny::textInput('vol_extraction', label = "Volume d'extraction", value = 600),
                              fluidRow(column(width = 6,
                                              shiny::textInput('conc_SI', label = label.help('Unité', 'unit_SI_help'), value = 0.1),
                                              bsTooltip('unit_SI_help', title = 'Utiliser la même unité que pour les standards externes (feuillet (3))', placement = 'right', trigger = 'hover')
                              ),
                                       column(width = 6,
                                              shiny::textInput('unit_SI', label = 'Unité', value = "µM")
                                       )
                              ),
                              shiny::textInput('dilution_fac', label = "Facteur de dilution", placeholder = '1 = pas de dilution', value = 1),
                              actionButton('submit_data_calc', 'Valider', icon = icon('check'))
                          ),
                          box(width = 9, solidHeader = T, status = 'primary',
                              plotOutput('SI_raw_plot', height='250px')
                          ),
                          box(width = 12, solidHeader = T, status = 'primary',
                              plotOutput('data_calc'))
                  ),
                  #################################################
                  tabItem(tabName = "correction",
                          h3("Appliquer une correction des données")
                          
                  ),
                  #################################################
                  tabItem(tabName = "analysis",
                          h3("Analyses")
                  )
                  #################################################
                )
  )
)
