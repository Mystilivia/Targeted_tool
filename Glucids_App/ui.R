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
library(magrittr)
library(DT)
library(shinydashboard)
library(shinyBS)
library(data.table)
library(ggplot2)
library(readxl)
library(markdown)

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
      menuItem("1 - Données",     tabName = "data",       icon = icon('database'), badgeLabel = "In test", badgeColor = "blue", selected = T),
      menuItem("2 - Calculs",     tabName = "calculs",    icon = icon('tasks'), badgeLabel = "In progress", badgeColor = "orange"),
      menuItem("3 - Corrections", tabName = "correction", icon = icon('check-square-o'), badgeLabel = "In project", badgeColor = "red"),
      menuItem("4 - Analyses",    tabName = "analysis",   icon = icon('bar-chart'), badgeLabel = "In project", badgeColor = "red")
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
                  tabItem(tabName = "data",
                          fluidRow(
                            box(width = 12,
                                title = "Description", solidHeader = T, status = 'success',
                                includeMarkdown('text/description.md')
                            )
                          ),
                          fluidRow(
                            box(width = 6,
                                title = 'Choix des données', solidHeader = T, status = 'primary',
                                selectInput('dataset', label = label.help('Choisir un jeu de données', 'dataset_help'), choices = c('Aucun', 'Glucides (GC-FID)', 'Acides aminés (UPLC-DAD)', 'Importer un fichier')),
                                bsTooltip(id = 'dataset_help', title = '', placement = 'right', trigger = 'hover'),
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
                                )
                            ),
                            box(width = 6, title = 'Vérification des données', solidHeader = T, status = 'primary',
                                column(width = 12, uiOutput('progress_box'))
                            )
                          )
                  ),
                  tabItem(tabName = "calculs",
                          h3("Calculs des concentrations")
                          
                  ),
                  tabItem(tabName = "correction",
                          h3("Appliquer une correction des données")
                          
                  ),
                  tabItem(tabName = "analysis",
                          h3("Analyses")
                  )
                )
  )
)
