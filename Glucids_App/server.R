#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(curl)

# from https://github.com/davesteps/machLearn/blob/master/init.R
label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

# data.exemple1 <- list('datamatrix' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfSUJVZTczWm9fUmM'),
#                       'samplemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfNlN1cVZzYzh6aFk'),
#                       'variablemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfTlB3eUJwNXE0Szg'))
# 
# data.exemple2 <- list('datamatrix' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfbkRzcGtQYTZGc00'),
#                       'samplemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfcWdoNWlid2d2WHc'),
#                       'variablemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfbi0zZ0txM2daQ1U'))

data.exemple1 <- readRDS('data/glucids.rds')
data.exemple2 <- readRDS('data/aminoacids.rds')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #### Importation des données [OK]
  file_input_sheets <- reactive({
    validate(need(!is.null(input$file_input), "Aucun fichier compatible chargé"))
    data <- input$file_input
    if (is.null(data)) {return(NULL)}
    file.rename(data$datapath, paste0(data$datapath, '.xlsx'))
    temp <- readxl::excel_sheets(paste0(data$datapath, '.xlsx'))
    return(setNames(as.list(1:length(temp)), temp))
  })
  
  ## Choix des onglets [OK]
  output$select_sheets <- renderUI({
    sheets_list <- file_input_sheets()
    if(is.null(sheets_list)) {return(p("Aucune feuille de calcul détectée"))}
    if(length(sheets_list) < 3) {return(p("Le fichier excel doit avoir au moins 3 feuilles de calculs (voir Description)."))}
    return(
      list(
        selectInput('sheet_datamatrix', label = label.help('(1) Données', 'sheet_datamatrix_help'), sheets_list, selected = 1),
        bsTooltip('sheet_datamatrix_help', title = 'Feuille de calcul comprenant les données des variables pour chaque échantillon', placement = 'right', trigger = 'hover'),
        selectInput('sheet_samples', label = label.help('(2) Echantillons', 'sheet_samples_help'), sheets_list, selected = 2),
        bsTooltip('sheet_samples_help', title = 'Feuille de calcul comprenant la description des échantillons avec au moins la colonne "class"', placement = 'right', trigger = 'hover'),
        selectInput('sheet_variables', label = label.help('(3) Variables', 'sheet_variables_help'), sheets_list, selected = 3),
        bsTooltip('sheet_variables_help', title = 'Feuille de calcul comprenant la liste des variables avec une colonne "class" et "conc"', placement = 'right', trigger = 'hover')
      )
    )
  }) 
  
  
  
  ## choix du jeux de donnée (exemple ou personnalisé) [OK]
  dataset <- reactive({
    data <- input$dataset
    if (data == 'Aucun') {return(NULL)}
    if (data == 'Glucides (GC-FID)') {return(data.exemple1)}
    if (data == 'Acides aminés (UPLC-DAD)') {return(data.exemple2)}
    if (data == 'Importer un fichier' & !is.null(input$file_input)) {
      validate(
        need(input$sheet_datamatrix, "Importer un fichier"),
        need(input$sheet_samples, ""),
        need(input$sheet_variables, "")
      )
      data <- input$file_input
      if (!is.null(data)) {
        file.rename(data$datapath, paste0(data$datapath, '.xlsx'))
        return(list("datamatrix" = as.data.table(readxl::read_excel(paste0(data$datapath, '.xlsx'), sheet = as.numeric(input$sheet_datamatrix))),
                    "samplemetadata" = as.data.table(readxl::read_excel(paste0(data$datapath, '.xlsx'), sheet = as.numeric(input$sheet_samples))),
                    "variablemetadata" = as.data.table(readxl::read_excel(paste0(data$datapath, '.xlsx'), sheet = as.numeric(input$sheet_variables)))
        ))
      } else {return(NULL)}
    } else {return(NULL)}
  })
  
  #### Demonstration data download [OK]
  output$download_exemple1 <- downloadHandler(
    filename = 'Demo_Glucids.xlsx',
    content = function(file) {download.file('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfVmxtMm1kbG1hRzA', file, mode = 'wb')}
  )
  output$download_exemple2 <- downloadHandler(
    filename = 'Demo_Amino_acids.xlsx',
    content = function(file) {download.file('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfMmJjOHlKR3ZXOVk', file, mode = 'wb')}
  )
  
  #### Status check and ui generation [OK]
  output$progress_box <- renderUI({
    data <- dataset()
    status <- as.list(rep("primary", 7))
    
    status[[1]] <- ifelse(is.null(data), "warning", "success")
    if (status[[1]] == "success") {status[[1]] == "success"
      status[[2]] <- ifelse(any(duplicated(data[[1]][1]) | duplicated(data[[2]][1])) == TRUE, "danger", "success")
      status[[3]] <- ifelse(any(duplicated(names(data[[1]]))) == TRUE | any(duplicated(data[[3]][[1]])) == TRUE, "danger", "success")
      status[[4]] <- ifelse(!identical(data[[1]][,1], data[[2]][,1]), "danger", "success")
      status[[5]] <- ifelse(!identical(names(data[[1]])[-1], data[[3]][[1]]), "danger", "success")
      status[[6]] <- ifelse(!'class' %in% names(data[[2]]), "danger", "success")
      status[[7]] <- ifelse(!'class' %in% names(data[[3]]), "danger", ifelse(!'SI' %in% data[[3]][,class], "danger", "success"))
    }
    
    list(
      box(width = 12, height = 40, solidHeader = T, title = 'Import', status = status[[1]]),
      box(width = 12, height = 40, solidHeader = T, title = 'Duplicats échantillons', status = status[[2]]),
      box(width = 12, height = 40, solidHeader = T, title = 'Duplicats variables', status =  status[[3]]),
      box(width = 12, height = 40, solidHeader = T, title = 'lignes (1) = lignes (2)', status =  status[[4]]),
      box(width = 12, height = 40, solidHeader = T, title = 'colonnes (1) = lignes (3)', status =  status[[5]]),
      box(width = 12, height = 40, solidHeader = T, title = 'colonne "class" dans (2)', status =  status[[6]]),
      box(width = 12, height = 40, solidHeader = T, title = 'colonne "class" avec SI dans (3)', status =  status[[7]])
    )
  })
  
  #### Sidebar information
  output$sidebar_info <- renderUI({
    data <- dataset()
    sidebar_info_val <- as.list(rep(0, 6))
    
    if (!is.null(data)) {
      sidebar_info_val[[1]] <- dim(data[[1]])[1]
      sidebar_info_val[[2]] <- dim(data[[3]])[1]
      sidebar_info_val[[3]] <- ifelse(any(names(data[[2]]) == "batch") == FALSE, 1, length(unique(data[[2]][,batch])))
      sidebar_info_val[[4]] <- ifelse(any(names(data[[2]]) == "class") == FALSE, 0, data[[2]][class == "sample", .N])
      sidebar_info_val[[5]] <- ifelse(any(names(data[[2]]) == "class") == FALSE, 0, data[[2]][class == "standard", .N])
      sidebar_info_val[[6]] <- ifelse(any(names(data[[3]]) == "class") == FALSE, 0, 
                                      ifelse(!'SI' %in% data[[3]][,class], 0, as.character(data[[3]][class == "SI", 1])))
    }
    
    return(
      list(
        p(paste0("Echantillons (total) : ", sidebar_info_val[[1]])),
        p(paste0("Variables : ", sidebar_info_val[[2]])),
        p(paste0("Batchs : ", sidebar_info_val[[3]])),
        p(paste0("Echantillons : ", sidebar_info_val[[4]])),
        p(paste0("Standards externes : ", sidebar_info_val[[5]])),
        p(paste0("Standard Interne : ", sidebar_info_val[[6]]))
      )
    )
  })
  
  #### Afficher les tables
  
  
  
  
  #### Loading Screen too hide after all above is loaded
  #hide(id = "loading_screen", anim = TRUE, animType = "fade")
  #show(id = "main_content")
  ####
  
  ##########
})