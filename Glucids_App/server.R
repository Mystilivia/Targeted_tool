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

#### Loading data from google drive [SERVER][LOCAL]
# data.exemple1 <- list('datamatrix' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfSUJVZTczWm9fUmM'),
#                       'samplemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfNlN1cVZzYzh6aFk'),
#                       'variablemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfTlB3eUJwNXE0Szg'))
# 
# data.exemple2 <- list('datamatrix' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfbkRzcGtQYTZGc00'),
#                       'samplemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfcWdoNWlid2d2WHc'),
#                       'variablemetadata' = fread('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfbi0zZ0txM2daQ1U'))

#### Loading data from local file [SERVER]
# data.exemple1 <- readRDS('data/glucids.rds')
# data.exemple2 <- readRDS('data/aminoacids.rds')

#### Loading data from local file [LOCAL]
#data.exemple1 <- readRDS('./Glucids_App/data/glucids.rds')
#data.exemple2 <- readRDS('./Glucids_Appdata/aminoacids.rds')

## From excel file
#data.exemple1 <- list('datamatrix' = as.data.table(read_excel('C:/Users/sdechaumet/Google Drive/Perso/Programming/R/03_Development/Targeted_tool/Data/Amino_acids/Aminoacids_vf.xlsx', sheet = 1, na = "")),
#                      'samplemetadata' = as.data.table(read_excel('C:/Users/sdechaumet/Google Drive/Perso/Programming/R/03_Development/Targeted_tool/Data/Amino_acids/Aminoacids_vf.xlsx', sheet = 2, na = "")),
#                      'variablemetadata' = as.data.table(read_excel('C:/Users/sdechaumet/Google Drive/Perso/Programming/R/03_Development/Targeted_tool/Data/Amino_acids/Aminoacids_vf.xlsx', sheet = 3, na = "")))
#



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  data.exemple1 <- readRDS('data/glucids.rds')
  data.exemple2 <- readRDS('data/aminoacids.rds')
  
  #### Importation des données [OK]
  file_input_sheets <- reactive({
    req(input$dataset)
    if(input$dataset != 'Importer un fichier'){return(NULL)}
    validate(need(!is.null(input$file_input), "Aucun fichier compatible chargé"))
    data <- input$file_input
    if (is.null(data)) {return(NULL)}
    file.rename(data$datapath, paste0(data$datapath, '.xlsx'))
    temp <- readxl::excel_sheets(paste0(data$datapath, '.xlsx'))
    return(setNames(as.list(1:length(temp)), temp))
  })

  ## choix du jeux de donnée (exemple ou personnalisé) [OK]
  dataset <- eventReactive(input$submit_data, {
    disable(id = 'submit_data')
    on.exit(enable(id = 'submit_data'))
    dataset_choice <- req(input$dataset)
    if (dataset_choice == 'Aucun') {return(NULL)}
    if (dataset_choice == 'Glucides (GC-FID)') {return(data.exemple1)}
    if (dataset_choice == 'Acides aminés (UPLC-DAD)') {return(data.exemple2)}
    if (dataset_choice == 'Importer un fichier' & !is.null(input$file_input)) {
      validate(
        need(input$sheet_datamatrix, "Importer un fichier"),
        need(input$sheet_samples, ""),
        need(input$sheet_variables, "")
      )
      data <- input$file_input
      if (!is.null(data)) {
        file.rename(data$datapath, paste0(data$datapath, '.xlsx'))
        
        temp.list <- list("datamatrix" = as.data.table(readxl::read_excel(paste0(data$datapath, '.xlsx'), sheet = as.numeric(input$sheet_datamatrix))),
             "samplemetadata" = as.data.table(readxl::read_excel(paste0(data$datapath, '.xlsx'), sheet = as.numeric(input$sheet_samples))),
             "variablemetadata" = as.data.table(readxl::read_excel(paste0(data$datapath, '.xlsx'), sheet = as.numeric(input$sheet_variables)))
        )
        if (!'batch' %in% names(temp.list[[2]])) {temp.list[[2]] <- temp.list[[2]][,batch := 1]}
        return(temp.list)
      } else {return(NULL)}
    } else {return(NULL)}
  }, ignoreNULL = F)
  
  ################
  
  #### Check dataset [OK]
  dataset_input_check <- reactive({
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
    return(status)
  })
  
  #### Extract info from dataset [OK]
  dataset_input_val <- reactive({
    data <- dataset()
    sidebar_info_val <- as.list(rep(0, 6))
    if (!is.null(data)) {
      sidebar_info_val[[1]] <- dim(data[[1]])[1]
      sidebar_info_val[[2]] <- dim(data[[3]])[1]
      sidebar_info_val[[3]] <- ifelse(any(names(data[[2]]) == "batch") == FALSE, 0, length(unique(data[[2]][,batch])))
      sidebar_info_val[[4]] <- ifelse(any(names(data[[2]]) == "class") == FALSE, 0, data[[2]][class == "sample", .N])
      sidebar_info_val[[5]] <- ifelse(any(names(data[[2]]) == "class") == FALSE, 0, data[[2]][class == "standard", .N])
      sidebar_info_val[[6]] <- ifelse(any(names(data[[3]]) == "class") == FALSE, 0, 
                                      ifelse(!'SI' %in% data[[3]][,class], 0, as.character(data[[3]][class == "SI", 1])))
    }
    return(sidebar_info_val)
  })
  
  ########### CALCULATION TAB ###########
  #### Plot SI in raw datas with error bar by batch and sample class
  SI_raw_data <- reactive({
    data <- dataset()
    if(is.null(data)) {return(NULL)}
    datamatrix.splid <- names(data[[1]])[1]
    samplemetadata.splid <- names(data[[2]])[1]
    SI_val <- as.character(data[[3]][class == 'SI', 1])
    temp <- merge(data[[2]][,.('SampleID' = get(samplemetadata.splid), class, batch)], data[[1]][,.('SampleID' = get(datamatrix.splid), 'SI' = get(SI_val), 'Variable' = paste0(SI_val))], by.x = datamatrix.splid, by.y = samplemetadata.splid)
    temp <- temp[,.(meanSI = mean(SI, na.rm = T), sdSI = sd(SI, na.rm = T), Variable), by = c('batch', 'class')]
    return(temp)
  })

  output$SI_raw_plot <- renderPlot({
    temp <- SI_raw_data()
    if (is.null(SI_raw_data)) {return(NULL)}
    ggplot(temp, aes(as.factor(batch), meanSI, fill = class, ymin = meanSI-sdSI, ymax = meanSI+sdSI)) +
      geom_bar(stat = "identity", position = position_dodge(width=0.9), color = 'black') +
      geom_errorbar(position = position_dodge(width=0.9), width = 0.5) +
      theme_bw() +
      labs(title = paste0("Moyenne et écarts-types des valeurs brutes du SI (", unique(temp[,Variable]), ")"), x = 'batch', y = '', fill = 'Classe')
  })
  
  #### Calculate content in samples with extraction volume, SI concentration and unit
  data_conc <- eventReactive(input$submit_data_calc, {
    disable(id = 'submit_data_calc')
    on.exit(enable(id = 'submit_data_calc'))
      data <- dataset()
      if(is.null(data)) {return(NULL)}
      datamatrix.splid <- names(data[[1]])[1]
      samplemetadata.splid <- names(data[[2]])[1]
      SI_val <- as.character(data[[3]][class == 'SI', 1])
      ## calculate sample amount
      temp.datamatrix <- data[[1]]
      batch.list <- split(temp.datamatrix, data[[2]][,.(batch)])
      batch.class.list <- lapply(batch.list, function(x) {split(x, data[[2]][,.(class)])})
      temp.std <- lapply(batch.class.list, function(x) {t(x$standard[,-1][, lapply(.SD, function(x) {mean(x, na.rm = T)})])})
      temp.respF <- lapply(temp.std, function(x) {x/data[[3]][,conc]})
      temp.list.val <- mapply(function(x,y) {lapply(x, function(z) rbind(t(z)[1,], t(z)[-1,]/y[,1]))}, batch.class.list, temp.respF, SIMPLIFY = F)
      temp.list.val <- lapply(temp.list.val, function(x) {lapply(x, t)})
      temp.datamatrix.amount <- as.data.table(do.call(rbind, lapply(temp.list.val, function(x) do.call(rbind, x))), keep.rownames = F)
      setnames(temp.datamatrix.amount, "V1", "SampleID")
      # divide by ms and calculate amount in extraction volume
      setkeyv(temp.datamatrix.amount, "SampleID")
      setkeyv(data[[2]], samplemetadata.splid)
      validate(need(identical(temp.datamatrix.amount[,1], data[[2]][,1]), "Problème dans la fonction 'data_conc', contacter le développeur."),
               need('MS' %in% names(data[[2]]), "Il faut définir la colonne à utiliser pour la MS/MF"),
               need(!is.null(input$vol_extraction), "Entrer un volume d'extraction"),
               need(!is.numeric(input$vol_extraction), "Entrer un chiffre pour le volume d'extraction"),
               need(!is.null(input$dilution_fac), "Entrer un facteur de dilution"),
               need(!is.numeric(input$dilution_fac), "Entrer un chiffre pour le facteur de dilution"))
      
      temp.datamatrix <- cbind(temp.datamatrix.amount[,1], (((temp.datamatrix.amount[,-1]/data[[2]][,MS])*1000/input$vol_extraction)/input$dilution_fac))
      return(list('datamatrix' = temp.datamatrix,
                  'samplemetadata' = data[[2]],
                  'variablemetadata' = data[[3]]))
      
      # conc_X.Ec3 <- (((Resp_X.Ec)*(Conc_X.St)) / (Resp_X.St))
      # conc_X.Ec3 <- conc_X.Ec3 / ((Resp_IS.Ec/Conc_IS.Ec)/(Resp_IS.St/Conc_IS.St))
  })
  
  output$data_calc <- renderPlot({
    temp <- data_conc()
    if (is.null(temp)) {return(NULL)}
    metadata.spleid <- names(temp[[2]])[1]
    datamatrix.spleid <- names(temp[[1]])[1]
    temp.plot <- merge(temp[[2]], temp[[1]], by.x = metadata.spleid, by.y = datamatrix.spleid)
    temp.plot <- melt(temp.plot, id.vars = names(temp[[2]]))[,.('Mean' = mean(value, na.rm = T)), by = c('class', 'batch', 'variable')]
    
    return(
      ggplot(temp.plot, aes(variable, Mean, fill = class)) +
      geom_bar(stat = 'identity', position = position_dodge(width=0.9), color = 'black') +
      facet_grid(.~batch) +
      theme_bw() +
      coord_flip()
      )
  })
  
  #### Calculate SI deviation in samples (or show SI levels in barplot)
  #### Correct SI by batch
  
  
  
  ########### CORRECTIONS TAB ########### SI correction ; opt: inter and intra-batch normalization for each compounds using QCs or STD

  
  
  ############ ANALYSES TAB ############
  ## Interactive PCA
  ## Interactive boxplot (choose one or multiple compoundand biological factor)
  ## Propose PLSDA opt: OPLS ; VIP threshold and boxplot associated
  
  
  ############################### OUTPUT ###############################
  
  #### Choix des onglets [OK]
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
  
  output$dataset_check <- renderUI({
    data <- dataset()
    if (is.null(data)) {return(p("Importer un fichier"))}
    if (any(apply(data[[1]][,-1], 2, is.numeric)) == F) {
      return(
        list(div("Présence de texte dans le feuillet (1)", style = "color:red"),
             div("Vérifier l'abscence de NA, nd et autre caractères", style = "color:red"))
      )
    }
    return(div("Tout semble OK", style = "color:green"))
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

  output$progress_box <- renderUI({
    status <- dataset_input_check()
    return(list(
             box(width = 12, height = 40, solidHeader = T, title = 'Import', status = status[[1]]),
             box(width = 12, height = 40, solidHeader = T, title = 'Duplicats échantillons', status = status[[2]]),
             box(width = 12, height = 40, solidHeader = T, title = 'Duplicats variables', status =  status[[3]]),
             box(width = 12, height = 40, solidHeader = T, title = 'lignes (1) = lignes (2)', status =  status[[4]]),
             box(width = 12, height = 40, solidHeader = T, title = 'colonnes (1) = lignes (3)', status =  status[[5]]),
             box(width = 12, height = 40, solidHeader = T, title = 'colonne "class" dans (2)', status =  status[[6]]),
             box(width = 12, height = 40, solidHeader = T, title = 'colonne "class" avec SI dans (3)', status =  status[[7]])
    ))
  })
  
  #### Sidebar information [OK]
  output$sidebar_info <- renderUI({
    sidebar_info_val <- dataset_input_val()
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
  
####################### END #######################
})
