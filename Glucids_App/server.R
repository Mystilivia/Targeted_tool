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




# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ################## Data importation ##################
  data.exemple1 <- readRDS('data/glucids.rds')
  data.exemple2 <- readRDS('data/aminoacids.rds')
  data.import <- reactive({
    validate(
      need(file_input_sheets(), "Sélectionnez les feuilles de calculs"),
      need(input$file_input, "Importer un fichier"),
      need(input$sheet_datamatrix, ""),
      need(input$sheet_samples, ""),
      need(input$sheet_variables, "")
    )
    data.file <- input$file_input
    file.rename(data.file$datapath, paste0(data.file$datapath, '.xlsx'))
    temp.list <- list("datamatrix" = setDT(readxl::read_excel(paste0(data.file$datapath, '.xlsx'), sheet = as.numeric(input$sheet_datamatrix))),
                      "samplemetadata" = setDT(readxl::read_excel(paste0(data.file$datapath, '.xlsx'), sheet = as.numeric(input$sheet_samples))),
                      "variablemetadata" = setDT(readxl::read_excel(paste0(data.file$datapath, '.xlsx'), sheet = as.numeric(input$sheet_variables)))
    )
    if (!'batch' %in% names(temp.list[[2]])) {temp.list[[2]] <- temp.list[[2]][,batch := 1]}
    setnames(temp.list[[1]], 1, 'SampleID')
    setnames(temp.list[[2]], 1, 'SampleID')
    setkeyv(temp.list[[1]], 'SampleID')
    setkeyv(temp.list[[2]], 'SampleID')
    return(temp.list)
  })
  ######
  file_input_sheets <- reactive({
    req(input$dataset)
    if (input$dataset != 'Importer un fichier') {return(NULL)}
    validate(need(!is.null(input$file_input), "Aucun fichier compatible chargé"))
    data <- input$file_input
    if (is.null(data)) {return(NULL)}
    file.rename(data$datapath, paste0(data$datapath, '.xlsx'))
    temp <- readxl::excel_sheets(paste0(data$datapath, '.xlsx'))
    return(setNames(as.list(1:length(temp)), temp))
  })
  ######
  dataset <- eventReactive(input$submit_data, {
    disable(id = 'submit_data')
    on.exit(enable(id = 'submit_data'))
    dataset_choice <- req(input$dataset)
    if (dataset_choice == 'Aucun') {return(NULL)}
    if (dataset_choice == 'Glucides (GC-FID)') {return(data.exemple1)}
    if (dataset_choice == 'Acides aminés (UPLC-DAD)') {return(data.exemple2)}
    if (dataset_choice == 'Importer un fichier') {data.imported <- req(data.import()) ; return(data.imported)}
    else {return(NULL)}
  }, ignoreNULL = F)
  
  ######################################################
  
  # #### Check dataset [OK]
  # dataset_input_check <- reactive({
  #   data <- dataset()
  #   status <- as.list(rep("primary", 7))
  #   status[[1]] <- ifelse(is.null(data), "warning", "success")
  #   if (status[[1]] == "success") {
  #     status[[2]] <- ifelse(any(duplicated(data[[1]][1]) | duplicated(data[[2]][1])) == TRUE, "danger", "success")
  #     status[[3]] <- ifelse(any(duplicated(names(data[[1]]))) == TRUE | any(duplicated(data[[3]][[1]])) == TRUE, "danger", "success")
  #     status[[4]] <- ifelse(!identical(data[[1]][,1], data[[2]][,1]), "danger", "success")
  #     status[[5]] <- ifelse(!identical(names(data[[1]])[-1], data[[3]][[1]]), "danger", "success")
  #     status[[6]] <- ifelse(!'class' %in% names(data[[2]]), "danger", "success")
  #     status[[7]] <- ifelse(!'class' %in% names(data[[3]]), "danger", ifelse(!'SI' %in% data[[3]][,class], "danger", "success"))
  #   }
  #   return(status)
  # })

   #### common check #### [DEV]
   dataset_checker <- reactive({
     data <- dataset()
     status_list <- rep(list(list('status' = "primary", 'value' = NULL, 'message' = NULL)), 14)
     
     if (is.null(data)) {
       status_list[[1]] <- list('status' = "warning", 'value' = NULL, 'message' = 'Aucune données compatible chargées.')
     } else {
       ## Importation ok
       status_list[[1]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)
       ## Check datamatrix as only values
       if (any(data[[1]][,-1][,lapply(.SD, is.numeric)] == F)) {
         status_list[[2]] <- list('status' = 'danger',
                                  'value' = names(data[[1]])[which(data[[1]][,lapply(.SD, is.numeric)] == F)],
                                  'message' = "Le feuillet 1 comprends du texte ou des caractères spéciaux. Vérifier l'absence de 'NA' 'Nan' 'nd' 'espaces' ','.")
       } else {status_list[[2]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)}
       ## Check duplicates in (1) or (2) or (3)
       if(any(duplicated(data[[1]][,1]) == T)) {
         status_list[[3]] <- list('status' = 'danger', 'value' = data[[1]][duplicated(),1][[1]], 'message' = "Certains échantillons sont dupliqués dans l'onglet (1)")
       } else {status_list[[3]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)}
       if(any(duplicated(data[[2]][,1]) == T)) {
         status_list[[4]] <- list('status' = 'danger', 'value' = data[[2]][duplicated(),1][[1]], 'message' = "Certains échantillons sont dupliqués dans l'onglet (2)")
       } else {status_list[[4]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)}
       if(any(duplicated(data[[3]][,1]) == T)) {
         status_list[[5]] <- list('status' = 'danger', 'value' = data[[3]][duplicated(),1][[1]], 'message' = "Certains échantillons sont dupliqués dans l'onglet (3)")
       } else {status_list[[5]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)}
       ## Check consistency in (1) (2) and (1) (3)
       if(!identical(data[[1]][,1], data[[2]][,1])) {
         status_list[[6]] <- list('status' = 'danger', 'value' = NULL, 'message' = "La première colonne de l'onglet (1) doit être identique à celle de l'onglet (2)")
       } else {status_list[[6]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)}
       if(any(names(data[[1]][,-1]) %in% data[[3]][,1][[1]] == F)) {
         status_list[[7]] <- list('status' = 'danger', 'value' = NULL, 'message' = "Les noms de variables de l'onglet (1) ne sont pas présent dans l'onglet (3)")
       } else {status_list[[7]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)
       if(!identical(data[[3]][,1][[1]], names(data[[1]][,-1]))) {
         status_list[[8]] <- list('status' = 'danger', 'value' = 0, 'message' = "Les noms de variables de l'onglet (1) ne sont pas dans le même ordre que dans l'onglet (3), ils ont été replacés dans l'ordre")
       } else {status_list[[8]] <- list('status' = 'success', 'value' = data[[3]][,.N], 'message' = NULL)}
       if(!'class' %in% names(data[[2]])) {
         status_list[[9]] <- list('status' = 'danger', 'value' = NULL, 'message' = "Il n'y a pas de colonne 'class' dans l'onglet (2)")
       } else {status_list[[9]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)
       if(!'standard' %in% data[[2]][,class]) {
         status_list[[10]] <- list('status' = 'danger', 'value' = 0, 'message' = "Il n'y a pas de 'standard' dans l'onglet (2)")
       } else {status_list[[10]] <- list('status' = 'success', 'value' = data[[2]][class == 'standard', .N], 'message' = NULL)
       if(!'sample' %in% data[[2]][,class]) {
         status_list[[11]] <- list('status' = 'danger', 'value' = 0, 'message' = "Il n'y a pas de 'sample' dans l'onglet (2)")
       } else {status_list[[11]] <- list('status' = 'success', 'value' = data[[2]][class == 'sample', .N], 'message' = NULL)}}}
       if(!'class' %in% names(data[[3]])) {
         status_list[[12]] <- list('status' = 'danger', 'value' = NULL, 'message' = "Il n'y a pas de colonne 'class' dans l'onglet (3)")
       } else {status_list[[12]] <- list('status' = 'success', 'value' = NULL, 'message' = NULL)
       if(!'SI' %in% data[[3]][,class]) {
         status_list[[13]] <- list('status' = 'warning', 'value' = 0, 'message' = "Il n'y a pas de 'SI' dans l'onglet (3)")
       } else {status_list[[13]] <- list('status' = 'success', 'value' = data[[3]][class == 'SI', 1][[1]], 'message' = NULL)
       if(!'batch' %in% names(data[[2]])) {
         status_list[[14]] <- list('status' = 'warning', 'value' = 0, 'message' = "Il n'y a pas de colonne 'batch' dans l'onglet (3)")
       } else {status_list[[14]] <- list('status' = 'success', 'value' = length(unique(data[[2]][, batch])), 'message' = NULL)}}}}}
     return(status_list)
   })
   
  # if checker ok, load data and set keys and variable order
  output$progress_box <- renderUI({
    status <- dataset_checker()
    return(list(
      column(width = 4,
             box(width = 12, height = 40, solidHeader = T, title = '[1]', status = status[[1]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[2]', status = status[[2]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[3]', status = status[[3]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[4]', status = status[[4]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[5]', status = status[[5]]$status)
             ),
      column(width = 4,
             box(width = 12, height = 40, solidHeader = T, title = '[6]', status = status[[6]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[7]', status = status[[7]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[8]', status = status[[8]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[9]', status = status[[9]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[10]', status = status[[10]]$status)
      ),
      column(width = 4,
             box(width = 12, height = 40, solidHeader = T, title = '[11]', status = status[[11]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[12]', status = status[[12]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[13]', status = status[[13]]$status),
             box(width = 12, height = 40, solidHeader = T, title = '[14]', status = status[[14]]$status)
      )
      ))
  })
  ######################
  #### Sidebar information [OK]
  output$sidebar_info <- renderUI({
    status <- dataset_checker()
    return(
      list(
        p(paste0("Variables : ", status[[8]]$value)),
        p(paste0("Batchs : ", status[[14]]$value)),
        p(paste0("Echantillons : ", status[[11]]$value)),
        p(paste0("Standards externes : ", status[[10]]$value)),
        p(paste0("Standard Interne : ", status[[13]]$value))
      )
    )
  })
  
  output$dataset_check <- renderUI({
    status <- dataset_checker()
    status <- as.data.table(do.call(rbind, status))
    if(any(status[,message] != "success")) {
      div(status[,message])
    } else {
      div("Tout semble ok !")
    }
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
    temp <- temp[,.(meanSI = mean(SI, na.rm = T), sdSI = sd(SI, na.rm = T), Variable = unique(Variable)), by = c('batch', 'class')]
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
      temp.datamatrix <- merge(data[[2]][,.(SampleID, batch, class)], data[[1]], by.x = 'SampleID', by.y = 'SampleID')
      batch.list <- split(temp.datamatrix, by = c('batch', 'class'), flatten = F)
      temp.std <- lapply(batch.list, function(x) {t(x$standard[,-c(1:3)][, lapply(.SD, function(x) {mean(x, na.rm = T)})])})
      temp.respF <- lapply(temp.std, function(x) {x/data[[3]][,conc]})
      temp.list.val <- mapply(function(x,y) {lapply(x, function(z) rbind(t(z[,1:3]), t(z[,-c(1:3)])/y[,1]))}, batch.list, temp.respF, SIMPLIFY = F)
      temp.list.val <- lapply(temp.list.val, function(x) {lapply(x, t)})
      temp.datamatrix.amount <- as.data.table(do.call(rbind, lapply(temp.list.val, function(x) do.call(rbind, x))), keep.rownames = F)[,-c('batch', 'class')]
      #temp.datamatrix.amount <- as.data.table(lapply(temp.list.val, function(x) do.call(rbind, x)), keep.rownames = F)
      #divide by ms and calculate amount in extraction volume
      temp.datamatrix.amount <- temp.datamatrix.amount[,SampleID := as.character(SampleID)]
      data[[2]] <- data[[2]][,SampleID := as.character(SampleID)]
      
      setkeyv(temp.datamatrix.amount, "SampleID")
      setkeyv(data[[2]], "SampleID")
      
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
  
  # output$dataset_check <- renderUI({
  #   data <- dataset()
  #   if (is.null(data)) {return(p("Importer un fichier"))}
  #   if (any(apply(data[[1]][,-1], 2, is.numeric)) == F) {
  #     return(
  #       list(div("Présence de texte dans le feuillet (1)", style = "color:red"),
  #            div("Vérifier l'abscence de NA, nd et autre caractères", style = "color:red"))
  #     )}
  #   return(div("Tout semble OK", style = "color:green"))
  # })
  
  #### Demonstration data download [OK]
  output$download_exemple1 <- downloadHandler(
    filename = 'Demo_Glucids.xlsx',
    content = function(file) {download.file('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfVmxtMm1kbG1hRzA', file, mode = 'wb')}
  )
  output$download_exemple2 <- downloadHandler(
    filename = 'Demo_Amino_acids.xlsx',
    content = function(file) {download.file('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfMmJjOHlKR3ZXOVk', file, mode = 'wb')}
  )
  
####################### END #######################
})
