#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
source('Helper.R')

shinyServer(function(input, output, session) {
  ################## Data importation ##################
  shinyjs::disable('submit_data_calc')
  shinyjs::disable('download_data_norm')
  shinyjs::disable('submit_data_correction')
  
  data.import <- reactive({
    validate(
      need(file_input_sheets(), "Sélectionnez les feuilles de calculs"),
      need(input$file_input, "Importer un fichier"),
      need(input$sheet_datamatrix, ""),
      need(input$sheet_samples, ""),
      need(input$sheet_variables, "")
    )
    cat(file=stderr(), "data.import : Get file", "\n")
    data.file <- input$file_input
    cat(file=stderr(), "data.import : Rename File", "\n")
    file.rename(data.file$datapath, paste0(data.file$datapath, '.xlsx'))
    cat(file=stderr(), "data.import : Read files", "\n")
    temp.list <- list("datamatrix" = setDT(readxl::read_excel(paste0(data.file$datapath, '.xlsx'), sheet = as.numeric(input$sheet_datamatrix))),
                      "samplemetadata" = setDT(readxl::read_excel(paste0(data.file$datapath, '.xlsx'), sheet = as.numeric(input$sheet_samples))),
                      "variablemetadata" = setDT(readxl::read_excel(paste0(data.file$datapath, '.xlsx'), sheet = as.numeric(input$sheet_variables)))
    )
    if (!'batch' %in% names(temp.list[[2]])) {temp.list[[2]] <- temp.list[[2]][,batch := 1]}
    return(temp.list)
  })
  ###############################################
  file_input_sheets <- reactive({
    req(input$dataset)
    if (input$dataset != 'Importer un fichier') {return(NULL)}
    validate(need(!is.null(input$file_input), ""))
    data <- input$file_input
    if (is.null(data)) {return(NULL)}
    file.rename(data$datapath, paste0(data$datapath, '.xlsx'))
    temp <- readxl::excel_sheets(paste0(data$datapath, '.xlsx'))
    return(setNames(as.list(1:length(temp)), temp))
  })
  ###############################################
  dataset <- eventReactive(input$submit_data, {
    disable(id = 'submit_data')
    on.exit(enable(id = 'submit_data'))
    dataset_choice <- req(input$dataset)
    if (dataset_choice == 'Aucun') {return(NULL)}
    if (dataset_choice == 'Glucides (GC-FID)') {return(test.data.gc())}
    if (dataset_choice == 'Acides aminés (UPLC-DAD)') {return(test.data.aa())}
    if (dataset_choice == 'Importer un fichier') {data.imported <- req(data.import()) ; return(data.imported)}
    else {return(NULL)}
  }, ignoreNULL = F)
  ###############################################
  dataset_keyed <- reactive({
    data <- req(dataset())
    status <- req(dataset_checker())
    if (all(as.data.table(do.call(rbind,status))[,status] == 'success')) {
      setnames(data[[1]], 1, 'SampleID')
      setnames(data[[2]], 1, 'SampleID')
      setnames(data[[3]], 1, 'VarID')
      setkeyv(data[[1]], 'SampleID')
      setkeyv(data[[2]], 'SampleID')
      setkeyv(data[[3]], 'VarID')
      setcolorder(data[[1]], c('SampleID', data[[3]][,VarID]))
      Mass_col_pre <- ifelse('MS' %in% names(data[[2]]), 'MS',
                             ifelse('MF' %in% names(data[[2]]), 'MF', names(data[[2]])[1]))
      updateSelectizeInput(session, 'Mass_col', label = NULL, choices = names(data[[2]]), selected = Mass_col_pre, server = T)
      return(data)
    } else {return(NULL)}
  })
  ######################################################
   #### common check #### [OK]
   dataset_checker <- reactive({
     data <- dataset()
     return(check_dlist(data))
   })
  #### Sidebar information [OK]
  output$sidebar_info <- renderUI({
    status <- dataset_checker() 
    return(
      list(
        p(paste0("Variables : ", status[[8]]$value)),
        p(paste0("Batchs : ", status[[14]]$value)),
        p(paste0("Echantillons : ", status[[11]]$value)),
        p(paste0("QC : ", status[[15]]$value)),
        p(paste0("Blancs : ", status[[16]]$value)),
        p(paste0("Standards externes : ", status[[10]]$value)),
        p(paste0("Standard Interne : ", status[[13]]$value))
      )
    )
  })
  
  output$dataset_check <- renderUI({
    status <- dataset_checker()
    status <- as.data.table(do.call(rbind, status))
    if(any(!status[,status] %in% c("success", "primary"))) {
      return(
        HTML(
          c("<font color = 'blue'> Warnings :", paste0(c("<br/>", status[status == "warning", message]), collapse = "<br/>")),
          c("<br/> <font color = 'red'> Danger :", paste0(c("<br/>", status[status == "danger", message]), collapse = "<br/>"))
        )
      )
    } else {
      temp <- dataset_keyed()
      shinyjs::enable('submit_data_calc')
      return(HTML("<font color = 'green'> Tout semble ok !"))
              }
  })
  ########### CALCULATION TAB ###########
  #### Plot SI in raw datas with error bar by batch and sample class
  SI_raw_data <- reactive({
    data <- dataset_keyed()
    if(is.null(data)) {return(NULL)}
    SI_val <- as.character(data[[3]][class == 'SI', 1])
    temp <- merge(data[[2]][,.(SampleID, class, batch)], data[[1]][,.(SampleID, 'SI' = get(SI_val), 'Variable' = paste0(SI_val))], by = 'SampleID')
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
  data_conc2 <- eventReactive(input$submit_data_calc, {
    disable(id = 'submit_data_calc')
    on.exit(enable('submit_data_correction'))
    
    cat(file=stderr(), "data_conc2 : Validation", "\n")
    data <- req(dataset_keyed())
    conc_SI <- as.numeric(req(input$conc_SI))
    dilution_vac <- as.numeric(req(input$dilution_fac))
    isolate({
      cat(file=stderr(), "data_conc2 : Starting calculation 1", "\n")
      data_calc_step1 <- data_DT(data_Rdmt(data, conc_SI, dilution_vac))
      cat(file=stderr(), "data_conc2 : Starting calculation Validation step", "\n")
      validate(need(identical(data_calc_step1[[1]][,1], data_calc_step1[[2]][,1]), "Problème dans la fonction 'data_conc2', contacter le développeur."),
               need(!is.null(input$vol_extraction), "Entrer un volume d'extraction"),
               need(!is.null(input$dilution_fac), "Entrer un facteur de dilution"),
               need(!is.null(input$Mass_col), "Choisissez une colonne contenant les masses de départ"),
               need(is.numeric(data_calc_step1[[2]][,get(as.character(req(input$Mass_col)))]), paste0("La colonne : ", input$Mass_col, " doit contenir des chiffres"))
               )
      cat(file=stderr(), "data_conc2 : Requirement 2", "\n")
      Col_sel <- names(data_calc_step1[[1]])[-1]
      Fact_corr <- req(as.numeric(input$vol_extraction)/1000*req(as.numeric(input$dilution_fac)))
      Mass_sel <- as.character(req(input$Mass_col))
      cat(file=stderr(), "data_conc2 : Starting calculation 2", "\n")
      data_calc_step1[[1]][, (Col_sel) := lapply(.SD, function(x) {x/data_calc_step1[[2]][,get(Mass_sel)]*Fact_corr}), .SDcols = Col_sel]
      cat(file=stderr(), "data_conc2 : Calculation finished", "\n")
      enable(id = 'submit_data_calc')
      return(data_calc_step1)
      })
    enable(id = 'submit_data_calc')
  })
  
  #### Correct SI
  data_SI_corrected <- eventReactive(input$submit_data_correction, {
    disable(id = 'submit_data_correction')
    on.exit(enable(id = 'submit_data_correction'))
    shinyjs::enable('download_data_norm')
    cat(file=stderr(), "data_SI_corrected : Initialize", "\n")
    data_conc2 <- req(data_conc2())
    return(data_SI(data_conc2))
  })
  
  #######################################
  
  ########### SI stability in samples + overall CV [DEV]
  data_conc_raw_plot <- reactive({
    cat(file=stderr(), "data_conc_raw_plot : Requirement", "\n")
    temp.data <- req(data_conc2())
    mass_unit_val <- req(input$unit_Mass)
    SE_unit_val <- req(input$unit_SI)
    cat(file=stderr(), "data_conc_raw_plot : Data formating", "\n")
    temp.plot.data <- melt(merge(temp.data[[2]], temp.data[[1]], by = 'SampleID'), id.vars = 1:dim(temp.data[[2]])[2])
    SI_val <- req(dataset_checker()[[13]]$value)
    cat(file=stderr(), "data_conc_raw_plot : Finished", "\n")
    return(list('data.plot' = temp.plot.data,
                'mass_unit_val' = mass_unit_val,
                'SE_unit_val' = SE_unit_val,
                'SI_val' = SI_val))
  })
  
  data_conc_norm_plot <- reactive({
    cat(file=stderr(), "data_conc_norm_plot : Requirement", "\n")
    temp.data <- req(data_SI_corrected())
    mass_unit_val <- req(input$unit_Mass)
    SE_unit_val <- req(input$unit_SI)
    cat(file=stderr(), "data_conc_norm_plot : Data formating", "\n")
    temp.plot.data <- melt(merge(temp.data[[2]], temp.data[[1]], by = 'SampleID'), id.vars = 1:dim(temp.data[[2]])[2])
    SI_val <- req(dataset_checker()[[13]]$value)
    cat(file=stderr(), "data_conc_norm_plot : Finished", "\n")
    return(list('data.plot' = temp.plot.data,
                'mass_unit_val' = mass_unit_val,
                'SE_unit_val' = SE_unit_val,
                'SI_val' = SI_val))
  })
  
  ###### PLOT SI
  output$data_calc <- renderPlotly({
    cat(file=stderr(), "data_calc : Requirement", "\n")
    samples_selection <- req(input$sample_choice_1)
    temp.plot <- req(data_conc_raw_plot())
    cat(file=stderr(), "data_calc : subsetting", "\n")
    temp.plot.sub <- temp.plot$data.plot[SampleID %in% samples_selection]
    label_title <- "Teneurs dans les échantillons"
    cat(file=stderr(), "data_calc : plot", "\n")
    return(
     ggplotly(ggplot(temp.plot.sub, aes(variable, value, fill = as.factor(SampleID), label1 = SampleID)) +
                 geom_bar(stat = 'identity', position = 'dodge') +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0)) +
                 coord_flip() +
                 labs(title = label_title, subtitle = "Données non-normalisées", x = "", y = paste0("en ", temp.plot$SE_unit_val, " / ", temp.plot$mass_unit_val), fill = "ID"),
        tooltip = c("label1", "variable", "value"),
        height = 600
        ) %>% config(displayModeBar = F)
    )
    cat(file=stderr(), "data_calc : Finished", "\n")
  })
  
  ###### PLOT PROFILES
  output$data_calc_Rmdt_plot <- renderPlotly({
    cat(file=stderr(), "data_calc_Rmdt_plot : Requirement", "\n")
    temp.plot <- req(data_conc_raw_plot())
    batch_selection <- req(input$batch_choice_1)
      cat(file=stderr(), "data_calc_Rmdt_plot : Subsetting", "\n")
      temp.plot.sub <- temp.plot$data.plot[class == 'sample' & variable %in% temp.plot$SI_val & batch %in% batch_selection]
      ## Outliers using 2 * sd from mean... sensible to outliers
      #temp.plot.sub[,outliers := ifelse(abs(Rdmt-mean(Rdmt, na.rm = T)) > 2*sd(Rdmt, na.rm = T), "outliers", ""), by = batch]
      ## Outliers : using MAD from : http://www.sciencedirect.com/science/article/pii/S0022103113000668
      temp.plot.sub[, outliers := ifelse(Rdmt > median(Rdmt, na.rm = T)+2.5*mad(Rdmt, na.rm = T) | Rdmt < median(Rdmt, na.rm = T)-2.5*mad(Rdmt, na.rm = T), "outliers", ""), by = batch]
      #temp.plot.sub <- temp.plot.sub[, SampleID := as.factor(SampleID)][, batch := as.factor(batch)]
      title_label <- paste0("Rendements d'extraction")
      cat(file=stderr(), "data_calc_Rmdt_plot : Plot", "\n")
      return(
        ggplotly(ggplot(temp.plot.sub, aes(reorder(SampleID, as.numeric(batch)), Rdmt, fill = as.factor(batch), label1 = SampleID, alpha = outliers)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          scale_alpha_discrete("outliers", range = c(0.4,1)) +
          coord_flip() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0), strip.background = element_blank(), strip.text.x = element_blank()) +
          labs(title = title_label, x = "", y = "", fill = "batchs"),
        tooltip = c("label1", "Rdmt"),
        height = 600) %>% config(displayModeBar = F)
      )
      cat(file=stderr(), "data_calc_Rmdt_plot : Finished", "\n")
  })
  
  #### Calculate SI deviation in samples (or show SI levels in barplot)
  #### Correct SI by batch

  output$data_calc_norm_plot <- renderPlotly({
    cat(file=stderr(), "data_calc_norm_plot : Requirement", "\n")
    samples_selection <- req(input$sample_choice_2)
    temp.plot <- req(data_conc_norm_plot())
    cat(file=stderr(), "data_calc_norm_plot : subsetting", "\n")
    temp.plot.sub <- temp.plot$data.plot[SampleID %in% samples_selection]
    label_title <- "Teneurs dans les échantillons"
    cat(file=stderr(), "data_calc_norm_plot : plot", "\n")
    return(
      ggplotly(ggplot(temp.plot.sub, aes(variable, value, fill = as.factor(SampleID), label1 = SampleID)) +
                 geom_bar(stat = 'identity', position = 'dodge') +
                 theme_bw() +
                 theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0)) +
                 coord_flip() +
                 labs(title = label_title, subtitle = "Données normalisées", x = "", y = paste0("en ", temp.plot$SE_unit_val, " / ", temp.plot$mass_unit_val), fill = "ID"),
               tooltip = c("label1", "variable", "value"),
               height = 600
      ) %>% config(displayModeBar = F)
    )
    cat(file=stderr(), "data_calc_norm_plot : Finished", "\n")
  })
  
  ########### CORRECTIONS TAB ########### SI correction ; opt: inter and intra-batch normalization for each compounds using QCs or STD
  
  
  ############ ANALYSES TAB ############
  ## Interactive PCA
  ## Interactive boxplot (choose one or multiple compoundand biological factor)
  ## Propose PLSDA opt: OPLS ; VIP threshold and boxplot associated
  
  
  ############################### OUTPUT ###############################
  observe({
    data_raw_calc <- req(data_conc2())
    updateSelectizeInput(session, 'sample_choice_1', choices = data_raw_calc[[2]][class != 'standard'], selected = data_raw_calc[[2]][class == "sample", SampleID][1], server = T, options = list(
      placeholder = 'Choisissez un ou plusieurs échantillons',
      valueField = 'SampleID',
      labelField = 'SampleID',
      searchField = names(data_raw_calc[[2]]),
      render = I("{
      option: function(item, escape) {
                  return '<div>' +
                  escape(item.SampleID) + ', ' + escape(item.class) +
                  '</div>';  
                  }
                  }")
    ))
    updateSelectizeInput(session, 'batch_choice_1', choices = data_raw_calc[[2]][class != 'standard', batch], selected = data_raw_calc[[2]][class == "sample", batch], server = T)
  })
  
  observe({
    data_SI_calc <- req(data_SI_corrected())
    render.text <- paste0("{ option: function(item, escape) { return '<div>' + ", paste(lapply(names(data_SI_calc[[2]])[1:length(names(data_SI_calc[[2]]))-1], function(x) {paste0("escape(item.", x, ")")}), collapse = " + ', ' + "), " + '</div>';   } }")
    
    updateSelectizeInput(session, 'sample_choice_2', choices = data_SI_calc[[2]][class != 'standard'], selected = data_SI_calc[[2]][class == "sample", SampleID][1], server = T, options = list(
      placeholder = 'Choisissez un ou plusieurs échantillons',
      valueField = 'SampleID',
      labelField = 'SampleID',
      searchField = names(data_SI_calc[[2]]),
      render = I(render.text)
      # render = I("{
      # option: function(item, escape) {
      #             return '<div>' +
      #             escape(item.SampleID) + ', ' + escape(item.class) +
      #             '</div>';  
      #             }
      #             }")
    ))
  })
  
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
  
  #### Demonstration data download [OK]
  output$download_exemple1 <- downloadHandler(
    filename = 'Demo_Glucids.xlsx',
    # content = function(file) {download.file('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfVmxtMm1kbG1hRzA', file, mode = 'wb')}
    content = function(file) {
      temp <- req(test.data.gc())
      wb = createWorkbook()
      sheet1 = createSheet(wb, "datamatrix")
      addDataFrame(temp[[1]], sheet = sheet1, row.names = F)
      sheet2 = createSheet(wb, "samplemetadata")
      addDataFrame(temp[[2]], sheet = sheet2, row.names = F)
      sheet3 = createSheet(wb, "variablemetadata")
      addDataFrame(temp[[3]], sheet = sheet3, row.names = F)
      saveWorkbook(wb, file = file)
    }
  )
  
  output$download_exemple2 <- downloadHandler(
    filename = 'Demo_Amino_acids.xlsx',
    #content = function(file) {download.file('https://drive.google.com/uc?export=download&id=0BzRPQoqAbZxfMmJjOHlKR3ZXOVk', file, mode = 'wb')}
    content = function(file) {
      temp <- req(test.data.aa())
      wb = createWorkbook()
      sheet1 = createSheet(wb, "datamatrix")
      addDataFrame(temp[[1]], sheet = sheet1, row.names = F)
      sheet2 = createSheet(wb, "samplemetadata")
      addDataFrame(temp[[2]], sheet = sheet2, row.names = F)
      sheet3 = createSheet(wb, "variablemetadata")
      addDataFrame(temp[[3]], sheet = sheet3, row.names = F)
      saveWorkbook(wb, file = file)
    }
  )
  
  output$download_data_norm <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y.%m.%d_%H.%M_Resultats.xlsx"))
    },
    content = function(file) {
      temp <- req(data_SI_corrected())
      wb = createWorkbook()
      sheet1 = createSheet(wb, "datamatrix")
      addDataFrame(temp[[1]], sheet = sheet1, row.names = F)
      sheet2 = createSheet(wb, "samplemetadata")
      addDataFrame(temp[[2]], sheet = sheet2, row.names = F)
      sheet3 = createSheet(wb, "variablemetadata")
      addDataFrame(temp[[3]], sheet = sheet3, row.names = F)
      saveWorkbook(wb, file = file)
    }
  )
  
####################### END #######################
})
