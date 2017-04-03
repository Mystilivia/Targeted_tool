## install package if not installed
install_fonction <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

## install_fonction
install_fonction(c("shiny",
                   "shinyjs",
                   "magrittr",
                   "DT",
                   "shinydashboard",
                   "shinyBS",
                   "data.table",
                   "ggplot2",
                   "readxl",
                   "xlsx",
                   "markdown",
                   "curl",
                   "plotly",
                   "openssl",
                   "tidyr"))

############ DATASET ############
## Dataset ##
test.data.aa <- function() {
  datamatrix <- rbind(data.table('SampleID' = paste0("Sample_", 1:40),
             'BABA'  = rnorm(40, 100, 2),
             'asp'   = rnorm(40, 10,2),
             'glu'   = rnorm(40, c(100,50), 2),
             'asn'   = rnorm(40, c(10,50), 5),
             'ser'   = rnorm(40, 20, 1),
             'SMCSO' = rnorm(40, 60, 1),
             'gln'   = rnorm(40, c(100,60), 5),
             'phe'   = rnorm(40, 5, 1),
             'ile'   = rnorm(40, 10, 2),
             'leu'   = rnorm(40, 20, 3),
             'pro'   = rnorm(40, c(200,60), 4),
             'trp'   = rnorm(40, 40, 1),
             'tyr'   = rnorm(40, 30, 2),
             'val'   = rnorm(40, 10, 1),
             'lys'   = rnorm(40, 20, 1),
             'arg'   = rnorm(40, c(40,30), 1)),
  as.list(c('Std_1a', rnorm(16, 50, 2))),
  as.list(c('Std_2a', rnorm(16, 50, 2))),
  as.list(c('Std_1b', rnorm(16, 50, 2))),
  as.list(c('Std_2b', rnorm(16, 50, 2))))
  datamatrix[, (names(datamatrix)[-1]) := lapply(.SD, as.numeric), .SDcols = names(datamatrix)[-1]]
  datamatrix[, (names(datamatrix)[-1]) := lapply(.SD, function(x) {x/c(rnorm(40, 1, 0.1), rep(1, 4))}), .SDcols = names(datamatrix)[-1]]
  return(list(
    'datamatrix' = datamatrix,
    'samplemetadata' = data.table(datamatrix[,1],
                                  'class' = c(rep('sample', 40), rep('standard', 4)),
                                  'batch' = rep(c(1,2), 22),
                                  'MS' = round(c(rnorm(40, 10, 1), rep(NA, 4)), 1)),
    'variablemetadata' = data.table('VarID' = names(datamatrix)[-1],
                                    'class' = c('SI', rep('acide aminé', 15)),
                                    'conc' = c(rep(100, 4), 50, rep(100, 11)))
  ))
}
###############
test.data.gc <- function() {
  datamatrix <- rbind(data.table('SampleID' = paste0("Sample_", 1:40),
                                 'Adonitol'    = rnorm(40, 100, 2),
                                 'Cellobiose'  = rnorm(40, 10,2),
                                 'D-Fructose'  = rnorm(40, c(100,50), 2),
                                 'D-Galactose' = rnorm(40, c(10,50), 5),
                                 'D-Glucose'   = rnorm(40, 20, 1),
                                 'D-mannitol'  = rnorm(40, 60, 1),
                                 'D-Mannose'   = rnorm(40, c(100,60), 5),
                                 'D-Melezitose'= rnorm(40, 5, 1),
                                 'D-Melibiose' = rnorm(40, 10, 2),
                                 'D-Sorbitol'  = rnorm(40, 20, 3),
                                 'D-Trehalose' = rnorm(40, c(200,60), 4),
                                 'Dulcitol'    = rnorm(40, 40, 1),
                                 'D-Xylose'    = rnorm(40, 30, 2),
                                 'Galactinol'  = rnorm(40, 10, 1),
                                 'Gentiobiose' = rnorm(40, 20, 1),
                                 'L-Arabinose' = rnorm(40, c(40,30), 1),
                                 'Maltose'     = rnorm(40, 150, 2),
                                 'Myo-Inositol'= rnorm(40, 60, 2),
                                 'Raffinose'   = rnorm(40, 30, 2),
                                 'Saccharose'  = rnorm(40, 20, 2),
                                 'Xylitol'     = rnorm(40, 80, 2)),
                      as.list(c('Std_1a', rnorm(21, 50, 2))),
                      as.list(c('Std_2a', rnorm(21, 50, 2))),
                      as.list(c('Std_1b', rnorm(21, 50, 2))),
                      as.list(c('Std_2b', rnorm(21, 50, 2))))
  datamatrix[, (names(datamatrix)[-1]) := lapply(.SD, as.numeric), .SDcols = names(datamatrix)[-1]]
  datamatrix[, (names(datamatrix)[-1]) := lapply(.SD, function(x) {x/c(rnorm(40, 1, 0.1), rep(1, 4))}), .SDcols = names(datamatrix)[-1]]
  return(list(
    'datamatrix' = datamatrix,
    'samplemetadata' = data.table(datamatrix[,1],
                                  'class' = c(rep('sample', 40), rep('standard', 4)),
                                  'batch' = rep(c(1,2), 22),
                                  'MS' = round(c(rnorm(40, 10, 1), rep(NA, 4)), 1)),
    'variablemetadata' = data.table('VarID' = names(datamatrix)[-1],
                                    'class' = c('SI', rep('glucides', 20)),
                                    'conc' = 400)
  ))
}
############ FUNCTIONS ############
# from https://github.com/davesteps/machLearn/blob/master/init.R
label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}
################################## CONCENTRATION CALCULATION
data_DT <- function(x) {
  # x <- data
  y <- merge(x[[2]][,.(SampleID, batch, class)], x[[1]], by = 'SampleID')
  col_sel <- names(x[[1]])[-1]
  temp.list <- lapply(split(y, by = 'batch'), function(w) {
    # w <- split(y, by = 'batch')[[1]]
    temp.std.respF <- w[class == 'standard', lapply(.SD, function(v) {mean(v, na.rm = T)}), .SDcols = col_sel]/x[[3]][,conc]
    temp.calc <- w[, (col_sel) := mapply(function(y,z){y/z}, .SD, temp.std.respF, SIMPLIFY = F), .SDcols = col_sel]
    return(list("Results" = temp.calc,
                "respF" = data.table("batch" = paste0("RF_", w[,unique(batch)]), temp.std.respF))
    )
  })
  temp.calc <- do.call(rbind, sapply(temp.list,`[`,1))[order(batch, SampleID)]
  temp.rf <- t(data.frame(do.call(rbind, sapply(temp.list,`[`,2))[order(batch)], row.names = T))
  setkeyv(temp.calc, 'SampleID')
  return(list('datamatrix' = temp.calc[,-c('batch', 'class')],
              'samplemetadata' = x[[2]],
              'variablemetadata' = data.table(x[[3]], temp.rf))
  )
}

################################## RENDEMENT CALCULATION
data_Rdmt <- function(x, conc_SI, dilution_fac) {
  # x <- data
  y <- merge(x[[2]][,.(SampleID, batch, class)], x[[1]], by = 'SampleID')
  col_sel <- x[[3]][class == "SI", VarID]
  temp <- lapply(split(y[,c('SampleID', 'batch', 'class', col_sel), with = F], by = 'batch'), function(w) {
    # w <- split(y[,c('SampleID', 'batch', 'class', col_sel), with = F], by = 'batch')[[1]]
    temp.STD.SI <- w[class == 'standard', mean(get(col_sel), na.rm = T)]/x[[3]][VarID == col_sel, conc]
    temp.SPL.SI <- w[class == 'sample', get(col_sel)/conc_SI*dilution_fac]
    w[class == 'sample', Rdmt := temp.SPL.SI/temp.STD.SI]
    return(w)
  })
  temp <- do.call(rbind, temp)[order(batch, SampleID)]
  temp.sple <- merge(x[[2]], temp[,.(SampleID, Rdmt)], by = 'SampleID')
  setkeyv(temp.sple, 'SampleID')
  return(list('datamatrix' = x[[1]],
              'samplemetadata' = temp.sple,
              'variablemetadata' = x[[3]])
  )
}
################################## NORMALIZE WITH STD
data_SI <- function(data.calc) {
  #x <- data_Rdmt(data, 100, 1)
  x <- data.calc
  x[[2]] <- x[[2]][class != "standard"]
  x[[1]] <- x[[1]][SampleID %in% x[[2]][, SampleID]]
  x[[1]] <- data.table(x[[1]][,.(SampleID)], x[[1]][,-1]/x[[2]][,Rdmt])
  return(x)
}
################################## CHECK DATA FORMAT AND RETURN SUMMARY LIST
check_dlist <- function(data) {
  status_list <- rep(list(list('status' = "primary", 'value' = 0, 'message' = NULL)), 16)
  if (is.null(data)) {
    status_list[[1]] <- list('status' = "warning", 'value' = NA, 'message' = 'Aucune données compatible chargées.')
  } else {
    ## Importation ok
    status_list[[1]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)
    ## Check datamatrix as only values
    if (any(data[[1]][,-1][,lapply(.SD, is.numeric)] == F)) {
      status_list[[2]] <- list('status' = 'danger',
                               'value' = names(data[[1]])[which(data[[1]][,lapply(.SD, is.numeric)] == F)],
                               'message' = "Le feuillet 1 comprends du texte ou des caractères spéciaux. Vérifier l'absence de 'NA' 'Nan' 'nd' 'espaces' ','.")
    } else {status_list[[2]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)}
    ## Check duplicates in (1) or (2) or (3)
    if(any(duplicated(data[[1]][,1]) == T)) {
      status_list[[3]] <- list('status' = 'danger', 'value' = data[[1]][duplicated(),1][[1]], 'message' = "Certains échantillons sont dupliqués dans l'onglet (1)")
    } else {status_list[[3]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)}
    if(any(duplicated(data[[2]][,1]) == T)) {
      status_list[[4]] <- list('status' = 'danger', 'value' = data[[2]][duplicated(),1][[1]], 'message' = "Certains échantillons sont dupliqués dans l'onglet (2)")
    } else {status_list[[4]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)}
    if(any(duplicated(data[[3]][,1]) == T)) {
      status_list[[5]] <- list('status' = 'danger', 'value' = data[[3]][duplicated(),1][[1]], 'message' = "Certains échantillons sont dupliqués dans l'onglet (3)")
    } else {status_list[[5]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)}
    ## Check consistency in (1) (2) and (1) (3)
    if(!identical(data[[1]][,1], data[[2]][,1])) {
      status_list[[6]] <- list('status' = 'danger', 'value' = NA, 'message' = "La première colonne de l'onglet (1) doit être identique à celle de l'onglet (2)")
    } else {status_list[[6]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)}
    if(any(names(data[[1]][,-1]) %in% data[[3]][,1][[1]] == F)) {
      status_list[[7]] <- list('status' = 'danger', 'value' = NA, 'message' = "Les noms de variables de l'onglet (1) ne sont pas présent dans l'onglet (3)")
    } else {status_list[[7]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)
    if(!identical(data[[3]][,1][[1]], names(data[[1]][,-1]))) {
      status_list[[8]] <- list('status' = 'danger', 'value' = 0, 'message' = "Les noms de variables de l'onglet (1) ne sont pas dans le même ordre que dans l'onglet (3)")
    } else {status_list[[8]] <- list('status' = 'success', 'value' = data[[3]][,.N], 'message' = NULL)}
    if(!'class' %in% names(data[[2]])) {
      status_list[[9]] <- list('status' = 'danger', 'value' = NA, 'message' = "Il n'y a pas de colonne 'class' dans l'onglet (2)")
    } else {status_list[[9]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)
    if(!'standard' %in% data[[2]][,class]) {
      status_list[[10]] <- list('status' = 'danger', 'value' = 0, 'message' = "Il n'y a pas de 'standard' dans l'onglet (2)")
    } else {status_list[[10]] <- list('status' = 'success', 'value' = data[[2]][class == 'standard', .N], 'message' = NULL)
    if(!'sample' %in% data[[2]][,class]) {
      status_list[[11]] <- list('status' = 'danger', 'value' = 0, 'message' = "Il n'y a pas de 'sample' dans l'onglet (2)")
    } else {status_list[[11]] <- list('status' = 'success', 'value' = data[[2]][class == 'sample', .N], 'message' = NULL)}
    if(!'qc' %in% data[[2]][,class]) {
      status_list[[15]] <- list('status' = 'warning', 'value' = 0, 'message' = "Il n'y a pas de 'qc' dans l'onglet (2)")
    } else {status_list[[15]] <- list('status' = 'success', 'value' = data[[2]][class == 'qc', .N], 'message' = NULL)}
    if(!'blanc' %in% data[[2]][,class]) {
      status_list[[16]] <- list('status' = 'warning', 'value' = 0, 'message' = "Il n'y a pas de 'blanc' dans l'onglet (2)")
    } else {status_list[[16]] <- list('status' = 'success', 'value' = data[[2]][class == 'blanc', .N], 'message' = NULL)}}}
    if(!'class' %in% names(data[[3]])) {
      status_list[[12]] <- list('status' = 'danger', 'value' = NA, 'message' = "Il n'y a pas de colonne 'class' dans l'onglet (3)")
    } else {status_list[[12]] <- list('status' = 'success', 'value' = NA, 'message' = NULL)
    if(!'SI' %in% data[[3]][,class]) {
      status_list[[13]] <- list('status' = 'warning', 'value' = 0, 'message' = "Il n'y a pas de 'SI' dans l'onglet (3)")
    } else {status_list[[13]] <- list('status' = 'success', 'value' = data[[3]][class == 'SI', 1][[1]], 'message' = NULL)
    if(!'batch' %in% names(data[[2]])) {
      status_list[[14]] <- list('status' = 'warning', 'value' = 0, 'message' = "Il n'y a pas de colonne 'batch' dans l'onglet (3)")
    } else {status_list[[14]] <- list('status' = 'success', 'value' = length(unique(data[[2]][, batch])), 'message' = NULL)}}}}}
  return(status_list)
}
##################################