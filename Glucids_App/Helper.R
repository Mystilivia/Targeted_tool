library(shiny)
library(shinyjs)
library(magrittr)
library(DT)
library(shinydashboard)
library(shinyBS)
library(data.table)
library(ggplot2)
library(readxl)
library(xlsx)
library(markdown)
library(curl)
library(plotly)
library(openssl)
library(tidyr)




############ DATASET ############
## Dataset ##
datamatrix <- rbindlist(list(
  data.table('SampleID' = paste0("Sample_", 1:40),
             'BABA' = rnorm(40, 100, 2),
             'asp' = rnorm(40, 10,2),
             'glu' = rnorm(40, c(100,50), 2),
             'asn' = rnorm(40, c(10,50), 5),
             'ser' = rnorm(40, 20, 1),
             'SMCSO' = rnorm(40, 60, 1),
             'gln' = rnorm(40, c(100,60), 5),
             'phe' = rnorm(40, 5, 1),
             'ile' = rnorm(40, 5, 2),
             'leu' = rnorm(40, 5, 3),
             'pro' = rnorm(40, c(200,60), 4),
             'trp' = rnorm(40, 40, 1),
             'tyr' = rnorm(40, 30, 2),
             'val' = rnorm(40, 10, 1),
             'lys' = rnorm(40, 20, 1),
             'arg' = rnorm(40, c(40,30), 1)),
  as.list(c('Std_1a', rnorm(16, 50, 2))),
  as.list(c('Std_2a', rnorm(16, 50, 2))),
  as.list(c('Std_1b', rnorm(16, 50, 2))),
  as.list(c('Std_2b', rnorm(16, 50, 2))))
)

datamatrix[, (names(datamatrix)[-1]) := lapply(.SD, as.numeric), .SDcols = names(datamatrix)[-1]]
datamatrix[, (names(datamatrix)[-1]) := lapply(.SD, function(x) {x/c(rnorm(40, 1, 0.1), rep(1, 4))}), .SDcols = names(datamatrix)[-1]]
#############
data.exemple1 <- list('datamatrix' = datamatrix,
                      'samplemetadata' = data.table('SampleID' = c(paste0("Sample_", 1:40), 'Std_1a', 'Std_2a', 'Std_1b', 'Std_2b'),
                                                    'class' = c(rep('sample', 40), rep('standard', 4)),
                                                    'batch' = rep(c(1,2), 22),
                                                    'MS' = c(rnorm(40, 10, 1), rep(NA, 4))),
                      'variablemetadata' = data.table('VarID' = c('BABA','asp','glu','asn','ser','SMCSO','gln','phe','ile','leu','pro','trp','tyr','val','lys','arg'),
                                                      'class' = c('SI', rep('acide aminé', 15)),
                                                      'conc' = c(rep(100, 4), 50, rep(100, 11))))
#############
data.exemple2 <- data.exemple1
#################################


############ FUNCTIONS ############
# from https://github.com/davesteps/machLearn/blob/master/init.R
label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}
############ 
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

############
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
############
data_SI <- function(data.calc) {
  #x <- data_Rdmt(data, 100, 1)
  x <- data.calc
  x[[2]] <- x[[2]][class == "sample"]
  x[[1]] <- x[[1]][SampleID %in% x[[2]][, SampleID]]
  x[[1]] <- data.table(x[[1]][,.(SampleID)], x[[1]][,-1]/x[[2]][,Rdmt])
  return(x)
}

############
# data_SI <- function(x, Conc_IS.Ec) {
#   # x <- temp_data_SI
#   y <- merge(x[[2]][,.(SampleID, batch, class)], x[[1]], by = 'SampleID')
#   col_sel <- names(x[[1]])[-1]
#   col_SI <- x[[3]][class == "SI", VarID]
#   Conc_IS.St <- x[[3]][class == "SI", conc]
#   
#   temp <- lapply(split(y, by = 'batch'), function(w) {
#     # w <- split(y, by = 'batch')[[1]]
#     Resp_IS.St <- w[class == 'standard', mean(get(col_SI), na.rm = T)]
#     
#     
#     temp.calc.si <- w[, (col_sel) := mapply(function(y,z) {
#       ((Resp_IS.Ec/Conc_IS.Ec)/(Resp_IS.St/Conc_IS.St))
#     }, Resp_IS.Ec, Resp_IS.St)]
#     
#     
#     #conc_X.Ec3 <- conc_X.Ec3 / ((Resp_IS.Ec/Conc_IS.Ec)/(Resp_IS.St/Conc_IS.St))
#     
#     temp.std.respF <- w[class == 'standard', lapply(.SD, function(v) {mean(v, na.rm = T)}), .SDcols = col_sel]/x[[3]][,conc]
#     temp.calc <- w[, (col_sel) := mapply(function(y,z){y/z}, .SD, temp.std.respF, SIMPLIFY = F), .SDcols = col_sel]
#     return(temp.calc)
#   })
#   temp <- do.call(rbind, temp)[order(batch, SampleID)]
#   setkeyv(temp, 'SampleID')
#   return(list('datamatrix' = temp[,-c('batch', 'class')],
#               'samplemetadata' = x[[2]],
#               'variablemetadata' = x[[3]])
#   )
# }
###################################


############ Archives ############
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


##################################