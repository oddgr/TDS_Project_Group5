# Code Purpose: Identify cases before entry using X53.0.0

library('ROSE')
library('caret')
library(stringr)
library("dplyr")
library("tidyr")
library("ggplot2")
library(Hmisc)
library('openxlsx')
library('bit64')
library('data.table')

label_df = readRDS("/rds/general/user/jl2420/home/HPC_Result/0309_31cols_recoded_label.rds")
label_df<- label_df%>% select_if(is.numeric)
log_df<- lapply(label_df,log10)
Map(hist, log_df, main = names(log_df), breaks = 100)

hist(sqrt(label_df$tdi))

#Output exluded edi for furture use
saveRDS(log_df, file = "/rds/general/user/jl2420/home/HPC_Result/log_df_complete.rds")



