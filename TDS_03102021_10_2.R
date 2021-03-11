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

label_df = readRDS("/rds/general/user/jl2420/home/label_df.rds")
hist(sqrt(label_df$tdi   ))
hist(log10(label_df$waist_hip_ratio   ))
hist(log10(label_df$bio_triglycerides))
hist(log10(label_df$bio_hba1c))
hist(log10(label_df$bio_glucose ))
hist(log10(label_df$bio_HDL  ))
hist(log10(label_df$bio_SHBG  ))
hist(log10(label_df$bio_IGF1   ))

#Output exluded edi for furture use




