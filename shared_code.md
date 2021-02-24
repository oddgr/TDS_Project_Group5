

```R
library('ROSE')
```


```R
library('caret')
```


```R
library(stringr)
```


```R
library("dplyr")
library("tidyr")
library("ggplot2")
library(Hmisc)
```


```R
library('openxlsx')
```


```R
library('bit64')
```


```R
library('data.table')
```


```R

```

# Inclusion or Exclusion Criteria

+ exclude cases who had prior diagnosis of cancer
+ exclude cases that are diagnosed of breast cancer within 1 year of entering the programme as discussed in the meeting (as these people are likely not in good health conditions to start with)
+ exclude cases that lack the information on the predictors that we want to investigate
* Benign neoplasm excluded

## Prepare Data


```R
df_icd10=data.frame(fread("coding19.tsv" ))
```


```R
hes=data.frame(fread("hesin_diag.txt"))
```


```R
length(unique(hes $eid))
```


```R
hes_epi=data.frame(fread("hesin.txt"))
```


```R
dim(hes_epi)
```


```R
length(unique(hes_epi $eid))
```


```R
episode_ID=paste0(hes_epi$eid, "_", hes_epi$ins_index)
```


```R
hes_epi$episode_ID <- episode_ID
```


```R
h_t1 <- hes_epi %>% select(episode_ID,epiend )
```


```R
episode_ID_diag=paste0(hes$eid, "_", hes$ins_index)
```


```R
hes$episode_ID <- episode_ID_diag
```


```R
head(episode_ID_diag)
```


```R
h_t2 <- left_join(hes, h_t1 , by='episode_ID')
```


```R
head(h_t2)
```


```R
length(unique(h_t2$eid))
```


```R
# Cancer (all except benign)

icd10_cancer=c(c(paste0("C0", 0:9), paste0("C", 10:14)), # lip, oral cavity, larynx
               paste0("C", 15:26), # digestive
               paste0("C", 30:39), # respiratory
               paste0("C", 40:41), # bone, cartilage
               paste0("C", 43:44), # skin
               paste0("C", 45:49), # mesothelial
               paste0("C", 50), # breast
               paste0("C", 51:58), # female genital
               paste0("C", 60:63), # male genital
               paste0("C", 64:68), # urinary
               paste0("C", 69:72), # nervous
               paste0("C", 73:75), # endocrin
               paste0("C", 76:80), # other sites
               paste0("C", 81:96), # lymphoid
               paste0("C", 97), # multiple
               paste0("D0", 0:9), # in situ
               paste0("D", 37:48)) # uncertain behaviour

icd10_benign_neoplasm=c(
               paste0("D", 10:36) #Benign neoplasms, except benign neuroendocrine tumors D10-D36
     ) 

icd9_cancer=c(paste0(140:149), # lip, oral cavity, larynx
              paste0(150:159), # digestive
              paste0(160:165), # respiratory
              paste0(170:176), # bone, cartilage
              paste0(179:189), # genital
              paste0(190:199), # unspecified
              paste0(200:209), # lymphoid
              paste0(230:234), # in situ
              paste0(235:238), # uncertain 
              paste0(239)) # unspecified

icd9_benign_neoplasm=c(
               paste0(210:229) #Benign neoplasms, except benign neuroendocrine tumors D10-D36
     ) 
# Cardiovascular (all circulatory except hypertension)

icd10_cardiovascular=c(paste0("I0",0:2), # acute rheumatic fever
                       paste0("I0",5:9), # chronic rheumatic
                       paste0("I",20:25), # ischemic heart
                       paste0("I",26:28), # pulmonary
                       paste0("I",30:52), # other heart disease
                       paste0("I",60:69), # cerebrovascular
                       paste0("I",70:76), # arteries -- excluding I77.6 (in autoimmune)
                       paste0("I77", 0:5),
                       paste0("I77", 7:9),
                       paste0("I",78:79), 
                       paste0("I",81:82), # vein thrombosis
                       paste0("I",95:99)) # other circulatory

icd9_cardiovascular=c(paste0(390:392), # acute rheumatic fever
                      paste0(393:398), # chronic rheumatic
                      paste0(410:414), # ischemic heart
                      paste0(415:417), # pulmonary
                      paste0(420:429), # other heart disease
                      paste0(430:438), # cerebrovascular
                      paste0(440:445), # arteries -- excluding 446 and 4476 (in autoimmune)
                      paste0(447, 0:5), 
                      paste0(447, 7:9),
                      paste0(448:449), 
                      "453") #other vein thrombosis 


# Hypertension

icd10_hypertension=c(paste0("I", 10:15))

icd9_hypertension=paste0(401:405)


# Diabetes
icd10_diabetes=c("E11", "E12", "E13", "E14")

# icd9_diabetes="250"
icd9_diabetes=c(paste0(paste0("250", 0:9), 0),paste0(paste0("250", 0:9), 2))


# Respiratory (all)
icd10_respiratory=c(paste0("J", 30:39), # other upper
                    paste0("J", 40:47), # chronic lower
                    paste0("J", 60:70), # lung 
                    paste0("J", 80:84), # other interstitium
                    paste0("J", 85:86), # lower
                    paste0("J", 90:94), # other pleura
                    paste0("J", 95:99)) # other respiratory

icd9_respiratory=c(paste0(470:478), # other upper
                   paste0(480:488), # influenza and pneumonia
                   paste0(490:496), # chronic
                   paste0(500:508), # lung
                   paste0(510:519)) # other

```


```R
df_icd10<- df_icd10 %>% 
  rename(
    diag_icd10 = coding

    )
```

## 1. Exclude individuals who had prior diagnosis of cancer and 4. Individuals with benign neoplasms excluded


```R
df_icd10<- df_icd10 %>% select(diag_icd10,meaning)
```


```R
h_t3 <- left_join(h_t2,df_icd10, by='diag_icd10')
```


```R
h_t4 <-h_t3  %>% mutate(ICD10_2=str_extract(diag_icd10,"^.{3}")) %>%  mutate(ICD9_2=str_extract(diag_icd9,"^.{3}"))
```


```R
h_t5  <- h_t4  %>% filter(!ICD10_2 %in% icd10_benign_neoplasm | !ICD9_2 %in% icd9_benign_neoplasm)
```


```R
h_t6<- h_t5 %>% mutate(ec_cancer_ICD10=ifelse(ICD10_2 %in% icd10_cancer[icd10_cancer!="C50"] ,"cancer", ifelse(ICD10_2=="C50","breast cancer","not cancer")))
```


```R
h_t7<- h_t6 %>% mutate(ec_cancer_ICD9=ifelse(ICD9_2%in% icd9_cancer[icd9_cancer!="174"] ,"cancer", ifelse(ICD9_2=="174","breast cancer","not cancer")))
```


```R
ex_ICD10_cancer <- h_t7 %>% group_by(ec_cancer_ICD10, eid) %>% summarise(n_ob=n()) %>% filter(ec_cancer_ICD10=="cancer") 
ex_ICD10_cancer<- ex_ICD10_cancer$eid
```


```R
ex_ICD9_cancer <- h_t7 %>% group_by(ec_cancer_ICD9, eid) %>% summarise(n_ob=n()) %>% filter(ec_cancer_ICD9=="cancer") 
ex_ICD9_cancer<- ex_ICD9_cancer$eid
```


```R
ex_id_ls<- append(ex_ICD9_cancer ,ex_ICD10_cancer )
```


```R
h_t8 <- h_t7  %>% filter(!eid %in% ex_id_ls)
```


```R
length(unique(h_t8$eid))
```

## 2. Exclude individuals that are diagnosed of breast cancer within 1 year of entering the programme 


```R
test<- h_t8[1:100,]
```


```R
test$epiend <- as.Date(test$epiend, "%d/%m/%Y")
```


```R
h_t9 <- h_t8 %>% filter(ec_cancer_ICD10=="breast cancer"|ec_cancer_ICD9=="breast cancer" ) 

```


```R
h_t9$epiend <- as.Date(h_t9$epiend, "%d/%m/%Y")
```


```R
ex_date<-  h_t9 %>% filter(epiend>="2005-01-01" & epiend<="2005-12-31")
ex_date<- ex_date$eid
```


```R

h_t10 <- h_t8  %>% filter(!eid %in% ex_date)
```


```R
length(unique(h_t10$eid))
```


```R
h_t11<- h_t10 %>% mutate(outcome_ICD10=ifelse(ec_cancer_ICD10=="breast cancer",1,0),outcome_ICD9=ifelse(ec_cancer_ICD9=="breast cancer",1,0)  )
```

## Add otucome status


```R
h_t12<- h_t11 %>% filter(ec_cancer_ICD10=="breast cancer"|ec_cancer_ICD9=="breast cancer" ) 
outcome<- h_t12$eid
```


```R
h_t13 <- h_t11 %>% mutate(outcome_status=ifelse(eid %in% outcome,1,0))
```


```R
length(unique(h_t13$eid))
```
