library(readr)
data <- read_csv("data/Banco_Geral_08032017.csv", col_types = cols(DN = col_date(format = "%m/%d/%Y"), Data_CP = col_date(format = "%m/%d/%Y"), Recrutamento = col_date(format = "%m/%d/%Y")))

recode_classroom <- function(dataset) {
    dataset$Turma[dataset$Turma=="3º A"]=1
    dataset$Turma[dataset$Turma=="3º B"]=2
    dataset$Turma[dataset$Turma=="3º C"]=3
    dataset$Turma[dataset$Turma=="3º D"]=4
    dataset$Turma[dataset$Turma=="4º A"]=5
    dataset$Turma[dataset$Turma=="4º B"]=6
    dataset$Turma[dataset$Turma=="4º C"]=7
    dataset$Turma[dataset$Turma=="4º D"]=8
    dataset$Turma[dataset$Turma=="5º A"]=9
    dataset$Turma[dataset$Turma=="5º B"]=10
    dataset$Turma[dataset$Turma=="5º C"]=11
    dataset$Turma[dataset$Turma=="5º D"]=12
    dataset$Turma[dataset$Turma=="6º A"]=13
    dataset$Turma[dataset$Turma=="6º B"]=14
    dataset$Turma[dataset$Turma=="6º C"]=15
    dataset$Turma[dataset$Turma=="6º D"]=16
    dataset$Turma=as.numeric(dataset$Turma)
    return (dataset)
}

normalize_skewness<-function(dataframe, columns) {

    for (colname in columns) {
        dataframe[colname]=log(dataframe[colname])
    }

    return (dataframe)
}


#Turma should be a factor variable since is a category
data$Turma=factor(data$Turma)
data$Sexo=factor(data$Sexo)

data$DN_Calc=as.integer(floor(as.numeric(difftime(data$Recrutamento, data$DN, units="days")/365)))
## data$IMC_0=data$Peso_0/((data$Altura_0/100)^2)
## data$IMC_6m=data$Peso_6m/((data$Altura_6m/100)^2)

#These participants have less than 8 years, so we force them to the 8 year group
data[data$ID==422,]$DN_Calc=8
data[data$ID==490,]$DN_Calc=8
data[data$ID==502,]$DN_Calc=8
data[data$ID==510,]$DN_Calc=8
data[data$ID==648,]$DN_Calc=8
data[data$ID==687,]$DN_Calc=8
data[data$ID==915,]$DN_Calc=8

#ID 899 tem 14 anos mas vai ficar fora da analise
data=data[data$ID!=899,]

#Skewness normalization on continous only for those with positive skewness
## skewcolumns=c("Peso_0","Peso_6m","Altura_0","Altura_6m","Peso_CP","Altura_CP","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1","C._Quadril_1","C._Cintura_1","C._Pescoco_1","DCT_1_1","DCT_2_1","DCT_3_1","DCS_1_1","DCS_2_1","DCS__3_1","DCP__1_1","DCP__2_1","DCP_3_1","Eritrocitos_1","Hemoglobina_1","Hematocrito_1","CHCM_1","RDW_1","Leucocitos_1","Neutofiloa_1","Bastonetes_1","Segmentados_1","Eosilofilos_1","Basofilo_1","Linfocitos_tipicos_1","Linfocitos_totais_1","Monocitos_1","Glicemia_1","Creatinina_1","Colesterol_Total_1","HDL_1","Nao_HDL_1","LDL_1","VLDL_1","Triglicerideos_1","Acido_Urico_1","Fosforo_1","TGO_1","TGP_1","GGT_1","Fosfatase_1","Insulina_1","PTH_1","HOMA-IR_1","PAS1_1","PAS2_1","PAS3_1","PAD1_1","PAD2_1","PAD3_1")


data$Basofilo_1=ifelse(data$Basofilo_1==0.0,NA,data$Basofilo_1)
data$Eosilofilos_1=ifelse(data$Eosilofilos_1==0.0,NA,data$Eosilofilos_1)
data$HOMA_IR_1=ifelse(data$HOMA_IR_1==0.0,NA,data$HOMA_IR_1)

skewcolumns=c("Peso_0","Peso_6m","Altura_0","Altura_6m","Peso_CP","Altura_CP","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1","C._Quadril_1","C._Cintura_1","C._Pescoco_1","DCT_1_1","DCT_2_1","DCT_3_1","DCS_1_1","DCS_2_1","DCS__3_1","DCP__1_1","DCP__2_1","DCP_3_1","Eritrocitos_1","Hemoglobina_1","Hematocrito_1","CHCM_1","RDW_1","Leucocitos_1","Neutofiloa_1","Segmentados_1","Eosilofilos_1","Basofilo_1","Linfocitos_tipicos_1","Linfocitos_totais_1","Monocitos_1","Glicemia_1","Creatinina_1","Colesterol_Total_1","HDL_1","Nao_HDL_1","LDL_1","VLDL_1","Triglicerideos_1","Acido_Urico_1","Fosforo_1","TGO_1","TGP_1","GGT_1","Fosfatase_1","Insulina_1","PTH_1","HOMA_IR_1","PAS1_1","PAS2_1","PAS3_1","PAD1_1","PAD2_1","PAD3_1")

data=normalize_skewness(data, skewcolumns)

control_group=data[grepl("Pq Bristol", data$Escola),]
control_group=control_group[(is.na(control_group$Exclusao) | control_group$Exclusao=="Transferido" | control_group$Exclusao=="Transferida"),]
control_group=recode_classroom(control_group)

#There are two participants in control group that have ZIMC_0==-0.8 and NA but also have body composition data

expr_group=data[grepl("C. Mar", data$Escola),]
expr_group=expr_group[(is.na(expr_group$Exclusao) | expr_group$Exclusao=="Transferido" | expr_group$Exclusao=="Transferida"),]
expr_group=recode_classroom(expr_group)

#Fetch all continue rows. Left behind:
# Blastos_1
# Promielocitos_1
# Mielocitos_1
# Metamielocitos_1
# Linfocitos_atipicos_1
#Because they have zeros or NAs (should find out why this happened)
#There is an special though: Bastonetes_1 has a single positive value, and the rest is zero or NA
#I could not verify if it was imputed correctly because there is no model to compare to

control_group_cont=subset(control_group, select=c("ID","DN_Calc","ZIMC_0","Turma","Sexo","Peso_0","Peso_6m","Altura_0","Altura_6m","Peso_CP","Altura_CP","Perc_Gordura_1","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1","C._Quadril_1","C._Cintura_1","C._Pescoco_1","DCT_1_1","DCT_2_1","DCT_3_1","DCS_1_1","DCS_2_1","DCS__3_1","DCP__1_1","DCP__2_1","DCP_3_1","Eritrocitos_1","Hemoglobina_1","Hematocrito_1","VCM_1","HCM_1","CHCM_1","RDW_1","Plaquetas_1","Leucocitos_1","Neutofiloa_1","Segmentados_1","Eosilofilos_1","Basofilo_1","Linfocitos_tipicos_1","Linfocitos_totais_1","Monocitos_1","Glicemia_1","Creatinina_1","Colesterol_Total_1","HDL_1","Nao_HDL_1","LDL_1","VLDL_1","Triglicerideos_1","Acido_Urico_1","Fosforo_1","Calcio_1","TGO_1","TGP_1","GGT_1","Fosfatase_1","Insulina_1","PTH_1","HOMA_IR_1","PAS1_1","PAS2_1","PAS3_1","PAD1_1","PAD2_1","PAD3_1"))


# IDs 65,251,928 have lots of body composition data and should not be considered as z-score<1
# IDs 131, 213 have less body composition data but I will do the same as in the case above
control_group_noweight=subset(control_group_cont, (control_group_cont$ZIMC_0<1 | is.na(control_group_cont$ZIMC_0)) & control_group_cont$ID!=65 & control_group_cont$ID!=251 & control_group_cont$ID!=928 & control_group_cont$ID!=131 & control_group_cont$ID!=213, select=c("ID","DN_Calc","Turma","Sexo","Peso_0","Peso_6m","Altura_0","Altura_6m"))

control_group_oweight=subset(control_group_cont, control_group_cont$ZIMC_0>=1 | control_group_cont$ID==65 | control_group_cont$ID==251 | control_group_cont$ID==928 | control_group_cont$ID==131 | control_group_cont$ID==213, select=c("ID","DN_Calc","Turma","Sexo","Peso_0","Peso_6m","Altura_0","Altura_6m","Peso_CP","Altura_CP","Perc_Gordura_1","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1","C._Quadril_1","C._Cintura_1","C._Pescoco_1","DCT_1_1","DCT_2_1","DCT_3_1","DCS_1_1","DCS_2_1","DCS__3_1","DCP__1_1","DCP__2_1","DCP_3_1","Eritrocitos_1","Hemoglobina_1","Hematocrito_1","VCM_1","HCM_1","CHCM_1","RDW_1","Plaquetas_1","Leucocitos_1","Neutofiloa_1","Segmentados_1","Eosilofilos_1","Basofilo_1","Linfocitos_tipicos_1","Linfocitos_totais_1","Monocitos_1","Glicemia_1","Creatinina_1","Colesterol_Total_1","HDL_1","Nao_HDL_1","LDL_1","VLDL_1","Triglicerideos_1","Acido_Urico_1","Fosforo_1","Calcio_1","TGO_1","TGP_1","GGT_1","Fosfatase_1","Insulina_1","PTH_1","HOMA_IR_1","PAS1_1","PAS2_1","PAS3_1","PAD1_1","PAD2_1","PAD3_1"))

## aggr(control_group_noweight, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(control_group_noweight), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Fetching continue variables for experimental group. Left behind following columns:
## Blastos_1
## Promielocitos_1
## Mielocitos_1
## Metamielocitos_1
## Bastonetes_1
## Linfocitos_atipicos_1
# For the same reason as in the control group! Also Bastonetes_1 is out!

expr_group_cont=subset(expr_group, select=c("ID","DN_Calc","ZIMC_0","Turma","Sexo","Peso_0","Peso_6m","Altura_0","Altura_6m","Peso_CP","Altura_CP","Perc_Gordura_1","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1","C._Quadril_1","C._Cintura_1","C._Pescoco_1","DCT_1_1","DCT_2_1","DCT_3_1","DCS_1_1","DCS_2_1","DCS__3_1","DCP__1_1","DCP__2_1","DCP_3_1","Eritrocitos_1","Hemoglobina_1","Hematocrito_1","VCM_1","HCM_1","CHCM_1","RDW_1","Plaquetas_1","Leucocitos_1","Neutofiloa_1","Segmentados_1","Eosilofilos_1","Basofilo_1","Linfocitos_tipicos_1","Linfocitos_totais_1","Monocitos_1","Glicemia_1","Creatinina_1","Colesterol_Total_1","HDL_1","Nao_HDL_1","LDL_1","VLDL_1","Triglicerideos_1","Acido_Urico_1","Fosforo_1","Calcio_1","TGO_1","TGP_1","GGT_1","Fosfatase_1","Insulina_1","PTH_1","HOMA_IR_1","PAS1_1","PAS2_1","PAS3_1","PAD1_1","PAD2_1","PAD3_1"))


#IDs 549, 579 have body composition data and should not be considered as having z-score for imc below 1
expr_group_noweight=subset(expr_group_cont, (is.na(expr_group_cont$ZIMC_0) | expr_group_cont$ZIMC_0<1) & expr_group_cont$ID!=549 & expr_group_cont$ID!=579, select=c("ID","DN_Calc","Turma","Sexo","Peso_0","Peso_6m","Altura_0","Altura_6m"))


expr_group_oweight=subset(expr_group_cont, expr_group_cont$ZIMC_0>=1 | expr_group_cont$ID==549 | expr_group_cont$ID==579, select=c("ID","DN_Calc","Turma","Sexo","Peso_0","Peso_6m","Altura_0","Altura_6m","Peso_CP","Altura_CP","Perc_Gordura_1","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1","C._Quadril_1","C._Cintura_1","C._Pescoco_1","DCT_1_1","DCT_2_1","DCT_3_1","DCS_1_1","DCS_2_1","DCS__3_1","DCP__1_1","DCP__2_1","DCP_3_1","Eritrocitos_1","Hemoglobina_1","Hematocrito_1","VCM_1","HCM_1","CHCM_1","RDW_1","Plaquetas_1","Leucocitos_1","Neutofiloa_1","Segmentados_1","Eosilofilos_1","Basofilo_1","Linfocitos_tipicos_1","Linfocitos_totais_1","Monocitos_1","Glicemia_1","Creatinina_1","Colesterol_Total_1","HDL_1","Nao_HDL_1","LDL_1","VLDL_1","Triglicerideos_1","Acido_Urico_1","Fosforo_1","Calcio_1","TGO_1","TGP_1","GGT_1","Fosfatase_1","Insulina_1","PTH_1","HOMA_IR_1","PAS1_1","PAS2_1","PAS3_1","PAD1_1","PAD2_1","PAD3_1"))


## md.pattern(control_group_noweight)
## md.pattern(control_group_oweight)

## md.pattern(expr_group_noweight)
## md.pattern(expr_group_oweight)

##We divided each group into two subgroups: one subgroup for overweight and other for non-overweight. For the overweight subgroup we are considering body composition, biochemical data. For non-overweight we are only considering weight and height at baseline and 6 months. For both subgroups we are also considering IMC, age, sex, classroom


#MULTIPLE IMPUTATION FOR EVERY SUBGROUP
library(mice)

control_group_noweight_imp_tmp = mice(control_group_noweight, maxit=0, print=FALSE)
control_group_noweight_pred = control_group_noweight_imp_tmp$pred
control_group_noweight_meth = control_group_noweight_imp_tmp$meth
control_group_noweight_pred[,"ID"]=0
control_group_noweight_meth["ID"]=""
## control_group_noweight_meth["IMC_0"]="~I(Peso_0/(Altura_0/100)^2)"
## control_group_noweight_meth["IMC_6m"]="~I(Peso_6m/(Altura_6m/100)^2)"
control_group_noweight_imp= mice(control_group_noweight, pred=control_group_noweight_pred, meth=control_group_noweight_meth, m=50, maxit=10, seed= 23109)
#Here is missing pool method, first we need to define model of interest!
control_group_noweight_complete=complete(control_group_noweight_imp)


control_group_oweight_imp_tmp = mice(control_group_oweight, maxit=0, print=FALSE)
control_group_oweight_pred = control_group_oweight_imp_tmp$pred
control_group_oweight_meth = control_group_oweight_imp_tmp$meth
control_group_oweight_pred[,"ID"]=0
control_group_oweight_meth["ID"]=""
## control_group_oweight_meth["IMC_0"]="~I(Peso_0/(Altura_0/100)^2)"
## control_group_oweight_meth["IMC_6m"]="~I(Peso_6m/(Altura_6m/100)^2)"
control_group_oweight_imp= mice(control_group_oweight, pred=control_group_oweight_pred, meth=control_group_oweight_meth, m=50, maxit=10, seed= 23142355)
#Here is missing pool method, first we need to define model of interest!
control_group_oweight_complete=complete(control_group_oweight_imp)


expr_group_noweight_imp_tmp = mice(expr_group_noweight, maxit=0, print=FALSE)
expr_group_noweight_pred = expr_group_noweight_imp_tmp$pred
expr_group_noweight_meth = expr_group_noweight_imp_tmp$meth
expr_group_noweight_pred[,"ID"]=0
expr_group_noweight_meth["ID"]=""
## expr_group_noweight_meth["IMC_0"]="~I(Peso_0/(Altura_0/100)^2)"
## expr_group_noweight_meth["IMC_6m"]="~I(Peso_6m/(Altura_6m/100)^2)"
expr_group_noweight_imp= mice(expr_group_noweight, pred=expr_group_noweight_pred, meth=expr_group_noweight_meth, m=50, maxit=10, seed= 2310997)
#Here is missing pool method, first we need to define model of interest!
expr_group_noweight_complete=complete(expr_group_noweight_imp)


expr_group_oweight_imp_tmp = mice(expr_group_oweight, maxit=0, print=FALSE)
expr_group_oweight_pred = expr_group_oweight_imp_tmp$pred
expr_group_oweight_meth = expr_group_oweight_imp_tmp$meth
expr_group_oweight_pred[,"ID"]=0
expr_group_oweight_meth["ID"]=""
## expr_group_oweight_meth["IMC_0"]="~I(Peso_0/(Altura_0/100)^2)"
## expr_group_oweight_meth["IMC_6m"]="~I(Peso_6m/(Altura_6m/100)^2)"
expr_group_oweight_imp= mice(expr_group_oweight, pred=expr_group_oweight_pred, meth=expr_group_oweight_meth, m=50, maxit=10, seed= 23142593)
#Here is missing pool method, first we need to define model of interest!
expr_group_oweight_complete=complete(expr_group_oweight_imp)


## Eosilofilos_1, HOMA_IR_1

## aggr(control_group_nobese, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(control_group_nobese), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

## aggr(control_group_obese, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(control_group_obese), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
## 

## 7year_fix=c(422,490,502,510,648,687,915)

## data[data$ID==422,]$DN_Calc=8
## data[data$ID==490,]$DN_Calc=8
## data[data$ID==502,]$DN_Calc=8
## data[data$ID==510,]$DN_Calc=8
## data[data$ID==648,]$DN_Calc=8
## data[data$ID==687,]$DN_Calc=8
## data[data$ID==915,]$DN_Calc=8

## #ID 899 tem 14 anos mas vai ficar fora da analise

## data=data[data$ID!=899,]

## baseline=data[(is.na(data$Exclusao) | data$Exclusao=="Transferido") & !grepl("^C",data$Turma) & !is.na(data$ZIMC_0),]



## baseline8years_male=subset(baseline,baseline$DN_Calc==8 & baseline$Sexo==1)
## baseline8years_female=subset(baseline,baseline$DN_Calc==8 & baseline$Sexo==2)
## baseline9years_male=subset(baseline,baseline$DN_Calc==9 & baseline$Sexo==1)
## baseline9years_female=subset(baseline,baseline$DN_Calc==9 & baseline$Sexo==2)
## baseline10years_male=subset(baseline,baseline$DN_Calc==10 & baseline$Sexo==1)
## baseline10years_female=subset(baseline,baseline$DN_Calc==10 & baseline$Sexo==2)
## baseline11years_male=subset(baseline,baseline$DN_Calc==11 & baseline$Sexo==1)
## baseline11years_female=subset(baseline,baseline$DN_Calc==11 & baseline$Sexo==2)
## baseline12years_male=subset(baseline,baseline$DN_Calc==12 & baseline$Sexo==1)
## baseline12years_female=subset(baseline,baseline$DN_Calc==12 & baseline$Sexo==2)

#data to imputate missing anthropometric data

## baseline8years_male_anthro = subset(baseline8years_male[!is.na(baseline8years_male$C._Cintura_1) & !is.na(baseline8years_male$C._Quadril_1) & !is.na(baseline8years_male$C._Pescoco_1) | baseline8years_male$ID==774,], select=c("C._Cintura_1", "C._Quadril_1", "C._Pescoco_1","Perc_Gordura_1","Peso_0","Altura_0","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1"))

## baseline8years_female_anthro = baseline8years_female[!is.na(baseline8years_female$C._Cintura_1) & !is.na(baseline8years_female$C._Quadril_1) & !is.na(baseline8years_female$C._Pescoco_1) | baseline8years_female$ID==770,]
## baseline8years_female_anthro[baseline8years_female_anthro$ID==770,]$C_Cintura_1=NA
## baseline8years_female_anthro=subset(baseline8years_female_anthro, select=c("C._Cintura_1", "C._Quadril_1", "C._Pescoco_1","Perc_Gordura_1","Peso_0","Altura_0","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1"))

## baseline9years_male_anthro = subset(baseline9years_male[!is.na(baseline9years_male$C._Cintura_1) & !is.na(baseline9years_male$C._Quadril_1) & !is.na(baseline9years_male$C._Pescoco_1) | baseline9years_male$ID==487,],select=c("C._Cintura_1", "C._Quadril_1", "C._Pescoco_1","Perc_Gordura_1","Peso_0","Altura_0","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1"))


## #configure imputation models for every column


## gen_mice_array <- function(mice_hash, colnames) {
##     output=character(length(colnames))
##     for (idx in 1:length(colnames)) {
##         curname=colnames[idx]
##         method_name=mice_methods[[curname]]
##         output[idx]=method_name
##     }
##     return(output)
## }


## ## colnames=names(data)
## ## library(hash)
## ## mice_methods=hash(keys=colnames,values="")

## ## mice_methods[["C._Cintura_1"]]="pmm"
## ## mice_methods[["C._Quadril_1"]]="pmm"
## ## mice_methods[["C._Pescoco_1"]]="pmm"

## library(mice)

## impBaseline8years_male_anthro = mice(baseline8years_male_anthro, m=50, maxit=300, seed= 23109)
## impBaseline8years_female_anthro = mice(baseline8years_female_anthro, m=50, maxit=300, seed=23477)
## impBaseline9years_male_anthro = mice(baseline9years_male_anthro, m=50, maxit=300, seed=29777)

## modelFit8yrs_male_anthro=with(impBaseline8years_male_anthro,lm(Bioresistencia_1 ~ C._Cintura_1+C._Quadril_1+C._Pescoco_1))
## pool_8years_male_anthro=pool(modelFit8yrs_male_anthro)
## round(summary(pool_8years_male_anthro),2)

## modelFit8yrs_female_anthro=with(impBaseline8years_female_anthro,lm(Bioresistencia_1 ~ C._Cintura_1+C._Quadril_1+C._Pescoco_1))
## pool_8years_female_anthro=pool(modelFit8yrs_female_anthro)
## round(summary(pool_8years_female_anthro),2)

## modelFit9yrs_male_anthro=with(impBaseline9years_male_anthro,lm(Bioresistencia_1 ~ C._Cintura_1+C._Quadril_1+C._Pescoco_1))
## pool_9years_male_anthro=pool(modelFit9yrs_male_anthro)
## round(summary(pool_9years_male_anthro),2)


## complete8years_male_anthro=complete(impBaseline8years_male_anthro)
## complete8years_female_anthro=complete(impBaseline8years_female_anthro)
## complete9years_male_anthro=complete(impBaseline9years_male_anthro)





## #There should be 
## #colesterol ~ c_quadril + c_cintura + c_pescoco

## ## library(mice)
## ## library(VIM)

## ## plot_missing_data_anthro(baseline8years_male, 8,"male")

## ## plot_missing_data_anthro <- function(data_input, years, gender) {
## ##     columns1=c("ZIMC_0","Peso_0","Altura_0","C._Quadril_1","C._Cintura_1","C._Pescoço_1")
## ##     data_columns=subset(data_input,select=columns1)
## ##     title=paste("Missing data - Baseline - ",years," years - ",gender)
## ##     aggr(data_columns,col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data_columns), cex.axis=.7, gap=3, ylab=c(title,"Pattern"))
## ## }

## ## plot_missing_data_

## ## missingcolumns8years=subset(baseline8years, select=c("C_Quadril_1","C_Cintura_1","C_Pescoco_1"))
## ## impModelMissing8Years=mice(missingcolumns8years, m=5, maxit=50, meth='pmm',seed=500)
## ## summary(impModelMissing8Years)


## ## aggr_plot_8years_missing <- aggr(missingcolumns8years, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(missingcolumns8years), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

## ## completeColumns8Years = complete(impModelMissing8Years,1)

## ## subset(baseline,baseline$DN_Calc>12)

## ## length(baseline_8years$Nome)


## ## plotbaseline<-function(basedata) {
## ##     plot(basedata$ZIMC_0, basedata$Altura_0, basedata$Peso_0)
## ## }

## ## #Check missing data from every cluster for fields "C. Quadril_1","C. Cintura_1" and "C. Pescoço_1"
