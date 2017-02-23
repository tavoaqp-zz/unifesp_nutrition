library(readr)
data <- read_csv("data/Banco_Geral_15022017.csv", col_types = cols(DN = col_date(format = "%m/%d/%Y"), Data_CP = col_date(format = "%m/%d/%Y"), Recrutamento = col_date(format = "%m/%d/%Y")))

data$DN_Calc=as.integer(floor(as.numeric(difftime(data$Recrutamento, data$DN, units="days")/365)))
7year_fix=c(422,490,502,510,648,687,915)

data[data$ID==422,]$DN_Calc=8
data[data$ID==490,]$DN_Calc=8
data[data$ID==502,]$DN_Calc=8
data[data$ID==510,]$DN_Calc=8
data[data$ID==648,]$DN_Calc=8
data[data$ID==687,]$DN_Calc=8
data[data$ID==915,]$DN_Calc=8

#ID 899 tem 14 anos mas vai ficar fora da analise

data=data[data$ID!=899,]

baseline=data[(is.na(data$Exclusao) | data$Exclusao=="Transferido") & !grepl("^C",data$Turma) & !is.na(data$ZIMC_0),]

baseline8years_male=subset(baseline,baseline$DN_Calc==8 & baseline$Sexo==1)
baseline8years_female=subset(baseline,baseline$DN_Calc==8 & baseline$Sexo==2)
baseline9years_male=subset(baseline,baseline$DN_Calc==9 & baseline$Sexo==1)
baseline9years_female=subset(baseline,baseline$DN_Calc==9 & baseline$Sexo==2)
baseline10years_male=subset(baseline,baseline$DN_Calc==10 & baseline$Sexo==1)
baseline10years_female=subset(baseline,baseline$DN_Calc==10 & baseline$Sexo==2)
baseline11years_male=subset(baseline,baseline$DN_Calc==11 & baseline$Sexo==1)
baseline11years_female=subset(baseline,baseline$DN_Calc==11 & baseline$Sexo==2)
baseline12years_male=subset(baseline,baseline$DN_Calc==12 & baseline$Sexo==1)
baseline12years_female=subset(baseline,baseline$DN_Calc==12 & baseline$Sexo==2)

#data to imputate missing anthropometric data

baseline8years_male_anthro = subset(baseline8years_male[!is.na(baseline8years_male$C._Cintura_1) & !is.na(baseline8years_male$C._Quadril_1) & !is.na(baseline8years_male$C._Pescoco_1) | baseline8years_male$ID==774,], select=c("C._Cintura_1", "C._Quadril_1", "C._Pescoco_1","Perc_Gordura_1","Peso_0","Altura_0","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1"))

baseline8years_female_anthro = baseline8years_female[!is.na(baseline8years_female$C._Cintura_1) & !is.na(baseline8years_female$C._Quadril_1) & !is.na(baseline8years_female$C._Pescoco_1) | baseline8years_female$ID==770,]
baseline8years_female_anthro[baseline8years_female_anthro$ID==770,]$C_Cintura_1=NA
baseline8years_female_anthro=subset(baseline8years_female_anthro, select=c("C._Cintura_1", "C._Quadril_1", "C._Pescoco_1","Perc_Gordura_1","Peso_0","Altura_0","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1"))

baseline9years_male_anthro = subset(baseline9years_male[!is.na(baseline9years_male$C._Cintura_1) & !is.na(baseline9years_male$C._Quadril_1) & !is.na(baseline9years_male$C._Pescoco_1) | baseline9years_male$ID==487,],select=c("C._Cintura_1", "C._Quadril_1", "C._Pescoco_1","Perc_Gordura_1","Peso_0","Altura_0","Gordura_Kg_1","MM_Kg_1","TMB_1","Agua_corpo_l_1","Perc_Peso_do_corpo_1","Perc_MM_1","Bioresistencia_1","Reatancia_1"))


#configure imputation models for every column


gen_mice_array <- function(mice_hash, colnames) {
    output=character(length(colnames))
    for (idx in 1:length(colnames)) {
        curname=colnames[idx]
        method_name=mice_methods[[curname]]
        output[idx]=method_name
    }
    return(output)
}


## colnames=names(data)
## library(hash)
## mice_methods=hash(keys=colnames,values="")

## mice_methods[["C._Cintura_1"]]="pmm"
## mice_methods[["C._Quadril_1"]]="pmm"
## mice_methods[["C._Pescoco_1"]]="pmm"

library(mice)

impBaseline8years_male_anthro = mice(baseline8years_male_anthro, m=50, maxit=300)
impBaseline8years_female_anthro = mice(baseline8years_female_anthro, m=50, maxit=300)
impBaseline9years_male_anthro = mice(baseline9years_male_anthro, m=50, maxit=300)

modelFit8yrs_male_anthro=with(impBaseline8years_male_anthro,lm(Bioresistencia_1 ~ C._Cintura_1+C._Quadril_1+C._Pescoco_1))
pool_8years_male_anthro=pool(modelFit8yrs_male_anthro)
summary(pool_8years_male_anthro)

modelFit8yrs_female_anthro=with(impBaseline8years_female_anthro,lm(Bioresistencia_1 ~ C._Cintura_1+C._Quadril_1+C._Pescoco_1))
pool_8years_female_anthro=pool(modelFit8yrs_female_anthro)
summary(pool_8years_female_anthro)

modelFit9yrs_male_anthro=with(impBaseline9years_male_anthro,lm(Bioresistencia_1 ~ C._Cintura_1+C._Quadril_1+C._Pescoco_1))
pool_9years_male_anthro=pool(modelFit9yrs_male_anthro)
summary(pool_9years_male_anthro)


complete8years_male_anthro=complete(impBaseline8years_male_anthro)
complete8years_female_anthro=complete(impBaseline8years_female_anthro)
complete9years_male_anthro=complete(impBaseline9years_male_anthro)

#There should be 
#colesterol ~ c_quadril + c_cintura + c_pescoco

library(mice)
library(VIM)

plot_missing_data_anthro(baseline8years_male, 8,"male")

plot_missing_data_anthro <- function(data_input, years, gender) {
    columns1=c("ZIMC_0","Peso_0","Altura_0","C._Quadril_1","C._Cintura_1","C._Pescoço_1")
    data_columns=subset(data_input,select=columns1)
    title=paste("Missing data - Baseline - ",years," years - ",gender)
    aggr(data_columns,col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data_columns), cex.axis=.7, gap=3, ylab=c(title,"Pattern"))
}

plot_missing_data_

missingcolumns8years=subset(baseline8years, select=c("C_Quadril_1","C_Cintura_1","C_Pescoco_1"))
impModelMissing8Years=mice(missingcolumns8years, m=5, maxit=50, meth='pmm',seed=500)
summary(impModelMissing8Years)


aggr_plot_8years_missing <- aggr(missingcolumns8years, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(missingcolumns8years), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

completeColumns8Years = complete(impModelMissing8Years,1)

subset(baseline,baseline$DN_Calc>12)

length(baseline_8years$Nome)


plotbaseline<-function(basedata) {
    plot(basedata$ZIMC_0, basedata$Altura_0, basedata$Peso_0)
}

#Check missing data from every cluster for fields "C. Quadril_1","C. Cintura_1" and "C. Pescoço_1"
