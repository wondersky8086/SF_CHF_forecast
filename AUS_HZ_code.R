install.packages(c("boot", "ftsa"))
require(boot)
require(ftsa)
require(ie2misc)
require(xtable)
require(philentropy)
require(abind)

setwd("D:/kk/my documents/academic/MRes/work/Thesis Submission/data")

# read Australian life-table data

year = 1921:2020
age = 0:110
n_year = length(year)
n_age = length(age)

AUS_female_qx = matrix(read.table("AUS_female_lifetable.txt", 
                                  header = TRUE)[,4], n_age, n_year, byrow = FALSE)
AUS_male_qx = matrix(read.table("AUS_male_lifetable.txt", 
                                header = TRUE)[,4], n_age, n_year, byrow = FALSE)
colnames(AUS_female_qx) = colnames(AUS_male_qx) = year

AUS_female_dx = matrix(NA, n_age, n_year)
for(ik in 1:n_year)
{
  start_pop = 10^5
  for(ij in 1:n_age)
  {
    AUS_female_dx[ij,ik] = AUS_female_qx[ij,ik] * start_pop
    start_pop = start_pop - AUS_female_dx[ij,ik]
  }
  rm(start_pop)
}

AUS_female_lx = matrix(NA, n_age, n_year)
for(ik in 1:n_year)
{
  lx = 10^5
  AUS_female_lx[1, ik] <- lx
  
  for(ij in 1:n_age)
  {
    AUS_female_dx[ij,ik] =AUS_female_qx[ij,ik] * lx
    lx = lx - AUS_female_dx[ij,ik]
    if (ij < n_age) {
      AUS_female_lx[ij + 1, ik] <- lx
    }
  }
}
colnames(AUS_female_dx) = colnames(AUS_female_lx) = year

AUS_male_dx = matrix(NA, n_age, n_year)
for(ik in 1:n_year)
{
  start_pop = 10^5
  for(ij in 1:n_age)
  {
    AUS_male_dx[ij,ik] = AUS_male_qx[ij,ik] * start_pop
    start_pop = start_pop - AUS_male_dx[ij,ik]
  }
  rm(start_pop)
}

AUS_male_lx = matrix(NA, n_age, n_year)
for(ik in 1:n_year)
{
  lx = 10^5
  AUS_male_lx[1, ik] <- lx
  
  for(ij in 1:n_age)
  {
    AUS_male_dx[ij,ik] =AUS_male_qx[ij,ik] * lx
    lx = lx - AUS_male_dx[ij,ik]
    if (ij < n_age) {
      AUS_male_lx[ij + 1, ik] <- lx
    }
  }
}
colnames(AUS_male_dx) = colnames(AUS_male_lx) = year

#########################################################################
# compute Cumulative Hazard Function - Nelson-Aalen estimator at time t
#########################################################################

H_female = matrix(NA, n_age, n_year)
for(ik in 1:n_age)
{
  for(ij in 1:n_year)
  {
    if (ik == 1) {
      H_female[ik, ij] <- AUS_female_dx[ik, ij] / AUS_female_lx[ik, ij]
    } else {
      H_female[ik, ij] <- H_female[ik - 1, ij] + (AUS_female_dx[ik, ij] / AUS_female_lx[ik, ij])
    }
  }
}

H_male = matrix(NA, n_age, n_year)
for(ik in 1:n_age)
{
  for(ij in 1:n_year)
  {
    if (ik == 1) {
      H_male[ik, ij] <- AUS_male_dx[ik, ij] / AUS_male_lx[ik, ij]
    } else {
      H_male[ik, ij] <- H_male[ik - 1, ij] + (AUS_male_dx[ik, ij] / AUS_male_lx[ik, ij])
    }
  }
}
colnames(H_female) = colnames(H_male) = year

# take the log transformation

H_log_female = log(H_female)
H_log_male = log(H_male)

colnames(H_log_female) = colnames(H_log_male) = year
rownames(H_log_female) = rownames(H_log_male) = age

###############
# Back testing
###############

training_H_female <- H_female[66:110,1:80]
testing_H_female <- H_female[66:110,81:100]
training_H_male <- H_male[66:110,1:80]
testing_H_male <- H_male[66:110,81:100]

# take the log transformation

training_years = 1921:2000
testing_years = 2001:2020

log_training_H_female = log(training_H_female)
colnames(log_training_H_female) = training_years

log_training_H_male = log(training_H_male)
colnames(log_training_H_male) = training_years

# model the transformed training data FPCR (20-years-ahead to compare with testing data)

fh = 20
fore_training_H_female_FPCR = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                                log_training_H_female),order = 6, ), h = fh)
fore_training_H_male_FPCR = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                              log_training_H_male), order = 6, ), h = fh)

training_H_female_FPCR_back_transform = exp(fore_training_H_female_FPCR$mean$y)
training_H_male_FPCR_back_transform = exp(fore_training_H_male_FPCR$mean$y)
colnames(training_H_female_FPCR_back_transform) = testing_years
colnames(training_H_male_FPCR_back_transform) = testing_years

# model the transformed training data LC method (20-years-ahead to compare with testing data)

fh = 20
fore_training_H_female_LC = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                                log_training_H_female),order = 1), h = fh)
fore_training_H_male_LC = forecast(ftsm(fts(age[66:(n_age - 1)], 
                                              log_training_H_male), order = 1), h = fh)

training_H_female_LC_back_transform = exp(fore_training_H_female_LC$mean$y)
training_H_male_LC_back_transform = exp(fore_training_H_male_LC$mean$y)
colnames(training_H_female_LC_back_transform) = testing_years
colnames(training_H_male_LC_back_transform) = testing_years

##############################
# Perform MAFE and MAPE test
##############################

Calc_FPCR_female <- abs(testing_H_female-training_H_female_FPCR_back_transform)
n <- ncol(Calc_FPCR_female)
MAE_FPCR_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAE_FPCR_female[i] <- sum(Calc_FPCR_female[, i]) / (divisor*46)
}
cat("Mean Absolute Error (MAE):", MAE_FPCR_female, "\n")
Calc_LC_female <- abs(testing_H_female - training_H_female_LC_back_transform)
n <- ncol(Calc_LC_female)
MAE_LC_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAE_LC_female[i] <- sum(Calc_LC_female[, i])/(divisor * 46)
}
cat("Mean Absolute Error (MAE):", MAE_LC_female, "\n")

CalcMAPE_FPCR_female <- abs((testing_H_female - training_H_female_FPCR_back_transform)/testing_H_female)
n <- ncol(CalcMAPE_FPCR_female)
MAPE_FPCR_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAPE_FPCR_female[i] <- sum(CalcMAPE_FPCR_female[, i], na.rm = TRUE) * 100/(divisor * 46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_FPCR_female, "\n")
CalcMAPE_LC_female <- abs((testing_H_female - training_H_female_LC_back_transform)/testing_H_female)
n <- ncol(CalcMAPE_LC_female)
MAPE_LC_female <- numeric(n)
for(i in 1:n)
{
    divisor <- n+1-i
    MAPE_LC_female[i] <- sum(CalcMAPE_LC_female[, i], na.rm = TRUE) * 100/(divisor * 46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_LC_female, "\n")

####################################################
# Perform MAFE and MAPE test (repeat tests for Male)
####################################################

Calc_FPCR_male<-abs(testing_H_male-training_H_male_FPCR_back_transform)
n<-ncol(Calc_FPCR_male)
MAE_FPCR_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAE_FPCR_male[i] <- sum(Calc_FPCR_male[, i]) / (divisor*46)
}
cat("Mean Absolute Error (MAE):", MAE_FPCR_male, "\n")
Calc_LC_male<-abs(testing_H_male-training_H_male_LC_back_transform)
n<-ncol(Calc_LC_male)
MAE_LC_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAE_LC_male[i] <- sum(Calc_LC_male[, i]) / (divisor*46)
}
cat("Mean Absolute Error (MAE):", MAE_LC_male, "\n")

CalcMAPE_FPCR_male<-abs((testing_H_male-training_H_male_FPCR_back_transform)/testing_H_male)
n<-ncol(CalcMAPE_FPCR_male)
MAPE_FPCR_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAPE_FPCR_male[i] <- sum(CalcMAPE_FPCR_male[, i],na.rm=TRUE) *100 / (divisor*46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_FPCR_male, "\n")
CalcMAPE_LC_male<-abs((testing_H_male-training_H_male_LC_back_transform)/testing_H_male)
n<-ncol(CalcMAPE_LC_male)
MAPE_LC_male <- numeric(n)
for (i in 1:n) {
  divisor <- n+1-i
  MAPE_LC_male[i] <- sum(CalcMAPE_LC_male[, i],na.rm=TRUE) *100 / (divisor*46)
}
cat("Mean Absolute Percent Error (MAPE):", MAPE_LC_male, "\n")

#Making Latex table for MAFE and MAPE test

MATable<-data.frame(MAFE = c(mean(MAE_FPCR_female),mean(MAE_LC_female),mean(MAE_FPCR_male),mean(MAE_LC_male)),
                    MAPE = c(mean(MAPE_FPCR_female),mean(MAPE_LC_female),mean(MAPE_FPCR_male),mean(MAPE_LC_male)))
colnames(MATable) = c('MAFE','MAPE')
rownames(MATable) = c('Female FPCR method','Female LC method','Male FPCR method','Male LC method')
xtable(MATable, caption = 'Point forecast accuracy test result summary',digits = 4)

#######################################################################
# model the FPCR transformed data (46-years-ahead for annuity pricing)
#######################################################################

fhap = 46
set.seed(1)

# Construct point forecast and prediction intervals

H_female_forecast <- exp(forecast(ftsm(fts(age[65:(n_age - 1)], H_log_female[65:110,]), 
                                               order = 1), h = fhap)$mean$y)


H_female_forecast_lower<-exp(forecast(ftsm(fts(age[65:(n_age - 1)], H_log_female[65:110,]), 
                                                   order = 1), h = 46,method = 'ets',level = 95, 
                                                   pimethod = 'nonparametric',B = 1000)$lower$y)

H_female_forecast_upper<-exp(forecast(ftsm(fts(age[65:(n_age - 1)], H_log_female[65:110,]), 
                                                    order = 1), h = 46,method = 'ets',level = 95, 
                                                    pimethod = 'nonparametric',B = 1000)$upper$y)

H_male_forecast <- exp(forecast(ftsm(fts(age[65:(n_age - 1)], H_log_male[65:110,]), 
                                             order = 6), h = fhap)$mean$y)

H_male_forecast_lower<-exp(forecast(ftsm(fts(age[65:(n_age - 1)], H_log_male[65:110,]), 
                                                  order = 6), h = 46,method = 'ets',level = 95, 
                                                  pimethod = 'nonparametric',B = 1000)$lower$y)
 
H_male_forecast_upper<-exp(forecast(ftsm(fts(age[65:(n_age - 1)], H_log_male[65:110,]), 
                                                  order = 6), h = 46,method = 'ets',level = 95, 
                                                  pimethod = 'nonparametric',B = 1000)$upper$y)

colnames(H_female_forecast) = colnames(H_male_forecast) =2021:2066
colnames(H_female_forecast_lower) = colnames(H_male_forecast_lower) =2021:2066
colnames(H_female_forecast_upper) = colnames(H_male_forecast_upper) =2021:2066

# Turn cumulative Hazard function into Survival function S(t) = exp(-H(t))
SV_H_female_forecast<-rbind(exp(-H_female_forecast), rep(0, fhap))
SV_H_female_forecast_lower<-rbind(exp(-H_female_forecast_upper), rep(0, fhap))
SV_H_female_forecast_upper<-rbind(exp(-H_female_forecast_lower), rep(0, fhap))
SV_H_male_forecast<-rbind(exp(-H_male_forecast), rep(0, fhap))
SV_H_male_forecast_lower<-rbind(exp(-H_male_forecast_upper), rep(0, fhap))
SV_H_male_forecast_upper<-rbind(exp(-H_male_forecast_lower), rep(0, fhap))