########## Determining weights and errors for regress.func and EBMA
# Note Results Array is data generated from "MakingBoostraps.R", or alternatively imported from "Boostrap Samples"

regress.func <- function(Y, preds.var){
  
  orgcols <- length(preds.var[1,])
  notNA <- which(!is.na(preds.var[1,]))
  predX <- preds.var[,notNA ]
  
  library(quadprog)
  d.mat <- solve(chol(t(predX)%*%predX))
  a.mat <- cbind(rep(1, ncol(predX)), diag(ncol(predX)))
  b.vec <- c(1, rep(0, ncol(predX)))
  d.vec <- t(Y) %*% predX
  out<- solve.QP(Dmat = d.mat, factorized =TRUE, dvec = d.vec, Amat = a.mat, bvec = b.vec, meq = 1)
  coefs <- rep(NA, orgcols)
  notDel <- c(1:orgcols)[notNA]#[notCor]
  coefs[notDel] <- out$solution
  return(coefs)
}


###### excluding KRLS regress.func

Y.final<- approve_bi<- ifelse(svdat$approval<3, 1, 0)#line 292 of rep code 

excluding_7<- results[,c(1:6,8:9),]

set.seed(10)
seednum = sample(10000,num.boostraps)
Y.boostrap = matrix(nrow = 1074,ncol = 500)
for (i in 1:num.boostraps){
  set.seed(seednum[i])
  bootstramp.sample.indexes = sample(1074,1074,replace = TRUE)
  
  ordered = bootstramp.sample.indexes[order(as.numeric(bootstramp.sample.indexes))]
  
  Y.boostrap[,i] = Y.final[ordered]
  
}

regress.func.results<- matrix(nrow = 500, ncol = 8)
for(i in 1:500){
  regress.func.results[i,] <- regress.func(Y.boostrap[,i], excluding_7[,,i])
}


mean.coefs_no7 = numeric(8)
error_no7 = numeric(8)
for (i in 1:8){
  error_no7[i] =sd(regress.func.results[,i])
  mean.coefs_no7[i] = mean(regress.func.results[,i])
}

mean.coefs_no7
error_no7



###### excluding KRLS EBMA

#install.packages("EBMAforecast")
library(EBMAforecast)


round_estimates <- function(x){
  if (x >= 1 | x <= 0){
    return(round(x)+0.001)}
  else {return(x)}
}


rounded_output <- array(dim = c(1074,8,500))
for (i in 1:length(excluding_7)){
  rounded_output[i] <- round_estimates(excluding_7[i])
}
rounded_output


EBMA_results_no7 <- data.frame()
Names <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "SVM_SMO", "Simple Average")
for(i in 1:500){
  results_slice <- rounded_output[,,i]
  ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y.boostrap[,i], .modelNames = Names)
  myCal <- calibrateEnsemble(ForecastData)
  weights <- myCal@modelWeights
  EBMA_results_no7 <- rbind(EBMA_results_no7, weights)
}
colnames(EBMA_results_no7) <- Names
head(EBMA_results_no7)


mean_coefs_EBMA_no7 = numeric(8)
error_EBMA_no7 = numeric(8)
for (i in 1:8){
  error_EBMA_no7[i] =sd(EBMA_results_no7[,i])
  mean_coefs_EBMA_no7[i] = mean(EBMA_results_no7[,i])
}

mean_coefs_EBMA_no7
error_EBMA_no7


###### including KRLS regress.func

Y.final<- approve_bi<- ifelse(svdat$approval<3, 1, 0)#line 292 of rep code 



set.seed(10)
seednum = sample(10000,num.boostraps)
Y.boostrap = matrix(nrow = 1074,ncol = 500)
for (i in 1:num.boostraps){
  set.seed(seednum[i])
  bootstramp.sample.indexes = sample(1074,1074,replace = TRUE)
  
  ordered = bootstramp.sample.indexes[order(as.numeric(bootstramp.sample.indexes))]
  
  Y.boostrap[,i] = Y.final[ordered]
  
}

regress.func.results<- matrix(nrow = 500, ncol = 9)
for(i in 1:500){
  regress.func.results[i,] <- regress.func(Y.boostrap[,i], results[,,i])
}


mean.coefs = numeric(9)
error = numeric(9)
for (i in 1:9){
  error[i] =sd(regress.func.results[,i])
  mean.coefs[i] = mean(regress.func.results[,i])
}

plotting_data<- as.data.frame(mean.coefs)
plotting_data
error


###### including KRLS EBMA

round_estimates <- function(x){
  if (x <= 0){
    return(0.001)}
  if (x >= 1){
    return(0.999)}
  else {return(x)}
}

rounded_output <- array(dim = c(1074,9,500))
for (i in 1:length(results)){
  rounded_output[i] <- round_estimates(results[i])
}
rounded_output


EBMA_results <- data.frame()
Names = c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest",  "KRLS", "SVM_SMO", "Simple Average")
for(i in 1:500){
  results_slice <- rounded_output[,,i]
  ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y.boostrap[,i], .modelNames = Names)
  myCal <- calibrateEnsemble(ForecastData)
  weights <- myCal@modelWeights
  EBMA_results <- rbind(EBMA_results, weights)
}
colnames(EBMA_results) <- Names
EBMA_results

mean_coefs_EBMA = numeric(9)
error_EBMA = numeric(9)
for (i in 1:9){
  error_EBMA[i] =sd(EBMA_results[,i])
  mean_coefs_EBMA[i] = mean(EBMA_results[,i])
}

mean_coefs_EBMA
error_EBMA
