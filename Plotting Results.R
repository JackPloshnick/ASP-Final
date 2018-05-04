library(ggplot2)

##################### Plotting regress.func results without KRLS

plotting_data_no7<- as.data.frame(mean.coefs_no7)

Names_no7 <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART",
               "Random Forest", "SVM_SMO", "Simple Average")

plotting_data_no7 <- cbind(Names_no7, plotting_data_no7)

upper_no7<-(mean.coefs_no7+ 1.96*error_no7)
lower_no7<- (mean.coefs_no7 - 1.96*error_no7)

ggplot(plotting_data_no7, aes(x = plotting_data_no7$Names_no7, y = plotting_data_no7$mean.coefs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = upper_no7, ymin = lower_no7))+ 
  labs(title = "Model Weights of Regression Ensamble \n(missing Model KRLS)")+
  ylab("Model Weights")+
  xlab("Model") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

##################### Plotting regress.func results with KRLS

plotting_data<- as.data.frame(mean.coefs)

Names <- c("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", 
           "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")

plotting_data <- cbind(Names, plotting_data)
upper<-(mean.coefs+ 1.96*error)
lower<- (mean.coefs - 1.96*error)

ggplot(plotting_data, aes(x = plotting_data$Names, y = plotting_data$mean.coefs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = upper, ymin = lower))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+ 
  labs(title = "Model weights of Regression Ensamble ")+
  ylab("Model Weights")+
  xlab("Model")

################# Plotting EBMA results without KRLS

plotting_data_EBMA<- as.data.frame(mean_coefs_EBMA_no7)

Names_no7 <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART",
               "Random Forest", "SVM_SMO", "Simple Average")

plotting_data_EBMA <- cbind(Names_no7, plotting_data_EBMA)
upper_EBMA<-(mean_coefs_EBMA_no7+ 1.96*error_EBMA_no7)
lower_EBMA<- (mean_coefs_EBMA_no7 - 1.96*error_EBMA_no7)

ggplot(plotting_data_EBMA, aes(x = plotting_data_EBMA$Names, y = plotting_data_EBMA$mean_coefs_EBMA_no7)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = upper_EBMA, ymin = lower_EBMA))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+ 
  labs(title = "Model weights of  EBMA")+
  ylab("Model Weights")+
  xlab("Model")


############ Plotting regress.func, EBMA comparison without KRLS

colnames(plotting_data_EBMA)[2] <- "coef"
plotting_data_EBMA$type <- "EBMA"
plotting_data_EBMA <- cbind(plotting_data_EBMA, error_EBMA_no7)
colnames(plotting_data_EBMA)[4] <- "error"

colnames(plotting_data_no7)[2] <- "coef"
plotting_data_no7$type <- "Regression"
plotting_data_no7 <- cbind(plotting_data_no7, error_no7)
colnames(plotting_data_no7)[4] <- "error"

all_data <- rbind(plotting_data_no7, plotting_data_EBMA)

upper<-(all_data$coef+ 1.96*all_data$error)
lower<- (all_data$coef - 1.96*all_data$error)

ggplot(all_data, aes(fill=  type , x = Names_no7, y = coef))  +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin= lower, ymax= upper), width=.2,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))+ 
  labs(title = "Model Weight and Error Comparison ")+
  ylab("Model Weights")+
  xlab("Model")
