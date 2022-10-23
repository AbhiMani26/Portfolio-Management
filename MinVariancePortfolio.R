
library(PCRA)
library(RobStatTM)
library(tidyverse)
library(dplyr)
library(fpp2)
library(tidyquant)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggthemes)
library(curl)
library(readxl)
library(cowplot)
library(ggpubr)
library(plotly)
library(pastecs)
library(kableExtra)
library(broom)
install.packages("fPortfolio")
library(fPortfolio)

#each item of the list denotes a different industry
mr_fm.data <- read.table("a2_indu.csv", header=TRUE,sep=",",as.is=TRUE)
mr_factor_fm.data <- read.table("a2_5factors.csv", header=TRUE,sep=",",as.is=TRUE)

#removes month from the data
mr_fm.data <- mr_fm.data[-1]
industry_return_date_data <- mr_fm.data
mr_factor_fm.data <- mr_factor_fm.data[-1]

industry_matrix <- do.call("cbind",mr_fm.data)
factor_matrix <- do.call("cbind",mr_factor_fm.data)

#Calculating excess for each element
for (i in 1:nrow(industry_matrix))   
  for (j in 1:ncol(industry_matrix))  
    industry_matrix[i,j] <-  industry_matrix[i,j] - factor_matrix[i,1]




#Question1
data <- 1:196
coff_matrix <- matrix(data, ncol = 4, byrow = FALSE)
colnames(coff_matrix) <- c("alpha", "beta", "t-stat","r-square")

for(j in 1:ncol(industry_matrix)){
  result2 <- lm(industry_matrix[,j]~factor_matrix[,3])
  coff_result <- summary(result2)
  coff_matrix[j,1] <- coff_result$coefficients[1]
  coff_matrix[j,2] <- coff_result$coefficients[2]
  coff_matrix[j,3] <- coff_result$coefficients[1,3]
  coff_matrix[j,4] <- coff_result$adj.r.squared
}

#Coffecient Matrix
coff_matrix
#bar plot of alpha
barplot(coff_matrix[,1])
#bar plot of beta beta(intercept)
barplot(coff_matrix[,2])
#bar plot of alpha's t-stat
barplot(coff_matrix[,3])


#Question2
data <- 1:294
coff_matrix_multi_reg <- matrix(data, ncol = 6, byrow = FALSE)
colnames(coff_matrix_multi_reg) <- c("alpha", "beta","betaSBM","betaHML", "t-stat","r-square")
coff_matrix_multi_reg

for(j in 1:ncol(industry_matrix)){
  result3 <- lm(industry_matrix[,j]~factor_matrix[,3] + factor_matrix[,4] + factor_matrix[,5])
  coff_result <- summary(result3)
  coff_matrix_multi_reg[j,1] <- coff_result$coefficients[1]
  coff_matrix_multi_reg[j,2] <- coff_result$coefficients[2]
  coff_matrix_multi_reg[j,3] <- coff_result$coefficients[3]
  coff_matrix_multi_reg[j,4] <- coff_result$coefficients[4]
  coff_matrix_multi_reg[j,5] <- coff_result$coefficients[1,3]
  coff_matrix_multi_reg[j,6] <- coff_result$adj.r.squared
}

#Coffecient Matrix
coff_matrix_multi_reg
#bar plot of alpha
barplot(coff_matrix[,1])
#bar plot of beta-Mkt.Rf
barplot(coff_matrix[,2])
#bar plot of beta-SBM
barplot(coff_matrix[,3])
#bar plot of beta-HML
barplot(coff_matrix[,4])
#bar plot of alpha's t-stat
barplot(coff_matrix[,5])


#Question 3
data <- 1:392
coff_matrix_multi_reg2 <- matrix(data, ncol = 8, byrow = FALSE)
colnames(coff_matrix_multi_reg2) <- c("alpha", "beta","betaSBM","betaHML","betaRMW","betaCMA", "t-stat","r-square")

for(j in 1:ncol(industry_matrix)){
  result4 <- lm(industry_matrix[,j]~factor_matrix[,3] + factor_matrix[,4] + factor_matrix[,5] +  factor_matrix[,6] + factor_matrix[,7])
  summary(result4)
  coff_result2 <- summary(result4)
  coff_matrix_multi_reg2[j,1] <- coff_result2$coefficients[1]
  coff_matrix_multi_reg2[j,2] <- coff_result2$coefficients[2]
  coff_matrix_multi_reg2[j,3] <- coff_result2$coefficients[3]
  coff_matrix_multi_reg2[j,4] <- coff_result2$coefficients[4]
  coff_matrix_multi_reg2[j,5] <- coff_result2$coefficients[5]
  coff_matrix_multi_reg2[j,6] <- coff_result2$coefficients[6]
  coff_matrix_multi_reg2[j,7] <- coff_result2$coefficients[1,3]
  coff_matrix_multi_reg2[j,8] <- coff_result2$adj.r.squared
}

#Coeffcient Matrix-Problem3
coff_matrix_multi_reg2
#bar plot of alpha
barplot(coff_matrix_multi_reg2[,1])
#bar plot of beta-Mkt.Rf
barplot(coff_matrix_multi_reg2[,2])
#bar plot of beta-SBM
barplot(coff_matrix_multi_reg2[,3])
#bar plot of beta-HML
barplot(coff_matrix_multi_reg2[,4])
#bar plot of beta-RMW
barplot(coff_matrix_multi_reg2[,5])
#bar plot of beta-CMA
barplot(coff_matrix_multi_reg2[,6])
#bar plot of alpha's t-stat
barplot(coff_matrix_multi_reg2[,7])


#Question4 Mean Variance Optimization Portfolio
library(xts)
industry_matrix_portfolio <- do.call("cbind",industry_return_date_data)
d <- as.Date(1:nrow(industry_matrix_portfolio))
industry_matrix.xts <- xts(industry_matrix_portfolio, order.by=d)
index(industry_matrix.xts)

funds <- colnames(industry_matrix.xts)
portfolio.object <- portfolio.spec(assets = funds)
portfolio.object <- add.constraint(portfolio = portfolio.object,type = "full investment")
portfolio.object <- add.constraint(portfolio = portfolio.object,type = "box",min=0,max=1)
portfolio.object <- add.constraint(portfolio = portfolio.object,type = "long_only")
portfolio.object <- add.objective(portfolio = portfolio.object,type = "risk", name="var")
portfolio.object <- add.objective(portfolio = portfolio.object,type = "return", name="mean")

outport <- optimize.portfolio(R=industry_matrix.xts,portfolio = portfolio.object,optimize_method = "ROI" )

weights_roi <- outport$weights
barplot(weights_roi)

sum <- 0
for(i in 1:72){
  sum = sum + factor_matrix[i,1]
}
avg_monthly_return <- sum/72

return.matrix <- as.timeSeries(industry_matrix.xts)
efficient_frontier <- portfolioFrontier(return.matrix,`setRiskFreeRate<-`(portfolioSpec(),avg_monthly_return),
                                        
                                        constraints = "LongOnly")

#Efficient Frontier,Minimun Risk Frontier,Tangency Portfolio,Risk/Return of Single assets
plot(efficient_frontier,c(1,2,3,4))

#Sharp Ratio
plot(efficient_frontier,c(8))



