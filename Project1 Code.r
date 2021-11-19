#Soyoung Chung(soyoungchung625@gmail.com)

# PartA
getwd()
wdir <- "/Users/soyoungchung/Desktop"
setwd(wdir)

PartA_IV <- read.csv('P1A_IV46738.csv', header = TRUE)
PartA_DV <- read.csv('P1A_DV46738.csv', header = TRUE)
#merge two file
PartA <- merge(PartA_IV, PartA_DV, by = 'ID')

str(PartA)
##output
# 'data.frame':    780 obs. of  3 variables:
# $ ID: int  1 2 3 4 5 6 7 8 9 10 ...
# $ IV: num  8.15 4.13 2.68 5.19 6.31 ...
# $ DV: num  NA 18.2 22.2 NA 44.6 ...

# count the number of subject ID that has some conditions are done by Excel
PartA_incomplete <- PartA
#install.packages('mice')
library(mice)
md.pattern(PartA_incomplete)
##output
#    ID IV  DV
#575  1  1   1   0
#158  1  1   0   1
#41   1  0   1   1
#6    1  0   0   2
#     0 47 164 211

PartA_imp <- PartA[!is.na(PartA$IV)==TRUE|!is.na(PartA$DV)==TRUE,]
imp <- mice(PartA_imp, method = "norm.boot", printFlag = FALSE)
PartA_complete <- complete(imp)

md.pattern(PartA_complete)

##output
# /\     /\
#{  `---'  }
#{  O   O  }
#==>  V <==  No need for mice. This data set is completely observed.
# \  \|/  /
#  `-----'

#    ID IV DV
#774  1  1  1 0
#     0  0  0 0

M <- lm(DV ~ IV, data=PartA_complete)
summary(M)

##output
#Call:
#lm(formula = DV ~ IV, data = PartA_complete)

#Residuals:
#     Min       1Q   Median       3Q      Max
#-27.3343  -5.0359  -0.0891   5.1273  20.4025

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)
#(Intercept)  12.7544     0.8484   15.03   <2e-16 ***
#IV            3.9158     0.1357   28.87   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.36 on 772 degrees of freedom
#Multiple R-squared:  0.5191,    Adjusted R-squared:  0.5185
#F-statistic: 833.3 on 1 and 772 DF,  p-value: < 2.2e-16

# install.packages('knitr')
library(knitr)
kable(anova(M), caption='ANOVA Table')
##output is in report

plot(PartA_complete$DV ~ PartA_complete$IV, main='Scatter : DV ~ IV', xlab='IV', ylab='DV', pch=20)
abline(M, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')

#getting 95% confidence interval for the slope
confint(M, level = 0.95)
##output
#              2.5 %   97.5 %
#(Intercept) 11.088935 14.419881
#IV           3.649503  4.182081

#getting 99% confidence interval for the slope
confint(M, level = 0.99)

##output
#              0.5 %    99.5 %
#(Intercept) 10.563621 14.945195
#IV           3.565512  4.266072

#PartB
data <- read.csv('P1B46738.csv', header = TRUE)
#transformate the data
data_trans <- data.frame(xtrans=data$x, ytrans=data$y^(-3/2))
# some of my data's difference is less than 0.01, so I used 0.05 as an interval
groups <- cut(data_trans$xtrans,breaks=c(-Inf,seq(min(data_trans$xtrans)+0.005, max(data_trans$xtrans)-0.005,by=0.005),Inf))
table(groups)

##output
#groups
#(-Inf,1.107] (1.107,1.112] (1.112,1.117] (1.117,1.122] (1.122,1.127] (1.127,1.132] (1.132,1.137]
#            6             3             6             8             5             5             8
#(1.137,1.142] (1.142,1.147] (1.147,1.152] (1.152,1.157] (1.157,1.162] (1.162,1.167] (1.167,1.172]
#            9             5             6             4             7             6             3
#(1.172,1.177] (1.177,1.182] (1.182,1.187] (1.187,1.192] (1.192,1.197] (1.197,1.202] (1.202,1.207]
#           5            10             7             2             1             1             5
#(1.207,1.212] (1.212,1.217] (1.217,1.222] (1.222,1.227] (1.227,1.232] (1.232,1.237] (1.237,1.242]
#            3             2             5             4             7             6             7
#(1.242,1.247] (1.247,1.252] (1.252,1.257] (1.257,1.262] (1.262,1.267] (1.267,1.272] (1.272,1.277]
#            5             3             6             5             7             5             4
#(1.277,1.282] (1.282,1.287] (1.287,1.292] (1.292,1.297] (1.297,1.302] (1.302,1.307] (1.307,1.312]
#            6             9             3            10             3             5             3
#(1.312,1.317] (1.317,1.322] (1.322,1.327] (1.327,1.332] (1.332,1.337] (1.337,1.342] (1.342,1.347]
#            3             7             3             1             3             6             4
#(1.347,1.352] (1.352,1.357] (1.357,1.362] (1.362,1.367] (1.367,1.372] (1.372,1.377] (1.377,1.382]
#            4             4            11             3             6             4             4
#(1.382,1.387] (1.387,1.392] (1.392,1.397] (1.397,1.402] (1.402,1.407] (1.407,1.412] (1.412,1.417]
#            5             5             6             6             7             7             5
#(1.417,1.422] (1.422,1.427] (1.427,1.432] (1.432,1.437] (1.437,1.442] (1.442,1.447] (1.447,1.452]
#            2             6             4             5             4             6             3
#(1.452,1.457] (1.457,1.462] (1.462,1.467] (1.467,1.472] (1.472,1.477] (1.477,1.482] (1.482,1.487]
#            5             7             5             3             9             3             4
#(1.487,1.492] (1.492,1.497] (1.497,1.502] (1.502,1.507] (1.507,1.512] (1.512,1.517] (1.517,1.522]
#            1             4             2             3             7             3             5
#(1.522,1.527] (1.527,1.532] (1.532,1.537] (1.537,1.542] (1.542,1.547] (1.547,1.552] (1.552,1.557]
#            7             3             4             8             8             4             2
#(1.557,1.562] (1.562,1.567] (1.567,1.572] (1.572,1.577] (1.577,1.582] (1.582,1.587] (1.587,1.592]
#            1             3             8             5             4             6             3
#(1.592, Inf]
#            9

x <- ave(data_trans$xtrans, groups)
data_bin <- data.frame(x=x, y=data_trans$ytrans)

#install.packages('alr3')
library(alr3)
fit_b <- lm(y ~ x, data = data_bin)
#apply the Lack of Fit test.
pureErrorAnova(fit_b)
##output
#Analysis of Variance Table
#
#Response: y
#              Df Sum Sq Mean Sq  F value Pr(>F)
#x              1 520.97  520.97 797.9299 <2e-16 ***
#Residuals    488 302.16    0.62
#Lack of fit  97  46.88    0.48   0.7402 0.9628
#Pure Error  391 255.29    0.65
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# The LOF F value is 0.7402, showing that there is no significant lack of fit.
