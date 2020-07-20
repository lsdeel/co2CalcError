library(tidyverse)
library(readxl)
library(gridExtra)
library(propagate)
library(msm)

# r2 is not good for non-linear.
#https://statisticsbyjim.com/regression/r-squared-invalid-nonlinear-regression/
#https://blog.minitab.com/blog/adventures-in-statistics-2/why-is-there-no-r-squared-for-nonlinear-regression


#set working directory
dir = 'E:/Research/postdoc_proj'


# literature data -------------------------------------------------
litWQ <- read_csv(paste0(dir, '/paper/CO2error/datasets/literatureWaterQuality.csv'))
litWQStratched <- litWQ[(!is.na(litWQ$pH) & !is.na(litWQ$DICuM)) |
                           (!is.na(litWQ$pH) & !is.na(litWQ$alkuM)),]

litWQStratched <- litWQStratched %>% gather('type', 'DIC', DICuM, alkuM)
litWQStratched <- litWQStratched[litWQStratched$DIC < 15000,]

table(!is.na(litWQStratched$pH)& !is.na(litWQStratched$DIC)) # 1727

# write_csv(litWQStratched, paste0(dir, '/datasets/CO2errorsData/pHDIC.csv'))

df <- litWQStratched[(litWQStratched$DIC < 15000) &
                       !is.na(litWQStratched$DIC) &
                       !is.na(litWQStratched$pH),]
fit <- nls(pH ~ y0 + a * exp(b * DIC),
           data = df,
           start = list(y0 = 7.9, a = -3.3, b = -0.004),
           control = list(maxiter = 100), na.action = na.omit)
summary(fit)
# y0 = 8.3 +- 0.03
# a = -2.4 +- 0.05
# b = -0.001 +- 0.00005


#plotting confidence interval for non-linear regressions
#check out the following cross-validated pages
#https://stats.stackexchange.com/questions/85448/shape-of-confidence-and-prediction-intervals-for-nonlinear-regression
#https://stats.stackexchange.com/questions/15423/how-to-compute-prediction-bands-for-non-linear-regression
#https://stats.stackexchange.com/questions/323369/basic-question-about-standard-error-in-predictive-model?noredirect=1&lq=1
#construct the covariance matrix
#https://stats.seandolinar.com/making-a-covariance-matrix-in-r/
# The following webpage is good
#https://stats.idre.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/

yvar<-c()
cv<-vcov(fit)
for (i in 1:length(df$DIC)){
  if (i%%50==1){ print(i)}
  grad<-c(1,exp((-0.001)*df$DIC[i]),(-2.4)*df$DIC[i]*exp((-0.001)*df$DIC[i]))
  fvar<-t(grad)%*%cv%*%grad
  yvar<-c(yvar,fvar[1])
}
#deltamethod(~ x1+x2*exp(x3*5000),coef(fit),vcov(fit))

df<-cbind(df,yvar)
df$upper<-8.3-2.4*exp(-0.001*df$DIC)+2*sqrt(df$yvar)
df$lower<-8.3-2.4*exp(-0.001*df$DIC)-2*sqrt(df$yvar)
df$prediction<-8.3-2.4*exp(-0.001*df$DIC)

mean(sqrt(df$yvar))

p1 <- df %>%
  ggplot(aes(DIC, pH)) +
  geom_point(size = 2, alpha = 0.6, color = 'darkgrey') +
  geom_line(aes(DIC,prediction,linetype='pred'),size=1,color='red')+
  geom_line(aes(DIC,upper,linetype='bound'),size=0.5,color='red')+
  geom_line(aes(DIC,lower,linetype='bound'),size=0.5,color='red')+
  scale_x_continuous(limits = c(0, 10000),
                     breaks = seq(0, 9000, 2000),
                     labels = seq(0, 9000, 2000)) +
  scale_y_continuous(limits = c(4, 10)) +
  scale_linetype_manual(values=c(2,1),labels=c('95% Confidence Interval','Prediction'))+
  theme_classic()+
  xlab(expression(bold('DIC '*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('pH')))+
  ggtitle(expression(bold('(a) Literature Data, N = 1727'))) +
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 20),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.6,0.4),
        legend.title=element_blank(), 
        legend.background = element_blank(),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('1.2', 'cm'),
        legend.key.height = unit('0.7', 'cm'))+ 
  annotate('text', 6000, 5,
           label = 'y = 8.3 - 2.4 exp (-0.001 x)',
           size = 6)

p1


# USGS data --------------------------------------------------------

datapHDICTA <- read_csv(paste0(dir, '/paper/CO2error/datasets/datapHDICTA-USGS.csv'))

fit <- nls(pH ~ y0 + a * exp(b * DICuM),
           data = datapHDICTA,
           start = list(y0 = 7.9, a = -3.3, b = -0.004),
           control = list(maxiter = 100), na.action = na.omit)

summary(fit)
# y0 = 8.1 +- 0.02
# a = -2.4 +- 0.05
# b = -0.001 +- 0.00007

df<-datapHDICTA

yvar<-c()
cv<-vcov(fit)
for (i in 1:length(df$DICuM)){
  if (i%%50==1){ print(i)}
  grad<-c(1,exp((-0.001)*df$DICuM[i]),(-2.4)*df$DICuM[i]*exp((-0.001)*df$DICuM[i]))
  fvar<-t(grad)%*%cv%*%grad
  yvar<-c(yvar,fvar[1])
}

df<-cbind(df,yvar)
df$upper<-8.1-2.4*exp(-0.001*df$DICuM)+2*sqrt(df$yvar)
df$lower<-8.1-2.4*exp(-0.001*df$DICuM)-2*sqrt(df$yvar)
df$prediction<-8.1-2.4*exp(-0.001*df$DICuM)

mean(sqrt(df$yvar))

p2 <- df %>%
  ggplot(aes(DICuM, pH)) +
  geom_point(size = 2, alpha = 0.6, color = 'darkgrey') +
  geom_line(aes(DICuM,prediction,linetype='pred'),size=1,color='red')+
  geom_line(aes(DICuM,upper,linetype='bound'),size=0.5,color='red')+
  geom_line(aes(DICuM,lower,linetype='bound'),size=0.5,color='red')+
  scale_x_continuous(limits = c(0, 8500)) +
  scale_y_continuous(limits = c(4, 10)) +
  scale_linetype_manual(values=c(2,1),labels=c('95% Confidence Interval','Prediction'))+
  theme_classic() +
  xlab(expression(bold('DIC '*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  ggtitle(expression(bold('(b) USGS, N = 1345'))) +
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 20),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.6,0.4),
        legend.title=element_blank(), 
        legend.background = element_blank(),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('1.2', 'cm'),
        legend.key.height = unit('0.7', 'cm')) +
  annotate('text', 4500, 5,
           label = 'y = 8.1 - 2.4 exp (-0.001 x)',
           size = 6)

p2

fit <- nls(pH ~ y0 + a * exp(b * TAuM),
           data = datapHDICTA,
           start = list(y0 = 7.9, a = -3.3, b = -0.004),
           control = list(maxiter = 100), na.action = na.omit)

summary(fit)
# y0 = 8.1 +- 0.02
# a = -2.5 +- 0.05
# b = -0.001 +- 0.00007

p3 <- datapHDICTA %>%
  ggplot(aes(TAuM, pH)) +
  geom_point(size = 2, alpha = 0.6, color = 'darkgrey') +
  geom_smooth(method = "nls",
              formula = y ~ y0 + a * exp(b*x),
              method.args = list(start = list(y0 = 7.9, a = -3.3, b = -0.004),
                                 control = list(maxiter = 100)),
              color = 'red', size = 2, linetype = 5,
              se = FALSE) +
  scale_x_continuous(limits = c(0, 8500)) +
  scale_y_continuous(limits = c(4, 10)) +
  theme_classic() +
  xlab(expression(bold(italic('TA ')*' ('*mu*'mol '*L^-1*')'))) +
  ylab(expression(bold(italic('pH')))) +
  ggtitle(expression(bold('(b) USGS, N = 1345'))) +
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 20),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 20),
        legend.key.size = unit('0.5', 'cm'),
        legend.key.height = unit('1', 'cm')) +
  annotate('text', 6000, 5,
           label = 'y = 8.1 - 2.5 exp (-0.001x)',
           size = 6)

p3

# ggsave(paste(dir, 'output', 'figure', 'CO2error' , 'pHDIC.png', sep = '/'),
#        plot = grid.arrange(p1, p2, nrow = 1),
#        width = 30, height = 12, units = 'cm', device = 'png')



