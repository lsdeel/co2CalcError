library(tidyverse)
library(readxl)
library(gridExtra)

#set working directory
dir = 'E:/Research/postdoc_proj'

# literature data ----------------------------------------------------------
litWQ <- read_csv(paste0(dir, '/paper/CO2error/datasets/literatureWaterQuality.csv'))

# 730 rows overlaped
table(!is.na(litWQ$DOCmgL) & !is.na(litWQ$DICuM))
litWQ$DOCuM <- litWQ$DOCmgL / 12 * 1000

litWQ[!is.na(litWQ$DOCuM) & !is.na(litWQ$DICuM),] %>% 
  group_by(reference) %>% summarise(
    n = n()
  )

fit <- nls(DOCuM ~ y0 + a * exp(b * DICuM),
           data = litWQ[!is.na(litWQ$DOCuM) & !is.na(litWQ$DICuM),],
           start = list(y0 = 331, a = 1500, b = -0.001),
           control = list(maxiter = 100), na.action = na.omit)

summary(fit)
# y0 = 331 +- 91
# a = 1562 +- 110
# b = -0.0012 +- 0.0003

df<-litWQ[!is.na(litWQ$DOCuM) & !is.na(litWQ$DICuM),]
yvar<-c()
cv<-vcov(fit)
for (i in 1:length(df$DICuM)){
  if (i%%50==1){ print(i)}
  grad<-c(1,exp((-0.001)*df$DICuM[i]),(-2.4)*df$DICuM[i]*exp((-0.001)*df$DICuM[i]))
  fvar<-t(grad)%*%cv%*%grad
  yvar<-c(yvar,fvar[1])
}

df<-cbind(df,yvar)
df$upper<-331+1562*exp(-0.0012*df$DICuM)+1.96*sqrt(df$yvar)
df$lower<-331+1562*exp(-0.0012*df$DICuM)-1.96*sqrt(df$yvar)
df$prediction<-331+1562*exp(-0.0012*df$DICuM)
mean(sqrt(df$yvar))
mean(df$DOCuM)

p1 <- df %>%
  ggplot(aes(DICuM, DOCuM)) + 
  geom_point(alpha = 0.6, size = 2, color = 'darkgrey') +
  geom_line(aes(DICuM,upper,linetype='bound'),size=0.5,color='red')+
  geom_line(aes(DICuM,lower,linetype='bound'),size=0.5,color='red')+
  geom_line(aes(DICuM,prediction,linetype='pred'),size=1,color='red')+
  geom_vline(xintercept=1000,linetype='dashed')+
  geom_hline(yintercept=1000,linetype='dashed')+
  scale_linetype_manual(values=c(2,1),labels=c('95% Confidence Interval','Prediction'))+
  theme_classic() +
  annotate(geom='point',x=3.07/12*1000,y=26.2/12*1000,color='blue',size=3)+#Dyson2011
  annotate(geom='point',x=1.22/12*1000,y=17.5/12*1000,color='blue',size=3)+#Dyson2011
  annotate(geom='point',x=3.1/12*1000,y=4.7/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.9/12*1000,y=3.4/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=3.0/12*1000,y=4.0/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.4/12*1000,y=3.2/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.2/12*1000,y=3.2/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.4/12*1000,y=1.6/12*1000,color='gold1',size=3)+#Giesler2014
  xlab(expression(bold('DIC '*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('DOC '*'('*italic(mu)*'mol '*L^-1*')'))) +
  ggtitle(expression(bold('(a) Literature Data, N = 730'))) +
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 20),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.6,0.6),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('1.2', 'cm'),
        legend.key.height = unit('0.7', 'cm')) +
  annotate('text', 3800, 6500,
           label = 'y = 331 + 1562 exp(-0.0012 x)',
           size = 6)
p1


# USGS data ---------------------------------------------------------------


dataDOCDICTA <- read_csv(paste0(dir, '/paper/CO2error/datasets/dataDOCDICTA-USGS.csv'))

fit <- nls(DOCuM ~ y0 + a * exp(b * DICuM),
           data = dataDOCDICTA,
           start = list(y0 = 331, a = 1500, b = -0.001),
           control = list(maxiter = 100), na.action = na.omit)

summary(fit)
# y0 = 381 +- 17
# a = 1516 +- 110
# b = -0.0038 +- 0.0008

df<-dataDOCDICTA
yvar<-c()
cv<-vcov(fit)
for (i in 1:length(df$DICuM)){
  if (i%%50==1){ print(i)}
  grad<-c(1,exp((-0.001)*df$DICuM[i]),(-2.4)*df$DICuM[i]*exp((-0.001)*df$DICuM[i]))
  fvar<-t(grad)%*%cv%*%grad
  yvar<-c(yvar,fvar[1])
}

df<-cbind(df,yvar)
df$upper<-381+1516*exp(-0.0038*df$DICuM)+1.96*sqrt(df$yvar)
df$lower<-381+1516*exp(-0.0038*df$DICuM)-1.96*sqrt(df$yvar)
df$prediction<-381+1516*exp(-0.0038*df$DICuM)
mean(sqrt(df$yvar))
mean(df$DOCuM)

p2 <- df %>%
  ggplot(aes(DICuM, DOCuM)) + 
  geom_point(alpha = 0.6, size = 2, color = 'darkgrey') +
  geom_line(aes(DICuM,upper,linetype='bound'),size=0.5,color='red')+
  geom_line(aes(DICuM,lower,linetype='bound'),size=0.5,color='red')+
  geom_line(aes(DICuM,prediction,linetype='pred'),size=1,color='red')+
  geom_vline(xintercept=1000,linetype='dashed')+
  geom_hline(yintercept=1000,linetype='dashed')+
  annotate(geom='point',x=3.07/12*1000,y=26.2/12*1000,color='blue',size=3)+#Dyson2011
  annotate(geom='point',x=1.22/12*1000,y=17.5/12*1000,color='blue',size=3)+#Dyson2011
  annotate(geom='point',x=3.1/12*1000,y=4.7/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.9/12*1000,y=3.4/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=3.0/12*1000,y=4.0/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.4/12*1000,y=3.2/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.2/12*1000,y=3.2/12*1000,color='gold1',size=3)+#Giesler2014
  annotate(geom='point',x=2.4/12*1000,y=1.6/12*1000,color='gold1',size=3)+#Giesler2014
  theme_classic() +
  scale_linetype_manual(values=c(2,1),labels=c('95% Confidence Interval','Prediction'))+
  xlab(expression(bold('DIC '*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('DOC'*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ggtitle(expression(bold('(b) USGS, N = 717'))) +
  scale_x_continuous(limits = c(0, 9000),
                     breaks = seq(0, 9000, by = 2000),
                     labels = format(seq(0, 9000, by = 2000))) +
  scale_y_continuous(limits = c(0, 4000),
                     breaks = seq(0, 4000, by = 1000)) +
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 20),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.6,0.6),
        legend.title = element_blank(),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('1.2', 'cm'),
        legend.key.height = unit('0.7', 'cm')) +
  annotate('text', 4500, 3500,
           label = 'y = 381 + 1516 exp(-0.0038 x)',
           size = 6)
p2

p <- grid.arrange(p1, p2, nrow = 1)

# ggsave(paste(dir, 'output', 'figure', 'CO2error', 'docDIC.png', sep = '/'),
#        plot = p, width = 30, height = 12, units = 'cm', device = 'png')




