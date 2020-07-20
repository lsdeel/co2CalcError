library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(tidyr)

dir = 'D:/Research/postdoc_proj'

litWQ <- read_csv(paste0(dir, '/paper/CO2error/datasets/literatureWaterQuality.csv'))
datapHDICTA <- read_csv(paste0(dir, '/paper/CO2error/datasets/datapHDICTA-USGS.csv'))
OrgCarb <- read_csv(paste0(dir, '/paper/CO2error/datasets/dataDOCDICTA-USGS.csv'))

litWQ$OrgAlk_perc<-litWQ$DOCmgL/12*1000*0.08/litWQ$DICuM*100
litWQ[litWQ$OrgAlk_perc>=100 & !is.na(litWQ$OrgAlk_perc),]$OrgAlk_perc<-NA
table(!is.na(litWQ$DICuM) & !is.na(litWQ$alkuM)) # A total of 425 samples

mean(litWQ$DOCmgL/12*1000*0.08,na.rm=TRUE)
# mean(datapHDICTA$/12*1000*0.08,na.rm=TRUE)
mean(litWQ$alkuM,na.rm=TRUE)
mean(datapHDICTA$TAuM,na.rm=TRUE)


# litWQ[!is.na(litWQ$DOCmgL) & !is.na(litWQ$DICuM),] %>% View()
fit <- lm(alkuM ~ DICuM, 
          data = litWQ[!is.na(litWQ$DICuM) & !is.na(litWQ$alkuM),] )

summary(fit)
# Intercept = 35 +- 15
# a = 0.97 +- 0.008

p1 <- litWQ[!is.na(litWQ$OrgAlk_perc),] %>%
  ggplot(aes(DICuM, alkuM, color=log(OrgAlk_perc))) +
  geom_point(size = 4, alpha = 1) +
  geom_smooth(method = lm,color='grey') +
  scale_x_continuous(limits = c(0, 8000)) +
  scale_y_continuous(limits = c(0, 8000)) +
  theme_classic() +
  xlab(expression(bold('DIC '*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('TA '*' ('*italic(mu)*'eq '*L^-1*')'))) +
  labs(color=expression('Ln OrgAlk %'))+
  ggtitle(expression('Literature Data')) +
  scale_color_gradient2(low='skyblue1',mid='skyblue3',high='red',na.value='grey20')+
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.8,0.3),
        legend.title = element_text(size=12),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('0.5', 'cm'),
        legend.key.height = unit('0.5', 'cm')) +
  annotate('text', 2500, 7000,
           label = expression('y = 0.97x + 35, '*R^2*' = 0.97'),
           size = 6)

p1

fit <- lm(TAuM ~ DICuM, 
          data = datapHDICTA)

summary(fit)
# Intercept = 35 +- 15
# a = 0.97 +- 0.008


datapHDICTA<-left_join(datapHDICTA,OrgCarb[,c('site_no','sample_dt','DOCmgL')],
                       by=c("site_no"="site_no","sample_dt"="sample_dt"))
datapHDICTA$OrgAlk_perc<-datapHDICTA$DOCmgL/12*1000*0.08/datapHDICTA$TAuM*100

p2 <- datapHDICTA %>%
  ggplot(aes(DICuM, TAuM,color=log(OrgAlk_perc))) +
  geom_point(size = 4, alpha = 0.4) +
  geom_smooth(method = lm,color='grey') +
  # scale_x_continuous(limits = c(0, 6000)) +
  # scale_y_continuous(limits = c(4, 10)) +
  scale_color_gradient2(low='skyblue1',mid='skyblue3',high='red',na.value='grey20')+
  theme_classic() +
  xlab(expression(bold('DIC '*' ('*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('TA '*' ('*italic(mu)*'eq '*L^-1*')'))) +
  labs(color=expression('Ln OrgAlk %'))+
  ggtitle(expression('USGS')) +
  theme(title = element_text(size = 10),
        axis.title = element_text(size = 20, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.8,0.3),
        legend.title = element_text(size=12),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('0.5', 'cm'),
        legend.key.height = unit('0.5', 'cm')) +
  annotate('text', 2500, 7000,
           label = expression('y = 0.94x + 19, '*R^2*' = 0.93'),
           size = 6)

p2

p <- grid.arrange(p1, p2, nrow = 1)

ggsave(paste(dir, 'output', 'figure', 'CO2error' , 'DICalk.png', sep = '/'),
       plot = p, width = 30, height = 12, units = 'cm', device = 'png')


#join Stream Order
gauges<-read_csv(paste0('D:/research/globalEmission/dataset/NHDplus/NHDplus_Gage.csv'))
SO<-read_csv(paste0('D:/research/globalEmission/dataset/NHDplus/NHDFlowline_Network_1.csv'))
gauges<-left_join(gauges[,c('FLComID','FEATUREDET','DASqKm')],SO[,c('COMID','StreamOrde')],by=c('FLComID'='COMID'))
rm(SO)
gauges<-gauges%>%separate(FEATUREDET,into=c('FEATUREDET','site_no'), sep='=')
gauges$site_no<-as.numeric(gauges$site_no)
datapHDICTA<-left_join(datapHDICTA,gauges,by='site_no')
rm(gauges)

#join prec
prec<-read_csv(paste0('D:/research/globalEmission/output/table/earthEnv_variables/earthEnv_upstream.csv'))
# prec$site_no<-as.character(prec$site_no)
# prec[nchar(prec$site_no)%in%c(7),]$site_no<-paste0('0',prec[nchar(prec$site_no)%in%c(7),]$site_no)
prec<-prec[,c('site_no','flow_accumulation2','hydro_avg12')]
prec$flow_accumulation2<-as.numeric(prec$flow_accumulation2)
prec$hydro_avg12<-as.numeric(prec$hydro_avg12)
prec$prec<-prec$hydro_avg12/prec$flow_accumulation2 #mm/yr
datapHDICTA<-left_join(datapHDICTA,prec[,c('site_no','prec')],by='site_no')

datapHDICTA<-datapHDICTA[datapHDICTA$DASqKm>0 & !is.na(datapHDICTA$DASqKm),]
datapHDICTA<-
  datapHDICTA%>%mutate(
    tag=case_when(DASqKm<10~10,
                  # DASqKm>10&DASqKm<=100~100,
                  DASqKm>10&DASqKm<=1000~1000,
                  DASqKm>1000&DASqKm<=10000~10000,
                  DASqKm>10000&DASqKm<=100000~100000,
                  DASqKm>100000&DASqKm<=1000000~1000000,
                  DASqKm>1000000&DASqKm<=10000000~100000000),
    tagp=case_when(prec<=500~'<500',
                   prec>500&prec<=1000~'500-1000',
                   prec>1000&prec<=1500~'1000-1300'))


datapHDICTA%>%group_by(tag)%>%
  summarise(n=n(),
            n_m=sum(TAuM<1000),
            perc=n_m/n*100)


datapHDICTA%>%group_by(tagp)%>%
  summarise(n=n(),
            n_m=sum(TAuM<1000),
            perc=n_m/n*100)




