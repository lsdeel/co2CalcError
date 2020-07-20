library(tidyverse)
library(readxl)
library(gridExtra)

#set working directory
dir = 'E:/Research/postdoc_proj'


# literature data 
litWQ <- read_csv(paste0(dir, '/paper/CO2error/datasets/literatureWaterQuality.csv'))

dataLitCondDICTA <- litWQ[,c('reference', 'conduScm', 'DICuM', 'alkuM')]
names(dataLitCondDICTA) <- c('reference', 'Cond_uScm', 'DICuM', 'TAuM')
dataLitCondDICTA$Source <- 'Literature'

dataLitCondDICTA <- dataLitCondDICTA[!is.na(dataLitCondDICTA$Cond_uScm) |
                                       !is.na(dataLitCondDICTA$DICuM) |
                                       !is.na(dataLitCondDICTA$TAuM),]

# read USGS data

dataUSGSCondDICTA <- read_excel(paste0(dir, '/paper/CO2error/datasets/dataCondDICTA-USGS.xlsx'))

names(dataUSGSCondDICTA)
dataUSGSCondDICTA <- dataUSGSCondDICTA[,c('site_no', 'Cond_uScm', 'DICuM', 'TAuM')]
dataUSGSCondDICTA$Source <- 'USGS'
names(dataUSGSCondDICTA)[1] <- 'reference'
dataUSGSCondDICTA$reference <- as.character(dataUSGSCondDICTA$reference)

dataCondDICTA <- bind_rows(dataLitCondDICTA, dataUSGSCondDICTA)

# 
# lm_eqn <- function(y, x){
#   m <- lm(y ~ x);
#   eq <- substitute(italic(y) == a(±4.9) + b(±0.002) %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[[1]], digits = 1), 
#                         b = format(coef(m)[[2]], digits = 1), 
#                         r2 = format(summary(m)$r.squared, digits = 2)))
#   as.character(as.expression(eq));                 
# }

# lm_eqn_1 <- function(y, x){
#   m <- lm(y ~ x);
#   eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(b = format(coef(m)[[2]], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 2)))
#   as.character(as.expression(eq));                 
# }



# y = 0.12497 * x + 64, r2 = 0.85, p < 0.001
fit <- lm(Cond_uScm ~ DICuM, data = dataCondDICTA)
summary(fit)
# intercept: 64+-4.6
# a:0.12+-0.002

# DIC versus Cond ----------------------------------------------------


p1 <- dataCondDICTA[!is.na(dataCondDICTA$Cond_uScm) &
                      !is.na(dataCondDICTA$DICuM),] %>% 
  as.tibble() %>% group_by(Source) %>%
  ggplot(aes(DICuM, Cond_uScm, color = Source)) +
  # geom_smooth(method = 'lm', color = 'darkgreen', size = 1.5) +
  geom_abline(slope = 0.121, intercept = 76, color = 'dimgrey', size = 1.5) +
  geom_point(alpha = 0.8, size = 2) + theme_classic() +
  xlab(expression(bold('DIC'*' ( '*italic(mu)*'mol '*L^-1*')'))) +
  ylab(expression(bold('Conductivity'*' ( '*italic(mu)*'S '*cm^-1*')'))) +
  labs(color = 'Data Source') +
  scale_x_continuous(limits = c(0,5500),
                     breaks = seq(0, 4500, by = 1500)) +
  scale_y_continuous(limits = c(-10, 1500)) +
  scale_color_discrete(labels = c('Literature Data (N = 198)', 'USGS (N = 862)')) +
  ggtitle(expression(bold('(a)'))) +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.3, 0.85),
        legend.text=  element_text(size = 14),
        legend.key.size = unit('0.4', 'cm'),
        legend.key.height = unit('0.7', 'cm'),
        legend.title = element_text(size = 14, face = 'bold')
  ) +
geom_text(x = 2000, y = 1000, 
          label = expression('y = 0.12 x + 64, '*R^2*' = 0.85'),
          size = 6, color = 'black')

# p1


# y = 0.124684 * x + 67, r2 = 0.86, r = .93 p < 0.001
fit <- lm(Cond_uScm ~ TAuM, data = dataCondDICTA)
summary(fit)
# intercept:67+-4.9
# 0.12+-0.002

p2 <- dataCondDICTA[!is.na(dataCondDICTA$Cond_uScm) &
                      !is.na(dataCondDICTA$TAuM),] %>% 
  as.tibble() %>% group_by(Source) %>%
  ggplot(aes(TAuM, Cond_uScm, color = Source)) +
  # geom_smooth(method = 'lm', color = 'darkgreen', size = 1.5) +
  geom_abline(slope = 0.125, intercept = 67, color = 'dimgrey', size = 1.5) +
  geom_point(alpha = 0.8, size = 2) + theme_classic() +
  xlab(expression(bold('TA'*' ( '*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('Conductivity'*' ( '*italic(mu)*'S '*cm^-1*')'))) +
  labs(color = 'Data Source') +
  scale_x_continuous(limits = c(0,5500),
                     breaks = seq(0, 4500, by = 1500)) +
  scale_y_continuous(limits = c(-10, 1500)) +
  scale_color_discrete(labels = c('Literature Data (N = 57)', 'USGS (N = 862)')) +
  ggtitle(expression(bold('(b)'))) +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.3, 0.85),
        legend.text=  element_text(size = 14),
        legend.key.size = unit('0.4', 'cm'),
        legend.key.height = unit('0.7', 'cm'),
        legend.title = element_text(size = 14, face = 'bold')
  ) +
  geom_text(x = 2000, y = 1000, 
            label = expression('y = 0.12 x + 67, '*R^2*' = 0.86'),
            size = 6, color = 'black')

# p2

# p <- grid.arrange(p1, p2, ncol = 2)

ggsave(paste(dir, 'output', 'figure', 'CO2error', 'CondDICTA.png', sep = '/'),
       plot = grid.arrange(p1, p2, ncol = 2), width = 28, height = 12, units = 'cm', device = 'png')


