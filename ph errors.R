library(tidyverse)
library(readxl)
library(gridExtra)

#set working directory
dir = 'E:/Research/postdoc_proj'

# read pH error file
pHerror <- read_excel(paste0(dir, '/paper/CO2error/datasets/pHErrorIonicStrength.xlsx'), 'pH error')

sapply(pHerror, class) # correct type
pHerror$waterType <- stringr::str_wrap(pHerror$waterType, 20)

pHerror$waterType_f <- factor(pHerror$waterType,
                              levels = c('(a) Standard Buffer', '(b) Diluted Buffer',
                                         '(c) Freshwater', '(d) Diluted Acid',
                                         '(e) Distilled Water', '(f) Standard Buffer\n(Stirring Effect)',
                                         '(g) Diluted Buffer\n(Stirring Effect)', '(h) Freshwater\n(Stirring Effect)',
                                         '(i) Diluted Acid\n(Stirring Effect)', '(j) Distilled Water\n(Stirring Effect)'),
                              ordered = TRUE)

levels(pHerror$waterType_f)

pHerrorSum <- pHerror %>% group_by(waterType_f) %>%
  summarise(
    n = n(),
    mean = round(mean(phError),2),
    sd = round(sd(phError),2),
    min = min(phError),
    max = max(phError)
  )

pHerrorSum

# pHerrorSum$ionStrength = c(0.1, 0.00865, 0.000278, 0.0004, NA, NA, NA, NA, NA, NA)
pHerrorSum$ionStrength = c(0.075, 0.0106, 0.0004, 0.0001, NA, NA, NA, NA, NA, NA)
pHerrorSum$logIonStrength = log10(pHerrorSum$ionStrength)
pHerrorSum$label <- c('Standard Buffer', 'Diluted Buffer', 
                      'Freshwater', 'Diluted Acid', 'Distilled Water')

lm(pHerrorSum$mean ~ pHerrorSum$logIonStrength)
summary(lm(pHerrorSum$mean ~ pHerrorSum$logIonStrength))
# intercept:0.03+-0.03)
#a: 0.05+-0.01

# dealing with the stirring effect
pHerrorStirring <- pHerror[grepl('Effect', pHerror$waterType),]
pHerrorStirringSum <- pHerrorStirring %>% group_by(waterType_f) %>%
  summarise(
    n = n(),
    mean = round(mean(phError),2),
    sd = round(sd(phError),2),
    min = min(phError),
    max = max(phError)
  )

# pHerrorStirringSum$ionStrength = c(0.1, 0.00865, 0.000278, 0.0004, NA)
pHerrorStirringSum$ionStrength = c(0.075, 0.0106, 0.0004, 0.0001, NA)
pHerrorStirringSum$logIonStrength = log10(pHerrorStirringSum$ionStrength)
pHerrorStirringSum$label <- c('Standard Buffer', 'Diluted Buffer', 
                      'Freshwater', 'Diluted Acid', 'Distilled Water')

lm(pHerrorStirringSum$mean ~ pHerrorStirringSum$logIonStrength)
summary(lm(pHerrorStirringSum$mean ~ pHerrorStirringSum$logIonStrength))
#intercept:0.03+-0.02
#a:0.003L0.006

lm_eqn <- function(y, x){
  m <- lm(y ~ x);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~R^2~"="~r2, 
                   list(a = format(coef(m)[[1]], digits = 1), 
                        b = format(coef(m)[[2]], digits = 1), 
                        r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}

p1 <- pHerrorSum %>% as.tibble() %>%
  ggplot(aes(logIonStrength, mean, label = label)) + 
  stat_smooth(method = 'lm', size = 1.5, fullrange=TRUE) +
  geom_pointrange(aes(ymin = mean - sd / sqrt(n), 
                      ymax = mean + sd / sqrt(n)), 
                  color = 'grey50', size = 1) +
  geom_text(aes(label = label), hjust = -0.1, nudge_y = -0.03, angle = -90,
            size = 4) +
  geom_text(aes(label=n),nudge_y=0.07,color='black',face='bold',size=5)+
  xlab(expression(bold('log Ionic Strength (mol '*L^-1*')'))) +
  ylab(expression(bold('pH bias'))) +
  labs(title = expression(bold('(a) pH Measurement Error'))) +
  scale_x_continuous(limits = c(-6, -0.5),
                     breaks = seq(-5, -1, by = 1)) +
  scale_y_continuous(limits = c(-0.7, 0.2),
                     breaks = seq(-0.6, 0.2, by = 0.2),
                     labels = function(x){round(x, 1)}) +
  theme_classic() + 
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 18),
        legend.key.size = unit('0.5', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 18, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  ) +
  geom_text(x = -4, y = 0.1, 
            label = lm_eqn(pHerrorSum$mean, pHerrorSum$logIonStrength),
            parse = TRUE, size = 5) +
  annotate('pointrange', x = -6, y = -0.45, ymin = -0.45 - 0.2, ymax = -0.45 + 0.2,
                color = 'red', size = 1, fill = 'white') +
  geom_text(x = -5.3, y = -0.45, label = 'Distilled Water', size = 4)+
  geom_text(x=-5.8,y=-0.37,label=16, color='black',size=5)

p1

# ggsave(paste(dir, 'output', 'figure', 'CO2error', 'pherrorIonStrength.png', sep = '/'),
#        plot = p1, width = 16, height = 12, units = 'cm', device = 'png')


# Figure for pHerrorStirringEffectIonStrengh

p2 <- pHerrorStirringSum %>% as.tibble() %>%
  ggplot(aes(logIonStrength, mean, label = label)) + 
  stat_smooth(method = 'lm', size = 1.5, fullrange=TRUE) +
  geom_pointrange(aes(ymin = mean - sd / sqrt(n), 
                      ymax = mean + sd / sqrt(n)), 
                  color = 'grey50', size = 1) +
  geom_text(aes(label = label), hjust = -0.1, nudge_y = -0.03, angle = -90,
            size = 4) +
  geom_text(aes(label=n),nudge_y=0.07,color='black',face='bold',size=5)+
  xlab(expression(bold('log Ionic Strength (mol '*L^-1*')'))) +
  ylab(expression(bold('pH bias'))) +
  labs(title = expression(bold('(b) Stirring Effect'))) +
  scale_x_continuous(limits = c(-6, -0.5),
                     breaks = seq(-5, -1, by = 1)) +
  scale_y_continuous(limits = c(-0.7, 0.2),
                     breaks = seq(-0.6, 0.2, by = 0.2),
                     labels = function(x){round(x, 1)}) +
  theme_classic() + 
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 18),
        legend.key.size = unit('0.5', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 18, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  ) +
  geom_text(x = -4, y = 0.12, 
            label = lm_eqn(pHerrorStirringSum$mean, pHerrorStirringSum$logIonStrength),
            parse = TRUE, size = 5) +
  annotate('pointrange', x = -6, y = -0.15, ymin = -0.15 - 0.13, ymax = -0.15 + 0.13,
           color = 'red', size = 1, fill = 'white') +
  geom_text(x = -5.2, y = -0.15, label = 'Distilled Water', size = 4)+
  geom_text(x=-5.9,y=-0.08,label=8, color='black',size=5)

p2

# ggsave(paste(dir, 'output', 'figure', 'CO2error', 'pherrorStirringIonStrength.png', sep = '/'),
#        plot = p, width = 16, height = 12, units = 'cm', device = 'png')

p <- grid.arrange(p1, p2, nrow = 1)

ggsave(paste(dir, 'output', 'figure', 'CO2error','phErrorIonStrength.png', sep = '/'),
       plot = p, width = 30, height = 12, units = 'cm', device = 'png')



p1 <- pHerror %>% as.tibble() %>%
  group_by(waterType_f) %>%
  ggplot(aes(phError, fill = waterType)) +
  facet_wrap(facets = ~waterType_f, ncol = 5, scales = 'free_x') +
  geom_histogram(alpha = 1, bins = 50) +
  geom_vline(xintercept = 0, linetype = 2, color = 'grey50', size = 1) +
  theme_classic() +
  labs(fill = 'Test Solution') +
  xlab(expression(bold('pH Error'))) +
  ylab(expression(bold('Count'))) +
  ggtitle(expression('')) +
  scale_x_continuous(limits = c(-1, 0.2),
                     breaks = function(x){pretty(x, n = 3)},
                     labels = function(x){round(x, 2)}) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0, 20, by = 5)) +
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 18),
        legend.key.size = unit('0.5', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 18, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  ) +
  annotate('text', x = -0.5, y = 18, label = '(n =       )', size = 5)
p1

ggsave(paste(dir, 'output', 'figure', 'CO2error', 'phError.png', sep = '/'),
       plot = p1, width = 40, height = 20, units = 'cm', device = 'png')

# sd(pHerror[pHerror$waterType %in% c('(a) Standard Buffer'),]$phError)
# ph <- pHerror[pHerror$waterType %in% c('(d) Diluted Acid', '(c) Freshwater'),]
# 
# t.test(phError ~ waterType, ph)

