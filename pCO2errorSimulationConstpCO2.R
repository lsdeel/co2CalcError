library(tidyverse)
library(readxl)
library(gridExtra)

#set working directory
dir = 'd:/Research/postdoc_proj'

dic <- seq(10, 10000, by = 10)
pco2Error <- data_frame(dic)

pco2Error <- pco2Error %>% mutate(
  Tw = 15,
  HenryCons = 10^(-1*(-0.00007*Tw^2 + 0.016*Tw + 1.11)),
  K1 = 10^(-1*(0.00011*Tw^2-0.012*Tw+6.58)),
  K2 = 10^(-1*(0.00009*Tw^2-0.0137*Tw+10.62)),
  pco2500 = 500,
  pco21000 = 1000,
  pco22000 = 2000,
  pco24000 = 4000,
  pco28000 = 8000,
  pco212000 = 12000,
  CO2500 = pco2500 * HenryCons,
  CO21000 = pco21000 * HenryCons,
  CO22000 = pco22000 * HenryCons,
  CO24000 = pco24000 * HenryCons,
  CO28000 = pco28000 * HenryCons,
  CO212000 = pco212000 * HenryCons,
  biCar500 = K1 / (2*K2) * (sqrt(1+4*K2/K1*(dic - CO2500)) - 1),
  biCar1000 = K1 / (2*K2) * (sqrt(1+4*K2/K1*(dic - CO21000)) - 1),
  biCar2000 = K1 / (2*K2) * (sqrt(1+4*K2/K1*(dic - CO22000)) - 1),
  biCar4000 = K1 / (2*K2) * (sqrt(1+4*K2/K1*(dic - CO24000)) - 1),
  biCar8000 = K1 / (2*K2) * (sqrt(1+4*K2/K1*(dic - CO28000)) - 1),
  biCar12000 = K1 / (2*K2) * (sqrt(1+4*K2/K1*(dic - CO212000)) - 1),
  hydroIon500 = CO2500 * K1 / biCar500,
  hydroIon1000 = CO21000 * K1 / biCar1000,
  hydroIon2000 = CO22000 * K1 / biCar2000,
  hydroIon4000 = CO24000 * K1 / biCar4000,
  hydroIon8000 = CO28000 * K1 / biCar8000,
  hydroIon12000 = CO212000 * K1 / biCar12000,
  pH500 = -log10(hydroIon500),
  pH1000 = -log10(hydroIon1000),
  pH2000 = -log10(hydroIon2000),
  pH4000 = -log10(hydroIon4000),
  pH8000 = -log10(hydroIon8000),
  pH12000 = -log10(hydroIon12000),
  Car500 = biCar500 * K2 / hydroIon500,
  Car1000 = biCar1000 * K2 / hydroIon1000,
  Car2000 = biCar2000 * K2 / hydroIon2000,
  Car4000 = biCar4000 * K2 / hydroIon4000,
  Car8000 = biCar8000 * K2 / hydroIon8000,
  Car12000 = biCar12000 * K2 / hydroIon12000,
  carAlk500 = biCar500 + 2*Car500,
  carAlk1000 = biCar1000 + 2*Car1000,
  carAlk2000 = biCar2000 + 2*Car2000,
  carAlk4000 = biCar4000 + 2*Car4000,
  carAlk8000 = biCar8000 + 2*Car8000,
  carAlk12000 = biCar12000 + 2*Car12000,
  # carAlk500 = biCar500 + 2*Car500 - hydroIon500,
  # carAlk1000 = biCar1000 + 2*Car1000 - hydroIon1000,
  # carAlk2000 = biCar2000 + 2*Car2000 - hydroIon2000,
  # carAlk4000 = biCar4000 + 2*Car4000 - hydroIon4000,
  # carAlk8000 = biCar8000 + 2*Car8000 - hydroIon8000,
  # carAlk12000 = biCar12000 + 2*Car12000 - hydroIon12000,
  # doc = 1846 * exp(-0.0013 * dic)+252, # umoll-1
  doc = 331 + 1562 * exp(-0.0012 * dic),
  orgAlk = doc * 0.1, # ueql-1
  totAlk500 = carAlk500 + orgAlk, # ueql-1
  totAlk1000 = carAlk1000 + orgAlk, # ueql-1
  totAlk2000 = carAlk2000 + orgAlk, # ueql-1
  totAlk4000 = carAlk4000 + orgAlk, # ueql-1
  totAlk8000 = carAlk8000 + orgAlk, # ueql-1
  totAlk12000 = carAlk12000 + orgAlk, # ueql-1
  orgAlkFrac500 = (orgAlk / totAlk500) * 100, # %
  orgAlkFrac1000 = (orgAlk / totAlk1000) * 100, # %
  orgAlkFrac2000 = (orgAlk / totAlk2000) * 100, # %
  orgAlkFrac4000 = (orgAlk / totAlk4000) * 100, # %
  orgAlkFrac8000 = (orgAlk / totAlk8000) * 100, # %
  orgAlkFrac12000 = (orgAlk / totAlk12000) * 100, # %
  pco2ErrorOrgAlk500 = orgAlk / carAlk500 * 100, # %
  pco2ErrorOrgAlk1000 = orgAlk / carAlk1000 * 100, # %
  pco2ErrorOrgAlk2000 = orgAlk / carAlk2000 * 100, # %
  pco2ErrorOrgAlk4000 = orgAlk / carAlk4000 * 100, # %
  pco2ErrorOrgAlk8000 = orgAlk / carAlk8000 * 100, # %
  pco2ErrorOrgAlk12000 = orgAlk / carAlk12000 * 100, # %
  specCond = dic * 0.2, # uS/cm
  I = 0.000013 * specCond, #moll-1, ionic Strength
  log10I = log10(I),
  pHerror = log10I * 0.05 + 0.03,
  pco2ErrorpH = (10^(-1 * pHerror) - 1) * 100, # %
  totpco2Error500 = ((pco2ErrorOrgAlk500 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error1000 = ((pco2ErrorOrgAlk1000 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error2000 = ((pco2ErrorOrgAlk2000 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error4000 = ((pco2ErrorOrgAlk4000 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error8000 = ((pco2ErrorOrgAlk8000 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error12000 = ((pco2ErrorOrgAlk12000 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  pHerrorStir = log10I * 0.08 + 0.06,
  pco2ErrorpHStir = (10^(-1 * pHerrorStir) - 1) * 100, # %
  totpco2Error500Stir = ((pco2ErrorOrgAlk500 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error1000Stir = ((pco2ErrorOrgAlk1000 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error2000Stir = ((pco2ErrorOrgAlk2000 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error4000Stir = ((pco2ErrorOrgAlk4000 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error8000Stir = ((pco2ErrorOrgAlk8000 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error12000Stir = ((pco2ErrorOrgAlk12000 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100
)

table(pco2Error$dic <= pco2Error$CO2500) # 2
table(pco2Error$dic <= pco2Error$CO21000) # 4
table(pco2Error$dic <= pco2Error$CO22000) # 9
table(pco2Error$dic <= pco2Error$CO24000) # 18
table(pco2Error$dic <= pco2Error$CO28000) # 37
table(pco2Error$dic <= pco2Error$CO212000) # 55
table(is.na(pco2Error)) # 125

pco2Error[pco2Error$dic <= pco2Error$CO2500, c('biCar500', 'hydroIon500', 'pH500',
                                               'Car500', 'carAlk500', 'totAlk500',
                                               'orgAlkFrac500', 'pco2ErrorOrgAlk500', 
                                               'totpco2Error500', 'totpco2Error500Stir')] <- NA

pco2Error[pco2Error$dic <= pco2Error$CO21000, c('biCar1000', 'hydroIon1000', 'pH1000',
                                               'Car1000', 'carAlk1000', 'totAlk1000',
                                               'orgAlkFrac1000', 'pco2ErrorOrgAlk1000', 
                                               'totpco2Error1000', 'totpco2Error1000Stir')] <- NA

pco2Error[pco2Error$dic <= pco2Error$CO22000, c('biCar2000', 'hydroIon2000', 'pH2000',
                                                'Car2000', 'carAlk2000', 'totAlk2000',
                                                'orgAlkFrac2000', 'pco2ErrorOrgAlk2000', 
                                                'totpco2Error2000', 'totpco2Error2000Stir')] <- NA

pco2Error[pco2Error$dic <= pco2Error$CO24000, c('biCar4000', 'hydroIon4000', 'pH4000',
                                                'Car4000', 'carAlk4000', 'totAlk4000',
                                                'orgAlkFrac4000', 'pco2ErrorOrgAlk4000', 
                                                'totpco2Error4000', 'totpco2Error4000Stir')] <- NA

pco2Error[pco2Error$dic <= pco2Error$CO28000, c('biCar8000', 'hydroIon8000', 'pH8000',
                                                'Car8000', 'carAlk8000', 'totAlk8000',
                                                'orgAlkFrac8000', 'pco2ErrorOrgAlk8000', 
                                                'totpco2Error8000', 'totpco2Error8000Stir')] <- NA

pco2Error[pco2Error$dic <= pco2Error$CO212000, c('biCar12000', 'hydroIon12000', 'pH12000',
                                                'Car12000', 'carAlk12000', 'totAlk12000',
                                                'orgAlkFrac12000', 'pco2ErrorOrgAlk12000', 
                                                'totpco2Error12000', 'totpco2Error12000Stir')] <- NA

# write_csv(pco2Error, paste0(dir, '/datasets/CO2errorsData/pco2ErrorSimulationConspco2.csv'))

# Statistics ---------------------------------------------------------

# CO2 error caused by pH biases
range(pco2ErrorCO2$pco2ErrorpH) # 8-53%
range(pco2ErrorCO2$totAlk500, na.rm = TRUE) # 210-6560 ueql-1
range(pco2ErrorCO2$totAlk1000, na.rm = TRUE) # 202-6386
range(pco2ErrorCO2$totAlk2000, na.rm = TRUE) # 195-6283
range(pco2ErrorCO2$totAlk4000, na.rm = TRUE) # 174-6200
range(pco2ErrorCO2$totAlk8000, na.rm = TRUE) # 147-6096
range(pco2ErrorCO2$totAlk12000, na.rm = TRUE) # 118-6004

#co2 error caused by organic alkalinity
range(pco2ErrorCO2$pco2ErrorOrgAlk500, na.rm = TRUE) #0.39-2965%
range(pco2ErrorCO2$pco2ErrorOrgAlk1000, na.rm = TRUE) #0.4-5384%
range(pco2ErrorCO2$pco2ErrorOrgAlk2000, na.rm = TRUE) #0.4-2545%
range(pco2ErrorCO2$pco2ErrorOrgAlk4000, na.rm = TRUE) #0.41-3585%
range(pco2ErrorCO2$pco2ErrorOrgAlk8000, na.rm = TRUE) #0.41-1459%
range(pco2ErrorCO2$pco2ErrorOrgAlk12000, na.rm = TRUE) #0.42-2734%

# comparing the errors
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk500 < 
               pco2ErrorCO2$pco2ErrorpH,]$totAlk500[1:10] # 596 ueql
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk1000 < 
               pco2ErrorCO2$pco2ErrorpH,]$totAlk1000[1:10] # 581 ueql
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk2000 < 
               pco2ErrorCO2$pco2ErrorpH,]$totAlk2000[1:10] # 570 ueql
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk4000 < 
               pco2ErrorCO2$pco2ErrorpH,]$totAlk4000[1:20] # 542 ueql
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk8000 < 
               pco2ErrorCO2$pco2ErrorpH,]$totAlk8000[30:40] # 495 ueql
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk12000 < 
               pco2ErrorCO2$pco2ErrorpH,]$totAlk12000[40:60] # 450 ueql

# see how OrgAlk-error goes
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk500 < 10,]$totAlk500[1:10] # 908
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk1000 < 10,]$totAlk1000[1:10] # 893
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk2000 < 10,]$totAlk2000[1:20] # 876
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk4000 < 10,]$totAlk4000[1:20] # 835
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk8000 < 10,]$totAlk8000[20:40] # 762
pco2ErrorCO2[pco2ErrorCO2$pco2ErrorOrgAlk12000 < 10,]$totAlk12000[40:60] # 697


pco2ErrorCO2[pco2ErrorCO2$totAlk500 > 2000,]$pco2ErrorOrgAlk500[1:10] #1.7
pco2ErrorCO2[pco2ErrorCO2$totAlk1000 > 2000,]$pco2ErrorOrgAlk1000[1:10] #1.7
pco2ErrorCO2[pco2ErrorCO2$totAlk2000 > 2000,]$pco2ErrorOrgAlk2000[1:10] #1.6
pco2ErrorCO2[pco2ErrorCO2$totAlk4000 > 2000,]$pco2ErrorOrgAlk4000[10:20] #1.6
pco2ErrorCO2[pco2ErrorCO2$totAlk8000 > 2000,]$pco2ErrorOrgAlk8000[20:40] #1.5
pco2ErrorCO2[pco2ErrorCO2$totAlk12000 > 2000,]$pco2ErrorOrgAlk12000[40:60] #1.5

# p1 500----------------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk500, pco2ErrorpH, pco2ErrorpHStir, totpco2Error500Stir)

p1 <- pco2ErrorG %>%
  ggplot(aes(totAlk500, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(a) '*italic(p)*CO[2]*' = 500 '*italic(mu)*'atm'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.6, 0.7),
        legend.text=  element_text(size = 12),
        legend.key.size = unit('1.5', 'cm'),
        legend.key.height = unit('0.6', 'cm'),
        legend.title = element_text(size = 12, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p1

p1c <- pco2ErrorG %>%
  ggplot(aes(totAlk500, pH500)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(g) '*italic(p)*CO[2]*' = 500 '*italic(mu)*'atm')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  scale_y_continuous(limits = c(5,9),
                     breaks = seq(5, 9, by = 1)) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.7, 0.8),
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p1c

# p2 1000----------------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk1000, pco2ErrorpH, pco2ErrorpHStir, totpco2Error1000Stir)

p2 <- pco2ErrorG %>%
  ggplot(aes(totAlk1000, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(b) '*italic(p)*CO[2]*' = 1000 '*italic(mu)*'atm'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p2

p2c <- pco2ErrorG %>%
  ggplot(aes(totAlk1000, pH1000)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(h) '*italic(p)*CO[2]*' = 1000 '*italic(mu)*'atm')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  scale_y_continuous(limits = c(5,9),
                     breaks = seq(5, 9, by = 1)) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.7, 0.8),
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p2c


# p3 2000----------------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk2000, pco2ErrorpH, pco2ErrorpHStir, totpco2Error2000Stir)

p3 <- pco2ErrorG %>%
  ggplot(aes(totAlk2000, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(c) '*italic(p)*CO[2]*' = 2000 '*italic(mu)*'atm')))+
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p3

p3c <- pco2ErrorG %>%
  ggplot(aes(totAlk2000, pH2000)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(c) '*italic(p)*CO[2]*' = 2000 '*italic(mu)*'atm')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  scale_y_continuous(limits = c(5,9),
                     breaks = seq(5, 9, by = 1)) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.7, 0.8),
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p3c


# p4 4000 ---------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk4000, pco2ErrorpH, pco2ErrorpHStir, totpco2Error4000Stir)

p4 <- pco2ErrorG %>%
  ggplot(aes(totAlk4000, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(d) '*italic(p)*CO[2]*' = 4000 '*italic(mu)*'atm')))+
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p4

p4c <- pco2ErrorG %>%
  ggplot(aes(totAlk4000, pH4000)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(j) '*italic(p)*CO[2]*' = 4000 '*italic(mu)*'atm')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  scale_y_continuous(limits = c(5,9),
                     breaks = seq(5, 9, by = 1)) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.7, 0.8),
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p4c


# p5 8000 ---------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk8000, pco2ErrorpH, pco2ErrorpHStir, totpco2Error8000Stir)

p5 <- pco2ErrorG %>%
  ggplot(aes(totAlk8000, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(e) '*italic(p)*CO[2]*' = 8000 '*italic(mu)*'atm')))+
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p5

p5c <- pco2ErrorG %>%
  ggplot(aes(totAlk8000, pH8000)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(k) '*italic(p)*CO[2]*' = 8000 '*italic(mu)*'atm')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  scale_y_continuous(limits = c(5,9),
                     breaks = seq(5, 9, by = 1)) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.7, 0.8),
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p5c


# p6 12000 ---------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk12000, pco2ErrorpH, pco2ErrorpHStir, totpco2Error12000Stir)

p6 <- pco2ErrorG %>%
  ggplot(aes(totAlk12000, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(f) '*italic(p)*CO[2]*' = 12000 '*italic(mu)*'atm')))+
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  # scale_x_reverse() +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring', 'Total')) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = 'none',
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p6

p6c <- pco2ErrorG %>%
  ggplot(aes(totAlk12000, pH12000)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(l) '*italic(p)*CO[2]*' = 12000 '*italic(mu)*'atm')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold('pH'))) +
  scale_y_continuous(limits = c(4,9),
                     breaks = seq(5, 9, by = 1)) +
  theme_classic()+
  theme(axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(.25, 'cm'),
        plot.title = element_text(hjust = 0, size = 16),
        legend.position = c(0.7, 0.8),
        legend.text=  element_text(size = 16),
        legend.key.size = unit('2', 'cm'),
        legend.key.height = unit('1', 'cm'),
        legend.title = element_text(size = 16, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p6c

# combine graphs ----------------------------------------------------------

margin_p <- 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'))

p <- grid.arrange(p1 + margin_p, p2 + margin_p, p1c + margin_p, p2c + margin_p,
                  p3 + margin_p, p4 + margin_p, p3c + margin_p, p4c + margin_p,
                  p5 + margin_p, p6 + margin_p, p5c + margin_p, p6c + margin_p, ncol = 4)

ggsave(paste(dir, 'output', 'figure', 'CO2error', 'pco2ErrorConstpco2.png', sep = '/'),
       plot = p, width = 42, height = 28, units = 'cm', device = 'png')


range(pco2Error$totAlk500,na.rm = TRUE)
range(pco2Error$totAlk1000,na.rm = TRUE)
range(pco2Error$totAlk2000,na.rm = TRUE)
range(pco2Error$totAlk4000,na.rm = TRUE)
range(pco2Error$totAlk8000,na.rm = TRUE)
range(pco2Error$totAlk12000,na.rm = TRUE)

