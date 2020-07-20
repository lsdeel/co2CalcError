library(tidyverse)
library(readxl)
library(gridExtra)

#set working directory
dir = 'd:/Research/postdoc_proj'

dic <- seq(10, 10000, by = 10)
pco2Error <- data_frame(dic)

pco2Error <- pco2Error %>% mutate(
  pH = 8.3 - 2.4 * exp(-0.001 * dic),
  pH5 = 5,
  pH6 = 6,
  pH7 = 7,
  pH8 = 8,
  pH9 = 9,
  Tw = 15,
  hydroIonpH = 10^(-pH),
  hydroIonpH5 = 10^(-pH5),
  hydroIonpH6 = 10^(-pH6),
  hydroIonpH7 = 10^(-pH7),
  hydroIonpH8 = 10^(-pH8),
  hydroIonpH9 = 10^(-pH9),
  HenryCons = 10^(-1*(-0.00007*Tw^2 + 0.016*Tw + 1.11)),
  K1 = 10^(-1*(0.00011*Tw^2-0.012*Tw+6.58)),
  K2 = 10^(-1*(0.00009*Tw^2-0.0137*Tw+10.62)),
  biCarpH = dic / (hydroIonpH / K1 + 1 + K2 / hydroIonpH),
  biCarpH5 = dic / (hydroIonpH5 / K1 + 1 + K2 / hydroIonpH5),
  biCarpH6 = dic / (hydroIonpH6 / K1 + 1 + K2 / hydroIonpH6),
  biCarpH7 = dic / (hydroIonpH7 / K1 + 1 + K2 / hydroIonpH7),
  biCarpH8 = dic / (hydroIonpH8 / K1 + 1 + K2 / hydroIonpH8),
  biCarpH9 = dic / (hydroIonpH9 / K1 + 1 + K2 / hydroIonpH9),
  CarpH = biCarpH * K2 / hydroIonpH,
  CarpH5 = biCarpH5 * K2 / hydroIonpH5,
  CarpH6 = biCarpH6 * K2 / hydroIonpH6,
  CarpH7 = biCarpH7 * K2 / hydroIonpH7,
  CarpH8 = biCarpH8 * K2 / hydroIonpH8,
  CarpH9 = biCarpH9 * K2 / hydroIonpH9,
  CO2pH = biCarpH * hydroIonpH / K1,
  CO2pH5 = biCarpH5 * hydroIonpH5 / K1,
  CO2pH6 = biCarpH6 * hydroIonpH6 / K1,
  CO2pH7 = biCarpH7 * hydroIonpH7 / K1,
  CO2pH8 = biCarpH8 * hydroIonpH8 / K1,
  CO2pH9 = biCarpH9 * hydroIonpH9 / K1,
  pCO2pH = CO2pH / HenryCons,
  pCO2pH5 = CO2pH5 / HenryCons,
  pCO2pH6 = CO2pH6 / HenryCons,
  pCO2pH7 = CO2pH7 / HenryCons,
  pCO2pH8 = CO2pH8 / HenryCons,
  pCO2pH9 = CO2pH9 / HenryCons,
  carAlk = biCarpH + 2*CarpH - hydroIonpH,
  carAlk5 = biCarpH5 + 2*CarpH5,
  carAlk6 = biCarpH6 + 2*CarpH6,
  carAlk7 = biCarpH7 + 2*CarpH7,
  carAlk8 = biCarpH8 + 2*CarpH8,
  carAlk9 = biCarpH9 + 2*CarpH9,
  # carAlk = biCarpH + 2*CarpH - hydroIonpH,
  # carAlk5 = biCarpH5 + 2*CarpH5 - hydroIonpH5,
  # carAlk6 = biCarpH6 + 2*CarpH6 - hydroIonpH6,
  # carAlk7 = biCarpH7 + 2*CarpH7 - hydroIonpH7,
  # carAlk8 = biCarpH8 + 2*CarpH8 - hydroIonpH8,
  # carAlk9 = biCarpH9 + 2*CarpH9 - hydroIonpH9,
  # doc = 1846 * exp(-0.0013 * dic)+252, # umoll-1
  doc = 331 + 1562 * exp(-0.0012 * dic),
  orgAlk = doc * 0.1, # ueql-1
  totAlk = carAlk + orgAlk, # ueql-1
  totAlk5 = carAlk5 + orgAlk, # ueql-1
  totAlk6 = carAlk6 + orgAlk, # ueql-1
  totAlk7 = carAlk7 + orgAlk, # ueql-1
  totAlk8 = carAlk8 + orgAlk, # ueql-1
  totAlk9 = carAlk9 + orgAlk, # ueql-1
  orgAlkFrac = (orgAlk / totAlk) * 100, # %
  orgAlkFrac5 = (orgAlk / totAlk5) * 100, # %
  orgAlkFrac6 = (orgAlk / totAlk6) * 100, # %
  orgAlkFrac7 = (orgAlk / totAlk7) * 100, # %
  orgAlkFrac8 = (orgAlk / totAlk8) * 100, # %
  orgAlkFrac9 = (orgAlk / totAlk9) * 100, # %
  pco2ErrorOrgAlk = orgAlk / carAlk * 100, # %
  pco2ErrorOrgAlk5 = orgAlk / carAlk5 * 100, # %
  pco2ErrorOrgAlk6 = orgAlk / carAlk6 * 100, # %
  pco2ErrorOrgAlk7 = orgAlk / carAlk7 * 100, # %
  pco2ErrorOrgAlk8 = orgAlk / carAlk8 * 100, # %
  pco2ErrorOrgAlk9 = orgAlk / carAlk9 * 100, # %
  specCond = dic * 0.2, # uS/cm
  I = 0.000013 * specCond, #moll-1, ionic Strength
  log10I = log10(I),
  pHerror = log10I * 0.05 + 0.03,
  pco2ErrorpH = (10^(-1 * pHerror) - 1) * 100, # %
  totpco2Error = ((pco2ErrorOrgAlk / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error5 = ((pco2ErrorOrgAlk5 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error6 = ((pco2ErrorOrgAlk6 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error7 = ((pco2ErrorOrgAlk7 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error8 = ((pco2ErrorOrgAlk8 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  totpco2Error9 = ((pco2ErrorOrgAlk9 / 100 + 1) * (pco2ErrorpH / 100 + 1) - 1) * 100,
  pHerrorStir = log10I * 0.08 + 0.06,
  pco2ErrorpHStir = (10^(-1 * pHerrorStir) - 1) * 100, # %
  totpco2ErrorStir = ((pco2ErrorOrgAlk / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error5Stir = ((pco2ErrorOrgAlk5 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error6Stir = ((pco2ErrorOrgAlk6 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error7Stir = ((pco2ErrorOrgAlk7 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error8Stir = ((pco2ErrorOrgAlk8 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100,
  totpco2Error9Stir = ((pco2ErrorOrgAlk9 / 100 + 1) * (pco2ErrorpHStir / 100 + 1) - 1) * 100
)

# write_csv(pco2Error, paste0(dir, '/datasets/CO2errorsData/pco2ErrorSimulationConspH.csv'))

# Statistics ---------------------------------------------------------

# CO2 error caused by pH biases
range(pco2Error$pco2ErrorpH) # 8-53%
range(pco2Error$pco2ErrorpHStir) # 16-103%
range(pco2Error$totAlk) # 200-9730 ueql-1
range(pco2Error$totAlk5) # 105-388
range(pco2Error$totAlk6) # 210-2758
range(pco2Error$totAlk7) # 215-7928
range(pco2Error$totAlk8) # 217-9802
range(pco2Error$totAlk9) # 218-10353

# errors due to 
range(pco2Error$pco2ErrorOrgAlk) #0.26-104922%
range(pco2Error$pco2ErrorOrgAlk5) #6.9-57230%
range(pco2Error$pco2ErrorOrgAlk6) #0.92-7589%
range(pco2Error$pco2ErrorOrgAlk7) #0.32-2624%
range(pco2Error$pco2ErrorOrgAlk8) #0.26-2121%
range(pco2Error$pco2ErrorOrgAlk9) #0.24-2008%

# comparing the errors
pco2Error[pco2Error$pco2ErrorOrgAlk < pco2Error$pco2ErrorpHStir,]$totAlk[1] # 589 ueql, 389
pco2Error[pco2Error$pco2ErrorOrgAlk5 < pco2Error$pco2ErrorpHStir,]$totAlk5[1] # 262 ueql, 168
pco2Error[pco2Error$pco2ErrorOrgAlk6 < pco2Error$pco2ErrorpHStir,]$totAlk6[1] # 385 ueql, 312
pco2Error[pco2Error$pco2ErrorOrgAlk7 < pco2Error$pco2ErrorpHStir,]$totAlk7[1] # 570 ueql, 399
pco2Error[pco2Error$pco2ErrorOrgAlk8 < pco2Error$pco2ErrorpHStir,]$totAlk8[1] # 602 ueql, 410
pco2Error[pco2Error$pco2ErrorOrgAlk9 < pco2Error$pco2ErrorpHStir,]$totAlk9[1] # 611 ueql, 415

# see how OrgAlk-error goes
pco2Error[pco2Error$pco2ErrorOrgAlk < 10,]$totAlk[1] # 927
pco2Error[pco2Error$pco2ErrorOrgAlk5 < 10,]$totAlk5[1] # 364
pco2Error[pco2Error$pco2ErrorOrgAlk6 < 10,]$totAlk6[1] # 553
pco2Error[pco2Error$pco2ErrorOrgAlk7 < 10,]$totAlk7[1] # 878
pco2Error[pco2Error$pco2ErrorOrgAlk8 < 10,]$totAlk8[1] # 956
pco2Error[pco2Error$pco2ErrorOrgAlk9 < 10,]$totAlk9[1] # 977

pco2Error[pco2Error$pco2ErrorOrgAlk < 5,]$totAlk[1] # 1346
pco2Error[pco2Error$pco2ErrorOrgAlk5 < 5,]$totAlk5 # no
pco2Error[pco2Error$pco2ErrorOrgAlk6 < 5,]$totAlk6[1] # 809
pco2Error[pco2Error$pco2ErrorOrgAlk7 < 5,]$totAlk7[1] # 1244
pco2Error[pco2Error$pco2ErrorOrgAlk8 < 5,]$totAlk8[1] # 1365
pco2Error[pco2Error$pco2ErrorOrgAlk9 < 5,]$totAlk9[1] # 1399

pco2Error[pco2Error$totAlk > 2000,]$pco2ErrorOrgAlk[1] #2.4
pco2Error[pco2Error$totAlk5 > 2000,]$pco2ErrorOrgAlk5[1]
pco2Error[pco2Error$totAlk6 > 2000,]$pco2ErrorOrgAlk6[1] #1.7
pco2Error[pco2Error$totAlk7 > 2000,]$pco2ErrorOrgAlk7[1] #2.1
pco2Error[pco2Error$totAlk8 > 2000,]$pco2ErrorOrgAlk8[1] #2.4
pco2Error[pco2Error$totAlk9 > 2000,]$pco2ErrorOrgAlk9[1] #2.5

# find a point where pH caused error equals OrgAlk caused error
pco2Error[abs(pco2Error$pco2ErrorOrgAlk/pco2Error$pco2ErrorpHStir - 1) < 0.05,]$totAlk
pco2Error[abs(pco2Error$pco2ErrorOrgAlk/pco2Error$pco2ErrorpHStir - 1) < 0.05,]$pco2ErrorOrgAlk
pco2Error[abs(pco2Error$pco2ErrorOrgAlk/pco2Error$pco2ErrorpHStir - 1) < 0.05,]$pco2ErrorpH

pco2Error[abs(pco2Error$pco2ErrorOrgAlk6/pco2Error$pco2ErrorpH - 1) < 0.01,]$totAlk6 # 383
pco2Error[abs(pco2Error$pco2ErrorOrgAlk6/pco2Error$pco2ErrorpH - 1) < 0.01,]$pco2ErrorOrgAlk6 #20
pco2Error[abs(pco2Error$pco2ErrorOrgAlk6/pco2Error$pco2ErrorpH - 1) < 0.01,]$pco2ErrorpH #20%

pco2Error[abs(pco2Error$pco2ErrorOrgAlk7/pco2Error$pco2ErrorpH - 1) < 0.01,]$totAlk7 # 563
pco2Error[abs(pco2Error$pco2ErrorOrgAlk7/pco2Error$pco2ErrorpH - 1) < 0.01,]$pco2ErrorOrgAlk7 #25
pco2Error[abs(pco2Error$pco2ErrorOrgAlk7/pco2Error$pco2ErrorpH - 1) < 0.01,]$pco2ErrorpH #25

pco2Error[abs(pco2Error$totAlk7 / 500-1) < 0.01,]$pco2ErrorpHStir *
  pco2Error[abs(pco2Error$totAlk7 / 500-1) < 0.01,]$pCO2pH7 / 100

pco2Error[abs(pco2Error$totAlk6 / 500-1) < 0.01,]$pco2ErrorpHStir *
  pco2Error[abs(pco2Error$totAlk6 / 500-1) < 0.01,]$pCO2pH6 / 100


pco2Error[abs(pco2Error$totAlk7 / 500-1) < 0.01,]$pco2ErrorOrgAlk7 *
  pco2Error[abs(pco2Error$totAlk7 / 500-1) < 0.01,]$pCO2pH7 / 100

pco2Error[abs(pco2Error$totAlk6 / 500-1) < 0.01,]$pco2ErrorOrgAlk6 *
  pco2Error[abs(pco2Error$totAlk6 / 500-1) < 0.01,]$pCO2pH6 / 100




# p1 pH constrained----------------------------------------------------------------------
pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk, pco2ErrorpH, pco2ErrorpHStir, totpco2ErrorStir)

p1 <- pco2ErrorG %>%
  ggplot(aes(totAlk, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(a) pH ~ DIC')))+
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
        legend.key.height = unit('0.5', 'cm'),
        legend.title = element_text(size = 12, face = 'bold'),
        strip.text.x = element_text(size = 13, face = 'bold'),
        strip.placement = 'inside',
        strip.background = element_blank()
  )

p1

p1c <- pco2ErrorG %>%
  ggplot(aes(totAlk, pCO2pH)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(g) pH ~ DIC')))+
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' ('*italic(mu)*'atm'*')'))) +
  scale_y_continuous(limits = c(0,8000),
                     breaks = seq(0, 8000, by = 2000)) +
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

# p2 pH = 9----------------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk9, pco2ErrorpH, pco2ErrorpHStir, totpco2Error9Stir)

p2 <- pco2ErrorG %>%
  ggplot(aes(totAlk9, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(b) pH = 9'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
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
  ggplot(aes(totAlk9, pCO2pH9)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(h) pH = 9'))) +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' ('*italic(mu)*'atm'*')'))) +
  scale_y_continuous(limits = c(0,8000),
                     breaks = seq(0, 8000, by = 2000)) +
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


# p3 pH = 8----------------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk8, pco2ErrorpH, pco2ErrorpHStir, totpco2Error8Stir)

p3 <- pco2ErrorG %>%
  ggplot(aes(totAlk8, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(c) pH = 8'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
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
  ggplot(aes(totAlk8, pCO2pH8)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(i) pH = 8'))) +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' ('*italic(mu)*'atm'*')'))) +
  scale_y_continuous(limits = c(0,8000),
                     breaks = seq(0, 8000, by = 2000)) +
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


# p4 pH = 7 ---------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk7, pco2ErrorpH, pco2ErrorpHStir, totpco2Error7Stir)

pco2ErrorG <- pco2ErrorG[pco2ErrorG$pCO2pH7 <= 15000,]

p4 <- pco2ErrorG %>%
  ggplot(aes(totAlk7, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(d) pH = 7'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect',  'Total')) +
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
  ggplot(aes(totAlk7, pCO2pH7)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(j) pH = 7'))) +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' ('*italic(mu)*'atm'*')'))) +
  # scale_y_continuous(limits = c(0,6000),
  #                    breaks = seq(0, 6000, by = 2000)) +
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


# p5 pH = 6 ---------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk6, pco2ErrorpH, pco2ErrorpHStir, totpco2Error6Stir)

pco2ErrorG <- pco2ErrorG[pco2ErrorG$pCO2pH6 <= 15000,]

p5 <- pco2ErrorG %>%
  ggplot(aes(totAlk6, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(e) pH = 6'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 120),
                     breaks = seq(0, 120, by = 40)) +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
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
  ggplot(aes(totAlk6, pCO2pH6)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(k) pH = 6'))) +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' ('*italic(mu)*'atm'*')'))) +
  # scale_y_continuous(limits = c(0,6000),
  #                    breaks = seq(0, 6000, by = 2000)) +
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


# p6 pH = 5 ---------------------------------------------------------------

pco2ErrorG <- pco2Error %>%
  gather('pco2ErrorCause', 'pco2Error', 
         pco2ErrorOrgAlk5, pco2ErrorpH, pco2ErrorpHStir, totpco2Error5Stir)

pco2ErrorG <- pco2ErrorG[pco2ErrorG$pCO2pH6 <= 15000,]

p6 <- pco2ErrorG %>%
  ggplot(aes(totAlk5, pco2Error, color = pco2ErrorCause, linetype = pco2ErrorCause)) +
  geom_line(size = 1.5) +
  # geom_vline(xintercept = c(345, 600, 900), color = 'grey50', alpha = 0.8) +
  ggtitle(expression(bold('(f) pH = 5'))) +
  labs(color = 'Error Source', linetype = 'Error Source') +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' Error (%)'))) +
  scale_y_continuous(limits = c(0, 1000),
                     breaks = seq(0, 1000, by = 200)) +
  # scale_x_reverse() +
  scale_color_manual(values = c('deepskyblue', 'chartreuse', 'chartreuse3', 'coral1'),
                     labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
  scale_linetype_manual(values = c(6, 5, 5, 1),
                        labels = c('OrgAlk', 'pH Error', 'pH Error + Stirring Effect', 'Total')) +
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
  ggplot(aes(totAlk6, pCO2pH6)) +
  geom_line(size = 1) +
  ggtitle(expression(bold('(l) pH = 5'))) +
  xlab(expression(bold('Total Alkalinity ('*italic(mu)*'eq '*L^-1*')'))) +
  ylab(expression(bold(italic('p')*CO[2]*' ('*italic(mu)*'atm'*')'))) +
  # scale_y_continuous(limits = c(0,6000),
  #                    breaks = seq(0, 6000, by = 2000)) +
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

ggsave(paste(dir, 'output', 'figure', 'CO2error', 'pco2ErrorConstpH.png', sep = '/'),
       plot = p, width = 42, height = 28, units = 'cm', device = 'png')



# # Transforming ------------------------------------------------------------
# 
# names(pco2Error)
# pco2ErrorGpco2 <- 
#   pco2Error %>% gather('pco2type', 'pco2', 
#                        pCO2pH, pCO2pH9, pCO2pH8, pCO2pH7, pCO2pH6, pCO2pH5)
# 
# # rm pco2 that > 15000 uatm
# pco2ErrorGpco2 <- pco2ErrorGpco2[pco2ErrorGpco2$pco2 <= 15000,]
# # spread again
# pco2ErrorClean <- pco2ErrorGpco2 %>% spread(pco2type, pco2)
# 
# write_csv(pco2ErrorClean, paste(dir, 'paper', 'pco2ErrorClean.csv', sep = '/'))
# 

