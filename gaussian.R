library(dplyr) 
library(ggplot2)

setwd("H:/geob/402_Air_Pollution_Meteorology/Assg_3/")

## reading in data with columns of z, effective stack height, and stability class.
gau_df <- read.csv("gaussian.csv")

## inserting new columns for sigma values
gau_df$sigmay <- NA
gau_df$sigmaz <- NA

## for this study, crosswind y component is 0
y <- 0

## define sigma value based on Gifford (12) for z between 100 and 10000m
## class A:
sigmay_A <- 0.22*10000*(1+0.0001*10000)^(-0.5)
sigmaz_A <- 0.20*10000

gau_df$sigmay[gau_df$class == "A"] <- sigmay_A
gau_df$sigmaz[gau_df$class == "A"] <- sigmaz_A

## class B
sigmay_B <- 0.16*10000*(1+0.0001*10000)^(-0.5)
sigmaz_B <- 0.12*10000

gau_df$sigmay[gau_df$class == "B"] <- sigmay_B
gau_df$sigmaz[gau_df$class == "B"] <- sigmaz_B

## class C
sigmay_C <- 0.11*10000*(1+0.0001*10000)^(-0.5)
sigmaz_C <- 0.08*10000*(1+0.0002*10000)^(-0.5)

gau_df$sigmay[gau_df$class == "C"] <- sigmay_C
gau_df$sigmaz[gau_df$class == "C"] <- sigmaz_C

## class D
sigmay_D <- 0.08*10000*(1+0.0001*10000)^(-0.5)
sigmaz_D <- 0.06*10000*(1+0.0015*10000)^(-0.5)

gau_df$sigmay[gau_df$class == "D"] <- sigmay_D
gau_df$sigmaz[gau_df$class == "D"] <- sigmaz_D

## class_E
sigmay_E <- 0.06*10000*(1+0.0001*10000)^(-0.5)
sigmaz_E <- 0.03*10000*(1+0.0003*10000)^(-1)

gau_df$sigmay[gau_df$class == "E"] <- sigmay_E
gau_df$sigmaz[gau_df$class == "E"] <- sigmaz_E

## class_F
sigmay_F <- 0.04*10000*(1+0.0001*10000)^(-0.5)
sigmaz_F <- 0.016*10000*(1+0.0003*10000)^(-1)

gau_df$sigmay[gau_df$class == "F"] <- sigmay_F
gau_df$sigmaz[gau_df$class == "F"] <- sigmaz_F

## computing Gaussian concentration for each class and inputting results in dataframe
gau_df <- gau_df %>% 
  group_by(class) %>% 
  mutate(conc = 1000000*5/(2*pi*1*sigmay*sigmaz)*(exp((-(z-Effective.Stack.Height)^2)/(2*sigmaz^2)) + exp(-(z+Effective.Stack.Height)^2/(2*sigmaz^2)))*exp(-(y^2)/(2*sigmay)))
                                     
## plotting modelled concentrations for different classes at different heights
ggplot(gau_df, aes(conc, z, group = class, colour = class)) +
  xlab(expression(Concentration ~(mu~g/m^{3}))) +
  ylab("Height (m)") +
  geom_line() +
  geom_point(size = 0.9) +
  theme_bw()
