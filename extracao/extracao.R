################################################################################
##################### Análise do /s/ em coda ###################################
######################### V1 06/12/2025 ########################################
################################################################################

#manipualção dos dados obtidos do praat e análises

#Carregar Pacotes
library(tidyverse)

#definir diretório
setwd("C:/Users/sarah/Desktop/LL434/trabalho-final/extracao")

#Carregar Arquivos

dados1.F <- read.csv("SpectralMoments1-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados2.F <- read.csv("SpectralMoments2-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados3.F <- read.csv("SpectralMoments3-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados4.F <- read.csv("SpectralMoments4-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados5.F <- read.csv("SpectralMoments5-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados6.F <- read.csv("SpectralMoments6-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados7.F <- read.csv("SpectralMoments7-F.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados8.F <- read.csv("SpectralMoments8-F.txt", header = TRUE, fileEncoding = "UTF-16LE")

dados1.M <- read.csv("SpectralMoments1-M.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados2.M <- read.csv("SpectralMoments2-M.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados3.M <- read.csv("SpectralMoments3-M.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados4.M <- read.csv("SpectralMoments4-M.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados5.M <- read.csv("SpectralMoments5-M.txt", header = TRUE, fileEncoding = "UTF-16LE")
dados6.M <- read.csv("SpectralMoments6-M.txt", header = TRUE, fileEncoding = "UTF-16LE")


#juntas df
dados <- rbind(
  dados1.F, dados2.F, dados3.F, dados4.F,
  dados5.F, dados6.F, dados7.F, dados8.F,
  dados1.M, dados2.M, dados3.M, dados4.M,
  dados5.M, dados6.M)

str(dados)

write.csv(dados, file = "SpectralMoments-FM.csv", row.names = TRUE)

#Filtrar apenas casos de s em coda

