#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%% Análise do /s/ em coda %%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% V1 06/12/2025 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#manipualção dos dados obtidos do praat e análises: antes de carregar os dados 
#fiz uma limpeza na planilha, desconsiderando casos de s em ataque, s em coda 
#além dos 20 min delimitados, s em coda em contextos de assimilação e s em morfemas de plural

#install.packages("hms"); install.packages("fuzzyjoin")

#Carregar Pacotes
library(tidyverse)
library(hms)
library(fuzzyjoin)
library(RColorBrewer)

#definir diretório
#setwd("C:/Users/sarah/Desktop/LL434/trabalho-final")
setwd("C:/Users/224425/Downloads/s-acustic")

#Carregar dados################################################################
## CONTINUO ###################################################################
dados.acusticos <- read_csv("SpectralMoments-FM.csv",
                            col_types = cols(.default = col_factor(),
                                             word  = col_character(),
                                             starttime = col_double(),
                                             duration = col_integer(),
                                             centergravity = col_double())
)

dados.acusticos$centergravity <- round(dados.acusticos$centergravity, 0)


### checagem ####
str(dados.acusticos)
dados.acusticos$starttime
unique(dados.acusticos$audiofile)

#transformar formato de hora para HH:mm:ss
dados.acusticos <- dados.acusticos %>%
  mutate(TEMPO = as_hms(starttime))
dados.acusticos$TEMPO


## DISCRETA ###################################################################
dados.oitiva <- read_csv("dadosS_Amostra2-planilhaV3.csv", locale = locale(encoding = "latin1"),
   col_types = cols(.default = col_factor(),
    VD = col_factor(levels = c("A", "P", "0", "H", "M")),
   CONT_PREC = col_character(),
   OCORRENCIA = col_character(),
   CONT_SEGUINTE = col_character(),
   IDADE = col_integer(),
   INDICE_SOCIO = col_double(),
   CLASSE_SOCIAL = col_factor(levels = c("B2", "C1", "C2", "D", "E")),
   IDADE_MIGRACAO = col_integer(),
   TEMPO_RESIDENCIA_fat = col_factor(levels = c("9-", "10+")),
   TEMPO_RESIDENCIA = col_integer(),
   LOCALIZACAO = col_time(format = "%H:%M:%S")
)
)
###checagem ####
#View(dados.oitiva)
str(dados.oitiva)
levels(dados.oitiva$VD)
dados.oitiva$LOCALIZACAO


#filtrar dados metalinguísticos
dados.oitiva <- dados.oitiva %>% 
  filter(VD != "M") %>% 
  droplevels()

#checagem Comparação entre falantes
setdiff(dados.acusticos$audiofile, dados.oitiva$ARQUIVO)

#COMBINAR DADOS ###############################################################
#Agrupa por palavras com tempo mais próximo
dados.combinados <- fuzzy_left_join(
dados.acusticos,
dados.oitiva,
by = c(
  "audiofile"      = "ARQUIVO",
  "word"           = "ITEM_LEXICAL"
),
match_fun = list(`==`, `==`)
) %>%
  # Calcula diferença absoluta de tempo
  mutate(diferenca_tempo = abs(as.numeric(TEMPO - LOCALIZACAO))) %>%
  group_by(audiofile, word, TEMPO) %>%
  slice_min(diferenca_tempo, with_ties = FALSE) %>%
  ungroup()
view(dados.combinados)


#filtrar casos que não deram match
dados.combinados <- dados.combinados %>%
  filter(!is.na(word) & !is.na(ITEM_LEXICAL))
str(dados.combinados)

#checagem
str(dados.combinados)
summary(dados.combinados)

write_csv2(dados.combinados, "dados_combinados.csv")

# DISTRIBUIÇÃO GERAL #########################################################
prop.VD <- dados.combinados %>% 
  count(VD) %>%
  mutate(prop = prop.table(n)*100,
         label = paste0(round(prop, 1), "%\n(", n, ")")) %>% 
  print()


png("VD.png", width = 10, height = 6, units = "in", res = 300)
ggplot(prop.VD, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal", "Zero Fonético", "Aspirada"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_light()+
  theme(legend.position = "none")
dev.off()

## VD/ CG e duração ####
#medidas centrais
#grafico n absolutos


png("VD_CG_duration.png", width = 10, height = 6, units = "in", res = 300)
ggplot(dados.combinados, aes(x = centergravity, y = duration, shape = VD, fill = VD, color = VD)) +
  geom_point(color = "black", size = 2.5, stroke = 0.3, alpha = 0.8) + 
  scale_shape_manual(values = c("A" = 21, "P" = 22, "0" = 24, "H" = 23))+
  scale_x_continuous(name = "Centro de Gravidade (Hz)") +
  scale_y_continuous(name = "Duração (ms)") +
  facet_wrap(. ~ VD) +
  theme_light()
dev.off()

png("VD_CG_duration2.png", width = 10, height = 6, units = "in", res = 300)
ggplot(dados.combinados, aes(x = centergravity, y = duration, fill = VD)) +
  geom_point(shape = 21, size = 2.5, stroke = 0.3, color = "black", alpha = 1) +
  #geom_point(data = CG.DURACAO_medidas.centrais, aes(x = media_cg, y = media_dur), fill = "#07c099", shape = 21, size = 2, color = "black", stroke = 1) +
  scale_x_continuous(name = "Centro de Gravidade (Hz)", limits = c(NA, 10000)) +
  scale_y_continuous(name = "Duração (ms)") +
  scale_fill_brewer(palette = "Reds") +
  #scale_fill_manual(values = c("A" = "#fef0d9", "P" = "#fdcc8a", "0" = "#fc8d59", "H" = "#d7301f")) +
  facet_wrap(. ~ VD, labeller = labeller(VD = c("A" = "Alveolar (N=2922)",
                                                "P" = "Palatal (N = 754)",
                                                "0" = "Zero fonético N = 701)",
                                                "H" = "Aspirada N = 119)"))) +
  theme_light() +
  theme(legend.position = "none")
dev.off()

CG.DURACAO_medidas.centrais <- dados.combinados %>% 
  group_by(VD) %>% 
  summarize(ocorrencias = n(),
    media_cg = mean(centergravity),
    mediana_cg = median(centergravity),
    media_dur = mean(duration),
    mediana_dur = median(duration)) %>%
  print()

rotulos_com_n <- c(
  "A" = paste0("Alveolar (N=", CG.DURACAO_medidas.centrais$ocorrencias[CG.DURACAO_medidas.centrais$VD == "A"], ")"),
  "P" = paste0("Palatal (N=", CG.DURACAO_medidas.centrais$ocorrencias[CG.DURACAO_medidas.centrais$VD == "P"], ")"),
  "0" = paste0("Zero fonético (N=", CG.DURACAO_medidas.centrais$ocorrencias[CG.DURACAO_medidas.centrais$VD == "0"], ")"),
  "H" = paste0("Aspirada (N=", CG.DURACAO_medidas.centrais$ocorrencias[CG.DURACAO_medidas.centrais$VD == "H"], ")")
)

#histograma medidas
png("VD_histograma.png", width = 10, height = 6, units = "in", res = 300)
ggplot(dados.combinados, aes(x = centergravity, fill = VD)) +
  geom_histogram(binwidth = 300, color = "black", alpha = 0.7) +
  geom_vline(data = CG.DURACAO_medidas.centrais,
             aes(xintercept = media_cg, color = "Média"),
             linetype = "solid", size = 0.8) +
  geom_vline(data = CG.DURACAO_medidas.centrais,
             aes(xintercept = mediana_cg, color = "Mediana"),
             linetype = "solid", size = 0.8) +
  labs(x = "Centro de Gravidade (Hz)",
       y = "Frequência",
       color = "Medidas") +  # Título da legenda para as linhas
  facet_wrap(~ VD, labeller = labeller(VD = rotulos_com_n)) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_manual(values = c("Mediana" = "#07c099", "Média" = "#c0072e")) +
  scale_x_continuous(name = "Centro de Gravidade (Hz)", limits = c(NA, 10000)) +
  theme_light() +
  guides(fill = "none")
dev.off()


## Teste de normalidade ####
shapiro.test(dados.combinados$centergravity)
shapiro.test(dados.combinados$duration)


# FILTRAGEM POR PROCESSO #####################################################
#filtrar todos os casos de apagamento porque não teve revisão
dados.combinados2 <- dados.combinados %>% 
  filter(VD %in% c("A", "P", "H")) %>% 
  droplevels()%>% 
  print()
levels(dados.combinados2$VD)


## Palatalização ####
dados.combinados_AP <- dados.combinados2 %>% 
  filter(VD %in% c("A", "P"),
         CFS_ponto %in% "coronal") %>% 
  droplevels() %>% 
  print()
shapiro.test(dados.combinados_AP$centergravity)


## Aspiração ####
dados.combinados_HAP <- dados.combinados2 %>%
  filter(CONT_FON_SEG %in% c("b", "l", "n", "d", "m", "v", "g")) %>% 
  mutate(VD = as.character(VD),
         VD = ifelse(VD %in% c("A", "P"), "AP", VD),
         VD = factor(VD, levels = c("H", "AP"))) %>%
  print()
levels(dados.combinados_HAP$VD)
shapiro.test(dados.combinados_HAP$centergravity)


#ANÁLISES  ####################################################################
## Palatalização ####
#distribuição discreta
prop.AP <- dados.combinados_AP %>% 
  count(VD) %>% 
  mutate(prop = prop.table(n),
         ocorrencias = n,
         label = paste0(round(prop * 100, 1), "%\n(", ocorrencias, ")")) %>% 
  print()

png("VD_AP.png", width = 10, height = 6, units = "in", res = 300)
ggplot(prop.AP, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_light()+
  theme(legend.position = "none")
dev.off()

#distribuição da VD por participante
prop.AP.participantes <- dados.combinados_AP %>% 
  count(ARQUIVO, VD) %>%
  group_by(ARQUIVO) %>% 
  mutate(prop = prop.table(n),
         ocorrencias = n,
         label = paste0(round(prop * 100, 1), "%\n(", ocorrencias, ")")) %>% 
  print()

#png("VD_AP.png", width = 10, height = 6, units = "in", res = 300)
ggplot(prop.AP.participantes, aes(x = VD, y = prop, fill = VD, label = label)) + 
  geom_bar(stat = "identity", color = "white") + 
  #labs(x = "Variável Dependente", y = "Proporção de Ocorrência", fill = "VD") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  facet_wrap(~ ARQUIVO)+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_light()+
  theme(legend.position = "none")


##### CENTRO DE GRAVIDADE ####
#Analise do centro de gravidade com hipóteses baseadas em Barbosa 2023, compara-se duas analises discreta e contínua
ggplot(dados.combinados_AP, aes(x = centergravity, y = duration, fill = VD)) +
  geom_point(color = "black", size = 2.2, stroke = 0.3, shape = 21, position = position_jitter(width = 10, height = 5), alpha = 1) + 
  facet_wrap(. ~ VD,  labeller = labeller(VD = c("A" = "Alveolar (N = 1129)",
                                                 "P" = "Palatal (N = 738)"))) +
  scale_x_continuous(name = "Centro de Gravidade (Hz)") +
  scale_y_continuous(name = "Duração (ms)") +
  scale_fill_brewer(palette = "Reds")+
  #scale_fill_manual(values = c("feminino" = "#fa9770", "masculino" = "#70fadc"))+
  theme_light() +
  theme(legend.position = "none")


###### Medidas centrais ####
AP.medidas_centrais <- dados.combinados_AP %>% 
  group_by(VD) %>% 
  summarize(media_cg = mean(centergravity),
            mediana_cg = median(centergravity),
            media_dur = mean(duration),
            mediana_dur = median(duration),
            sd_cg =  sd(centergravity)) %>%
  mutate(label_cg = paste0(round(media_cg), "Hz"),
         label_dur = paste0(round(media_dur), "Ms")) %>% 
  print()


ggplot(AP.medidas_centrais, aes(x = VD, y = media_cg, fill = VD, label = label_cg)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Média do centro de Gravidade (Hz)", fill = "VD") + 
  scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_light()+
  theme(legend.position = "none")

wilcox.test(centergravity ~ VD, data = dados.combinados_AP, conf.int = T)
  
  
ggplot(AP.medidas_centrais, aes(x = VD, y = media_dur, fill = VD, label = label_dur)) + 
  geom_bar(stat = "identity", color = "white") + 
  labs(x = "Variável Dependente", y = "Média da duração (ms)", fill = "VD") + 
  #scale_x_discrete(labels = c("Alveolar", "Palatal"))+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds")+
  theme_light()+
  theme(legend.position = "none")

wilcox.test(duration ~ VD, data = dados.combinados_AP, conf.int = T) #não tem correlação


#### GENERO ####
#discreta
#HIPÓTESE: n esperava que tivesse por causa de Barbosa 2023
tab.GENERO <- dados.combinados_AP %>% 
  count(GENERO, VD) %>% 
  group_by(GENERO) %>% 
  mutate(prop = prop.table(n),
         ocorrencias = n,
         label = paste0(round(prop * 100, 1), "%\n(", ocorrencias, ")")) %>% 
  print()

ggplot(tab.GENERO, aes(x = GENERO, y = prop * 100, fill = VD, label = label)) +
  geom_bar(stat = "identity", color = "black") + 
  labs(x = "Gênero", y = "Proporção de Ocorrência", fill = "VD") +
  #scale_x_discrete(labels = c("Masculino", "Feminino")) +
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Reds", name = "Variantes", labels = c("Alveolar", "Palatal"))+
  theme_light()

(tab.GENERO.X2 <- with(dados.combinados_AP, table(GENERO, VD)))
chisq.test(tab.GENERO.X2)


##CONTINUA 
#hipótese de que não há diferença significativa em relação ao centro de gravidade, mas sim à duração 

#CG
dados.combinados_AP %>%
  ggplot(aes(x = centergravity, y = duration, fill = GENERO)) +
  geom_point(color = "black", size = 2.2, stroke = 0.3, shape = 21, position = position_jitter(width = 10, height = 5), alpha = 0.8) + 
  facet_wrap(. ~ VD, labeller = as_labeller(rotulos_com_n)) +
  scale_x_continuous(name = "Centro de Gravidade (Hz)") +
  scale_y_continuous(name = "Duração (ms)") +
  scale_fill_manual(values = c("feminino" = "#fa9770", "masculino" = "#07c099"))+
  theme_light()



ggplot(dados.combinados_AP, aes(x = GENERO, y = centergravity, fill = GENERO)) +
  geom_boxplot(notch = TRUE)+
  scale_y_continuous(name = "Centro de Gravidade (Hz)", limits = c(NA, 9000)) +
  scale_fill_manual(values = c("feminino" = "#fa9770", "masculino" = "#07c099"))+
  theme_light()
  
wilcox.test(centergravity ~ GENERO, data = dados.combinados_AP, conf.int = T)
#Mediana do grupo de referência menos mediana do outro grupo


#duração
ggplot(dados.combinados_AP, aes(x = GENERO, y = duration, fill = GENERO)) +
  geom_boxplot(notch = TRUE)+
  scale_y_continuous(name = "Duração (Hz)") +
  scale_fill_manual(values = c("feminino" = "#fa9770", "masculino" = "#07c099"))+
  theme_light()

wilcox.test(duration ~ GENERO, data = dados.combinados_AP, conf.int = T)


### IDADE MIGRACAO ###########
#discreta

AP_IDADE.MIGRACAO <- dados.combinados_AP %>% 
  count(VD, IDADE_MIGRACAO) %>%
  group_by(IDADE_MIGRACAO) %>% 
  mutate(prop = prop.table(n)) %>% 
  print()


#png("VD_AP-09.tempo_sp.png")
ggplot(AP_IDADE.MIGRACAO[23:44,], aes(x = IDADE_MIGRACAO, y = prop * 100, label = round(prop * 100, 1))) +
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Tempo de Residência", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

mod.AP_IDADE.MIGRACAO <- glm(VD ~ IDADE_MIGRACAO, data = dados.combinados_AP, family = binomial)
summary(mod.AP_IDADE.MIGRACAO)


#continua
palatal_idade.migracao <- dados.combinados_AP %>%
  filter(VD == "P") %>% 
  group_by(ARQUIVO) %>% 
  print()

palatal_idade.migracao %>%
  ggplot(aes(x = IDADE_MIGRACAO, y = centergravity))+
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method="lm", se=TRUE, color="red")+
  geom_point()


cor.test(palatal_idade.migracao$centergravity, palatal_idade.migracao$IDADE_MIGRACAO, method = "spearman")  # não paramétrico


### TEMPO RESIDENCIA ###########
#discreta

AP_TEMPO.RESIDENCIA <- dados.combinados_AP %>% 
  count(VD, TEMPO_RESIDENCIA) %>%
  group_by(TEMPO_RESIDENCIA) %>% 
  mutate(prop = prop.table(n)) %>% 
  print()


#png("VD_AP-09.tempo_sp.png")
ggplot(AP_TEMPO.RESIDENCIA[26:40,], aes(x = TEMPO_RESIDENCIA, y = prop * 100, label = round(prop * 100, 1))) +
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method=lm, se=TRUE, color="red")+
  #labs(x = "Tempo de Residência", y = "Proporção de Palatalização") +
  #geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  theme_light()
#dev.off()

mod.AP_TEMPO.RESIDENCIA <- glm(VD ~ TEMPO_RESIDENCIA, data = dados.combinados_AP, family = binomial)
summary(mod.AP_TEMPO.RESIDENCIA)


#continua
AP_palatal_tempo.residencia <- dados.combinados_AP %>%
  filter(VD == "P") %>% 
  group_by(ARQUIVO) %>%
  print()

AP_palatal_tempo.residencia %>%
  ggplot(aes(x = TEMPO_RESIDENCIA, y = centergravity))+
  geom_point(stat = "identity", color = "black") + 
  stat_smooth(method="lm", se=TRUE, color="red")+
  geom_point()

cor.test(AP_palatal_tempo.residencia$centergravity, AP_palatal_tempo.residencia$TEMPO_RESIDENCIA, method = "spearman") 


#PARTICIPANTE####
unique(dados.combinados_AP$ARQUIVO)

dados.combinados_AP <- dados.combinados_AP %>%
  mutate(PARTICIPANTE = str_extract(ARQUIVO, "[^_]+$"))

unique(dados.combinados_AP$PARTICIPANTE)


#dados.PARTICIPANTE <- dados.combinados_AP %>% 
 
dados.combinados_AP %>% 
  ggplot(aes(x = VD, y=centergravity, fill = VD))+
  geom_boxplot(notch = FALSE)+
  scale_y_continuous(name = "Duração (Hz)") +
  scale_fill_brewer(palette = "Reds")+
  facet_wrap(. ~ PARTICIPANTE)+
  theme_light()
