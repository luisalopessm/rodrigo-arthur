source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #
library(tidyverse)
data("ToothGrowth")



dados_dose <- ToothGrowth %>%
  group_by(dose) %>%
  summarise(media_len = mean(len))

dados_supp <- ToothGrowth %>%
  group_by(supp) %>%
  summarise(media_len = mean(len))




head(ToothGrowth)
library(ggplot2)



box <- ggplot(ToothGrowth, aes(x = factor(dose), y = len, fill = supp)) +
  geom_boxplot(
    color = "yellow",
    outlier.color = "blue",
    outlier.size = 5
  ) +
  scale_fill_manual(values = c("green", "pink")) +
  geom_jitter(color = "purple", size = 4, width = 0.5) +
  labs(
    x = "tipo de suplemento",
    y = "dose",
    title = "box plot"
  ) +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "lightgreen"),
    axis.text = element_text(color = "red", size = 14),
    axis.title = element_text(color = "orange", size = 16),
    legend.background = element_rect(fill = "yellow")
  )



analise <- ggplot(dados_dose, aes(x = media_len, y = dose)) +
  geom_bar(stat = "identity", fill = "pink", alpha = 0.2) +
  geom_line(color = "yellow", size = 2) +
  labs(
    x = "Dose (mg)",
    y = "Crescimento médio do dente (mm)",
    title = "Gráfico honesto"
  ) +
  theme_bw()




analise3 <- ggplot(ToothGrowth, aes(x = supp, fill = supp)) +
  geom_bar(color = "yellow", linewidth = 3) +
  scale_fill_manual(values = c("green", "pink")) +
  geom_text(stat = "count", aes(label = ..count..), color = "black", size = 10) +
  labs(
    x = "",
    y = "",
    title = "TESTE"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "red")
  )


analise4 <- ggplot(dados_supp, aes(x = supp, y = media_len, fill = supp)) +
  geom_col(color = "yellow", linewidth = 3) +
  scale_fill_manual(values = c("green", "pink")) +
  geom_text(aes(label = round(media_len, 2)), color = "blue", size = 8) +
  labs(
    x = "Crescimento médio (mm)",
    y = "Suplemento",
    title = "grafico"
  ) +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.background = element_rect(fill = "lightgreen"),
    axis.text = element_text(color = "red", size = 16),
    axis.title = element_text(color = "orange", size = 18),
    plot.title = element_text(color = "purple", size = 20),
    legend.background = element_rect(fill = "yellow"),
    legend.text = element_text(color = "pink", size = 14),
    legend.title = element_text(color = "blue", size = 16)
  )

