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


library(dplyr)
library(tidyr)
library(knitr)

dados_dose <- ToothGrowth %>%
  group_by(dose) %>%
  summarise(media_len = mean(len))



analise1 <- ggplot(dados_dose) +
  aes(x = dose, y = media_len, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(size = 2, colour = "#A11D21") +
  labs(
    x = "Dose (mg)",
    y = "Crescimento médio do dente (mm)"
  ) +
  theme_estat()
ggsave("crescimento_medio_dose.pdf",
       analise1,
       width = 158,
       height = 93,
       units = "mm")



analise2 <- ToothGrowth %>%
  mutate(
    supp = case_when(
      supp == "OJ" ~ "Suco de laranja",
      supp == "VC" ~ "Ácido ascórbico"
    )
  ) %>%
  ggplot() +
  aes(x = dose, y = len) +
  geom_jitter(aes(colour = supp), size = 2) +
  labs(
    x = "Dose de vitamina C (mg)",
    y = "Comprimento do dente (mm)",
    colour = "Suplemento"
  ) +
  theme_estat()
ggsave("dispersao_dose_suplemento.pdf",
       analise2,
       width = 158,
       height = 93,
       units = "mm")



ToothGrowth2 <- ToothGrowth %>%
  mutate(
    supp = case_when(
      supp == "OJ" ~ "Suco de laranja",
      supp == "VC" ~ "Ácido ascórbico"
    )
  )
analise3 <- ggplot(ToothGrowth2) +
  aes(x = reorder(supp, len, FUN = median), y = len) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(
    fun = "mean",
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white"
  ) +
  labs(
    x = "Suplemento",
    y = "Comprimento do dente (mm)"
  ) +
  theme_estat() +
  scale_x_discrete()

ggsave("boxplot_suplemento.pdf",
       analise3,
       width = 158,
       height = 93,
       units = "mm")



tabela_lado <- ToothGrowth2 %>%
  group_by(supp) %>%
  summarise(
    Média = mean(len),
    `Desvio Padrão` = sd(len),
    Variância = var(len),
    Mínimo = min(len),
    `1º Quartil` = quantile(len, 0.25),
    Mediana = median(len),
    `3º Quartil` = quantile(len, 0.75),
    Máximo = max(len)
  ) %>%
  mutate(across(-supp, ~ round(., 2))) %>%
  pivot_longer(-supp) %>%
  pivot_wider(names_from = supp, values_from = value)


latex <- knitr::kable(
  tabela_lado,
  format = "latex",
  booktabs = TRUE,
  col.names = c("Estatística", "Suco de laranja", "Ácido ascórbico"),
  align = "lcc"
)


dados_supp <- ToothGrowth %>%
  group_by(supp) %>%
  summarise(media_len = mean(len))

dados_supp <- dados_supp %>%
  mutate(
    label = round(media_len, 2) %>% 
      gsub("\\.", ",", .)
  )



analise4 <- dados_supp %>%
  mutate(
    supp = case_when(
      supp == "OJ" ~ "Suco de laranja",
      supp == "VC" ~ "Ácido ascórbico"
    )
  ) %>%
  ggplot() +
  aes(
    x = fct_reorder(supp, media_len, .desc = TRUE),
    y = media_len,
    label = label
  ) +
  geom_col(fill = "#A11D21", width = 0.7) +
  geom_text(
    vjust = -0.5,
    size = 3
  ) +
  labs(
    x = "Suplemento",
    y = "Crescimento médio (mm)"
  ) +
  theme_estat() +
  scale_x_discrete()  