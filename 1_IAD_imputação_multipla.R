## ÍNDICE DE AVALIAÇÃO DA DEMOCRACIA: 

# IMPUTAÇÃO MÚLTIPLA COM PMM

# LIMPAR AMBIENTE ---------------------------------------------------------

rm(list = ls())
gc()

# PACOTES -----------------------------------------------------------------

pacotes <- c("haven", "survey",
             "tidyverse",  # já carrega: dplyr, ggplot2, tidyr, purrr, stringr, scales
             "psych", "lavaan", "semTools", "mice", "VIM")  

instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(instalar)) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

# BANCO -------------------------------------------------------------------

vox2025 <- read_sav("vox2025.sav")

# CODIFICAÇÃO DAS VARIÁVEIS -----------------------------------------------

# VARIÁVEL INTERVENIENTE: VISÕES DE DEMOCRACIA

vox2025 <- vox2025 %>%
  mutate(P02A = ifelse(P02A > 10, NA, P02A), # Eleições livres e justas
         P04 = ifelse(P04 > 10, NA, P04),    # Competição política
         P05 = ifelse(P05 > 10, NA, P05),    # Liberdade de oposição
         P14 = ifelse(P14 > 10, NA, P14),    # Accountability vertical 1
         P12 = ifelse(P12 > 10, NA, P12),    # Accountability vertical 2
         P03 = ifelse(P03 > 10, NA, P03),    # Liberdade de expressão
         P06 = ifelse(P06 > 10, NA, P06),    # Liberdade da mídia 
         P07 = ifelse(P07 > 10, NA, P07),    # Confiabilidade da mídia
         P08 = ifelse(P08 > 10, NA, P08),    # Proteção a minorias
         P10 = ifelse(P10 > 10, NA, P10),    # Igualdade perante a lei
         P11 = ifelse(P11 > 10, NA, P11),    # Accountability horizontal
         P13 = ifelse(P13 > 10, NA, P13),    # Proteção social
         P18 = ifelse(P18 > 10, NA, P18),    # Bem-estar básico
         P19 = ifelse(P19 > 10, NA, P19),    # Equidade econômica
         P09 = ifelse(P09 > 10, NA, P09))    # Democracia direta

# VARIÁVEIS INDEPENDENTES: INSEGURANÇA E VITIMIZAÇÃO

vox2025 <- vox2025 %>% 
  mutate(inseguranca    = ifelse(P75 %in% c(3,4), 1, 0),
         vitimadireta   = ifelse(P73 == 1, 1, 0),
         vitimaindireta = ifelse(P74 == 1, 1, 0))

# CONTROLES

# SEXO
vox2025 <- vox2025 %>%
  mutate(mulher = ifelse(SEXO == 2, 1, 0))

# FAIXA ETÁRIA
vox2025 <- vox2025 %>%
  mutate(jovem_adulto = ifelse(IDADE_FX %in% c(1,2), 1, 0),
         adulto = ifelse(IDADE_FX %in% c(3,4,5), 1, 0),
         idoso = ifelse(IDADE_FX == 6, 1, 0))

# ESCOLARIDADE
vox2025 <- vox2025 %>%
  mutate(sem_instr_formal = ifelse(ESCOLARIDADE %in% c(1,2), 1, 0),
         ensino_superior  = ifelse(ESCOLARIDADE > 14, 1, 0))

# RENDA
vox2025 <- vox2025 %>%
  mutate(renda_baixa = ifelse(RENDA_1 == 6, 1, 0),
         renda_alta   = ifelse(RENDA_1 %in% c(1, 2), 1, 0))

# RAÇA
vox2025 <- vox2025 %>%
  mutate(nao_brancos = ifelse(RACA %in% c(2,3,4,5), 1, 0))

# IDEOLOGIA
vox2025 <- vox2025 %>% 
  mutate(direita = ifelse(P63 %in% c(8, 9, 10), 1, 0))

# VARIÁVEL DEPENDENTE: AVALIAÇÕES

vox2025 <- vox2025 %>%
  mutate(P23 = ifelse(P23 > 10, NA, P23), # Eleições livres e justas
         P24 = ifelse(P24 > 10, NA, P24), # Liberdade de expressão 
         P25 = ifelse(P25 > 10, NA, P25), # Competição política
         P26 = ifelse(P26 > 10, NA, P26), # Liberdade de oposição 
         P27 = ifelse(P27 > 10, NA, P27), # Liberdade da mídia
         P28 = ifelse(P28 > 10, NA, P28), # Confiabilidade da mídia
         P29 = ifelse(P29 > 10, NA, P29), # Proteção a minorias
         P30 = ifelse(P30 > 10, NA, P30), # Democracia direta
         P31 = ifelse(P31 > 10, NA, P31), # Igualdade perante a lei
         P32 = ifelse(P32 > 10, NA, P32), # Accountability vertical 1
         P33 = ifelse(P33 > 10, NA, P33), # Proteção social
         P34 = ifelse(P34 > 10, NA, P34), # Accountability vertical 2
         P35 = ifelse(P35 > 10, NA, P35)) # Equidade econômica


# IMPUTAÇÃO MÚLTIPLA COM PMM ---------------------------------------------------

# O que o algoritmo usa para imputar? Os outros itens do índice, mais as 
# variáveis que você já tem no banco (renda, escolaridade, sexo, etc.). A 
# lógica é: dado o perfil completo dessa pessoa, qual valor seria mais plausível 
# para o item que ela não respondeu?

# PMM (Predictive Mean Matching): encontra pessoas com perfil parecido que 
# responderam o item e "empresta" um valor real delas. Muito usado para escalas 
# Likert porque preserva os valores possíveis (não imputa 7.3 numa escala de
#                                              0 a 10).


# ETAPA 1 — DIAGNÓSTICO DOS DADOS AUSENTES -------------------------------------

# Antes de imputar qualquer coisa, você precisa entender quanto está faltando 
# e onde. Isso serve para dois propósitos: justificar a imputação e verificar 
# se o padrão de ausência é compatível com o pressuposto MAR (Missing At Random).

# MAR (Missing At Random) = a probabilidade de um dado estar ausente 
# pode depender de outras variáveis observadas no banco 
# (renda, escolaridade, etc.), mas não do próprio valor ausente. 
# Por exemplo: pessoas com menos escolaridade podem ter mais dificuldade de 
# responder certos itens — isso é MAR, e é tratável. Se a ausência dependesse 
# do valor em si (pessoas insatisfeitas se recusam a avaliar), seria MNAR, e aí 
# a imputação seria problemática.

# Percentual de ausentes por item do índice
ausentes_resumo <- vox2025 %>%
  select(P23:P35) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "pct_ausente") %>%
  arrange(desc(pct_ausente))

print(ausentes_resumo)

# Número total de casos completos antes da imputação
cat("Casos completos nos itens P23:P35 antes da imputação:",
    sum(complete.cases(vox2025 %>% select(P23:P35))), "\n")

# Mapa visual de ausência 
# dev.new(width = 12, height = 8)
# 
# aggr(vox2025 %>% select(P23:P35),
#      col      = c("steelblue", "tomato"),
#      numbers  = TRUE,
#      sortVars = TRUE,
#      labels   = names(vox2025 %>% select(P23:P35)),
#      ylab     = c("Proporção de ausentes", "Padrão de ausência"),
#      cex.axis = 0.7,
#      gap      = 3)

# E um gráfico simples de barras no lugar do aggr
# ggplot(ausentes_resumo, aes(x = reorder(item, pct_ausente), y = pct_ausente)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(title = "Percentual de dados ausentes por item (P23–P35)",
#        x = NULL, y = "% ausente") +
#   theme_minimal()

# ETAPA 2 — ESPECIFICAÇÃO DO MODELO DE IMPUTAÇÃO -------------------------------

# Aqui você define quais variáveis entram no modelo que vai prever os valores 
# ausentes. 
# A regra geral é: quanto mais informação você usar, melhor a imputação.

# Os 13 itens do índice (P23 a P35) — porque são correlacionados entre si, 
# e quem respondeu 12 itens dá muita informação sobre o 13º
# As variáveis sociodemográficas que você já tem: sexo, renda, escolaridade, 
# raça, ideologia
# As variáveis independentes do seu modelo: insegurança, vitimização direta, 
# vitimização indireta
# Lógica do PMM especificamente: para cada valor ausente, o algoritmo encontra 
# um conjunto de "doadores" (respondentes com perfil predito similar) e sorteia
# um valor real de um deles. Isso garante que o valor imputado esteja dentro da 
# escala original (0 a 10) e tenha distribuição realista.

# Seleciona as variáveis que entram no modelo de imputação:
# - Os 13 itens do índice (alvos da imputação + preditores entre si)
# - Variáveis auxiliares: sociodemográficas e independentes do modelo
# As variáveis auxiliares NÃO serão imputadas (predictorMatrix controla isso)

vars_imputacao <- vox2025 %>%
  select(P23, P24, P25, P26, P27, P28, P29, P30, P31, P32, P33, P34, P35,
         mulher, renda_baixa, renda_alta, 
         jovem_adulto, adulto, idoso,
         sem_instr_formal, ensino_superior,
         nao_brancos, direita, inseguranca, vitimadireta, vitimaindireta)

# Define a matriz de preditores:
# Linhas = variável a ser imputada
# Colunas = variável usada como preditora
# 1 = usa como preditor | 0 = não usa
ini <- mice(vars_imputacao, maxit = 0)  # rodada inicial só para extrair estrutura
pred <- ini$predictorMatrix

# Garante que as variáveis auxiliares NÃO sejam imputadas
# (linhas delas na predictorMatrix zeradas)
vars_aux <- c("mulher", "renda_baixa", "renda_alta", 
              "jovem_adulto", "adulto", "idoso",
              "sem_instr_formal", "ensino_superior", 
              "nao_brancos", 
              "direita",
              "inseguranca", "vitimadireta", "vitimaindireta")

pred[vars_aux, ] <- 0  # essas variáveis não recebem imputação

# ETAPA 3 - GERAÇÃO DOS M BANCOS IMPUTADOS -------------------------------------

# O pacote mice vai gerar M bancos de dados completos. O padrão recomendado é 
# M = 20 para pesquisa acadêmica 
# (alguns autores aceitam M = 5 para análises exploratórias, 
#   mas para dissertação vale usar 20).

# Cada banco terá valores ligeiramente diferentes para os casos imputados, 
# refletindo a incerteza sobre o valor real. Você então constrói o índice 
# (soma/média e normalização 0–10) dentro de cada um dos 20 bancos.

set.seed(2025)  # semente para reprodutibilidade

imp <- mice(vars_imputacao,
            m              = 20,       # 20 bancos imputados
            method         = "pmm",    # Predictive Mean Matching
            predictorMatrix = pred,
            maxit          = 10,       # 10 iterações por cadeia (padrão robusto)
            printFlag      = FALSE)    # suprime output iteração a iteração

# Diagnóstico de convergência: as cadeias devem se misturar sem tendência
plot(imp)  # inspecione visualmente — linhas entrelaçadas = bom sinal

# Verifica o método atribuído a cada variável (deve ser "pmm" para P23:P35)
print(ini$method)

# ETAPA 4 —  ANÁLISE E COMBINAÇÃO DOS RESULTADOS (REGRAS DE RUBIN) -------------

# Você roda suas análises — médias, regressões, o que for — nos 20 bancos 
# separadamente. Depois combina os resultados usando as Regras de Rubin, que 
# calculam:
# 
# A estimativa final como a média dos M resultados
# O erro padrão combinando a variância dentro dos bancos e a variância entre eles
# 
# Na prática, o R faz isso automaticamente com funções do próprio pacote mice. 
# Você não precisa fazer as contas na mão.

construir_indice <- function(dados) {
  dados %>%
    mutate(avaliacao_bruta = rowSums(across(P23:P35)),
           avaliacao = 10 * (avaliacao_bruta - min(avaliacao_bruta)) /
             (max(avaliacao_bruta) - min(avaliacao_bruta)))}

bancos_imputados <- lapply(1:20, function(i) {
  banco_imp <- complete(imp, action = i)
  
  banco_completo <- vox2025
  banco_completo[, names(banco_imp)] <- banco_imp
  
  construir_indice(banco_completo)})

# Verifica: todos os 20 bancos devem ter 1504 casos (ou o N total do seu banco)
cat("N em cada banco imputado:", nrow(bancos_imputados[[1]]), "\n")

# ETAPA 5 — ÍNDICE FINAL: MÉDIA ENTRE OS 20 BANCOS ------------------------

# Para uso descritivo e nas análises principais, extrai o índice como
# a média dos 20 valores imputados por respondente — isso é a estimativa
# pontual combinada pelas Regras de Rubin para uma variável contínua

avaliacao_media <- sapply(bancos_imputados, function(b) b$avaliacao)
vox2025$avaliacao <- rowMeans(avaliacao_media)

cat("N com índice após imputação:", sum(!is.na(vox2025$avaliacao)), "\n")


# TESTES DE CONFIABILIDADE ------------------------------------------------

# ANÁLISE FATORIAL CONFIRMATÓRIA (AFC)
# Roda no banco original com FIML — não precisa de imputação para a AFC
# porque o FIML já usa toda a informação disponível por caso

vars_afc <- vox2025 %>% select(P23:P35)

modelo_afc <- '
  avaliacao =~ P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 +
               P31 + P32 + P33 + P34 + P35'

resultado_afc <- cfa(modelo_afc, 
                     data      = vars_afc, 
                     estimator = "ML",
                     missing   = "fiml")  # FIML trata ausentes sem excluir casos

summary(resultado_afc, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitMeasures(resultado_afc, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

modificationIndices(resultado_afc, sort = TRUE, minimum.value = 3.84)

parameterEstimates(resultado_afc, standardized = TRUE) %>%
  filter(op == "=~") %>%
  select(lhs, rhs, est, std.all, pvalue)

reliability(resultado_afc)

lavInspect(resultado_afc, "rsquare")


# ÍNDICE DE AVALIAÇÃO DA DEMOCRACIA — ESTATÍSTICAS DESCRITIVAS ------------

# Desenho amostral com o índice já imputado
desenho <- svydesign(ids     = ~1,
                     weights = ~FATOR_POND,
                     data    = vox2025)

svymean(~avaliacao, desenho, na.rm = TRUE)
svyquantile(~avaliacao, desenho, c(0.5), na.rm = TRUE)
sqrt(svyvar(~avaliacao, desenho, na.rm = TRUE))

