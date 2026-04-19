## SCRIPT DA DISSERTAÇÃO DE MARÍLIA FERREIRA

# LIMPAR AMBIENTE ---------------------------------------------------------

rm(list = ls())
gc()

# BANCO ------------------------------------------------------------------------

source(here::here("1_IAD_imputação_multipla.R"))

# PACOTES -----------------------------------------------------------------

pacotes <- c("here", "haven", "survey", "tidyverse", "janitor", "interactions", "rio",
             "patchwork", "car", "psych", "tidyr", "dplyr", "writexl", "readxl",
             "DescTools", "ggplot2", "descr", "scales", "purrr", "Hmisc",
             "coefplot", "sjPlot", "lavaanPlot", "lavaan", "stringr", "gt",
             "extrafont", "semTools", "diagram", "plotrix", "tibble", 
             "webshot2", "DiagrammeR", "DiagrammeRsvg")

instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(instalar)) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))


# FONTE -------------------------------------------------------------------

loadfonts()  
fonts <- fonttable()
loadfonts(device = "win")

# PASTAS PARA ELEMENTOS VISUAIS -----------------------------------------------

dir.create("graficos_analise", showWarnings = FALSE)
dir.create("tabelas_png", showWarnings = FALSE)

# GRÁFICO 1: DISTRIBUIÇÃO DOS PERFIS ------------------------------------------

# CRIAR OBJETO DE DESENHO AMOSTRAL COM PONDERAÇÃO
dados_ponderados <- svydesign(ids = ~1,  
                              weights = ~FATOR_POND, 
                              data = vox2025)

# CALCULAR TABELA PONDERADA
tabela_ponderada <- svytable(~tipologia_democracia, design = dados_ponderados)

# CALCULAR PERCENTUAIS PONDERADOS
percentual_ponderado <- prop.table(tabela_ponderada) * 100

# CRIAR DATA FRAME PARA O GRÁFICO CORRETAMENTE
dados_perfis_ponderados <- data.frame(
  Perfil = names(tabela_ponderada),
  Frequencia_ponderada = as.numeric(tabela_ponderada),
  Percentual_ponderado = as.numeric(percentual_ponderado))

dados_perfis_ponderados <- dados_perfis_ponderados[order(dados_perfis_ponderados$Percentual_ponderado), ]

# GRÁFICO COM DADOS PONDERADOS
grafico_1 <- ggplot(dados_perfis_ponderados, aes(x = reorder(Perfil, Percentual_ponderado), y = Percentual_ponderado)) +
  geom_bar(stat = "identity", fill = "#1f77b4", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentual_ponderado, 1), "%")), 
            hjust = -0.1, 
            size = 6,
            family = "Times New Roman") +
  labs(title = " ",
       x = " ",
       y = "Percentual (%)") +
  theme_minimal(base_size = 20, base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 50), expand = expansion(mult = c(0, 0.1)))

ggsave("graficos_analise/grafico_1.png",
       plot = grafico_1, width = 12, height = 8, dpi = 300, bg = "white")

# GRÁFICO 2: MÉDIAS DE VISÕES E AVALIAÇÕES -----------------------------------

variaveis_visoes <- c("P02A", "P04", "P05", "P03", "P06", "P07",
                      "P08", "P09", "P10", "P12", "P13", "P14", "P19")

nomes_visoes <- c("P02A" = "Eleições livres e justas",
                  "P04"  = "Competição política",
                  "P05"  = "Liberdade de oposição",
                  "P03"  = "Liberdade de expressão",
                  "P06"  = "Liberdade da mídia",
                  "P07"  = "Confiabilidade da mídia",
                  "P08"  = "Proteção a minorias",
                  "P09"  = "Democracia direta",
                  "P10"  = "Igualdade perante a lei",
                  "P12"  = "Accountability vertical 2",
                  "P13"  = "Proteção social",
                  "P14"  = "Accountability vertical 1",
                  "P19"  = "Equidade econômica")

variaveis_avaliacao <- c("P23", "P24", "P25", "P26", "P27", "P28",
                         "P29", "P30", "P31", "P32", "P33", "P34", "P35")

nomes_avaliacao <- c("P23" = "Eleições livres e justas",
                     "P24" = "Liberdade de expressão",
                     "P25" = "Competição política",
                     "P26" = "Liberdade de oposição",
                     "P27" = "Liberdade da mídia",
                     "P28" = "Confiabilidade da mídia",
                     "P29" = "Proteção a minorias",
                     "P30" = "Democracia direta",
                     "P31" = "Igualdade perante a lei",
                     "P32" = "Accountability vertical 1",
                     "P33" = "Proteção social",
                     "P34" = "Accountability vertical 2",
                     "P35" = "Equidade econômica")

media_ponderada_imp <- function(var) {
  medias <- sapply(bancos_imputados, function(b) {
    weighted.mean(b[[var]], b$FATOR_POND, na.rm = TRUE)})
  mean(medias)}

resultados_visoes <- map_df(variaveis_visoes, \(v) {
  data.frame(Codigo = v,
             Item   = nomes_visoes[v],
             Valor  = media_ponderada_imp(v))})

resultados_avaliacoes <- map_df(variaveis_avaliacao, \(v) {
  data.frame(Codigo = v,
             Item   = nomes_avaliacao[v],
             Valor  = media_ponderada_imp(v))})

correspondencia <- tibble(Item = nomes_visoes[variaveis_visoes],
                          Codigo_Visao = variaveis_visoes,
                          Codigo_Avaliacao = variaveis_avaliacao)

dados_ponderados <- correspondencia %>%
  left_join(resultados_visoes,  by = c("Codigo_Visao" = "Codigo")) %>%
  rename(Visoes = Valor) %>%
  left_join(resultados_avaliacoes, by = c("Codigo_Avaliacao" = "Codigo")) %>%
  rename(Avaliacoes = Valor) %>%
  select(Item, Avaliacoes, Visoes) %>%
  arrange(Avaliacoes)

dados_long <- dados_ponderados %>%
  pivot_longer(cols = c(Avaliacoes, Visoes),
               names_to = "Tipo",
               values_to = "Valor")

dados_long$Item <- factor(dados_long$Item, levels = dados_ponderados$Item)

grafico_2 <- ggplot(
  dados_long,
  aes(x = Item, y = Valor, fill = Tipo)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(label = round(Valor, 2)),
            position = position_dodge(width = 0.9),
            hjust = -0.05,
            size = 6,
            family = "Times New Roman") +
  scale_fill_manual(
    values = c("Avaliacoes" = "#1f77b4", "Visoes" = "#ff7f0e"),
    labels = c("Avaliações da democracia", "Visões de democracia")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 10)) +
  labs(title = " ",
       x = " ",
       y = "Pontuação (0–10)",
       fill = " ") +
  theme_minimal(base_size = 20) +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.caption = element_text(size = 14, hjust = 1))

# SALVAR

ggsave("graficos_analise/grafico_2.png",
       plot = grafico_2, width = 12, height = 8, dpi = 300, bg = "white")

# GRÁFICO 3: ÍNDICE DE AVALIAÇÃO DA DEMOCRACIA (PONDERADO) ---------------------

media_p <- weighted.mean(x = vox2025$avaliacao,
                         w = vox2025$FATOR_POND,
                         na.rm = TRUE)

mediana_p <- wtd.quantile(x = vox2025$avaliacao,
                          weights = vox2025$FATOR_POND,
                          probs = 0.5,
                          na.rm = TRUE)

range(vox2025$avaliacao, na.rm = TRUE)

# ESTATÍSTICAS DESCRITIVAS PONDERADAS

media_p <- as.numeric(svymean(~avaliacao, desenho, na.rm = TRUE))

q <- svyquantile(~avaliacao, desenho, 0.5, na.rm = TRUE)
mediana_p <- as.numeric(q$quantiles[1])

dp_p <- as.numeric(sqrt(svyvar(~avaliacao, desenho, na.rm = TRUE)))

desc_avaliacao <- tibble(
  N = sum(!is.na(vox2025$avaliacao)),
  Media = media_p,
  Mediana = mediana_p,
  Desvio_Padrao = dp_p,
  Minimo = min(vox2025$avaliacao, na.rm = TRUE),
  Maximo = max(vox2025$avaliacao, na.rm = TRUE))

# segurança: garantir que estão no range do eixo

media_plot   <- pmin(pmax(media_p, 0), 10)
mediana_plot <- pmin(pmax(mediana_p, 0), 10)

# DADOS PARA AS BARRAS (PONDERADOS E EM %)

dados_barras <- vox2025 %>% 
  dplyr::filter(!is.na(avaliacao)) %>%
  dplyr::mutate(avaliacao_bin = cut(avaliacao,
                                    breaks = seq(0, 10.1, by = 1),
                                    include.lowest = TRUE,
                                    right = FALSE,
                                    labels = c("0-1", "1.01-2", "2.01-3", 
                                               "3.01-4", "4.01-5", "5.01-6", 
                                               "6.01-7", "7.01-8", "8.01-9", 
                                               "9.01-10"))) %>%
  dplyr::group_by(avaliacao_bin) %>%
  dplyr::summarise(n = sum(FATOR_POND, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::mutate(percentual = (n / sum(n)) * 100,
                centro     = seq(0.5, 9.5, by = 1),
                label_pos  = percentual + max(percentual) * 0.05)

# HISTOGRAMA PONDERADO

grafico_3 <- ggplot(vox2025 %>% 
                     dplyr::filter(!is.na(avaliacao)),
                   aes(x = avaliacao)) +
  geom_histogram(aes(y = after_stat(100 * count / sum(count)),
                     weight = FATOR_POND),
                 breaks = seq(0, 10.1, by = 1),
                 fill = "#1f77b4",
                 alpha = 0.7,
                 color = "white",
                 closed = "left") +
  
  # Rótulos das barras
  geom_text(data = dados_barras,
            aes(x = centro,
                y = label_pos,
                label = paste0(round(percentual, 1), "%")),
            size = 5,
            family = "Times New Roman",
            fontface = "bold",
            color = "black") +
  
  # Linha da média
  geom_vline(xintercept = media_plot,
             color = "red",
             linetype = "dashed",
             linewidth = 1) +
  
  # Linha da mediana
  geom_vline(xintercept = mediana_plot,
             color = "darkgreen",
             linetype = "dotted",
             linewidth = 1) +
  
  # Texto da média
  annotate("text",
           x = media_plot,
           y = Inf,
           label = paste("Média:", round(media_plot, 2)),
           vjust = 2,
           hjust = -0.1,
           color = "red",
           size = 5,
           family = "Times New Roman") +
  
  # Texto da mediana
  annotate("text",
           x = mediana_plot,
           y = Inf,
           label = paste("Mediana:", round(mediana_plot, 2)),
           vjust = 4,
           hjust = -0.1,
           color = "darkgreen",
           size = 5,
           family = "Times New Roman") +
  
  labs(x = "Índice de Avaliação da Democracia (0–10)",
       y = "Percentual de respondentes") +
  
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.3))) +
  
  coord_cartesian(xlim = c(0, 10.5),
                  clip = "off") +
  
  theme_minimal(base_size = 20, base_family = "Times New Roman") +
  theme(plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title   = element_text(size = 18),
        axis.text    = element_text(size = 18),
        legend.position = "none",
        plot.margin  = margin(t = 20, r = 20, b = 40, l = 20))

# SALVAR

ggsave("graficos_analise/grafico_3.png",
       plot = grafico_3, width = 12, height = 8, dpi = 300, bg = "white")

# GRÁFICO 4: VARIÁVEIS DE SEGURANÇA ------------------------------------------

dados_completos <- vox2025[!is.na(vox2025$avaliacao) & 
                             !is.na(vox2025$inseguranca) & 
                             !is.na(vox2025$vitimadireta) & 
                             !is.na(vox2025$vitimaindireta), ]

cat("Tamanho da amostra com todos os dados completos:", nrow(dados_completos), 
    "\n")

inseguranca_ponderado <- c(
  Nao = sum(dados_completos$FATOR_POND[dados_completos$inseguranca == 0], 
            na.rm = TRUE),
  Sim = sum(dados_completos$FATOR_POND[dados_completos$inseguranca == 1], 
            na.rm = TRUE))

cat("INSEGURANÇA PONDERADA (apenas com avaliação):\n")
print(inseguranca_ponderado)
cat("Total:", sum(inseguranca_ponderado), "\n")
cat("Percentuais:\n")
print(round((inseguranca_ponderado / sum(inseguranca_ponderado)) * 100, 1))
cat("\n")

vitimadireta_ponderado <- c(
  Nao = sum(dados_completos$FATOR_POND[dados_completos$vitimadireta == 0], 
            na.rm = TRUE),
  Sim = sum(dados_completos$FATOR_POND[dados_completos$vitimadireta == 1], 
            na.rm = TRUE))

cat("VITIMAÇÃO DIRETA PONDERADA (apenas com avaliação):\n")
print(vitimadireta_ponderado)
cat("Total:", sum(vitimadireta_ponderado), "\n")
cat("Percentuais:\n")
print(round((vitimadireta_ponderado / sum(vitimadireta_ponderado)) * 100, 1))
cat("\n")

vitimaindireta_ponderado <- c(
  Nao = sum(dados_completos$FATOR_POND[dados_completos$vitimaindireta == 0], 
            na.rm = TRUE),
  Sim = sum(dados_completos$FATOR_POND[dados_completos$vitimaindireta == 1], 
            na.rm = TRUE))

cat("VITIMAÇÃO INDIRETA PONDERADA (apenas com avaliação):\n")
print(vitimaindireta_ponderado)
cat("Total:", sum(vitimaindireta_ponderado), "\n")
cat("Percentuais:\n")
print(round((vitimaindireta_ponderado / sum(vitimaindireta_ponderado)) * 100, 
            1))

dados_seguranca_ponderado <- data.frame(
  Variavel = c("Insegurança", 
               "Vitimização Direta", 
               "Vitimização Indireta"),
  Nao = c(inseguranca_ponderado["Nao"], 
          vitimadireta_ponderado["Nao"], 
          vitimaindireta_ponderado["Nao"]),
  Sim = c(inseguranca_ponderado["Sim"], 
          vitimadireta_ponderado["Sim"], 
          vitimaindireta_ponderado["Sim"])) %>%
  mutate(Total = Nao + Sim,
         Perc_Nao = round((Nao/Total)*100, 1),
         Perc_Sim = round((Sim/Total)*100, 1))

print(dados_seguranca_ponderado)

N_total_ponderado <- sum(dados_completos$FATOR_POND)

cat("\nN total ponderado:", round(N_total_ponderado), "\n")
cat("N de casos:", nrow(dados_completos), "\n")


dados_sim <- dados_seguranca_ponderado %>%
  mutate(Percentual = Perc_Sim) %>%
  select(Variavel, Percentual)

grafico_4 <- ggplot(dados_sim, aes(x = Variavel, y = Percentual, fill = Variavel)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(Percentual, "%")), 
            vjust = -0.5, 
            size = 6,
            family = "Times New Roman",
            fontface = "bold") +
  scale_fill_manual(values = c("Insegurança" = "#ff7f0e", 
                               "Vitimização Direta" = "#ff7f0e", 
                               "Vitimização Indireta" = "#ff7f0e")) +
  labs(title = NULL,
       x = "Variável",
       y = "Percentual de Respondentes (%)",
       fill = NULL,
       caption = paste("N:", 
                       format(round(N_total_ponderado), big.mark = ".", decimal.mark = ","))) +
  theme_minimal(base_size = 20, base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 25)),
        legend.position = "none", 
        plot.caption = element_text(size = 14, hjust = 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 40))

# SALVAR
ggsave("graficos_analise/grafico_4.png", 
       plot = grafico_4, width = 12, height = 8, dpi = 300, bg = "white")


# ANÁLISE DE MODERAÇÃO ----------------------------------------------------

# ANÁLISE SEM NÃO-DEMOCRATAS ----------------------------------------------

# PREPARAÇÃO DOS DADOS SEM NÃO-DEMOCRATAS

dados_analise_sem_nao_democratas <- vox2025 %>%
  filter(!is.na(avaliacao),
         !is.na(inseguranca),
         !is.na(vitimadireta), 
         !is.na(vitimaindireta),
         !is.na(tipologia_democracia)) %>%
  filter(!grepl("Não-democrata", tipologia_democracia, ignore.case = TRUE)) %>%
  mutate(perfil_democratico = factor(tipologia_democracia)) %>% 
  mutate(perfil_democratico = factor(perfil_democratico,
                                     levels = c("Maximalista Social Participativo", 
                                                "Maximalista Social",
                                                "Maximalista Participativo",
                                                "Maximalista",
                                                "Minimalista Social Participativo",
                                                "Minimalista Social",
                                                "Minimalista Participativo",
                                                "Minimalista")))

# FUNÇÃO PARA ANÁLISE DE MODERAÇÃO ------------------------------------

analisar_moderacao_simples <- function(bancos, var_seguranca, titulo = "") {
  
  # Prepara cada banco: filtra, cria e nivela o fator
  bancos <- lapply(bancos, function(b) {
    b %>%
      filter(!is.na(inseguranca),
             !is.na(vitimadireta),
             !is.na(vitimaindireta),
             !is.na(tipologia_democracia)) %>%
      filter(!grepl("Não-democrata", tipologia_democracia, ignore.case = TRUE)) %>%
      mutate(perfil_democratico = factor(tipologia_democracia,
                                         levels = c("Maximalista Social Participativo",
                                                    "Maximalista Social",
                                                    "Maximalista Participativo",
                                                    "Maximalista",
                                                    "Minimalista Social Participativo",
                                                    "Minimalista Social",
                                                    "Minimalista Participativo",
                                                    "Minimalista"))) %>%
      mutate(perfil_democratico = droplevels(perfil_democratico))
  })
  
  formula <- as.formula(paste0(
    "avaliacao ~ ", var_seguranca, " * perfil_democratico + ",
    "renda_baixa + renda_alta + mulher + ",
    "ensino_superior + sem_instr_formal + ",
    "nao_brancos + direita + ",
    "jovem_adulto + adulto + idoso"
  ))
  
  modelos <- lapply(bancos, function(b) {
    lm(formula, data = b, weights = FATOR_POND)
  })
  
  nomes_coef <- names(coef(modelos[[1]]))
  niveis     <- levels(bancos[[1]]$perfil_democratico)
  ref_level  <- niveis[1]
  
  combinar_efeito <- function(nivel) {
    
    if (nivel == ref_level) {
      estimativas <- sapply(modelos, function(m) coef(m)[var_seguranca])
      variancias  <- sapply(modelos, function(m) vcov(m)[var_seguranca, var_seguranca])
    } else {
      termo <- paste0(var_seguranca, ":perfil_democratico", nivel)
      
      if (!termo %in% nomes_coef) return(NULL)
      
      estimativas <- sapply(modelos, function(m) {
        val <- coef(m)[var_seguranca] + coef(m)[termo]
        if (is.na(val)) NA_real_ else val
      })
      
      variancias <- sapply(modelos, function(m) {
        v     <- vcov(m)
        val_v <- v[var_seguranca, var_seguranca] +
          v[termo, termo] +
          2 * v[var_seguranca, termo]
        if (is.na(val_v)) NA_real_ else val_v
      })
      
      # Se todos os 20 bancos retornaram NA, N insuficiente — avisa e pula
      if (all(is.na(estimativas))) {
        warning(paste0("Perfil '", nivel, "' omitido — N insuficiente para estimação (",
                       sum(bancos[[1]]$perfil_democratico == nivel, na.rm = TRUE),
                       " casos)."))
        return(NULL)
      }
    }
    
    # Regras de Rubin (apenas com bancos válidos)
    validos <- !is.na(estimativas)
    M       <- sum(validos)
    Q_bar   <- mean(estimativas, na.rm = TRUE)
    U_bar   <- mean(variancias,  na.rm = TRUE)
    B       <- var(estimativas,  na.rm = TRUE)
    T_var   <- U_bar + (1 + 1/M) * B
    se_comb <- sqrt(T_var)
    p_val   <- 2 * (1 - pnorm(abs(Q_bar / se_comb)))
    
    data.frame(perfil        = nivel,
               efeito        = Q_bar,
               se            = se_comb,
               lower         = Q_bar - 1.96 * se_comb,
               upper         = Q_bar + 1.96 * se_comb,
               p_value       = p_val,
               significativo = p_val < 0.05)
  }
  
  resultados <- bind_rows(lapply(niveis, combinar_efeito))
  
  p <- ggplot(resultados,
              aes(x = reorder(perfil, efeito), y = efeito, color = significativo)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, linewidth = 1) +
    scale_color_manual(values = c("FALSE" = "#2C3E50", "TRUE" = "red"),
                       labels = c("FALSE" = "Não", "TRUE" = "Sim"),
                       na.translate = FALSE) +
    labs(x     = "Perfil",
         y     = paste("Efeitos da", titulo, "no Índice de Avaliação da Democracia"),
         title = NULL,
         color = "Significativo\n(p < 0.05)") +
    theme_minimal(base_size = 20, base_family = "Times New Roman") +
    theme(text            = element_text(family = "Times New Roman"),
          plot.title      = element_blank(),
          axis.text.x     = element_text(angle = 45, hjust = 1),
          legend.position = "bottom") +
    coord_flip()
  
  return(list(modelos = modelos, resultados = resultados, grafico = p))
}
# EXECUTAR NOS 20 BANCOS IMPUTADOS ------------------------------------

resultado_inseguranca <- analisar_moderacao_simples(
  bancos_imputados, "inseguranca", "Insegurança")

resultado_vitimadireta <- analisar_moderacao_simples(
  bancos_imputados, "vitimadireta", "Vitimização Direta")

resultado_vitimaindireta <- analisar_moderacao_simples(
  bancos_imputados, "vitimaindireta", "Vitimização Indireta")

# GRÁFICOS 5-7 --------------------------------------------------------

# EXPORTAR GRÁFICOS EM PNG

ggsave("graficos_analise/grafico_5.png",
       plot = resultado_inseguranca$grafico,  width = 12, height = 8, dpi = 300, bg = "white")
ggsave("graficos_analise/grafico_6.png",
       plot = resultado_vitimadireta$grafico, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("graficos_analise/grafico_7.png",
       plot = resultado_vitimaindireta$grafico, width = 12, height = 8, dpi = 300, bg = "white")

# GRÁFICO 8: COMPARATIVO SEM NÃO-DEMOCRATAS ---------------------------

padronizar_resultados <- function(resultado, nome_variavel) {
  df <- resultado$resultados
  df$variavel <- nome_variavel
  return(df)
}

todos_resultados <- rbind(
  padronizar_resultados(resultado_inseguranca,    "Insegurança"),
  padronizar_resultados(resultado_vitimadireta,   "Vitimização Direta"),
  padronizar_resultados(resultado_vitimaindireta, "Vitimização Indireta")) %>%
  filter(!is.na(perfil) & !is.na(significativo))

grafico_comparativo <- ggplot(todos_resultados, 
                              aes(x = perfil, y = efeito, color = variavel)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.3,
                position = position_dodge(width = 0.5), 
                linewidth = 1.2) +  # <-- size depreciado, usar linewidth
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", 
             linewidth = 0.8) +  # <-- idem
  facet_wrap(~ significativo,
             labeller = as_labeller(c("TRUE"  = "Efeitos significativos",
                                      "FALSE" = "Efeitos não-significativos"))) +
  scale_y_continuous(limits = c(-12, 12)) +
  scale_color_brewer(palette = "Set1") +
  labs(x     = "Perfil",
       y     = "Efeitos no Índice de Avaliação da Democracia",
       color = "Variáveis") +
  theme_minimal(base_size = 20, base_family = "Times New Roman") +
  theme(text            = element_text(family = "Times New Roman"),
        plot.title      = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  coord_flip()

ggsave("graficos_analise/grafico_8.png",
       plot = grafico_comparativo, width = 14, height = 10, dpi = 300, bg = "white")

# ANÁLISE COM NÃO-DEMOCRATAS ------------------------------------

# FUNÇÃO PARA ANÁLISE COM NÃO-DEMOCRATAS ------------------------------

analisar_moderacao_completa <- function(bancos, var_seguranca, titulo = "") {
  
  # Prepara cada banco: filtra, cria e nivela o fator
  bancos <- lapply(bancos, function(b) {
    b %>%
      filter(!is.na(inseguranca),
             !is.na(vitimadireta),
             !is.na(vitimaindireta),
             !is.na(tipologia_democracia)) %>%
      mutate(perfil_democratico = factor(tipologia_democracia,
                                         levels = c("Não-democrata",
                                                    "Minimalista",
                                                    "Minimalista Participativo",
                                                    "Minimalista Social",
                                                    "Minimalista Social Participativo",
                                                    "Maximalista",
                                                    "Maximalista Participativo",
                                                    "Maximalista Social",
                                                    "Maximalista Social Participativo"))) %>%
      mutate(perfil_democratico = droplevels(perfil_democratico))
  })
  
  formula <- as.formula(paste0(
    "avaliacao ~ ", var_seguranca, " * perfil_democratico + ",
    "renda_baixa + renda_alta + mulher + ",
    "ensino_superior + sem_instr_formal + ",
    "nao_brancos + direita + ",
    "jovem_adulto + adulto + idoso"
  ))
  
  modelos <- lapply(bancos, function(b) {
    lm(formula, data = b, weights = FATOR_POND)
  })
  
  nomes_coef <- names(coef(modelos[[1]]))
  niveis     <- levels(bancos[[1]]$perfil_democratico)
  ref_level  <- niveis[1]
  
  combinar_efeito <- function(nivel) {
    
    if (nivel == ref_level) {
      estimativas <- sapply(modelos, function(m) coef(m)[var_seguranca])
      variancias  <- sapply(modelos, function(m) vcov(m)[var_seguranca, var_seguranca])
    } else {
      termo <- paste0(var_seguranca, ":perfil_democratico", nivel)
      
      if (!termo %in% nomes_coef) return(NULL)
      
      estimativas <- sapply(modelos, function(m) {
        val <- coef(m)[var_seguranca] + coef(m)[termo]
        if (is.na(val)) NA_real_ else val
      })
      
      variancias <- sapply(modelos, function(m) {
        v     <- vcov(m)
        val_v <- v[var_seguranca, var_seguranca] +
          v[termo, termo] +
          2 * v[var_seguranca, termo]
        if (is.na(val_v)) NA_real_ else val_v
      })
      
      # Se todos os 20 bancos retornaram NA, N insuficiente — avisa e pula
      if (all(is.na(estimativas))) {
        warning(paste0("Perfil '", nivel, "' omitido — N insuficiente para estimação (",
                       sum(bancos[[1]]$perfil_democratico == nivel, na.rm = TRUE),
                       " casos)."))
        return(NULL)
      }
    }
    
    # Regras de Rubin (apenas com bancos válidos)
    M       <- sum(!is.na(estimativas))
    Q_bar   <- mean(estimativas, na.rm = TRUE)
    U_bar   <- mean(variancias,  na.rm = TRUE)
    B       <- var(estimativas,  na.rm = TRUE)
    T_var   <- U_bar + (1 + 1/M) * B
    se_comb <- sqrt(T_var)
    p_val   <- 2 * (1 - pnorm(abs(Q_bar / se_comb)))
    
    data.frame(
      perfil        = nivel,
      efeito        = Q_bar,
      se            = se_comb,
      lower         = Q_bar - 1.96 * se_comb,
      upper         = Q_bar + 1.96 * se_comb,
      p_value       = p_val,
      significativo = p_val < 0.05,
      tipo_perfil   = case_when(
        nivel == "Não-democrata" ~ "Não-democratas",
        grepl("Min", nivel)      ~ "Minimalistas",
        grepl("Max", nivel)      ~ "Maximalistas"
      )
    )
  }
  
  resultados <- bind_rows(lapply(niveis, combinar_efeito))
  
  p <- ggplot(resultados,
              aes(x = reorder(perfil, efeito), y = efeito,
                  color = tipo_perfil, shape = significativo)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, linewidth = 1) +
    scale_color_manual(values = c("Não-democratas" = "black",
                                  "Minimalistas"   = "blue",
                                  "Maximalistas"   = "red"),
                       name = "Tipo de Perfil",
                       na.translate = FALSE) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                       labels = c("FALSE" = "Não", "TRUE" = "Sim"),
                       name = "Significativo (p < 0.05)",
                       na.translate = FALSE) +
    labs(x = "Perfil",
         y = paste("Efeitos da", titulo, "no Índice de Avaliação da Democracia")) +
    theme_minimal(base_size = 20, base_family = "Times New Roman") +
    theme(text             = element_text(family = "Times New Roman"),
          plot.title       = element_blank(),
          axis.text.x      = element_text(angle = 45, hjust = 1),
          legend.position  = "bottom",
          legend.box       = "horizontal",
          legend.direction = "horizontal",
          legend.box.just  = "center",
          legend.spacing.x = unit(1, "cm")) +
    coord_flip() +
    guides(color = guide_legend(title.position = "top", nrow = 1),
           shape = guide_legend(title.position = "top", nrow = 1))
  
  return(list(modelos = modelos, resultados = resultados, grafico = p))
}

# EXECUTAR NOS 20 BANCOS IMPUTADOS ------------------------------------

resultado_inseg   <- analisar_moderacao_completa(
  bancos_imputados, "inseguranca",    "Insegurança")
resultado_vitdir  <- analisar_moderacao_completa(
  bancos_imputados, "vitimadireta",   "Vitimização Direta")
resultado_vitind  <- analisar_moderacao_completa(
  bancos_imputados, "vitimaindireta", "Vitimização Indireta")

# GRÁFICOS 9-11 -------------------------------------------------------

ggsave("graficos_analise/grafico_9.png",
       plot = resultado_inseg$grafico,  width = 12, height = 8, dpi = 300, bg = "white")
ggsave("graficos_analise/grafico_10.png",
       plot = resultado_vitdir$grafico, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("graficos_analise/grafico_11.png",
       plot = resultado_vitind$grafico, width = 12, height = 8, dpi = 300, bg = "white")

# GRÁFICO 12: COMPARATIVO COM NÃO-DEMOCRATAS --------------------------

todos_resultados_completos <- rbind(
  resultado_inseg$resultados  %>% mutate(variavel = "Insegurança"),
  resultado_vitdir$resultados %>% mutate(variavel = "Vitimização Direta"),
  resultado_vitind$resultados %>% mutate(variavel = "Vitimização Indireta"))

labels_quebrados <- c(
  "Maximalista"                    = "Maximalista",
  "Maximalista Participativo"      = "Maximalista\nParticipativo",
  "Maximalista Social"             = "Maximalista\nSocial",
  "Maximalista Social Participativo" = "Maximalista\nSocial\nParticipativo",
  "Minimalista"                    = "Minimalista",
  "Minimalista Participativo"      = "Minimalista\nParticipativo",
  "Minimalista Social"             = "Minimalista\nSocial",
  "Minimalista Social Participativo" = "Minimalista\nSocial\nParticipativo",
  "Não-democrata"                  = "Não-\ndemocrata")

todos_resultados_completos <- todos_resultados_completos %>%
  mutate(variavel = factor(variavel, 
                           levels = c("Insegurança", 
                                      "Vitimização Direta", 
                                      "Vitimização Indireta")))

fazer_painel <- function(grupo) {
  
  dados_grupo <- todos_resultados_completos %>%
    filter(tipo_perfil == grupo) %>%
    mutate(variavel = factor(variavel,          # <-- aqui dentro
                             levels = c("Insegurança",
                                        "Vitimização Direta",
                                        "Vitimização Indireta")))
  
  lims <- limites[[grupo]]
  
  ggplot(dados_grupo,
         aes(x = reorder(perfil, efeito), y = efeito,
             color = variavel, shape = significativo)) +
    geom_point(size = 5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2,
                  position = position_dodge(width = 0.5),
                  linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
    scale_color_manual(values = c("Insegurança"          = "#E41A1C",
                                  "Vitimização Direta"   = "#377EB8",
                                  "Vitimização Indireta" = "#4DAF4A"),
                       name = "Variáveis",
                       na.translate = FALSE) +
    scale_shape_manual(values = c("FALSE" = 1, "TRUE" = 16),
                       labels = c("FALSE" = "Não", "TRUE" = "Sim"),
                       name = "Significativo (p < 0.05)",
                       na.translate = FALSE) +
    scale_x_discrete(labels = labels_quebrados) +
    ggtitle(grupo) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 20, base_family = "Times New Roman") +
    theme(text            = element_text(family = "Times New Roman"),
          plot.title      = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.text.y     = element_text(size = 20, hjust = 0.5),
          axis.text.x     = element_text(size = 20, hjust = 0.5),
          legend.position = "none") +
    coord_flip(ylim = lims)
}

limites <- list(
  "Maximalistas"   = c(-5, 5),
  "Minimalistas"   = c(-12, 12),
  "Não-democratas" = c(-5, 5)
)

p_max <- fazer_painel("Maximalistas")
p_min <- fazer_painel("Minimalistas")
p_nao <- fazer_painel("Não-democratas")

grafico_12 <- (p_max | p_min | p_nao) +
  plot_layout(guides = "collect") &
  theme(legend.position  = "bottom",
        legend.box       = "horizontal",
        legend.box.just  = "center",
        legend.spacing.x = unit(1, "cm"))

ggsave("graficos_analise/grafico_12.png",
       plot = grafico_12, width = 18, height = 10, dpi = 300, bg = "white")

# GRÁFICO 13: ESPECÍFICO DOS NÃO-DEMOCRATAS ---------------------------

nao_democratas_resultados <- rbind(
  resultado_inseg$resultados  %>% filter(perfil == "Não-democrata") %>%
    mutate(variavel = "Insegurança"),
  resultado_vitdir$resultados %>% filter(perfil == "Não-democrata") %>%
    mutate(variavel = "Vitimização Direta"),
  resultado_vitind$resultados %>% filter(perfil == "Não-democrata") %>%
    mutate(variavel = "Vitimização Indireta"))

grafico_13 <- ggplot(nao_democratas_resultados,
                     aes(x = variavel, y = efeito, fill = significativo)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "lightgray", "TRUE" = "darkred"),
                    labels = c("FALSE" = "Não", "TRUE" = "Sim"),
                    name = "Significativo") +
  labs(x = "Variável de Segurança",
       y = "Coeficiente de Efeito") +
  theme_minimal(base_size = 20, base_family = "Times New Roman") +
  theme(text            = element_text(family = "Times New Roman"),
        plot.title      = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("graficos_analise/grafico_13.png",
       plot = grafico_13, width = 12, height = 8, dpi = 300, bg = "white")

# EXPORTAR TABELAS EM PNG --------------------------------------------------------

criar_tabela <- function(resultado, titulo) {
  
  # Rótulos legíveis para todas as variáveis
  rotulos <- c(
    "Eleições livres e justas",
    "Liberdade de expressão",
    "Competição política",
    "Liberdade de oposição",
    "Liberdade da mídia",
    "Confiabilidade da mídia",
    "Proteção a minorias",
    "Democracia direta",
    "Igualdade perante a lei",
    "Accountability vertical 1",
    "Proteção social",
    "Accountability vertical 2",
    "Equidade econômica",
    "Mulher",
    "Renda baixa",
    "Renda alta",
    "Ensino superior",
    "Sem instrução formal",
    "Não-brancos",
    "Ser de direita",
    "Jovem adulto",
    "Adulto",
    "Idoso"
  )
  
  # Combina N e R² dos 20 modelos pelas Regras de Rubin
  n_obs   <- nobs(resultado$modelos[[1]])
  r2_vals <- sapply(resultado$modelos, function(m) summary(m)$r.squared)
  r2_med  <- mean(r2_vals)
  
  # Extrai coeficientes e SEs de todos os controles combinados entre os 20 modelos
  extrair_controles <- function(nome_var) {
    estimativas <- sapply(resultado$modelos, function(m) {
      cf <- coef(m)
      if (nome_var %in% names(cf)) cf[nome_var] else NA
    })
    variancias <- sapply(resultado$modelos, function(m) {
      v <- vcov(m)
      if (nome_var %in% rownames(v)) v[nome_var, nome_var] else NA
    })
    
    if (all(is.na(estimativas))) return(NULL)
    
    M       <- sum(!is.na(estimativas))
    Q_bar   <- mean(estimativas, na.rm = TRUE)
    U_bar   <- mean(variancias,  na.rm = TRUE)
    B       <- var(estimativas,  na.rm = TRUE)
    T_var   <- U_bar + (1 + 1/M) * B
    se_comb <- sqrt(T_var)
    p_val   <- 2 * (1 - pnorm(abs(Q_bar / se_comb)))
    
    data.frame(
      perfil        = nome_var,
      efeito        = Q_bar,
      se            = se_comb,
      lower         = Q_bar - 1.96 * se_comb,
      upper         = Q_bar + 1.96 * se_comb,
      p_value       = p_val,
      significativo = p_val < 0.05,
      tipo_linha    = "Controle",
      stringsAsFactors = FALSE
    )
  }
  
  vars_controle <- c("mulher", "renda_baixa", "renda_alta",
                     "ensino_superior", "sem_instr_formal",
                     "nao_brancos", "direita",
                     "jovem_adulto", "adulto", "idoso")
  
  controles <- bind_rows(lapply(vars_controle, extrair_controles))
  
  # Junta efeitos de moderação + controles
  dados_moderacao <- resultado$resultados %>%
    mutate(tipo_linha = "Moderação")
  
  dados_completos <- bind_rows(dados_moderacao, controles)
  
  # Rótulos legíveis para controles
  labels_controles <- c(
    "mulher"           = "Mulher",
    "renda_baixa"      = "Renda baixa",
    "renda_alta"       = "Renda alta",
    "ensino_superior"  = "Ensino superior",
    "sem_instr_formal" = "Sem instrução formal",
    "nao_brancos"      = "Não-brancos",
    "direita"          = "Ser de direita",
    "jovem_adulto"     = "Jovem adulto",
    "adulto"           = "Adulto",
    "idoso"            = "Idoso"
  )
  
  dados_completos <- dados_completos %>%
    mutate(perfil = ifelse(perfil %in% names(labels_controles),
                           labels_controles[perfil],
                           perfil))
  
  # Formata efeito e p-valor
  dados_formatados <- dados_completos %>%
    mutate(
      efeito_formatado = case_when(
        p_value < 0.001 ~ paste0(round(efeito, 3), "***"),
        p_value < 0.01  ~ paste0(round(efeito, 3), "**"),
        p_value < 0.05  ~ paste0(round(efeito, 3), "*"),
        TRUE            ~ as.character(round(efeito, 3))),
      p_value_formatado = case_when(
        p_value < 0.001 ~ "< 0.001",
        TRUE            ~ as.character(round(p_value, 3)))) %>%
    select(tipo_linha, perfil, efeito_formatado, se, lower, upper,
           p_value_formatado, significativo)
  
  dados_formatados %>%
    gt(groupname_col = "tipo_linha") %>%
    
    fmt_number(columns = c(se, lower, upper), decimals = 3) %>%
    
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = significativo)) %>%
    
    tab_style(style = cell_fill(color = "#f5f5f5"),
              locations = cells_row_groups()) %>%
    
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups()) %>%
    
    cols_label(perfil            = "Variável",
               efeito_formatado  = "Efeito",
               se                = "Erro Padrão",
               lower             = "IC Inferior",
               upper             = "IC Superior",
               p_value_formatado = "P-Valor") %>%
    
    cols_hide(columns = significativo) %>%
    
    cols_width(perfil            ~ px(220),
               efeito_formatado  ~ px(90),
               se                ~ px(90),
               lower             ~ px(90),
               upper             ~ px(90),
               p_value_formatado ~ px(90)) %>%
    
    cols_align(align = "center",
               columns = c(efeito_formatado, se, lower, upper, p_value_formatado)) %>%
    cols_align(align = "left", columns = perfil) %>%
    
    opt_table_font(font = "Times New Roman") %>%
    
    tab_options(
      table.width                        = px(750),
      table.background.color             = "white",
      table.font.size                    = px(14),
      column_labels.font.size            = px(14),
      column_labels.font.weight          = "bold",
      column_labels.background.color     = "white",
      column_labels.border.top.color     = "black",
      column_labels.border.bottom.color  = "black",
      data_row.padding                   = px(8),
      data_row.padding.horizontal        = px(8),
      table.border.top.color             = "black",
      table.border.bottom.color          = "black",
      source_notes.background.color      = "white",
      source_notes.padding               = px(6),
      source_notes.font.size             = px(14)) %>%
    
    tab_source_note(source_note = md(paste0(
      "N = ", n_obs,
      "<br>R² médio (20 imputações) = ", round(r2_med, 3),
      "<br>*** p < 0.001, ** p < 0.01, * p < 0.05"))) %>%
    
    tab_style(style = cell_text(align = "center"),
              locations = cells_source_notes())}

# LISTA DE RESULTADOS -------------------------------------------------

resultados <- list(
  list(dados = resultado_inseguranca,   nome = "Insegurança"),
  list(dados = resultado_vitimadireta,  nome = "Vitimização Direta"),
  list(dados = resultado_vitimaindireta,nome = "Vitimização Indireta"),
  list(dados = resultado_inseg,         nome = "Insegurança - Todos os Perfis"),
  list(dados = resultado_vitdir,        nome = "Vitimização Direta - Todos os Perfis"),
  list(dados = resultado_vitind,        nome = "Vitimização Indireta - Todos os Perfis"))

# EXPORTAR ------------------------------------------------------------

for (i in seq_along(resultados)) {
  cat("Exportando:", resultados[[i]]$nome, "\n")
  
  tabela_bonita <- criar_tabela(resultado = resultados[[i]]$dados,
                                titulo    = resultados[[i]]$nome)
  
  gtsave(tabela_bonita,
         file = paste0("tabelas_png/", sprintf("%02d", i), "_",
                       gsub(" ", "_", tolower(resultados[[i]]$nome)), ".png"),
         vwidth = 750)}

cat("Todas as tabelas exportadas para 'tabelas_png/'\n")

