library(lubridate)
library(dplyr)

tabela_apropriacao <- data.frame(matrix(NA, nrow = 4, ncol = 15))
colnames(tabela_apropriacao) <- c(
  "Descricao", "Anterior", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", 
  "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro", "Total"
)

tabela_apropriacao[, 1] <- c(
  "Dia_Inicio", "Dia_Fim", "Juros_Reais_Apropriados", "Atualização_Mon_Apropriada")

tabela_apropriacao[1, 2] <- df_input$Data_Estoque_Inicial

data_inicio <- as.Date(df_input$Data_Estoque_Inicial, format = "%d/%m/%Y")
tabela_apropriacao[1, 2] <- format(data_inicio, "%d/%m/%Y")
tabela_apropriacao[2, 2] <- format(ceiling_date(data_inicio, "month") - days(1), "%d/%m/%Y")

for (i in 3:14) {
  ultimo_dia_mes_anterior <- as.Date(tabela_apropriacao[2, i - 1], format = "%d/%m/%Y")
  
  if (i == 2) {
    tabela_apropriacao[2, i] <- format(ceiling_date(ultimo_dia_mes_anterior, "month"), "%d/%m/%Y")
  } else {
    primeiro_dia_mes_atual <- ultimo_dia_mes_anterior + days(1)
    ultimo_dia_mes_atual <- ceiling_date(primeiro_dia_mes_atual, "month") - days(1)
    
    tabela_apropriacao[1, i] <- format(primeiro_dia_mes_atual, "%d/%m/%Y")
    tabela_apropriacao[2, i] <- format(ultimo_dia_mes_atual, "%d/%m/%Y")
  }
}

tabela_apropriacao[2, 2] <- tabela_apropriacao[1, 3]  
tabela_apropriacao[1, 15] <- tabela_apropriacao[1, 2]  
tabela_apropriacao[2, 15] <- tabela_apropriacao[2, 14]



# Juros_Reais_Apropriados
for (i in 2:14) {
  juros_anterior <- recalculo %>%
    filter(Data == tabela_apropriacao[2, i]) %>%
    select(Juros_Acumulados_para_Variacoes_Patrimoniais) %>%
    pull()
  
  juros_inicial <- recalculo %>%
    filter(Data == tabela_apropriacao[1, i]) %>%
    select(Juros_Acumulados_para_Variacoes_Patrimoniais) %>%
    pull()
  
  if (length(juros_anterior) == 0 || is.na(juros_anterior)) {
    juros_anterior <- 0
  }
  
  if (length(juros_inicial) == 0 || is.na(juros_inicial)) {
    juros_inicial <- 0
  }
  
  juros_reais_anterior <- juros_anterior - juros_inicial
  
  tabela_apropriacao[3, i] <- juros_reais_anterior
}

# Atualização_Mon_Apropriada
for (i in 2:14) {
  atualizacao_anterior <- recalculo %>%
    filter(Data == tabela_apropriacao[2, i]) %>%
    select(Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais) %>%
    pull()
  
  atualizacao_inicial <- recalculo %>%
    filter(Data == tabela_apropriacao[1, i]) %>%
    select(Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais) %>%
    pull()
  
  if (length(atualizacao_anterior) == 0 || is.na(atualizacao_anterior)) {
    atualizacao_anterior <- 0
  }
  
  if (length(atualizacao_inicial) == 0 || is.na(atualizacao_inicial)) {
    atualizacao_inicial <- 0
  }
  
  atualizacao_Mone <- atualizacao_anterior - atualizacao_inicial
  
  tabela_apropriacao[4, i] <- atualizacao_Mone
}


# Total

for (i in 15:15) {
  juros_anterior_Total <- recalculo %>%
    filter(Data == tabela_apropriacao[2, i]) %>%
    select(Juros_Acumulados_para_Variacoes_Patrimoniais) %>%
    pull()
  
  juros_inicial_Total <- recalculo %>%
    filter(Data == tabela_apropriacao[1, i]) %>%
    select(Juros_Acumulados_para_Variacoes_Patrimoniais) %>%
    pull()
  
  if (length(juros_anterior_Total) == 0 || is.na(juros_anterior_Total)) {
    juros_anterior_Total <- 0
  }
  
  if (length(juros_inicial_Total) == 0 || is.na(juros_inicial_Total)) {
    juros_inicial_Total <- 0
  }
  
  juros_reais_anterior_total <- juros_anterior_Total - juros_inicial_Total
  
  tabela_apropriacao[3, i] <- juros_reais_anterior_total
}

for (i in 15:15) {
  atualizacao_anterior_total <- recalculo %>%
    filter(Data == tabela_apropriacao[2, i]) %>%
    select(Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais) %>%
    pull()
  
  atualizacao_inicial_total <- recalculo %>%
    filter(Data == tabela_apropriacao[1, i]) %>%
    select(Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais) %>%
    pull()
  
  if (length(atualizacao_anterior_total) == 0 || is.na(atualizacao_anterior_total)) {
    atualizacao_anterior_total <- 0
  }
  
  if (length(atualizacao_inicial_total) == 0 || is.na(atualizacao_inicial_total)) {
    atualizacao_inicial_total <- 0
  }
  
  atualizacao_Mone_total <- atualizacao_inicial_total - atualizacao_inicial_total
  
  tabela_apropriacao[4, i] <- atualizacao_Mone_total
}

