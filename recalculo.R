# Definir as datas do intervalo 
datas <- format(seq(as.Date("2021-09-01"), as.Date("2023-09-30"), by = "day"), "%d/%m/%Y")


n <- length(datas)
recalculo <- data.frame(
  indice = numeric(n), 
  Data = character(n),
  Qtd_Total_Emitido = numeric(n),
  Valor_Financeiro_Total_Emitido = numeric(n),
  Valor_Contabil_de_Emissao = numeric(n),
  Qtd_Total_Acumulado = numeric(n),
  Saldo_Devedor_Inicio_Dia = numeric(n),
  Juros_Reais_Acumulados_Inicio_do_Dia = numeric(n),
  Atualizacao_Monetaria_Acumulada_Inicio_do_Dia = numeric(n),
  Principal_Acumulado_Inicio_Dia = numeric(n),
  Pagamento_Cupom = numeric(n),
  Pagamento_Vencimento = numeric(n),
  Pagamento_Total = numeric(n),
  Pagamento_Juros_Reais = numeric(n),
  Pagamento_Atualizacao_Monetaria = numeric(n),
  Pagamento_Principal = numeric(n),
  Saldo_Devedor_Apos_Pagamentos = numeric(n),
  PMU = numeric(n),
  PMU_Iterativo = numeric(n),
  Cupom_Unitario = numeric(n),
  Pagamento_Vencimento_Unitario = numeric(n),
  Fluxo_de_Caixa_Iterativo = numeric(n),
  TIR_Atual_Iterativo = numeric(n),
  Juros_Efetivos_Nominais_Diario = numeric(n),
  Indice_de_Atualizacao_Monetaria_Diario = numeric(n),
  Juros_Efetivos_Reais_Diario = numeric(n),
  Juros_Efetivos_Nominais_Diario_R = numeric(n),
  Atualizacao_Monetaria_Diaria_R = numeric(n),
  Juros_Efetivos_Reais_Diario_R = numeric(n),
  Saldo_Devedor_Fim_Dia = numeric(n),
  Principal_Acumulado_Fim_do_Dia = numeric(n),
  Atualizacao_Monetaria_Acumulada_Fim_do_Dia = numeric(n),
  Juros_Reais_Acumulados_Fim_do_Dia = numeric(n),
  Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais = numeric(n),
  Juros_Acumulados_para_Variacoes_Patrimoniais = numeric(n)
)

df_input$Numero_de_Linhas <- nrow(recalculo)
recalculo$indice <- seq_len(nrow(recalculo))
recalculo$Data <- datas

# Preencher as colunas de Financeiro e Venda
for (i in 1:nrow(recalculo)) {
  data_recalculo <- recalculo$Data[i]
  
  # Procurar por correspondências entre as datas nos datasets
  #correspondencia <- subset(dataset_vencimento_2033, LIQUIDACAO == data_recalculo)
  
  ano_vencimento <- as.integer(format(as.Date(df_input$Data_Vencimento, "%d/%m/%Y"), "%Y"))
  
  # Encontrar o dataset_vencimento correspondente ao ano encontrado
  dataset_vencimento_ano <- get(paste("dataset_vencimento", ano_vencimento, sep = "_"))
  
  # Procurar por correspondências entre as datas no dataset_vencimento_ano
  correspondencia <- subset(dataset_vencimento_ano, LIQUIDACAO == data_recalculo)
  
  # Se houver correspondência, atualizar o valor em recalculo$Qtd_Total_Emitido
  if (nrow(correspondencia) > 0) {
    recalculo$Qtd_Total_Emitido[i] <- correspondencia$QUANTIDADE_ACEITA_TOTAL
    recalculo$Valor_Financeiro_Total_Emitido[i] <- correspondencia$FINANCEIRO_ACEITO_TOTAL
    
  }
}

# Preenchimento do resto das colunas 

recalculo$Valor_Contabil_de_Emissao <- ifelse(recalculo$Valor_Financeiro_Total_Emitido >= 0, 
                                              recalculo$Valor_Financeiro_Total_Emitido, 
                                              recalculo$Qtd_Total_Emitido * recalculo$PMU)



for (i in 1:nrow(recalculo)) {
  if (i == 1) {
    recalculo$Qtd_Total_Acumulado[i] <- recalculo$Qtd_Total_Emitido[i]
  } else {
    recalculo$Qtd_Total_Acumulado[i] <- recalculo$Qtd_Total_Acumulado[i - 1] + recalculo$Qtd_Total_Emitido[i]
  }
}

recalculo$Saldo_Devedor_Inicio_Dia <- cumsum(recalculo$Valor_Contabil_de_Emissao)

recalculo$Juros_Reais_Acumulados_Inicio_do_Dia <- ifelse(seq_along(recalculo$indice) == 1, 
                                                         0,
                                                         lag(recalculo$Juros_Reais_Acumulados_Fim_do_Dia)
                                                         )

recalculo$Atualizacao_Monetaria_Acumulada_Inicio_do_Dia <- ifelse(seq_along(recalculo$indice) == 1, 
                                                         0,
                                                         lag(recalculo$Atualizacao_Monetaria_Acumulada_Fim_do_Dia)
)

recalculo$Principal_Acumulado_Inicio_Dia <- ifelse(seq_along(recalculo$indice) == 1,
                                                   recalculo$Valor_Financeiro_Total_Emitido,
                                                  lag(recalculo$Principal_Acumulado_Fim_do_Dia) + recalculo$Valor_Financeiro_Total_Emitido
                                                )

recalculo$Pagamento_Cupom <- ifelse(
  format(as.Date(recalculo$Data, "%d/%m/%Y"), "%d") == "01" &
    (format(as.Date(recalculo$Data, "%d/%m/%Y"), "%m") == "01" |
       format(as.Date(recalculo$Data, "%d/%m/%Y"), "%m") == "07"),
  df_input$Valor_Cupom * recalculo$Qtd_Total_Acumulado,
  0
)

recalculo$Pagamento_Vencimento <- ifelse(
  as.Date(recalculo$Data, "%d/%m/%Y") == df_input$Data_Vencimento,
  1000 * recalculo$Qtd_Total_Acumulado,
  0
)

recalculo$Pagamento_Total <- recalculo$Pagamento_Cupom + recalculo$Pagamento_Vencimento


recalculo$Pagamento_Juros_Reais <- ifelse(
  recalculo$Pagamento_Total <= 0 | recalculo$Juros_Reais_Acumulados_Inicio_do_Dia <= 0,
  0,
  ifelse(
    recalculo$Pagamento_Total > recalculo$Juros_Reais_Acumulados_Inicio_do_Dia,
    recalculo$Juros_Reais_Acumulados_Inicio_do_Dia,
    recalculo$Pagamento_Total
  )
)

recalculo$Pagamento_Atualizacao_Monetaria <- ifelse(
  recalculo$Pagamento_Total - recalculo$Pagamento_Juros_Reais <= 0 |
    recalculo$Atualizacao_Monetaria_Acumulada_Inicio_do_Dia <= 0,
  0,
  ifelse(
    recalculo$Pagamento_Total - recalculo$Pagamento_Juros_Reais > recalculo$Atualizacao_Monetaria_Acumulada_Inicio_do_Dia,
    recalculo$Atualizacao_Monetaria_Acumulada_Inicio_do_Dia,
    recalculo$Pagamento_Total - recalculo$Pagamento_Juros_Reais
  )
)


recalculo$Pagamento_Principal <- ifelse(recalculo$Pagamento_Total - recalculo$Pagamento_Juros_Reais - recalculo$Pagamento_Atualizacao_Monetaria <= 0,
                                        0,
                                        recalculo$Pagamento_Total - recalculo$Pagamento_Juros_Reais - recalculo$Pagamento_Atualizacao_Monetaria)


recalculo$Saldo_Devedor_Apos_Pagamentos <- recalculo$Saldo_Devedor_Inicio_Dia - recalculo$Pagamento_Total


recalculo$PMU <- ifelse(is.na(recalculo$Saldo_Devedor_Apos_Pagamentos / recalculo$Qtd_Total_Acumulado),
                        0,
                        recalculo$Saldo_Devedor_Apos_Pagamentos / recalculo$Qtd_Total_Acumulado)

recalculo$Cupom_Unitario <- ifelse(
  format(as.Date(recalculo$Data, "%d/%m/%Y"), "%d") == "01" & 
    (format(as.Date(recalculo$Data, "%d/%m/%Y"), "%m") == "01" | 
       format(as.Date(recalculo$Data, "%d/%m/%Y"), "%m") == "07"),
  -df_input$Valor_Cupom,
  0
)


recalculo$Pagamento_Vencimento_Unitario <- ifelse(
  as.Date(recalculo$Data, "%d/%m/%Y") == df_input$Data_Vencimento,
  -1000,
  0
)

recalculo$Fluxo_de_Caixa_Iterativo <- recalculo$PMU_Iterativo + recalculo$Cupom_Unitario + recalculo$Pagamento_Vencimento_Unitario


recalculo$TIR_Atual_Iterativo <- ifelse(
  which(recalculo$indice == recalculo$indice) == 1,
  0,
  recalculo$TIR_Atual_Iterativo[which(recalculo$indice == recalculo$indice) - 1]
)


recalculo$Juros_Efetivos_Nominais_Diario <- ifelse(
  recalculo$TIR_Atual_Iterativo > 0,
  (1 + recalculo$TIR_Atual_Iterativo)^(1 / 365) - 1,
  0
)


recalculo$Indice_de_Atualizacao_Monetaria_Diario <- ifelse(recalculo$Data %in% IPCA_IGPM$Data,
                                                           IPCA_IGPM$Indice_Diario_2[match(recalculo$Data, IPCA_IGPM$Data)],
                                                           0)


recalculo$Juros_Efetivos_Reais_Diario <- (1 + recalculo$Juros_Efetivos_Nominais_Diario) / 
  (1 + recalculo$Indice_de_Atualizacao_Monetaria_Diario) - 1


recalculo$Juros_Efetivos_Nominais_Diario_R <- recalculo$Juros_Efetivos_Nominais_Diario * recalculo$Saldo_Devedor_Apos_Pagamentos


recalculo$Atualizacao_Monetaria_Diaria_R <- recalculo$Indice_de_Atualizacao_Monetaria_Diario * recalculo$Saldo_Devedor_Apos_Pagamentos


recalculo$Juros_Efetivos_Reais_Diario_R <- recalculo$Juros_Efetivos_Reais_Diario * recalculo$Saldo_Devedor_Apos_Pagamentos


recalculo$Saldo_Devedor_Fim_Dia <- recalculo$Juros_Efetivos_Nominais_Diario_R + recalculo$Saldo_Devedor_Apos_Pagamentos


recalculo$Principal_Acumulado_Fim_do_Dia <- recalculo$Principal_Acumulado_Inicio_Dia - recalculo$Pagamento_Principal


recalculo$Atualizacao_Monetaria_Acumulada_Fim_do_Dia <- recalculo$Atualizacao_Monetaria_Acumulada_Inicio_do_Dia - recalculo$Pagamento_Atualizacao_Monetaria + recalculo$Atualizacao_Monetaria_Diaria_R


recalculo$Juros_Reais_Acumulados_Fim_do_Dia <- recalculo$Juros_Reais_Acumulados_Inicio_do_Dia - recalculo$Pagamento_Juros_Reais +recalculo$Juros_Efetivos_Reais_Diario_R



recalculo$Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais[1] <- recalculo$Atualizacao_Monetaria_Diaria_R[1]

for (i in 2:nrow(recalculo)) {
  recalculo$Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais[i] <- recalculo$Atualizacao_Monetaria_Acumulada_para_Variacoes_Patrimoniais[i - 1] + recalculo$Atualizacao_Monetaria_Diaria_R[i]
}


recalculo$Juros_Acumulados_para_Variacoes_Patrimoniais[1] <- recalculo$Juros_Efetivos_Reais_Diario_R[1]

for (i in 2:nrow(recalculo)) {
  recalculo$Juros_Acumulados_para_Variacoes_Patrimoniais[i] <- recalculo$Juros_Acumulados_para_Variacoes_Patrimoniais[i - 1] + recalculo$Juros_Efetivos_Reais_Diario_R[i]
}
