data_estoque_inicial <- gsub("/", "_", df_input$Data_Estoque_Inicial)
data_estoque_final <- gsub("/", "_", df_input$Data_Estoque_Final)

nomes_colunas <- c(
  paste0("Titulos_Acumulados_", data_estoque_inicial),
  paste0("Saldo_Devedor_Acumulado_", data_estoque_inicial),
  paste0("Titulos_Acumulados_", data_estoque_final),
  paste0("Saldo_Devedor_Acumulado_", data_estoque_final)
)

saldo_fim_exercicio <- data.frame(matrix(NA, ncol = length(nomes_colunas), nrow = 1))
colnames(saldo_fim_exercicio) <- nomes_colunas

data_estoque_inicial_var <- df_input$Data_Estoque_Inicial
data_estoque_final_var <- df_input$Data_Estoque_Final


indice_coluna_titulo_1 <- which(colnames(saldo_fim_exercicio) == paste0("Titulos_Acumulados_", data_estoque_inicial))
data_correspondente <- recalculo$Data == data_estoque_inicial_var
valor_correto <- recalculo$Qtd_Total_Acumulado[data_correspondente]
saldo_fim_exercicio[1, indice_coluna_titulo_1] <- valor_correto


indice_coluna_saldo_1 <- which(colnames(saldo_fim_exercicio) == paste0("Saldo_Devedor_Acumulado_", data_estoque_inicial))
data_correspondente_saldo <- recalculo$Data == data_estoque_inicial_var
valor_saldo_correto <- recalculo$Saldo_Devedor_Inicio_Dia[data_correspondente_saldo]
saldo_fim_exercicio[1, indice_coluna_saldo_1] <- valor_saldo_correto


indice_coluna_titulo_2 <- which(colnames(saldo_fim_exercicio) == paste0("Titulos_Acumulados_", data_estoque_final))
data_correspondente_final <- recalculo$Data == data_estoque_final_var
valor_correto_final <- recalculo$Qtd_Total_Acumulado[data_correspondente_final]
saldo_fim_exercicio[1, indice_coluna_titulo_2] <- valor_correto_final


indice_coluna_saldo_2 <- which(colnames(saldo_fim_exercicio) == paste0("Saldo_Devedor_Acumulado_", data_estoque_final))
data_correspondente_saldo_final <- recalculo$Data == data_estoque_final_var
valor_saldo_correto_final <- recalculo$Saldo_Devedor_Inicio_Dia[data_correspondente_saldo_final]
saldo_fim_exercicio[1, indice_coluna_saldo_2] <- valor_saldo_correto_final

