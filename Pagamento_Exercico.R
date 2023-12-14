ano_data_estoque_final <- as.Date(df_input$Data_Estoque_Final, "%d/%m/%Y")
ano <- unique(format(ano_data_estoque_final, "%Y"))


tabela_pagamentos <- data.frame(
  Pagamento_em = c("Pagamento_Cupom", "Pagamento_Juros_Reais", "Pagamento_Atualização_Mon", 
                   "Pagamento_Principal", "Total_Amortização", "Quantidade"),
  Primeiro_Semestre = 0, 
  Segundo_Semestre = 0, 
  Total = 0 
)

colnames(tabela_pagamentos)[1] <- paste("Pagamento_em", ano, sep = "_")


data_correspondente_primeiro_semestre <- recalculo$Data == df_input$Data_Primeiro_Cupom
data_correspondente_segundo_semestre <- recalculo$Data == df_input$Data_Segundo_Cupom


# Pagamento_Cupom 
valor_pagamento_cupom_primeiro_semestre <- recalculo$Pagamento_Total[data_correspondente_primeiro_semestre]
tabela_pagamentos[1, 2] <- valor_pagamento_cupom_primeiro_semestre

valor_pagamento_cupom_segundo_semestre <- recalculo$Pagamento_Total[data_correspondente_segundo_semestre]
tabela_pagamentos[1, 3] <- valor_pagamento_cupom_segundo_semestre

tabela_pagamentos[1, 4] <- tabela_pagamentos[1, 2] + tabela_pagamentos[1, 3]

#	Pagamento_Juros_Reais

valor_Pagamento_Juros_Reais_primeiro_semestre <- recalculo$Pagamento_Juros_Reais[data_correspondente_primeiro_semestre]
tabela_pagamentos[2, 2] <- valor_Pagamento_Juros_Reais_primeiro_semestre

valor_Pagamento_Juros_Reais_segundo_semestre <- recalculo$Pagamento_Juros_Reais[data_correspondente_segundo_semestre]
tabela_pagamentos[2, 3] <- valor_Pagamento_Juros_Reais_segundo_semestre

tabela_pagamentos[2, 4] <- tabela_pagamentos[2, 2] + tabela_pagamentos[2, 3]

#	Pagamento_Atualização_Mon

valor_Pagamento_Atualização_Mon_primeiro_semestre <- recalculo$Pagamento_Atualizacao_Monetaria[data_correspondente_primeiro_semestre]
tabela_pagamentos[3, 2] <- valor_Pagamento_Atualização_Mon_primeiro_semestre

valor_Pagamento_Atualização_Mon_segundo_semestre <- recalculo$Pagamento_Atualizacao_Monetaria[data_correspondente_segundo_semestre]
tabela_pagamentos[3, 3] <- valor_Pagamento_Atualização_Mon_segundo_semestre
tabela_pagamentos[3, 4] <- tabela_pagamentos[3, 2] + tabela_pagamentos[3, 3]

# Pagamento_Principal

valor_Pagamento_Principal_primeiro_semestre <- recalculo$Pagamento_Principal[data_correspondente_primeiro_semestre]
tabela_pagamentos[4, 2] <- valor_Pagamento_Principal_primeiro_semestre

valor_Pagamento_Principal_segundo_semestre <- recalculo$Pagamento_Principal[data_correspondente_segundo_semestre]
tabela_pagamentos[4, 3] <- valor_Pagamento_Principal_segundo_semestre

tabela_pagamentos[4, 4] <- tabela_pagamentos[4, 2] + tabela_pagamentos[4, 3]

# Total_Amortização

tabela_pagamentos[5, 2] <- tabela_pagamentos[3,2] + tabela_pagamentos[4, 2]

tabela_pagamentos[5, 3] <- tabela_pagamentos[3,3] + tabela_pagamentos[4, 3]

tabela_pagamentos[5, 4] <- tabela_pagamentos[5,2] + tabela_pagamentos[5, 3]

# Quantidade 

valor_Quantidade_primeiro_semestre <- recalculo$Qtd_Total_Acumulado[data_correspondente_primeiro_semestre]
tabela_pagamentos[6, 2] <- valor_Quantidade_primeiro_semestre

valor_Quantidade_segundo_semestre <- recalculo$Qtd_Total_Acumulado[data_correspondente_segundo_semestre]
tabela_pagamentos[6, 3] <-valor_Quantidade_segundo_semestre

