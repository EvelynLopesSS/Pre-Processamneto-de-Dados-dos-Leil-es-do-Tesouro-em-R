

df_input <- data.frame(
  Valor_Cupom = numeric(1),
  Data_Vencimento = as.Date("2029-01-01"),
  Data_Primeiro_Cupom = as.Date("2023-01-01"),
  Data_Segundo_Cupom = as.Date("2023-07-01"),
  Data_Estoque_Inicial = as.Date("2022-12-31"),
  Data_Estoque_Final = as.Date("2023-09-30")
)

df_input$Valor_Cupom <- 48.81

#df_input$Numero_de_Linhas <- nrow(recalculo) # colocar essa line em recalculo

df_input$Data_Vencimento <- format(df_input$Data_Vencimento, "%d/%m/%Y")
df_input$Data_Primeiro_Cupom <- format(df_input$Data_Primeiro_Cupom, "%d/%m/%Y")
df_input$Data_Segundo_Cupom <- format(df_input$Data_Segundo_Cupom, "%d/%m/%Y")
df_input$Data_Estoque_Inicial <- format(df_input$Data_Estoque_Inicial, "%d/%m/%Y")
df_input$Data_Estoque_Final <- format(df_input$Data_Estoque_Final, "%d/%m/%Y")
