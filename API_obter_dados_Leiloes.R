library(httr)
library(jsonlite)

# Função para obter dados dos leilões 
get_leiloes <- function(ano, titulo) {
  url <- "https://apiapex.tesouro.gov.br/aria/v1/api-leiloes-pub/custom/resultados"
  
  params <- list(
    ano = ano,
    titulo = titulo
  )
  
  response <- tryCatch(GET(url, query = params), error = function(e) e)
  
  if (inherits(response, "error")) {
    print("Erro ao acessar a API")
    return(NULL)
  } else {
    content <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    if (length(data$registros) > 0) {
      return(as.data.frame(data$registros))
    } else {
      print("Nenhum registro encontrado para os parâmetros especificados.")
      return(NULL)
    }
  }
}

# Anos desejados
#anos <- c( "2010", "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
anos <- c("2019","2020","2021","2022","2023")
# Título desejado
titulo <- "NTN-F"

# Dataframes para cada ano
for (ano in anos) {
  dataframe_nome <- paste("leiloes", ano, sep = "_") 
  dados <- get_leiloes(ano, titulo) 
  
  if (!is.null(dados)) {
    assign(dataframe_nome, dados)
    print(paste("Dataset para", ano, "criado com sucesso!", sep = " "))
    
    
    # Selecionar as colunas desejadas em cada dataframe
    colunas_selecionadas <- c("LIQUIDACAO", "QUANTIDADE ACEITA", "QUANTIDADE ACEITA SEGUNDA VOLTA", "QUANTIDADE BCB",
                              "FINANCEIRO ACEITO", "FINANCEIRO ACEITO SEGUNDA VOLTA", "FINANCEIRO BCB", "VENCIMENTO", "TIPO")
    novo_dataframe_nome <- paste("dataset", ano, sep = "_")
    assign(novo_dataframe_nome, dados[colunas_selecionadas])
    dataset <- dados[dados$TIPO %in% c("Venda", "Compra"), ]
    assign(novo_dataframe_nome, dataset)
    
    dataset <- get(paste("dataset", ano, sep = "_"))
    dataset[dataset == "-"] <- 0
    dataset[dataset == " "] <- 0
    dataset[is.na(dataset)] <- 0
    assign(novo_dataframe_nome, dataset)
    
    
    numeric_cols <- c("QUANTIDADE ACEITA", "QUANTIDADE ACEITA SEGUNDA VOLTA", "QUANTIDADE BCB",
                      "FINANCEIRO ACEITO", "FINANCEIRO ACEITO SEGUNDA VOLTA","FINANCEIRO BCB")
    
    for (col in numeric_cols) {
      dataset[[col]] <- gsub(",", ".", dataset[[col]])
    }
    assign(novo_dataframe_nome, dataset)
    
    dataset[, numeric_cols] <- lapply(dataset[, numeric_cols], as.numeric)
    assign(novo_dataframe_nome, dataset)

    dataset$QUANTIDADE_ACEITA_TOTAL <- dataset$`QUANTIDADE ACEITA` + dataset$`QUANTIDADE ACEITA SEGUNDA VOLTA` + dataset$`QUANTIDADE BCB`
    dataset$FINANCEIRO_ACEITO_TOTAL <- dataset$`FINANCEIRO ACEITO` + dataset$`FINANCEIRO ACEITO SEGUNDA VOLTA` + dataset$`FINANCEIRO BCB`
    assign(novo_dataframe_nome, dataset)
    
  }
}


#-------------------------Datas duplicadas com Venda e Compra-----------------------
commented_code <- function() {
  for (ano in anos) {
    dataframe_nome <- paste("dataset", ano, sep = "_")
    if(exists(dataframe_nome)) {
      dataset <- get(dataframe_nome)
      
      # Identificar datas duplicadas na coluna LIQUIDACAO
      duplicated_dates <- dataset[duplicated(dataset$LIQUIDACAO) | duplicated(dataset$LIQUIDACAO, fromLast = TRUE), "LIQUIDACAO"]
      
      for (date in duplicated_dates) {
        duplicate_rows <- dataset[dataset$LIQUIDACAO == date, ]
        
        vendas <- subset(duplicate_rows, TIPO == "Venda")
        compras <- subset(duplicate_rows, TIPO == "Compra")
       
        sum_vendas <- colSums(vendas[, numeric_cols])
        sum_compras <- colSums(compras[, numeric_cols])
        
        
        if (nrow(vendas) > 0 && nrow(compras) > 0) {
          #diferença entre os totais de vendas e compras
          diferenca <- sum_vendas - sum_compras
          
          # Atualizar as linhas com o resultado da diferença
          dataset[dataset$LIQUIDACAO == date, numeric_cols] <- diferenca
        } else {
          # Se todas as transações forem do mesmo tipo, soma os valores
          total <- ifelse(nrow(vendas) > 0, sum_vendas, sum_compras)
          dataset[dataset$LIQUIDACAO == date, numeric_cols] <- total
        }
      }
      
      # Remove as linhas duplicadas 
      dataset <- dataset[!duplicated(dataset$LIQUIDACAO), ]
      
      assign(dataframe_nome, dataset)
    }
  }
}


# commented_code()

#-----------------------------Verificar anos  de vencimento únicos em cada dataframe

anos_unicos <- list()

for (ano in anos) {
  dataframe_nome <- paste("dataset", ano, sep = "_")
  if(exists(dataframe_nome)) {
    dataset <- get(dataframe_nome)
    anos_unicos[[ano]] <- unique(as.integer(format(as.Date(dataset$VENCIMENTO, "%d/%m/%Y"), "%Y")))
  }
}

for (ano in anos) {
  if(length(anos_unicos[[ano]]) > 0) {
    cat("Anos únicos/ data de vencimento no dataset de", ano, "são:", unique(anos_unicos[[ano]]), "\n")
  } else {
    cat("Nenhum dataset encontrado para o ano", ano, "\n")
  }
}


# Criar um dataframe para cada ano de vencimento 
anos_vencimento_unicos <- unique(unlist(anos_unicos))

for (ano_vencimento in anos_vencimento_unicos) {
  dataset_vencimento_nome <- paste("dataset_vencimento", ano_vencimento, sep = "_")
  
  dataset_vencimento <- data.frame()
  
  for (ano in anos) {
    dataframe_nome <- paste("dataset", ano, sep = "_")
    if(exists(dataframe_nome)) {
      dataset <- get(dataframe_nome)
      vencimentos_ano <- as.integer(format(as.Date(dataset$VENCIMENTO, "%d/%m/%Y"), "%Y"))
      
      if (ano_vencimento %in% vencimentos_ano) {
        dados_filtrados <- subset(dataset, vencimentos_ano == ano_vencimento)
        
        if (nrow(dados_filtrados) > 0) {
          dataset_vencimento <- rbind(dataset_vencimento, dados_filtrados[, c("LIQUIDACAO", "QUANTIDADE_ACEITA_TOTAL", "FINANCEIRO_ACEITO_TOTAL", "TIPO")])
        }
      }
    }
  }
  
  if (nrow(dataset_vencimento) > 0) {
    assign(dataset_vencimento_nome, dataset_vencimento)
    cat("Dataset", dataset_vencimento_nome, "criado com sucesso!\n")
  } else {
    cat("Nenhum dado encontrado para o ano de vencimento", ano_vencimento, "\n")
  }
}

#--------------------------Datas duplicadas nos Dataset_Vencimento

for (ano_vencimento in anos_vencimento_unicos) {
  dataset_vencimento_nome <- paste("dataset_vencimento", ano_vencimento, sep = "_")
  dataset_vencimento <- get(dataset_vencimento_nome)
  
  # Identificar e tratar datas duplicadas
  unique_dates <- unique(dataset_vencimento$LIQUIDACAO)
  
  for (date in unique_dates) {
    same_dates <- dataset_vencimento$LIQUIDACAO == date
    
    if (sum(same_dates) > 1) {
      # Duplicadas encontradas
      tipo_venda <- subset(dataset_vencimento[same_dates, ], TIPO == "Venda")
      tipo_compra <- subset(dataset_vencimento[same_dates, ], TIPO == "Compra")
      
      # Soma das quantidades e valores financeiros para cada tipo
      sum_venda <- colSums(tipo_venda[, c("QUANTIDADE_ACEITA_TOTAL", "FINANCEIRO_ACEITO_TOTAL")])
      sum_compra <- colSums(tipo_compra[, c("QUANTIDADE_ACEITA_TOTAL", "FINANCEIRO_ACEITO_TOTAL")])
      
      # Verificação do tipo para atualização dos valores
      if (nrow(tipo_venda) > 0 && nrow(tipo_compra) > 0) {
        diferenca <- sum_venda - sum_compra
        dataset_vencimento$QUANTIDADE_ACEITA_TOTAL[same_dates] <- diferenca[1]
        dataset_vencimento$FINANCEIRO_ACEITO_TOTAL[same_dates] <- diferenca[2]
      } else {
        dataset_vencimento$QUANTIDADE_ACEITA_TOTAL[same_dates] <- sum_venda[1] - sum_compra[1]
        dataset_vencimento$FINANCEIRO_ACEITO_TOTAL[same_dates] <- sum_venda[2] - sum_compra[2]
      }
    }
  }
  
  dataset_vencimento <- dataset_vencimento[!duplicated(dataset_vencimento$LIQUIDACAO), ]
  
  assign(dataset_vencimento_nome, dataset_vencimento)
}


