#install.packages("sidrar")
library(sidrar)
library(dplyr)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyr)
library(readxl)

cod_sidra <- "/t/1737/n1/all/v/all/p/last%2048/d/v63%202,v69%202,v2263%202,v2264%202,v2265%202,v2266%2013"
dados_ipca <- sidrar::get_sidra(api = cod_sidra)

colunas <- names(dados_ipca)

novos_nomes <- c("Nivel_Territorial_COD", "Nivel_Territorial", "Unidade_Medida_COD", "Unidade_Medida",
                 "Valor", "Brasil_COD", "Brasil", "Variavel_COD", "Variavel", "Mes_COD", "Mes")

names(dados_ipca) <- novos_nomes

print(names(dados_ipca))

valores_unicos_variavel_cod <- unique(dados_ipca$Variavel_COD)
print(valores_unicos_variavel_cod)

valores_unicos_variavel <- unique(dados_ipca$Variavel)
print(valores_unicos_variavel)

equivalencia_variaveis <- data.frame(Variavel_COD = valores_unicos_variavel_cod, Variavel = valores_unicos_variavel)
print(equivalencia_variaveis)


for (valor_cod in valores_unicos_variavel_cod) {
  df_nome <- paste("IPCA_", valor_cod, sep = "") 
  assign(df_nome, filter(dados_ipca, Variavel_COD == valor_cod) %>% 
           select( Variavel,Valor, Mes, Mes_COD) %>%
           mutate(Ano = sub(".*\\s", "", Mes),
                  Mês = sub("\\s.*", "", Mes)) %>%
           select(-Mes) %>%
           mutate(Dias_no_Mes = days_in_month(dmy(paste("01", Mês, Ano, sep = " "))))%>%
           rename(Mes = Mês))
  
  cat("Dataframe", df_nome, "criado com", nrow(get(df_nome)), "linhas.\n")
}


#----------------------IPCA_2266--------------------------


#IPCA_2266 <- IPCA_2266 %>% arrange(Ano, Mes)

meses_numero <- c("janeiro" = 01, "fevereiro" = 02, "março" = 03, "abril" = 04, "maio" = 05, "junho" = 06,
                  "julho" = 07, "agosto" = 08, "setembro" = 09, "outubro" = 10, "novembro" = 11, "dezembro" = 12)

IPCA_2266 <- IPCA_2266 %>%
  mutate(Mes = tolower(Mes)) %>%
  mutate(Mes_Numero = match(Mes, names(meses_numero)))


IPCA_2266 <- IPCA_2266 %>%
  mutate(Acumulado_Mensal = ifelse(is.na(lag(Valor)), 0, Valor / lag(Valor)))

IPCA_2266$Indice_Diario <- ifelse(is.na(lag(IPCA_2266$Valor)), 0,
                                  (IPCA_2266$Valor / lag(IPCA_2266$Valor))^ (1 / IPCA_2266$Dias_no_Mes))

#----------------------IPCA--------------------------

IPCA <- IPCA_2266 %>%
  select(Valor, Ano, Mes_Numero, Dias_no_Mes, Acumulado_Mensal, Indice_Diario) %>%
  mutate(Data = map2(Ano, Mes_Numero, ~seq(as.Date(paste0(.x, "-", sprintf("%02d", .y), "-01")), 
                                           by = "days", 
                                           length.out = .x %>% make_date() %>% days_in_month()))) %>%
  unnest(cols = Data) %>%
  mutate_at(vars(Valor, Acumulado_Mensal, Indice_Diario), ~rep(., length.out = n())) %>%
  select(Mes_Numero, Data, Valor, Acumulado_Mensal, Dias_no_Mes, Indice_Diario) %>%
  rename(Mes = Mes_Numero) %>%
  filter(Data >= as.Date("2019-12-31")) %>%
  mutate(Data = format(Data, "%d/%m/%Y"))


IPCA$Indice_Diario_Acumulado <- 0  
IPCA$Indice_Diario_Acumulado[1] <- IPCA$Valor[1]  
for (i in 2:nrow(IPCA)) {
  IPCA$Indice_Diario_Acumulado[i] <- IPCA$Indice_Diario[i] * IPCA$Indice_Diario_Acumulado[i - 1]
}

IPCA <- IPCA %>%
  mutate(Indice_Diario_2 = Indice_Diario - 1)

IPCA <- IPCA %>%
  mutate(Tipo = "IPCA")

#---------------------------IGPM---------------------------------------------------

IGPM <-
  read_excel(
    "C:/Users/Evelyn/Documents/TIR/IGPM.xlsx",
    col_types = c(
      "date",
      "date",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )

IGPM$Mes <- month(IGPM$Mes)
IGPM$Data <- format(as.Date(IGPM$Data), "%d/%m/%Y")

IGPM <- IGPM %>%
  mutate(Indice_Diario_2 = Indice_Diario - 1)

IGPM <- IGPM %>%
  mutate(Tipo = "IGPM")
# ---------------------------Concat------------------------------------------------


IPCA_filtrado <- IPCA %>%
  filter(as.Date(Data, format = "%d/%m/%Y") >= as.Date("01/01/2020", format = "%d/%m/%Y"))

IPCA_IGPM <- bind_rows(IGPM, IPCA_filtrado) %>%
  arrange(as.Date(Data, format = "%d/%m/%Y"))