library(httr)
library(lubridate)
library(jsonlite)
library(data.table)
library(stringi)
library(ggplot2)
library(scales)
library(ggalt)

##########################################
# IPCA
##########################################

############## manual ##############
# https://dadosabertos.bcb.gov.br/dataset/16121-indice-nacional-de-precos-ao-consumidor---amplo-ipca---nucleo-por-exclusao---ex2/resource/591a20e5-a50e-41db-9fe9-08df1788247c

# http://api.sidra.ibge.gov.br/home/ajuda

# url = "http://api.bcb.gov.br/dados/serie/bcdata.sgs.16121/dados?formato=json"
url = "http://api.sidra.ibge.gov.br/values/t/1419/p/all/v/63,69,2265/n1/1/h/n"

ipca = as.data.table(
  fromJSON(
    rawToChar(
      GET(url)$content
    )
  )
)

names(ipca) = c("data","nm_data","cd_variavel","variavel","cd_regiao","nm_regiao","cd_categoria","nm_categoria","cd_unidade","nm_unidade","valor_pct")

ipca = ipca[,.SD, .SDcols = names(ipca)[!grepl("cd_|nm_",names(ipca))]]

ipca[grepl("mensal",variavel), variavel := "mensal"]
ipca[grepl("ano",variavel), variavel := "mensal_acumulada"]
ipca[grepl("12",variavel), variavel := "12meses"]

ipca[, valor_pct := as.numeric(valor_pct)]
ipca[, data := as.Date(paste0(data,"01"), format = "%Y%m%d")]

ipca = dcast.data.table(ipca, "data ~ variavel", value.var = "valor_pct")
##########################################
# SELIC
##########################################

############## manual ##############
# https://dadosabertos.bcb.gov.br/dataset/11-taxa-de-juros---selic/resource/b73edc07-bbac-430c-a2cb-b1639e605fa8


url = "http://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados?formato=json"

selic = as.data.table(
  fromJSON(
    rawToChar(
      GET(url)$content
    )
  )
)

selic[, `:=`(data = as.Date(data, format = "%d/%m/%Y"), valor = as.numeric(valor))]

selic_ipca = merge(selic, ipca, by = "data")
selic_ipca[, dif := valor - `12meses`]

ggplot(selic_ipca, aes(x = data, y = valor/100)) +
  geom_xspline(spline_shape = -0.4, size=0.5, aes(colour = "SELIC")) +
  geom_xspline(spline_shape = -0.4, size=0.5, aes(x = data, y = `12meses`/100, colour = "IPCA")) +
  geom_xspline(spline_shape = -0.4, size=0.5, aes(x = data, y = dif/100, colour = "Diferença")) +
  xlab("") +
  ylab("Valor") +
  scale_color_brewer(palette = 'Set1', name = "Índice") +
  scale_y_continuous(labels = percent, breaks = seq(0, max(selic_ipca$valor), by = 0.025)) +
  scale_x_date(date_breaks = '3 month', date_labels = "%b %y") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey80"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1)
    )

