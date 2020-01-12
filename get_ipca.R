
get_ipca = function(){
ipca = html_table(html_nodes(read_html("https://www.portalbrasil.net/ipca.htm"), "table")[6], fill = TRUE)[[1]]

ipca_names = as.character(ipca[1,])
ipca_names = ipca_names[1:13]

ipca = ipca[2:nrow(ipca),1:ncol(ipca)-1]
names(ipca) = ipca_names[1:ncol(ipca)]
names(ipca)[1] = 'ano'

ipca_names = ipca_names[2:13]

ipca = as.data.table(melt(ipca,id.vars ='ano',variable.name = 'mes',value.name = 'ipca'))

ipca[,ipca := stri_replace_all_fixed(ipca,'-','')]
ipca[,ipca := as.numeric(stri_replace_all_fixed(ipca,',','.'))]
ipca[,mes_num:=match(mes,ipca_names)]

ipca[,data:=as.Date(paste0(ano,'-',mes_num,'-01'),format = '%Y-%m-%d')]
ipca = ipca[!is.na(ipca),.(data,ipca)]

setorder(ipca,-data)

ipca[,idx := ipca/head(ipca,1)]
return(ipca)
}