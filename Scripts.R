# Setando WD
setwd("/home/n/Documentos/branch_dev4/aprendendo_git")

# Instalando pacote necessários
install.packages("RcolorBrewer",dependencies = T)
install.packages("plotly",dependencies = T)
install.packages("httr",dependencies = T)
install.packages("magrittr",dependencies = T)

install.packages("ggplot",dependencies = T)
install.packages("randomForest",dependencies = T)
install.packages("e1071",dependencies = T)
install.packages("rpart",dependencies = T)
install.packages("rpart.plot",dependencies = T)


# Leitura de arquivo
library(readr)
# Preenchimento com paleta de cores do histograma
library(RColorBrewer)
# Plotagem especial de gráficos
library(plotly)
# Preenchimento com densidade
library(magrittr)

# Leitura do arquivo
Anatel_Consolidado <- read_delim("Anatel_Consolidado.csv", 
                                   ";", escape_double = FALSE, col_types = cols(Ano = col_integer(), 
                                   Mes = col_integer(), QtdeSolic = col_integer()), 
                                   locale = locale(encoding = "latin1"), 
                                   na = "NA", trim_ws = TRUE)


str(Anatel_Consolidado)
summary(Anatel_Consolidado)

# Histogramas de ocorrências / mês e por anos
par(mfrow=c(1,2))
hist(Anatel_Consolidado$Mes, breaks = 12, xlab="Meses", 
     ylab = "Qtd de Registros", main = "Ocorrências / Mês",
     col=brewer.pal(12,"Paired"), ylim=c(0,1000000))

hist(Anatel_Consolidado$Ano, breaks = 12, xlab="Anos", 
     ylab = "Qtd de Registros", main = "Ocorrências / Anos",
     col=brewer.pal(12,"Paired"), ylim=c(0,1000000))

# Tentativa de análise de variações por densidade

plot_ly(x=Anatel_Consolidado$Ano,type = "histogram")

fit <- density(Anatel_Consolidado$Ano[!(is.na(Anatel_Consolidado$Ano))])
  
fit
             
plot_ly(x = Anatel_Consolidado$Ano, type = "histogram") %>% add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Densidade") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))


# Testa sobreposição de layers com variáveis categóricas

df_reclamacao <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Reclamação",]
df_informacao <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Pedido de Informação",]

plot_ly(alpha = 0.6) %>% add_histogram(x = df_reclamacao$Tipo, name="Reclamação") %>% 
          add_histogram(x = df_informacao$Tipo, name="Pedido de Informação") %>% 
          #add_histogram(x = Anatel_Consolidado$Tipo, name="Denúncia") %>% 
          #add_histogram(x = Anatel_Consolidado$Tipo, name="Elogio") %>% 
          layout(barmode = "overlay")

# Testa sobreposição de layers com variáveis numéricas

df_reclamacao <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Reclamação",]
df_informacao <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Pedido de Informação",]
df_denuncia <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Denúncia",]
df_elogio <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Elogio",]

plot_ly(alpha = 0.6) %>% add_histogram(x = df_reclamacao$Ano, name="Reclamação") %>% 
  add_histogram(x = df_informacao$Ano, name="Pedido de Informação") %>% 
  add_histogram(x = df_denuncia$Ano, name="Denúncia") %>% 
  add_histogram(x = df_elogio$Ano, name="Elogio") %>% 
  layout(barmode = "overlay")
                                                                              
# Análise Operadoras com maior frequência de reclamação, não maior quantidade de reclamações

df_rec_tim <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Reclamação" & Anatel_Consolidado$GrupoEconNorm=="TIM",] 
df_rec_oi <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Reclamação" & Anatel_Consolidado$GrupoEconNorm=="OI",] 
df_rec_vivo <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Reclamação" & Anatel_Consolidado$GrupoEconNorm=="VIVO",] 
df_rec_claro <- Anatel_Consolidado[Anatel_Consolidado$Tipo=="Reclamação" & Anatel_Consolidado$GrupoEconNorm=="CLARO",] 

plot_ly(alpha = 0.6) %>% 
  add_histogram(x = df_rec_oi$Ano, y = df_rec_oi$QtdeSolic, name="OI") %>% 
  add_histogram(x = df_rec_vivo$Ano, y = df_rec_vivo$QtdeSolic, name="VIVO") %>% 
  add_histogram(x = df_rec_tim$Ano, y = df_rec_tim$QtdeSolic, name="TIM") %>% 
  add_histogram(x = df_rec_claro$Ano, y = df_rec_claro$QtdeSolic, name="CLARO") %>% 
  layout(barmode = "overlay")
                                                                              

# Testes com barras

plot_ly(x = Anatel_Consolidado$GrupoEconNorm, y = Anatel_Consolidado$QtdeSolic, type = "bar") %>% layout(title="Análise Quantitativa de reclamações")
                                                                              
#Histograma para apenas uma variável
#Tipos:'scatter', 'box', 'bar', 'heatmap', 'histogram', 'histogram2d', 'histogram2dcontour', 'pie', 'contour', 'scatterternary', 'sankey', 'scatter3d', 'surface', 'mesh3d', 'scattergeo', 'choropleth', 'scattergl', 'pointcloud', 'heatmapgl', 'parcoords', 'scattermapbox', 'carpet', 'scattercarpet', 'contourcarpet', 'ohlc', 'candlestick', 'area'
                                                                              
plot_ly(x=dados$Idade,type="histogram")
                                                                              
                                                                              
#adiconando linha de densidade da distribuição
                                                                              
fit<-density(dados$Idade)
                                                                              
                                                                              
plot_ly(x = Idade, type = "histogram") %>% 
  add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Densidade") %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right"))
                                                                                                                                                            
                                                                              
                                                                              library(readr)
                                                                              Anatel_Consolidado <- read_delim("Anatel_Consolidado.csv", 
                                                                                                               ";", escape_double = FALSE, enccol_types = cols(Ano = col_integer(), 
                                                                                                                                                               DataExtracao = col_character(), Mes = col_integer(), 
                                                                                                                                                               
                                                                                                                                                               QtdeSolic = col_integer()), trim_ws = TRUE)
View(Anatel_Consolidado)

head(Anatel_Consolidado)
unique(Anatel_Consolidado$GrupoEconNorm)
summary(Anatel_Consolidado$GrupoEconNorm)
table(Anatel_Consolidado$GrupoEconNorm)
ocorrencias_ope <- table(Anatel_Consolidado$GrupoEconNorm)
plot(ocorrencias_ope)
table(Anatel_Consolidado$Ano)
ocorrencias_ano <- table(Anatel_Consolidado$Ano)
plot(ocorrencias_ano)
table(Anatel_Consolidado$CanalEntrada)
ocorrencias_canal <- table(Anatel_Consolidado$CanalEntrada)
plot(ocorrencias_canal)
table(Anatel_Consolidado$Condicao)
ocorrencias_cond <- (table(Anatel_Consolidado$Condicao))
plot(ocorrencias_cond)
table(Anatel_Consolidado$Tipo)
ocorrencias_tipo <- (table(Anatel_Consolidado$Tipo))
plot(ocorrencias_tipo)
table(Anatel_Consolidado$Servico)
ocorrencias_serv <- table(Anatel_Consolidado$Servico)
plot(ocorrencias_serv)
table(Anatel_Consolidado$Modalidade)
ocorrencias_mod <- (table(Anatel_Consolidado$Modalidade))
plot(ocorrencias_mod)
table(Anatel_Consolidado$Motivo)
ocorrencias_mot <- table(Anatel_Consolidado$Motivo)
plot(ocorrencias_mot)
table(Anatel_Consolidado$UF)
ocorrencias_uf <- table(Anatel_Consolidado$UF)
plot(ocorrencias_uf)

