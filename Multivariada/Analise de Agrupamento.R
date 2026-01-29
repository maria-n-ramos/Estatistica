"""
Os dados dos registos nacionais dos homens são apresentados
na Tabela 8.6. Os recordes nacionais de corrida masculina em 54 países podem ser
examinados considerando as relações entre os tipos de corrida (banco de dados T8-
                                                                 6.dat em anexo ). As variáveis a serem analisadas são: x1 = 100m (s); x2 = 200m
(s); x3 = 400m (s); x4 = 800m (min); x5 = 1500m (min); x6 = 5000m (min),
x7 = 10000m (min) e x8 =Maratona (min).
(a) Verique possíveis outliers multivariados.
(b) Use dados padronizados, se necessário.
(c) Calcule as distâncias Euclidianas entre os pares dos países.
(d) Tratando as distâncias em (a) como medidas de (dis)similaridade, agrupar os

países utilizando os seguintes métodos hierárquicos: ligação simples (vizi-
                                                                        nho mais próximo), ligação completa (vizinho mais distante), ligação

média, centróide e Ward. Construa os dendrogramas e compare os resulta-
  dos.

(e) Faça a análise de agrupamento pelo método não-hierárquico K-means. Agrupe
os países em grupos usando vários valores de K. Compare os resultados com
os da Parte (d). Coloque no trabalho: quais os países
"""


"""
#A
promo <- read.table("T8-6.DAT")
promo <- promo[-1]
#p.cov <- var(scale(promo)) # standardize first
p.cov <- var(promo)
p.mean <- apply(promo,2,mean)
p.mah <- mahalanobis(promo, p.mean, p.cov)

boxplot(p.mah)

#b) n foi necessário

#c
d.eucl <- dist(promo, method = "euclidean")

#Vizualizando a distância euclidiana 
round(as.matrix(d.eucl)[1:8, 1:54], 1)

#d)



res.hc <- hclust(d = d.eucl, method = "single")


res.hc <- hclust(d = d.eucl, method = "complete")


hc.m <- hclust(d.eucl, method = "average")
cor(d.eucl, cophenetic(hc.m))
library("factoextra")
# Obtendo o dendograma
fviz_dend(hc.m, cex = 0.5)


res.hc <- hclust(d = d.eucl, method = "centroid")


res.hc <- hclust(d = d.eucl, method = "ward.D2")
res.coph <- cophenetic(res.hc)
cor(d.eucl, res.coph)
fviz_dend(res.hc, cex = 0.5)


#e)
#autovalores <- eigen(promo)[2]
#screeplot(autovalores)
"""


                          #In�cio

library(cluster)
library(dendextend)
library(reshape2)
library(ggplot2)
promo <- read.table("T8-6.DAT")
promo_O <- promo
promo <- promo[-1]
p.cov <- var(promo)
p.mean <- apply(promo,2,mean)
p.mah <- mahalanobis(promo, p.mean, p.cov)
#heatmap
#heatmap(p.mah.espelhada)

#heatmap(df_long)
boxplot(p.mah)

#Padronizar variaveis
Dados.pad <- scale(promo)

#CALCULANDO MATRIZ DE DISTANCIAS
d <- dist(Dados.pad, method = "euclidean")

dist_matrix <- as.matrix(d)
dist_df <- as.data.frame(as.table(dist_matrix))
#Triangular
dist_df <- dist_df[upper.tri(dist_matrix), ]


scatter_plot <- ggplot(dist_df, aes(Var1, Var2, size = Freq, color = Freq)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Gráfico de Dispersão das Distâncias Euclidianas",
       x = "Observação 1",
       y = "Observação 2",
       size = "Distância",
       color = "Distância") +
  theme_minimal()
print(scatter_plot)

#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single", "complete" e "ward.D"
hc1 <- hclust(d, method = "single" )
hc2 <- hclust(d, method = "complete" )
hc3 <- hclust(d, method = "average" )
hc4 <- hclust(d, method = "centroid" )
hc5 <- hclust(d, method = "ward.D" )

#Laço para arquivar os valores de Cor pra cada método do Cluster
colunas <- c("Simples","Completa","Media","Centroide","Ward")

correlacoes <- c(cor(d, cophenetic(hc1)),cor(d, cophenetic(hc2)),cor(d, cophenetic(hc3)),cor(d, cophenetic(hc4)),cor(d, cophenetic(hc5)))

x<-matrix(c(colunas, 
            round(correlacoes,3)), nrow=2,byrow = T);x

#DESENHANDO O DENDOGRAMA
plot(hc1, cex = 0.6, hang = -1)
plot(hc2, cex = 0.6, hang = -1)
plot(hc3, cex = 0.6, hang = -1)
plot(hc4, cex = 0.6, hang = -1)
plot(hc5, cex = 0.6, hang = -1)

#COMPARANDO DENDOGRAMAS


#kmeans
library(cluster)
set.seed(42)

library(dplyr)

wss <- numeric(10)
for (i in 1:10) {
  # Fit the model: km.out
  km.out <- kmeans(d, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

wss_df <- tibble(clusters = 1:10, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot


km.out <- kmeans(d, centers = 3, nstart = 20)

clusplot(Dados.pad,km.out$cluster, main='Representa��o bidimensional do agrupamento',color=TRUE, shade=TRUE,labels=2, lines=0)

km.out2 <- kmeans(d, centers = 2, nstart = 20)

clusplot(Dados.pad,km.out2$cluster, main='Representação bidimensional do agrupamento',color=TRUE, shade=TRUE,labels=2, lines=0)

promo_O[km.out$cluster == 1, ]$V1
promo_O[km.out$cluster == 2, ]$V1
promo_O[km.out$cluster == 3, ]$V1

var <- c("V2","V3","V4","V5","V6","V7","V8","V9")
cluster1_medias <- colMeans(promo[km.out$cluster == 1, ])
cluster2_medias <- colMeans(promo[km.out$cluster == 2, ])
cluster3_medias <- colMeans(promo[km.out$cluster == 3, ])


medias_df <- data.frame(
  Cluster1 = cluster1_medias,
  Cluster2 = cluster2_medias,
  Cluster3 = cluster3_medias
);t(medias_df)
