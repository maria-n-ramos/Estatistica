library(psych)
library(GPArotation)
library(corrplot)
library(ggplot2)
data <- read.csv("C:/Users/Maria Nilza/Downloads/MBA_CAR_ATTRIB.csv", header = TRUE, sep = ";")
data <- data[, 2:18]
data <- data[sapply(data, is.numeric)]
data <- na.omit(data)


eig <- eigen(cov(data))
eigvals <- eig$values


eig_df <- data.frame(Componente = 1:length(eigvals), Autovalor = eigvals)

ggplot(eig_df, aes(x = Componente, y = Autovalor)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ggtitle("Scree plot") +
  ylab("Autovalor") +
  xlab("Componente Principal") +
  theme_minimal()


# (a) Teste de Esfericidade de Bartlett
bartlett_test <- cortest.bartlett(cor(data), n = nrow(data))
print(bartlett_test)

# (b) Critério de Kaiser-Meyer-Olkin (KMO)
kmo_test <- KMO(data)
print(kmo_test)

# (c) Escolha de Dois Métodos de Extração das Cargas Fatoriais
pca <- principal(data, nfactors = 5, rotate = "none")
print(pca)

fa <- fa(data, nfactors = 5, rotate = "none")
print(fa)

# (d) Rotação: Sem Rotação, Rotação Ortogonal e Rotação Oblíqua
print(pca)
print(fa)

pca_varimax <- principal(data, nfactors = 5, rotate = "varimax")
fa_varimax <- fa(data, nfactors = 5, rotate = "varimax")
print(pca_varimax)
print(fa_varimax)

pca_oblimin <- principal(data, nfactors = 5, rotate = "oblimin")
fa_oblimin <- fa(data, nfactors = 5, rotate = "oblimin")
print(pca_oblimin)
print(fa_oblimin)

# (e) Calcular as Comunalidades e as Variâncias Específicas
communalities_pca <- pca$communality
specific_variance_pca <- 1 - communalities_pca
print(communalities_pca)
print(specific_variance_pca)

communalities_fa <- fa$communality
specific_variance_fa <- 1 - communalities_fa
print(communalities_fa)
print(specific_variance_fa)

# (f) Calcular a Matriz Residual
residuals_pca <- pca$residual
residuals_fa <- fa$residual
print(residuals_pca)
print(residuals_fa)

corrplot(residuals_pca, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)
corrplot(residuals_fa, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# (g) Calcular a Variância Explicada para Cada Fator
variance_explained_pca <- pca$values / sum(pca$values) * 100
variance_explained_fa <- fa$values / sum(fa$values) * 100
print(variance_explained_pca[1:3])
print(variance_explained_fa[1:3])
print(residuals_pca[1:3])
print(residuals_fa[1:3])

# (H)
factor_scores_pca_regression <- factor.scores(data, pca, method = "regression")
factor_scores_fa_regression <- factor.scores(data, fa, method = "regression")
print(factor_scores_pca_regression$scores)
print(factor_scores_fa_regression$scores)


factor_scores_pca_components <- factor.scores(data, pca, method = "components")
factor_scores_fa_components <- factor.scores(data, fa, method = "components")
print(factor_scores_pca_components$scores)
print(factor_scores_fa_components$scores)

