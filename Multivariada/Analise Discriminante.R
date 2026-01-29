
data(iris)
iris_ <- iris[, c("Sepal.Width", "Petal.Width", "Species")]

# Renomear as colunas para facilitar a referência
colnames(iris_) <- c("X2", "X4", "Especies")

# Plotar os dados no espaço (X2, X4)
library(ggplot2)

ggplot(iris_, aes(x = X2, y = X4, color = Especies)) +
  geom_point() +
  labs(title = "Plot das variáveis X2 (largura sépala) e X4 (largura da pétala) para as três espécies",
       x = "Largura da Sépala (X2)",
       y = "Largura da Pétala (X4)") +
  theme_minimal()

#b

modelo <- manova(cbind(X2, X4) ~ Especies, data = iris_)
summary(modelo, test = "Pillai")
summary(modelo, test ="Wilks")
summary(modelo, test ="Hotelling")
summary(modelo, test ="Roy")

cov_setosa <- cov(iris_[iris_$Especies == "setosa", 1:2])
cov_versicolor <- cov(iris_[iris_$Especies == "versicolor", 1:2])
cov_virginica <- cov(iris_[iris_$Especies == "virginica", 1:2])


cov_setosa
cov_versicolor
cov_virginica


#c
mean_setosa <- colMeans(iris_[iris_$Especies == "setosa", 1:2])
mean_versicolor <- colMeans(iris_[iris_$Especies ==  "versicolor", 1:2])
mean_virginica <- colMeans(iris_[iris_$Especies == "virginica", 1:2])



x0 <- c(3.4, 1.75)
det_setosa <- det(cov_setosa)
det_versicolor <- det(cov_versicolor)
det_virginica <- det(cov_virginica)

inv_setosa <- solve(cov_setosa)
inv_versicolor <- solve(cov_versicolor)
inv_virginica <- solve(cov_virginica)

# escores discriminantes quadráticos
dQ_setosa <- -0.5 * log(det_setosa) - 0.5 * t(x0 - mean_setosa) %*% inv_setosa %*% (x0 - mean_setosa)
dQ_versicolor <- -0.5 * log(det_versicolor) - 0.5 * t(x0 - mean_versicolor) %*% inv_versicolor %*% (x0 - mean_versicolor)
dQ_virginica <- -0.5 * log(det_virginica) - 0.5 * t(x0 - mean_virginica) %*% inv_virginica %*% (x0 - mean_virginica)

# priori (ln(1/3))
ln_prior <- log(1/3)
dQ_setosa <- dQ_setosa + ln_prior
dQ_versicolor <- dQ_versicolor + ln_prior
dQ_virginica <- dQ_virginica + ln_prior

# escores discriminantes quadráticos
dQ_setosa
dQ_versicolor
dQ_virginica


classificacao <- which.max(c(dQ_setosa, dQ_versicolor, dQ_virginica))
especies <- c("setosa", "versicolor", "virginica")
especies[classificacao]

#d)
n_setosa <- n_versicolor <- n_virginica <- nrow(iris_subset[iris_subset$Species == "virginica", ])

Sp <- ((n_setosa - 1) * cov_setosa + (n_versicolor - 1) * cov_versicolor + (n_virginica - 1) * cov_virginica) / (n_setosa + n_versicolor + n_virginica - 3)
Sp_inv <- solve(Sp)


# escores discriminantes lineares
dL_setosa <- t(mean_setosa) %*% Sp_inv %*% x0 - 0.5 * t(mean_setosa) %*% Sp_inv %*% mean_setosa
dL_versicolor <- t(mean_versicolor) %*% Sp_inv %*% x0 - 0.5 * t(mean_versicolor) %*% Sp_inv %*% mean_versicolor
dL_virginica <- t(mean_virginica) %*% Sp_inv %*% x0 - 0.5 * t(mean_virginica) %*% Sp_inv %*% mean_virginica

ln_prior <- log(1/3)
dL_setosa <- dL_setosa + ln_prior
dL_versicolor <- dL_versicolor + ln_prior
dL_virginica <- dL_virginica + ln_prior


dL_setosa
dL_versicolor
dL_virginica

# Classificar a nova observação
classificacaodL <- which.max(c(dL_setosa, dL_versicolor, dL_virginica))

classification_result <- especies[classificacaodL]

# Resultado dos escores e classificação final
list(
  dL_setosa = dL_setosa,
  dL_versicolor = dL_versicolor,
  dL_virginica = dL_virginica,
  classification = classification_result
)

#E
dk1_setosa_versicolor <- (t(mean_setosa - mean_versicolor) %*% Sp_inv %*% x0 - 0.5 * t(mean_setosa - mean_versicolor) %*% Sp_inv %*% (mean_setosa + mean_versicolor)) >= log(1/1)
dk1_setosa_virginica <- (t(mean_setosa - mean_virginica) %*% Sp_inv %*% x0 - 0.5 * t(mean_setosa - mean_virginica) %*% Sp_inv %*% (mean_setosa + mean_virginica)) >= log(1/1)
dk1_versicolor_virginica <- (t(mean_versicolor - mean_virginica) %*% Sp_inv %*% x0 - 0.5 * t(mean_versicolor - mean_virginica) %*% Sp_inv %*% (mean_versicolor + mean_virginica)) >= log(1/1)

classification <- if (dk1_setosa_versicolor & dk1_setosa_virginica) {
  "setosa"
} else if (dk1_versicolor_virginica) {
  "versicolor"
} else {
  "virginica"
}

# resultados e classificação final
list(
  dk1_setosa_versicolor,
  dk1_setosa_virginica,
  dk1_versicolor_virginica,
  classification
)

library(ggplot2)
decision_boundary <- function(mean1, mean2, Sp_inv, x) {
  a <- Sp_inv %*% (mean1 - mean2)
  b <- 0.5 * t(mean1) %*% Sp_inv %*% mean1 - 0.5 * t(mean2) %*% Sp_inv %*% mean2
  (-a[1] * x - b) / a[2]
}


plot <- ggplot(iris_subset, aes(x = X2, y = X4, color = Species)) +
  geom_point() +
  labs(title = "Regiões de Classificação Linear",
       x = "Largura da Sépala (X2)",
       y = "Largura da Pétala (X4)") +
  theme_minimal()

plot <- plot +
  stat_function(fun = function(x) decision_boundary(mean_setosa, mean_versicolor, Sp_inv, x), geom = "line", color = "blue") +
  stat_function(fun = function(x) decision_boundary(mean_setosa, mean_virginica, Sp_inv, x), geom = "line", color = "green") +
  stat_function(fun = function(x) decision_boundary(mean_versicolor, mean_virginica, Sp_inv, x), geom = "line", color = "red")



print(plot)








#f
library(MASS)
lda_model <- lda(Especies ~ X2 + X4, data = iris_)

#ressubstituição
pred <- predict(lda_model, iris_)$class


confusion_matrix <- table(iris_$Especies, pred)
TEA <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
TEA


TEAE <- lda(Especies ~ X2 + X4, data = iris_, CV = TRUE)
CV_pred <- TEAE$class
CV_confusion_matrix <- table(iris_$Especies, CV_pred)
TEAE_rate <- 1 - sum(diag(CV_confusion_matrix)) / sum(CV_confusion_matrix)

TEA
TEAE_rate



