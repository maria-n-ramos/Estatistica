load("C:/Users/Maria Nilza/Downloads/Microdados Alunos SP Enem 2015.RData")
ls()

# Exibir as primeiras linhas do dataframe
head(dados)

# Obter um resumo estatístico dos dados
summary(dados)

# Verificar a estrutura do dataframe
str(dados)

dados2 <- na.omit(dados)
colunas_interesse <- c("nu.inscricao", "nota.media.enem", "nu.idade", "tp.sexo", "tp.cor.raca", "in.deficiencia.fisica",
                       "in.nunca.trabalhou","q007","q024","complex.gestao.escola","ind.reg.doc.escola" ,"inse.escola",
                       "num.alunos.escola","prop.adeq.docente.alta")
dadosint <- dados2[, colunas_interesse]


"""
dicionário ocup.pai.mae
Grupo 1: Lavrador, agricultor sem empregados, bóia fria, criador de animais (gado, porcos, galinhas, ovelhas, cavalos etc.), apicultor, pescador, lenhador, seringueiro, extrativista.
Grupo 2: Diarista, empregado doméstico, cuidador de idosos, babá, cozinheiro (em casas particulares), motorista particular, jardineiro, faxineiro de empresas e prédios, vigilante, porteiro, carteiro, office-boy, vendedor, caixa, atendente de loja, auxiliar administrativo, recepcionista, servente de pedreiro, repositor de mercadoria.
Grupo 3: Padeiro, cozinheiro industrial ou em restaurantes, sapateiro, costureiro, joalheiro, torneiro mecânico, operador de máquinas, soldador, operário de fábrica, trabalhador da mineração, pedreiro, pintor, eletricista, encanador, motorista, caminhoneiro, taxista.
Grupo 4: Professor (de ensino fundamental ou médio, idioma, música, artes etc.), técnico (de enfermagem, contabilidade, eletrônica etc.), policial, militar de baixa patente (soldado, cabo, sargento), corretor de imóveis, supervisor, gerente, mestre de obras, pastor, microempresário (proprietário de empresa com menos de 10 empregados), pequeno comerciante, pequeno proprietário de terras, trabalhador autônomo ou por conta própria.
Grupo 5: Médico, engenheiro, dentista, psicólogo, economista, advogado, juiz, promotor, defensor, delegado, tenente, capitão, coronel, professor universitário, diretor em empresas públicas e privadas, político, proprietário de empresas com mais de 10 empregados.

- Categorias de adequação da formação dos docentes em relação
à disciplina que lecionam.
Fonte: INEP.
Grupo Descrição
1.Docentes com formação superior de licenciatura na mesma disciplina
que lecionam ou bacharelado na mesma disciplina com curso de
complementação pedagógica concluído.
2.Docentes com formação superior de bacharelado na disciplina
correspondente,mas sem licenciatura ou complementação pedagógica.
3.Docentes com licenciatura em área diferente daquela que lecionam ou com
bacharelado nas disciplinas da base curricular comum e complementação
pedagógica concluída em área diferente daquela que lecionam.
4.Docentes com outra formação superior não considerada nas categorias
anteriores.
5.Docentes que não possuem curso superior completo

Tabela 4 - Descrição dos níveis de complexidade de gestão
Níveis Descrição¹
Nível 1 Porte inferior a 50 matrículas, operando em único turno e etapa e apresentando a
Educação Infantil ou Anos Iniciais como etapa mais elevada*.
Nível 2 Porte entre 50 e 300 matrículas, operando em 2 turnos, com oferta de até 2 etapas e
apresentando a Educação Infantil ou Anos Iniciais como etapa mais elevada*.
Nível 3 Porte entre 50 e 500 matrículas, operando em 2 turnos, com 2 ou 3 etapas e
apresentando os Anos Finais como etapa mais elevada*.
Nível 4 Porte entre 150 e 1000 matrículas, operando em 2 ou 3 turnos, com 2 ou 3 etapas,
apresentando Ensino Médio/profissional ou a EJA como etapa mais elevada*.
Nível 5 Porte entre 150 e 1000 matrículas, operando em 3 turnos, com 2 ou 3 etapas,
apresentando a EJA como etapa mais elevada*.
Nível 6 Porte superior à 500 matrículas, operando em 3 turn

"""



# O CONJUNTO DE DADOS TEM ORIGINALMENTE 65844 LINHAS E 33 COLUNAS
# DADOSINT TEM 59424 L E 14 C

summary(dadosint)



unique(dadosint$complex.gestao.escola);c # pessoas de nível 2 fazendo enem,no fundamental 1
escolas_EF1 <- dadosint[dadosint$complex.gestao.escola == "Nível 2", ]
escolas_pre_EM <- dadosint[dadosint$complex.gestao.escola == "Nível 3",]
escolas_EM <- dadosint[dadosint$complex.gestao.escola == "Nível 4" | dadosint$complex.gestao.escola == "Nível 5"| dadosint$complex.gestao.escola == "Nível 6",]

(nrow(escolas_EF1) + nrow(escolas_pre_EM))/nrow(dadosint)*100
(nrow(escolas_EF1) + nrow(escolas_pre_EM))/(nrow(escolas_EF1) + nrow(escolas_pre_EM)+nrow(escolas_EM))*100

par(mfrow = c(1, 1))
hist(escolas_EF1$nota.media.enem,xlim=c(100,900),xlab="",ylab='Média das Notas do ENEM', main='Notas Médias do ENEM por Escolas com turmas até o Fundamental I')
hist(escolas_pre_EM$nota.media.enem,xlim=c(100,900),xlab="",ylab='Média das Notas do ENEM', main='Notas Médias do ENEM por Escolas com turmas até o Fundamental II')
hist(escolas_EM$nota.media.enem,xlim=c(100,900),xlab="",ylab='Média das Notas do ENEM', main='Notas Médias do ENEM por Escolas com turmas com Ensino Médio')



median(escolas_EF1$nota.media.enem)
median(escolas_pre_EM$nota.media.enem)
median(escolas_EM$nota.media.enem)


median(escolas_EF1$prop.adeq.docente.alta)
median(escolas_pre_EM$prop.adeq.docente.alta)
median(escolas_EM$prop.adeq.docente.alta)


mean(escolas_EF1$nota.media.enem)
mean(escolas_pre_EM$nota.media.enem)
mean(escolas_EM$nota.media.enem)

max(escolas_EF1$nota.media.enem)
max(escolas_pre_EM$nota.media.enem)
max(escolas_EM$nota.media.enem)

par(mfrow = c(1, 1))

media_enem_ef1_feminino <- mean(escolas_EF1$nota.media.enem[escolas_EF1$tp.sexo == "F"], na.rm = TRUE)
media_enem_pre_em_feminino <- mean(escolas_pre_EM$nota.media.enem[escolas_pre_EM$tp.sexo == "F"], na.rm = TRUE)
media_enem_em_feminino <- mean(escolas_EM$nota.media.enem[escolas_EM$tp.sexo == "F"], na.rm = TRUE)


media_enem_ef1_masculino <- mean(escolas_EF1$nota.media.enem[escolas_EF1$tp.sexo == "M"], na.rm = TRUE)
media_enem_pre_em_masculino <- mean(escolas_pre_EM$nota.media.enem[escolas_pre_EM$tp.sexo == "M"], na.rm = TRUE)
media_enem_em_masculino <- mean(escolas_EM$nota.media.enem[escolas_EM$tp.sexo == "M"], na.rm = TRUE)

medias_feminino <- c(media_enem_ef1_feminino, media_enem_pre_em_feminino, media_enem_em_feminino)
medias_masculino <- c(media_enem_ef1_masculino, media_enem_pre_em_masculino, media_enem_em_masculino)

par(mfrow = c(1, 1))
etapas <- c('até Fundamental I', 'após Fundamental I', 'após Fundamental II')

# Plote
plot(1:3, medias_feminino, type="o", col="violet", pch=16, ylim=c(min(c(medias_feminino, medias_masculino)) - 10, max(c(medias_feminino, medias_masculino)) + 10), xaxt='n', xlab='Etapa Escolar', ylab='Média das Notas do ENEM', main='Notas Médias do ENEM por Sexo baseadas na Complexidade de Gestão Escolar')
lines(1:3, medias_masculino, type="o", col="blue", pch=16)
axis(1, at=1:3, labels=etapas)

legend("topright", legend=c("Feminino", "Masculino"), col=c("violet", "blue"), pch=16)

#médias de prop.adeq.docente.alta
media_prop_adeq_docente_ef1 <- mean(escolas_EF1$prop.adeq.docente.alta, na.rm = TRUE)
media_prop_adeq_docente_pre_em <- mean(escolas_pre_EM$prop.adeq.docente.alta, na.rm = TRUE)
media_prop_adeq_docente_em <- mean(escolas_EM$prop.adeq.docente.alta, na.rm = TRUE)
medias_prop_adeq_docente <- c(media_prop_adeq_docente_ef1, media_prop_adeq_docente_pre_em, media_prop_adeq_docente_em);medias_prop_adeq_docente
#médias denum alunos
media_num_alunos_ef1 <- mean(escolas_EF1$num.alunos.escola, na.rm = TRUE)
media_num_alunos_pre_em <- mean(escolas_pre_EM$num.alunos.escola, na.rm = TRUE)
media_num_alunos_em <- mean(escolas_EM$num.alunos.escola, na.rm = TRUE)
medias_num_alunos <- c(media_num_alunos_ef1, media_num_alunos_pre_em, media_num_alunos_em);medias_num_alunos


media_enem_ef1_feminino <- mean(escolas_EF1$nota.media.enem[escolas_EF1$q024 == "Não"], na.rm = TRUE)
media_enem_pre_em_feminino <- mean(escolas_pre_EM$nota.media.enem[escolas_pre_EM$q024 == "Não"], na.rm = TRUE)
media_enem_em_feminino <- mean(escolas_EM$nota.media.enem[escolas_EM$q024 == "Não"], na.rm = TRUE)


media_enem_ef1_masculino <- mean(escolas_EF1$nota.media.enem[escolas_EF1$q024 == "Sim"], na.rm = TRUE)
media_enem_pre_em_masculino <- mean(escolas_pre_EM$nota.media.enem[escolas_pre_EM$q024 == "Sim"], na.rm = TRUE)
media_enem_em_masculino <- mean(escolas_EM$nota.media.enem[escolas_EM$q024 == "Sim"], na.rm = TRUE)

medias_feminino <- c(media_enem_ef1_feminino, media_enem_pre_em_feminino, media_enem_em_feminino)
medias_masculino <- c(media_enem_ef1_masculino, media_enem_pre_em_masculino, media_enem_em_masculino)


etapas <- c('até Fundamental I', 'após Fundamental I', 'após Fundamental II')

# Plote
plot(1:3, medias_feminino, type="o", col="violet", pch=16, ylim=c(min(c(medias_feminino, medias_masculino)) - 10, max(c(medias_feminino, medias_masculino)) + 10), xaxt='n', xlab='Etapa Escolar', ylab='Média das Notas do ENEM', main='Notas Médias do ENEM por Possuir Computador baseadas na Complexidade de Gestão Escolar')
lines(1:3, medias_masculino, type="o", col="blue", pch=16)
axis(1, at=1:3, labels=etapas)

legend("topright", legend=c("Não", "Sim"), col=c("violet", "blue"), pch=16)



media_enem_ef1_feminino <- mean(escolas_EF1$nota.media.enem[escolas_EF1$q007 == "Não"], na.rm = TRUE)
media_enem_pre_em_feminino <- mean(escolas_pre_EM$nota.media.enem[escolas_pre_EM$q007 == "Não"], na.rm = TRUE)
media_enem_em_feminino <- mean(escolas_EM$nota.media.enem[escolas_EM$q007 == "Não"], na.rm = TRUE)


media_enem_ef1_masculino <- mean(escolas_EF1$nota.media.enem[escolas_EF1$q007 == "Sim"], na.rm = TRUE)
media_enem_pre_em_masculino <- mean(escolas_pre_EM$nota.media.enem[escolas_pre_EM$q007 == "Sim"], na.rm = TRUE)
media_enem_em_masculino <- mean(escolas_EM$nota.media.enem[escolas_EM$q007 == "Sim"], na.rm = TRUE)

medias_feminino <- c(media_enem_ef1_feminino, media_enem_pre_em_feminino, media_enem_em_feminino)
medias_masculino <- c(media_enem_ef1_masculino, media_enem_pre_em_masculino, media_enem_em_masculino)


etapas <- c('até Fundamental I', 'após Fundamental I', 'após Fundamental II')

# Plote
plot(1:3, medias_feminino, type="o", col="violet", pch=16, ylim=c(min(c(medias_feminino, medias_masculino)) - 10, max(c(medias_feminino, medias_masculino)) + 10), xaxt='n', xlab='Etapa Escolar', ylab='Média das Notas do ENEM', main='Notas Médias do ENEM por Possuir Empregada Doméstica baseadas na Complexidade de Gestão Escolar')
lines(1:3, medias_masculino, type="o", col="blue", pch=16)
axis(1, at=1:3, labels=etapas)

legend("topright", legend=c("Não", "Sim"), col=c("violet", "blue"), pch=16)


# função para calcular médias agrupadas por tp.cor.raca
calcula_media_por_cor_raca <- function(df, col) {
  aggregate(df[[col]], by=list(df$tp.cor.raca), FUN=mean, na.rm=TRUE)
}


media_enem_ef1_por_cor_raca <- calcula_media_por_cor_raca(escolas_EF1, "nota.media.enem")
media_prop_adeq_docente_ef1_por_cor_raca <- calcula_media_por_cor_raca(escolas_EF1, "prop.adeq.docente.alta")
media_num_alunos_ef1_por_cor_raca <- calcula_media_por_cor_raca(escolas_EF1, "num.alunos.escola")

media_enem_pre_em_por_cor_raca <- calcula_media_por_cor_raca(escolas_pre_EM, "nota.media.enem")
media_prop_adeq_docente_pre_em_por_cor_raca <- calcula_media_por_cor_raca(escolas_pre_EM, "prop.adeq.docente.alta")
media_num_alunos_pre_em_por_cor_raca <- calcula_media_por_cor_raca(escolas_pre_EM, "num.alunos.escola")

media_enem_em_por_cor_raca <- calcula_media_por_cor_raca(escolas_EM, "nota.media.enem")
media_prop_adeq_docente_em_por_cor_raca <- calcula_media_por_cor_raca(escolas_EM, "prop.adeq.docente.alta")
media_num_alunos_em_por_cor_raca <- calcula_media_por_cor_raca(escolas_EM, "num.alunos.escola")


print(media_enem_ef1_por_cor_raca)
print(media_prop_adeq_docente_ef1_por_cor_raca)
print(media_num_alunos_ef1_por_cor_raca)

print(media_enem_pre_em_por_cor_raca)
print(media_prop_adeq_docente_pre_em_por_cor_raca)
print(media_num_alunos_pre_em_por_cor_raca)

print(media_enem_em_por_cor_raca)
print(media_prop_adeq_docente_em_por_cor_raca)
print(media_num_alunos_em_por_cor_raca)




####
colunas_interesse <- c("nota.media.enem", "nu.idade", "tp.sexo", "tp.cor.raca","q007","q024",
                       "num.alunos.escola","prop.adeq.docente.alta")
df_para_RLS <- escolas_EM[, colunas_interesse]
summary(df_para_RLS)
y <- as.double(df_para_RLS$nota.media.enem)
x1 <- as.double(df_para_RLS$nu.idade)
x2 <- as.double(df_para_RLS$num.alunos.escola)
x3 <- as.double(df_para_RLS$prop.adeq.docente.alta)
sexo <- as.factor(df_para_RLS$tp.sexo)
cor_raca <- as.factor(df_para_RLS$tp.cor.raca)
q007 <- as.factor(df_para_RLS$q007)
q024 <- as.factor(df_para_RLS$q024)
base <- data.frame(y, x1, x2, x3, sexo,cor_raca,q007,q024)
base<- na.omit(base)
# Ajustando o modelo de regressão linear
fit <- lm(y ~ x1 + x2 + x3 + sexo+ cor_raca+q007+q024, data = base)


# Exibindo o resumo do modelo ajustado
summary(fit)

step(fit, trace = FALSE, direction = 'backward')


fit_novo <- lm(formula = y ~ x1 + x2 + x3 + s1 + rc1 + rc2 + rc3 + rc4 + 
                 q7_1 + q24_1, data = base)
summary(fit_novo)



residuos <- residuals(fit_novo)

# Plotando o histograma dos resíduos
hist(residuos, main="Histograma dos Resíduos", xlab="Resíduos", col="lightblue", border="black", breaks=30)

# Q-Q plot para verificar a normalidade dos resíduos
qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)

# K-Smirnov para normalidade
ks.test(residuos, "pnorm", mean = mean(residuos), sd = sd(residuos)) #rejeita-se H0 de que os resíduos seguem uma distribuição normal





# Gráfico de Resíduos vs Valores Ajustados
plot(fit_novo$fitted.values, residuals(fit_novo), main="Resíduos vs Valores Ajustados", xlab="Valores Ajustados", ylab="Resíduos")
abline(h = 0, col = "red")

par(mfrow = c(2, 2))
# Gráfico de Alavancagem
plot(hatvalues(fit_novo), main="Gráfico de Alavancagem", xlab="Índice", ylab="Valores de Alavancagem")
abline(h = 2*mean(hatvalues(fit_novo)), col = "red")  # Linha de referência

# Gráfico da Distância de Cook
plot(cooks.distance(fit_novo), main="Distância de Cook", xlab="Índice", ylab="Distância de Cook")
abline(h = 4/(nrow(base)-length(fit_novo$coefficients)-2), col = "red")  # Linha de referência

# Resíduos Padronizados
std_res <- rstandard(fit_novo)
plot(std_res, main="Resíduos Padronizados", xlab="Índice", ylab="Resíduos Padronizados")
abline(h = c(-2, 2), col = "red")  # Linhas de referência

# Resíduos Studentizados
stud_res <- rstudent(fit_novo)
plot(stud_res, main="Resíduos Studentizados", xlab="Índice", ylab="Resíduos Studentizados")
abline(h = c(-3, 3), col = "red")  # Linhas de referência

# Identificar pontos com alta alavancagem
high_leverage <- which(hatvalues(fit_novo) > 2*mean(hatvalues(fit_novo)))

# Identificar pontos com alta distância de Cook
high_cooks <- which(cooks.distance(fit_novo) > 4/(nrow(base)-length(fit_novo$coefficients)-2))

# Identificar pontos com resíduos padronizados fora do intervalo [-2, 2]
high_std_res <- which(abs(stud_res) > 2)

# Identificar pontos com resíduos studentizados fora do intervalo [-3, 3]
high_stud_res <- which(abs(stud_res) > 3)

# Combinar todas as identificações para obter um conjunto de outliers
outliers <- unique(c(high_leverage, high_cooks, high_std_res, high_stud_res))

# Exibir os índices dos outliers
print(outliers)

#sem outlier
fit <- lm(y ~ x1 + x2 + x3 + sexo+ cor_raca+q007+q024, data = base_no_outliers)
residuos <- residuals(fit)

# Exibindo o resumo do modelo ajustado
summary(fit)
step(fit, trace = FALSE, direction = 'backward')


fit_novo <- lm(formula = y ~ x1 + x2 + x3 + s1 + rc1 + rc2 + rc3 + rc4 + 
                 q7_1 + q24_1, data = base_no_outliers)
summary(fit_novo)



residuos <- residuals(fit_novo)

# Plotando o histograma dos resíduos
hist(residuos, main="Histograma dos Resíduos", xlab="Resíduos", col="lightblue", border="black", breaks=30)

# Q-Q plot para verificar a normalidade dos resíduos
qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)

# WLS
pesos <- 1 / (residuos^2)
base_no_outliers <- base[-outliers, ]
fit_wls <- lm(y ~ x1 + x2 + x3 + sexo+ cor_raca+q007+q024, data = base_no_outliers, weights = pesos[-outliers])


# Exibindo o resumo do modelo ajustado
summary(fit_wls)

variables_included <- names(coef(fit_wls))
print(variables_included)
intercept_included <- "(Intercept)" %in% variables_included
print(intercept_included)



residuos <- residuals(fit_wls)
qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)
par(mfrow = c(1, 1))
plot(base_no_outliers$y, fitted(fit_wls),
     xlab = "Valores Reais",
     ylab = "Valores Ajustados",
     main = "Ajuste do Modelo WLS",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)
library(car)
durbinWatsonTest(fit_wls)

levels(base_no_outliers$cor_raca)
plot(residuos, main = "Índices vs Resíduos", xlab = "Índices", ylab = "Resíduos")
abline(h = 0, col = "red", lwd = 2)






# analise de residuos



plot(base_no_outliers$y, fitted(fit_wls),
     xlab = "Valores Reais",
     ylab = "Valores Ajustados",
     main = "Ajuste do Modelo WLS",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)
# Plotando o histograma dos resíduos
hist(residuos, main="Histograma dos Resíduos", xlab="Resíduos", col="lightblue", border="black", breaks=30)

# Q-Q plot para verificar a normalidade dos resíduos
qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)

"""
base_no_outliers<-scale(base_no_outliers)
library(splines)
fit_wls_spline <- lm(y ~ bs(x1, df = 4) + bs(x2, df = 4) + bs(x3, df = 4) + sexo + cor_raca + q007 + q024, 
                     data = base_no_outliers, weights = pesos[-outliers])

# Exibir o resumo do modelo ajustado
summary(fit_wls_spline)
par(mfrow = c(1, 1))
# Extraindo os resíduos do modelo WLS com splines
residuos <- residuals(fit_wls_splines)
plot(base_no_outliers$y, fitted(fit_wls_spline),
     xlab = "Valores Reais",
     ylab = "Valores Ajustados",
     main = "Ajuste do Modelo WLS com Spline",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)
# Plotando o histograma dos resíduos
hist(residuos, main="Histograma dos Resíduos", xlab="Resíduos", col="lightblue", border="black", breaks=30)

# Q-Q plot para verificar a normalidade dos resíduos
qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)

# K-Smirnov para normalidade
ks.test(residuos, "pnorm", mean = mean(residuos), sd = sd(residuos)) #rejeita-se H0 de que os resíduos seguem uma distribuição normal
"""