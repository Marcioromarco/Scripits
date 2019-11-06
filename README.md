Scripits
========
#install.packages("forestmangr")
require(forestmangr)

# Ajuste Clutter

getwd()


# Usando arquivo para aula CLUTTER com a funcao do forestmangr-----

dados_ajuste <- read.csv2("dados_clutter.csv", head=T, sep = ";", dec = ",")
head(dados_ajuste,1)
names(dados_ajuste)


coefs_clutter  <- fit_clutter (dados_ajuste , "IDADE", "HD" , "B" ,"VTCC" ,"S", "CHAVE")
coefs_clutter 
summary(coefs_clutter)

# Caso queira eliminar o parâmetro da área basal
coefs_clutter  <- fit_clutter (dados_ajuste , "IDADE", "HD" , "B" ,"VTCC" ,"S", "CHAVE", model = "mod")
coefs_clutter 

# Caos queira eliminar o parâmetro da área basal
coefs_clutter  <- fit_clutter (dados_ajuste , "IDADE", "HD" , "B" ,"VTCC" ,"S", "CHAVE", keep_model=TRUE)
coefs_clutter 


# Ajustando o modelo de Clutter passo a passo ------

# COMPARANDO AJUSTE DOIS ESTAGIOS PASSO A PASSO -----

# Importando o arquivo I1 e I2

dados<-read.csv2("dados_clutter_i2.csv",header=TRUE, dec = ",")
head(dados)
names(dados)

#Criando as variáveis

dados$X1 <- log(dados$B)*(dados$IDADE/dados$IDADE2)
dados$X2 <- (1-dados$IDADE/dados$IDADE2)
dados$X3 <- (1-dados$IDADE/dados$IDADE2) * dados$S
dados$S1 <- dados$S
dados$LNB2 <- log(dados$B2) 
dados$INVI2 <- 1/dados$IDADE2
dados$LNVCC2 <- log(dados$VTCC2) 



#1) Ajustou-se uma equação do tipo ----

REGb2b <- lm(LNB2 ~ X1+X2+X3+INVI2+S1, data=dados)
summary(REGb2b)
anova(REGb2b)

dados$LNB2est<-fitted(REGb2b)

# 2) Ajustou-se a equação de projeção do volume, com os valores estimados de LnB2 ----
#    obtidos no ajuste, no passo 1.

REGV2 <- lm(LNVCC2 ~ INVI2 + S1 + LNB2est, data = dados)
summary(REGV2)
anova(REGV2)


# 3) Ajuste da equação de projeção de área basal----


REGB2 <- lm( LNB2 ~-1+offset(X1) + X2 + X3, data=dados)
summary(REGB2)
anova(REGB2)

coef(REGB2)[1]
coef(REGB2)[2]
coef(REGV2)[1]
coef(REGV2)[2]
coef(REGV2)[3]
coef(REGV2)[4]
