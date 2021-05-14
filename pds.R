setwd("C:/Users/pasca/OneDrive/Bureau/Cours ING3/PDS/Machine learning" )
###############
####importation de excel
library(readxl)
data.tot <- read_excel("dataset.xlsx")
data.tot

# nom de ligne
library(tibble)
data.tot<-column_to_rownames(data.tot,var="Residence")
data.tot
has_rownames(data.tot)

data=data.tot
data

## transformation en matrice
XX=as.matrix(data) ## Transforme le "Data Frame" en "Matrice"
XX

#####################
### Graphique 2D
####################
## graphique simple
plot(XX)
text(XX, labels=rownames(XX), cex= 0.6, pos=3)
## Graphique sofistiqué avec ggplot
library(ggplot2)
library(ggrepel)
set.seed(42)
ggplot(as.data.frame(XX), aes(Occupation,salaire_moyen))+ # Nuage des points
  geom_point(size=3, color="black")+#proprietés des points
  geom_text_repel(aes(label=rownames(XX)),size = 5) + #etiquette
  #scale_x_continuous( limits=c(-1, 20)) + # echelle axe x
  #scale_y_continuous( limits=c(-1, 20)) + #echelle axe y
  geom_hline(yintercept = 0, linetype="solid",
             color = "black", size=1)+ #axe x
  geom_vline(xintercept = 0, linetype="solid",
             color = "black", size=1) #axe y

set.seed(42)
ggplot(as.data.frame(XX), aes(Occupation,Nombre_moyen_enfants))+ # Nuage des points
  geom_point(size=3, color="black")+#proprietés des points
  geom_text_repel(aes(label=rownames(XX)),size = 5) + #etiquette
  #scale_x_continuous( limits=c(-1, 20)) + # echelle axe x
  #scale_y_continuous( limits=c(-1, 20)) + #echelle axe y
  geom_hline(yintercept = 0, linetype="solid",
             color = "black", size=1)+ #axe x
  geom_vline(xintercept = 0, linetype="solid",
             color = "black", size=1) #axe y
set.seed(42)
ggplot(as.data.frame(XX), aes(Occupation,vivant_seuls))+ # Nuage des points
  geom_point(size=3, color="black")+#proprietés des points
  geom_text_repel(aes(label=rownames(XX)),size = 5) + #etiquette
  #scale_x_continuous( limits=c(-1, 20)) + # echelle axe x
  #scale_y_continuous( limits=c(-1, 20)) + #echelle axe y
  geom_hline(yintercept = 0, linetype="solid",
             color = "black", size=1)+ #axe x
  geom_vline(xintercept = 0, linetype="solid",
             color = "black", size=1) #axe y

#######################################
###############Matrice de poids##########
poids=data[,2]/sum(data[,2])
# vecteur des poids proportionel à l'occupation
poids
DD = diag(poids)
DD
####vecteur de 1
ln = matrix(1,nrow=50,ncol=1)
ln
xb= t(XX)%*%DD%*%ln 
xb### moyenne ponderée
### cacul matriciel:
#t(XX) est le transposé de XX
#et %*% est le produit matriciel
xb ### C'est une matrice n x 1

YY<- XX-ln%*%t(xb)
YY
#### Matrice covariance ############
###################################
####Avec calcul Matriciel
VV=t(YY)%*%DD%*%YY #Matrice co-variance
VV

v=diag(VV) #vecteur des variances
v
s=v^0.5 #vecteur des ecart-types
s
SS=diag(s) # Matrice écart-type
SS
SSi=diag(s^(-1)) # Matrice inverse écart-type S^{-1}
SSi

####### Matrice centrée
###################
YY<- XX-ln%*%t(xb)
YY

# Matrice centré reduite
######################
ZZ= YY%*%SSi
ZZ
## Matrice correlations4
##############################
RR=SSi%*%VV%*%SSi
RR
RR=t(ZZ)%*%DD%*%ZZ
RR
rownames(RR)<-colnames(XX) ### renome lignes
colnames(RR)<-colnames(XX) ### renom colonnes
RR

###Fonction interne cor
RRnp=cor(XX) ### non ponderé; conserve les noms des variables

### Graphique des correlation
library(corrplot)
corrplot(RR)
corrplot(RR, method = "number")
corrplot.mixed(RR)
#####
### Distances entre individus ##
#################
dist(ZZ[c(1:20),],method = "euclidian",diag=TRUE)

BB = as.matrix(dist(ZZ[c(1:20),],method = "euclidian",diag=TRUE))
write.csv(BB,file="matrice.csv")

corrplot(as.matrix(dist(ZZ[c(1:10),], method = "euclidian",diag=TRUE)),
         is.corr=FALSE)
corrplot(as.matrix(dist(ZZ,method = "euclidian",diag=TRUE)),
         is.corr=FALSE)
rbind(ZZ,0) ## ajoute une ligne de 0=barycentre


as.matrix(dist(rbind(ZZ,0),method = "euclidian",diag=TRUE))[,50+1] # derniere ligne distance au barycentre

## distances au barycetre
db=as.matrix(dist(rbind(ZZ,0),method = "euclidian",diag=TRUE))[,50+1]
write.csv(db,file="dist-bar-QV.csv")

######Regression linéaire
fit<-lm(Occupation~., data = data)
summary(fit)
step(fit, direction = "backward")
####################détection des outliers########################

library(car)
outlierTest(fit2)
##On peut donc conclure que la résidence 4 est un outlier###

scatterplotMatrix(XX, smooth=FALSE,
                  pch=19, regLine = list(method=lm, lwd=1, col="red"))
#######################################Méthode des moindres carrées
mco <- function(X,Y){
  
  Xbar <- mean(X)
  Ybar <- mean(Y)
  
  betachap <- sum((X-Xbar) * (Y-Ybar))/sum((X-Xbar)^2)
  alphachap <- Ybar - (betachap * Xbar)
  
  return(c(alphachap, betachap))}
######################Comparaison de lm et de la méthode des moindres carrés####################
fitmcoX1 <- mco(data$salaire_moyen, data$Occupation)
fitmcoX1
fitlmX1<-coefficients(lm(Occupation~salaire_moyen, data = data))
fitlmX1
all.equal(fitmcoX1, fitlmX1, check.names = FALSE)
maxVraissX1 <- coefficients(glm(Occupation~salaire_moyen, data = data))
maxVraissX1
all.equal(maxVraissX1, fitlmX1, check.names = FALSE)

#######################  Optimisation du modèle  ###############""
fit<-lm(Occupation~., data = data)
step(fit, direction = "backward")

######Donc les variables pertinentes sont : 
#Taux_maladies
#Nombre_moyen_enfants
#salaire_moyen 
#nombre_hôpitaux 
#vivant_seuls
summary(fit)

#Adjusted R-squared:  0.9454 pour toutes les variables

######retestons avec les variables pertinentes
fit2<-lm(Occupation~Taux_maladies+Nombre_moyen_enfants+salaire_moyen+nombre_hôpitaux+vivant_seuls  , data = data)
summary(fit2)
#Adjusted R-squared:  0.9466 pour toutes ces variables légère amélioration

#########################  Vérification de la validité du modèle#########################

############## Test de Shapiro wilk ####################"
#Hypothèses :
#H0 : les résidus suivent une loi normale
#H1 : Les résidus ne suivent pas une loi normale
fit2<-lm(Occupation~Taux_maladies+Nombre_moyen_enfants+salaire_moyen+nombre_hôpitaux+vivant_seuls  , data = data)
res<-residuals(fit2)
shapiro.test(res)

###p-value>0.05 donc on rejette l'hypothèse H1 et les residus suivent une loi normale

#################### Test de non autocorrélation par la méthode de Durbin Watson#####################
library(lmtest)
durbinWatsonTest(fit2)
###On a rho != 0 donc on rejette l'hypothèse nulle dans notre modèle il y a donc autocorrélation des résidus.
#Et donc l'hypothèse d'indépendance des résidus est acceptée.

###########Test de l'homoscédasticité des résidus###################"
ncvTest(fit2)
#H0 : La variance des résidus est constante (homoscédasticité)
#H1 : La variance des résidus n'est pas constante (hétéroscédasticité)

#p = 0.13739 > 0.05 donc on rejette l'hypothèse alternative.Et il y a homoscédasticité

#############Validation globale du modèle###########################"""
library(gvlma)
gvmod1<-gvlma(fit2)
summary(gvmod1)
plot(fit2)
###########################coefficient de multicolinéarité############################
vif(fit2)
##############################
#Le modèle semble etre bon mais vu les résultats, il ne peut pas être maintenu
#sauf si on retire les variables non significatives