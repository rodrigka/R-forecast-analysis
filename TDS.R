#                            #
#  Techniques de prevision   #
#                            #

#### TD1 ####

#install.packages("readxl")   si ca marche pas
library(readxl)
mydata1 <- read_excel("L3-SNGI/S5/Techniques de prevision - Logiciel R/donnees_cons.xls")

mydata1$taille <- mydata1$taille_menage #créer la variable taille = taille menage
mydata1$taille_menage <- NULL  #suprimmer une variable

#fonction attach : permet de faire un lien avec une base de données
attach(mydata1)
cons
detach(mydata1)

#median, mean 25th and 75th quartiles, min , max
summary(mydata1)
#conso moyenne = 2127
#50% consomme : moins de 2108
#25% consomme : moins de 1877
#75% consomme : moins de 2274, donc 25% plus de 2274

#min, max, range, sum, median, var, sd
#install.packages("pastecs")
library(pastecs)
stat.desc(mydata1)
# 6*10^1 = 60  -> echantillon cylindré :pas de valeurs manquantes
# valeur manquante : na = not available.
# min = 1.4976*10^3 = 1497.6
# variance = 1.06001*10^5 = 106001
# ecartype ( : sd) = 3.255*10^2 = 325,5 (on s'eloigne très peu de la moyenne)
# moyenne = 2127

stat.desc(mydata1)["cons"]
attach(mydata1)
mean(mydata1$cons)
sd(cons)

median(cons)

#mean sd, median, mad, min, max, skewness, kurtosis
#install.packages("psych")
library(psych)
describe(mydata1)

#Histogramme cons
hist(mydata1$cons)

#densité de cons
d <- density(mydata1$cons)
plot(d)
#maintenant on voit la fonc de densité

#decrire une varible quantitative discrete
table(mydata1$taille)

#average cons par taille (conso moyenne par taille) 
tapply(mydata1$cons,mydata1$taille, mean)

#average cons et rev par taille : ici plusieurs variables
ag <- aggregate(mydata1[c("cons","rev")],by=list(mydata1$taille), mean)
#(avec "ag <-" on enregistre ca sous la forme d'une base de données)

#Cammembert
mytable <- table(mydata1$taille)
pie(mytable, col =rainbow(6), clockwise = TRUE ) #sens montre

attach(mydata1)
#Calculer le coeff de correlation entre cons et rev
cor(cons, rev)   #Pas encore de relation de causalité


#representer un nuage de points
plot(cons ~ rev)

#Créer une variable dummy =1 ( 2 possibilités : 1 si vrai, 0 si non)
mydata1$taille4 <- 1*(taille<4)

#Afficher le nombre de 1 et de 0 :
table(mydata1$taille4)

#Tableau de contingeance (dynamique)
table(mydata1$taille4, mydata1$taille)

attach(mydata1)
#Moyenne cons by taille4
tapply(cons, taille4, mean)

#t-test : Test de comparaison de moyenne
#H0 : mean 0 = mean 1 OU mean- mean 1 = 0
t.test(cons ~ taille4, data=mydata1) #H1 : mean 0 != mean 1
#Conclusion : p-value = probabilité critique   < 0,05 (Maximum : risque de première espece)
#On rejete H0 : Il y a bien une difference entre l'echantillon "0" et l'echantillon"1"
#Conso moyenne des menages (>4)   >   Conso moyenne des menages (<4)

#H1 : mean 0 > mean 1 OU mean 0-mean 1 >0
t.test(cons ~ taille4, alternative=c("greater"), data=mydata1) 
#Conclusion : p-value = probabilité critique   < 0,05 (Maximum : risque de première espece)

#H1 : mean 0 < mean 1 OU mean 0-mean 1 <0
t.test(cons ~ taille4, alternative=c("less"), data=mydata1) 
#Quand on met less -> 100% de risque de se tromper en rejetant H0 : 
#jpp rejeter H0 alors que j'ai pu dans le test bilateral

#C.  REGRESSIONS
#Loi psycologique fondamentale => fonction de conso keynesienne : 
#C = C0 + cY, avec C = propension marginale à consommer, 0<c<1
#Modèle économétrique : cons_i= a + b*rev_i + residu

#Modèle de regression simple
model1 <- lm(cons ~ rev, data = mydata1)
#lm : methode des moindres carrés ordinaires
summary(model1)
#Si peu de difference entre le parametre (2) et la taille n (~60) -> R² corrigé (pas utile dans notre cas)
#Le R² proche de 1 : regression est de bonne qualité 
#=> 91,59% de la variance observée de la consommation est expliquée par le modèle.

#Representer la droite de regression
#y à x=0 : 328.32254
#Coefficient : 0.75839
plot(cons~rev, data=mydata1)
abline(lm(cons ~ rev, data = mydata1))

#predicted cons (1) : en copiant collant
mydata1$consp<- 328.32254 + 0.75839*mydata1$rev

#predicted cons (2) : automatiquement (sans arrondis)
mydata1$consp2 <- model1$fitted.values


summary(model1)
#Significativité du coefficient : le coeff est il diff de 0 dans la pop?
# => Test de student
#H0 : coeff b = 0   vs   H1 : coeff b != 0
#Statistique de test : t = 25,133 (=0.75839("Estimate")/0.03017("std error"))
#Valeur théorique : t* = ~2 pour un risque de première espèce = 5% et n-k=58
#Decision : On rejète H0 car |t|>t*   => b est different de 0, donc significatif
#Sens de l'effet : signe + => effet positif de revenu, sur la consommation
#L'ampleur de l'effet : en moyenne et toutes choses egales par ailleurs, 
#quand le revenu augmente de 1, la consommation augmente de 0,76

confint(model1)
#Il ya 95% de chances que la vraie valeur du coeff soit comprise entre
# 0.6979.. et 0.8187...

#full model
model2 <- lm(cons ~ rev + taille, data=mydata1)
summary(model2)

# qualité de la regression 
# R2 -> 98,81% de la variance observée de la conso est expliquée par le moèle
#i.e. par les differences de revenu et de taille de menage

#Test de significativité globale:
#1/ Hypothèses :H0 : tous coeffs sont nulls   VS   H1: au moins un coeff = 0
#2/Statistique de test : F = 2370?
#3/ Valeur th : F* = 3.1
#4/Decision : rejet de H0 car F>F*, On accepte H1, le modèle est globalement explicatif

#OU:

#2/ Probabilité critique = p-value <0,05 => moins de 5 pourcent de risque de se tromper en rejettant H0
# => On rejette H0, on accepte H1, le modèle est globalement explicatif.


#Effet de la significativité de la taille sur la consommation:

#1/ Significativité : test de student: H0: b=0   VS   H1!=0 (coeff significatif)
#Statistique de test : t=18,01
#valeur thorique t*= 2.00
#rejet de H0 car |t|>t* => effet significatif de la taille sur la conso ceteris paribus.

#2/ Sens de l'effet => effet positif de la taille sur la conso ceteris paribus car b2>0

#3/Ampleur de l'effet => en moyene et toute chose égale par ailleurs lors que la taille augmente de 1 (membre), la consomation augmente de 93,8 unités monetaires.


#### TD2 ####
library(readxl)
mydata2 <- read_excel("L3-SNGI/S5/Techniques de prevision - Logiciel R/donnees_sal.xls")

#Il y a 2 variables inutiles, on va les supprimer :
mydata2$...1 <- NULL
mydata2$individu <- NULL

#Attacher ma base de données:
attach(mydata2)

##Statistiques univariées:

library(psych)
describe(mydata2)
#l'echantillon est cylindré (balanced) = aucune valeur manquante.
#Analyse sur 2822 individus
#Variables continues :age, educ, sal, pas de valeurs extremes car medianne/moyenne pas très concequent.
#Peu de dispersion : sd/moyenne
#Skewness : 1.51 (sal) asymètrie vers la droite 
#Kurtosis : courbe plus pointue que la loi normale : salaires plus concentres autour de la moyenne (si distribution normale)


mydata2$lnsal <- as.numeric(mydata2$lnsal)


#matrice de correlation:
cor(mydata2)
#Pour faire representation graphique
#install.packages("corrplot")
library(corrplot)
corrplot(cor(mydata2),method="color", type="upper", diag = FALSE)  #color si on veut
#Conserver une partie inferieur ou superieure de la matrice : type.
#Supprimer la diagonale (relation educ/educ par exemple) : diag


#Effet attendu des diffferentes variables sur le salaire:

#age : effet positif proxy de l'expérience, th du capital humain.
#educ : effet positif théorie du capital humain
#noir : effet négatif : discrimination salariale
#urbain : effet positif : salaire plus élévés dans les grandes ville
#sud : effet negatif...
#dist : effet neutre sur le salaire.

#Estimation du modèle loglinéaire.
#Modèle économètrique :
#tester la nonlinéarité de l'age sur le salaire (age² et age...)
# lnsal_iv = b0 + b1 age_i + b2 age²_i + b3 noir_i + b4 urbain_i + b5 sud_i +b6 educ_i+ residu_i
reg1 <- lm(lnsal ~ age+I(age^2)+educ+noir+urbain+sud,data=mydata2)
summary(reg1)
#R² => 28.52% de la variance observée des salaires est expliquée par le modèle

#Il faut faire un test de significativité globale car le modèle est pas ouf
#Test de significativité globale :
#1/ Hypothèses : H0 tous les coeffs =O VS H1 = au moins 1 est non nul (significatif)
#2/Statistique de test : F = 187,2
#3/ Valeur th : F* = 2,10
#4/ Decision : |F|>F* on rejette l'hypothèse nulle, on accepte H1.

###Effet de la varible éduc : H0 b3=0 VS H1= b3!=0
#Statistique de test :t= ~12
# valeur th : t* = 1.96
#Rejet de H0 car |t|>t* = > educ a un effet significatif sur le salaire ceteris paribus.

#2/Sens de l'effet : => effet positif de l'éduc sur le salaire cetreis paribus car b3>0

#3/ ampleur de l'effet de l'educ : en moyenne et toute chose égale par ailleurs, une année d'education suplementaire, entarine une hausse de salaire de :
#Valeur exacte :
(exp(0.0346326-1))*100

## 3.d Effet de l'age
# 1) significativité du coefficient de la variable du carré -> effet non-linéaire

#Test de student : on prend l'age^2
#Hypothese : H0 : b2=0 vs b2!=0
#statistique de test : t= -1.97
#Decision : rejet de H0 car |t|>t*
#-> l'age a un effet non-linéaire sur le salaire.

# 2) significativité et sens de l'effet de la variable -> signe de l'effet (+ convexe /- concave)

#test de student: on prend l'age
#Hypothese : H0 : b1=0 vs b1!=0
#p-value = 0.00491 < 0,05 (risque de première espèce)
#Decision : rejet de H0 donc b1!=0
#Coefficient positif
#-> l'age à un effet non-nul (significatif) sur le salaire ceteris paribus.

# 3)Signe du coefficient de la variable au carré (age^2)

# b2 <0 => rendement marginaux de l'age sur le salaire decroissants
#=> L'age a un effet positif non-linéaire, avec des rendements marginaux décroissants sur le salaire.
#NB : comme l'effet est non-linéaire, on peut pas donner l'ampleur de l'effet.
#-> si l'effet est linéaire il faut donner l'ampleur de l'effet.

### 4. Procedures de correction

## Multicolinéairté : variables explicatives fortement corrélées entre elles.
#Test de Klein : H0 : pas de pb de multicolinéarité vs H1= multicolinéarité.
#Stat de test : Comparaison de R² avec les coefficients de corrélation linéaire entre elles
#Si coeff > R² => risque de colinéarité.

#Base ou il y a que les varibles explicatives (création d'un sous-ensemble) :
datacor <- subset(mydata2, select=c(age,educ,noir,urbain,sud))
cor(datacor)
#R²=0,2852
#noir et sud sont potentiellement collinéaires car 0.34 > 0.2852
reg2 <- lm(lnsal ~ age+I(age^2)+educ+noir+urbain+sud+I(noir*sud),data=mydata2)
summary(reg2)
#test de student sur le coefficient de I (noir*sud) :
# |t| > t* donc le coeffcient est significatif => preuve que (noir*sud) sont colinéaires.
#-> On conserve le terme d'interaction I dans le modèle.

##Heteroscedasticité : la variance des erreurs n'est pas constante entre les individus.
#i.e. la variance des erreurs ets plus importante pour certains individus qui ont de caractéristiques espécifiques.
# concequence : estimateur sans biais mais inéficaces = coefficients ok, mais ecartypes faux -> tests de student sont faux, car t de student faux.

#2 tests possibles : White ou Borshwagan
#Cherchons la souce d'hetéroscédasticité -> creons la variable residus²
mydata2$resid2 <- reg2$residuals^2

#age :
plot(resid2 ~ age,data=mydata2)
#-> l'age n'est apparament pas source d'hétéroscedasticité
#education :
plot(resid2 ~ educ,data=mydata2)
#-> l'education n'est apparament pas source d'hétéroscedasticité

#On doit donc réaliser le tests de White : 
#H0 : Homocédasticité vs H1: hétéroscedasticité
white <- lm(resid2 ~ (age + educ + noir + urbain + sud)^2
            +I(age^2)+I(educ^2)+I(noir^2)+I(urbain^2)+I(sud^2),
            data=mydata2)
summary(white)
#statistique de test :
0.009147*2822 #25.81   (2822: nombre d'observations)
#valeur th pour alpha 0.05 et p=18 : 28.87

#Decision : statistique de test < valeur th, on accepte H0, il n'y a pas d'heteroscédasticité

#Correction de white :
#install.packages("lmtest")
#install.packages("sandwich")
library(lmtest)
library(sandwich)
coeftest(reg2,vcov=vcovHC)
#vcovHC=

summary(reg2)

### 5. la variable educ peut elle etre endogène lorsqu'elle est corrélée au résidu
#La variable educ est suceptible d'etre corrélée au résidu (endogène)
#car elle est corrélée a des facteurs explicatifs inobservés (ex : motivation, habileté individuelle)
#Consequences : estimateurs biaisés, et inéficaces.

### 6.Definition d'un instrument
#C'est une varable qui est corrélée avec la variable explicative endogène, 
#mais qui n'es pas corrélée à la variable expliquée, elle est independente du résidu.

#Test de hausman :
#H0 : exogénéité vs H1: endogénéité
reg3 <- lm(educ~dist,data = mydata2)
summary(reg3)

mydata2$educp <- reg3$fitted.values
attach(mydata2)
mydata2$h <-educ - educp
reg4 <- lm(lnsal ~ age+I(age^2)+educ+noir+urbain+sud+I(noir*sud)+h,data=mydata2)
summary(reg4)
#Tests student sur le coef de h => ce coeff n'est pas significatif
#=> On ne peut pas rejetter H0 du test Hausman => la variable educ est exogène

### 8. Interpretation des differents effets

    #VARIABLE URBAIN :
      
#1/ Significativité => test de Student sur le coef de urbain
  #Hypothèses: H0: b6=0 vs b6!= 0
  #Statistique de test :  t = 9.784
  #Valeur th : t*=1.96
  #Decision : on rejète H0 car t>t* => Il y a un effet significatif

#1/ Significativité => test de Student sur le coef de urbaineffet positif car coeffcient positif.

#3/ Ampleur de l'effet : Le fait d'habiter en ville permet un 
  #salaire plus élévé que les autre individus, d'environ 15,8% ceteris paribus.
  #Valeur precise
  #(exp(0.1586)-1)*100 = 17,19%.

    #VARIABLE NOIR:

#1/ Significativité => test de Student sur le coef de noir
  #Hypothèses: H0: b4=0 vs b4!= 0
  #Statistique de test : p-value : 0.000165 < 0.05
  #Decision : On rejète H0, le coeff de noir est significatif : les affroaméricans gagnent 
    #moins que les autres dans les états du nord ceteris paribus.

#3/ Sens de l'effet : effet négatif car coefficient négatif.

#3/ Ampleur de l'effet : en moyenne et tt chose égale par ailleurs
  #dans les états du nord les affroaméricains ont un salaire 
  #inférieur à celui des autres individus de 
  #(exp(-0.1105468)-1)*100 = 10.47%

#NB : coef du terme d'intéraction noir*sud = differentiel de penalité
    #salariale des affroaméricains entre les états du sud et du nord
  #-> la penalite salariale des afro dans les états du sud est plus importante dans les états du nord
  # = 0.11 - 0.14 = -0.25


#### TD3 ####

library(readxl)
mydata3 <- read_excel("C:/Users/kater/Downloads/donnees_sal2.xls")

attach(mydata3)
mydata3$n__indiv <- NULL

### 1. Analyse statistique de salaire

library(psych)
describe(mydata3)

#Echantillon cylindré : oui, car n = 1412 pour toutes les variables
#moyenne / écartype du salaire : salaire moyen = 445.78, écartype= 179 peu de dispersion dans le salaire
#skewness / kurtosis du salaire : skewness = 1.85 => distribution asymétrique vers la droite
  #kurtosis :  7.3 plus pointue que la loi normale : très forte concentration autour de la moyenne.
# % Hommes, % diplomés : moyenne de la dummy homme : 61%, 61% d'hommes dans l'échantillon, femmes sous-representées.
 #23% des individus sont diplommés de l'enseignement supérieur (dummy: educ 5)

#Densité salaire
d <- density(salaire)
plot(d)

#average salaire by homme
mean <- tapply(salaire, homme, mean)
mean
#Les hommes ont un salaire moyen de 466,4, 
#tandis que les femmes, un salaire moyen de 413,9

#t-test H0: µ0=µ1
#t.test(salaire~ homme, alternative("less"), data=mydata3) # unilateral test avec H1

#average salaire by educ
mean2 <- tapply(salaire, educ, mean)
mean2
#On supconne que le rendement marginal de l'éductation est croissant.

#plot salaire by exper
plot(salaire~exper, data=mydata3)
#En debut de carrière pas de dispersion / fin de carrière : plus forte dispersion

### 3. regression

model1 <- lm(salaire~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5, data=mydata3)
summary(model1)

#test de significativité globale :
#H0 : tous les coeffs : nulls     vs     H1 : au moins un non null.
#p-value < 0.05 donc oui !

#Tests de ramsey : modèle bien éspécifié?
mydata3$salairerep <- model1$fitted.values

model2 <- lm(salaire~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5 +I(salairerep^2), data=mydata3)
summary(model2)
# tests de student sur salaire²
#Decision : |t| >t*  On rejète H0 : donc modèle mal spécifié.

###Quelle est la source de la mauvaise spécification ?

# 2 test test de forme fonctionnelle et test de non-omission
model3 <- lm(log(salaire)~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5, data=mydata3)
summary(model3)

##Test de forme fonctionnelle
mydata3$lsalairerep <- model3$fitted.values
mydata3$diff <- log(mydata3$lsalairerep) - mydata3$lsalairerep

model4 <- lm(log(salaire)~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5 +diff, data=mydata3)
summary(model4)
#On laisse le modèle 

##Tests de non omission (j-test)
#Introduction de la valeur prédite
model5 <- lm(log(salaire)~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5 + lsalairerep, data=mydata3)
summary (model5)

#NA par tout, on ne peut pas conclure sur le test de non-omission

### 7. MULTICOLINEARITE -> tests de Klein

datacor <-subset(mydata3,select=c(exper,homme,educ1,educ2,educ3,educ4,educ5))
cor(datacor)
summary(model3)  #R² du modèle log-linéaire = 0,3995
#Pas de risque de multicolinéairté : car aucun r est > R²

### 8. Tester l'hétérocedasticité

library(lmtest)
library(sandwich)
coeftest(model3,vcov=vcovHC) #avec correction de white : on compare que les écartypes
summary(model3)
#Difference très marquée sur les écartypes entre sans et avec la correction de white
# -> Il y avait donc de l'hetéroscédasticité dans le modèle loglinéaire (model3)

### 9. Quel est l'effet de l'eductation sur le salaire

#1) Significativité des coefs : tous significatifs avec tests de student classiques
#2) Sens des effets : effets positifs ceteris paribus car tous les t positifs
    #-> Individus avec un niveau d'éduc >1 sont mieux payés que les individus avec educ =1ceteris paribus.
#3) Ampleur des effets : 
#Pour educ2 : les individus qui on attent le cycle du secondaire ont 14% de salaire que ceux qui se sont arrtés avant, ceteris paribus.
#effet précis educ2: +15.21%
(exp(0.14163)-1)*100
#effet précis educ3 :+36.17%
(exp(0.30874)-1)*100
#effet précis educ4 :+61.8%
(exp(0.48123)-1)*100
#effet précis educ5 :+89.78%
(exp(0.64072)-1)*100
#-> effet non-linéaire, car les differentes entre les niveaux sont differentes.
#La diff de salaire est + importante entre 4 et 5, que pour 3 et 4...
#L'efffet sur le salaire est de plus en plus important.

coeftest(model3,vcov=vcovHC)

### 10. Interpretation de l'effet de l'expérience 

#1) Significativité du coeff de'exper² -> effet linéaire ou non linéaire
#T-student sur b2 : H0 b2 = 0 vs. H1: b2!=0
#Stat de test : t-value = -5.3962
#Valeur th : risque de 5% et ddl = n-k : t* = 1.96
#Decision : 5.3962 > 1.96 -> b2 est significatif : effet non-linéaire de l'expérience sur le salaire ceteris paribus.
#p-value : 0<0.05 -> b1 est significatif
#b1 > 0 donc l'expérience a un effet positif non-linéaire sur le salaire ceteris paribus.

#3) Signe du coeff d'exper² -> nature des rendements marginaux
#b2 < 0 => rendements marginaux décroissants
#l'expérience a un effet positif non-linéaire sur le salaire avec des rendements marginaux décroissants

### 11. Tests de significativité d'un ens de coefficients

#modèle complet : R² = 0.3995
summary(model3)
#modèle réduit :
model6 <- lm(log(salaire)~ exper + I(exper^2)+ homme, data=mydata3)
summary (model6)

#Stat de test : 165.05
((0.3995-0.1287)/(1-0.3995))*(1472-8)/4
# Valeur th : 2.37
#Decision : F > valeur th : on rejète H0 -> l'ensemble de coefficient sont significatifs
#On prefère le modèle complet au modèle reduit

### 12. Test d'homogénéité des coefficents : stabilité entre sous pop?

#modèle echantillon complet :
summary(model3)

#modèle sur l'écantillon A : femmes
model7 <- lm(log(salaire)~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5, subset=mydata3$homme==0)
summary(model7)

#modèle sur l'écantillon A : hommes
model8 <- lm(log(salaire)~ exper + I(exper^2)+ homme + educ2 + educ3 + educ4 + educ5, subset=mydata3$homme==1)
summary(model8)

#SCR echantillon complet = 116,1
sum(model3$residuals^2)

#SCR echantillon A = 44.34
sum(model7$residuals^2)

#SCR echantillon B = 70.28
sum(model8$residuals^2)

#Stat de test : 
((116.1-(44.34+70.28))/8)/((44.34+70.28)/(1472-8*2))
# Valeur th : = 1.94
#Décision : on rejette H0, les determinants de salaires sont differents entre les femmes (A) et les hommes(B)
#dans la pop.

