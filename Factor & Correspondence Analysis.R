##Tema2-Lica_Amalia


date<-Date_AD
#statistici descriptive
summary(date)


#2. Curățați setul de date, redenumiți variabilele astfel încât să fie clar de identificat și
#gestionați valorile lipsă și outlierii.

# Redenumirea coloanelor pentru claritate
colnames(date) <- c("Judete", "Agricultura_Vanatoare", "Pescuit", 
                    "Energie_Gaze_Apa", "Constructii", 
                    "Comert", "Hoteluri_Restaurante", "Transport_Depozitare", 
                    "Posta", "Educatie", 
                    "Admin_publica_Aparare", "Sanatate")
View(date)
#outlieri
windows()
par(mfrow=c(2,6))
boxplot(date$Agricultura_Vanatoare, col = "lightblue", main="Boxplot Agricultura_Vanatoare")

boxplot(date$Pescuit, col = "darkgreen", main="Boxplot Pescuit")

boxplot(date$Energie_Gaze_Apa, col = "red", main="Boxplot Energie_Gaze_Apa")

boxplot(date$Constructii, col = "pink", main="Boxplot Constructii")

boxplot(date$Comert, col = "darkblue", main="Boxplot Comert")

boxplot(date$Hoteluri_Restaurante, col = "purple", main="Boxplot Hoteluri_Restaurante")

boxplot(date$Transport_Depozitare, col = "yellow", main="Boxplot Transport_Depozitare")

boxplot(date$Posta, col = "orange", main="Boxplot Posta")

boxplot(date$Educatie, col = "darkred", main="Boxplot Educatie")

boxplot(date$Admin_publica_Aparare, col = "beige", main="Boxplot Admin_publica_Aparare")

boxplot(date$Sanatate, col = "pink3", main="Boxplot Sanatate")



#outlieri
boxplot(date$Agricultura_Vanatoare, plot=FALSE)$out
boxplot(date$Pescuit, plot=FALSE)$out
boxplot(date$Energie_Gaze_Apa, plot=FALSE)$out
boxplot(date$Constructii, plot=FALSE)$out
boxplot(date$Comert, plot=FALSE)$out
boxplot(date$Hoteluri_Restaurante, plot=FALSE)$out
boxplot(date$Transport_Depozitare, plot=FALSE)$out
boxplot(date$Posta, plot=FALSE)$out
boxplot(date$Educatie, plot=FALSE)$out
boxplot(date$Admin_publica_Aparare, plot=FALSE)$out
boxplot(date$Sanatate, plot=FALSE)$out

date_noi <- date

# Gestionarea valorilor lipsă: Înlocuirea cu medianele coloanelor respective
for (col in colnames(date_noi)[-1]) {  # Exclude prima coloană (Judete)
  date_noi[[col]][is.na(date_noi[[col]])] <- median(date_noi[[col]], na.rm = TRUE)
}

# Înlocuirea outlierilor cu medianele
for (col in colnames(date_noi)[-1]) {  # Exclude prima coloană (Judete)
  Q1 <- quantile(date_noi[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(date_noi[[col]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  median_value <- median(date_noi[[col]], na.rm = TRUE)
  
  date_noi[[col]] <- ifelse(date_noi[[col]] < lower_bound | date_noi[[col]] > upper_bound, 
                            median_value, 
                            date_noi[[col]])
}
View(date_noi)


#3. Descrieți în cuvinte setul de date, din perspectiva variabilelor (coloanelor) și
#observațiilor (liniilor). Enunțați clar sursa datelor și perioada de timp la care se referă.-in PDF


#4. Descrieți obiectivul general al analizei voastre.-in PDF

#5. Calculați indicatorii statistici și interpretați din punct de vedere economic

install.packages("psych")
library(psych)
describe(date_noi[-1])

#6. Calculați matricea de corelație și covarianță și interpretați din punct de vedere
#economic. Reprezentați grafic matricea de corelație.

#Standardizare date
date_std = scale(date_noi[-1], scale=T)
View(date_std)

install.packages("raster")
library(raster)

round(apply(date_std,2,mean),5) #medie=0
apply(date_std,2,sd) #sd=1

#matrice de corelație
matrice_corelatie <- cor(date_std)
View(matrice_corelatie)

# Reprezentare grafică a matricei de corelație
install.packages("corrplot")
library(corrplot)
corrplot(matrice_corelatie, method = "number", type = "lower")

#matrice de covarianță
matrice_covarianta <- cov(date_std)
View(matrice_covarianta)


#7.Realizați cel puțin trei tipuri de reprezentări grafice care ajută în descrierea
#statistică a datelor.

#a.
install.packages("ggpubr")
library(ggpubr)

ggscatter(date_noi, x = "Constructii", y = "Transport_Depozitare", 
          add = "reg.line",          
          conf.int = TRUE,           
          cor.coef = TRUE,           
          cor.method = "pearson",    
          xlab = "Constructii", 
          ylab = "Transport și Depozitare", 
          color = "blue")   

#b.
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(date_noi[-1], hist = TRUE)

#c.
install.packages("ggplot2")
library(ggplot2)
ggplot(date_noi, aes(x = Educatie, y = Sanatate)) +
  geom_point() +
  geom_text(aes(label = Judete), 
            color = "purple", 
            nudge_x = 0.25, 
            nudge_y = 0.25, 
            check_overlap = TRUE)

#8. Evaluați factorabilitatea setului de date. Calculați indicele KMO și testul Bartlett pentru
#a decide dacă se poate parcurge analiza factorială
install.packages("psych")
library(psych)

KMO(date_std)

R=cor(date_std)
cortest.bartlett(R, n=42, diag=TRUE)

#9. Aplicați criteriile/metodele relevante pentru alegerea numărului de factori din analiză
#și generați o concluzie cu privire la numărul optim de factori.

# 1. Criteriul grafic (Scree Plot)
# Calcularea valorilor proprii utilizând analiza componentelor principale
screeplot_result <- prcomp(date_std)
eigenvalues <- screeplot_result$sdev^2 # Valori proprii

# Realizarea graficului Scree Plot
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Componenta", ylab = "Valori proprii", col = "magenta")
abline(h = 1, col = "red", lty = 2) # Linie orizontală pentru criteriul lui Kaiser

# Interpretare Scree Plot:
# Observăm punctul de cotitură, unde panta începe să se aplatizeze (aproape de zero).
# Alegem numărul de componente înainte de această cotitură.

# 2. Criteriul lui Kaiser
componente_kaiser <- sum(eigenvalues >= 1) # valorile proprii >= 1
print(paste("Numărul de factori conform criteriului lui Kaiser:", componente_kaiser))

# Interpretare Criteriul lui Kaiser:
# Componenta trebuie să aibă valori proprii >= 1 pentru a fi păstrată.

# 3. Criteriul procentului de varianță cumulată
var_cumulativa <- cumsum(eigenvalues) / sum(eigenvalues) * 100 # Varianță cumulată în procente
print(var_cumulativa)

# Determinarea numărului minim de componente pentru a explica 70-80% din variabilitate
componente_var_cumulativa <- which(var_cumulativa >= 80)[1]
print(paste("Numărul de factori conform criteriului procentului de varianță cumulată:", componente_var_cumulativa))



#10. Aplicați două metode pentru estimarea modelului factorial și analizați care este mai
#potrivit. Interpretați output-urile.


install.packages("GPArotation")
library(GPArotation)
library(psych)

factor1=fa(date_std, nfactors = 3, rotate="none", fm="pa")

print(factor1$loadings, cutoff=0.4)
#Vrem sa vedem corelatii peste 0.4



#2.METODA VEROSIMILITATII MAXIME
factor2=fa(date_std, nfactors=3, rotate="none", fm="ml")

print(factor2$loadings, cutoff=0.4)


#11. Realizați diagramele corespunzătoare celor două modele și interpretați rezultatele. 

fa.diagram(factor1)


fa.diagram(factor2)


#12.Denumiți factorii, interpretați rezultatele și salvați noul set de date.
scoruri_PA <- factor1$scores  # Scorurile factoriale pentru metoda PA
colnames(scoruri_PA) <- c("Dimensiunea_socio_economica_generala", 
                          "Utilitati_publice_administratie", 
                          "Redundanta_PA")  # Redenumirea factorilor

date_noi_PA <- cbind(date_noi, scoruri_PA)  # Combinarea cu setul de date original curățat

# Salvarea setului de date cu scoruri factoriale pentru metoda PA
write.csv(date_noi_PA, "Date_Factoriale_PA.csv", row.names = FALSE)


# Adăugarea scorurilor factoriale pentru metoda ML
scoruri_ML <- factor2$scores  # Scorurile factoriale pentru metoda ML
colnames(scoruri_ML) <- c("Dimensiunea_socio_economica_generala", 
                          "Infrastructura_si_logistica", 
                          "Redundanta_ML")  # Redenumirea factorilor

date_noi_ML <- cbind(date_noi, scoruri_ML)  # Combinarea cu setul de date original curățat

# Salvarea setului de date cu scoruri factoriale pentru metoda ML
write.csv(date_noi_ML, "Date_Factoriale_ML.csv", row.names = FALSE)

#13.Realizați cel puțin 3 tipuri de reprezentări grafice pentru analiza factorială (similar cu
#ACP - biplot, scatterplot-ul observațiilor, etc.)

#1)scatterplot
scoruri <- as.data.frame(factor1$scores) 

library(ggplot2)
ggplot(scoruri, aes(x = PA1, y = PA2)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot al observațiilor pe factori", x = "Factor 1", y = "Factor 2") +
  theme_minimal()

#2) histograme
# Histogramă pentru Factorul 1
hist(scoruri$PA1, 
     freq = FALSE, 
     col = "darkblue",  
     main = "Histograma scorurilor factoriale (Factor 1)", 
     xlab = "Scoruri Factor 1") 

lines(density(scoruri$PA1), 
      lwd = 3,  
      col = "red")

# Histogramă pentru Factorul 2
hist(scoruri$PA2, 
     freq = FALSE, 
     col = "magenta",  
     main = "Histograma scorurilor factoriale (Factor 2)", 
     xlab = "Scoruri Factor 2") 

lines(density(scoruri$PA1), 
      lwd = 3,  
      col = "black")
#3)corrplot
cor_factori <- cor(scoruri)

# Corrplot simplificat cu numere
library(corrplot)
corrplot(cor_factori, 
         method = "number", 
         type = "upper",    
         main = "Corelațiile dintre factori") 



#------------ANALIZA CORESPONDENTELOR

date_ac<-Date_AC_tema
date2=date_ac[,-1]
date2=as.table(as.matrix(date2))
rownames(date2)=date_ac$Populatie_Categorii_varsta

View(date2)

#3)Reprezentați grafic tabelul de contingență (de exemplu prin balloonplot).
install.packages("gplots")
library(gplots)

balloonplot(t(date2),main="Matricea de contingenta",)
#4. Aplicați testul Chi-Pătrat (inclusiv ipotezele) și interpretați rezultatele.

#Testul de independenta chi^2
#-ipoteza h0 - nu exista asocieri intre liniile si coloanele matricei
#-ipoteza h1 - exista asocieri intre liniile si coloanele matricei

X2=chisq.test(date2)
X2

#5.Aplicați metoda analizei de corespondență. Calculați și afișați inerția totală, valorile
#proprii și procentele de varianță.
#valoarea observata:
a11=date2[1,1]
a11 

row_totals=rowSums(date2) #totaluri pe linii
col_totals=colSums(date2) #totaluri pe coloane

#valoarea asteptata:
e11=(row_totals[1]*col_totals[1])/sum(date2)
e11 

X2$expected
X2$observed

#Rezultatele analizei corespondentelor
install.packages("FactoMineR")
library(FactoMineR)

install.packages("factoextra")
library(factoextra)
rez=CA(date2, graph=F)
rez

eig=get_eigenvalue(rez)
eig
#inertia totala
s=sum(eig[,1])
s 

summary(rez, nb.dec=2)

#6. Realizați Scree Plot-ul și interpretați.
#Reprezentari grafice:
#Contributiile in procente ale fiecarei coloane 
rez$col

fviz_screeplot(rez)

#7.Extrageți și interpretați indicatorii pentru dimensiunea linie (cos2, contrib, coord, inerția).
rezultate_linii <- get_ca_row(rez)

# Afișăm coordonatele liniilor pe dimensiuni
print(rezultate_linii$coord)

# Afișăm calitatea reprezentării (cos²) pentru fiecare linie
print(rezultate_linii$cos2)

# Afișăm contribuția fiecărei linii la inerția totală
print(rezultate_linii$contrib)


#8. Realizați matricea factor pentru rânduri și interpretați.

install.packages("FactoMineR")
library(FactoMineR)

# Matricea factor pentru rânduri
matrice_factori_rânduri <- rez$row$coord

# Afișăm matricea
print(matrice_factori_rânduri)

install.packages("factoextra")
library(factoextra)

# Graficul rândurilor pe dimensiunile factorilor
fviz_ca_row(rez, repel = TRUE)

#9. Extrageți și interpretați indicatorii pentru coloane (cos2, contrib, coord, inerția).
rezultate_coloane <- get_ca_col(rez)

# Afișăm coordonatele coloanelor pe dimensiuni
print(rezultate_coloane$coord)

# Afișăm calitatea reprezentării (cos²) pentru fiecare coloană
print(rezultate_coloane$cos2)

# Afișăm contribuția fiecărei coloane la inerția totală
print(rezultate_coloane$contrib)

#10.Realizați matricea factor pentru coloane și interpretați.

matrice_factori_coloane <- rez$col$coord
print(matrice_factori_coloane)

install.packages("factoextra")
library(factoextra)

# Graficul coloanelor pe dimensiunile factorilor
fviz_ca_col(rez, repel = TRUE)


#Realizați cel puțin 3 reprezentări grafice (similar cu ACP - biploturi, reprezentări ale
#contrib/cos2/coord/inerția pe rânduri/coloane, etc).
#a) Biplot
fviz_ca_biplot(rez, map = "row principal", arrow = c(T,T), repel = TRUE)

#b) Grafic al contribuțiilor (corrplot)
install.packages("corrplot")
library(corrplot)
corrplot(rez$col$contrib, is.corr = FALSE, main = "Contribuția coloanelor")
corrplot(rez$row$contrib, is.corr = FALSE, main = "Contribuția liniilor")

# c) Calitatea reprezentării (cos²) pentru linii și coloane
fviz_cos2(rez, choice = "row", axes = 1:2) 
fviz_cos2(rez, choice = "col", axes = 1:2) 