# sieci uczone i samouczące sie

#Samouczące się
#SOM
library(ggplot2)

#PRZYGOTOAWANIE DANYCH
set.seed(123)
n <- 100 #wybieramy liczbe punktów ile chcemy miec

# Klasa 0
x1_0 <- rnorm(n, mean = 0)
x2_0 <- rnorm(n, mean = 0)

# Klasa 1
x1_1 <- rnorm(n, mean = 3)
x2_1 <- rnorm(n, mean = 3)

# Dane
X <- rbind(
  cbind(x1_0, x2_0),
  cbind(x1_1, x2_1)
) #łaczymy kolumny i wiersze 

y <- c(rep(0, n), rep(1, n))

data <- data.frame(x1 = X[,1], x2 = X[,2], y = y)

data$y <- as.factor(data$y)

#WIZUALIZACJA DANYCH
ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Klasa 0", "Klasa 1")) +
  labs(
    title = "Dane do klasyfikacji",
    x = "x1",
    y = "x2",
    color = "Klasa"
  ) +
  theme_minimal()


#tworzymy perceptron, (ale mozna zrobic prostą regresją liniową)
#TWORZENIE FUNKCJI AKTYWACJI
# KLASYCZNY PERCEPTRON FUNKCJA PROGOWA
activation <- function(z) {
  ifelse(z >= 0, 1, 0)
}

#PRZYGOTOWANIE PARAMTERÓW: INICJALIZACJA WAG, LICZBY EPOK I KROKU UCZENIA
set.seed(123)
# runif(n, min, max)
w1 <- runif(1, -1, 1)
w2 <- runif(1, -1, 1) #kazda waga do kazdej zmiennej, a mamy 2 zmienne
b  <- runif(1, -1, 1) #bias

learning_rate <- 0.1 #co jaki kawałek sie przybliza do wartości rzeczywistych
epochs <- 20

data$y <- as.integer(y)

#FUNKCJA PREDYCKJI
predict_perceptron <- function(x1, x2, w1, w2, b) {
  z <- w1 * x1 + w2 * x2 + b
  activation(z)
}

#UCZENIE PERCEPTRONU
errors_history <- c()

for (epoch in 1:epochs) {
  
  total_error <- 0
  
  for (i in 1:nrow(data)) {
    
    x1_i <- data$x1[i]
    x2_i <- data$x2[i]
    y_true <- data$y[i]
    
    # Predykcja
    y_pred <- predict_perceptron(x1_i, x2_i, w1, w2, b)
    
    # Błąd
    error <- y_true - y_pred
    
    # Aktualizacja wag (KLUCZ!)
    w1 <- w1 + learning_rate * error * x1_i
    w2 <- w2 + learning_rate * error * x2_i
    b  <- b  + learning_rate * error
    
    total_error <- total_error + abs(error)
  }
  
  errors_history <- c(errors_history, total_error)
  
  cat("Epoka:", epoch, "Błąd:", total_error, "/n")
}
#'Epoka: 5 Błąd: 4' pokazuje blad uczenia w iterakcji

#BŁAD UCZENIA
df_errors <- data.frame(
  epoch = 1:length(errors_history),
  error = errors_history
)

ggplot(df_errors, aes(x = epoch, y = error)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, max(df_errors$error))) + 
  labs(
    title = "Błąd w kolejnych epokach",
    x = "Epoka",
    y = "Błąd"
  ) +
  theme_minimal()
#pokazuje czy sieć sie dalej uczy
#najlepiej jak wykres uczenia sie jest ciagły..


#WIZUALIZACJA WYNIKU
data$y <- as.factor(data$y)
# granica decyzji 
# w1*x1 + w2*x2 + b = 0

ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2) +
  
  # granica decyzyjna
  geom_abline(
    intercept = -b / w2,
    slope = -w1 / w2,
    color = "darkgrey",
    linewidth = 1
  ) +
  
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Klasa 0", "Klasa 1")
  ) +
  
  labs(
    title = "Perceptron - granica decyzyjna",
    x = "x1",
    y = "x2",
    color = "Klasa"
  ) +
  
  theme_minimal()

#wynik jest ok ale mozna ulepszyc
#iteracje -> wiecej
#learning rate -> mniejsze
#czy perceptron zakwalifukuje 2 czerwone kulki do niebieskich? czy 1 niebieską do czerwonych? to zalezy jak bardzo dokładnie chcemy zdefiniować którą grupę!



#perceptron to jeden neuron.


#Samoucząca się sieć neuronowa - SOM....................................................................
#do czego wykorzystywane? do klasyfikacji.

install.packages("kohonen")
install.packages("clusterSim")

#Biblioteki
library(kohonen)
library(ggplot2)
library(clusterSim)
library(dplyr)



#Zbiór danych
#Wczytanie danych

dane <- read.csv("C:/Users/AleksandraDobrowolsk/OneDrive - University of Gdansk (for Students)/Pulpit/Zajecia-Pulpit/Sieci neuronowe R/uzytkownicy_portalu_spolecznosciowego.csv") %>%
  glimpse()


dane=as.data.frame(dane)
dane


## Rows: 5,000
## Columns: 5
## $ user_id         <chr> "U001", "U002", "U003", "U004", "U005", "U006", "U007"…
## $ czas_na_stronie <dbl> 4.2172863, 2.8830514, 6.6452743, 0.1578868, 0.2810549,…
## $ liczba_klikniec <int> 8, 12, 12, 8, 14, 10, 7, 10, 6, 7, 10, 2, 8, 7, 11, 10…
## $ liczba_sesji    <int> 2, 6, 9, 5, 3, 6, 4, 7, 7, 4, 3, 4, 5, 4, 3, 1, 6, 4, …
## $ konwersja       <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, …
dane=as.data.frame(dane)


#Podział na train/test .... może byc jeszcze trzeci - walidacyjny, żeby potwierdzic ponownie (np instytucje pod KNF maja taki obowiazek) - ktoś z zewnatrz moze sprawdzic
set.seed(123)

# indeksy
train_idx <- sample(1:nrow(dane), size = 0.7 * nrow(dane))


train_raw <- dane[train_idx, ]
test_raw  <- dane[-train_idx, ]

train_scaled <- scale(train_raw[, -1])  # usuń tylko ID
test_scaled  <- scale(test_raw[, -1])
#scale - skaluje (normalizuje) bo dane są w rożnych jednostkach.  

#Budowa sieci:

#Struktura sieci ma paramatry, które musimy określic:
#Liczba neuronów

#Kształt sieci:
#-arkusz
#-cylinder
#-toroid

#Topologia sieci:
#-prostokątna - “rectangular”
#-heksagonalna - “hexagonal”

set.seed(100) #st seed bo losowo wybieramy punkt startu (set seed zebysmy mieli takie same wyniki)
ads.grid <- somgrid(xdim = 5, ydim = 5, topo = "rectangular")


#Utworzenie modelu SOM
set.seed(100)
ads.model <- som(train_scaled, ads.grid, rlen = 500, radius = 2, keep.data = TRUE,
                 dist.fcts = "euclidean") #rlen - jak długo bedziemy uczyc sieć, radious - o 2 neurony może przeskoczyc w trakcie uczenia, dist.fcts - odległosci liczone euklidesem

str(ads.model)
#nauczylismy siec!

head(ads.model$unit.classif, 10 ) # -> do kórego z neuronów trafił klient..
table(ads.model$unit.classif) # 1 neuron reprezentuje 19 klientów, drugi 87 klientów...
# nie ma 18go neuronu..
ads.model$grid

#wizualizacja sieci:
plot(ads.model, type = "mapping", pchs = 19, shape = "straight")
#mamy 25 'okienek' czyli 25 neuronów.


#jeśli w sieci SOM 5x5 1 mamy martwy neuron (jak ten 18) to ok, jak w sieci 5x5 mamy 6 martwych.. to moze siec jest za duza i trzeba zmienic rozmiar
# jesli duuuze skupisko w 1 neuronie to tez do zmiany
#wymiar albo liczba iterakcji

plot(ads.model, type = "codes", main = "Codes Plot", palette.name = rainbow)
#kazdy neuron to reprezentacja okreslonej charakterystyki klienta
#1 neuron - klienci którzy spadzaja na stronie..

#18 neuron - w naszym zbiorze nie było takich klientów ale neuron 18 reprezentuje zestaw cech.

plot(ads.model, type = "changes") #kroki uczenia -> sieć miała 1 przeskok..

plot(ads.model, type = "counts")
#pokazuje liczebnosc

plot(ads.model, type = "dist.neighbours")
#dystans do neuronów obok. 1.neuron jest jasny wiec to znaczy zejest dośc daleko pozostałych, te w tych samycho kolorach są blisko, np drugi jest blisko 25ego


heatmap.som <- function(model){
  for (i in 1:4) {
    plot(model, type = "property", property = getCodes(model)[,i], 
         main = colnames(getCodes(model))[i]) 
  }
}
par(mfrow=c(2,2))
heatmap.som(ads.model)
#im wiekszy udział danej zmiennej tym jest jaśniejszy

#mozna zobaczyc korelację -> układ kolorów jeśli jest podobny to korelacja jest wieksza
# dane kolorystyczne tak samo pokazują 2 zmienne



### Sprawdzenie na zbiorze testowym

test_mapping <- predict(ads.model, newdata = test_scaled)
# przypisanie klastrów(skupień)
grupy <- kmeans(ads.model$codes[[1]], centers = 3) #wybieramy z epodziałna 3 grupy

train_raw$grupa <- grupy$cluster[ads.model$unit.classif]
test_raw$grupa  <- grupy$cluster[test_mapping$unit.classif]

# porównanie
aggregate(konwersja ~ grupa, data = train_raw, mean)
aggregate(konwersja ~ grupa, data = test_raw, mean) 
#sprawdzenie modelu pokazało że wynik modelu jest bardzo zły -> nie moze wyjśc w punkt! tak samo wynik grupy treningowej i testowej
# róznice są duze. klienci którzy wpadają do 1 grupy zawsze klikna w reklamę, w drugiej zaden nigdy a w trzeciej 

#co sie stało?

par(mfrow = c(1,2))

plot(ads.model, type = "mapping",
     main = "Train")

plot(ads.model, type = "mapping",
     bgcol = test_mapping$unit.classif,
     main = "Test (projekcja)")

# w zbiorze testowym nie mamy klientów którzy pasują do neuronu 1 i 3.. - sa puste


#Przyczyny:
#liczebnośc????????????????????????????????????????????????????????????????????????????????????????????????????????
#zmieniamy rozmiar:

set.seed(100)
ads1.grid <- somgrid(xdim = 7, ydim = 7, topo = "hexagonal")

set.seed(100)
ads1.model <- som(train_scaled, ads1.grid, rlen = 500, radius = 2, keep.data = TRUE,
                 dist.fcts = "euclidean")
str(ads1.model)

plot(ads1.model, type = "mapping", pchs = 19, shape = "round")


#test:
test_mapping1 <- predict(ads1.model, newdata = test_scaled)
# przypisanie klastrów(skupień)
grupy <- kmeans(ads1.model$codes[[1]], centers = 3) #wybieramy z epodziałna 3 grupy

train_raw$grupa <- grupy$cluster[ads1.model$unit.classif]
test_raw$grupa  <- grupy$cluster[test_mapping1$unit.classif]

# porównanie
aggregate(konwersja ~ grupa, data = train_raw, mean)
aggregate(konwersja ~ grupa, data = test_raw, mean) 


# wyniki konwersji wyszły lepsze!!! z tego wynika ze zwiekszenie liczby neuronów poprawiło
# ale mamy wiecej martwych...duzo.
# 
#zmiana topografii...też pomaga! neurony sie inaczej ukłądają.
#a elementy siatki są lepiej połączone. hexagonalny ma wiecej połączeń niż kółkowy


par(mfrow = c(1,2))

plot(ads1.model, type = "mapping",
     main = "Train")

plot(ads1.model, type = "mapping",
     bgcol = test_mapping$unit.classif,
     main = "Test (projekcja)")

#...............................................................................................................





par(mfrow = c(1,2))

plot(ads.model, type = "mapping",
     main = "Train")

plot(ads.model, type = "mapping",
     bgcol = test_mapping$unit.classif,
     main = "Test (projekcja)")


plot(ads.model, type = "dist.neighbours")
#mamy tu dystanse, odległosci miedzy neuronami --> analiza skupień!


#dendrogram:
neurony<-as.data.frame( ads.model$codes)
dd = (dist(neurony, method = "euclidean"))
fitc <- hclust(dd, method="ward.D")


par(mfrow = c(1,1))# wracamy do 1 okna z wykresem..
plot(fitc, hang=-1)
#na dendrogramie - jednostką jest neuron! - chcemy to uogólnic i podzielic na skupienia

#odcinanie:
#reguła mojeny
#wizualnie..;)


source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
par(mfrow=c(1,1))

wys<-c(0,fitc$height)
Mojena1<-mean(wys)+1.25* sd(wys)
Mojena1#wychodzi 5 ;)

A2Rplot(fitc, k = 6, boxes = TRUE, col.up = "gray50", 
        col.down = c("#FF6B6B","#8470FF","green4","#66CDAA","#8B7E66","grey"), main="Klasyfikacja" )
groupes <- cutree(fitc,k=6)
table(groupes)
plot(ads.model,type="mapping",bgcol=c("steelblue1","sienna1","yellowgreen","grey","pink")[groupes])
add.cluster.boundaries(ads.model,clustering=groupes)

wnk.korelacje.kofenetyczne <- c()
met.dist <- c( "euclidean")
met.hcl <- c("single", "complete", "average","ward.D","centroid")

for(i in 1:1){ for(j in 1:5){ dds = dist(neurony, method = met.dist[i])
hcdds = hclust((dds), method = met.hcl[j])
hcdds.c = cophenetic(hcdds)
korelacja.kofenetyczna = cor(hcdds.c, dds)
wnk.korelacje.kofenetyczne = c(wnk.korelacje.kofenetyczne, korelacja.kofenetyczna)}}

wnk.korelacje.kofenetyczne = matrix(round( wnk.korelacje.kofenetyczne, 5), ncol = 1)

colnames(wnk.korelacje.kofenetyczne)<-"euclidean"
met.hcl1 <- c("single", "complete", "average","ward.D", "centroid")
rownames(wnk.korelacje.kofenetyczne)<-met.hcl1
wnk.korelacje.kofenetyczne


#...........................................................................................

#sieć nnet - tylko 1 warstwa ukryta
#prosta sieć

#sieci neuronowe swietnie radza sobie w zagadnienieach nieliniowych
# w banku GML - uogolnione modele liniowe do scoringu - bo szybciej i mniej zasobów

#odpowiednik tensorflow w R to torch - opiera sie na Pythonowej PyTorch
#www.torch.mlverse.org
#rpubs.com/Agata_Maj
#https://rpubs.com/Agata_Maj/SNN








