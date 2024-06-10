grzyby <- read.csv("agaricus-lepiota.csv", header = FALSE, fileEncoding="UTF-8", sep=",")

grzyby$V1 = factor(grzyby$V1)
grzyby$V12[which(grzyby$V12 == "?")] = NA #najpierw przeprowadzimy analizę z brakami danych

grzyby = grzyby[sample(1:nrow(grzyby)), ] #porządkujemy dane losowo;



#tworzymy zbiór uczący i testowy
grzyby_train = grzyby[1:2500, ]
grzyby_test = grzyby[2501:8124, ]

#czy zbiory uczący i testowy mają podobne frakcje grzybów trujących?
prop.table(table(grzyby_train$V1))
prop.table(table(grzyby_test$V1))

#tak - można przystąpić do dalszych analiz

install.packages('e1071')
install.packages('gmodels')
install.packages('latex2exp')
library('e1071')
library('gmodels')

#tworzymy model, na razie bez wygładzania Laplace'a

m_1 = naiveBayes(grzyby_train[-1], grzyby_train$V1, laplace = 0)

p_1 = predict(m_1, grzyby_test[-1])

CrossTable(p_1, grzyby_test$V1, prop.chisq = FALSE, dnn=c('Predykcja', 'Stan faktyczny'))
#niestety, aż 320 grzybów trujących zostało uznanych za jadalne!!!
#próbujemy wygładzania Laplace'a dla różnych parametrów

m_2 = naiveBayes(grzyby_train[-1], grzyby_train$V1, laplace = 1)
p_2 = predict(m_2, grzyby_test[-1])
CrossTable(p_2, grzyby_test$V1, prop.chisq = FALSE, dnn=c('Predykcja', 'Stan faktyczny'))
#tym razem 304 - nieznaczna poprawa

m_3 = naiveBayes(grzyby_train[-1], grzyby_train$V1, laplace = 0.5)
p_3 = predict(m_3, grzyby_test[-1])
CrossTable(p_3, grzyby_test$V1, prop.chisq = FALSE, dnn=c('Predykcja', 'Stan faktyczny'))
#teraz 268 - wartość maleje, będziemy nadal zmniejszać parametr


#narysujemy wykres liczby grzybów trujących uznanych za jadalne w zależności od
#wartości parametru l

l = 1:10
trujaki = NULL

for(x in l){
  m = naiveBayes(grzyby_train[-1], grzyby_train$V1, laplace = 10**(-x))
  p = predict(m, grzyby_test[-1])
  trujaki = c(trujaki, CrossTable(p, grzyby_test$V1, prop.chisq = FALSE)$t[3])
}

wyniki = data.frame(l, trujaki)

library('ggplot2')
library('latex2exp')

ggplot(wyniki, aes(x = l, y = trujaki)) + 
  geom_point(size=2, color="#69b3a2") +
  scale_x_continuous(labels=TeX(
    c("$10^{-1}$", "$10^{-2}$", "$10^{-3}$", "$10^{-4}$", "$10^{-5}$", "$10^{-6}$", "$10^{-7}$", "$10^{-8}$", 
      "$10^{-9}$", "$10^{-10}$")), breaks=l)+
  ggtitle("Grzyby trujące uznane za jadalne") +
  xlab("Parametr wygładzania")+
  ylab("Liczba grzybów")+
  theme(
    panel.background = element_rect(fill = "#efffef", color = "#efffef"),
    panel.grid = element_line(color = "#b4aea9"),
    plot.title = element_text(size=14, hjust=0.5),
    axis.text=element_text(size=12)
  )

#wynika stąd, że optymalna wartość to 10^(-6)

m = naiveBayes(grzyby_train[-1], grzyby_train$V1, laplace = 10**(-6))
p = predict(m, grzyby_test[-1])
CrossTable(p, grzyby_test$V1, prop.chisq = FALSE, dnn=c('Predykcja', 'Stan faktyczny'))
