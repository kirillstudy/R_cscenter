#Лекция 2
#Элементы программирования в R
#Описательные статистики

#Определение подлинности банкноты набора данных Swiss Bank Notes
install.packages("mclust") #Пакет с набором данных Swiss Bank Notes
library(mclust)
data("banknote")
head(banknote)
str(banknote)

#Описательные статистики
library(dplyr)
dim(banknote)            #Размерность
nrow(banknote)           #Количество строк
ncol(banknote)           #Количество столбцов
str(banknote)            #Структура датасета
dplyr::glimpse(banknote) #Структура датасета

summary(banknote)        #Описательные статистики переменных

hist(banknote$Diagonal, breaks = 18, probability = T)
plot(banknote$Diagonal, banknote$Right)
plot(banknote)
pairs(banknote)

library(ggplot2)
ggplot(banknote, aes(x = Right, y = Diagonal, col = Status))+
  geom_point()

#Выбор только четных значений
my_vector = c(3, 2, 1, 5, 2, 4, 7, 5, 8)
my_vector[my_vector %% 2 == 0]

#Логические операторы
my_vector[my_vector > 2 & my_vector < 8]
my_vector[my_vector < 1 | my_vector > 7]

#Создание матрицы
my_matrix1 <- c(1:12)
my_matrix2 <- c(1:12)
dim(my_matrix1) <- c(3, 4)
my_matrix2 <- matrix(data = my_matrix2, nrow = 3, ncol = 4)


