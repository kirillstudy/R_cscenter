#Лекция 1
#Анализ данных на R в примерах и задачах

#Определение подлинности банкноты набора данных Swiss Bank Notes
install.packages("mclust") #Пакет с набором данных Swiss Bank Notes
library(mclust)
data("banknote")
head(banknote)
str(banknote)

#Графики
hist(banknote$Diagonal)

#Создание выборки на обучение и тестирование
train_ind <- sample(seq_len(nrow(banknote)), size = nrow(banknote) * 0.8) #Создание ветора индексов
train_df <- banknote[train_ind,]
test_df <- banknote[-train_ind,]
rm(train_ind)

#Логит и пробит модели
install.packages("lmtest") #Пакет с логит и пробит моделированием
library(lmtest)
m.logit <- glm(Status ~ Length + Left + Right + Bottom + Top,
               data = train_df, family = binomial(link = "logit"))
m.probit <- glm(Status ~ Length + Left + Right + Bottom + Top,
               data = train_df, family = binomial(link = "probit"))

#Предсказание для тестовой выборки и результат верных предсказаний
test_df$status_logit <- ifelse(predict.glm(m.logit, test_df) > 0, "genuine", "counterfeit")
test_df$status_probit <- ifelse(predict.glm(m.probit, test_df) > 0, "genuine", "counterfeit")
right_prediction_logit <- nrow(test_df[test_df$Status == test_df$status_logit, ]) / nrow(test_df) 
right_prediction_probit <- nrow(test_df[test_df$Status == test_df$status_probit, ]) / nrow(test_df) 
