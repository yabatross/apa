library(e1071)

# Data set
ds <- df

# Funcions d'ajuda

minus1 <- function(x) { return (x-1); }

# Binaritza la x donada.
tobin <- function (x) { if (x >= 0.5) return (1); return (0) }

# Calcula l'error de la prediccio del model donat amb les dades donades.
err <- function (model, data, pr, salary)
{
  return (mean(abs(pr-salary)))
}

# Creem els models i mesurem l'error.

# Salari de tot el data frame.
salary <- sapply(as.numeric(ds$X..50K), minus1)

# Salari del primer conjunt de test de CV.
salary_1 <- sapply(as.numeric(test1$X..50K), minus1)
salary_2 <- sapply(as.numeric(test2$X..50K), minus1)
salary_3 <- sapply(as.numeric(test3$X..50K), minus1)

model_lineal <- svm (X..50K~., data=ds, type='C', kernel='linear')
# Cal restar 1 a la prediccio, ja que les classes prenen valors numerics 1 i 2 i nosaltres volem 0 i 1.
pr_lineal <- sapply(as.numeric(predict (model_lineal, ds)), minus1)
# err_lineal = 0.1848894
err_lineal <- err (model_lineal, ds, pr_lineal, salary)

model_poli <- svm (X..50K~., data=ds, type='C', kernel='polynomial')
pr_poli <- sapply(as.numeric(predict (model_poli, ds)), minus1)
# err_poli = 0.1511978
err_poli <- err (model_poli, ds, pr_poli, salary)

model_radial <- svm (X..50K~., data=ds, type='C', kernel='radial')
pr_radial <- sapply(as.numeric(predict (model_radial, ds)), minus1)
# err_radial = 0.1437654
err_radial <- err (model_radial, ds, pr_radial, salary)

# A continuacio fem el mateix amb cross validation. Fem servir el primer conjunt de test/training i veiem
# quin kernel dona millors resultats.

# Test/Training 1

model_lineal_1 <- svm (X..50K~., data=train1, type='C', kernel='linear')
pr_lineal_1 <- sapply(as.numeric(predict (model_lineal_1, test1)), minus1)
# err_lineal_1 = 0.1870278
err_lineal_1 <- err (model_lineal_1, test1, pr_lineal_1, salary_1)

model_poli_1 <- svm (X..50K~., data=train1, type='C', kernel='polynomial')
pr_poli_1 <- sapply(as.numeric(predict (model_poli_1, test1)), minus1)
# err_poli_1 = 0.1566243
err_poli_1 <- err (model_poli_1, test1, pr_poli_1, salary_1)

model_radial_1 <- svm (X..50K~., data=train1, type='C', kernel='radial')
pr_radial_1 <- sapply(as.numeric(predict (model_radial_1, test1)), minus1)
# err_radial_1 = 0.1512806
err_radial_1 <- err (model_radial_1, test1, pr_radial_1, salary_1)

# Test/Training 2

model_lineal_2 <- svm (X..50K~., data=train2, type='C', kernel='linear')
pr_lineal_2 <- sapply(as.numeric(predict (model_lineal_2, test2)), minus1)
# err_lineal_2 = 0.1851852
err_lineal_2 <- err (model_lineal_2, test2, pr_lineal_2, salary_2)

model_poli_2 <- svm (X..50K~., data=train2, type='C', kernel='polynomial')
pr_poli_2 <- sapply(as.numeric(predict (model_poli_2, test2)), minus1)
# err_poli_2 = 0.1579141
err_poli_2 <- err (model_poli_2, test2, pr_poli_2, salary_2)

model_radial_2 <- svm (X..50K~., data=train2, type='C', kernel='radial')
pr_radial_2 <- sapply(as.numeric(predict (model_radial_2, test2)), minus1)
# err_radial_2 = 0.1517413
err_radial_2 <- err (model_radial_2, test2, pr_radial_2, salary_2)

# Test/Training 2

model_lineal_3 <- svm (X..50K~., data=train3, type='C', kernel='linear')
pr_lineal_3 <- sapply(as.numeric(predict (model_lineal_3, test3)), minus1)
# err_lineal_3 = 0.1836359
err_lineal_3 <- err (model_lineal_3, test3, pr_lineal_3, salary_3)

model_poli_3 <- svm (X..50K~., data=train3, type='C', kernel='polynomial')
pr_poli_3 <- sapply(as.numeric(predict (model_poli_3, test3)), minus1)
# err_poli_3 = 0.1612457
err_poli_3 <- err (model_poli_3, test3, pr_poli_3, salary_3)

model_radial_3 <- svm (X..50K~., data=train3, type='C', kernel='radial')
pr_radial_3 <- sapply(as.numeric(predict (model_radial_3, test3)), minus1)
# err_radial_3 = 0.1535059
err_radial_3 <- err (model_radial_3, test3, pr_radial_3, salary_3)
