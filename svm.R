library(e1071)

# Data set
ds <- df
ds$X..50K <- as.factor(ds$X..50K)

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

salary <- sapply(as.numeric(ds$X..50K), minus1)

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
