## Problème 3


## LIFT
x <- c(11/81, 15/81, 17/81, 17/81)
y <- c(0.579, 0.577, 0.425, 0.21)

plot(x, y, type = "o", pch = 16, col = "blue", xlab = "RPP", ylab = "Se")
lines(x, y)

x_biss <- seq(0,0.3, by=0.0001)
y_biss <- x_biss

plot(x_biss, y_biss)

## LIFT normalisé
x <- c(11/81, 15/81, 17/81, 17/81)
y <- c(0.579/(11/81), 0.577/(15/81), 0.425/(17/81), 0.21/(17/81))
plot(x, y, type = "o", pch = 16, col = "blue", xlab = "RPP", ylab = "Se/RPP")
lines(x, y)

