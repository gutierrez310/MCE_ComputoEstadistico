# Importamos los datos
data(cars)
print(cars)

# Probamos con varios modelos, sobre posibles relaciones con la variable speed
{
  spd.mod0 <- lm(dist ~ speed+log(speed), data=cars)
  spd.mod1 <- lm(dist ~ 1+speed+speed**2, data=cars)
  spd.mod2 <- lm(dist ~ 0+speed+speed**2+speed**3, data=cars)
  spd.mod3 <- lm(log(dist) ~ 1+speed+speed**2+log(speed), data=cars)
}

# Obtemos el estimador de prediccion de error (Aikaiken Information Criterion)
AIC(spd.mod0, spd.mod1, spd.mod2, spd.mod3)

# Al comparar diferentes modelos podemos observar que un menor AIC no equivale
#  a una mayor R^2 :
summary(spd.mod3)
summary(spd.mod2)

# Podemos observar posibles outliers
par(mfrow=c(1,2))
speed <- cars$speed
dist <- cars$dist
plot(speed, dist, main = "cars")
plot(speed[-c(23,35,49)], dist[-c(23,35,49)], xlab="dist", ylab="speed", main = "cars sin outliers")

print("Tomando en cuenta outliers...")

# Buscamos modelos sin outliers con buenos puntajes AIC y R^2
{
  spd.mod0 <- lm(dist ~ speed+log(speed), data=cars[-c(1,2,3),])
  spd.mod1 <- lm(dist ~ 1+speed+speed**2, data=cars[-c(1,2,3),])
  spd.mod2 <- lm(dist ~ 0+speed+speed**2+speed**3, data=cars[-c(1,2,3),])
  spd.mod3 <- lm(log(dist) ~ 1+speed+speed**2+log(speed), data=cars[-c(1,2,3),])
  spd.mod4 <- lm(dist ~ 0+speed+speed**2, data=cars[-c(23,35,49),])
  spd.mod5 <- lm(dist ~ 1+speed+speed**2+log(speed), data=cars[-c(23,35,49),])
}
AIC(spd.mod0, spd.mod1, spd.mod2, spd.mod3, spd.mod4, spd.mod5)

# El siguiente fue el modelo con mejor R^2 pero el tercero mejor puntaje
#  AIC (de 6 modelos) (mala comparacion de AIC)
summary(spd.mod4)

#=============================== 7.2

# Removemos los outliers
speed <- cars[-c(23,35,49),]$speed
dist <- cars[-c(23,35,49),]$dist
n <- length(dist)

print("Promedio de la muestra")
print()

#=============================== 8.1


# https://search.r-project.org/CRAN/refmans/aplore3/html/lowbwt.html

# install.packages("aplore3")
df <- aplore3::lowbwt



# Dicotomizamos la variable bwt
n <- length(df[,1])
lbw <- vector("numeric", n)
for (i in 1:n){
  if (df[i,]$low == "< 2500 g")
    lbw[i] = 1
  else
    lbw[i] = 0
}

# Consideramos las siguientes variables

# Age of mother (Years)
age <- df$age
# Weight of mother at last menstrual period (Pounds)
lwt <- df$lwt
# Race (1: White, 2: Black, 3: Other)
race <- df$race
# Number of physician visits during the first trimester (1: None, 2: One, 3: Two, etc)
ftv <- df$ftv

df <- data.frame(lbw, age, lwt, race ,ftv)

x_matrix <- cbind(rep(1,n), age, lwt, race ,ftv)

# Para encontrar el valor esperado y la varianza de las betas asociadas, primero obtenemos el modelo
#bwt.mod0 <- lm(log(lbw) ~ age + lwt + race +ftv, data=df)


mu_i <- function(x, beta, i){
  # receives x as matrix, and beta as numeric
  return (1/(1+exp(-x %*% beta)))
}


H_beta <- function(beta, x){
  n_beta <- length(beta)
  every_sum <- rep(0, n_beta)
  n <- length(x[,1])
  for (idx in 1:n){
    this_mu <- mu_i(x[idx,], beta, idx)
    for (z_i in 1:n_beta){
      every_sum[z_i] <- every_sum[z_i] + (lbw[idx] - this_mu) * x[idx, z_i]
    }
  }
  return (every_sum)
}


delta_H_beta_over_delta_beta <- function(beta, x){
  this_sum <- 0
  for (idx in 1:n){
    this_mu <- mu_i(x[idx,], beta, idx)
    this_sum <- this_sum + this_mu * (1 - this_mu) * norm(x[idx,], type="2")
  }
  return (-1*this_sum)
}

tolm <- 1e-6
iterm <- 100
a0 <- 2
a1 <- 0
iters <- 0

beta_kplus1 <- c(.5,.5,.5,.5,.5)

# e <- mu_i(x_matrix[1,], beta_kplus1, 1)
# aber <- H_beta(beta_kplus1, x_matrix)
# uhh <- delta_H_beta_over_delta_beta(beta_kplus1, x_matrix)

while ((a1-a0)/a0 <= tolm || iters > iterm){
  beta_k <- beta_kplus1
  invers <- delta_H_beta_over_delta_beta(beta_k, x_matrix)**(-1)
  H_k <- H_beta(beta_k, x_matrix)
  iters <- iters+1
  beta_kplus1 <- beta_k - invers * H_k

  print("current beta estimates")
  print(beta_kplus1)
  
  a0 <- norm(beta_k, type="2")
  a1 <- norm(beta_kplus1, type="2")
}



















#1