library("stats")

AR<-function(theta,n){ # Вариация процесса, с передаваемыми theta. Вообще это процесс общего порядка, но тк я передаю
                       # Тета, то порядок эквивалентен размерности тета.  
    p<-length(theta)
    x<-rnorm(n+p)# Вектор размерности n+p

    for (i in (p+1):(n+p)) {
    x[i] <- sum(theta * rev(x[(i-p):(i-1)])) + rnorm(1)
    }

    x <- x[(p+1):(n+p)]

    return(x)
}

Estimation_MNK <- function(x) {
  n <- length(x)
  numerator <- 0
  denominator <- 0

  for (i in 2:n) {
    numerator <- numerator + x[i] * x[i-1]
    denominator <- denominator + x[i-1] * x[i-1]
  }

  theta <- numerator / denominator

  return(theta)
}

Estimation_MLE <- function(x) {
  n <- length(x)
  numerator <- 0
  denominator <- 0

  for (i in 2:n) {
    numerator <- numerator + x[i] * x[i-1]
    denominator <- denominator + x[i-1] * x[i-1]
  }

  theta <- numerator / denominator

  return(theta)
}


Generate_vector_theta_stationary <- function() { # размерность 2
  max_iterations <- 1000
  num_iterations <- 0

 while (num_iterations < max_iterations) {
   theta_local <- c(rnorm(1), rnorm(1))
   roots <- polyroot(c(-theta_local[2], -theta_local[1], 1))

   if (all(abs(roots)) < 1) {
     return(theta_local)
   }

   num_iterations <- num_iterations + 1
 }

 # Если не удалось найти устойчивый вектор theta
 return(c(0.5, -0.2))
}

#######################################################
#                      Задания                        #
#######################################################
par(mfrow = c(2, 3)) # Разбил окно вывод графков на сетку 2*3
############ 1 ##########


########################
#       Типы тета:     #
#    1 - |theta| < 1   #
#    2 - |theta| = 1   #
#    3 - |theta| > 1   #
########################

theta1 <- rnorm(1, mean=0, sd=0.5)

theta2 <- sample(c(-1, 1), 1)

theta3 <- rnorm(1, mean=2, sd=1)


# 1.1
theta <- c(theta1)
n<-1000
x<- AR(theta,n)
# png(filename="./AR_Plots/theta_lower_1.png")
plot.ts(x, main = "thetha < 1")
# dev.off()

# 1.2
theta <- c(theta2)
n <-1000
x <- AR(theta,n)
# png(filename="./AR_Plots/theta_equal_1.png")
plot.ts(x, main = "thetha = 1")
# dev.off()

# 1.3

theta <- c(theta3)
n <-1000
x <- AR(theta,n)
# png(filename="./AR_Plots/theta_upper_1.png")
plot.ts(x, main = "thetha > 1")
# dev.off()

############ 2 ##########
#Оценка МНК
theta <- c(theta1)
n <-1000
x <- AR(theta,n)

estimated_theta_mnk <- Estimation_MNK(x)
print("############ 2 ##########")
print(paste("Real theta: ", theta, " Estimated theta: ", estimated_theta_mnk))

############ 3 ##########
#Оценка ММН

#
# В случае гауссовского шума МНК и ММП выглядят одинаково.
# Для MП ошибка распредлена нормально, а авторегрессия описывается ошибкой
# Отриц логарифм правдоподоия эквивалентен МНК
# В выводе оценки будут разные потому, что данные заново генерируются.
#
theta <- c(theta1)
n <-1000
x <- AR(theta,n)

estimated_theta_mle <- Estimation_MLE(x)
print("############ 3 ##########")
print(paste("Real theta: ", theta, " Estimated theta: ", estimated_theta_mle))

############ 4 ##########

#4.a

theta <- theta2
n <- 1000
x <- AR(theta,n)
#4.b

estimated_theta_mnk_by_10_numbers <-Estimation_MNK(head(x,10))
print("############ 4 ##########")
print(paste("Real theta: ", theta, " Estimated theta: ", estimated_theta_mnk_by_10_numbers))

#4.c

estimated_theta_mnk_vector <- numeric(n)
for (i in 11:n){
  #Пусть первые 11 элементов вектора будут пустые
  estimated_theta_mnk_vector[i]<-estimated_theta_mnk_by_i_numbers <-Estimation_MNK(head(x,i))
}
estimated_theta_mnk_vector <- estimated_theta_mnk_vector[-c(1:11)]

#png(filename="./AR_Plots/Estimation_MNK_from_11_to_1000.png")
plot(estimated_theta_mnk_vector, type = 'b', main = "Dynamics estimating theta", xlab = "I", ylab = "Estimation_MNK", pch = 1)
# dev.off()

############ 5 ##########

theta <- Generate_vector_theta_stationary()
print("############ 5 ##########")
print("Theta vector:")
print(theta)

n <- 1000
x <- AR(theta,n)
plot.ts(x, main = "Stable AR(2)")

############ 6 ##########

model <- arima(x, order=c(2, 0, 0), include.mean=FALSE)

print("############ 6 ##########")
print (model)

