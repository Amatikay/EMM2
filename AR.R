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

#######################################################
#                      Задания                        #
#######################################################

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
png(filename="./AR_Plots/theta_lower_1.png")
plot.ts(x)
dev.off()

# 1.2
theta <- c(theta2)
n <-1000
x <- AR(theta,n)
png(filename="./AR_Plots/theta_equal_1.png")
plot.ts(x)
dev.off()

# 1.3

theta <- c(theta3)
n <-100
x <- AR(theta,n)
png(filename="./AR_Plots/theta_upper_1.png")
plot.ts(x)
dev.off()

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
#
theta <- c(theta1)
n <-1000
x <- AR(theta,n)

estimated_theta_mle <- Estimation_MLE(x)
print("############ 3 ##########")
print(paste("Real theta: ", theta, " Estimated theta: ", estimated_theta_mle))

############ 4 ##########

#4.a

theta <- theta1
n <- 1000
x <- AR(theta,n)
#4.b

estimated_theta_mnk_by_10_numbers <-Estimation_MNK(head(x,10))
print("############ 4 ##########")
print(paste("Real theta: ", theta, " Estimated theta: ", estimated_theta_mnk_by_10_numbers))

#4.c

estimated_theta_mnk_vector <- rep(NA,n)
for (i in 11:n){
  #Для того, чтобы не придумывать лишние итераторы и не вводить волшенбые константы типо i-11
  #Пусть первые 11 элементов вектора будут пустые
  estimated_theta_mnk_vector[i]<-estimated_theta_mnk_by_i_numbers <-Estimation_MNK(head(x,i))
}
estimated_theta_mnk_vector <- estimated_theta_mnk_vector[-c(1:11)]


png(filename="./AR_Plots/Estimation_MNK_from_11_to_1000.png")
plot(x, type = 'b', main = "Dynamics of estimates relative to sample size", xlab = "I", ylab = "Estimation_MNK", pch = 1)
dev.off()
############ 5 ##########
############ 6 ##########