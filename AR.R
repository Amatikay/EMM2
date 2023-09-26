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
png(filename="theta_lower_1.png")
plot.ts(x)
dev.off()

# 1.2
theta <- c(theta2)
n<-1000
x<- AR(theta,n)
png(filename="theta_equal_1.png")
plot.ts(x)
dev.off()

# 1.3

theta <- c(theta3)
n<-100
x<- AR(theta,n)
png(filename="theta_upper_1.png")
print(x)
plot.ts(x)
dev.off()

############ 2 ##########

theta <- c(theta1)
n<-1000
x<- AR(theta,n)

estimated_theta <- Estimation_MNK(x)

print(paste("Real theta: ", theta, " Estimated theta: ", estimated_theta))

############ 3 ##########

############ 4 ##########
############ 5 ##########
############ 6 ##########