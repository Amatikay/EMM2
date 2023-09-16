library("stats")


# Я пытался сделать сделать для первого порядка, но пока разбирался - написалось для общего порядка

##################################
#        "Type of theta:         #
#        1 - |theta| < 1         #
#        2 - |theta| = 1         #
#        3 - |theta| > 1"        #
##################################

AR<-function(theta,x0,p){
    X<-matrix(1,p,1)
    X[1]<-x0
    for(i in 2:p){
        X[i]<-t(theta[1:i-1]) %*% X[1:i-1] + rnorm(1)#scale multiply vectors + Gauss noize
    }
    return(X[p])
}



plot_AR_n <-function(n){

    x0<-runif(1, min = -10, max = 10)
    theta1<-runif(1, min = -0.999, max = 0.999)
    theta2<-(-1) ** (sample(1:10,1))
    theta3<-runif(1, min = -1.0001, max = 1.0001)

    list_AR_<-c()
    for(i in 1:n){
        list_AR_[i]<- AR(theta1,x0,2)
    }
    x11()
    plot(1:i,list_AR_,type="l")

    list_AR_<-c()
    for(i in 1:n){
        list_AR_[i]<- AR(theta2,x0,2)
    }
    x11()
    plot(1:i,list_AR_,type="l")

    list_AR_<-c()
    for(i in 1:n){
        list_AR_[i]<- AR(theta2,x0,2)
    }
    x11()
    plot(1:i,list_AR_,type="l")
}
MNK<-function(X,n){
    numeric<-0
    denumeric<-0
    for (i in 2:n){
        numeric <- numeric + as.numeric((as.numeric(X[i-1])*as.numeric(X[i])))
        denumeric <- denumeric + as.numeric(as.numeric(X[i])*as.numeric(X[i]))
    }
    return(numeric/denumeric)
}
MNK_k<-function(X,n){
    TETHA<-c()
    for (i in 1:n){ # Для оценки тета наблюдений меняю 1:n на [1:10 or 10:n]
        TETHA[i]<- MNK(X,i)
    }
    return(TETHA)
}

#Оценка максимального правдоподобия будет реализована как копипипаста МНК же для этой модели, тк дисперсия единичная
MP<-function(X){
}

findPolyroots<-function(theta){
    a<-1
    #Подставляю коэффиценты со знаком минус сразу
    b<--theta[1]
    c<--theta[2]
    D=b^2-4*a*c
   m=ifelse(D<0,complex(1,0,sqrt(abs(D))),sqrt(D))
   roots<-c((-b+m)/(2*a),(-b-m)/(2*a))
   return(c(abs(roots[1]),abs(roots[2])))
}

graphSustainabilityProcess<-function(theta,n=1000){
    x0<-runif(1, min = -10, max = 10)
    list_AR_<-c()
    for(i in 1:n){
        list_AR_[i]<- AR(theta,x0,3)
    }
    x11()
    plot(1:i,list_AR_,type="l")


}

########################################################################################
#                                       Start test                                     #
########################################################################################



############ 1 ##########
plot_AR_n(500)
############ 2 ##########
print(MNK(list_AR1_thetha2,n))
############ 3 ##########



############ 4 ##########

x0<-runif(1, min = -10, max = 10)
theta2<-(-1) ** (sample(1:10,1))
list_AR1_thetha2<-c()
    for(i in 1:n){
        list_AR1_thetha2[i]<- AR(theta2,x0,1)
    }

x11()
plot(MNK_k(list_AR1_thetha2,n),type="l")


############ 5 ##########
#print(findPolyroots(c(-0.53,0.1))) # Усточивые корни для этого вектора тета
graphSustainabilityProcess(c(-0.53,0.1))
############ 6 ##########
# К примеру временной ряд стоимости бензина
gas=c(17.573,17.337,15.809,14.153,
15.513,14.535,14.154,14.807,
15.467,15.709,16.054,16.018,
16.165,16.691,17.351,17.282,
17.737,17.844,17.729, 17.590,
17.726,18.129, 17.795, 17.819,
17.739)

print(arima(gas,order=c(2,0,0)))
