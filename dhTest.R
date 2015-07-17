dhTest <-
function (data, qqplot = FALSE) 
{
    dataframe=as.data.frame(data)
    dname <- deparse(substitute(data))
    n <- dim(data)[1]
    p <- dim(data)[2]
    S <- var(data)
    dif <- scale(data, scale = FALSE)
    Dj <- diag(dif%*%solve(S)%*%t(dif))  #squared-Mahalanobis' distances
    D <- diag(S)
    V <- diag(D^(-0.5))
    C <- V %*% S %*% V
    L <- diag((eigen(C)$values)^-0.5)
    H <- eigen(C)$vectors
    y.i <- H %*% L %*% t(H) %*% V %*% t(data - sapply(data, mean))
    B.1 <- apply(y.i, 1, function(x) {
        skew(x, "moments")
    })
    B.2 <- apply(y.i, 1, function(x) {
        kurt(x, "moments")
    })
    del <- (n - 3) * (n + 1) * (n^2 + (15 * n) - 4)
    a <- ((n - 2) * (n + 5) * (n + 7) * (n^2 + (27 * n) - 70))/(6 * 
        del)
    c <- ((n - 7) * (n + 5) * (n + 7) * (n^2 + (2 * n) - 5))/(6 * 
        del)
    k <- ((n + 5) * (n + 7) * (n^3 + 37 * n^2 + (11 * n) - 313))/(12 * 
        del)
    alpha <- a + B.1^2 * c
    chi <- (B.2 - 1 - B.1^2) * 2 * k
    Z.2 <- (((chi/(2 * alpha))^(1/3)) - 1 + (1/(9 * alpha))) * 
        ((9 * alpha)^(0.5))
    del <- (n - 3) * (n + 1) * (n^2 + (15 * n) - 4)
    a <- ((n - 2) * (n + 5) * (n + 7) * (n^2 + (27 * n) - 70))/(6 * 
        del)
    c <- ((n - 7) * (n + 5) * (n + 7) * (n^2 + (2 * n) - 5))/(6 * 
        del)
    k <- ((n + 5) * (n + 7) * (n^3 + 37 * n^2 + (11 * n) - 313))/(12 * 
        del)
    alpha <- a + B.1^2 * c
    chi <- (B.2 - 1 - B.1^2) * 2 * k
    Z.2 <- (((chi/(2 * alpha))^(1/3)) - 1 + (1/(9 * alpha))) * 
        ((9 * alpha)^(0.5))
    beta <- (3 * (n^2 + (27 * n) - 70) * (n + 1) * (n + 3))/((n - 
        2) * (n + 5) * (n + 7) * (n + 9))
    w2 <- -1 + ((2 * (beta - 1))^0.5)
    del <- 1/((log(sqrt(w2)))^0.5)
    y <- B.1 * ((((w2 - 1)/2) * (((n + 1) * (n + 3))/(6 * (n - 
        2))))^0.5)
    Z.1 <- del * (log(y + (y^2 + 1)^0.5))
    E <- as.numeric(t(Z.1) %*% Z.1 + t(Z.2) %*% Z.2)
    P = as.numeric(pchisq(E, 2 * p, lower.tail = FALSE))
    
  if (qqplot){    
            d <- Dj    
            r <- rank(d)  
            chi2q <- qchisq((r-0.5)/n,p)
            plot(d, chi2q , pch = 19, main = "Chi-Square Q-Q Plot",
                 xlab = "Squared Mahalanobis Distance",ylab="Chi-Square Quantile")
            abline(0, 1,lwd = 2, col = "black")
        }
        
     result <- new("dh", TS = E, p.value = P, dname = dname, dataframe = dataframe)
     
     result
}