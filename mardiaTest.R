mardiaTest <-
function (data, cov = TRUE, qqplot = FALSE) 
    {
        dataframe=as.data.frame(data)
        dname <- deparse(substitute(data))
        data <- as.matrix(data)
        n <- dim(data)[1]
        p <- dim(data)[2]
        data.org <- data
        data <- scale(data, scale = FALSE)
        if (cov) {
            S <- ((n - 1)/n) * cov(data)
        }
        else {
            S <- cov(data)
        }
        D <- data %*% solve(S) %*% t(data)
        g1p <- sum(D^3)/n^2
        g2p <- sum(diag((D^2)))/n
        df <- p * (p + 1) * (p + 2)/6
        k <- (p + 1) * (n + 1) * (n + 3)/(n * ((n + 1) * (p + 1) - 
                                                   6))
        small.skew <- n * k * g1p/6
        skew <- n * g1p/6
        kurt <- (g2p - p * (p + 2)) * sqrt(n/(8 * p * (p + 2)))
        p.skew <- pchisq(skew, df, lower.tail = FALSE)
        p.small <- pchisq(small.skew, df, lower.tail = FALSE)
        p.kurt <- 2 * (1 - pnorm(abs(kurt)))
        
        if (qqplot) {
             d <- diag(D)
             r <- rank(d)
            chi2q <- qchisq((r - 0.5)/n, p)
            df = data.frame(d, chi2q)
            
            qqPlotPrint = ggplot(df, aes(d, chi2q),environment = environment())+geom_point(shape=16, size=4)+
            geom_abline(intercept =0, slope =1,color="black",size=2)+
            xlab("Squared Mahalanobis Distance")+
            ylab("Chi-Square Quantile")+
            ggtitle("Chi-Square Q-Q Plot")+
            theme(plot.title = element_text(lineheight=.8, face="bold"))
            
            print(qqPlotPrint)
        }
        result <- new("mardia", g1p = g1p, chi.skew = skew, p.value.skew = p.skew,
                      chi.small.skew = small.skew, p.value.small = p.small, g2p = g2p,
                      z.kurtosis = kurt, p.value.kurt = p.kurt, dname = dname, dataframe = dataframe)
        
        result
    }

