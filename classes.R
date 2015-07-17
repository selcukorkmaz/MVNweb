setClass("mardia",
    slots = c(g1p = "numeric", chi.skew="numeric", p.value.skew="numeric", chi.small.skew="numeric",
        p.value.small="numeric", g2p="numeric", z.kurtosis="numeric", p.value.kurt="numeric", dname="character", dataframe="data.frame"))


setGeneric("mardia", function(object) standardGeneric("mardia"))


setMethod("show",
signature = "mardia",
definition = function(object) {
    n=dim(object@dataframe)[1]
    cat("   Mardia's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------", "\n", sep = " ")
    cat("   data :", object@dname, "\n\n", sep = " ")
    cat("   g1p            :", object@g1p, "\n", sep = " ")
    cat("   chi.skew       :", object@chi.skew, "\n", sep = " ")
    cat("   p.value.skew   :", object@p.value.skew, "\n\n", sep = " ")
    cat("   g2p            :", object@g2p, "\n", sep = " ")
    cat("   z.kurtosis     :", object@z.kurtosis, "\n", sep = " ")
    cat("   p.value.kurt   :", object@p.value.kurt, "\n\n", sep = " ")
    cat("   chi.small.skew :", object@chi.small.skew, "\n", sep = " ")
    cat("   p.value.small  :", object@p.value.small, "\n\n", sep = " ")
    if(n>=20){
        cat(if((object@p.value.skew > 0.05) & (object@p.value.kurt > 0.05)){"   Result         : Data is multivariate normal."}
        else {"   Result          : Data is not multivariate normal."},"\n\n")
    }
    if(n<20){
        cat(if((object@p.value.small > 0.05) & (object@p.value.kurt > 0.05)){"   Result         : Data is multivariate normal."}
        else {"   Result          : Data is not multivariate normal."},"\n\n")
    }
    cat("NOTE: For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than 0.05. If sample size (n) is less than 20 then 'p.value.small' should be used as significance value of skewness instead of 'p.value.skew'.", "\n", sep = " ")
    cat("---------------------------------------", "\n\n", sep = " ")
    
    invisible(NULL)
})

setClass("hz",
slots = c(HZ = "numeric", p.value="numeric", dname="character", dataframe="data.frame"))


setGeneric("hz", function(object) standardGeneric("hz"))


setMethod("show",
signature = "hz",
definition = function(object) {
    cat("  Henze-Zirkler's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------------", "\n", sep = " ")
    cat("  data :", object@dname, "\n\n", sep = " ")
    cat("  HZ      :", object@HZ, "\n", sep = " ")
    cat("  p-value :", object@p.value, "\n\n", sep = " ")
    cat(if(object@p.value > 0.05){"  Result  : Data is multivariate normal."}
        else {"  Result  : Data is not multivariate normal."},"\n")
    cat("---------------------------------------------", "\n\n", sep = " ")
    invisible(NULL)
})



setClass("royston",
slots = c(H = "numeric", p.value="numeric", dname="character", dataframe="data.frame"))


setGeneric("royston", function(object) standardGeneric("royston"))


setMethod("show",
signature = "royston",
definition = function(object) {
    cat("  Royston's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------------", "\n", sep = " ")
    cat("  data :", object@dname, "\n\n", sep = " ")
    cat("  H       :", object@H, "\n", sep = " ")
    cat("  p-value :", object@p.value, "\n\n", sep = " ")
    cat(if(object@p.value > 0.05){"  Result  : Data is multivariate normal."}
        else {"  Result  : Data is not multivariate normal."},"\n")
    cat("---------------------------------------------", "\n\n", sep = " ")
    invisible(NULL)
})


setClass("dh",
slots = c(TS = "numeric", p.value="numeric", dname="character", dataframe="data.frame"))


setGeneric("dh", function(object) standardGeneric("dh"))


setMethod("show",
signature = "dh",
definition = function(object) {
    cat("  Doornick-Hansen's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------------", "\n", sep = " ")
    cat("  data :", object@dname, "\n\n", sep = " ")
    cat("  DH      :", object@TS, "\n", sep = " ")
    cat("  p-value :", object@p.value, "\n\n", sep = " ")
    cat(if(object@p.value > 0.05){"  Result  : Data is multivariate normal."}
    else {"  Result  : Data is not multivariate normal."},"\n")
    cat("---------------------------------------------", "\n\n", sep = " ")
    invisible(NULL)
})


