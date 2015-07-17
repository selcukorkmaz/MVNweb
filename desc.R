desc <- function(data, desc = TRUE){
  
  if (is.data.frame(data) || is.matrix(data)){ 
    varNames = colnames(data)
    dims = dim(data)
    
    if(desc){
    descriptives = describe(data)
    descriptives = descriptives[c(-1,-6,-7,-10,-13)]
    a=t(apply(data, 2, quantile))
    b=cbind(a[,2], a[,4])
    colnames(b)=c("25th", "75th")
    descriptives = cbind(descriptives, b)
    descriptives = round(cbind(descriptives[,1:6], descriptives[,9:10], descriptives[,7:8]),3)
    names(descriptives)=c("n", "Mean", "Std.Dev", "Median", "Min", "Max", "25th", "75th", "Skew", "Kurtosis")
    #name.width <- max(sapply(names(descriptives), nchar))
    #descriptives = format(descriptives, width = name.width, justify = "centre")
    }else descriptives = NULL
    
    if (is.matrix(data)){
      data = data.frame(data)
    }
   
    result = list(Descriptive = descriptives)
    return(result)
  }}