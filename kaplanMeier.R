kaplanMeier <- function(survivalTime, statusVariable, status, factors, survivalTable = TRUE, caseSummary = TRUE, hr=TRUE, 
                        meanMedianSurvivalTimes = TRUE, quartilesOfSurvivalTimes = FALSE, ci = "log", 
                        varianceEstimation = "greenwood", comparisonTest = "logRank", confidenceLevel = 95,
                        referenceCategory = "first", typeOfTest = "asymptotic", data = dataSet){
  
  if(!is.null(survivalTime)){
    survivalTime = as.matrix(data[, survivalTime, drop = FALSE])
  }
  
  if(!is.null(factors)){
    factors = as.factor(data[, factors])
  }
  
  if(referenceCategory != "first" && !is.null(factors)){
    factors <- factor(factors, levels=rev(levels(factors)))
  }
  
  
  if(!is.null(statusVariable)){
    statusVariable = data[, statusVariable]
    
  }
  
  if(!is.null(status)){
    if(is.numeric(status)){status = as.numeric(status)}else{status = as.factor(status)}
  }
  
  if(!is.null(factors)){
    newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime, 
                         statusVar=statusVariable,factor = factors)
    newData = newData[complete.cases(newData),]
    
  }else{
    
    newData = data.frame(id =seq(1,dim(survivalTime)[1], 1), survivalTime= survivalTime, 
                         statusVar=statusVariable)
    newData = newData[complete.cases(newData),]
    
  }
  

  if(!is.null(factors)){
    
    
    if(caseSummary){
      
      splitFactor = split(newData, newData$factor)
      
      caseSummary <- lapply(splitFactor, FUN = function(x){
        n = nrow(x)
        nOfEvent =  as.numeric(table(x$statusVar)[as.factor(names(table(x$statusVar))) %in% status][[1]])
        percentOfEvent = as.numeric(formatC((nOfEvent/n)*100, digits = 3, format = "f"))
        nOfCensor =  as.numeric(table(x$statusVar)[(!(names(table(x$statusVar))) %in% status)][[1]])
        percentOfCensor = as.numeric(formatC((nOfCensor/n)*100, digits = 3, format = "f"))
        caseSummary = data.frame(n,nOfEvent, percentOfEvent, nOfCensor, percentOfCensor)
        colnames(caseSummary) = c("n", "n of event", "% of event", "n of censor", "% of censor")
        
        return(caseSummary)
        
      })
      
    }
    
    assign("newData", newData, envir=.GlobalEnv)  # put the dat in the global env
    compareCurves <- survfit(Surv(time, statusVar) ~ factor, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
    
    summary = summary(compareCurves, rmean = TRUE)
    
    if(survivalTable){
      survivalTableResult = data.frame(summary[c(2:4,6,8,9,11,10)])
      survivalTableResult$surv = as.numeric(formatC(survivalTableResult$surv, digits = 3, format = "f"))
      survivalTableResult$std.err = as.numeric(formatC(survivalTableResult$std.err, digits = 3, format = "f"))
      survivalTableResult$upper = as.numeric(formatC(survivalTableResult$upper, digits = 3, format = "f"))
      survivalTableResult$lower = as.numeric(formatC(survivalTableResult$lower, digits = 3, format = "f"))
      
      survivalTableResult = split(survivalTableResult[-5], survivalTableResult$strata)
      names(survivalTableResult) = levels(factors)
      
      survivalTableLastResult = lapply(survivalTableResult, function(x)
      {
        colnames(x) = c("Time", "Number at risk", "Number of event", "Cumulative probability of surviving", "S.E.", "Lower limit", "Upper limit")
        return(x)
      }
      )

    }else{
      survivalTableLastResult = NULL
    }
    
    if(hr){
      
      if(!is.null(survivalTableResult)){
        hazardRatio = lapply(survivalTableResult, function(x)
        {
          survHat = x$surv
          hazard = data.frame(Time = x$time, Hazard = as.numeric(formatC(-log(survHat), digits = 3, format = "f")))
          
          return(hazard)
          
        })
      }else{hazardRatio = NULL}
      
    }else{hazardRatio = NULL}
    
    
    if(meanMedianSurvivalTimes){
      
      meanMedian = as.data.frame(summary$table[,-c(2:4)])
      meanMedian$meanLL = meanMedian$`*rmean` - qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
      meanMedian$meanUL = meanMedian$`*rmean` + qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
      meanMedian$factor = levels(factors)
      row.names(meanMedian) = NULL
      meanMedian2 = meanMedian[c(9,1,2:3,7:8,4:6)]
      colnames(meanMedian2) = c("Factor", "n", "Mean", "S.E.(mean)", "Lower (mean)", "Upper (mean)", "Median", "Lower (median)", "Upper (median)")
      meanMedian2$Mean = as.numeric(formatC(meanMedian2$Mean, digits = 3, format ="f"))
      meanMedian2$`S.E.(mean)` = as.numeric(formatC(meanMedian2$`S.E.(mean)`, digits = 3, format ="f"))
      meanMedian2$`Lower (mean)` = as.numeric(formatC(meanMedian2$`Lower (mean)`, digits = 3, format ="f"))
      meanMedian2$`Upper (mean)` = as.numeric(formatC(meanMedian2$`Upper (mean)`, digits = 3, format ="f"))
      meanMedian2$Median = as.numeric(formatC(meanMedian2$Median, digits = 3, format ="f"))
      meanMedian2$`Lower (median)` = as.numeric(formatC(meanMedian2$`Lower (median)`, digits = 3, format ="f"))
      meanMedian2$`Upper (median)` = as.numeric(formatC(meanMedian2$`Upper (median)`, digits = 3, format ="f"))
      
    }else{meanMedian2 = NULL}

    if(quartilesOfSurvivalTimes){
      
        quan = as.data.frame(quantile(compareCurves)$quantile)
        names(quan) = c("25%", "50% (median)", "75%")
        quan2 = cbind(Factor = levels(factors), quan)
        row.names(quan2) = NULL
          
    }else{quan2 = NULL}    
    
    comparisonTests = survMisc::comp(compareCurves)$tests$lrTests
    
    if(comparisonTest == "logRank"){
      testResults = data.frame(cbind(Test = "Log-rank", Chi_square= as.numeric(formatC(comparisonTests[1,1], digits = 3, format = "f")), DF = comparisonTests[1,2], p_value= as.numeric(formatC(comparisonTests[1,3], digits = 3, format = "f")))) 
    }
    
    if(comparisonTest == "gehanBreslow"){
      testResults = data.frame(cbind(Test = "Gehan-Breslow", Chi_square= as.numeric(formatC(comparisonTests[2,1], digits = 3, format = "f")), DF = comparisonTests[2,2], p_value= as.numeric(formatC(comparisonTests[2,3], digits = 3, format = "f")))) 
    }
    
    if(comparisonTest == "taroneWare"){
      testResults = data.frame(cbind(Test = "Tarone-Ware", Chi_square= as.numeric(formatC(comparisonTests[3,1], digits = 3, format = "f")), DF = comparisonTests[3,2], p_value= as.numeric(formatC(comparisonTests[3,3], digits = 3, format = "f")))) 
    }
    
    if(comparisonTest == "petoPeto"){
      testResults = data.frame(cbind(Test = "Peto-Peto", Chi_square= as.numeric(formatC(comparisonTests[4,1], digits = 3, format = "f")), DF = comparisonTests[4,2], p_value= as.numeric(formatC(comparisonTests[4,3], digits = 3, format = "f")))) 
    }
    
    if(comparisonTest == "modPetoPeto"){
      testResults = data.frame(cbind(Test = "Modified Peto-Peto", Chi_square= as.numeric(formatC(comparisonTests[5,1], digits = 3, format = "f")), DF = comparisonTests[5,2], p_value= as.numeric(formatC(comparisonTests[5,3], digits = 3, format = "f")))) 
    }
    
    if(comparisonTest == "flemingtonHarnington"){
      testResults = data.frame(cbind(Test = "Flemington-Harnington", Chi_square= as.numeric(formatC(comparisonTests[6,1], digits = 3, format = "f")), DF = comparisonTests[6,2], p_value= as.numeric(formatC(comparisonTests[6,3], digits = 3, format = "f")))) 
    }
  }else{
    
    if(caseSummary){
      
      n = nrow(newData)
      nOfEvent =  as.numeric(table(newData$statusVar)[as.factor(names(table(newData$statusVar))) %in% status][[1]])
      percentOfEvent = as.numeric(formatC((nOfEvent/n)*100, digits = 3, format = "f"))
      nOfCensor =  as.numeric(table(newData$statusVar)[(!(names(table(newData$statusVar))) %in% status)][[1]])
      percentOfCensor = as.numeric(formatC((nOfCensor/n)*100, digits = 3, format = "f"))
      caseSummary = data.frame(n,nOfEvent, percentOfEvent, nOfCensor, percentOfCensor)
      colnames(caseSummary) = c("n", "n of event", "% of event", "n of censor", "% of censor")
      
    }
    
    
    assign("newData", newData, envir=.GlobalEnv)  # put the dat in the global env
    compareCurves <- survfit(Surv(time, statusVar) ~ 1, data = newData, conf.type = ci, error = varianceEstimation, conf.int = confidenceLevel/100)
    
    summary = summary(compareCurves, rmean = TRUE)
    
    if(survivalTable){
      survivalTableResult = data.frame(summary[c(2:4,6,8:11)])
      survivalTableResult$surv = as.numeric(formatC(survivalTableResult$surv, digits = 3, format = "f"))
      survivalTableResult$std.err = as.numeric(formatC(survivalTableResult$std.err, digits = 3, format = "f"))
      survivalTableResult$upper = as.numeric(formatC(survivalTableResult$upper, digits = 3, format = "f"))
      survivalTableResult$lower = as.numeric(formatC(survivalTableResult$lower, digits = 3, format = "f"))
      survivalTableLastResult = survivalTableResult[-8]
      colnames(survivalTableLastResult) = c("Time", "Number at risk", "Number of event", "Cumulative probability of surviving", "S.E.", "Upper limit", "Lower limit")      
    }else{
      survivalTableLastResult = NULL
    }
    
    
    if(hr){
      
      if(!is.null(survivalTableResult)){
        
          survHat = survivalTableResult$surv
          hazardRatio = data.frame(Time = survivalTableResult$time, Hazard = as.numeric(formatC(-log(survHat), digits = 3, format = "f")))
      
      }else{hazardRatio = NULL}
      
    }else{hazardRatio = NULL}
    
    if(meanMedianSurvivalTimes){
      
      meanMedian = as.data.frame(t(summary$table))
      meanMedian$meanLL = meanMedian$`*rmean` - qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
      meanMedian$meanUL = meanMedian$`*rmean` + qnorm(1-((1-confidenceLevel/100)/2))*meanMedian$`*se(rmean)`
      row.names(meanMedian) = NULL
      meanMedian2 = meanMedian[c(1,5:6,10:11,7:9)]
      colnames(meanMedian2) = c("n", "Mean", "S.E.(mean)", "Lower (mean)", "Upper (mean)", "Median", "Lower (median)", "Upper (median)")
      meanMedian2$Mean = as.numeric(formatC(meanMedian2$Mean, digits = 3, format ="f"))
      meanMedian2$`S.E.(mean)` = as.numeric(formatC(meanMedian2$`S.E.(mean)`, digits = 3, format ="f"))
      meanMedian2$`Lower (mean)` = as.numeric(formatC(meanMedian2$`Lower (mean)`, digits = 3, format ="f"))
      meanMedian2$`Upper (mean)` = as.numeric(formatC(meanMedian2$`Upper (mean)`, digits = 3, format ="f"))
      meanMedian2$Median = as.numeric(formatC(meanMedian2$Median, digits = 3, format ="f"))
      meanMedian2$`Lower (median)` = as.numeric(formatC(meanMedian2$`Lower (median)`, digits = 3, format ="f"))
      meanMedian2$`Upper (median)` = as.numeric(formatC(meanMedian2$`Upper (median)`, digits = 3, format ="f"))
      
    }else{meanMedian2 = NULL}
    
    
    if(quartilesOfSurvivalTimes){
      
      quan2 = as.data.frame(t(quantile(compareCurves)$quantile))
      names(quan2) = c("25%", "50% (median)", "75%")
      row.names(quan2) = NULL
      
    }else{quan2 = NULL}    
    
      testResults = NULL
  }
  
  
  result = list(tableResult = list(caseSummary = caseSummary, meanMedianSurvivalTimes = meanMedian2, quartilesOfSurvivalTimes = quan2), testResult = list(testResults = testResults, survivalTable = survivalTableLastResult, hazardRatio = hazardRatio), plotResult = "under construction...")
  return(result)

}

