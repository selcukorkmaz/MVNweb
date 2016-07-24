shinyServer(function(input, output, session) {

library("DT")
library("survival")
library("KMsurv")
library("survMisc")
source("lifeTables.R")
source("kaplanMeier.R")
source("coxRegression.R")
source("getDescriptiveResultsCoxRegression.R")
source("stepwise.R")


   dataM <- reactive({  ## Data input.
       if(input$dataInput==1){  ## Load example data.
           
           if(input$sampleData == 1){
           
                data <- read.table("hmohiv.txt", header=TRUE, sep = "\t")
          
                data$time <- as.numeric(data$time)
           }
           
           else if(input$sampleData == 2){
               
               data <- read.table("Rossi.txt", header=TRUE, sep = "\t")

           }
       }
       
       else if(input$dataInput==2){  ## Upload data.
           
           inFile <- input$upload
           
           mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
           
           if (is.null(input$upload))  {return(NULL)}
           
           if (file.info(inFile$datapath)$size <= 10485800){
               data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE, na.strings = c("", "NA","."))
           }
           
           else print("File is bigger than 10MB and will not be uploaded.")
           
       }

       ind = complete.cases(data)
       return(data[ind,])

       #else {
       #    data[,dim(data)[2]] = as.factor(data[,dim(data)[2]])
       #    ind = complete.cases(data)
       #    return(data[ind,])
       #}
   })
   
   
   output$dataUpload <- DT::renderDataTable({
       
       datatable(dataM(), extensions = 'ColReorder', options = list(colReorder = TRUE))
       
   })
   
   
   
   ################################ Observe Life Table (start) ###############################
   
   # observe({
   #    updateSelectInput(session, "statusVariable", choices = colnames(dataM()), selected = colnames(dataM())[4])
   #})
   
   observe({
       updateSelectInput(session, "survivalTime", choices = colnames(dataM()), selected = colnames(dataM())[1])
   })
   
   observe({
       
       if(input$factorVar){
       updateSelectInput(session, "factor", choices = colnames(dataM()), selected = colnames(dataM())[3])
       }else{
           
        updateSelectInput(session, "factor", choices = NULL, selected = NULL)
       }
   })
   
   
   statusVarLT <- reactive({return(input$statusVariable)})
   
   
   observe({
       data_tmp <- dataM()
       if (!is.null(data_tmp)){
           updateSelectInput(session = session, inputId = "statusVariable",
           choices = colnames(data_tmp), selected = colnames(data_tmp)[4])
       } else {
           updateSelectInput(session = session, inputId = "statusVariable",
           choices = "", selected = "")
       }
   })
   
   observe({
       data_tmp <- dataM()
       if (!is.null(data_tmp)){
           idx <- which(colnames(data_tmp) %in% statusVarLT())
           categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
           
           updateSelectizeInput(session = session, inputId = "status", choices = categories,
           selected = categories[2])
       } else {
           updateSelectizeInput(session = session, inputId = "status", choices = "",
           selected = "")
       }
   })
   

   
 ################################ Observe Life Table (end) ###############################
 #########################################################################################
 ################################ Observe Kaplan-Meier (start) ###########################
 observe({
     updateSelectInput(session, "statusVariableKM", choices = colnames(dataM()), selected = colnames(dataM())[4])
 })
 
 observe({
     updateSelectInput(session, "survivalTimeKM", choices = colnames(dataM()), selected = colnames(dataM())[1])
 })
 
 observe({
     
     if(input$factorVarKM){
         updateSelectInput(session, "factorKM", choices = colnames(dataM()), selected = colnames(dataM())[3])
     }else{
         
         updateSelectInput(session, "factoKMr", choices = NULL, selected = NULL)
     }
 })
 
 statusVarKM <- reactive({return(input$statusVariableKM)})
 
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         updateSelectInput(session = session, inputId = "statusVariableKM",
         choices = colnames(data_tmp), selected = colnames(data_tmp)[4])
     } else {
         updateSelectInput(session = session, inputId = "statusVariableKM",
         choices = "", selected = "")
     }
 })
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         idx <- which(colnames(data_tmp) %in% statusVarKM())
         categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
         
         updateSelectizeInput(session = session, inputId = "statusKM", choices = categories,
         selected = categories[2])
     } else {
         updateSelectizeInput(session = session, inputId = "statusKM", choices = "",
         selected = "")
     }
 })
 


 ################################ Observe Kaplan-Meier (end) ###########################
 #########################################################################################
 #########################################################################################
 ################################ Observe Cox Regression (start) ###########################
 observe({
     updateSelectInput(session, "survivalTimeCox", choices = colnames(dataM()), selected = colnames(dataM())[1])
 })

 observe({
     updateSelectInput(session, "statusVariableCox", choices = colnames(dataM()), selected = colnames(dataM())[2])
 })
 
 statusVar <- reactive({return(input$statusVariableCox)})

 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         updateSelectInput(session = session, inputId = "statusVariableCox",
         choices = colnames(data_tmp), selected = colnames(data_tmp)[2])
     } else {
         updateSelectInput(session = session, inputId = "statusVariableCox",
         choices = "", selected = "")
     }
 })
 
 observe({
     data_tmp <- dataM()
     if (!is.null(data_tmp)){
         idx <- which(colnames(data_tmp) %in% statusVar())
         categories <- levels(as.factor(as.character(data_tmp[ ,idx])))
         
         updateSelectizeInput(session = session, inputId = "statusCox", choices = categories,
         selected = categories[2])
     } else {
         updateSelectizeInput(session = session, inputId = "statusCox", choices = "",
         selected = "")
     }
 })
 
 
 
 # observe({
  #   updateSelectInput(session, "statusCox", choices = statusVar(), selected = statusVar()[2])
 #})
 

observe({
    updateSelectInput(session, "categoricalInput", choices = colnames(dataM()), selected = colnames(dataM())[5])
})

observe({
    updateSelectInput(session, "continuousInput", choices = colnames(dataM()), selected = colnames(dataM())[4])
})

 ################################ Observe Cox Regression (end) #########################
 #######################################################################################
 ###################### Life Table (start) #############################################
 
 output$descriptivesText <- renderText({
     if (input$run && input$caseSummary){
         'Table 1: Descriptives'
     }
 })
 
 output$lifeTableText <- renderText({
     if (input$run && input$lifeTable){
         'Table 2: Life Table'
     }
 })
 
 
 output$medianLifeTimeText <- renderText({
     if (input$run && input$medianLifeTime){
         'Table 3: Median Life Time'
     }
 })
 
 
 output$hrText <- renderText({
     if (input$run && input$hr){
         'Table 4: Hazard Ratio'
     }
 })
 
 output$compTestText <- renderText({
     if (input$run && input$compTest && input$factorVar){
         'Table 5: Comparison Test'
     }
 })
 
 
 result <- reactive({
 
  if(input$run){
    dataSet = dataM()
    
    
        if(input$factorVar){
            
            fctr = input$factor
        }else{fctr = NULL}
        
 
        lt = lifeTables(survivalTime = input$survivalTime, statusVariable = input$statusVariable, status = input$status, factors= fctr, fromTime = input$from, toTime = input$to, by = input$by, lifeTable = input$lifeTable, descriptives = input$caseSummary, hr = input$hr, medianLifeTime = input$medianLifeTime, ci = input$ci,
            varianceEstimation = input$varianceEstimation, compare = input$compTest, comparisonTest = input$comparisonTest, confidenceLevel = input$confidenceLevel,
        referenceCategory = input$refCategory, typeOfTest = "asymptotic", data = dataSet)
    
    
    lt
    }
 
 })
 
 
 descriptivesReactive <- reactive({
 
 if(input$run){
     
     desc = result()$tableResult$caseSummary
     
     if(!is.null(desc)){
         if(input$factorVar){
             descs = do.call(rbind.data.frame, desc)
             
         }else{
             
             descs = desc
             
         }
     }else{descs = NULL}
     
     descs
     
 }else{descs = NULL}

 
 
 })
 
 
 output$descriptives <- DT::renderDataTable({
  
  datatable(descriptivesReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
  ))

 })
 
 
 lifetableReactive <- reactive({
 
 if(input$run){
     ltResult = result()$testResult$lifeTable
     
     if(!is.null(ltResult)){
         if(input$factorVar){
             ltResults = do.call(rbind.data.frame, ltResult)
             
         }else{
             
             ltResults = ltResult
             
         }
     }else{ltResults = NULL}
     
     ltResults
 }else{ltResults = NULL}



 })
 
 
 
 output$lifetableResult <- DT::renderDataTable({
     
     datatable(lifetableReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
     ))
     
 })
 
 medianLifeTimeReactive <- reactive({
 
 if(input$run){
     mlt = result()$tableResult$medianLifeTime
     
     if(!is.null(mlt)){
         if(input$factorVar){
             mltResults = do.call(rbind.data.frame, mlt)
             
         }else{
             
             mltResults = mlt
             
         }
     }else{mltResults = NULL}
     
     mltResults
 }else{mltResults = NULL}
 
 
 
 })
 
 
 output$medianLifeTimeResult <- DT::renderDataTable({
     
     datatable(medianLifeTimeReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
     ))
     
 })
 
 
 hazardRatioReactive <- reactive({
  
 if(input$run){
     
     hrResult = result()$testResult$hazardRatio
     
     if(!is.null(hrResult)){
         if(input$factorVar){
             hrResults = do.call(rbind.data.frame, hrResult)
             
         }else{
             
             hrResults = hrResult
             
         }
     }else{hrResults = NULL}
     
     hrResults
 }else{hrResults = NULL}
 
 })
 
 
 output$hazardRatioResult <- DT::renderDataTable({
    
    datatable(hazardRatioReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
 
 })
 
 
 comparisonTestReactive <- reactive({
 
 if(input$factorVar && input$compTest && input$run){
     
     compTestResult = result()$testResult$testResults
     
 }else{
     
     compTestResult = NULL
     
 }
 
 
 })
 
 output$comparisonTestResults <- DT::renderDataTable({
     
     datatable(comparisonTestReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
     ))
   
 })
 
 ###################### Life Table (end) #######################################################
 ###############################################################################################
 ###################### Kaplan-Meier (start) ###################################################


output$descriptivesTextKM <- renderText({
    if (input$runKM && input$caseSummaryKM){
        'Table 1: Descriptives'
    }
})

output$survivalTableTextKM <- renderText({
    if (input$runKM && input$survivalTable){
        'Table 2: Survival Table'
    }
})


output$meanMedianSurvivalTimesText <- renderText({
    if (input$runKM && input$meanMedianSurvivalTimes){
        'Table 3: Mean and Median Life Time'
    }
})

#output$quartilesOfSurvivalTimesText <- renderText({
#    if (input$runKM && input$quartilesOfSurvivalTimes){
#        'Table 4: Quartiles of Survival Times'
#    }
#})



output$hrTextKM <- renderText({
    if (input$runKM && input$hrKM){
        'Table 4: Hazard Ratio'
    }
})

output$compTestTextKM <- renderText({
    if (input$runKM && input$compTestKM && input$factorVarKM){
        'Table 5: Comparison Test'
    }
})


resultKM <- reactive({
    
    if(input$runKM){
        dataSet = dataM()
        
         if(input$factorVarKM){
             
             fctr = input$factorKM
         }else{fctr = NULL}
         
            
            km = kaplanMeier (survivalTime = input$survivalTimeKM, statusVariable  = input$statusVariableKM, status = input$statusKM, factors = fctr, survivalTable = TRUE, caseSummary = input$caseSummaryKM, hr=input$hrKM,
            meanMedianSurvivalTimes = input$meanMedianSurvivalTimes, quartilesOfSurvivalTimes = FALSE, ci = input$ciKM,
            varianceEstimation = input$varianceEstimationKM, comparisonTest = input$comparisonTestKM, confidenceLevel = input$confidenceLevelKM,
            referenceCategory = input$refCategoryKM, typeOfTest = "asymptotic", data = dataSet)

        
        km
    }
    
})


descriptivesReactiveKM <- reactive({
    
    if(input$runKM && input$caseSummaryKM){
        
        desc = resultKM()$tableResult$caseSummary
        
        if(!is.null(desc)){
            if(input$factorVarKM){
                descs = do.call(rbind.data.frame, desc)
                
            }else{
                
                descs = desc
                
            }
        }else{descs = NULL}
        
        descs
        
    }else{descs = NULL}
    
    
    
})


output$descriptivesKM <- DT::renderDataTable({
    
    datatable(descriptivesReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


survivalTableReactive <- reactive({
    
    if(input$runKM && input$survivalTable){
        stResult = resultKM()$testResult$survivalTable
        
        if(!is.null(stResult)){
            if(input$factorVarKM){
                stResults = do.call(rbind.data.frame, stResult)
                
            }else{
                
                stResults = stResult
                
            }
        }else{stResults = NULL}
        
        stResults
    }else{stResults = NULL}
    
    
    
})



output$survivaltableResult <- DT::renderDataTable({
    
    datatable(survivalTableReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

meanMedianSurvivalTimesReactive <- reactive({
    
    if(input$runKM){
        mst = resultKM()$tableResult$meanMedianSurvivalTimes
        
        if(!is.null(mst)){
            
                mstResults = mst
                
        }else{mstResults = NULL}
        
        rownames(mstResults) = mstResults$Factor
        mstResults[-1]
        
    }else{mstResults = NULL}
    
    
    
})


output$meanMedianSurvivalTimesResult <- DT::renderDataTable({
    
    datatable(meanMedianSurvivalTimesReactive(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


hazardRatioReactiveKM <- reactive({
    
    if(input$runKM){
        
        hrResult = resultKM()$testResult$hazardRatio
        
        if(!is.null(hrResult)){
            if(input$factorVarKM){
                hrResults = do.call(rbind.data.frame, hrResult)
                
            }else{
                
                hrResults = hrResult
                
            }
        }else{hrResults = NULL}
        
        hrResults
    }else{hrResults = NULL}
    
})


output$hazardRatioResultKM <- DT::renderDataTable({
    
    datatable(hazardRatioReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


comparisonTestReactiveKM <- reactive({
    
    if(input$factorVarKM && input$compTestKM && input$runKM){
        
        compTestResult = resultKM()$testResult$testResults
        
    }else{
        
        compTestResult = NULL
        
    }
    
    
})

output$comparisonTestResultsKM <- DT::renderDataTable({
    
    datatable(comparisonTestReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

###################### Kaplan-Meier (end) ###################################################
#############################################################################################
###################### Cox Regression (start) #################################################


output$displayCoefficientEstimatesCox <- renderText({
    if (input$runCox && input$displayCoefficientEstimates){
        'Table 1: Coefficient Estimates'
    }
})

output$hazardRatioCox <- renderText({
    if (input$runCox && input$hrcox){
        'Table 2: Hazard Ratio'
    }
})


output$goodnessOfFitTestsText <- renderText({
    if (input$runCox && input$goodnessOfFitTests){
        'Table 3: Goodness of Fit Tests'
    }
})

output$analysisOfDevianceCox <- renderText({
    if (input$runCox && input$analysisOfDeviance){
        'Table 4: Analysis of Deviance'
    }
})


output$storePredictionsCox <- renderText({
    if (input$runCox && input$storePredictions){
        'Table 5: Predictions'
    }
})

output$residualsCoxText <- renderText({
    if (input$runCox && input$residuals){
        'Table 6: Residuals'
    }
})

output$martingaleResidualsCoxText <- renderText({
    if (input$runCox && input$martingaleResiduals){
        'Table 7: Martingale Residuals'
    }
})

output$schoenfeldResidualsCoxText <- renderText({
    if (input$runCox && input$schoenfeldResiduals){
        'Table 8: Schoenfeld Residuals'
    }
})


output$dfBetasCoxText <- renderText({
    if (input$runCox && input$dfBetas){
        'Table 9: DfBetas'
    }
})


resultCox <- reactive({
    
    #if(input$runKM){
        dataSet = dataM()
        
        #if(input$factorVarKM){
            
        #    fctr = input$factorKM
        #}else{fctr = NULL}
        
        
        cox = coxRegression(survivalTime = input$survivalTimeCox, categoricalInput = input$categoricalInput, continuousInput = input$continuousInput, statusVariable = input$statusVariableCox, status = input$statusCox, displayDescriptives = TRUE, displayCoefficientEstimates = input$displayCoefficientEstimates, displayModelFit = TRUE, hazardRatio = input$hrcox, goodnessOfFitTests = input$goodnessOfFitTests, analysisOfDeviance = input$analysisOfDeviance,ties = input$ties, confidenceLevel = input$confidenceLevelCox, alternativeHypothesis = "equalToTestValue", modelSelectionCriteria = input$modelSelectionCriteria, modelSelection = input$modelSelection, alphaEnter = input$alphaToEnter, referenceCategory = input$refCategoryCox,alphaRemove = input$alphaToRemove, storePredictions = input$storePredictions, storeResiduals = input$residuals,storeMartingaleResiduals = input$martingaleResiduals, storeSchoenfeldResiduals = input$schoenfeldResiduals, storeDfBetas = input$dfBetas,data = dataSet)
        #,
        #
        #
        #
        #  data = dataSet)
        
        
        cox
        #}


})


#descriptivesReactiveCox <- reactive({
    
#    if(input$runCox && input$caseSummaryCox){
        
#        desc = resultKM()$tableResult$caseSummary
        
#        if(!is.null(desc)){
#            if(input$factorVarKM){
#                descs = do.call(rbind.data.frame, desc)
#
#            }else{
                
#               descs = desc
#
#            }
#        }else{descs = NULL}
        
#        descs
        
#    }else{descs = NULL}
    
    
    
#})


#output$descriptivesKM <- DT::renderDataTable({
    
#    datatable(descriptivesReactiveKM(), extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
#    dom = 'Bfrtip',
#    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
#    ))
    
#})


displayCoefficientEstimatesReactive <- reactive({
    
    if(input$runCox && input$displayCoefficientEstimates){
    
        coeffResults = resultCox()$testResult$displayCoefficientEstimatesResults
        
       }else{coeffResults = NULL}
    
    #coeffResults
    
})



output$displayCoefficientEstimatesResult <- DT::renderDataTable({
    
    datatable(displayCoefficientEstimatesReactive(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})





hazardRatioReactiveCox <- reactive({
    
    if(input$runCox && input$hrcox){
        
        hrResult = resultCox()$testResult$hazardRatioResults
        

    }else{hrResults = NULL}
  
  # hrResult
  
})


output$hazardRatioResultCox <- DT::renderDataTable({
    
    datatable(hazardRatioReactiveCox(), rownames= FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


goodnessOfFitTestsResultsReactiveCox <- reactive({
    
    if(input$runCox && input$goodnessOfFitTests){
        
            gof = resultCox()$testResult$goodnessOfFitTestsResults
            
    }else{gof = NULL}
    
    gof
})

output$goodnessOfFitTestsRes <- DT::renderDataTable({
    
    datatable(goodnessOfFitTestsResultsReactiveCox(), rownames=FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


analysisOfDevianceResultsReactiveCox <- reactive({
    
    if(input$runCox && input$analysisOfDeviance){
    
        aod = resultCox()$testResult$analysisOfDevianceResults
    
    }else{aod = NULL}
    
    aod
})

output$analysisOfDevianceRes <- DT::renderDataTable({
    
    datatable(analysisOfDevianceResultsReactiveCox(), rownames=FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

predictionsReactiveCox <- reactive({
    
    if(input$runCox && input$storePredictions){
    
        preds = resultCox()$testResult$Store$Predictions
    
    }else{preds = NULL}
    
    preds
})

output$predictionsCox <- DT::renderDataTable({
    
    datatable(predictionsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


residualsReactiveCox <- reactive({
    
    if(input$runCox && input$residuals){
        
        residuals = resultCox()$testResult$Store$Residuals
    
    }else{residuals = NULL}
    
    residuals
})

output$residualsCox <- DT::renderDataTable({
    
    datatable(residualsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

martingaleResidualsReactiveCox <- reactive({
    
    if(input$runCox && input$martingaleResiduals){
        
        martingale = resultCox()$testResult$Store$MartingaleResiduals
    
    }else{martingale = NULL}
    
    martingale
})

output$martingaleResidualsCox <- DT::renderDataTable({
    
    datatable(martingaleResidualsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})

schoenfeldResidualsReactiveCox <- reactive({
    
    if(input$runCox && input$schoenfeldResiduals){
        
        schoenfeld = resultCox()$testResult$Store$SchoenfeldResiduals
    
    }else{schoenfeld = NULL}
    
    schoenfeld
})

output$schoenfeldResidualsCox <- DT::renderDataTable({
    
    datatable(schoenfeldResidualsReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})



dfBetasReactiveCox <- reactive({
    
    if(input$runCox && input$dfBetas){
    
            dfbetas = resultCox()$testResult$Store$DfBetas
        
    }else{dfbetas = NULL}
    
    dfbetas
})

output$dfBetasCox <- DT::renderDataTable({
    
    datatable(dfBetasReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})




phAssumptionReactiveCox <- reactive({
    
    if(input$runCox){
        
        ph = resultCox()$testResult$displayCoxPh
        
    }else{ph = NULL}
    
    ph
})

output$phAssumptionCox <- DT::renderDataTable({
    
    datatable(phAssumptionReactiveCox(), extensions = c('Buttons','KeyTable','Responsive'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), keys = TRUE
    ))
    
})


output$phPlot <- renderPlot({
    
    #if(input$runCox){
        # n_row = nrow(resultCox()$testResult$displayCoxPh)
    
        #par(mfrow=c(n_row, 1))
        #  lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
    
        resultCox()$plotResult
    #}
})


###################### Cox Regression (end) ###################################################

 output$str <- renderPrint({
     
     dim(resultCox()$testResult$displayCoxPh)[1]*100

 })
 
 
 
})





