library("shinythemes")
library("shinyBS")



    shinyUI(
    fluidPage(
    theme = "css/mytheme.css",
    sidebarPanel(width=3,


        conditionalPanel(condition="input.tabs1=='About'"


        ),


        conditionalPanel(condition="input.tabs1=='Data Upload'",

            h4("Input data"),
            radioButtons("dataInput", "", list("Upload a file"=2, "Load example data"=1), selected=2),


            conditionalPanel(condition="input.dataInput=='1'",
            h5("Load example data:"),

            radioButtons("sampleData", "", list("Example data (Life Table and Kaplan-Meier)"=1, "Example data2 (Cox Regression)"=2), selected=1)
        ),

            conditionalPanel(condition="input.dataInput=='2'",
            h5("Upload a delimited text file: "),

            fileInput("upload", "", multiple = FALSE),

            radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2),

            HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
            HTML('<p>Note: First row must be header.</p>')
        )

        ),

################################# Life Table (start) #####################################

        conditionalPanel(condition="input.tabs1=='Life Table'",

        checkboxInput(inputId = "inputs", label = "Inputs", value = TRUE),
          conditionalPanel(condition = "input.inputs",

            selectizeInput("survivalTime", "Survival time", choices = NULL, multiple = FALSE),

            selectizeInput("statusVariable", "Name of the status variable", choices = NULL, multiple = FALSE),

            selectizeInput("status", "Value of the status:", choices = NULL, multiple = FALSE),

            checkboxInput(inputId = "factorVar", label = "Factor variable", value = TRUE),
                conditionalPanel(condition = "input.factorVar",
                selectizeInput("factor", "Factor", choices = NULL, multiple = FALSE)
        ),


        fluidRow(column(4,numericInput("from", "From", value = 0)),
            column(4,numericInput("to", "To", value = 60)),
            column(4,numericInput("by", "By", value = 12))
        ),

        checkboxInput(inputId = "advancedOptions", label = "Advanced Options", value = FALSE),
          conditionalPanel(condition = "input.advancedOptions",

            selectizeInput("ci", "Confidence interval type", choices = list("Log" = "log", "Log-Log" = "log-log", "Plain" = "plain", "None" = "none"), multiple = FALSE),

            selectizeInput("varianceEstimation", "Variance estimation", choices = list("Greenwood" = "greenwood", "Tsiatis" = "tsiatis"), multiple = FALSE),

           selectizeInput("comparisonTest", "Comparison test", choices = list("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow", "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modPetoPeto", "Flemington-Harnington" = "flemingtonHarnington"), multiple = FALSE),

            numericInput("confidenceLevel", "Confidence level", value = 95, min = 0, max = 100),

            radioButtons("refCategory", "Reference category", choices = list("First" = "first", "Last" = "last"))


            )),

            checkboxInput(inputId = "outputs", label = "Outputs", value = FALSE),
                conditionalPanel(condition = "input.outputs",


                checkboxInput(inputId = "caseSummary", label = "Case summary", value = TRUE),
                checkboxInput(inputId = "lifeTable", label = "Life table", value = TRUE),
                checkboxInput(inputId = "medianLifeTime", label = "Median life time", value = TRUE),
                checkboxInput(inputId = "hr", label = "Hazard ratio", value = TRUE),
                checkboxInput(inputId = "compTest", label = "Comparison test", value = TRUE)


            ),

            actionButton(inputId = "run", label = "Run", icon = NULL)

),

################################# Life Table (end) #####################################
########################################################################################
################################# Kaplan-Meier (start) #################################

      conditionalPanel(condition="input.tabs1=='Kaplan-Meier'",

checkboxInput(inputId = "inputsKM", label = "Inputs", value = TRUE),
conditionalPanel(condition = "input.inputsKM",

selectizeInput("survivalTimeKM", "Survival time", choices = NULL, multiple = FALSE),

selectizeInput("statusVariableKM", "Name of the status variable", choices = NULL, multiple = FALSE),

selectizeInput("statusKM", "Value of the status:", choices = NULL, multiple = FALSE),

checkboxInput(inputId = "factorVarKM", label = "Factor variable", value = TRUE),
conditionalPanel(condition = "input.factorVarKM",
selectizeInput("factorKM", "Factor", choices = NULL, multiple = FALSE)
),

checkboxInput(inputId = "advancedOptionsKM", label = "Advanced Options", value = FALSE),
conditionalPanel(condition = "input.advancedOptionsKM",

selectizeInput("ciKM", "Confidence interval type", choices = list("Log" = "log", "Log-Log" = "log-log", "Plain" = "plain", "None" = "none"), multiple = FALSE),

selectizeInput("varianceEstimationKM", "Variance estimation", choices = list("Greenwood" = "greenwood", "Tsiatis" = "tsiatis"), multiple = FALSE),

selectizeInput("comparisonTestKM", "Comparison test", choices = list("Log-rank" = "logRank", "Gehan-Breslow" = "gehanBreslow", "Tarone-Ware" = "taroneWare", "Peto-Peto" = "petoPeto", "Modified Peto-Peto" = "modPetoPeto", "Flemington-Harnington" = "flemingtonHarnington"), multiple = FALSE),

numericInput("confidenceLevelKM", "Confidence level", value = 95, min = 0, max = 100),

radioButtons("refCategoryKM", "Reference category", choices = list("First" = "first", "Last" = "last"))


)),



checkboxInput(inputId = "outputsKM", label = "Outputs", value = FALSE),
conditionalPanel(condition = "input.outputsKM",


checkboxInput(inputId = "caseSummaryKM", label = "Case summary", value = TRUE),
checkboxInput(inputId = "survivalTable", label = "Life table", value = TRUE),
checkboxInput(inputId = "meanMedianSurvivalTimes", label = "Median life time", value = TRUE),
#checkboxInput(inputId = "quartilesOfSurvivalTimes", label = "Quartiles of survival times", value = TRUE),
checkboxInput(inputId = "hrKM", label = "Hazard ratio", value = TRUE),
checkboxInput(inputId = "compTestKM", label = "Comparison test", value = TRUE)


),

actionButton(inputId = "runKM", label = "Run", icon = NULL)




        ),
################################# Kaplan-Meier (end) ###################################
########################################################################################
################################# Cox Regression (start) ###############################

      conditionalPanel(condition="input.tabs1=='Cox Regression'",
            checkboxInput(inputId = "inputsCox", label = "Inputs", value = TRUE),
            conditionalPanel(condition = "input.inputsCox",
            selectizeInput("survivalTimeCox", "Survival time", choices = NULL, multiple = FALSE),
            selectizeInput("statusVariableCox", "Name of the status variable", choices = NULL, multiple = FALSE),
            selectizeInput("statusCox", "Value of the status:", choices = NULL, multiple = FALSE),
            selectizeInput("categoricalInput", "Categorical variable(s)", choices = NULL, multiple = TRUE),
            selectizeInput("continuousInput", "Continuous variable(s)", choices = NULL, multiple = TRUE),
            checkboxInput(inputId = "advancedOptionsCox", label = "Advanced Options", value = FALSE),
            conditionalPanel(condition = "input.advancedOptionsCox",
            radioButtons("modelSelectionCriteria", "Model selection criteria", choices = list("AIC" = "aic", "p value" = "pValue"), selected = "aic"),
            conditionalPanel(condition = "input.modelSelectionCriteria == 'pValue'",
            numericInput("alphaToEnter", "Alpha to enter", value = 0.05, min = 0, max = 1, step = 0.05),
            numericInput("alphaToRemove", "Alpha to remove", value = 0.10, min = 0, max = 1, step = 0.05)
            ),
            selectizeInput("modelSelection", "Model selection", choices = list("Enter" = "enter", "Backward" = "backward", "Forward" = "forward", "Stepwise" = "stepwise"), selected = "stepwise"),
            numericInput("confidenceLevelCox", "Confidence level", value = 95, min = 0, max = 100),
            radioButtons("refCategoryCox", "Reference category", choices = list("First" = "first", "Last" = "last")),
            selectizeInput("ties", "Ties", choices = list("Efron" = "efron", "Breslow" = "breslow", "Exact" = "exact"), multiple = FALSE)

            )),
            checkboxInput(inputId = "outputsCox", label = "Outputs", value = FALSE),
            conditionalPanel(condition = "input.outputsCox",
#checkboxInput(inputId = "displayDescriptives", label = "Descriptives", value = TRUE),
            checkboxInput(inputId = "displayCoefficientEstimates", label = "Coefficient estimates", value = TRUE),
#checkboxInput(inputId = "displayModelFit", label = "Model fit", value = TRUE),
            checkboxInput(inputId = "hrcox", label = "Hazard ratio", value = TRUE),
            checkboxInput(inputId = "goodnessOfFitTests", label = "Goodness of fit tests", value = TRUE),
            checkboxInput(inputId = "analysisOfDeviance", label = "Analysis of deviance", value = FALSE),
            checkboxInput(inputId = "storePredictions", label = "Predictions", value = FALSE),
            checkboxInput(inputId = "residuals", label = "Residuals", value = FALSE),
            checkboxInput(inputId = "martingaleResiduals", label = "Martingale Residuals", value = FALSE),
            checkboxInput(inputId = "schoenfeldResiduals", label = "SchoenfeldResiduals", value = FALSE),
            checkboxInput(inputId = "dfBetas", label = "DfBetas", value = FALSE)
        ),
            actionButton(inputId = "runCox", label = "Run", icon = NULL)
    )

),

	mainPanel(

navbarPage("compSurv: Complete Survival Analysis v.0.3", id="tabs1", inverse = TRUE, collapsible = TRUE, fluid = TRUE, position = "fixed-top", class("navbar navbar-inverse"),
#tabsetPanel(

            tabPanel("About",

            br(),
            h5('An interactive tool for survival analysis!')

            ),

            tabPanel("Data Upload",

                DT::dataTableOutput('dataUpload')

            ),

            tabPanel("Life Table",

                h4(textOutput(outputId = "descriptivesText")),
                DT::dataTableOutput('descriptives'),

                h4(textOutput(outputId = "lifeTableText")),
                DT::dataTableOutput('lifetableResult'),

                h4(textOutput(outputId = "medianLifeTimeText")),
                DT::dataTableOutput('medianLifeTimeResult'),

                h4(textOutput(outputId = "hrText")),
                DT::dataTableOutput('hazardRatioResult'),

                h4(textOutput(outputId = "compTestText")),
                DT::dataTableOutput('comparisonTestResults')

#verbatimTextOutput('lifeTable'),
#verbatimTextOutput('str')

            ),

            tabPanel("Kaplan-Meier",
            h4(textOutput(outputId = "descriptivesTextKM")),
            DT::dataTableOutput('descriptivesKM'),

            h4(textOutput(outputId = "survivalTableTextKM")),
            DT::dataTableOutput('survivaltableResult'),

            h4(textOutput(outputId = "meanMedianSurvivalTimesText")),
            DT::dataTableOutput('meanMedianSurvivalTimesResult'),

            h4(textOutput(outputId = "hrTextKM")),
            DT::dataTableOutput('hazardRatioResultKM'),

            h4(textOutput(outputId = "compTestTextKM")),
            DT::dataTableOutput('comparisonTestResultsKM')

#verbatimTextOutput('str')
            ),

            tabPanel("Cox Regression",

                tabsetPanel(
                    tabPanel('Model',


                h4(textOutput(outputId = "displayCoefficientEstimatesCox")),
                DT::dataTableOutput('displayCoefficientEstimatesResult'),

                h4(textOutput(outputId = "hazardRatioCox")),
                DT::dataTableOutput('hazardRatioResultCox'),

                h4(textOutput(outputId = "goodnessOfFitTestsText")),
                DT::dataTableOutput('goodnessOfFitTestsRes'),

                h4(textOutput(outputId = "analysisOfDevianceCox")),
                DT::dataTableOutput('analysisOfDevianceRes'),

                h4(textOutput(outputId = "storePredictionsCox")),
                DT::dataTableOutput('predictionsCox'),

                h4(textOutput(outputId = "residualsCoxText")),
                DT::dataTableOutput('residualsCox'),

                h4(textOutput(outputId = "martingaleResidualsCoxText")),
                DT::dataTableOutput('martingaleResidualsCox'),

                h4(textOutput(outputId = "schoenfeldResidualsCoxText")),
                DT::dataTableOutput('schoenfeldResidualsCox'),

                h4(textOutput(outputId = "dfBetasCoxText")),
                DT::dataTableOutput('dfBetasCox')


),

        tabPanel('Proportional Hazard Assumption',

                DT::dataTableOutput('phAssumptionCox'),

                plotOutput('phPlot')

#verbatimTextOutput('str')


)



)


            ),

            tabPanel("Help"

            ),

            tabPanel("Authors"

            )
    ),

tags$head(
tags$style("body {padding-top: 58px};")

),

tags$head(
        tags$link(rel = "shortcut icon", href = "favicon-2.ico"))

 )
))#)




