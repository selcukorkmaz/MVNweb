shinyUI(pageWithSidebar(


	titlePanel(title="MVN: a web-tool for assessing multivariate normality (ver. 1.6)"),

	sidebarPanel(
		conditionalPanel(condition="input.tabs1=='Introduction'",
            HTML('<p align="center"><img src="GaussianFunction3D.png" width=200 height=200></p>'),
            tags$head(includeScript("google-analytics.js"))

        ),

        conditionalPanel(condition="input.tabs1=='Manual'",
            HTML('<p align="center"><img src="manual.png" width=200 height=200></p>')

        ),

        conditionalPanel(condition="input.tabs1=='News'",
            HTML('<p align="center"><img src="news.png" width=200 height=200></p>')

        ),

        conditionalPanel(condition="input.tabs1=='Citation'",
            HTML('<p align="center"><img src="cite.png" width=200 height=200></p>')
        ),

        conditionalPanel(condition="input.tabs1=='Outlier detection'",
            h5("Select a detection method"),
            radioButtons("outlierDetect", "", list("Mahalanobis"=1, "Adjusted Quantile"=2)), # PCOut removed.


        conditionalPanel(condition = "input.firstLast != '0'",
            selectizeInput("subsetOut", "Select a sub-group for Q-Q plot", choices = NULL, multiple = FALSE)
        ),



        checkboxInput(inputId = "advanced", label = "Advanced Options", value = FALSE),
        conditionalPanel(condition = "input.advanced",

        fluidRow(column(5, tags$div(title="A numeric parameter controlling the size of the subsets over which the determinant is minimized. Allowed values for the alpha are between 0.5 and 1.", numericInput(inputId = "alphaVal", label = "Alpha", value = 0.5, min = 0.5, max = 1, step = 0.01))),
        column(2),
        column(5, tags$div(title="A numeric tolerance value which isused for inversion of the covariance matrix (default = 1e-25).", numericInput(inputId = "tol", label = "Tolerance", value = 1e-25, step = 1e-50)))
        ),

        checkboxInput(inputId = "label", label = "Outlier Labels", value = FALSE),
        conditionalPanel(condition = "input.label",

        fluidRow(column(5, tags$div(title="A position specifier for the labels.",selectizeInput("position", "Position", choices = c("Below"=1,"Left"=2,"Above"=3,"Right"=4), multiple = FALSE, selected=1))),
        column(2),
        column(5, tags$div(title="This value gives the offset of the label from the specified coordinate in fractions of a character width.", numericInput(inputId = "offset", label = "Offset", value = 0.5, step = 0.01)))
        ))

        ),

        HTML('<br>'),

        fluidRow(column(5, sliderInput("myheight", "Plot height:", value=450, min=200, max=1200 )),
            column(2),
        column(5, sliderInput("mywidth", "Plot width:", value=500, min=200, max=1200 ))
        ),

        conditionalPanel(condition="input.outlierDetect=='1'",
            HTML('<br>'),
            helpText("This methodology has following steps:"),
            helpText("1. Compute robust Mahalanobis distances (MD(xi))"),
            helpText("2. Compute the 97.5%-Quantile Q of the Chi-Square distribution"),
            helpText("3. Declare MD(xi) > Q as possible outlier")
        ),

        conditionalPanel(condition="input.outlierDetect=='2'",
            HTML('<br>'),
            helpText("This methodology has following steps:"),
            helpText("1. Compute robust Mahalanobis distances (MD(xi))"),
            helpText("2. Compute the 97.5% Adjsuted Quantile (AQ) of the Chi-Square distribution"),
            helpText("3. Declare MD(xi) > AQ as possible outlier")
        ),

        conditionalPanel(condition="input.outlierDetect=='3'",
            HTML('<br>'),
            HTML('<p>Detailed information about this method can be found in <a href="http://www.sciencedirect.com/science/article/pii/S0167947307002204" target="_blank"> Filzmoser et al</a></p>'),
            HTML('<p>See also <a href="http://cran.r-project.org/package=mvoutlier" target="_blank"> mvoutlier</a> package from R</p>')
            )

        ),


        conditionalPanel(condition="input.tabs1=='Authors & News'",
        HTML('<p align="center"> <a href="https://www.hacettepe.edu.tr/english/" target="_blank"><img src="hulogo.JPEG" width=150 height=150></a> </p>')

        ),

		conditionalPanel(condition="input.tabs1=='Data upload'",
			h4("Input data"),
			radioButtons("dataInput", "", list("Load example data"=1,"Upload a file"=2,"Paste your data"=3), selected=1),

            tags$div(title="If data set has a group/output/label/class variable, plesase define whether this variable is in the first or last column, then the analysis will be performed in each sub-group.", radioButtons("firstLast", "Group variable", list("None"=0, "First column"=1, "Last column"=2),selected=0)),

			conditionalPanel(condition="input.dataInput=='1'",
				h5("Load example data:"),
				
				radioButtons("sampleData", "", list("Bivariate normal data set (n=1000, p=2)"=1, "Iris data set (n=50, p=2)"=2, "Iris data set (n=150, without Petal.Width)"=3),selected=2),
				
                HTML('<p>n: number of observations</p>'),
				HTML('<p>p: number of variables</p>')
			),
			
			conditionalPanel(condition="input.dataInput=='2'",
				h5("Upload a delimited text file: "),

				fileInput("upload", "", multiple = FALSE),
                #checkboxInput("groupVar", "Group variable (default is last column)", TRUE),
                #conditionalPanel(condition="input.groupVar",
                #radioButtons("firstLast", "", list("None"=0, "First column"=1, "Last column"=2),selected=2)),
				
				#radioButtons("firstLastUpload", "Group variable (default is none)", list("None"=0, "First column"=1, "Last column"=2),selected=0),  ## firstLast variable for uploaded data.
				#radioButtons("firstLast", "Group variable(default is last column)", list("None"=0, "First column"=1, "Last column"=2),selected=2),
				radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2),  

				HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
                HTML('<p>Note: First row must be header.</p>')
			),
			conditionalPanel(condition="input.dataInput=='3'",
				h5("Paste or enter your data below:"),
				tags$textarea(id="myData", rows=10, cols=5, ""),
				actionButton('clearText_button','Clear data'),
				HTML('<br>'),
				HTML('<br>'),
				
                #radioButtons("firstLastPaste", "Group variable (default is none)", list("None"=0, "First column"=1, "Last column"=2),selected=0), ## firstLast variable for pasted data.
                #radioButtons("firstLast", "Group variable(default is last column)", list("None"=0, "First column"=1, "Last column"=2),selected=2),
				radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3), selected=2),
                HTML('<p>You can paste or manually enter your data as separated by comma, tab or semicolon.</p>'),
                HTML('<p>Note: First row must be header.</p>')
			)
		),


        conditionalPanel(condition="input.tabs1=='Univariate analysis'",

            conditionalPanel(condition = "input.firstLast != '0'",
                selectizeInput("subsetUni", "Select a sub-group for tests and plots", choices = NULL, multiple = FALSE)
            ),

            h5("Choose a univariate normality test"),
            selectizeInput("normTest", "", choices = c("Shapiro-Wilk"="SW", "Cramer-von Mises"="CVM", "Lilliefors"="Lillie", "Shapiro-Francia"="SF", "Anderson-Darling"="AD"), multiple = FALSE, selected = "SW"),
            HTML('<br>'),
            h5("Choose a univariate plot"),
            selectizeInput("normPlot", "", choices = c("Q-Q plot"="qqplot", "Histogram"="histogram", "Box-plot "="box", "Scatterplot matrix"="scatter"), multiple = FALSE, selected = "qqplot"),

            conditionalPanel(condition="input.normPlot=='box'",
                helpText("Note: Box-plots are based on standardized values (centered and scaled).")
            ),

            HTML('<br>'),
            fluidRow(column(5,sliderInput("myheightUni", "Plot height:", value=400, min=200, max=1200 )),
            column(2),
            column(5,sliderInput("mywidthUni", "Plot width:", value=600, min=200, max=1200))
        )),




		conditionalPanel(condition="input.tabs1=='Multivariate analysis'",

        conditionalPanel(condition = "input.firstLast != '0'",
            selectizeInput("subset", "Select a sub-group for MVN tests and plots", choices = NULL, multiple = FALSE)
        ),

            h5("Choose a MVN test"),
			radioButtons("testType", "", list("Mardia"=0, "Henze-Zirkler"=1, "Royston"=2)),

        conditionalPanel(condition="input.testType=='0'",
            helpText("Values:"),
            helpText("g1p: Mardia's multivariate skewness statistic"),
            helpText("chi.skew: Chi-square value of the skewness statistic"),
            helpText("p.value.skew: p-value of the skewness statistic"),
            helpText("g2p: Mardia's multivariate kurtosis statistic"),
            helpText("z.kurtosis: z value of thekurtosis statistic"),
            helpText("p.value.kurt: p-value of the kurtosis statistic"),
            helpText("chi.small.skew: Chi-square value of the small sample skewness statistic"),
            helpText("p.value.small: p-value of small sample skewness statistic")
        ),

        conditionalPanel(condition="input.testType=='1'",
            helpText("Values:"),
            helpText("HZ: the value of Henze-Zirkler statistic at significance level 0.05"),
            helpText("p-value: significance value for the HZ test")
        ),

        conditionalPanel(condition="input.testType=='2'",
            helpText("Values:"),
            helpText("H: the value of Royston's H statistic at significance level 0.05"),
            helpText("p-value: an approximate p-value for the test with respect to equivalent degrees of freedom (edf)")
        ),
        HTML('<br>'),



            h5("Choose a MVN plot"),
            radioButtons("plotType", "", list("Q-Q"=0, "Perspective"=1, "Contour"=2)),

        conditionalPanel(condition="input.plotType=='1'",

        fluidRow(column(2, tags$div(title="Default options",checkboxInput(inputId = "defaultPersp", label = "Default", value = TRUE))),
        column(2),
        column(2, tags$div(title="Advanced options", checkboxInput(inputId = "persOpt", label = "Advanced", value = FALSE)))),



        conditionalPanel(condition = "input.persOpt",




        fluidRow(column(5, tags$div(title="Angles defining the viewing direction. It gives the azimuthal direction.",numericInput(inputId = "theta", label = "Theta", value = 1, step = 1))),
        column(2),
        column(5, tags$div(title="Angles defining the viewing direction. It gives the colatitude.",numericInput(inputId = "phi", label = "Phi", value = 30, step = 1)))),



        fluidRow(column(5, tags$div(title="The distance of the eyepoint from the centre of the plotting box.",numericInput(inputId = "r", label = "r", value = 1.5, step = 0.01))),
        column(2),
        column(5, tags$div(title="A value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it.",numericInput(inputId = "d", label = "d", value = 1, step = 1)))),

        fluidRow(column(5, tags$div(title="Before viewing the x, y and z coordinates of the points defining the surface are transformed to the interval [0,1]. If scale is TRUE the x, y and z coordinates are transformed separately. If scale is FALSE the coordinates are scaled so that aspect ratios are retained. This is useful for rendering things like DEM information.",checkboxInput(inputId = "scale", label = "Scale", value = TRUE))),
        column(2),
        column(5, tags$div(title="An expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction.",numericInput(inputId = "expand", label = "Expand", value = 1, step = 0.1)))),


        fluidRow(column(5, tags$div(title="The color(s) of the surface facets. Transparent colours are ignored. This is recycled to the (nx-1)(ny-1) facets.",textInput(inputId = "col", label = "Colour", value = "white"))),
        column(2),
        column(5, tags$div(title="The color of the line drawn around the surface facets. The default, NULL, corresponds to par('fg'). A value of NA will disable the drawing of borders: this is sometimes useful when the surface is shaded.",selectInput(inputId = "border", label = "Border", choices = c(NULL, "NA"), selected = NULL)))),


        fluidRow(column(5, tags$div(title="If finite values are specified for ltheta and lphi, the surface is shaded as though it was being illuminated from the direction specified by azimuth ltheta and colatitude lphi.",numericInput(inputId = "ltheta", label = "ltheta", value = -135, step = 1))),
        column(2),
        column(5, tags$div(title="If finite values are specified for ltheta and lphi, the surface is shaded as though it was being illuminated from the direction specified by azimuth ltheta and colatitude lphi.",numericInput(inputId = "lphi", label = "lphi", value = 0, step = 1)))),

        fluidRow(column(5, tags$div(title="x-axis label",textInput(inputId = "xlab", label = "x-axis label", value = "x-axis label"))),
        column(2),
        column(5, tags$div(title="y-axis label",textInput(inputId = "ylab", label = "y-axis label", value = "y-axis label")))),

        fluidRow(column(5, tags$div(title="z-axis label",textInput(inputId = "zlab", label = "z-axis label", value = "z-axis label"))),
        column(2),
        column(5, tags$div(title="Main and sub title",textInput(inputId = "main", label = "Main Title", value = "Main Title"))))


    )),

        conditionalPanel(condition="input.plotType=='2'",

        fluidRow(column(2, tags$div(title="Default options",checkboxInput(inputId = "defaultCon", label = "Default", value = TRUE))),
        column(2),
        column(2, tags$div(title="Advanced options", checkboxInput(inputId = "conOpt", label = "Advanced", value = FALSE)))),


        conditionalPanel(condition = "input.conOpt",


        fluidRow(column(5, tags$div(title="Number of contour levels desired.",numericInput(inputId = "nlevels", label = "nlevels", value = 20, step = 1))),
        column(2),
        column(5, tags$div(title="cex for contour labelling. This is an absolute size, not a multiple of par('cex').",numericInput(inputId = "labcex", label = "labcex", value = 0.6, min =0.1, step = 0.1)))),


        fluidRow(column(5, tags$div(title="Contours are labelled if TRUE.",checkboxInput(inputId = "drawlabels", label = "Draw Labels", value = TRUE))),
        column(2),
        column(5, tags$div(title="Logical indicating whether axes or a box should be drawn.",checkboxInput(inputId = "axesCon", label = "Axes", value = TRUE)))),

        fluidRow(column(5, tags$div(title="x-axis label",textInput(inputId = "xlabCon", label = "x-axis label", value = "x-axis label"))),
        column(2),
        column(5, tags$div(title="y-axis label",textInput(inputId = "ylabCon", label = "y-axis label", value = "y-axis label")))),


        fluidRow(column(5, tags$div(title="color for the lines drawn.",textInput(inputId = "colCon", label = "Colour", value = "black"))),
        column(2),
        column(5, tags$div(title="Specifying where the labels will be located.",selectInput(inputId = "methodCon", label = "Method", choices = c("simple", "edge", "flattest"), selected =  "flattest")))),


        fluidRow(column(5, tags$div(title="Line type for the lines drawn.",numericInput(inputId = "ltyCon", label = "lty", value = 1, step = 1))),
        column(2),
        column(5, tags$div(title="Line width for the lines drawn.",numericInput(inputId = "lwdCon", label = "lwd", value = 1, step = 1))))


        )),





        HTML('<br>'),

        fluidRow(column(5, sliderInput("myHeight", "Plot height:", value=450, min=200, max=1200 )),
            column(2),
        column(5,sliderInput("myWidth", "Plot width:", value=450, min=200, max=1200 ))
        )


    )

    ),




	mainPanel(
		tabsetPanel(
			tabPanel("Introduction",
                    HTML('<p align="justify">Assessing the assumption of multivariate normality is required by many parametric multivariate statistical methods, such as MANOVA, linear discriminant analysis, principal component analysis, canonical correlation, etc. It is important to assess multivariate normality in order to proceed with such statistical methods. There are many analytical methods proposed for checking multivariate normality. However, deciding which method to use is a challenging process, since each method may give different results under certain conditions. Hence, we may say that there is no best method, which is valid under any condition, for normality checking. In addition to numerical results, it is very useful to use graphical methods to decide on multivariate normality. Combining the numerical results from several methods with graphical approaches can be useful and provide more reliable decisions.</p>'),
                    HTML('<p align="justify">Here, we present a web-tool application to assess multivariate normality. This application uses the <a href="http://cran.r-project.org/web/packages/MVN/index.html" target="_blank"> MVN</a> package from R. This tool contains the three most widely used multivariate normality tests, including Mardia’s, Henze-Zirkler’s and Royston’s, and graphical approaches, including chi-square Q-Q, perspective and contour plots (<i>Multivariate analysis tab</i>). It also includes two multivariate outlier detection methods, which are based on robust Mahalanobis distances (<i>Outlier detection tab</i>). Moreover, this web-tool performs the univariate normality of marginal distributions through both tests and plots (<i>Univariate analysis tab</i>). More detailed information about the tests, graphical approaches and their implementations through this web-tool and MVN package can be found in the <a href="http://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf" target="_blank">paper of the package</a>. All source codes are in <a href="https://github.com/selcukorkmaz/MVNweb" target="_blank">GitHub</a>.</p>'),

                    HTML('<p><div align="center"><table cellpadding="0" cellspacing="0"><tr><td><img src="persp.png" width="250" height="250" border="0"></td><td><img src="contour.png" width="200" height="200" border="0"></td><td><img src="hist.png" width="350" height="350" border="0"></td><td><img src="outlier.png" width="250" height="250" border="0"><td></td></tr></table></div></p>'),

                    h6("If you use this tool for your research please cite: Korkmaz S, Goksuluk D, Zararsiz G. MVN: An R Package for Assessing Multivariate Normality. The R Journal. 2014 6(2):151-162.")



			),

			tabPanel("Data upload",
                navbarPage(
                               title = '',
                               tabPanel('Data',                dataTableOutput('RawData'))
                 )
            ),


            tabPanel("Univariate analysis",

                downloadButton("downloadNormTest", "Download univariate results"),
                downloadButton("downloadNormPlot", "Download univariate plots"),

                verbatimTextOutput("uniTest"),
                plotOutput("uniPlot")

                ),




            tabPanel("Outlier detection",
                downloadButton("downloadNewData", "Download new data set as txt-file"),
                downloadButton("downloadOutlier", "Download outlier set as txt-file"),
                downloadButton("downloadOutlierPlot", "Download plot as pdf-file"),
                navbarPage(
                    title = '',
                    tabPanel('Outliers',            dataTableOutput('OutlierData')),
                    tabPanel('New Data',            dataTableOutput('NewData'))
                ),
                plotOutput("outlierPLOT")
                #verbatimTextOutput("outliers")
                ),

            tabPanel("Multivariate analysis",
                downloadButton("downloadMVNresult", "Download results as txt-file"),
                downloadButton("downloadPlotPDF", "Download plot as pdf-file"),
                verbatimTextOutput("MVN"),
                plotOutput("mvnPLOT")
			),

            tabPanel("Manual",

            h5("Usage of the web-tool"),
            HTML('<p>In order to use this application,</p>'),
            HTML('<p> (i) load your data set using <b>Data upload</b> tab. If data set has a group variable, users can define whether this variable is in the first or last column then the analysis will be performed in each sub-group,</p>'),
            HTML('<p> (ii) check univariate normality through univariate normality tests and plots in the <b>Univariate analysis</b> tab. Users also can get descriptive statistics using this tab, </p>'),
            HTML('<p> (iii) check multivariate outliers in the <b>Outlier detection</b> tab, </p>'),
            HTML('<p> (iv) check multivariate normality through MVN tests and plots in the <b>Multivariate analysis</b> tab.</p>'),
            HTML('<p align="justify"> Users can download univariate results (both descriptive statistics and univariate normality tests, as txt) and univariate plots (as pdf) from <b>Univariate analysis</b> tab, outlier set (as txt), data set without outliers (as txt) and chi-square QQ plot (as pdf) from <b>Outlier detection</b> tab, also MVN test results (as txt) and plots (as pdf or png) can be downloaded by using <b>Multivariate analysis</b> tab. </p>'),
            HTML('<p> <b>Please note that box-plots are based on standardized values (centered and scaled), and perspective and contour plots are only available for bivariate normal distributions.</b></p>'),
            HTML('<p> <b>If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.</b></p>')
            ),

			tabPanel("Authors & News",
                h4("Authors"),
                HTML('<p><a href="http://yunus.hacettepe.edu.tr/~selcuk.korkmaz/" target="_blank"> <b>Selcuk Korkmaz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:selcuk.korkmaz@hacettepe.edu.tr" target="_blank">selcuk.korkmaz@hacettepe.edu.tr</a><p>'),
                HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Dincer_Goksuluk_CV_Eng.pdf" target="_blank"> <b>Dincer Goksuluk</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:dincer.goksuluk@hacettepe.edu.tr" target="_blank">dincer.goksuluk@hacettepe.edu.tr</a><p>'),
                #HTML('<br>'),
                #h4("Contributors"),
                #h5("Gokmen Zararsiz"),
                HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Gokmen_Zararsiz_CV_Eng.pdf" target="_blank"> <b>Gokmen Zararsiz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:gokmen.zararsiz@hacettepe.edu.tr" target="_blank">gokmen.zararsiz@hacettepe.edu.tr</a><p>'),
                h5("Izzet Parug Duru"),
                HTML('<p>Marmara University Faculty of Arts and Sciences<a href="http://fzk.fef.marmara.edu.tr/en/" target="_blank"> Department of Physics</a><p>'),
                HTML('<p><a href="mailto:izzet.duru@gedik.edu.tr" target="_blank">izzet.duru@gedik.edu.tr</a><p>'),
                h5("Vahap Eldem"),
                HTML('<p>Istanbul University Faculty of Science <a href="http://fen.istanbul.edu.tr/biyoloji/#" target="_blank"> Department of Biology</a><p>'),
                HTML('<p><a href="mailto:vahap.eldem@istanbul.edu.tr" target="_blank">vahap.eldem@istanbul.edu.tr</a><p>'),

                HTML('<br>'),


                h4("News"),

                h5("Version 1.6 (June 9, 2015)"),
                HTML('<p>(1) Advanced options have been added for both perspective and contour plots.<p>'),
                HTML('<br>'),

                h5("Version 1.5 (June 2, 2015)"),
                HTML('<p>(1) Advanced options have been added for the multivariate outlier detection.<p>'),
                HTML('<p> (2) Bug fixes.<p>'),
                HTML('<br>'),

                h5("Version 1.4 (January 20, 2015)"),
                HTML('<p>(1) <a href="http://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf" target="_blank">MVN paper </a> published at The R Journal. The complete reference information is at the <b>Citation</b> tab <p>'),
                HTML('<p> (2) Minor improvements and fixes.<p>'),
			          HTML('<br>'),
               
                h5("Version 1.3 (November 20, 2014)"),
                HTML('<p>Univariate descriptive statistics, tests and plots have been added.<p>'),
               
			          HTML('<br>'),
                h5("September 12, 2014"),
                HTML('<p>MVN web-tool presented at 16th National Biostatistics Congress in Antalya.<p>'),

			          HTML('<br>'),
                h5("Version 1.2 (June 8, 2014)"),
                HTML('<p>(1) Sub-group analysis has been added.<p>'),

			          HTML('<br>'),
                h5("Version 1.1 (May 13, 2014)"),
                HTML('<p>(1) Three different outlier detection methods, including Mahalanobis distance, adjusted quantile and PCOut, are available now. <p>'),
                HTML('<p>(2) New data set without outliers can be downloaded. <p>'),

			          HTML('<br>'),
                h5("Version 1.0 (March 10, 2014)"),
                HTML('<p>(1) Web-tool version of the <a href="http://cran.r-project.org/web/packages/MVN/index.html" target="_blank">MVN </a> package has been released. <p>'),


HTML('<br>'),

h5("Other Tools"),

HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/easyROC/" target="_blank"> <b>easyROC: a web-tool for ROC curve analysis </b></a><p>'),
HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/MLViS/" target="_blank"> <b>MLViS: machine learning-based virtual screening tool </b></a><p>'),
HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/DDNAA/" target="_blank"> <b>DDNAA: Decision support system for differential diagnosis of nontraumatic acute abdomen </b></a><p>'),
HTML('<br>'),



                h6("Please feel free to send us bugs and feature requests.")

            ),

            tabPanel("Citation",
             verbatimTextOutput("cite")
            ),

            id="tabs1"
		),

        tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", "textarea { max-width: 185px; }"),
        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
        tags$style(type='text/css', ".well { max-width: 330px; }"),
        tags$style(type='text/css', ".span4 { max-width: 330px; }")),

        tags$head(
        tags$link(rel = "shortcut icon", href = "favicon-2.ico"))

 )
))




