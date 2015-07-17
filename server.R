shinyServer(function(input, output, session) {

    source("classes.R")
    source("hzTest.R")
    source("roystonTest.R")
    source("mardiaTest.R")
    source("mvnPlot.R")
    source("dhTest.R")
    source("outlier.R")
    source("uniPlot.R")
    source("uniNorm.R")
    library("mvoutlier")
    library("nortest")
    library("robustbase")
    library("asbio")
    library("moments")
    library("MASS")
    library("shiny")
    library("plyr")
    library("MVN")
    library("psych")
    library("ggplot2")
    source("perspControl.R")
    source("contourControl.R")
     

    
    observe({
		if (input$clearText_button == 0) return()
		isolate({ updateTextInput(session, "myData", label = ",", value = "") })
	})
	

	
	dataM <- reactive({  ## Data input.
		if(input$dataInput==1){  ## Load example data.
            
            if(input$sampleData==1){
				data <- read.table("bivariate.txt", header=TRUE)
            }
			
			else if(input$sampleData==2){

                data <- iris[1:50,1:2]
            }
			
			else if(input$sampleData==3){
				data <- iris[,-4]
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
		
		else {  ## Paste data.
			if(is.null(input$myData)) {return(NULL)}
            
			
			tmp <- matrix(strsplit(input$myData, "\n")[[1]])
			mySep <- switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
			myColnames <- strsplit(tmp[1], mySep)[[1]]
			data <- matrix(0, length(tmp)-1, length(myColnames))
			colnames(data) <- myColnames
            
			for(i in 2:length(tmp)){
				myRow <- as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
				data[i-1,] <- myRow[-length(myRow)]
			}
			
            data <- data.frame(data)
		}
        
        {if (input$firstLast == 0){
            ind = complete.cases(data)
            return(data[ind,])
        }
        
        else if (input$firstLast == 1){
            data[,1] = as.factor(data[,1])
            ind = complete.cases(data)
            return(data[ind,])
        }
        
        else {
            data[,dim(data)[2]] = as.factor(data[,dim(data)[2]])
            ind = complete.cases(data)
            return(data[ind,])
        }}
	})


	
    heightSize <- reactive(input$myHeight)
	widthSize <- reactive(input$myWidth)
    
    heightsize <- reactive(input$myheight)
	widthsize <- reactive(input$mywidth)
    
    heightSizeUni <- reactive(input$myheightUni)
	widthSizeUni <- reactive(input$mywidthUni)
    
    
     observe({
 
        if(input$persOpt){
           updateCheckboxInput(session, "defaultPersp", label = "Default", FALSE)
        }
        
    })
    
    
    observe({
        
        if(input$defaultPersp){
            updateCheckboxInput(session, "persOpt", label = "Advanced", FALSE)
        }
        
    })
    
    
    
    observe({
        
        if(input$conOpt){
            updateCheckboxInput(session, "defaultCon", label = "Default", FALSE)
        }
        
    })
    
    
    observe({
        
        if(input$defaultCon){
            updateCheckboxInput(session, "conOpt", label = "Advanced", FALSE)
        }
        
    })
    
    
    
    observe({
        if (input$firstLast == 1){
            updateSelectInput(session, "subset", choices = levels(dataM()[,1]), selected = levels(dataM()[,1])[1])
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            updateSelectInput(session, "subset", choices = levels(dataM()[,dim(dataM())[2]]), selected = levels(dataM()[,dim(dataM())[2]])[1])
            
        }
        
	})
    
    
    
    observe({
        if (input$firstLast == 1){
            updateSelectInput(session, "subsetUni", choices = levels(dataM()[,1]), selected = levels(dataM()[,1])[1])
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            updateSelectInput(session, "subsetUni", choices = levels(dataM()[,dim(dataM())[2]]), selected = levels(dataM()[,dim(dataM())[2]])[1])
            
        }
        
	})
    
    
    observe({
        if (input$firstLast == 1){
            updateSelectInput(session, "subsetOut", choices = levels(dataM()[,1]), selected = levels(dataM()[,1])[1])
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            updateSelectInput(session, "subsetOut", choices = levels(dataM()[,dim(dataM())[2]]), selected = levels(dataM()[,dim(dataM())[2]])[1])
            
        }
        
	})
    
    
    
    
    
    
    
    output$uniTest <- renderPrint({
        
        if(input$firstLast==0){   ## Single group MVN test
    
         uniNorm(dataM(), type = input$normTest)
          
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                subNames = names(dataset.split)
                ind.sub = which(names(dataset.split) == input$subsetUni)
                
                uniNorm(dataset.split[[ind.sub]],type = input$normTest)
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                subNames = names(dataset.split)
                ind.sub = which(names(dataset.split) == input$subsetUni)
                
                uniNorm(dataset.split[[ind.sub]],type = input$normTest)
                
  
        }
    })
    
    
    
    
    
    output$uniPlot <- renderPlot({
        
        if(input$firstLast==0){   ## Single group MVN test
            
            uniPlot(dataM(), type = input$normPlot, pch = 16)


        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subsetUni)
            
            uniPlot(dataset.split[[ind.sub]],type = input$normPlot, pch = 16)
            
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subsetUni)
            
            uniPlot(dataset.split[[ind.sub]],type = input$normPlot, pch = 16)
            
        }
    },  height = heightSizeUni, width = widthSizeUni)
    
    
    
    output$downloadNormTest <- downloadHandler(
    filename <- function() { paste("Univariate test.txt") },
    content <- function(file) {
        
        
        if(input$firstLast==0){   ## Single group MVN test
            
            result <- uniNorm(dataM(), type = input$normTest)
            out <- capture.output(result)
            write.table(out, file, row.names=F, col.names=F, quote=F)
            
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subsetUni)
            
            result <- uniNorm(dataset.split[[ind.sub]],type = input$normTest)
            
            out <- capture.output(result)
            write.table(out, file, row.names=F, col.names=F, quote=F)
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subsetUni)
            
            result <- uniNorm(dataset.split[[ind.sub]],type = input$normTest)
            
            out <- capture.output(result)
            write.table(out, file, row.names=F, col.names=F, quote=F)
            
            
        }

    
    })
    
    
        
    
    
    output$downloadNormPlot <- downloadHandler(
    filename <- function() { paste('Univariate plot.pdf') },
    content <- function(file) {
        pdf(file, width = input$myWidthUni/72, height = input$myheightUni/72)
        
        
        if(input$firstLast==0){   ## Single group MVN test
            
            uniPlot(dataM(), type = input$normPlot, pch = 16)
            
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subsetUni)
            
            uniPlot(dataset.split[[ind.sub]],type = input$normPlot, pch = 16)
            
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subsetUni)
            
            uniPlot(dataset.split[[ind.sub]],type = input$normPlot, pch = 16)
            
            
            
        }
    
    dev.off()
    
    },
    contentType = 'application/pdf'
    )
    
    

    
  output$MVN <- renderPrint({
  
    if(input$firstLast==0){   ## Single group MVN test
	
        if(input$testType=='0'){
			dataset <- dataM()
			mardiaTest(dataset, qqplot=FALSE)
        } 
		
		else if(input$testType=='1'){
            dataset <- dataM()
            hzTest(dataset, qqplot=FALSE)
        } 
		
		else if(input$testType=='2'){
            dataset <- dataM()
            roystonTest(dataset, qqplot=FALSE)
        }
		
		else if(input$testType=='3'){
            dataset <- dataM()
            dhTest(dataset, qqplot=FALSE)
        }
    }
	
	else if (input$firstLast == 1){  ## First column is "Group" variable.
        
		if(input$testType=='0'){
            
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            
            dataset = dataset.split[[ind.sub]]
           
            mardiaTest(dataset, qqplot=FALSE)


        } 
		
		else if(input$testType=='1'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            hzTest(dataset, qqplot=FALSE)
			
        } 
		
		else if(input$testType=='2'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            roystonTest(dataset, qqplot=FALSE)
             
        }
		
		else if(input$testType=='3'){
            dataset <- dataM()
			dataset.split = split(dataset[,-1], dataset[,1])
			lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
            
        }
    }
	
	else if(input$firstLast == 2){  ## Last column is "Group" variable.
        
		if(input$testType=='0'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            mardiaTest(dataset, qqplot=FALSE)
			
        } 
		
		else if(input$testType=='1'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            hzTest(dataset, qqplot=FALSE)
           
        } 
		
		else if(input$testType=='2'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            roystonTest(dataset, qqplot=FALSE)
            
        }
		
		else if(input$testType=='3'){
            dataset <- dataM()
			ind.last = dim(dataset)[2]
			dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
			lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
            
        } 
    }
  })
  
  


  output$mvnPLOT <- renderPlot({
  
    if (input$firstLast==0){  ## Single group MVN test, plot rendering..
	
		if (input$plotType=='0'){  ## Q-Q Plots.
          
			if (input$testType=='0'){
				dataset <- dataM()
				mardiaTest(dataset, qqplot=TRUE)
			} 
			
			else if (input$testType=='1'){
				dataset <- dataM()
				hzTest(dataset, qqplot=TRUE)
			} 
			
			else if (input$testType=='2'){
				dataset <- dataM()
				roystonTest(dataset, qqplot=TRUE)
			}
          
			else if (input$testType=='3'){
				dataset <- dataM()
				dhTest(dataset, qqplot=TRUE)
			}
		}
		
		if (input$plotType=='1'){  ## Perspective Plots.
			dataset <- dataM()
			result <- mardiaTest(dataset, qqplot=FALSE)
            
            plotCtrl = perspControl(theta = input$theta, phi = input$phi, r = input$r, d = input$d, scale = input$scale,
                            expand = input$expand, col = input$col, border = input$border, ltheta = input$ltheta, lphi = input$lphi, xlab = input$xlab,
                            ylab = input$ylab, zlab = input$zlab, main = input$main)
            
            mvnPlot(result, type="persp", default = input$defaultPersp, plotCtrl = plotCtrl)
		}
		
		else if (input$plotType=='2'){	## Contour plots.
			dataset <- dataM()
			result <- mardiaTest(dataset)
            plotCtrl = contourControl(nlevels = input$nlevels, xlab = input$xlabCon, ylab = input$ylabCon,
                                    labcex = input$labcex, drawlabels = input$drawlabels, method = input$methodCon, axes = input$axesCon,
                                    col = input$colCon, lty = input$ltyCon, lwd = input$lwdCon)
            
            
			mvnPlot(result, type="contour", default = input$defaultCon, plotCtrl = plotCtrl)
		}
	}

    if (input$firstLast == 1){  ## First column is "Group" variable.
        
		if (input$plotType=='0'){  ## Q-Q Plots.
              
            if (input$testType=='0'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                
                subNames = names(dataset.split)
                ind.sub = which(names(dataset.split) == input$subset)
                dataset = dataset.split[[ind.sub]]

                mardiaTest(dataset, qqplot=TRUE)
				
            } 
			
			else if(input$testType=='1'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                subNames = names(dataset.split)
                ind.sub = which(names(dataset.split) == input$subset)
                dataset = dataset.split[[ind.sub]]
                
                hzTest(dataset, qqplot=TRUE)
				
            } 
			
			else if(input$testType=='2'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                subNames = names(dataset.split)
                ind.sub = which(names(dataset.split) == input$subset)
                dataset = dataset.split[[ind.sub]]

                roystonTest(dataset, qqplot=TRUE)
                
            }
              
            else if(input$testType=='3'){
                dataset <- dataM()
				dataset.split = split(dataset[,-1], dataset[,1])
				nrow.plot = length(dataset.split)		
				ncol.plot = 1							
				
				par(mfrow=c(nrow.plot, ncol.plot))
                lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
                
            }
		}
		
		if (input$plotType=='1'){  ## Perspective Plots.
            
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]

			result = hzTest(dataset, qqplot=FALSE)
            
            plotCtrl = perspControl(theta = input$theta, phi = input$phi, r = input$r, d = input$d, scale = input$scale,
            expand = input$expand, col = input$col, border = input$border, ltheta = input$ltheta, lphi = input$lphi, xlab = input$xlab,
            ylab = input$ylab, zlab = input$zlab, main = input$main)
            
            mvnPlot(result, type="persp", default = input$defaultPersp, plotCtrl = plotCtrl)
		}
		
		else if (input$plotType=='2'){	## Contour plots.
            dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
			result = hzTest(dataset, qqplot=FALSE)
            plotCtrl = contourControl(nlevels = input$nlevels, xlab = input$xlabCon, ylab = input$ylabCon,
            labcex = input$labcex, drawlabels = input$drawlabels, method = input$methodCon, axes = input$axesCon,
            col = input$colCon, lty = input$ltyCon, lwd = input$lwdCon)
            
            
            mvnPlot(result, type="contour", default = input$defaultCon, plotCtrl = plotCtrl)
		}
		
	}
    
    if(input$firstLast == 2){	## Last column is "Group" variable.
	
    
    if (input$plotType=='0'){  ## Q-Q Plots.
        
        if (input$testType=='0'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            mardiaTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='1'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            hzTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='2'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            roystonTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='3'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])
            nrow.plot = length(dataset.split)
            ncol.plot = 1
            
            par(mfrow=c(nrow.plot, ncol.plot))
            lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
            
        }
    }
    
    if (input$plotType=='1'){  ## Perspective Plots.
        
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        
        subNames = names(dataset.split)
        ind.sub = which(names(dataset.split) == input$subset)
        dataset = dataset.split[[ind.sub]]
        
        result = hzTest(dataset, qqplot=FALSE)
        plotCtrl = perspControl(theta = input$theta, phi = input$phi, r = input$r, d = input$d, scale = input$scale,
        expand = input$expand, col = input$col, border = input$border, ltheta = input$ltheta, lphi = input$lphi, xlab = input$xlab,
        ylab = input$ylab, zlab = input$zlab, main = input$main)
        
        mvnPlot(result, type="persp", default = input$defaultPersp, plotCtrl = plotCtrl)
    }
    
    else if (input$plotType=='2'){	## Contour plots.
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        
        subNames = names(dataset.split)
        ind.sub = which(names(dataset.split) == input$subset)
        dataset = dataset.split[[ind.sub]]
        
        result = hzTest(dataset, qqplot=FALSE)
        plotCtrl = contourControl(nlevels = input$nlevels, xlab = input$xlabCon, ylab = input$ylabCon,
        labcex = input$labcex, drawlabels = input$drawlabels, method = input$methodCon, axes = input$axesCon,
        col = input$colCon, lty = input$ltyCon, lwd = input$lwdCon)
        
        
        mvnPlot(result, type="contour", default = input$defaultCon, plotCtrl = plotCtrl)
    }
    
	}
  }, height = heightSize, width = widthSize)





	# 2. PDF Format
	output$downloadPlotPDF <- downloadHandler(
		filename <- function() { paste('MVN.pdf') },
		content <- function(file) {
			pdf(file, width = input$myWidth/72, height = input$myHeight/72)

if (input$firstLast==0){  ## Single group MVN test, plot rendering..
	
    if (input$plotType=='0'){  ## Q-Q Plots.
        
        if (input$testType=='0'){
            dataset <- dataM()
            mardiaTest(dataset, qqplot=TRUE)
        }
        
        else if (input$testType=='1'){
            dataset <- dataM()
            hzTest(dataset, qqplot=TRUE)
        }
        
        else if (input$testType=='2'){
            dataset <- dataM()
            roystonTest(dataset, qqplot=TRUE)
        }
        
        else if (input$testType=='3'){
            dataset <- dataM()
            dhTest(dataset, qqplot=TRUE)
        }
    }
    
    if (input$plotType=='1'){  ## Perspective Plots.
        dataset <- dataM()
        result <- mardiaTest(dataset, qqplot=FALSE)
        plotCtrl = perspControl(theta = input$theta, phi = input$phi, r = input$r, d = input$d, scale = input$scale,
        expand = input$expand, col = input$col, border = input$border, ltheta = input$ltheta, lphi = input$lphi, xlab = input$xlab,
        ylab = input$ylab, zlab = input$zlab, main = input$main)
        
        mvnPlot(result, type="persp", default = input$defaultPersp, plotCtrl = plotCtrl)
    }
    
    else if (input$plotType=='2'){	## Contour plots.
        dataset <- dataM()
        result <- mardiaTest(dataset)
        plotCtrl = contourControl(nlevels = input$nlevels, xlab = input$xlabCon, ylab = input$ylabCon,
        labcex = input$labcex, drawlabels = input$drawlabels, method = input$methodCon, axes = input$axesCon,
        col = input$colCon, lty = input$ltyCon, lwd = input$lwdCon)
        
        
        mvnPlot(result, type="contour", default = input$defaultCon, plotCtrl = plotCtrl)
    }
}

if (input$firstLast == 1){  ## First column is "Group" variable.
    
    if (input$plotType=='0'){  ## Q-Q Plots.
        
        if (input$testType=='0'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            mardiaTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='1'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            hzTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='2'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            roystonTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='3'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])
            nrow.plot = length(dataset.split)
            ncol.plot = 1
            
            par(mfrow=c(nrow.plot, ncol.plot))
            lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
            
        }
    }
    
    if (input$plotType=='1'){  ## Perspective Plots.
        
        dataset <- dataM()
        dataset.split = split(dataset[,-1], dataset[,1])
        
        subNames = names(dataset.split)
        ind.sub = which(names(dataset.split) == input$subset)
        dataset = dataset.split[[ind.sub]]
        
        result = hzTest(dataset, qqplot=FALSE)
        plotCtrl = perspControl(theta = input$theta, phi = input$phi, r = input$r, d = input$d, scale = input$scale,
        expand = input$expand, col = input$col, border = input$border, ltheta = input$ltheta, lphi = input$lphi, xlab = input$xlab,
        ylab = input$ylab, zlab = input$zlab, main = input$main)
        
        mvnPlot(result, type="persp", default = input$defaultPersp, plotCtrl = plotCtrl)
    }
    
    else if (input$plotType=='2'){	## Contour plots.
        dataset <- dataM()
        dataset.split = split(dataset[,-1], dataset[,1])
        
        subNames = names(dataset.split)
        ind.sub = which(names(dataset.split) == input$subset)
        dataset = dataset.split[[ind.sub]]
        
        result = hzTest(dataset, qqplot=FALSE)
        plotCtrl = contourControl(nlevels = input$nlevels, xlab = input$xlabCon, ylab = input$ylabCon,
        labcex = input$labcex, drawlabels = input$drawlabels, method = input$methodCon, axes = input$axesCon,
        col = input$colCon, lty = input$ltyCon, lwd = input$lwdCon)
        
        
        mvnPlot(result, type="contour", default = input$defaultCon, plotCtrl = plotCtrl)
    }
    
}

if(input$firstLast == 2){	## Last column is "Group" variable.
	
    
    if (input$plotType=='0'){  ## Q-Q Plots.
        
        if (input$testType=='0'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            mardiaTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='1'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            hzTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='2'){
            dataset <- dataM()
            ind.last = dim(dataset)[2]
            dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
            subNames = names(dataset.split)
            ind.sub = which(names(dataset.split) == input$subset)
            dataset = dataset.split[[ind.sub]]
            
            roystonTest(dataset, qqplot=TRUE)
            
        }
        
        else if(input$testType=='3'){
            dataset <- dataM()
            dataset.split = split(dataset[,-1], dataset[,1])
            nrow.plot = length(dataset.split)
            ncol.plot = 1
            
            par(mfrow=c(nrow.plot, ncol.plot))
            lapply(dataset.split, function(x)dhTest(x, qqplot=TRUE))
            
        }
    }
    
    if (input$plotType=='1'){  ## Perspective Plots.
        
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        
        subNames = names(dataset.split)
        ind.sub = which(names(dataset.split) == input$subset)
        dataset = dataset.split[[ind.sub]]
        
        result = hzTest(dataset, qqplot=FALSE)
        plotCtrl = perspControl(theta = input$theta, phi = input$phi, r = input$r, d = input$d, scale = input$scale,
        expand = input$expand, col = input$col, border = input$border, ltheta = input$ltheta, lphi = input$lphi, xlab = input$xlab,
        ylab = input$ylab, zlab = input$zlab, main = input$main)
        
        mvnPlot(result, type="persp", default = input$defaultPersp, plotCtrl = plotCtrl)
    }
    
    else if (input$plotType=='2'){	## Contour plots.
        dataset <- dataM()
        ind.last = dim(dataset)[2]
        dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
        
        subNames = names(dataset.split)
        ind.sub = which(names(dataset.split) == input$subset)
        dataset = dataset.split[[ind.sub]]
        
        result = hzTest(dataset, qqplot=FALSE)
        plotCtrl = contourControl(nlevels = input$nlevels, xlab = input$xlabCon, ylab = input$ylabCon,
        labcex = input$labcex, drawlabels = input$drawlabels, method = input$methodCon, axes = input$axesCon,
        col = input$colCon, lty = input$ltyCon, lwd = input$lwdCon)
        
        
        mvnPlot(result, type="contour", default = input$defaultCon, plotCtrl = plotCtrl)
    }
    
}

			dev.off()
		},
		contentType = 'application/pdf'
	)




    
    ## Plot area for outlier detection tab.
    output$outlierPLOT <- renderPlot({
   
   if (input$firstLast==0){   # Grup degiskeni yok
       
        if(input$outlierDetect=='1'){
            dataset <- dataM()
            set.seed(1)
            outlier(dataset, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
        }
    
        if(input$outlierDetect=='2'){
            dataset <- dataM()
            set.seed(1)
            outlier(dataset, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
        }
        
        #  if(input$outlierDetect=='3'){
        #    dataset <- dataM()
        #    outlier(dataset, method="pcout")
        #}
        
    }
   
   if (input$firstLast == 1){  ## First column is "Group" variable.
       
       
       if(input$outlierDetect=='1'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
           
           subNames = names(dataset.split)
           ind.sub = which(names(dataset.split) == input$subsetOut)
           
           dataset = dataset.split[[ind.sub]]
           set.seed(1)
           outlier(dataset, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
           
       }
       
       if(input$outlierDetect=='2'){
           
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
           
           subNames = names(dataset.split)
           ind.sub = which(names(dataset.split) == input$subsetOut)
           
           dataset = dataset.split[[ind.sub]]
           set.seed(1)
           outlier(dataset, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
           
       }
       
       if(input$outlierDetect=='3'){
           dataset <- dataM()
           dataset.split = split(dataset[,-1], dataset[,1])
           nrow.plot = length(dataset.split)		
           ncol.plot = 1							
           par(mfrow=c(nrow.plot, ncol.plot))
           lapply(dataset.split, function(x)outlier(x, method="pcout"))
           
       }
       
   }


  if(input$firstLast == 2){	## Last column is "Group" variable.
      
      if(input$outlierDetect=='1'){
          dataset <- dataM()
          ind.last = dim(dataset)[2]
          dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
          
          subNames = names(dataset.split)
          ind.sub = which(names(dataset.split) == input$subsetOut)
          dataset = dataset.split[[ind.sub]]
          set.seed(1)
          outlier(dataset, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
      }
      
      if(input$outlierDetect=='2'){
          dataset <- dataM()
          ind.last = dim(dataset)[2]
          dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
          
          subNames = names(dataset.split)
          ind.sub = which(names(dataset.split) == input$subsetOut)
          dataset = dataset.split[[ind.sub]]
          set.seed(1)
          outlier(dataset, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
          
      }
      
      if(input$outlierDetect=='3'){
          dataset <- dataM()
          ind.last = dim(dataset)[2]
          dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
          nrow.plot = length(dataset.split)		
          ncol.plot = 1							
          
          par(mfrow=c(nrow.plot, ncol.plot))
          lapply(dataset.split, function(x)outlier(x, method="pcout"))
          
          
        }
      
      }
   }, height = heightsize, width = widthsize)



        
    output$downloadNewData <- downloadHandler(
    	filename = function() { "newData.txt" },
   		content = function(file) {
            
            if (input$firstLast == 0){   ## Download graphs for single groups.
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    set.seed(1)
                    new = outlier(dataset, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$newData
                    write.table(new, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    set.seed(1)
                    new = outlier(dataset, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$newData
                    write.table(new, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    write.table(outlier(dataset, qqplot=FALSE, method="pcout")$newData, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
            }
            
            else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    quantile = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = quantile[[i]][2]
                    }
                    names(outlier) = names(quantile)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    adjquan = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = adjquan[[i]][2]
                    }
                    names(outlier) = names(adjquan)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = out[[i]][2]
                    }
                    names(outlier) = names(out)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
                
            }
            
            else if (input$firstLast == 2){  ## outlier download, last column is group variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    quantile = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = quantile[[i]][2]
                    }
                    names(outlier) = names(quantile)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    adjquan = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = adjquan[[i]][2]
                    }
                    names(outlier) = names(adjquan)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = out[[i]][2]
                    }
                    names(outlier) = names(out)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
                
            }
            
            
        })
        
        
        output$downloadOutlier <- downloadHandler(
    	filename = function() { "outlier.txt" },
   		content = function(file) {
            
            if (input$firstLast == 0){   ## Download graphs for single groups.
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    set.seed(1)
                    out = outlier(dataset, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$outlier
                    write.table(out, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    set.seed(1)
                    out = outlier(dataset, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$outlier
                    write.table(out, file, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    write.table(outlier(dataset, qqplot=FALSE, method="pcout")$outlier, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
            }
            
            else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    quantile = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = quantile[[i]][1]
                    }
                    names(outlier) = names(quantile)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    adjquan = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = adjquan[[i]][1]
                    }
                    names(outlier) = names(adjquan)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = out[[i]][1]
                    }
                    names(outlier) = names(out)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
                
            }
            
            else if (input$firstLast == 2){  ## outlier download, last column is group variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    quantile = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = quantile[[i]][1]
                    }
                    names(outlier) = names(quantile)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    adjquan = lapply(dataset.split, function(x){set.seed(1)
                        outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = adjquan[[i]][1]
                    }
                    names(outlier) = names(adjquan)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                    
                }else
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                    outlier = list()
                    for(i in 1:length(dataset.split)){
                        
                        outlier[i] = out[[i]][1]
                    }
                    names(outlier) = names(out)
                    o = ldply(outlier, rbind)
                    names(o)[1]="Group"
                    
                    
                    write.table(o, file, row.names=FALSE,
                    col.names=TRUE, quote=FALSE, sep="\t")
                }
                
                
            }
            
            
            
        })
        
        
        
    output$downloadOutlierPlot <- downloadHandler(
            filename <- function() { paste('outlier.pdf') },
            content <- function(file) {
			pdf(file, width = input$mywidth/72, height = input$myheight/72)
            
            if (input$firstLast==0){   # Grup degiskeni yok
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    set.seed(1)
                    outlier(dataset, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
                }
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    set.seed(1)
                    outlier(dataset, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
                }
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    outlier(dataset, method="pcout")
                }
                
            }
            
            if (input$firstLast == 1){  ## First column is "Group" variable.
                
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                    
                    subNames = names(dataset.split)
                    ind.sub = which(names(dataset.split) == input$subsetOut)
                    
                    dataset = dataset.split[[ind.sub]]
                    set.seed(1)
                    outlier(dataset, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
                    
                }
                
                if(input$outlierDetect=='2'){
                    
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                    
                    subNames = names(dataset.split)
                    ind.sub = which(names(dataset.split) == input$subsetOut)
                    
                    dataset = dataset.split[[ind.sub]]
                    set.seed(1)
                    outlier(dataset, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
                    
                }
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    nrow.plot = length(dataset.split)
                    ncol.plot = 1
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="pcout"))
                    
                }
                
            }
            
            
            if(input$firstLast == 2){	## Last column is "Group" variable.
                
                if(input$outlierDetect=='1'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    
                    subNames = names(dataset.split)
                    ind.sub = which(names(dataset.split) == input$subsetOut)
                    dataset = dataset.split[[ind.sub]]
                    set.seed(1)
                    outlier(dataset, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
                }
                
                if(input$outlierDetect=='2'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    
                    subNames = names(dataset.split)
                    ind.sub = which(names(dataset.split) == input$subsetOut)
                    dataset = dataset.split[[ind.sub]]
                    set.seed(1)
                    outlier(dataset, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)
                    
                }
                
                if(input$outlierDetect=='3'){
                    dataset <- dataM()
                    ind.last = dim(dataset)[2]
                    dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                    nrow.plot = length(dataset.split)		
                    ncol.plot = 1							
                    
                    par(mfrow=c(nrow.plot, ncol.plot))
                    lapply(dataset.split, function(x)outlier(x, method="pcout"))
                    
                    
                }
                
            }
            
            
			dev.off()
		},
		contentType = 'application/pdf'
        )
        
        
        output$downloadMVNresult <- downloadHandler(
		filename <- function() { paste("result.txt") },
		content <- function(file) {
            
             if (input$firstLast == 0){   ## Download graphs for single groups.
                 
                if(input$testType=='0'){
                    
                    result <- mardiaTest(dataM())
                    out <- capture.output(result)
   
                } else
                
                if(input$testType=='1'){
    
                    result <- hzTest(dataM())
                      out<-capture.output(result)
                    
                } else
                
                if(input$testType=='2'){
                  
                     result <- roystonTest(dataM())
                       out <- capture.output(result)
                    
                } else
                
                if(input$testType=='3'){
                   
                result <- dhTest(dataM())
                out <- capture.output(result)
                    
                }
                
                	write.table(out, file, row.names=F, col.names=F, quote=F)
             }
             
             else if (input$firstLast == 1){  ## Download graphs, first column is group variable.
                 
                 
                 if(input$testType=='0'){
                     
                     dataset <- dataM()
                     dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                     
                     subNames = names(dataset.split)
                     ind.sub = which(names(dataset.split) == input$subset)
                     
                     dataset = dataset.split[[ind.sub]]
                     
                     result = mardiaTest(dataset, qqplot=FALSE)
                     
                     out <- capture.output(result)
                     
                 } else
                 
                 if(input$testType=='1'){
                     
                     dataset <- dataM()
                     dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                     
                     subNames = names(dataset.split)
                     ind.sub = which(names(dataset.split) == input$subset)
                     
                     dataset = dataset.split[[ind.sub]]
                     
                     result = hzTest(dataset, qqplot=FALSE)
                     
                     out <- capture.output(result)
                     
                 } else
                 
                 if(input$testType=='2'){
                     
                     dataset <- dataM()
                     dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                     
                     subNames = names(dataset.split)
                     ind.sub = which(names(dataset.split) == input$subset)
                     
                     dataset = dataset.split[[ind.sub]]
                     
                     result = roystonTest(dataset, qqplot=FALSE)
                     
                     out <- capture.output(result)
                     
                 } else
                 
                 if(input$testType=='3'){
                     
                    dataset <- dataM()
                    dataset.split = split(dataset[,-1], dataset[,1])
                    result = lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
                    out <- capture.output(result)
                     
                 }
                 
                 write.table(out, file, row.names=F, col.names=F, quote=F)
                 
                 
             }
             
             else if(input$firstLast == 2){  ## Last column is "Group" variable.
                 
                 if(input$testType=='0'){
                     
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     
                     subNames = names(dataset.split)
                     ind.sub = which(names(dataset.split) == input$subset)
                     dataset = dataset.split[[ind.sub]]
                     result = mardiaTest(dataset, qqplot=FALSE)
                     out <- capture.output(result)
                     
                 }
                 
                 else if(input$testType=='1'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     
                     subNames = names(dataset.split)
                     ind.sub = which(names(dataset.split) == input$subset)
                     dataset = dataset.split[[ind.sub]]
                     result = hzTest(dataset, qqplot=FALSE)
                     out <- capture.output(result)
                    
                 }
                 
                 else if(input$testType=='2'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     
                     subNames = names(dataset.split)
                     ind.sub = which(names(dataset.split) == input$subset)
                     dataset = dataset.split[[ind.sub]]
                     result = roystonTest(dataset, qqplot=FALSE)
                     out <- capture.output(result)
                     
                 }
                 
                 else if(input$testType=='3'){
                     dataset <- dataM()
                     ind.last = dim(dataset)[2]
                     dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                     result = lapply(dataset.split, function(x)dhTest(x,qqplot=FALSE))
                     out <- capture.output(result)
                     
                 }
                 
                 write.table(out, file, row.names=F, col.names=F, quote=F)
             }
		})
        ###
        
        # display 10 rows initially
        output$RawData <- renderDataTable(dataM(), options = list(iDisplayLength = 10))
        
        # display 10 rows initially
        output$OutlierData <- renderDataTable(
        
        
        if(input$firstLast==0){   ## Single group outlier detection
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                set.seed(1)
                outlier(dataset, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$outlier
                
            }else
            
            if(input$outlierDetect=='2'){
                dataset <- dataM()
                set.seed(1)
                outlier(dataset, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$outlier
                
            }else
            
            if(input$outlierDetect=='3'){
                dataset <- dataM()
                o = outlier(dataset, qqplot=FALSE, method="pcout")$outlier
                names(o)= c("Observation", "Outlier")
                o
                
            }
            
            
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                quantile = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][1]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                adjquan = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][1]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                out =lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="pcout")})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][1]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o) = c("Group", "Observation", "Outlier")
                o
            }
            
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                quantile = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][1]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                adjquan = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][1]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
                
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                out = lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][1]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o) = c("Group", "Observation", "Outlier")
                o
            }
            
            
        },
			
        options = list(iDisplayLength = 10))
        
        # display 10 rows initially
        output$NewData <- renderDataTable(
        
        
        
        if(input$firstLast==0){   ## Single group outlier detection
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                set.seed(1)
                outlier(dataset, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$newData
                
            }else
            
            if(input$outlierDetect=='2'){
                dataset <- dataM()
                set.seed(1)
                outlier(dataset, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)$newData
                
            }else
            
            if(input$outlierDetect=='3'){
                dataset <- dataM()
                outlier(dataset, qqplot=FALSE, method="pcout")$newData
                
            }
            
            
        }
        
        else if (input$firstLast == 1){  ## First column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])   ## Split data through 1st column
                quantile = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][2]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                adjquan = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][2]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                dataset.split = split(dataset[,-1], dataset[,1])
                out =lapply(dataset.split, function(x)outlier(x, qqplot=FALSE, method="pcout"))
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][2]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            
        }
        
        else if(input$firstLast == 2){  ## Last column is "Group" variable.
            
            if(input$outlierDetect=='1'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                quantile = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = quantile[[i]][2]
                }
                names(outlier) = names(quantile)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            else if(input$outlierDetect=='2'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                adjquan = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="adj.quan", alpha = input$alphaVal, tol = input$tol, label = input$label, position = input$position, offset = input$offset)})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = adjquan[[i]][2]
                }
                names(outlier) = names(adjquan)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
                
            }
            
            else if(input$outlierDetect=='3'){
                dataset <- dataM()
                ind.last = dim(dataset)[2]
                dataset.split = split(dataset[,-ind.last], dataset[,ind.last])
                out = lapply(dataset.split, function(x){set.seed(1)
                    outlier(x, qqplot=FALSE, method="pcout")})
                outlier = list()
                for(i in 1:length(dataset.split)){
                    
                    outlier[i] = out[[i]][2]
                }
                names(outlier) = names(out)
                o = ldply(outlier, rbind)
                names(o)[1]="Group"
                o
            }
            
            
        },
			
        options = list(iDisplayLength = 10))
        
        output$cite <- renderPrint(
        
        citation("MVN")
        
        )
        
 
}

)





