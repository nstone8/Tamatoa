library(shiny)
library(Rasylum)
library(ggplot2)

ui=fluidPage(
    titlePanel("Rasylum"),
    mainPanel(
        fileInput(inputId="directory",label="Select input files",multiple=TRUE,),
        dataTableOutput("fileList"), 
        textInput(inputId="consts",label="Enter consts separated by commas"),
        selectInput(inputId="suffix",label="Enter file extension",c(".ibw"="ibw",".ardf"="ardf")),
        actionButton("loadData","Load Data"),
        dataTableOutput("allCases"),
        numericInput("rBead","Bead radius in meters",0),
        numericInput("CPMaxF","CPMaxF",value="0.05"),
        numericInput("percentToFit","percentToFit",value="0.2"),
        numericInput("roughness","roughness",value="0.05"),
        numericInput("Q","Q",value="0.5"),
        numericInput("approachTrim","approachTrim",value="0.2"),
        numericInput("minRise","minRise",value="0"),
        actionButton("goButton","Run Fits"),
        dataTableOutput("fitFrame"),
        plotOutput("fitPlot"),
        numericInput("plotNo","Fit number to plot",value=1)        
    )
)

server=function(input,output){
    options(shiny.maxRequestSize=1000*1024^2)
    dataset=eventReactive(input$loadData,{
        consts=c()
        if(input$suffix=="ibw"){
            print("consts")
            consts=strsplit(input$consts," *, *")[[1]]
            print(consts)
        }
        files=input$directory
        dirs=c()
        newFiles=c()
        #find all directories where temp files are stored
        for(i in 1:(dim(files)[1])){
            file=files[i,]
            thisPath=""
            allSegments=strsplit(file$datapath,"/")[[1]]
            for(segment in allSegments[c(-1,-1*length(allSegments))]){
                thisPath=paste(thisPath,segment,sep="/")
            }
                                                    #rename temporary files with original name
            dirs=c(dirs,thisPath)
            newFile=paste(thisPath,file$name,sep="/")
            newFiles=c(newFiles,newFile)
            file.rename(file$datapath,newFile)
        }
        allCases=list()
                                        #loadPreSorted or loadARDF for every dir, then concatenate
        if(input$suffix=="ibw"){
            for(directory in levels(factor(dirs))){
                cases=loadPreSorted(directory,consts,input$suffix)
                allCases=c(allCases,cases)
            }
        }else if(input$suffix=="ardf"){
            for(file in newFiles){
                ardf=read.ardf(file)
                cases=loadARDF(ardf)
                allCases=c(allCases,cases)
            }
        }
        return(allCases)
    })
    output$fileList=renderDataTable(input$directory[,c(1,2)])
    output$allCases=renderDataTable({
        allCases=dataset()
        allIdents=allCases[[1]]$ident
        for(case in allCases[-1]){
            allIdents=rbind(allIdents,case$ident)
        }
        return(allIdents)
    })
    
    getFits=eventReactive(input$goButton,{
     
        allFits=parExtractStiffness(input$rBead,dataset(),CPMaxF=input$CPMaxF,percentToFit=input$percentToFit,roughness=input$roughness,Q=input$Q,approachTrim=input$approachTrim,minRise=input$minRise,numCores=1)
        return(allFits)
    })
    output$fitFrame=renderDataTable(collateFits(getFits()))
    output$fitPlot=renderPlot({
        allFits=getFits()
        thisFit=allFits$fits[[input$plotNo]]
        id = thisFit$ident
        fields = names(id)
        name = ""
        if (length(fields) > 1) {
            for (i in 1:length(fields)) {
                name = paste(name, fields[i], id[1, fields[i]])
            }
        }
        else {
            name = paste(name, fields, id)
        }

        return(ggplot(thisFit$fit$curves)+geom_path(aes(x=zPos,y=F,color=curve))+labs(title=name)+theme_classic())
    })
}
shinyApp(ui,server)
