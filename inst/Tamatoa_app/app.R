ui=fluidPage(
    titlePanel("Rasylum"),
    mainPanel(
        fileInput(inputId="directory",label="Select input files",multiple=TRUE,),
        dataTableOutput("fileList"),
        selectInput(inputId="suffix",label="Enter file extension",c(".ibw"="ibw",".ardf"="ardf")),
        uiOutput("suffixControl"),
        actionButton("loadData","Load Data"),
        dataTableOutput("allCases"),
        selectInput(inputId="operation",label="Choose Operation to Perform",c("Extract Stiffness"=0,"Extract Time Constant"=1,"Extract Approach Adhesion"=2,"Extract Retract Adhesion"=3)),
        uiOutput("operationControl"),
        actionButton("goButton","Run Fits"),
        dataTableOutput("fitFrame"),
        uiOutput("fitPlotOutput")
    )
)

server=function(input,output){
    options(shiny.maxRequestSize=1000*1024^2)

    output$suffixControl=renderUI({
        if(input$suffix=="ibw"){
            textInput(inputId="consts",label="Enter consts separated by commas")
        }
    })
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

                                        #Correct for bad path separator on windows
        if(.Platform$OS.type=="windows"){
            files$datapath=gsub("\\\\","/",files$datapath)
        }
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
                cases=loadPreSorted(directory,consts,paste(".",input$suffix,sep=""))
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
        if(length(allCases[[1]]$ident)<2){
            allIdents=as.character(allCases[[1]]$ident)
            allIdents=data.frame(allIdents)
            names(allIdents)=names(allCases[[1]]$ident)
        }
        if(length(allCases)>1){
            for(case in allCases[-1]){
                newRow=case$ident
                if(length(newRow)<2){
                    newRow=data.frame(as.character(case$ident))
                    names(newRow)=names(case$ident)
                }
                allIdents=rbind(allIdents,newRow)
            }
        }
        return(allIdents)
    })
    output$operationControl=renderUI({
        if(input$operation==0){ #extractStiffness
            return(flowLayout(
                numericInput("r","Bead radius in meters",0),
                numericInput("approachLength","Approach Length",value="0.1"),
                numericInput("contactLength","Contact Length",value="0.1"),
                numericInput("searchWidth","Search Width",value="0.2"),
                numericInput("maxF","Max Force",value="0.5"),
                numericInput("weight","Weight",value="4"),
                checkboxInput("correctVirtDefl","Correct for Virtual Deflection?",value=TRUE)
            ))
        }else if(input$operation==2 || input$operation==3){
            return(flowLayout(
                numericInput("percentFlatAdhesion","Percent Flat",0.8,min=0,max=1)          
            ))
        }
    })
    
    getFits=eventReactive(input$goButton,{
        allFits=list()
        if(input$operation==0 || input$operation==1){
            output$fitPlotOutput=renderUI(return(verticalLayout(
                plotOutput("fitPlot"),
                numericInput("plotNo","Fit number to plot",value=1),
                downloadButton("download",label="Save Fits")
            )))
        }else{
            output$fitPlotOutput=renderUI(return(verticalLayout(
                downloadButton("download",label="Save Fits")
            )))
        }
        if(input$operation==0){#extractStiffness
            allFits=parExtractStiffness(dataset(),input$r,input$approachLength,input$contactLength,input$searchWidth,input$maxF,input$weight,input$correctVirtDefl,numCores=1)
        }else if(input$operation==1){#extractTimeConst
            allFits=parExtractTimeConst(dataset(),numCores=1)
        }else if(input$operation==2){#approach Adhesion
            allFits=parExtractApproachAdhesion(dataset(),input$percentFlatAdhesion,numCores=1)
        }else if(input$operation==3){#retract adhesion
            allFits=parExtractRetractionAdhesion(dataset(),input$percentFlatAdhesion,numCores=1)
        }
        return(allFits)
    })
    output$fitFrame=renderDataTable({
        if(input$operation==0 || input$operation==1){
            return(collateFits(getFits()))
        }else{
            return(getFits())     
        }
    })
    output$fitPlot=renderPlot({
        if(input$operation==0 || input$operation==1){
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
            plot=ggplot()
            if(input$operation==1){
                plot=ggplot(thisFit$fit$curves)+geom_path(aes(x=t,y=F,color=curve))
            }else{
                plot=ggplot(thisFit$fit$curves)+geom_path(aes(x=zPos,y=F,color=curve))
            }
            return(plot+labs(title=name)+theme_classic())
        }
    })
    output$download=downloadHandler(filename="fits.rds",content=function(file){
        fits=getFits()
        saveRDS(fits,file=file)
    })

}
shinyApp(ui,server)
