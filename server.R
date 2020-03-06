require(iNEXT)
require(ggplot2)
require(dplyr)
require(Rcpp)
library(shiny)
library(markdown)

data(spider)
data(ant)
source("source/Taxonomic_profile.R")

cbPalette <- rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#330066", "#CC79A7",  "#0072B2", "#D55E00"))


datainf <- function(data){
  if (class(data)!="dataframe") data <- as.data.frame(data)
  a1 <- matrix(0,13,1,dimnames=list(1:13, "value"))
  rownames(a1) <- c("n", "S.obs", "C.hat","f1","f2","f3","f4","f5","f6","f7","f8","f9","f10")
  a1[1,1] <- as.integer(sum(data))
  a1[2,1] <-  round(c(sum(data!=0)),0)
  a1[4:13,1] <- c(sum(data==1),sum(data==2),sum(data==3),sum(data==4),sum(data==5),sum(data==6),sum(data==7),sum(data==8),sum(data==9),sum(data==10))
  f1 = sum(data==1)
  f2 = sum(data==2)
  n = sum(data)
  a1[3,1] <- round(1 - (f1/n)*((n-1)*f1/((n-1)*f1+2*f2)),4)
  a1 = data.frame(a1)
  return(a1)
}

datainf.inc <- function(data){
  if (class(data)!="dataframe") data <- as.data.frame(data)
  a1 <- matrix(0,14,1,dimnames=list(1:14, "value"))
  rownames(a1) <- c("T", "U", "S.obs", "C.hat","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
  a1[1,1] <- data[1,1]
  data = data[-1,1]
  a1[2,1] <- round(sum(data),0)
  a1[3,1] <- round(c(sum(data!=0)),0)
  a1[5:14,1] <- c(sum(data==1),sum(data==2),sum(data==3),sum(data==4),sum(data==5),sum(data==6),sum(data==7),sum(data==8),sum(data==9),sum(data==10))
  f1 = sum(data==1)
  f2 = sum(data==2)
  n = sum(data)
  a1[4,1] <- round(1 - (f1/n)*((n-1)*f1/((n-1)*f1+2*f2)),4)
  a1 = data.frame(a1)
  return(a1)
}

shinyServer(function(input, output) { 
  
 
  loadPaste <- reactive({
    if (input$datatype=='abundance') text <- input$copyAndPaste_abun
    if (input$datatype=='incidence_freq') text <- input$copyAndPaste_inc
    Fun <- function(e){
      # split the text into many list, split by "\n".
      temp <- lapply(readLines(textConnection(text)), function(x) scan(text = x, what = 'char'))
      out <- list()
      out.name <- 0
      for (i in seq_along(temp)){
        out.name[i] <- temp[[i]][1]
        out[[i]] <- as.numeric(temp[[i]][-1])
      }
      names(out) <- t(data.frame(out.name))
      out
    }
    tryCatch(Fun(e), error = function(e){return()})
  })
  
  data <- reactive({
    if (input$source == "demo" & input$datatype=="abundance" & input$twodata=="spider") {  
      dat = spider
    }
    if (input$source == "demo" & input$datatype=="abundance" & input$twodata=="beetles") {  
      dat = beetles
    }
    if (input$source == "demo" & input$datatype=="incidence_freq") {  
      dat = ant
    }
    if (input$source == "upload") {
      if(is.null(input$files$datapath)!=T){
        da = read.table(input$files$datapath, header=T)
        dat = list()
        for(i in 1:ncol(da)){
          dat[[i]] <- da[,i]
        }
        names(dat) = colnames(da)
      } else{
        dat <- c()
      }
    } 
    if (input$source=="keyin") { out <- loadPaste() }
    dat
  })
  
  getDataName <- reactive({
    Fun <- function(e){
      if (input$source=="keyin") { out <- loadPaste() }
      else {out <- data()}
      out.name <- names(out)
      if(is.na(names(out)[1]) == TRUE){
        dat <- paste("No data")        
        dat
      }else{
        dat <- out
        for(i in seq_along(out)){
          dat[[i]] <- out.name[i]
        }
        dat
      }
    }
    tryCatch(Fun(e), error = function(e){return()})
  })
  
  selectedData <- reactive({
    if(input$goButton==0) return(NULL)
    isolate({
      out <- loadPaste()
      selected <- 1
      dataset <- list()
      # input$dataset : dat
      for(i in seq_along(input$dataset)){
        selected[i] <- which(names(out) == input$dataset[i])
      } 
      for(i in seq_along(selected)){
        k <- selected[i]
        dataset[[i]] <- out[[k]]
        # names(dataset[[i]]) = getDataName()[k]
      }
      names(dataset) <- input$dataset
      dataset
    })
    
  })
  
#######hsiaotung for incidence data format check
  validate_incidence <- function(ds) {
    maxindex<-sapply(ds, function(x) which.max(x))
  #  print(maxindex)
  #  print(max(maxindex))
   
    if (max(maxindex) !=1) {
     # HTML('<script type="text/javascript">alert("CSV, please!");</script>')
      paste(ds,"\n For incidence data, the first entry should be the # sampling units.\n Species incidence frequence should be less than the number of sampling units.\n   ")
    } else {
      NULL
    }
  }
  
  #######hsiaotung for incidence data format check
  dataModal <- function(failed = FALSE) {
    modalDialog(size="l",
      title = HTML('<span style="color:red; font-size: 20px; font-weight:bold; font-family:sans-serif ">Error Message: Your incidence data format is not correct!<span>'),
      
     div("Species incidence frequencies should be less than or equal to the # of sampling units.", 
         style="font-size:20px;font-weight:bold;font-family:sans-serif; color:darkred"),
     
     br(),
     div("Below is the example of incidence data:", 
         style="font-size:20px;font-weight:bold;font-family:sans-serif; color:black"),
     br(),
      tags$img(src=base64enc::dataURI(file = "./Images/incidenceErrorMsg.png", mime = "image/png")),
      footer = tagList(
        modalButton("Cancel")
      #  actionButton("ok", "OK")
      )
    )
  }
 
 
  
  
  mydata <- reactive({
    if(input$goButton==0) return(NULL)
    isolate({
      if (input$source=="keyin") {out = selectedData()}
      else { out <- data() }
      selected <- c()
      dataset <- list()
      # input$dataset : dat
      for(i in seq_along(input$dataset)){
        k <- which(names(out) == input$dataset[i])
        dataset[[i]] <- out[[k]]
        names(dataset)[i] <- names(out)[k]
      } 
      
  #######hsiaotung for incidence data format check   
      if (input$datatype=="incidence_freq"){
        #validate(validate_incidence(dataset))
        
        maxindex<-sapply(dataset, function(x) which.max(x))
      #  print(maxindex)
      #  print(max(maxindex))
        if (max(maxindex) !=1) {
          showModal(dataModal())
          validate(validate_incidence(dataset))
        }else {dataset}
        
      }else{
        dataset
      }
     
     
    })
  })
  
  output$choose_dataset <- renderUI({
    dat <- getDataName()
    selectInput("dataset", "Select dataset(s):", choices = dat, selected = dat[1], multiple = TRUE, selectize = FALSE)
  })
  
  output$choose_size <- renderUI({
    if (input$source=="keyin") {out = selectedData()}
    else { out <- data() }
    selected <- c()
    size <- c()
    # input$dataset : dat
    for(i in seq_along(input$dataset)){
      selected[i] <- which(names(out) == input$dataset[i])
    } 
    for(i in seq_along(selected)){
      k <- selected[i]
      if(input$datatype=="abundance"){
        size[i] <- 2*sum(out[[k]])
      } else {
        size[i] <- 2*out[[k]][1]
      }
    }
    if(length(size)==1){
      end <- size
    }else if(length(size)>1){
      end <- min(size)
    }else{
      end <- 0
    }
    numericInput("endpoint", "Endpoint setting",  min = 0,  max = 2000, value =end )
  })
  
  output$table <- renderTable({
    if(input$goButton==0) return(NULL)
    isolate({
      if(length(mydata())==1){
        if (input$datatype=="abundance"){
          output <- datainf(mydata())
          output[-3,] <- prettyNum(output[-3,])
          table <- output
        }
        if (input$datatype=="incidence_freq"){
          output <- datainf.inc(mydata()) 
          output[-4,] <- prettyNum(output[-4,])
          table <- output
          colnames(table) = names(mydata())
        }
      }else{
        if (input$datatype=="abundance"){
          output <- datainf(mydata()[[1]])
          output[-3,] <- prettyNum(output[-3,])
          table <- output
        }
        if (input$datatype=="incidence_freq"){
          output <- datainf.inc(mydata()[[1]]) 
          output[-4,] <- prettyNum(output[-4,])
          table <- output
        }
        for(i in 2:length(mydata())){ 
          if (input$datatype=="abundance"){
            output <- datainf(mydata()[[i]])
            output[-3,] <- prettyNum(output[-3,])
            table <- cbind(table,output) 
          }
          if (input$datatype=="incidence_freq"){
            output <- datainf.inc(mydata()[[i]]) 
            output[-4,] <- prettyNum(output[-4,])
            table <- cbind(table,output) 
          }
        }
        colnames(table) = names(mydata())
      }
      table
    })
  }, bordered = T, rownames = T)
  
  RE_table <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(input$q)
    data = mydata()
    size=as.numeric(sapply(readLines(textConnection(input$size)), function(x) scan(text = x, what = 'char')))
    if(is.na(size) == T) size=NULL
    a = c()
    for ( i in 1:length(mydata())){
      temp = iNEXT(x=data[[i]], q=q, datatype=input$datatype, endpoint=input$endpoint, knots=input$knots, conf=input$conf, nboot=input$nboot, size=size)$iNextEst
      site = names(data)[i]
      a = rbind(a,cbind(temp,site))
    }
    a[,1] <- as.integer(a[,1])
    a[,3] <- as.integer(a[,3])
    a
  })
  
  output$RE_table <- renderTable({
    if(input$goButton==0) return(NULL)
    isolate({
      RE_table()
    })
  }) 
  
  output$downloadprofile <- downloadHandler(
    filename = function() { paste('RE-', Sys.Date(), '.csv')},
    content = function(file){
      write.csv(RE_table(),file)
    })
  
  myplotprofilesize <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(input$q)
    if (length(mydata())==1) {
      data = mydata()[[1]]
      names(data) = names(mydata())[1]
    }
    else { data = mydata() }
    size=as.numeric(sapply(readLines(textConnection(input$size)), function(x) scan(text = x, what = 'char')))
    if(is.na(size) == T) size=NULL
    temp = iNEXT(x=data, q=q, datatype=input$datatype, endpoint=input$endpoint, knots=input$knots, conf=input$conf, nboot=input$nboot, size=size)
    t = ggiNEXT(temp, type=1)
    t+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
  })
  
  output$myplotprofilesize <- renderPlot({
    if(input$goButton==0) return(NULL)
    isolate({
      myplotprofilesize()
    })
  },height = 400, width = 850 )
  
  output$downloadplotsize <- downloadHandler(
    filename = function() { paste('RE-', '.png', sep='') },
    content = function(file) {
      ggsave(file,myplotprofilesize(),width=8, height=5,dpi=500)
    })
  
  myplotprofilesizeC <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(input$q)
    B = input$nboot
    isolate({
      if (length(mydata())==1) {
        data = mydata()[[1]]
        names(data) = names(mydata())[1]
      }
      else { data = mydata() }
      size=as.numeric(sapply(readLines(textConnection(input$size)), function(x) scan(text = x, what = 'char')))
      if(is.na(size) == T) size=NULL
      temp = iNEXT(x=data, q=q, datatype=input$datatype, endpoint=input$endpoint, knots=input$knots, conf=input$conf, nboot=input$nboot, size=size)
      t = ggiNEXT(temp, type=2)
      t+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
    })
  })
  
  output$myplotprofilesizeC <- renderPlot({
    if(input$goButton==0) return(NULL)
    isolate({
      myplotprofilesizeC()
    })
  },height = 400, width = 850 )
  
  output$downloadplotsizeC <- downloadHandler(
    filename = function() { paste('RE-', '.png', sep='') },
    content = function(file) {
      ggsave(file,myplotprofilesizeC(),width=8, height=5,dpi=500)
    })
  
  myplotprofileC <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(input$q)
    B = input$nboot
    size=as.numeric(sapply(readLines(textConnection(input$size)), function(x) scan(text = x, what = 'char')))
    if(is.na(size) == T) size=NULL
    isolate({
      if (length(mydata())==1) {
        data = mydata()[[1]]
        names(data) = names(mydata())[1]
      }
      else { data = mydata() }
      temp = iNEXT(x=data, q=q, datatype=input$datatype, endpoint=input$endpoint, knots=input$knots, conf=input$conf, nboot=input$nboot, size=size)
      t = ggiNEXT(temp, type=3)
      t+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
    })
  })
  output$myplotprofileC <- renderPlot({
    if(input$goButton==0) return(NULL)
    isolate({
      myplotprofileC()
    })
  },height = 400, width = 850 )
  
  output$downloadplotC <- downloadHandler(
    filename = function() { paste('RE-', '.png', sep='') },
    content = function(file) {
      ggsave(file,myplotprofileC(),width=8, height=5,dpi=500)
    })
  
  #===========================add asymptotic profile======================#
  
  #table for estimated profile
  Dprofile_table <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(sapply(readLines(textConnection(input$orderq)), function(x) scan(text = x, what = 'char')))
    B = input$nboot
    isolate({
      a = c()
      if(B == 0){
        if(input$datatype=="abundance"){
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_Proposeprofile_nose(mydata()[[i]],q) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
        if(input$datatype=="incidence_freq"){
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_Proposeprofile.inc_nose(mydata()[[i]],q) )
            
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
          
        }
      }else{
        if(input$datatype=="abundance"){
          print(mydata())
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_Proposeprofile(mydata()[[i]],B,q,input$conf) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
        if(input$datatype=="incidence_freq"){
          print(mydata())
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_Proposeprofile.inc(mydata()[[i]],B,q,input$conf) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
      }
      a
    })
  })
  
  output$Dprofile_table_diversity <- renderTable({
    if(input$goButton==0) return(NULL)
    out <- Dprofile_table()
    Diversity <- filter(out,Target=="Diversity")
    Diversity  
  }) 
  
  output$Dprofile_table_entropy <- renderTable({
    if(input$goButton==0) return(NULL)
    out <- Dprofile_table()
    Entropy <- filter(out,Target=="Entropy")
    Entropy  
  }) 
  
  myplotprofile <- reactive({
    if(input$goButton==0) return(NULL)
    isolate({
      out <- Dprofile_table()
      if(input$nboot==0){
        if(input$datatype == "abundance"){
          plot_diversity_profile_nose(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
        else if(input$datatype == "incidence_freq"){
          plot_diversity_profile.inc_nose(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
      }else{
        if(input$datatype == "abundance"){
          plot_diversity_profile(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
        else if(input$datatype == "incidence_freq"){
          plot_diversity_profile.inc(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
      }
      
    })
  })
  output$plotprofile <- renderPlot({
    if(input$goButton==0) return(NULL)
    isolate({
      myplotprofile()
    })
  },height = 400, width = 850 )
  
  output$downloadplotprofile <- downloadHandler(
    filename = function() { paste('Diversity Profile estimated-', '.png', sep='') },
    content = function(file) {
      ggsave(file,myplotprofile(),width=8, height=5,dpi=500)
    })
  
  output$downloadprofile2 <- downloadHandler(
    filename = function() { paste('Diversity Profile estimated-', Sys.Date(), '.csv')},
    content = function(file){
      write.csv(Dprofile_table(),file)
    })
  
  #table for empirical profile
  Emperical_table <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(sapply(readLines(textConnection(input$orderq)), function(x) scan(text = x, what = 'char')))
    B = input$nboot
    isolate({
      a = c()
      if(B==0){
        if(input$datatype=="abundance"){
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_Empericalprofile_nose(mydata()[[i]],q) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
        if(input$datatype=="incidence_freq"){
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_EmpericalDiversityprofile.inc_nose(mydata()[[i]],q) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
      }else{
        if(input$datatype=="abundance"){
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_Empericalprofile(mydata()[[i]],B,q,input$conf) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
        if(input$datatype=="incidence_freq"){
          for ( i in 1:length(mydata())){
            a = rbind(a,MakeTable_EmpericalDiversityprofile.inc(mydata()[[i]],B,q,input$conf) )
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)*2))
        }
      }
      a
    })
  })
  
  output$Emperical_table_diversity <- renderTable({
    if(input$goButton==0) return(NULL)
    out <- Emperical_table()
    Diversity <- filter(out,Target=="Diversity")
    Diversity  
  }) 
  
  output$Emperical_table_entropy <- renderTable({
    if(input$goButton==0) return(NULL)
    out <- Emperical_table()
    Entropy <- filter(out,Target=="Entropy")
    Entropy  
  }) 
  
  output$downloadprofile.em <- downloadHandler(
    filename = function() { paste('table diversity profile emperical-', Sys.Date(), '.csv')},
    content = function(file){
      write.csv(Emperical_table(),file)
    })
  
  myplotprofile.em <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(sapply(readLines(textConnection(input$orderq)), function(x) scan(text = x, what = 'char')))
    B = input$nboot
    isolate({
      out <- Emperical_table()
      if(B==0){
        if(input$datatype=="abundance"){
          plot_diversity_profile_nose(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
        else if(input$datatype=="incidence_freq"){
          plot_diversity_profile.inc_nose(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
      }else{
        if(input$datatype=="abundance"){
          plot_diversity_profile(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
        else if(input$datatype=="incidence_freq"){
          plot_diversity_profile.inc(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
        }
      }
      
    })
  })
  
  output$plotprofile.em <- renderPlot({
    if(input$goButton==0) return(NULL)
    myplotprofile.em()
  },height = 400, width = 850 )
  
  
  output$downloadplotprofile.em <- downloadHandler(
    filename = function() { paste('Diversity Profile emperical-', '.png', sep='') },
    content = function(file) {
      ggsave(file,myplotprofile.em(),width=8, height=5,dpi=500)
    })
  
  #===========================add Sample Completeness Curve======================#
  
  scprofile_table <- reactive({
    if(input$goButton==0) return(NULL)
    q = as.numeric(sapply(readLines(textConnection(input$orderq)), function(x) scan(text = x, what = 'char')))
    B = input$nboot
    isolate({
      a = c()
      if(B == 0){
        if(input$datatype=="abundance"){
          for ( i in 1:length(mydata())){
            a = rbind(a,sc_profile.nose(mydata()[[i]], q, "abundance"))
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)))
        }
        if(input$datatype=="incidence_freq"){
          for ( i in 1:length(mydata())){
            a = rbind(a,sc_profile.nose(mydata()[[i]], q, "incidence_freq"))
            
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)))
          
        }
      }else{
        if(input$datatype=="abundance"){
          print(mydata())
          for ( i in 1:length(mydata())){
            a = rbind(a,sc_profile(mydata()[[i]], q, B,input$conf, "abundance"))
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)))
        }
        if(input$datatype=="incidence_freq"){
          print(mydata())
          for ( i in 1:length(mydata())){
            a = rbind(a,sc_profile(mydata()[[i]], q, B,input$conf, "incidence_freq"))
          }
          a = cbind(a,"Community" = rep(names(mydata()),each=length(q)))
        }
      }
      a
    })
  })
  
  output$scprofile_table <- renderTable({
    if(input$goButton==0) return(NULL)
    scprofile_table()
  }) 
  
  
  scplotprofile <- reactive({
    if(input$goButton==0) return(NULL)
    isolate({
      out <- scprofile_table()
      if(input$nboot==0){
        plot_sc_profile_nose(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
      }else{
        plot_sc_profile(out)+scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)
      }
      
    })
  })
  output$plotprofile_sc <- renderPlot({
    if(input$goButton==0) return(NULL)
    isolate({
      scplotprofile()
    })
  },height = 400, width = 850 )
  
  output$downloadplotprofile_sc <- downloadHandler(
    filename = function() { paste('Sample Completeness Curve estimated-', Sys.Date(), '.png') },
    content = function(file) {
      ggsave(file,scplotprofile(),width=8, height=5,dpi=500)
    })
  
  output$downloadprofile2_sc <- downloadHandler(
    filename = function() { paste('Sample Completeness Curve estimated-', Sys.Date(), '.csv')},
    content = function(file){
      write.csv(scprofile_table(),file)
    })
  
  
})
