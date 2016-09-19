#################################################
#    Marketing research tool box                #
#################################################


shinyServer(function(input, output,session){

# Go in the end for calling function
  
#################################################
# 01 -  Factor Analysis                         #    
#################################################
  factor_server <- reactive({
    
    library("nFactors")
    library("qgraph")
    
    Dataset <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
        rownames(Dataset) = Dataset[,1]
        Dataset1 = Dataset[,2:ncol(Dataset)]
        #Dataset = t(Dataset)
        return(Dataset1)
      }
    })
    
    output$table22 <- renderTable ({ 
      round(cor(Dataset()),2) 
    })
    
    output$table <- renderTable({ Dataset()   })
    
    nS = reactive ({    
      
      if (is.null(input$file)) { return(NULL) }
      else{
        ev = eigen(cor(Dataset(), use = 'pairwise.complete.obs'))  # get eigenvalues
        ap = parallel(subject=nrow((Dataset())),var=ncol((Dataset())),rep=100,cent=.05);
        nS = nScree(ev$values, aparallel= ap$eigen$qevpea);
      }
    })
    
    output$fselect <- renderUI({ 
      if (is.null(input$file)) { return(NULL) }
      else{
        
        numericInput("fselect", "Number of Factors:", unlist((nS())[1])[3])
      }
    })
    
    fselect = reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        
        fselect=input$fselect
        return(fselect)
      }
    })
    
    fit = reactive ({ 
      if (is.null(input$file)) { return(NULL) }
      else{
        
        fit = factanal(na.omit(Dataset()), fselect() , scores="Bartlett", rotation="varimax");
        return (fit)
      }
    }) 
    
    output$xaxis <- renderUI({
      
      if (is.null(input$file)) { return(NULL) }
      else {
        n =(fselect())
        list = character(0)
        for (i in 1:n) { 
          temp = paste("Factor",i)
          list = c(list, temp)
        }
        
        selectInput("xaxis", "Choose Factor for plotting on X axis",
                    list, selected = "Factor 1")
      }
      
    })
    
    output$yaxis <- renderUI({
      
      if (is.null(input$file)) { return(NULL) }
      else {
        n =(fselect())
        list = character(0)
        for (i in 1:n) { 
          temp = paste("Factor",i)
          list = c(list, temp)
        }
        list2 = setdiff(list,input$xaxis)
        selectInput("yaxis", "Choose Factor for plotting on Y axis",
                    list2, selected = "Factor 2")
      }
      
    })
    
    f1 = reactive({
      f = input$xaxis
      s <- strsplit(f, "[^[:digit:]]")
      solution <- as.numeric(unlist(s))
      solution <- unique(solution[!is.na(solution)])
      return(solution)
    })
    
    f2 = reactive({
      f = input$yaxis
      s <- strsplit(f, "[^[:digit:]]")
      solution <- as.numeric(unlist(s))
      solution <- unique(solution[!is.na(solution)])
      return(solution)
      
    })
    
    output$text1 <- renderText({    
      if (is.null(input$file)) { return(NULL) }
      else {
        return(paste("Test of the hypothesis that",(fit())$factors,"factors are sufficient."))}
    })
    
    output$text2 <- renderText({
      if (is.null(input$file)) { return(NULL) }
      else{
        return(paste("The chi square statistic is",round((fit())$STATISTIC,3),"on",(fit())$dof," degrees of freedom.")) }
    })
    
    output$text3 <- renderText({
      if (is.null(input$file)) { return(NULL) }
      else{
        return(paste("The p-value is",round((fit())$PVAL,3)))
      }
    })
    #output$text4 <- renderText({ return(paste("Note - Optimal factors from are:",unlist((nS())[1])[3])) })
    
    output$plot1 = renderPlot({
      if (is.null(input$file)) { return(NULL) }
      else{
        
        plotnScree(nS())
      }
    })
    
    output$plot20 = renderPlot({
      if (is.null(input$file)) { return(NULL) }
      else{
        
        a = unclass((fit())$loadings)
        grp = NULL
        for (i in 1:nrow(a)){
          max = max(abs(a[i,]))
          temp0 =  which(abs(a[i,]) == max)
          temp = temp0[1]
          grp = c(grp,temp)
        }
        grp = matrix(c(grp,seq(1:length(grp))),,2)
        rownames(grp) = colnames(Dataset())
        
        gr = vector("list", length = length(table(grp[,1])))
        for (i in 1:length(table(grp[,1]))) {
          l1  = grp[(grp[,1] == as.numeric(names(table(grp[,1])[i]))),2]
          gr[[i]][1:length(l1)] = c(l1)   
        }
        
        qgraph(cor(Dataset(), use= 'complete.obs'),layout="spring", groups = gr, labels=names(Dataset()), label.scale=F, label.cex = 1, minimum=input$cutoffcorr)
      }
    })
    
    output$plot2 = renderPlot({
      if (is.null(input$file)) { return(NULL) }
      else{
        
        a0 = (fit())$loadings
        a1 = a0
        for (i in 1:ncol(a1)){  a1[,i] = a0[,i]*(abs(a0[,i]) > input$cutoff)}
        k2 = f1()
        k3 = f2()
        
        factor.plot = function(a0, a1, k2, k3){
          
          load = a0[((a1[, k2] != 0)|(a1[, k3] != 0)), c(k2, k3)]
          
          par(col="black") #black lines in plots
          
          plot(load,type="p",pch=19,col="red", xlim=c(-1, 1), ylim=c(-1, 1)) # set up plot
          
          abline(h=0);abline(v=0)#draw axes
          
          arrows(0,0, x1=load[,1], y1=load[,2], col="blaCK", lwd=1.5);
          
          text(load,labels = rownames(load),cex=1,pos=1)
          
        } # factor.plot() func ends
        
        factor.plot(a0, a1, k2, k3)
      }
    })
    
    
    
    
    
    
    output$plot3 = renderPlot({
      if (is.null(input$file)) { return(NULL) }
      else{
        
        plot(x=(fit())$scores[,(f1())], y=(fit())$scores[,(f2())], type="p", pch=19, col="red")
        
        text(x=(fit())$scores[,(f1())],y=(fit())$scores[,(f2())],labels=rownames(Dataset()), pos = 2, col="blue", cex=0.8)
        
        abline(h=0); abline(v=0)
      }
    })
    
    output$loadings <- renderTable({ 
      if (is.null(input$file)) { return(NULL) }
      else{
        
        unclass((fit())$loadings)
      }
    })
    
    mat = reactive({
      fact = (fit())
      # SS.loadings= colSums(fact$loading*fact$loading)
      # Proportion.Var = colSums(fact$loading*fact$loading)/dim(fact$loading)[1]
      # Cumulative.Var= cumsum(colSums(fact$loading*fact$loading)/dim(fact$loading)[1])
      # mat = rbind(SS.loadings,Proportion.Var,Cumulative.Var)
      laodings = print(fact$loadings, digits=2, cutoff=.25, sort = TRUE)
      # out = list(Stat = mat, loadings=laodings)
      # return(laodings)
      
    })
    
    output$mat <- renderPrint({ 
      if (is.null(input$file)) { return(NULL) }
      else{ mat() }
      
    })
    
    uni = reactive({ 
      a = matrix(fit()$uniqueness,1,)
      colnames(a) = rownames(as.matrix(fit()$uniqueness))
      rownames(a) = "Uniqueness"
      return(a)
    })
    
    output$uni <- renderTable({ 
      if (is.null(input$file)) { return(NULL) }
      else{ uni() }
    })
    
    output$scores <- renderTable({     if (is.null(input$file)) { return(NULL) }
      else{
        unclass((fit())$scores)
      }
    })  
    
    output$downloadData <- downloadHandler(
      filename = function() { "Big_five_Survey_data.csv" },
      content = function(file) {
        write.csv(read.csv("data/Big_five_Survey_data.csv"), file, row.names=F, col.names=F)
      }
    )
    
  })
#################################################
# 02 -  Joint Space Map                         #    
#################################################
  jsm_server <- reactive({
    library("shiny")
    library("fmsb")
    
    build.spiderplot = function(dat, title){  # input DF with top 2 rows being maxmins
      
      radarchart(dat,
                 axistype = 1,     # 0 means no axis label. 1 means center axis label only. 2 means around-the-chart label only. Upto 5
                 seg = round(max(dat)-min(dat))+1 ,          # The number of segments for each axis (default 4).
                 
                 plty = 1:(nrow(dat)-2),         # plot ka line type.
                 plwd = 1:(nrow(dat)-2),
                 pcol = 1:(nrow(dat)-2),
                 pdensity = c(20, 40),  # A vector of filling density of polygons: Default NULL, which is repeatedly used.
                 pangle = c(10, 45),  # A vector of the angles of lines used as filling polygons: Default 45, which is repeatedly used.
                 pfcol = 1:(nrow(dat)-2),   # plot.fill.color, vector of color codes for filling polygons: Default NA
                 vlabels = colnames(dat), #c("Total\nQOL", "Physical\naspects",   # look at the way line break \n is used inside the vlabels
                 # "Phychological\naspects", "Social\naspects", "Environmental\naspects"),
                 title = title, # "(axis=1, 5 segments, with specified vlabels)",
                 vlcex=1)  # Font size magnification for vlabels. If NULL, the font size is fixed at text()'s default. Default NULL.
      
      legend("bottomright",
             legend = rownames(dat)[3:nrow(dat)],
             # pch = c(15,16),
             lwd = c(1:(nrow(dat)-2)),
             col = c(1:(nrow(dat)-2)),
             lty = c(1:(nrow(dat)-2)))
    }
    
    #++_____________++
    ### Data import:
    Dataset <- reactive({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      Dataset <- read.csv(input$file$datapath ,header=TRUE)
      row.names(Dataset) = Dataset[,1]; Dataset= Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      return(Dataset)
    })
    
    #++_____________++
    # Select variables:
    output$varselect <- renderUI({
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      # Variable selection:
      
      checkboxGroupInput("Attr", "Choose Attributes (At least 3 attributes must be selected)",
                         rownames(Dataset()), rownames(Dataset()))
      
    })
    
    #++_____________++
    output$varselect2 <- renderUI({
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      # Variable selection:
      
      checkboxGroupInput("rows", "Choose Firms (At least 2 Firms must be selected) - Only for Spider Chart",
                         colnames(Dataset()), colnames(Dataset()))
      
    })
    
    #++_____________++
    Dataset1 <- reactive({
      if (is.null(input$file1)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      Dataset1 <- read.csv(input$file1$datapath ,header=TRUE)
      row.names(Dataset1) = Dataset1[,1]; Dataset1= Dataset1[,2:ncol(Dataset1)]
      return(Dataset1)
    })
    
    #++_____________++
    # Select variables:
    output$varselect1 <- renderUI({
      if (identical(Dataset1(), '') || identical(Dataset1(),data.frame())) return(NULL)
      # Variable selection:
      
      checkboxGroupInput("users", "Choose Users (At least 1 user must be selected)",
                         rownames(Dataset1()), head(rownames(Dataset1())))
      
    })
    
    #++_____________++
    output$table <- renderTable({
      if (is.null(input$Attr) || length(input$Attr)==0) return(NULL)
      return((Dataset()[input$Attr,]))
    })
    
    #++_____________++
    output$table1 <- renderTable({
      if (is.null(input$users) || length(input$users)==0) return(NULL)
      return((Dataset1()[input$users,]))
    })
    
    #++_____________++
    output$plot = renderPlot({  
      
      if (is.null(input$file))
        return(NULL)
      
      if (is.null(input$file1))
        return(NULL)
      
      mydata = read.csv(input$file$datapath ,header=TRUE)
      row.names(mydata) = mydata[,1];
      mydata = mydata[,2:ncol(mydata)]; 
      mydata = t(mydata)
      mydata = mydata[,input$Attr]
      
      
      # mydata = t(as.data.frame(read.table(input$file$datapath ,header=TRUE)))
      # mydata = mydata[,input$Attr]
      
      pref = read.csv(input$file1$datapath ,header=TRUE)
      row.names(pref) = pref[,1]
      pref = pref[,2:ncol(pref)]
      pref = pref[input$users,]
      
      ### --- Para 3 of code ---- ###
      # --- write the JSM func ---
      
      JSM <- function(inp1, prefs){
        
        # inp1 = perception matrix with row and column headers
        # brands in rows and attributes in columns
        # prefs = preferences matrix
        
        par(pty="m")
        
        fit = prcomp(inp1, scale.=TRUE) # extract prin compts
        
        plot(fit$rotation[,1:2], # use only top 2 prinComps
             
             type ="n",xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), # plot parms
             
             main ="Joint Space map ") # plot title
        
        abline(h=0); abline(v=0) # build horiz & vert axes
        
        attribnames = colnames(inp1)
        
        brdnames = rownames(inp1)
        
        # <-- insert attrib vectors as arrows--
        
        for (i1 in 1:nrow(fit$rotation)){
          
          arrows(0,0, x1=fit$rotation[i1,1]*fit$sdev[1], y1=fit$rotation[i1,2]*fit$sdev[2], col="blue", lwd=1.5);
          
          text(x=fit$rotation[i1,1]*fit$sdev[1],y=fit$rotation[i1,2]*fit$sdev[2], labels=attribnames[i1],col="blue", cex=1.1)}
        
        # <--- make co-ords within (-1,1) frame #
        
        fit1 = fit
        
        fit1$x[,1] = fit$x[,1]/apply(abs(fit$x),2,sum)[1]
        
        fit1$x[,2] = fit$x[,2]/apply(abs(fit$x),2,sum)[2]
        
        points(x = fit1$x[,1], y = fit1$x[,2], pch = 19, col ="red")
        
        text(x = fit1$x[,1], y = fit1$x[,2], labels = brdnames,col ="black", cex = 1.1)
        
        # --- add preferences to map ---#
        
        k1 = 2; #scale-down factor
        
        pref = data.matrix(prefs)# make data compatible
        
        pref1 = pref %*% fit1$x[, 1:2]
        
        for (i1 in 1:nrow(pref1)){
          
          segments(0, 0, x1 = pref1[i1,1]/k1, y1 = pref1[i1,2]/k1, col="maroon2", lwd=1.25)
          
          points(x = pref1[i1,1]/k1, y = pref1[i1,2]/k1, pch=19, col="maroon2")
          
          text(x = pref1[i1,1]/k1, y = pref1[i1,2]/k1, labels = rownames(pref)[i1], adj = c(0.5, 0.5), col ="maroon2", cex = 1.1)
          
        }
        
        # voila, we're done! #
        
      } 					# JSM func ends
      
      JSM(mydata, pref)
      
    })
    
    # Show table:
    output$plot1 = renderPlot({  
      
      if (is.null(input$file))
        return(NULL)
      
      if (is.null(input$file1))
        return(NULL)
      
      mydata2 = read.csv(input$file$datapath ,header=TRUE)
      row.names(mydata2) = mydata2[,1];
      mydata2 = mydata2[,2:ncol(mydata2)]; 
      mydata2 = t(mydata2)
      mydata2 = mydata2[,input$Attr]
      fit = prcomp(mydata2) # extract prin compts
      plot(fit, "Variance of PCA")
      
    })
    
    output$spiderplot = renderPlot({  
      
      if (is.null(input$file))
        return(NULL)
      # if (is.null(input$file1))
      #   return(NULL)
      # 
      
      mydata3 = read.csv(input$file$datapath ,header=TRUE)
      row.names(mydata3) = mydata3[,1];
      mydata3 = mydata3[,2:ncol(mydata3)]; 
      mydata3 = t(mydata3)
      mydata3 = mydata3[input$rows,input$Attr]
      
      #mydata3 = mydata3[input$rows,input$Attr]
      
      brand = t(mydata3)
      brd.mean = apply(brand, 2, mean)
      
      b0 = t(brand)
      max1 = apply(b0, 2, max); min1 = apply(b0, 2, min);
      # max1; min1
      maxmin.df =  data.frame(rbind(max1, min1));
      # maxmin.df
      colnames(b0) = colnames(maxmin.df)
      brd.df = data.frame(rbind(maxmin.df, b0))
      #brd.df
      
      build.spiderplot(brd.df, "Spider Chart")
    })
    
    
    # Download data files
    output$downloadData1 <- downloadHandler(
      filename = function() { "officestar perceptual.csv" },
      content = function(file) {
        write.csv(read.csv("data/officestar perceptual.csv"), file, row.names=F)
      }
    )
    output$downloadData2 <- downloadHandler(
      filename = function() { "officestar preference.csv" },
      content = function(file) {
        write.csv(read.csv("data/officestar preference.csv"), file, row.names=F)
      }
    )
  })
#################################################
# 03 - Likart Visualization                     #    
#################################################  
  lkrt_server <- reactive({
    require(grid)
    require(lattice)
    require(latticeExtra)
    require(HH)
    
    ### Data import:
    Dataset <- reactive({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      return(Dataset1)
    })
    
    # Select variables:
    output$varselect <- renderUI({
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      # Variable selection:
      
      checkboxGroupInput("Attr", "Choose Attributes (At least 2 attributes must be selected)",
                         colnames(Dataset()), colnames(Dataset()))
      
    })
    
    output$table <- renderTable({
      if (is.null(input$Attr) || length(input$Attr)==0) return(NULL)
      return((Dataset()[,input$Attr]))
    })
    
    output$plot = renderPlot({  
      if (is.null(input$file))
        return(NULL)
      mydata = Dataset()
      mydata = mydata[,input$Attr]
      raw <- NULL
      for(i in 1:ncol(mydata)){
        raw <- rbind(raw, cbind(i,mydata[,i]))
      }
      
      r <- data.frame( cbind(
        as.numeric( row.names( tapply(raw[,2], raw[,1], mean) ) ),
        tapply(raw[,2], raw[,1], mean),
        tapply(raw[,2], raw[,1], mean) + sqrt( tapply(raw[,2], raw[,1], var)/tapply(raw[,2], raw[,1], length) ) * qnorm(1-.05/2,0,1),
        tapply(raw[,2], raw[,1], mean) - sqrt( tapply(raw[,2], raw[,1], var)/tapply(raw[,2], raw[,1], length) ) * qnorm(1-.05/2,0,1)
      ))
      names(r) <- c("group","mean","ll","ul")
      
      gbar <- tapply(raw[,2], list(raw[,2], raw[,1]), length)
      
      sgbar <- data.frame( cbind(c(1:max(unique(raw[,1]))),t(gbar)) )
      
      sgbar.likert<- sgbar[,2:ncol(sgbar)]
      sgbar.likert[is.na(sgbar.likert)] = 0
      
      if (ncol(sgbar.likert) == 3) {colnames(sgbar.likert) = c("Disagree","Neutral","Agree")}
      else if (ncol(sgbar.likert) == 5) {colnames(sgbar.likert) = c("Strongly Disagree","Disagree","Neutral","Agree","Strongly Agree")}
      else if (ncol(sgbar.likert) == 7) {colnames(sgbar.likert) = c("Strongly Disagree","Disagree","Slightly Disagree","Neutral","Slightly Agree","Agree","Strongly Agree")}
      else {cat("IDK")}
      
      rownames(sgbar.likert) = colnames(mydata)
      
      likert(sgbar.likert,
             main='Distribution of responses over Likert Scale',
             sub="Likert Scale"
             # BrewerPaletteName="RdBu"
      )
      
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { "Big_five_Survey_data.csv" },
      content = function(file) {
        write.csv(read.csv("data/Big_five_Survey_data.csv"), file, row.names=F, col.names=F)
      }
    )
    
  })
#################################################
# 04 -     OLS Regression                       #    
#################################################
  ols_server <- reactive({
    library(pastecs)
    library(RColorBrewer)
    library(Hmisc)
    library(ggplot2)
    library(reshape2)
    
    Dataset <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
        return(Dataset)
      }
    })
    
    # Select variables:
    output$yvarselect <- renderUI({
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      
      selectInput("yAttr", "Select Y variable",
                  colnames(Dataset()), colnames(Dataset())[1])
      
    })
    
    output$xvarselect <- renderUI({
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      
      checkboxGroupInput("xAttr", "Select X variables",
                         setdiff(colnames(Dataset()),input$yAttr), setdiff(colnames(Dataset()),input$yAttr))
      
    })
    
    output$fxvarselect <- renderUI({
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      
      checkboxGroupInput("fxAttr", "Select factor variable in X",
                         setdiff(colnames(Dataset()),input$yAttr),"" )
      
    })
    
    mydata = reactive({
      mydata = Dataset()[,c(input$yAttr,input$xAttr)]
      
      if (length(input$fxAttr) >= 1){
        for (j in 1:length(input$fxAttr)){
          mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
        }
      }
      return(mydata)
      
    })
    
    out = reactive({
      data = mydata()
      Dimensions = dim(data)
      Head = head(data)
      Tail = tail(data)
      Class = NULL
      for (i in 1:ncol(data)){
        c1 = class(data[,i])
        Class = c(Class, c1)
      }
      
      nu = which(Class %in% c("numeric","integer"))
      fa = which(Class %in% c("factor","character"))
      nu.data = data[,nu] 
      fa.data = data[,fa] 
      Summary = list(Numeric.data = round(stat.desc(nu.data) ,4), factor.data = describe(fa.data))
      
      a = seq(from = 0, to=200,by = 4)
      j = length(which(a < ncol(nu.data)))
      out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
      return(out)
    })
    
    output$summary = renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        out()[1:2]
      }
    })
    
    # output$scatterplots <- renderUI({
    #   if (is.null(input$file)) {return(NULL)}
    #   else {
    #     
    #     plot_output_list <- lapply(1:out()[[7]], function(i) {
    #       plotname <- paste("plot", i, sep="")
    #       plotOutput(plotname, height = 700, width = 700)
    #     })
    #     # Convert the list to a tagList - this is necessary for the list of items
    #     # to display properly.
    #     do.call(tagList, plot_output_list)
    #   }
    # })
    # 
    # # Call renderPlot for each one. Plots are only actually generated when they
    # # are visible on the web page.
    # max_plots = 50
    # 
    # for (i in 1:max_plots) {
    #   # Need local so that each item gets its own number. Without it, the value
    #   # of i in the renderPlot() will be the same across all instances, because
    #   # of when the expression is evaluated.
    #   local({
    #     
    #     my_i <- i 
    #     plotname <- paste("plot", my_i, sep="")
    #     
    #     output[[plotname]] <- renderPlot({
    #       out1 = out()
    #       a = out1[[6]]
    #       j = my_i
    #       if (ncol(out1[[5]]) == a[j] + 1){
    #         a1 = a[j]+1
    #         a2 = a[j]-1
    #         dai = out1[[5]][,a1:a2]
    #         plot(dai)
    #         }
    #       
    #       else if ( ncol(out1[[5]]) < a[j + 1]){
    #         a1 = a[j]+1
    #         a2 = ncol(out1[[5]])
    #         dai = out1[[5]][,a1:a2]
    #         plot(dai)
    #       }
    #       
    #       else if(ncol(out1[[5]]) > a[j + 1]){
    #         a1 = a[j]+1
    #         a2 = a[j + 1]
    #         dai = out1[[5]][,a1:a2]
    #         plot(dai)
    #       }
    #       
    #       mtext(paste("Scater plot " ,my_i), side = 3, line = 2, cex=2)
    #         })
    #   })
    # }
    
    output$heatmap = renderPlot({ 
      
      qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
        scale_fill_gradient2(limits=c(-1, 1))
      
    })
    
    output$correlation = renderPrint({
      cor(out()[[5]], use = "pairwise.complete.obs")
    })
    
    ols = reactive({
      rhs = paste(input$xAttr, collapse = "+")
      ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())
      return(ols)
    })
    
    ols2 = reactive({
      
      drop = which(input$yAttr == colnames(out()[[5]]))
      
      x0 = out()[[5]][,-drop]
      x01 = scale(x0, center = T, scale = T)
      
      y = out()[[5]][,drop]
      
      dstd = data.frame(y,x01)
      colnames(dstd) = c(input$yAttr,colnames(x01))
      
      if (ncol(data.frame(out()[[4]])) == 1) {
        fdata = data.frame(out()[[4]])
        colnames(fdata) = input$fxAttr
        dstd = data.frame(dstd,fdata)
      }
      
      else if (ncol(data.frame(out()[[4]])) > 1) {
        fdata = data.frame(out()[[4]])
        dstd = data.frame(dstd,fdata)
      }
      
      rhs = paste(input$xAttr, collapse = "+")
      ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = dstd)
      return(ols)
      
    })
    
    output$resplot1 = renderPlot({
      plot(ols()$residuals)
    })
    
    output$resplot2 = renderPlot({
      plot(ols()$residuals,ols()$fitted.values)
    })
    
    output$resplot3 = renderPlot({
      plot(mydata()[,input$yAttr],ols()$fitted.values)#
    })
    output$olssummary = renderPrint({
      summary(ols())
    })
    
    output$olssummarystd = renderPrint({
      summary(ols2())
    })
    
    output$datatable = renderTable({
      Y.hat = ols()$fitted.values
      data.frame(Y.hat,mydata())
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { "beer data.csv" },
      content = function(file) {
        write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
      }
    )
    
  })
#################################################
# 05 - Segmentation Discriminant and Targeting  #    
#################################################
  sdt_server <- reactive({
    library("shiny")
    library("cluster")
    library("ggbiplot")
    library("mclust")
    library("MASS")
    
    Dataset <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
        rownames(Dataset) = Dataset[,1]
        Dataset1 = Dataset[,2:ncol(Dataset)]
        #Dataset = t(Dataset)
        Dataset1 = scale(Dataset1, center = T, scale = T)
        return(Dataset1)
      }
    })
    
    Dataset2 <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
        rownames(Dataset) = Dataset[,1]
        Dataset1 = Dataset[,2:ncol(Dataset)]
        return(Dataset1)
      }
    })
    
    discri_data <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- as.data.frame(read.csv(input$file1$datapath ,header=TRUE, sep = ","))
        rownames(Dataset) = Dataset[,1]
        Dataset1 = Dataset[,2:ncol(Dataset)]
        return(Dataset1)
      }
    })
    
    target_data <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- as.data.frame(read.csv(input$file2$datapath ,header=TRUE, sep = ","))
        rownames(Dataset) = Dataset[,1]
        Dataset1 = Dataset[,2:ncol(Dataset)]
        return(Dataset1)
      }
    })
    
    output$downloadData1 <- downloadHandler(
      filename = function() { "ConneCtorPDASegmentation.csv" },
      content = function(file) {
        write.csv(read.csv("data/ConneCtorPDASegmentation.csv"), file, row.names=F)
      }
    )
    
    output$downloadData2 <- downloadHandler(
      filename = function() { "ConneCtorPDADiscriminant.csv" },
      content = function(file) {
        write.csv(read.csv("data/ConneCtorPDADiscriminant.csv"), file, row.names=F)
      }
    )
    
    output$downloadData3 <- downloadHandler(
      filename = function() { "ConneCtorPDAClassification.csv" },
      content = function(file) {
        write.csv(read.csv("data/ConneCtorPDAClassification.csv"), file, row.names=F, col.names=F)
      }
    )
    
    
    
    output$table <- renderDataTable({
      set.seed(12345)
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        else {
          fit = kmeans(Dataset(),input$Clust)
          Segment.Membership =  fit$cluster
          d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
          d
        }
      })
      
      else  if (input$select == "Model Based") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        else {
          fit = Mclust(Dataset(),input$Clust)
          Segment.Membership =  fit$classification
          d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
          d
        }
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          distm <- dist(Dataset(), method = "euclidean") # distance matrix
          fit <- hclust(distm, method="ward") 
          Segment.Membership =  cutree(fit, k=input$Clust)
          d = data.frame(r.name = row.names(Dataset2()),Segment.Membership,Dataset2())
          d
        }
      })  
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption1 <- renderText({
      if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
      else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
      else return (NULL)
    })
    
    output$caption2 <- renderText({
      if (input$select == "Model Based") 
      {
        fit0 = Mclust(Dataset())
        return(paste("Note - Optimal Segments Should be:",fit0$G,""))
      }
      else return(NULL)
    })
    
    output$summary <- renderPrint({
      
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          fit = kmeans(Dataset(),input$Clust)
          Segment.Membership = as.character(fit$cluster)
          clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
          Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
          Summary
        }
      })
      
      else  if (input$select == "Model Based") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        else {
          fit = Mclust(Dataset(),input$Clust)
          Segment.Membership = as.character(fit$classification)
          clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
          Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans,ModelSumm = summary(fit) )
          Summary
        }
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          d <- dist(Dataset(), method = "euclidean") # distance matrix
          fit <- hclust(d, method="ward") 
          Segment.Membership =  as.character(cutree(fit, k=input$Clust))
          clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
          Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership), ModelSumm = fit )
          Summary
        }
      })  
    })
    
    output$discriminat <- renderPrint({
      set.seed(12345)
      if (input$select == "K-Means") ({
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$cluster)
      })
      else  if (input$select == "Model Based") ({
        fit = Mclust(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$classification)
      })
      else if (input$select == "Hierarchical") ({
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        Segment.Membership =  as.character(cutree(fit, k=input$Clust))
      })
      
      Classification = Segment.Membership
      
      data = discri_data()
      
      fit <- lda(Classification ~ . , data=data, na.action = "na.omit", CV=TRUE)
      fit0 <- lda(Classification ~ . , data=data)
      ct <- table(Classification, fit$class)
      Proportion = diag(prop.table(ct, 1))
      Percent.Correct = sum(diag(prop.table(ct)))*100
      discri = list(confusion.matrix = ct, 
                    Proportion= Proportion, Percent.Correct = Percent.Correct,modelout = fit0)
      discri
    })
    
    ############------------------------------------------------------------------------------------------#############
    output$targeting <- renderPrint({
      set.seed(12345)
      if (input$select == "K-Means") ({
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$cluster)
      })
      else  if (input$select == "Model Based") ({
        fit = Mclust(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$classification)
      })
      else if (input$select == "Hierarchical") ({
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        Segment.Membership =  as.character(cutree(fit, k=input$Clust))
      })
      
      Classification = Segment.Membership
      data = discri_data()
      
      fit = lda(Classification ~ ., data=data)
      # , na.action = "na.omit", CV=F)
      prediction <- predict(fit, newdata=target_data())
      return(prediction)
    })
    ###############3-------------------------------------------------------------------3############################################
    
    
    output$table1 <- renderDataTable({
      
      if (input$select == "K-Means") ({
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$cluster)
      })
      else  if (input$select == "Model Based") ({
        fit = Mclust(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$classification)
      })
      else if (input$select == "Hierarchical") ({
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        Segment.Membership =  as.character(cutree(fit, k=input$Clust))
      })
      
      Classification = Segment.Membership
      data = discri_data()
      
      fit = lda(Classification ~ ., data=data, na.action = "na.omit", CV=F)
      prediction <- predict(fit, newdata=target_data())
      
      Targeted.segment = prediction$class
      data.frame(Targeted.segment, target_data())
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
    
    
    output$plotpca = renderPlot({ 
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        data.pca <- prcomp(Dataset(),center = TRUE,scale. = TRUE)
        plot(data.pca, type = "l"); abline(h=1)    
      }
    })
    
    output$plot = renderPlot({  
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        fit = kmeans(Dataset(),input$Clust)
        
        classif1 = as.character(fit$cluster)
        data.pca <- prcomp(Dataset(),
                           center = TRUE,
                           scale. = TRUE)
        
        # plot(data.pca, type = "l"); abline(h=1)    
        
        g <- ggbiplot(data.pca,
                      obs.scale = 1,
                      var.scale = 1,
                      groups = classif1,
                      ellipse = TRUE,
                      circle = TRUE)
        
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
        print(g)
        
      })
      
      else if (input$select == "Model Based") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        fit = Mclust(Dataset(),input$Clust)
        classif1 = as.character(fit$classification)
        data.pca <- prcomp(Dataset(),
                           center = TRUE,
                           scale. = TRUE)
        
        # plot(data.pca, type = "l"); abline(h=1)    
        
        g <- ggbiplot(data.pca,
                      obs.scale = 1,
                      var.scale = 1,
                      groups = classif1,
                      ellipse = TRUE,
                      circle = TRUE)
        
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
        print(g)
        
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        plot(fit) # display dendogram
        groups <- cutree(fit, k=input$Clust) # cut tree into 5 clusters
        # draw dendogram with red borders around the 5 clusters
        rect.hclust(fit, k=input$Clust, border="red") 
      })
    })
    
  })
#################################################
# 06 - Basic Text Analysis                      #    
#################################################  
  bta_server <- reactive({
    
    # library("shiny")
    library("tm")
    library("wordcloud")
    library("qdap")
    library("RWeka")
    library("igraph")
    
    # set.seed=2092014   
    
    distill.cog = function(dtm1, # input dtm
                           title, # title for the graph
                           s,    # no. of central nodes
                           k1){  # max no. of connections  
      
      mat = as.matrix((dtm1))  # input dtm here
      
      mat1 = t(mat) %*% mat    # build 1 mode term term matrix
      
      # diag(mat1) =  0 
      
      #  mat1[1:10,1:6]   # view a few rows n cols
      
      a = colSums(mat1)  # collect colsums into a vector obj a
      
      b = order(-a)     # nice syntax for ordering vector in decr order  
      
      mat2 = mat1[b,b]  # 
      
      #  mat2[1:10,1:6]
      
      diag(mat2) =  0
      
      ## +++ go row by row and find top k adjacencies +++ ##
      
      wc = NULL
      
      for (i1 in 1:s){ 
        
        thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
        
        mat2[i1, mat2[i1,] < thresh1] = 0   # wow. didn't need 2 use () in the subset here.
        
        mat2[i1, mat2[i1,] > 0 ] = 1
        
        word = names(mat2[i1, mat2[i1,] > 0])
        
        mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
        
        wc = c(wc,word)
        
      } # i1 loop ends
      
      
      mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
      
      ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
      
      mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
      
      graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
      
      graph = simplify(graph)  
      
      V(graph)$color[1:s] = "green"
      
      V(graph)$color[(s+1):length(V(graph))] = "pink"
      
      # plot(graph)#,layout=layout.lgl)
      
      plot(graph, layout=layout.kamada.kawai, main = title)
      
      # title(main = paste("Top Words used in review -",name))
      
    } # func ends
    
    
    len = function(x){
      if ( x == "-" && length(x) == 1)  {return (0)}
      else {return(length(unlist(x)))}
    }

    dataset <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        Document = readLines(input$file$datapath)
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        return(calib)}
      
    })
    
    x0 <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        progress <- shiny::Progress$new(session, min=1, max=2)
        on.exit(progress$close())
        
        progress$set(message = 'Text Cleaning in progress',
                     detail = 'This may take a while...')
        
        calib = (dataset())
        x  = as.character(calib$Document)        
        x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
        x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
        x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
        x  =  tolower(x)                          # convert to lower case characters
        x  =  removePunctuation(x)                # removing punctuation marks
        x  =  removeNumbers(x)                    # removing numbers
        x  =  stripWhitespace(x)                  # removing white space
        x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
        
        text  =  x;  rm(x)
        
        stp_word1 = stopwords('english')
        stp_word2 = readLines("data/stopwords.txt")
        comn  = unique(c(stp_word1, stp_word2))
        stp_word = unique(c(gsub("'","",comn),comn))
        sto = unique(c(stp_word,unlist(strsplit(input$stopw,","))))
        
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, tolower)
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,c(sto))
        myCorpus = tm_map(myCorpus, stripWhitespace)   # removes white space
        myCorpus = as.character(unlist(myCorpus))
        
        x1 = Corpus(VectorSource(myCorpus))
        return(x1)
        
        progress$set(value = 2)
        # Sys.sleep(0.5)
        
      }
    })
    
    tdm = reactive({ 
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        progress <- shiny::Progress$new(session, min=3, max=5)
        on.exit(progress$close())
        progress$set(message = 'Bigram creation in progress',
                     detail = 'This may take a while...')
        progress$set(value = 3)
        
        
        x1 = x0()
        calib = (dataset())
        ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
        
        tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                      tolower = TRUE, 
                                                      removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      stopwords = TRUE ))
        tdm = tdm0; rm('tdm0')
        a1 = apply(tdm, 1, sum)  
        a2 = ((a1 > 1))
        tdm.new = tdm[a2, ]
        rm('a1','a2','tdm')
        
        # remove blank documents (i.e. columns with zero sums)
        a0 = NULL; 
        for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
        length(a0)    # no. of empty docs in the corpus
        if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
        
        rm('a0','tdm.new')
        dim(tdm.new1)    # reduced tdm
        x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
        dim(x1mat);    # store[i1, 5] = ncol(x2mat);
        
        test = colnames(x1mat); 
        test1 = gsub(" ",".", test);  # replace spaces with dots
        colnames(x1mat) = test1
        
        a11 = apply(x1mat, 2, sum)
        a12 = order(a11, decreasing = T)
        a13 = as.matrix(a11[a12])
        
        #x1 = tm_map(x1, stripWhitespace)
        x1 = unlist(lapply(x1, content)) 
        for (i in 1:nrow(a13)){    
          focal.term = gsub("\\.", " ", rownames(a13)[i])
          replacement.term = gsub(" ", "-", focal.term)
          replacement.term=paste("",replacement.term,"")
          x1 = gsub(focal.term, replacement.term, x1)  
          
        }	# now, our x corpus has the top 400 bigrams encoded as unigrams
        
        progress$set(message = 'TDM creation in progress',
                     detail = 'This may take a while...')
        progress$set(value = 4)
        
        
        x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
        tdm = TermDocumentMatrix(x1)
        colnames(tdm) = calib$Doc.id
        
        if (input$ws == "tf"){
          # cat("Hi")
          tdm = weightTf(tdm)
        }
        else if (input$ws == "tf-idf"){
          
          tdm = weightTfIdf(tdm, normalize = F )
          
        }
        
        tdm = tdm[rowSums(as.matrix(tdm)) > 1,] 
        tdm = tdm[,colSums(as.matrix(tdm)) > 0] 
        
        return(tdm)
      }
      
      progress$set(value = 5)
    })
    
    output$wordword <- renderPlot({    
      
      # wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      
      # mat = as.matrix(tdm()[order(rowSums(as.matrix(tdm())),decreasing=T),])
      # mat = mat[1:input$max,]
      # 
      # cmat  =  mat %*% t(mat)
      # diag(cmat) = 0
      # cmat[cmat <  quantile(cmat,.90)] = 0
      # 
      # graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)
      # # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
      # plot(  graph,			#the graph to be plotted
      #       layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
      #       #main='Organizational network example',	#specifies the title
      #       #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
      #       vertex.frame.color='blue', 		#the color of the border of the dots 
      #       vertex.label.color='black',		#the color of the name labels
      #       vertex.label.font=1,			#the font of the name labels
      #       vertex.size = .00001,
      #       # vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
      #       vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
      #)
      distill.cog(t(tdm()),'Co-occurrence Graph',
                  input$nodes,
                  input$connection)
    })
    
    
    ###################})
    output$dtmsummary  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        cat("Weighing Scheme is ",input$ws,"\n\n")
        # tdm()[order(rowSums(as.matrix(tdm())),decreasing=T),]
        inspect(tdm()[order(rowSums(as.matrix(tdm())),decreasing=T),][1:10,1:10])}
      
    })
    
    output$wordcloud <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        m  = as.matrix((tdm()))
        v = sort(rowSums(m), decreasing = TRUE)
        wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      }
    })
    
    output$dtmsummary1  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        # cat("Weights of Terms in  Wordcloud","\n\n")
        m  = as.matrix(tdm())
        v = sort(rowSums(m), decreasing = TRUE)
        z = data.frame(v[1:input$max]); colnames(z) = "Weights"
        z
      }
    })
    
    
    norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
    
    m_norm = reactive({
      m_norm <- norm_eucl(t(tdm()))
      return(m_norm)
    })
    
    data.pca <- reactive({prcomp(m_norm(),center = TRUE,scale. = TRUE)})
    
    output$pcaplot <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        plot(data.pca(), type = "l"); abline(h=1)
      }
    })
    
    clust = reactive({
      set.seed(19890201)
      cl <- kmeans(m_norm(), input$seg)
      return(cl)
    })
    
    da = reactive({
      da = data.frame(as.numeric(colnames(tdm())),clust()$cluster)  
      colnames(da) = c("Doc.id","Seg Membership")
      return(da)
    })
    
    
    output$summary  <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        fit = clust()
        Segment.Membership = as.character(fit$cluster)
        # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list( Count = table(Segment.Membership),Segment.Membership = Segment.Membership )
        Summary
      }
    })
    
    
    output$segplots <- renderUI({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        plot_output_list <- lapply(1:input$seg, function(i) {
          plotname <- paste("plot", i, sep="")
          plotOutput(plotname, height = 700, width = 700)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      }
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    max_plots = 20
    
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        my_i <- i 
        plotname <- paste("plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          cl = clust()
          pct = round((cl$size/sum(cl$size))*100,2)
          
          stdm = t(t(tdm())[(cl$cluster == my_i),])
          m  = as.matrix(stdm)
          v = sort(rowSums(m), decreasing = TRUE)
          v = data.frame(v[v > 0 ])
          if (nrow(v) >= 40) {n = 40}
          else {n = nrow(v)}
          top_word = as.matrix(v[1:n,])
          row.names(top_word) = row.names(v)[1:n]
          
          # title(main =paste("Segment ",my_i))
          wordcloud(rownames(top_word), top_word,  scale=c(4,1), 1, random.order=FALSE, random.color=FALSE, colors=brewer.pal(8, "Dark2"));
          mtext(paste("Segment",my_i, "(",pct[my_i],"%)"), side = 3, line = 2, cex=2)
          
          box(lty = '11', col = 'black')
        })
      })
    }
    
    #-----------------------------------------
    
    output$segcoocrplots <- renderUI({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        plot_output_list <- lapply(1:input$seg, function(i) {
          plotname <- paste("plot1", i, sep="")
          plotOutput(plotname, height = 700, width = 700)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      }
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    max_plots = 20
    
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        my_i <- i 
        plotname <- paste("plot1", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          cl = clust()
          pct = round((cl$size/sum(cl$size))*100,2)
          
          stdm = t(t(tdm())[(cl$cluster == my_i),])
          # m  = as.matrix(stdm)
          # v = sort(rowSums(m), decreasing = TRUE)
          # v = data.frame(v[v > 0 ])
          # if (nrow(v) >= 40) {n = 40}
          # else {n = nrow(v)}
          
          # mat = as.matrix(stdm[order(rowSums(as.matrix(stdm)),decreasing=T),][1:n,])
          # 
          # cmat  =  mat %*% t(mat)
          # diag(cmat) = 0
          # cmat[cmat <  quantile(cmat,.90)] = 0
          # 
          # graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)
          # # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
          # plot(  graph,			#the graph to be plotted
          #        layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
          #        #main='Organizational network example',	#specifies the title
          #        #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
          #        vertex.frame.color='blue', 		#the color of the border of the dots 
          #        vertex.label.color='black',		#the color of the name labels
          #        vertex.label.font=1,			#the font of the name labels
          #        vertex.size = .00001,
          #        # vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
          #        vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
          #        # title( main = paste("Segemnt ",my_i))
          #        )
          
          distill.cog(t(stdm),'',
                      input$nodes,
                      input$connection)
          
          mtext(paste("Segment",my_i, "(",pct[my_i],"%)"), side = 3, line = 2, cex=2)
          
          box(lty = '11', col = 'black')
        })
      })
    }
    
    #-----------------------------------------
    
    p  = reactive({
      
      
      progress <- shiny::Progress$new(session, min=6, max=10)
      on.exit(progress$close())
      progress$set(message = 'Sentiment Analysis Started',
                   detail = 'This may take a while...')
      progress$set(value = 6)
      
      # head(data$Document)
      
      data = dataset()
      data$Document = gsub("<.*?>", "", data$Document)
      data$Document = gsub("/", "", data$Document) 
      data$Document = gsub("\\\\", "", data$Document) 
      data$Document = gsub("\\)", "", data$Document)
      data$Document = gsub("\\(", "", data$Document)
      
      data$Document =  tolower(data$Document)
      
      progress$set(message = 'Calculating Sentiment Score',
                   detail = 'This may take a while...')
      progress$set(value = 7)
      
      p = with(data, polarity(Document, Doc.id))
      
      
      progress$set(message = 'Identifying positive/negative words',
                   detail = 'This may take a while...')
      progress$set(value = 8)
      
      pwords = gsub(" ","-",setdiff(unique(unlist(p$all$pos.words)),"-"))
      nwords = gsub(" ","-",setdiff(unique(unlist(p$all$neg.words)),"-"))
      
      repl = c(pwords[grep("-",pwords)],nwords[grep("-",nwords)])
      
      progress$set(message = 'Creating Positive/Negative TDMS',
                   detail = 'This may take a while...')
      progress$set(value = 9)
      
      if (length(repl) != 0) {
        text = data$Document
        for (i in 1:length(repl)){
          fterm = gsub("-"," ",repl[i])
          rterm = repl[i]
          text = gsub(fterm,rterm,text)
        }
      }
      else {
        text = data$Document 
      }
      myCorpus = Corpus(VectorSource(text))
      tdm = TermDocumentMatrix(myCorpus)
      dim(tdm)
      pos.tdm = tdm[!is.na(match(row.names(tdm),pwords)),]
      neg.tdm = tdm[!is.na(match(row.names(tdm),nwords)),]
      out = list(p, pos.tdm,neg.tdm)
      
      progress$set(message = 'Storing Results',
                   detail = 'This may take a while...')
      progress$set(value = 10)
      return(out)
      
      
    })
    
    output$posplot <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        p1 = p()
        pos.tdm = p1[[2]]
        m  = as.matrix(pos.tdm)
        v = sort(rowSums(m), decreasing = TRUE)
        wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      }
    })
    
    
    output$negplot <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        p1 = p()
        neg.tdm = p1[[3]]
        m  = as.matrix(neg.tdm)
        v = sort(rowSums(m), decreasing = TRUE)
        wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      }
    })
    
    
    output$negplot <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        p1 = p()
        neg.tdm = p1[[3]]
        m  = as.matrix(neg.tdm)
        v = sort(rowSums(m), decreasing = TRUE)
        wordcloud(names(v), v, scale=c(4,1),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      }
    })
    
    output$flowplot <- renderPlot({
      p1 = p()
      plot(p1[[1]]$all$polarity, type = "l", ylab = "Polarity Score",xlab = "Document Number")
      abline(h=0)
    })
    
    output$table <- renderDataTable({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        p1 = p()
        d = data.frame(p1[[1]]$all$polarity,dataset())
        d = d[,c(2,1,3)]; colnames(d) = c("Doc.id","Polarity Score","Document")
        
        test = merge(da(),d,by.x ="Doc.id", by.y= "Doc.id", all=T)
        
        test}
      
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
    
    output$table0 <- renderDataTable({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        tb = da()
        test = merge(tb,dataset(),by.x ="Doc.id", by.y= "Doc.id", all=T)
        return(test)
      }
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
    
    output$start = renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        
        out = list(tdm(),p(),clust(),data.pca())
        cat("Calculation Completed")
      }
    })
    
    output$downloadData1 <- downloadHandler(
      filename = function() { "Nokia_Lumia_reviews.txt" },
      content = function(file) {
        writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
      }
    )
    
  })
#################################################
# 07 - Text Topic Analysis                      #    
#################################################  
  tta_server <- reactive({
    library(shiny)
    library(tm)
    library(RWeka)
    library(maptpx)
    library(wordcloud)
    library(stringr)
    library(igraph)
    
    distill.cog = function(dtm1, # input dtm
                           title, # title for the graph
                           s,    # no. of central nodes
                           k1){  # max no. of connections  
      
      mat = as.matrix((dtm1))  # input dtm here
      
      mat1 = t(mat) %*% mat    # build 1 mode term term matrix
      
      # diag(mat1) =  0 
      
      #  mat1[1:10,1:6]   # view a few rows n cols
      
      a = colSums(mat1)  # collect colsums into a vector obj a
      
      b = order(-a)     # nice syntax for ordering vector in decr order  
      
      mat2 = mat1[b,b]  # 
      
      #  mat2[1:10,1:6]
      
      diag(mat2) =  0
      
      ## +++ go row by row and find top k adjacencies +++ ##
      
      wc = NULL
      
      for (i1 in 1:s){ 
        
        thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
        
        mat2[i1, mat2[i1,] < thresh1] = 0   # wow. didn't need 2 use () in the subset here.
        
        mat2[i1, mat2[i1,] > 0 ] = 1
        
        word = names(mat2[i1, mat2[i1,] > 0])
        
        mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
        
        wc = c(wc,word)
        
      } # i1 loop ends
      
      
      mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
      
      ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
      
      mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
      
      graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
      
      graph = simplify(graph)  
      
      V(graph)$color[1:s] = "green"
      
      V(graph)$color[(s+1):length(V(graph))] = "pink"
      
      # plot(graph)#,layout=layout.lgl)
      
      plot(graph, layout=layout.kamada.kawai, main = title)
      
      # title(main = paste("Top Words used in review -",name))
      
    }  
    
    set.seed=2092014   
    
    dataset <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        Document = readLines(input$file$datapath)
        Document = iconv(Document, "latin1", "ASCII", sub="")
        x = Document
        
        if (length(x) < 2) {
          x2 = paste(x, collapse=" ")
          x2 = unlist(str_split(x2,"\\s"))
          x2 = x2[(x2 != "")]
          
          x3 = character(0)
          i = 1 ; j = 25
          for (k in 1:round(length(x2))) {        
            if (j+25 > length(x2)) {j = length(x2)}         
            temp = paste(x2[i:j], collapse = " ")        
            i = i+25 ;j = j+25         
            x3 = c(x3, temp)        
            if (j == length(x2)+25 ) break
          }
          Document = as.character(x3)
        }
        
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        return(calib)}
      
    })
    
    tdm <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        calib = (dataset())
        text  = as.character(calib$Document)      
        
        progress <- shiny::Progress$new(session, min=1, max=4)
        on.exit(progress$close())
        
        progress$set(value = 1)
        progress$set(message = 'Text Cleaning in progress',
                     detail = 'This may take a while...')
        
        
        x  =  gsub("<.*?>", "", text)                  # regex for removing HTML tags
        x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
        x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
        x  =  tolower(x)                          # convert to lower case characters
        x  =  removePunctuation(x)                # removing punctuation marks
        x  =  removeNumbers(x)                    # removing numbers
        x  =  stripWhitespace(x)                  # removing white space
        x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
        
        text  =  x;  rm(x)
        
        stp_word1 = stopwords('english')
        stp_word2 = readLines("data/stopwords.txt")
        comn  = unique(c(stp_word1, stp_word2))
        stp_word = unique(c(gsub("'","",comn),comn))
        sto = unique(c(stp_word,unlist(strsplit(input$stopw,","))))
        
        progress$set(value = 2)
        
        myCorpus = Corpus(VectorSource(text))
        myCorpus = tm_map(myCorpus, tolower)
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords,c(sto))
        myCorpus = tm_map(myCorpus, stripWhitespace)   # removes white space
        myCorpus = as.character(unlist(myCorpus))
        
        x1 = Corpus(VectorSource(myCorpus))
        
        progress$set(message = 'Bigram creation in progress',
                     detail = 'This may take a while...')
        progress$set(value = 3)
        
        
        ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
        
        tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                      tolower = TRUE, 
                                                      removePunctuation = TRUE,
                                                      removeNumbers = TRUE,
                                                      stopwords = TRUE ))
        #stemDocument = TRUE ))    # patience. Takes a minute.
        
        tdm = tdm0; rm('tdm0')
        
        a1 = apply(tdm, 1, sum)  
        a2 = ((a1 > 1))
        tdm.new = tdm[a2, ]
        rm('a1','a2','tdm')
        
        # remove blank documents (i.e. columns with zero sums)
        a0 = NULL; 
        for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
        length(a0)    # no. of empty docs in the corpus
        if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
        
        rm('a0','tdm.new')
        dim(tdm.new1)    # reduced tdm
        x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
        dim(x1mat);  	# store[i1, 5] = ncol(x2mat);
        
        test = colnames(x1mat); 
        test1 = gsub(" ",".", test);  # replace spaces with dots
        colnames(x1mat) = test1
        
        ## build quick wordcloud of 50 most freq terms
        
        a11 = apply(x1mat, 2, sum)
        a12 = order(a11, decreasing = T)
        a13 = as.matrix(a11[a12])
        
        #x1 = tm_map(x1, stripWhitespace)
        x1 = unlist(lapply(x1, content)) 
        for (i in 1:nrow(a13)){    
          focal.term = gsub("\\.", " ", rownames(a13)[i])
          replacement.term = gsub(" ", "-", focal.term)
          replacement.term=paste("",replacement.term,"")
          x1 = gsub(focal.term, replacement.term, x1)  
          
        }	# now, our x corpus has the top 400 bigrams encoded as unigrams
        
        progress$set(message = 'TDM creation in progress',
                     detail = 'This may take a while...')
        progress$set(value = 4)
        
        x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
        tdm = TermDocumentMatrix(x1)
        colnames(tdm) = calib$Doc.id
        
        return(tdm)
      }
    })
    
    output$wordcloud <- renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        m = as.matrix((tdm()))
        v = sort(rowSums(m), decreasing = TRUE)
        wordcloud(names(v), v, scale=c(4,0.5),input$freq, max.words=input$max,colors=brewer.pal(8, "Dark2"))
      }
    })
    
    x2mat <- reactive({
      
      if (is.null(input$file)) {return(NULL)}
      else {
        
        progress <- shiny::Progress$new(session, min=5, max=6)
        on.exit(progress$close())
        
        progress$set(message = 'TDM creation in progress',
                     detail = 'This may take a while...')
        progress$set(value = 5)
        
        tdm = (tdm())
        a1 = apply(tdm, 1, sum)
        a2 =((a1 >= input$tdmfreq))
        tdm.new = tdm[a2, ]
        
        # remove blank documents (i.e. columns with zero sums)
        a0 = NULL; 
        for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
        length(a0)    # no. of empty docs in the corpus
        if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
        
        dim(tdm.new1)		# reduced tdm
        x2mat = t(tdm.new1)		# don't do tfidf, not mentioned anywhere for topic modeling.
        dim(x2mat);		# store[i1, 5] = ncol(x2mat);
        
        test = colnames(x2mat); 
        test1 = gsub(" ",".", test);  # replace spaces with dots
        colnames(x2mat) = test1
        progress$set(value = 5)
        return(x2mat) 
      }
    })
    
    #   Optimal Topics by BIC 
    output$caption1 <- renderText({
      if (is.null(input$file)) {return(NULL)}
      else {
        progress <- shiny::Progress$new(session, min=7, max=7)
        on.exit(progress$close())
        
        progress$set(message = 'Finding optimal topics',
                     detail = 'This may take a while...')
        progress$set(value = 7)
        
        
        K=6
        x2mat = (x2mat())
        summary(simselect <- topics(x2mat, K=K+c(-4:4)), nwrd=0)
        return(paste("Note - Optimal Topics from Log Bayes Factor for selected Term Document Matrix Should be:",simselect$K,""))
      }
      
    })
    
    simfit <- reactive({ 
      
      progress <- shiny::Progress$new(session, min=8, max=8)
      on.exit(progress$close())
      
      progress$set(message = 'Fitting K topics Model',
                   detail = 'This may take a while...')
      progress$set(value = 8)
      
      summary(simfit <- topics((x2mat()),  K=input$topic, verb=2),nwrd = 12 )
      return(simfit)
    })
    
    output$summary <- renderPrint({
      summary(simfit())
    })
    
    lift = reactive({
      theta = (simfit())$theta
      x2mati = (x2mat())
      lift = theta*0;  sum1 = sum(x2mati)
      
      for (i in 1:nrow(theta)){  
        for (j in 1:ncol(theta)){
          
          ptermtopic = 0; pterm = 0;
          
          ptermtopic = theta[i, j]
          
          pterm = sum(x2mati[,i])/sum1
          
          lift[i, j] = ptermtopic/pterm
          
        }
      }
      return(lift)
    })
    
    theta = reactive({
      theta = as.matrix((simfit())$theta)
      return(theta)
    })
    
    output$plots2 <- renderUI({
      plot_output_list <- lapply(1:input$topic, function(i) {
        plotname <- paste("plot", i, sep="")
        plotOutput(plotname, height = 800, width = 800)
      })
      
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        my_i <- i 
        plotname <- paste("plot", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          freq = as.matrix((theta())[(match(rownames((lift())[((lift())[,my_i] > 1),]),rownames((theta())))),][,my_i])
          freq = as.matrix(freq[(order(freq[,1], decreasing=T)),])
          if (nrow(freq) >= 40) {n = 40}
          else {n = nrow(freq)}
          top_word = as.matrix(freq[1:n,])
          #plot.new()
          title(main =paste("Latent Topic",my_i))
          wordcloud(rownames(top_word), top_word,  scale=c(4,0.5), 1, , random.order=FALSE, random.color=FALSE, colors=brewer.pal(8, "Dark2"));
          #title(main =paste("Latent Topic",my_i))
          mtext(paste("Latent Topic",my_i), side = 3, line = 2, cex=2)
          box(lty = '11', col = 'black')
        })
      })
    }
    
    ############################--------=------
    output$plots3 <- renderUI({
      plot_output_list <- lapply(1:input$topic, function(i) {
        plotname <- paste("plot1", i, sep="")
        plotOutput(plotname, height = 800, width = 800)
      })
      
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    
    for (i in 1:max_plots) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        
        my_i <- i 
        plotname <- paste("plot1", my_i, sep="")
        
        output[[plotname]] <- renderPlot({
          
          freq = as.matrix((theta())[(match(rownames((lift())[((lift())[,my_i] > 1),]),rownames((theta())))),][,my_i])
          
          freq1 = as.matrix(freq[(order(freq[,1], decreasing=T)),])
          
          if (nrow(freq1) >= 40) {n = 40}
          else {n = nrow(freq1)}
          
          mat  = tdm()[match(row.names(freq1),row.names(tdm())),]
          
          # mat = as.matrix(mat[order(rowSums(as.matrix(mat)),decreasing=T),][1:n,])
          # 
          # cmat  =  mat %*% t(mat)
          # diag(cmat) = 0
          # cmat[cmat <  quantile(cmat,.90)] = 0
          
          distill.cog(t(mat),'',
                      input$nodes,
                      input$connection)
          
          
          # graph <- graph.adjacency(cmat, mode = "undirected",weighted=T)
          # # par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
          # plot(  graph,			#the graph to be plotted
          #        layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
          #        #main='Organizational network example',	#specifies the title
          #        #vertex.label.dist=0.5,			#puts the name labels slightly off the dots
          #        vertex.frame.color='blue', 		#the color of the border of the dots 
          #        vertex.label.color='black',		#the color of the name labels
          #        vertex.label.font=1,			#the font of the name labels
          #        vertex.size = .00001,
          #        # vertex.label=col.names,		#specifies the lables of the vertices. in this case the 'name' attribute is used
          #        vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
          #        # title( main = paste("Segemnt ",my_i))
          # )
          
          mtext(paste("Topic",my_i), side = 3, line = 2, cex=2)
          
          box(lty = '11', col = 'black')
          
        })
      })
    }
    
    #######################################
    eta = function(mat, dtm) {
      mat1 = mat/mean(mat);  terms1 = rownames(mat1);
      eta.mat = matrix(0, 1, ncol(mat1))
      
      for (i in 1:nrow(dtm)){
        a11 = as.data.frame(matrix(dtm[i,]));  rownames(a11) = colnames(dtm)
        a12 = as.matrix(a11[(a11>0),]);  rownames(a12) = rownames(a11)[(a11>0)];	rownames(a12)[1:4]
        a13 = intersect(terms1, rownames(a12));		a13[1:15];	length(a13)
        a14a = match(a13, terms1); 		# positions of matching terms in mat1 matrix
        a14b = match(a13, rownames(a12))		
        a15 = mat1[a14a,]*matrix(rep(a12[a14b,], ncol(mat1)), ncol = ncol(mat1))
        eta.mat = rbind(eta.mat, apply(a15, 2, mean))	
        rm(a11, a12, a13, a14a, a14b, a15)
      }
      eta.mat = eta.mat[2:nrow(eta.mat), ] 	# remove top zeroes row
      row.names(eta.mat)=row.names(dtm)
      return(eta.mat)
    }
    
    twc = reactive ({
      twc = eta((lift()),(x2mat()))
      return(twc)
    })
    
    # 
    #   output$summary2 <- renderTable({
    #   
    #       if (is.null(input$file)) {
    #         # User has not uploaded a file yet
    #         return(NULL)
    #       }
    #       
    #       else {
    # tb = data.frame(twc())
    # colnames(tb) = paste("Topic",1:ncol(tb))
    # round(tb,3)
    #       }
    #     })
    #     
    
    # Show table:
    output$table <- renderDataTable({
      if (is.null(input$file)) {return(NULL)}
      {
        tb = twc()
        tb = tb/rowSums(tb)
        tb = tb*100
        tb = data.frame(as.numeric(rownames(tb)),tb)
        colnames(tb) = c("Doc.id",paste("Topic",1:(ncol(tb)-1)))
        
        test = merge(tb,dataset(),by.x ="Doc.id", by.y= "Doc.id", all=T)
        return(test)}
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    output$tmp <- renderPrint({
      if (is.null(input$file)) {return(NULL)}
      else {
        out = list(tdm(),x2mat(),theta(),lift(),twc())
        cat("Calculation Completed")}
    })
    
    output$downloadData1 <- downloadHandler(
      filename = function() { "Nokia_Lumia_reviews.txt" },
      content = function(file) {
        writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
      }
    )    
    
  })
#################################################
# 08 - Network Analysis                         #    
#################################################  
  netwrk_server <- reactive({
    library("igraph")
    
    Dataset <- reactive({
      if (is.null(input$file)) { return(NULL) }
      else{
        Dataset <- read.csv(input$file$datapath ,header=TRUE, sep = ",")
        row.names(Dataset) = Dataset[,1]
        Dataset = as.matrix(Dataset[,2:ncol(Dataset)])
        return(Dataset)
      }
    })
    
    Dataset2 <- reactive({
      if (is.null(input$file1)) { return(NULL) }
      else{
        Dataset <- read.csv(input$file1$datapath ,header=TRUE, sep = ",")
        row.names(Dataset) = Dataset[,1]
        Dataset = Dataset[,2:ncol(Dataset)]
        return(Dataset)
      }
    })
    
    # Select variables:
    output$yvarselect <- renderUI({
      if (is.null(input$file1)) { return(NULL) }
      else{
        
        selectInput("colattr", "Select Color variable",
                    colnames(Dataset2()), colnames(Dataset2())[1])
      }
    })
    
    graph = reactive({
      graph <- graph.adjacency(Dataset(), mode = input$mode, weighted=NULL)
      graph = simplify(graph)  
      # col.names <- make.names(V(graph)$name, unique = TRUE)
      return(graph)
    })
    
    centralities = reactive({
      graph = graph()
      metrics <- data.frame(Resp.Name = make.names(V(graph)$name, unique = TRUE),Degree=degree(graph), Out.Degree =degree(graph, v=V(graph), mode=c("out")),In.Degree =degree(graph, v=V(graph), mode=c("in")),
                            Betweenness=betweenness(graph), Closeness = closeness(graph), Eigenvector.Centrality.Scores = evcent(graph)$vector, Graph.Coreness = graph.coreness(graph))
      # row.names(metrics) = V(graph)$name
      
      metrics = metrics[(order(metrics[,1],metrics[,2],metrics[,3],metrics[,4],metrics[,5],metrics[,6],metrics[,7], decreasing= T)),]
      
      return(metrics)
    })
    
    output$centdata = renderDataTable({
      centralities()
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
    
    output$graph1 = renderPlot({
      if (is.null(input$file)) { return(NULL) }
      else{
        graph = graph()
        par(mai=c(0,0,0,0))   		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
        plot( graph,			#the graph to be plotted
              layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
              vertex.frame.color='blue', 		#the color of the border of the dots 
              vertex.color= Dataset2()[,input$colattr],
              vertex.label.color='black',		#the color of the name labels
              vertex.label.font=1,    			#the font of the name labels
              vertex.size = input$cex2,     # size of the vertex
              vertex.label= make.names(V(graph)$name, unique = TRUE),	    	#specifies the lables of the vertices. in this case the 'name' attribute is used
              vertex.label.cex=input$cex		#specifies the size of the font of the labels. can also be made to vary
              
        ) 
      }
    })
    
  })
#################################################
# 08 - end of functions                         #    
#################################################  
  
uitemp  <-reactive({
    if (input$appname == 'Factor Analysis'){
      # input$goButton
      uis <- source('factor_ui.s.r', local = T)$value
    } else if (input$appname == 'Joint Space Map'){
      # input$goButton
      uis <- source('jsm_ui.s.r', local = T)$value
    } else if (input$appname == 'Likart Visualization'){
      # input$goButton
      uis <- source('lkrt_ui.s.r', local = T)$value
    } else if (input$appname == 'OLS Regression'){
      # input$goButton
      uis <- source('ols_ui.s.r', local = T)$value
    } else if (input$appname == 'Segmentation Discriminant and Targeting'){
      # input$goButton
      uis <- source('sdt_ui.s.r', local = T)$value
    } else if (input$appname == 'Text Analysis'){
      # input$goButton
      uis <- source('bta_ui.s.r', local = T)$value
    } else if (input$appname == 'Text Topic Analysis'){
      # input$goButton
      uis <- source('tta_ui.s.r', local = T)$value
    } else if (input$appname == 'Network Analysis'){
      # input$goButton
      uis <- source('netwrk_ui.s.r', local = T)$value
    }
    return(uis)
  })
  
  
    output$ui.s = renderUI({
    #   observe({
    # if (input$appname == 'Factor Analysis'){
    #  # input$goButton
    #   uis <- source('factor_ui.s.r', local = T)$value
    # } else if (input$appname == 'Joint Space Map'){
    #   # input$goButton
    #   uis <- source('jsm_ui.s.r', local = T)$value
    # } else if (input$appname == 'Likart Visualization'){
    #   # input$goButton
    #   uis <- source('lkrt_ui.s.r', local = T)$value
    # } else if (input$appname == 'OLS Regression'){
    #   # input$goButton
    #   uis <- source('ols_ui.s.r', local = T)$value
    # } else if (input$appname == 'Segmentation Discriminant and Targeting'){
    #   # input$goButton
    #   uis <- source('sdt_ui.s.r', local = T)$value
    # } else if (input$appname == 'Basic Text Analysis'){
    #   # input$goButton
    #   uis <- source('bta_ui.s.r', local = T)$value
    # } else if (input$appname == 'Text Topic Analysis'){
    #   # input$goButton
    #   uis <- source('tta_ui.s.r', local = T)$value
    # } else if (input$appname == 'Network Analysis'){
    #   # input$goButton
    #   uis <- source('netwrk_ui.s.r', local = T)$value
    # }
    # return(uis)
    #   })
      uitemp()
  })
  
  output$ui.m = renderUI({
    if (input$appname == 'Factor Analysis'){
      # input$goButton
      source('factor_ui.m.r', local = T)$value
    } else if (input$appname == 'Joint Space Map'){
      # input$goButton
      source('jsm_ui.m.r', local = T)$value
    } else if (input$appname == 'Likart Visualization'){
      # input$goButton
      source('lkrt_ui.m.r', local = T)$value
    } else if (input$appname == 'OLS Regression'){
      # input$goButton
      source('ols_ui.m.r', local = T)$value
    } else if (input$appname == 'Segmentation Discriminant and Targeting'){
      # input$goButton
      source('sdt_ui.m.r', local = T)$value
    } else if (input$appname == 'Text Analysis'){
      # input$goButton
      source('bta_ui.m.r', local = T)$value
    } else if (input$appname == 'Text Topic Analysis'){
      # input$goButton
      source('tta_ui.m.r', local = T)$value
      # input$goButton
    } else if (input$appname == 'Network Analysis'){
      # input$goButton
      source('netwrk_ui.m.r', local = T)$value
    }
    
  })
  
  output$start = renderPrint({
    
    # if (is.null(input$file)) {return(NULL)
    # } else 
    if (input$appname == 'Factor Analysis'){

      call.1 = factor_server()
      cat("Calculation Completed",getwd())
      
    } else if (input$appname == 'Joint Space Map'){
      
      call.1 = jsm_server()
      cat("Calculation Completed")
      
    } else if (input$appname == 'Likart Visualization'){

      call.1 = lkrt_server()
      cat("Calculation Completed")
      
    } else if (input$appname == 'OLS Regression'){
      
      call.1 = ols_server()
      cat("Calculation Completed")
      
    } else if (input$appname == 'Segmentation Discriminant and Targeting'){
      
      call.1 = sdt_server()
      cat("Calculation Completed")
      
    } else if (input$appname == 'Text Analysis'){
      
      call.1 = bta_server()
      cat("Calculation Completed")
      
    } else if (input$appname == 'Text Topic Analysis'){
      
      call.1 = tta_server()
      cat("Calculation Completed")
      
    } else if (input$appname == 'Network Analysis'){
      
      call.1 = netwrk_server()
      cat("Calculation Completed")
      
    }
    
  })
  
})