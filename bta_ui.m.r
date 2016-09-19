list( 
  tabsetPanel(type = "tabs",
      
      tabPanel("Overview",h4(p("How to use this App")),
                       
      p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
         To do basic Text Analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
         back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),

      p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed", align = "Justify"),

      h5("Note"),
 
      p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations are over in back-end results will be refreshed",
        align = "justify")),
      # verbatimTextOutput("start")),
      
      tabPanel("Example dataset", h4(p("Download Sample text file")), 
      downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
      p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
      img(src = "example1.png")),
              
      tabPanel("TDM & Word Cloud",h4("Term Document Matrix [1:10,1:10"),
                 verbatimTextOutput("dtmsummary"),
                     br(),
                     br(),
                 h4("Word Cloud"),
                   plotOutput("wordcloud",height = 700, width = 700),
                       h4("Weights Distribution of Wordcloud"),
                       verbatimTextOutput("dtmsummary1")),
      tabPanel("Term Co-occurrence",
                       #h4("Co-occurrence (Top 50 Term)"),
                       plotOutput("wordword",height = 700, width = 700)),
              #                         
      tabPanel("Segmentation - Summary",
                       h4("Principal component plot"),
                        plotOutput("pcaplot",height = 600, width = 700),
                       h4("Summary"),
                       verbatimTextOutput("summary")),
            
              
              
      tabPanel("Segmentation - Word cloud",uiOutput("segplots")),
      tabPanel("Segmentation - Co-occurrence",uiOutput("segcoocrplots")),
      tabPanel("Segmentation - Data",dataTableOutput("table0")),
              
      tabPanel("Sentiment Analysis",
                       h4("Sentiment Score across Documents"),
                       plotOutput("flowplot",height = 600, width = 700),
                       h4("Postive Words word cloud"),
                       plotOutput("posplot",height = 600, width = 700),
                       h4("Negative Words word cloud"),
                       plotOutput("negplot",height = 600, width = 700)),
              
      tabPanel("Sentiment Score Data",dataTableOutput("table"))
  )
)
