list(tabsetPanel(type = "tabs",
            #
            tabPanel("Overview",h4(p("How to use this App")),
                     
                     p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                          To fit topic models in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will fit 
                          the topic models with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                     p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                          ", align = "Justify"),
                     h5("Note"),
                     p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                          are over in back-end results will be refreshed",
                       align = "justify"),
                     verbatimTextOutput("tmp"),br()),
            
            tabPanel("Example dataset", h4(p("Download Sample text file")), 
                     downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                     p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                     img(src = "example1.png")),
            
            tabPanel("Corpus Word Cloud",plotOutput("wordcloud")),
            #                         
            tabPanel("Topic Model - Summary",verbatimTextOutput("summary")),
            tabPanel("Topics Wordcloud",uiOutput("plots2")),
            tabPanel("Topics Co-occurrence",uiOutput("plots3")),
            # tabPanel("Topics eta values",tableOutput("summary2")),
            
            #                         
            tabPanel("Data with Topic Proportions",dataTableOutput("table"))
            #                         
    )
)