list(
     tabsetPanel(type = "tabs",
            tabPanel("Overview",
                    h4(p("How to use this shiny application")),
                     p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                       Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                       and then proceed. Make sure you have top row as variable names and first column as respondent id/name in csv file",align="justify"),
                     p("Once csv file is uploaded successfully, application will fit a factor model with optimal factors from parallel Analysis and various 
                       results will be showed in the above tabs. In left-side bar panel you can change the parameters value and correspondingly new results 
                       will be showed",align="justify"),
                     br(),
                     h4(p("Download Sample Input File")),
                     downloadButton('downloadData', 'Download Example file'),
                     p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                     img(src = "example1.png") #, height = 280, width = 400
                     ),
            
              tabPanel("Summary",
                     (h6(p("Correlation"))),
                     (tableOutput("table22")),
                     (h6(p("Test Summary"))),(textOutput("text1")),(textOutput("text2")),(textOutput("text3")),
                     (h6(p("Factors Loadings Summary"))),
                     (verbatimTextOutput("mat")),
                     
                     (h6(p("Uniqueness"))),
                     (tableOutput("uni")),
                     #                          (textOutput("text4")),
                     plotOutput("plot1",height = 600, width = 850)),
            
            tabPanel("Loadings",tableOutput("loadings")),
            
            tabPanel("Scores",tableOutput("scores")),
            
            tabPanel("Factor vs Variables",plotOutput("plot20",height = 600, width = 850)),
            
            tabPanel("Factor vs Variables 2",plotOutput("plot2",height = 600, width = 850)),
            
            tabPanel("Factor vs Users",plotOutput("plot3",height = 600, width = 850)),
            
            tabPanel("Data",tableOutput("table")) 
        ) # end of tabset panel
     ) # end of list
