list(
  tabsetPanel(type = "tabs",
            #
            
            tabPanel("Overview",
                     # h5(p("Factor Analysis")), 
                     # p("Factor analysis is a statistical method used to describe variability among observed, correlated variables in terms of a potentially lower 
                     # number of unobserved variables called factors. For example, it is possible that variations in four observed variables mainly reflect the variations 
                     # in two unobserved variables. Factor analysis searches for such joint variations in response to unobserved latent variables. The observed variables 
                     # are modelled as linear combinations of the potential factors, plus error terms. The information gained about the interdependencies between 
                     # observed variables can be used later to reduce the set of variables in a dataset. Computationally this technique is equivalent to low rank 
                     # approximation of the matrix of observed variables. Factor analysis originated in psychometrics, and is used in behavioral sciences, social sciences,
                     # marketing, product management, operations research, and other applied sciences that deal with large quantities of data.",align="justify"),
                     # a(href="http://en.wikipedia.org/wiki/Factor_analysis","- Wikipedia"),
                     h4(p("How to use this shiny application")),
                     p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                       Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                       and then proceed. Make sure you have top row as variable names and first column as respondent id/name in csv file",align="justify"),
                     p("Once csv file is uploaded, all the variables in will appear in left side-bar panel. If you want to drop any variable from your plot just uncheck that variable and that variable will be dropped from plot",align="justify"),
                     br(),
                     h4(p("Download Sample Input File")),
                     downloadButton('downloadData', 'Download Example file'),br(),br(),
                     p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then 
                       download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -", align="justify"),
                     img(src = "example1.png") #, height = 280, width = 400
                     
                     ),
            
            tabPanel("Likert Plot",plotOutput("plot", width = "100%",height = 700)),
            #                 tabPanel("JSM Plot",plotOutput("plot", height = 800, width = 840)),
            tabPanel("Data",tableOutput("table"))
            #     tableOutput("table"),  
            #     tableOutput("table1"),
            #     plotOutput("plot", width = "100%"),
            #     plotOutput("plot1")
            )
)