list(tabsetPanel(type = "tabs",
            #
            
            tabPanel("Overview",
                     h4(p("How to use this shiny application")),
                     p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                       Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                       and then proceed. Make sure you have top row as variable names.",align="justify"),
                     p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                       dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                       If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                       If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                       by selecting that variable in the last list of variables
                       ",align="justify"),
                     br(),
                     h4(p("Download Sample Input File")),
                     br(),
                     downloadButton('downloadData', 'Download Example file'),
                     br(),
                     br(),
                     p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                     img(src = "example1.png") #, height = 280, width = 400
                     ),
            
            tabPanel("Summary Stats", verbatimTextOutput("summary")),
            tabPanel("Correlation", verbatimTextOutput("correlation"),plotOutput("heatmap")),
            tabPanel("Summary OLS", h5("Summary OLS Model"),verbatimTextOutput("olssummary"),
                     h5("Summary OLS standardized model"),
                     verbatimTextOutput("olssummarystd")),
            tabPanel("Residuals Plot",h4("Fitted Values vs Residuals"),
                     plotOutput("resplot2"),h4("Fitted Values vs Y"),
                     plotOutput("resplot3"),h4("Residuals plot"),
                     plotOutput("resplot1")),
            tabPanel("Data with predicted Y",tableOutput("datatable"))
            )
)