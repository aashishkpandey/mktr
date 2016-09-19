list(    tabsetPanel(type = "tabs",
                     tabPanel("Overview",
                              h4(p("Segmentation")), 
                              p("Market segmentation is a marketing strategy which involves dividing a broad target market into subsets of consumers, businesses, or countries who have, or are perceived to have, common needs, interests, and priorities, and then designing and implementing strategies to target them. Market segmentation strategies are generally used to identify and further define the target customers, and provide supporting data for marketing plan elements such as positioning to achieve certain marketing plan objectives. Businesses may develop product differentiation strategies, or an undifferentiated approach, involving specific products or product lines depending on the specific demand and attributes of the target segment.",
                                align="justify"),
                              a(href="https://en.wikipedia.org/wiki/Market_segmentation","- Wikipedia"),
                              br(),
                              h4(p("Discriminant analysis")), 
                              p("Linear discriminant analysis (LDA) is a generalization of Fisher's linear discriminant, a method used in statistics, pattern recognition and machine learning to find a linear combination of features that characterizes or separates two or more classes of objects or events. The resulting combination may be used as a linear classifier, or, more commonly, for dimensionality reduction before later classification.",
                                align="justify"),
                              p("LDA is closely related to analysis of variance (ANOVA) and regression analysis, which also attempt to express one dependent variable as a linear combination of other features or measurements. However, ANOVA uses categorical independent variables and a continuous dependent variable, whereas discriminant analysis has continuous independent variables and a categorical dependent variable (i.e. the class label). Logistic regression and probit regression are more similar to LDA than ANOVA is, as they also explain a categorical variable by the values of continuous independent variables. These other methods are preferable in applications where it is not reasonable to assume that the independent variables are normally distributed, which is a fundamental assumption of the LDA method.",
                                align="justify"),
                              a(href="https://en.wikipedia.org/wiki/Linear_discriminant_analysis","- Wikipedia"),
                              br(),
                              h4(p("Statistical classification")), 
                              p("In machine learning and statistics, classification is the problem of identifying to which of a set of categories (sub-populations) a new observation belongs, on the basis of a training set of data containing observations (or instances) whose category membership is known. An example would be assigning a given email into \"spam\" or \"non-spam\" classes or assigning a diagnosis to a given patient as described by observed characteristics of the patient (gender, blood pressure, presence or absence of certain symptoms, etc.). Classification is an example of pattern recognition.",
                                align="justify"),
                              p("In the terminology of machine learning, classification is considered an instance of supervised learning, i.e. learning where a training set of correctly identified observations is available. The corresponding unsupervised procedure is known as clustering, and involves grouping data into categories based on some measure of inherent similarity or distance.",
                                align="justify"),
                              a(href="https://en.wikipedia.org/wiki/Statistical_classification","- Wikipedia")),
                     
                     tabPanel("Data Input Format",
                              h4(p("Data input")),
                              p("This shiny application requires three data inputs from the user. To do so, click on the Browse (in left side-bar panel) and upload the Segmentation data input file. Similarly upload Discriminant and Classification data sets.
                                Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                                and then proceed. Make sure you have top row as variable names and first column as respondent id/name in csv file"
                                ,align="justify"),
                              img(src = "Segmentation.png", height = 180, width = 400),p('Segmentation Sample file'),
                              img(src = "Discriminant.png", height = 180, width = 400),p('Discriminant Sample file'),
                              img(src = "Classification.png", height = 180, width = 400),p('Classification Sample file'),
                              br(),
                              
                              p("Once csv file is uploaded successfully, by-default application will perform K-means segmentation with 3 segments. In left-side bar panel you can change the segmentation algorithm and number of segments. Click on Apply changes after making any change in the inputs. Accordingly results will be updates in all the tabs",
                                align="justify"),
                              br()),
                     tabPanel("Example dataset", h4(p("Download Sample files")), br(),
                              downloadButton('downloadData1', 'Download Segmentation Sample file'), br(),br(),
                              downloadButton('downloadData2', 'Download Discriminant Sample file'), br(),br(),
                              downloadButton('downloadData3', 'Download Classification Sample file'), br(),br(),
                              
                              p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -",
                                align="justify"),
                              img(src = "example1.png")),
                     #tabPanel("Data",h3(textOutput("caption"),tableOutput("table"))),
                     
                     tabPanel("Summary - Segmentation",h3(textOutput("caption1")), h4(div(textOutput("caption2"),style = "color:Red")),
                              plotOutput("plotpca",height = 400, width = 500),verbatimTextOutput("summary")),
                     
                     tabPanel("Summary - Discriminant", verbatimTextOutput("discriminat")),
                     tabPanel("Summary - Targeting", verbatimTextOutput("targeting")),
                     
                     # discriminat
                     
                     tabPanel("Plot",h3("Segments Plot"), plotOutput("plot",height = 700, width = 840)),
                     tabPanel("Data Segment",dataTableOutput("table"),tags$head(tags$style("tfoot {display: table-header-group;}"))), 
                     tabPanel("Data Target",dataTableOutput("table1")) 
      )
)