list(    h5(p("Data Input")),
         fileInput("file", "Upload input data (csv file with header))"),
         
         h5(p("Data Selection")),
         htmlOutput("yvarselect"),
         htmlOutput("xvarselect"),
         htmlOutput("fxvarselect"),
         br()
         # submitButton("Update App")
)