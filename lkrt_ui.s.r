list(    # Upload data:
         h5(p("Data Input")),
         fileInput("file", "Upload input data (csv file with header))"),
         
         h5(p("Data Selection (Optional)")),
         htmlOutput("varselect"),
         # submitButton("Update App"),
         br()
)