list(    h5(p("Data Input")),
         fileInput("file", "Upload Adjacency Matrix (csv file with header))"),
         fileInput("file1", "Upload Demographics data (csv file with header))"),
         selectInput("mode","Mode of Graph",c("directed", "undirected","max", "min", "upper",
                                              "lower", "plus"),"undirected"),
         htmlOutput("yvarselect"),
         sliderInput("cex", "Data point labels font size", min = 0.1,  max = 3, value = 1,round = FALSE),
         sliderInput("cex2", "Vertex Size", min = 0.1,  max = 20, value = 5,round = FALSE),
         # submitButton("Update App")
         br()
)