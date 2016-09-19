list(
fileInput("file", "Upload Segmentation data (csv file with header)"),
fileInput("file1", "Upload Discriminant data (csv file with header)"),
fileInput("file2", "Upload Classification data (csv file with header)"),

selectInput("select", "Choose Segmentation Algo", 
            c("K-Means","Hierarchical","Model Based"), selected = "K-Means"),

numericInput("Clust", "Number of Segments:", 3),
# submitButton("Update App")
br()
#submitButton(text = "Apply Changes", icon("refresh"))
)