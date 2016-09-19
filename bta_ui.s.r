list( fileInput("file", "Upload text file"),
    
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    selectInput("ws", "Weighing Scheme", 
                c("tf","tf-idf"), selected = "tf"),
    
    sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 1,  max = 50, value = 4),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 50),  
    
    numericInput("seg", "Number of Segments", 4),
    
    numericInput("nodes", "Number of Central Nodes in co-occurrence graph", 4),
    
    numericInput("connection", "Number of Max Connection with Central Node", 5)
    # submitButton("Update App")
#    submitButton(text = "Apply Changes", icon("refresh"))
    )
