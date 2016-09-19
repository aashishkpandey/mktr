list(    fileInput("file", "Upload text file"),
         
         textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
         
         sliderInput("freq", "Minimum Frequency in Wordcloud:", min = 1,  max = 50, value = 4),
         
         sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 300,  value = 80),  
         
         numericInput("tdmfreq", "Minimum frequency of terms for Topic Model:", 2),
         
         h6(div(textOutput("caption1"),style = "color:Blue")),
         
         h6(div(textOutput("caption2"))),
         
         numericInput("topic", "Number of Topics to fit:", 2),
         
         numericInput("nodes", "Number of Central Nodes in co-occurrence graph", 4),
         numericInput("connection", "Number of Max Connection with Central Node", 5)
         # submitButton("Update App")
         # submitButton(text = "Apply Changes", icon("refresh"))
         )