list(

  fileInput("file", "Upload input data (csv file with header))"),  

  htmlOutput("fselect"),
  
  sliderInput("cutoff", "Cut-off for factor loadings(for Plotting only)", min = 0,  max = 1, value = 0.25),
  
  htmlOutput("xaxis"),
  
  htmlOutput("yaxis"),

  sliderInput("cutoffcorr", "Cut-off for Correlation in Factors vs Variable", min = 0,  max = 1, value = 0.25),
  br()
  # submitButton("Update App")
  
  )