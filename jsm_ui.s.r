list(    # Upload data:
  h5(p("Perceptual Data Input")),
  fileInput("file", "Upload Perceptual data (tab delimited txt file)"),
  # upload secon data
  h5(p("Preference Data Input")),
  fileInput("file1", "Upload Preference Data (tab delimited txt file)"),
  # Variable selection:
  h5(p("Data Selection (Optional)")),
  h6(p("A -  Perceptual (Attributes)")),
  htmlOutput("varselect"),
  
  h6(p("B -  Perceptual (Firms - only for Spider Chart)")),
  htmlOutput("varselect2"),
  
  h6(p("C -  Preference")),
  # upload secon data
  htmlOutput("varselect1"),
  # submitButton("Update App"),
  br()
)