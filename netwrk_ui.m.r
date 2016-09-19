list(
  tabsetPanel(type = "tabs",
              #
              tabPanel("Network Plot",plotOutput("graph1", height = 800, width = 840)),
              tabPanel("Network Centralities",dataTableOutput("centdata"))
  ) 
)