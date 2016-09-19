#################################################
#    Marketing research tool box                #
#################################################

library("shiny")

shinyUI(fluidPage(
  #tags$head(includeScript("google_analytics.js")),
  headerPanel('Marketing Research Tool Box'),
  sidebarPanel(
    selectInput('appname','Select Shiny App',c("Factor Analysis",
                                               "Joint Space Map",
                                               "Likart Visualization",
                                               "OLS Regression",
                                               "Segmentation Discriminant and Targeting",
                                               "Text Analysis",
                                               "Text Topic Analysis",
                                               "Network Analysis"),'Factor Analysis'),
    # submitButton("Update App"),
    uiOutput('ui.s')
    # submitButton(text = "Apply Changes", icon("refresh"))
  ),
  mainPanel(
    uiOutput('ui.m'),
    verbatimTextOutput("start")
    )
  )
)