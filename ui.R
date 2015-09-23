
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Libraries
source('uilibs.R')


shinyUI(
  navbarPage(
    title = "KRESS",
    tabPanel(
      "Home",
      sidebarLayout(
        sidebarPanel(
          paste("KRESS Suitability Modeler is a program that employs a weighted sum multi-criteria decision analysis to genearte predictive models. ")
        ),
        mainPanel(
          helpText("Variables can be uploaded in the Variables tab and have weights applied based on their importance. From variables can be applied against a list of locations to create model. ")
        )
      )
    ),
    tabPanel(
      "Variables",
      fluidPage(
        h4("Variables"),
        hr(),
        column(
          4,
          fileInput(
            "AddVariables",
            "Add Variables",
            multiple = TRUE
          ),
          br(),
          htmlOutput("VariableDisplay"),
          hr(),
          htmlOutput("listNames")
        ),
        column(
          1,
          htmlOutput("weightEdit"),
          htmlOutput("weightButton"),
          hr()
          #htmlOutput("Submit1")
          #htmlOutput("Test1")
        ),
        column(
          6,
          plotOutput("ShowSelected")
        )
      )
    ),
    navbarMenu(
      "Raster Tools",
#       tabPanel(
#         "Map Algebra",
#         fluidPage(
#           h4("Map Algebra"),
#           hr(),
#           column(
#             3,
#             title = "Left Side Algebra",
#             htmlOutput("LeftSide"),
#             br(),
#             br()
#           ),
#           column(
#             1,
#             title = "Algebra Functions",
#             br(),
#             radioButtons(
#               inputId = "MathOptions",
#               label = NULL,
#               choices = list(
#                 "+" = 1,
#                 "-" = 2,
#                 "x" = 3,
#                 "÷" = 4,
#                 "^" = 5
#               )
#             ),
#             actionButton(
#               inputId = "ProcessAlgebra",
#               label = "Process"
#             )
#           ),
#           column(
#             3,
#             title = "Right Side Algebra",
#             htmlOutput("RightSide"),
#             br(),
#             br()
#           )
#         ),
#         hr(),
#         column(
#           3,
#           htmlOutput("NewFileNameUI"),
#           htmlOutput("DownloadButton"),
#           br(),
#           br()
#         ),
#         column(
#           6,
#           selectInput(
#             "downloadFormat",
#             "Select format",
#             choices = c(
#               'raster (.grd)' = '.grd',
#               'ascii (.asc)' = '.asc',
#               'SAGA (.sdat)' = '.sdat',
#               'IDRISI (.rst)' = '.rst',
#               'CDF (.nc)' = '.nc',
#               'GTiff (.tif)' = '.tif',
#               'ENVI (.envi)' = '.envi',
#               'EHdr (.bil)' = '.bil',
#               'HFA (.img)' = '.img'
#             ),
#             selected = '.asc',
#             width = '25%'
#           )
#         )
#       ),
      tabPanel(
        "Weighted Sum Model",
        h4("Weighted Sum Model"),
        hr(),
        column(
          3,
          htmlOutput("WSMVariables"),
          checkboxInput(
            "maskCheckbox",
            "Apply mask"
          )
        ),
        column(
          4,
          fileInput(
            "WSMData",
            "Add Location Data",
            multiple = FALSE
          ),
          htmlOutput("XCoord"),
          htmlOutput("YCoord"),
          HTML(paste("Remove outliers:")),
          br(),
          column(
            3,
            numericInput(
              "lOutlier",
              "Lower %",
              value = 15,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          column(
            3,
            numericInput(
              "uOutlier",
              "Upper %",
              value = 85,
              min = 0,
              max = 100,
              step = 1
            )
          )
        ),
        column(
          5,
          actionButton(
            "WSMgenerate",
            "Generate"
          ),
          plotOutput("testplot123")
        ),
        column(
          12,
          htmlOutput("WSMNewFileName"),
#           selectInput(
#             "WSMdownloadFormat",
#             "Select format",
#             choices = c(
#               'raster (.grd)' = 'raster',
#               'ascii (.asc)' = 'ascii',
#               'SAGA (.sdat)' = 'SAGA',
#               'IDRISI (.rst)' = 'IDRISI',
#               'CDF (.nc)' = 'CDF',
#               'GTiff (.tif)' = 'GTiff',
#               'ENVI (.envi)' = 'ENVI',
#               'EHdr (.bil)' = 'EHdr',
#               'HFA (.img)' = 'HFA'
#             ),
#             selected = '.asc',
#             width = '10%'
#           ),
          htmlOutput("WSMDownloadButton")
        )
      )
    ),
    tabPanel(
      "Map Statistics",
      h4("Map Statistics"),
      hr(),
      busyIndicator(text = "Creating Histogram", wait = 1500),
      column(6,
             plotOutput("bar.plot")
      ),
      column(6,
             plotOutput("stat.heat.map")
      ),
      hr(),
      column(
        3,
        htmlOutput("VariableDisplay2")
      ),
      column(
        3,
        h5("Map Parameters:"),
        htmlOutput("mapParam"),
        offset = 1
      ),
      column(
        3,
        h5("Map Statistics"),
        htmlOutput("mapStats")
      )
    )
  )
)
