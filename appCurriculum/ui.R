#========================================================================================*
# ---- SET-UP ----
#========================================================================================*

library(shiny)
library(shinythemes)
library(tidyr)
library(markdown)
library(dplyr)
library(readr)
library(DT)
library(stringr)
library(leaflet)
library(sp)

# No strings as factors!

options(stringsAsFactors = FALSE)

select <- dplyr::select
filter <- dplyr::filter

# Get vector of species names:

choiceSpecies <- c('Show all',
                   'Black-crowned night heron',
                   'Brown pelican',
                   'Black-bellied plover',
                   'Long-billed curlew',
                   'Pacific loon',
                   "Swainson's hawk")

# SMBC logo to be displayed at the top of every page:

imgSMBC <- function(){
  HTML(
    '<a target="_blank" href="https://nationalzoo.si.edu/migratory-birds">
      <img src="nzpLogoSideways2.png"
          align="left"
          width="80%"
          alt = "Migratory bird center logo and link">
    </a>
    <br>'
  )
}

# This function makes a fluid row with white space on either side:

centeredRow <- function(content){
  fluidRow(
    column(1, ''),
    column(10, content),
    column(1, '')
  )
}

#========================================================================================*
# ---- UI ----
#========================================================================================*

shinyUI(
  navbarPage(
    title = 'Follow that bird!',
    theme = shinytheme('yeti'),
    #--------------------------------------------------------------------------------------*
    # --- HOME PAGE ----
    #--------------------------------------------------------------------------------------*
    tabPanel("Home", 
             includeCSS("additionalStyles.css"), 
             fluidRow(
               column(5, imgSMBC()),
               column(7, '')
             ),
             fluidRow(HTML('<br>')),
             fluidRow(
               img(
                 src='header2.png',
                 align = "center",
                 width = "100%")
             ),
             fixedPage(
               br(),
               fluidRow(
                 HTML(read_file('homePage.txt'))
               ),
               fluidRow(HTML(read_file('imgFooter.txt')))
             )
    ),
    #--------------------------------------------------------------------------------------*
    # --- THE FULL ANNUAL CYCLE  ----
    #--------------------------------------------------------------------------------------*
    tabPanel("The full annual cycle",
             fluidRow(
               column(5, imgSMBC()),
               column(7, '')
             ),
             fixedPage(
               fluidRow(
                 HTML(read_file('fullAnnualCycle.txt'))
               ),
               fluidRow(HTML(read_file('imgFooter.txt')))
             )
    ),
    #--------------------------------------------------------------------------------------*
    # --- BIRD PROFILES  ----
    #--------------------------------------------------------------------------------------*
    navbarMenu('Bird profiles',
               tabPanel("Black-bellied plover",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('plover.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel("Black-crowned night heron",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('blackCrownedNightHeron.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel("Brown pelican",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('brownPelican.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel("Kirtland's warbler",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('kiwa.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel("Long-billed curlew",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('longBilledCurlew.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel("Pacific loon",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('pacificLoon.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel("Swainson's hawk",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          fluidRow(
                            HTML(read_file('swainson.txt'))
                          ),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               )
    ),
    #--------------------------------------------------------------------------------------*
    # --- TRACKING DEVICES ----
    #--------------------------------------------------------------------------------------*
    navbarMenu("Tracking devices",
               tabPanel('Technology table',
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        centeredRow(HTML(read_file('tagIntro.txt'))),
                        hr(),
                        br(),
                        fluidRow(DT::dataTableOutput('tagTable')),
                        fixedPage(HTML(read_file('imgFooter.txt')))
               ),
               tabPanel('Bird banding',
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          HTML(read_file('birdBanding.txt')),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel('Light-level geolocators',
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          HTML(read_file('llGeolocatorTag.txt')),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel('Radio telemetry',
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          HTML(read_file('radioTag.txt')),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel('Satellite telemetry',
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          HTML(read_file('satelliteTag.txt')),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               ),
               tabPanel('PIT tags',
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        fixedPage(
                          HTML(read_file('pitTag.txt')),
                          fluidRow(HTML(read_file('imgFooter.txt')))
                        )
               )
    ),
    #--------------------------------------------------------------------------------------*
    # --- TRACKING DATA ----
    #--------------------------------------------------------------------------------------*
    navbarMenu("Tracking data",
               tabPanel("Tracking table",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        sidebarLayout(
                          sidebarPanel(
                            fluidRow(
                              column(11, selectInput('sppTable', 'Bird species:',
                                                     selectize = FALSE,
                                                     choices = choiceSpecies)),
                              column(1, '')),
                            fluidRow(
                              column(11, uiOutput('uiBirdIDTable')),
                              column(1, '')),
                            fluidRow(
                              column(11, uiOutput('uiDateRangeTable')),
                              column(1, '')),
                            fluidRow(uiOutput('birdPicTable')),
                            width = 3, position = 'left'),
                          mainPanel(
                            fluidRow(
                              centeredRow(DT::dataTableOutput('tableQuery'))
                            ),
                            fluidRow(HTML(read_file('imgFooter.txt'))),
                            width = 9, position = 'right')
                        )),
               tabPanel("Tracking map",
                        fluidRow(
                          column(5, imgSMBC()),
                          column(7, '')
                        ),
                        br(),
                        fluidRow(
                          sidebarLayout(
                            sidebarPanel(
                              tags$style(type="text/css",
                                         ".selectize-input { font-size: 24px; line-height: 28px;} "),
                              h3('Mapping bird tracks'),
                              br(),
                              fluidRow(
                                # column(1, ''),
                                column(11, 'To view tracking data, select the bird species and identity of bird you would like to look at and then click the "Map it!" button'),
                                column(1, '')
                              ),
                              hr(),
                              fluidRow(
                                column(11, selectInput('sppMap', 'Bird species:',
                                                       selectize = FALSE,
                                                       choices = choiceSpecies)),
                                column(1, '')),
                              fluidRow(
                                column(11, uiOutput('uiBirdIDMap')),
                                column(1, '')),
                              fluidRow(
                                column(11, uiOutput('uiDateRangeMap')),
                                column(1, '')),
                              fluidRow(
                                column(11, actionButton('mapIt', 'Map it!'))
                              ),
                              fluidRow(uiOutput('birdPicMap')),
                              width = 4, position = 'left'),
                            mainPanel(
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              fluidRow(
                                centeredRow(leafletOutput("mapOut", width = "90%", height=580))
                              ),
                              fluidRow(HTML(read_file('imgFooter.txt'))),
                              width = 8, position = 'right')
                          )
                        )
               )
    )
  )
)