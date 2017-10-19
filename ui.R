# UI ----
# forecast_Q, V September 2017
# tobias siegfried, hydrosolutions Ltd., 2017

shinyUI(
  navbarPage(
    theme = shinytheme("cosmo"),
    "FORECAST!",                                                            # TITLE
    tabPanel("Stations Overview", icon = icon("eye"),                       # STATIONS OVERVIEW PANEL ----
             fluidRow(
               column(3,h3("Overview of Available Stations"),
                      helpText("Available stations are shown on the map. Gauging stations are shown in blue color, meteorological
                               stations are indicated in red color. Stations can be selected from the pull down menu or
                               through clicking on the map. The 'View Data' Button displays timeseries graphs as well as corresponding
                               norms and available data from the last two years."),
                      absolutePanel(top=200, left=30,
                                    checkboxInput("show_stations", "Show Stations", TRUE),
                                    conditionalPanel("input.show_stations == true",
                                                     selectInput("location", "Station", c(" ",locs$loc), selected="",width='100%'),
                                                     conditionalPanel("input.location !== null && input.location !== ''",
                                                                      br(),
                                                                      fluidRow(
                                                                        h3("Station information"),
                                                                        textOutput("rbinfo"),
                                                                        textOutput("stationName"),
                                                                        textOutput("stationCode"),
                                                                        textOutput("stationType"),
                                                                        textOutput("locText"),
                                                                        br(),
                                                                        actionButton("button_plot_and_table", "View Data", class="btn-block",width='100%')
                                                                      )
                                                     )
                                    ),
                                    bsModal("Plot_and_table", "Station Data", "button_plot_and_table", size = "large",
                                            fluidRow(
                                              radioButtons("pTypeData", "Choose plot type:",
                                                           list("Timeseries", "Norms")),
                                              conditionalPanel(
                                                condition = "input.pTypeData == 'Timeseries'", dygraphOutput("timeseriesGraphs")
                                              ),
                                              conditionalPanel(
                                                condition = "input.pTypeData == 'Norms'", plotOutput("NormsGraphs")
                                              )
                                            )
                                    )
                      )
               ),
               column(9,leafletOutput("Map", width="100%", height="1000px")
               )
             )
    ),

    tabPanel("Data",icon = icon("database"),                                # DATA PANEL -----
             fluidRow(
               column(3,h3("Station Data"),
                      helpText("From the pull down menu below, select station for viewing and editing data. You have to
                               submit the new data by clicking the corresponding Submit Data button below. The data will be stored remotely
                               and remain available for later sessions."),
                      # helpText("If you want to ")
                      selectInput("location_edit", "Select Station", c("", locs$loc), selected="Balykty_1",width='100%')
               ),
               column(9,h3("Enter/Edit New Data"),
                      hr(),
                      h4(textOutput("stationNameEdit")),
                      hr(),
                      fluidRow(
                        column(3,textOutput("dataTableEdit1TypeText"),
                               hr(),
                               rHandsontableOutput("editTable1"),
                               hr(),
                               actionButton("submitData","Submit New Data")
                               ),
                        column(9,
                               "Plot",
                               hr(),
                               plotOutput("plotEditData")
                               )
                      )
               )
             )
    ),

    tabPanel("Models", icon = icon("cog"),                                  # FORECAST PANEL ----
             fluidRow(
               column(3, h3("Information on Models"),
                      helpText("Summary information of models and their quality is provided for each station.
                               The reported RMSE is from the ensemble model and is assessed on a test set that is unseen during training.
                               The ensemble model is a simple 'linear blend' of the individual models trained.
                               The performance of the individual models is shown in the corresponding tables
                               (see also documentation on caretEnsemble for more information)."),
                      tags$div(class="header", checked=NA,
                               tags$a(href="https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html", "caretEnsemble Documentation")
                      ),
                      helpText("A delay coordinate embedding approach is utlizied.
                               For all models, lagged decadal (10-day) data from the closest meteorological station is utilized
                               together with the past lagged values from the corresponding discharge time series.
                               A lag five has been deemed optimal for performance. Seasonality is accounted for by adding
                               a decade identifier to the predictor list."),
                      helpText("For each station, model training is carried out with data upt to and including 31-12-1999.
                               The test set data includes data from 1-1-2000 onwards.")
               ),
               column(9,
                      tabsetPanel(
                        tabPanel("Decadal (10 days) Forecast",h3("Decadal Forecasts"),
                                 helpText(" "),
                                 h4("Nura"),
                                 textOutput("model_16279_dec"),
                                 tableOutput("model_16279_decEnsemble")
                                 #helpText(" "),
                                 #h4("520800: Qilian"),
                                 #textOutput("model_16290_dec"),
                                 #tableOutput("model_16290_decEnsemble"),
                                 #helpText(" "),
                                 #h4("520400: Zhamashike"),
                                 #textOutput("model_16294_dec"),
                                 #tableOutput("model_16294_decEnsemble")
                        ),
                        tabPanel("Monthly Forecasts",h3("Monthly Forecasts"),
                                 h4("Nura"),
                                 textOutput("model_16279_mon"),
                                 tableOutput("model_16279_monEnsemble")
                                 #h4("520800: Qilian"),
                                 #textOutput("model_16290_mon"),
                                 #tableOutput("model_16290_monEnsemble"),
                                 #h4("520400: Zhamashike"),
                                 #textOutput("model_16294_mon"),
                                 #tableOutput("model_16294_monEnsemble")
                        ),
                        tabPanel("Seasonal Forecasts (under dev)",h3("Seasonal Forecasts - Stay tuned"),
                                 h4("Nura")
                                 #h4("520800: Qilian"),
                                 #h4("520400: Zhamashike")
                        )
                      )
               )
             )
    ),

    tabPanel("Forecast", icon = icon("signal"),                             # FORECAST PANEL ----
             fluidRow(
               column(3, h3("Forecast Information"),
                      helpText("Decadal, monthly and seasonal forecasts are computed for each decade and shown for all gauging stations in the corresponding
                               tabs. The latest available data is fetched and forecasts are carried out while using the latest
                               decade for which complete data is available. The forecast values are highlighted in orange color."),
                      helpText("Important note: Please make sure that the latest data (see Data page) is always available (discharge and precipitation and temperature
                               from all stations) for a valid forecast (labelled [Valid]). Otherwise, forecast target decades are in the past and forecasted values are outdated (labelled [Outdated])."),
                      h3("Current Date and Decade"),
                      h4(textOutput("currentDate")),
                      h4(textOutput("decadeText"))
               ),
               column(9,
                      tabsetPanel(
                        tabPanel("Decadal (10 days) Forecast",h3("Decadal Forecasts"),
                                 h4("Nura"),
                                 verbatimTextOutput("fc521400dec"),
                                 plotOutput("obsFcComp521400dec")
                                 #h4("520800: Qilian"),
                                 #verbatimTextOutput("fc520800dec"),
                                 #plotOutput("obsFcComp520800dec"),
                                 #h4("520400: Zhamashike"),
                                 #verbatimTextOutput("fc520400dec"),
                                 #plotOutput("obsFcComp520400dec")

                        ),
                        tabPanel("Monthly Forecasts",h3("Monthly Forecasts"),
                                 h4("Nura"),
                                 verbatimTextOutput("fc521400mon"),
                                 plotOutput("obsFcComp521400mon")
                                 #h4("520800: Qilian"),
                                 #verbatimTextOutput("fc520800mon"),
                                 #plotOutput("obsFcComp520800mon"),
                                 #h4("520400: Zhamashike"),
                                 #verbatimTextOutput("fc520400mon"),
                                 #plotOutput("obsFcComp520400mon")
                        ),
                        tabPanel("Seasonal Forecasts (under dev)",h3("Seasonal Forecasts - Stay Tuned"),
                                 h4("Nura")
                                 #h4("520800: Qilian"),
                                 #h4("520400: Zhamashike")
                        )
                      )
               )
             )
    ),

    tabPanel("Assessment of Forecast Quality", icon = icon("check-circle"), # ASSESSMENT OF FORECAST QUALITY PANEL ----
             fluidRow(
               column(3, h3("Assessment of Forecast Quality"),
                      helpText("Assessment of quality of forecasts based on past model test performance up and including
                               the last known values. The test set includes
                               data from January 2000 until and including the last known data. the quality criteria are shown as red and blue
                               lines respectively where, for each decade, the red line is 0.674 * rmse(delta Q)/std(Q) in the case of the decadal
                               forecasts and 0.674 * rmse(Q)/std(Q) for monthly forecasts. The blue line, i.e. a less stringent quality criteria is
                               at 0.8 * rmse(delta Q)/std(Q) and 0.8 * rmse(Q)/std(Q) respectively. Forecast quality of all stations is shown.")
               ),
               column(9,
                      tabsetPanel(
                        tabPanel("Decadal (10 days) Forecast Quality",h3("Assessment of Decadal Forecast Quality"),
                                 h4("Nura"),
                                 plotOutput("forecastQual_16279_dec")
                                 #h4("520800: Qilian"),
                                 #plotOutput("forecastQual_16290_dec"),
                                 #h4("520400: Zhamashike"),
                                 #plotOutput("forecastQual_16294_dec")
                        ),
                        tabPanel("Monthly Forecast Quality",h3("Assessment of Monthly Forecast Quality"),
                                 h4("Nura"),
                                 plotOutput("forecastQual_16279_mon")
                                 #h4("520800: Qilian"),
                                 #plotOutput("forecastQual_16290_mon"),
                                 #h4("520400: Zhamashike"),
                                 #plotOutput("forecastQual_16294_mon")
                        ),
                        tabPanel("Seasonal Forecast Quality (under dev)",h3("Assessment of Seasonal Forecast Quality - Stay tuned"),
                                 h4("Nura")
                                 #h4("520800: Qilian"),
                                 #h4("520400: Zhamashike")
                        )
                      )
               )
             )
    ),
    tabPanel("Help", icon = icon("question-circle"),                        # HELP PANEL ----
             fluidRow(
               column(3, h3("Help"),
                      helpText("Need help. Please contact us!")
               ),
               column(9, uiOutput("feedbackform")
               )
             )
    )
  )
)
