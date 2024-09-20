# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Bravo Gene Expression Visualization"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            DTOutput("chem_input")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Top Genes",
                    fluidRow(
                        column(12, 
                            plotlyOutput("plot_top"),
                        )
                    ),
                    br(),
                    hr(),
                    fluidRow(
                        column(12, 
                            DTOutput("dt_top")
                        )
                    )
                ),
                tabPanel("Bottom Genes",
                    fluidRow(
                        column(12, 
                            plotlyOutput("plot_bot"),
                        )
                    ),
                    br(),
                    hr(),
                    fluidRow(
                        column(12, 
                            DTOutput("dt_bot")
                        )
                    )
                )
            )
        )
    )
)
