fluidPage(

    titlePanel("Bravo Gene Expression Visualization"),

    sidebarLayout(
        sidebarPanel(
            DTOutput("chem_input") %>% withSpinner(),
            hr(),
            DTOutput("pathway_input") %>% withSpinner()
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Gene Expression",
                    tabsetPanel(
                        tabPanel("Top Genes",
                            br(),
                            fluidRow(
                                column(12,
                                    plotlyOutput("plot_top") %>% withSpinner(),
                                )
                            ),
                            br(),
                            hr(),
                            fluidRow(
                                column(12,
                                    DTOutput("dt_top") %>% withSpinner()
                                )
                            )
                        ),
                        tabPanel("Bottom Genes",
                            br(),
                            fluidRow(
                                column(12,
                                    plotlyOutput("plot_bot") %>% withSpinner(),
                                )
                            ),
                            br(),
                            hr(),
                            fluidRow(
                                column(12,
                                    DTOutput("dt_bot") %>% withSpinner()
                                )
                            )
                        ),
                        tabPanel("Per-chemical Pathways",
                            br(),
                            fluidRow(
                                column(12, #style="overflow-x:scroll",
                                    plotlyOutput("plt_pathways_per_chemical") %>% withSpinner()
                                )
                            ),
                        )
                    )
                ),
                tabPanel("Pathways",
                    br(),
                    fluidRow(
                        column(6, style="overflow-x:scroll",
                            plotlyOutput("plt_pathways") %>% withSpinner()
                        ),
                        column(6, style="overflow-x:scroll",
                            plotlyOutput("plot_pathchem") %>% withSpinner(),
                        )
                    ),
                    br(),
                    hr(),
                    fluidRow(
                        column(12,
                            DTOutput("dt_pathchem") %>% withSpinner()
                        )
                    )
                )
            )
        )
    )
)
