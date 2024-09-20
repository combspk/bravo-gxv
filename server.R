# Load variables from config.yml & assign to global variables
config_vars <- config::get("db")
DB_HOST <- config_vars$host
DB_PORT <- config_vars$port
DB_USER <- config_vars$user
DB_PASS <- config_vars$pass

# Define database driver
pgdrv <- RPostgres::Postgres()

# Define functions
# Run database query on a given database
run_query <- function(query, args=list()){
    # Establish db connection
    psql_conn <-DBI::dbConnect(
        pgdrv, 
        dbname="bravo_v1",
        host=DB_HOST,
        port=DB_PORT, 
        user=DB_USER,
        password=DB_PASS
    )
    df <- data.frame()
    try_query <- tryCatch({ # Attempt to query db
        res <- dbSendQuery(psql_conn, query)
        if(length(args) > 0){
            dbBind(res, args) # Bind arguments to parameterized query, if supplied
        }
        df <- dbFetch(res)
        dbClearResult(res)
    }, error=function(cond){
        print("Error in query")
        print(cond)
    })
    dbDisconnect(psql_conn) # Disconnect from db
    resp <- data.frame()
    if(nrow(df) > 0){
        resp <- df
    }
    return(resp)
}

CHEMS_TOP <- run_query(paste0("SELECT * FROM bravo_top_significant_genes"))
CHEMS_BOT <- run_query(paste0("SELECT * FROM bravo_bottom_significant_genes"))

CHEMS <- rbind(CHEMS_TOP, CHEMS_BOT)
init <- unique(CHEMS[, c("dsstox_substance_id", "preferred_name")])

# Define server logic required to draw a histogram
function(input, output, session) {
    output$chem_input <- renderDT(
        init,
        selection="single",
        rownames=FALSE,
        caption=paste0("Select a chemical from the list:"), class="row-border stripe compact", filter=list(position="top", clear=TRUE), options=list(scrollX=TRUE)
    )
    
    observeEvent(input$chem_input_rows_selected, {
        sel <- init[input$chem_input_rows_selected, ]
        
        chem_name <- sel[sel$preferred_name != "DMSO", "preferred_name"][1]
        
        # TOP
        dat <- CHEMS_TOP[CHEMS_TOP$dsstox_substance_id == sel$dsstox_substance_id, ]
        d <- split(dat, dat$concentration)
        fig <- plot_ly()
        for(x in d){
            fig <- fig %>% add_trace(x, x=x$gene_name, y=x$adjusted_value, color=as.character(x$concentration), type="box", quartilemethod="exclusive", name=x$concentration[1])
        }
        fig <- fig %>% layout(title=paste0(chem_name, " Top Genes"), boxmode="group")
        output$plot_top <- renderPlotly(fig)
        output$dt_top <- renderDT(
            dat,
            extensions="Buttons",
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            filter=list(position="top", clear=TRUE),
            caption=paste0("Top gene expression for ", chem_name),
            options=list(
                scrollX = TRUE,
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                filter = "top",
                buttons = c("copy", "csv", "excel", "pdf"),
                dom = "Blfrtip",
                lengthMenu = c(10, 25, 50, 100)
            ),
            server=FALSE # to download entire table and not just current page
        )
        
        # BOT
        dat2 <- CHEMS_BOT[CHEMS_BOT$dsstox_substance_id == sel$dsstox_substance_id, ]
        d2 <- split(dat2, dat2$concentration)
        fig2 <- plot_ly()
        for(x in d2){
            fig2 <- fig2 %>% add_trace(x, x=x$gene_name, y=x$adjusted_value, color=as.character(x$concentration), type="box", quartilemethod="exclusive", name=x$concentration[1])
        }
        fig2 <- fig2 %>% layout(title=paste0(chem_name, " Bottom Genes"), boxmode="group")
        output$plot_bot <- renderPlotly(fig2)
        output$dt_bot <- renderDT(
            dat2,
            extensions="Buttons",
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            filter=list(position="top", clear=TRUE),
            caption=paste0("Top gene expression for ", chem_name),
            options=list(
                scrollX = TRUE,
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                filter = "top",
                buttons = c("copy", "csv", "excel", "pdf"),
                dom = "Blfrtip",
                lengthMenu = c(10, 25, 50, 100)
            ),
            server=FALSE # to download entire table and not just current page
        )
        
    }, ignoreNULL=TRUE, ignoreInit=TRUE)

}
