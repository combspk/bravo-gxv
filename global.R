library(data.table)
library(DBI)
library(dplyr)
library(DT)
library(plotly)
library(RPostgres)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(stringr)

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

#CHEMS_TOP <- run_query(paste0("SELECT * FROM bravo_top_significant_genes"))
#CHEMS_BOT <- run_query(paste0("SELECT * FROM bravo_bottom_significant_genes"))

#saveRDS(CHEMS_TOP, file="CHEMS_TOP.Rds")
#saveRDS(CHEMS_BOT, file="CHEMS_BOT.Rds")

CHEMS_TOP <- readRDS(file="CHEMS_TOP.Rds")
CHEMS_BOT <- readRDS(file="CHEMS_BOT.Rds")

CHEMS <- rbind(CHEMS_TOP, CHEMS_BOT)
init <- unique(CHEMS[, c("dsstox_substance_id", "preferred_name")])

#PATHWAYS_POS <- run_query(paste0("SELECT * FROM bravo_misgdbpathway_hcon_poszscopval_counts"))
#saveRDS(PATHWAYS_POS, file="PATHWAYS_POS.Rds")
PATHWAYS_POS <- readRDS(file="PATHWAYS_POS.Rds")

#PATHWAYS_NEG <- run_query(paste0("SELECT * FROM bravo_misgdbpathway_hcon_negzscopval_counts"))
#PATHWAYS <- rbind(PATHWAYS_POS[, c("msigdb_pathway_name", "msigdb_number_of_genes")], PATHWAYS_NEG[, c("msigdb_pathway_name", "msigdb_number_of_genes")])
PATHWAYS <- PATHWAYS_POS
init_pathways <- unique(PATHWAYS[, c("msigdb_pathway_name", "msigdb_number_of_genes")])
