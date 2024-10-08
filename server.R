# Define server logic required to draw a histogram
function(input, output, session) {


    # Init plots
    plot_empty_chem_genes <- plotly_empty(type="scatter", mode="markers") %>%
        config(displayModeBar=FALSE) %>%
        layout(
            title=list(
                text="Select a chemical from the list on the left to view associated genes.",
                yref="paper",
                y=0.5
            )
        )
    plot_empty_chem_pathways <- plotly_empty(type="scatter", mode="markers") %>%
        config(displayModeBar=FALSE) %>%
        layout(
            title=list(
                text="Select a chemical from the list on the left to view associated pathways.",
                yref="paper",
                y=0.5
            )
        )
    output$plot_top <- renderPlotly(plot_empty_chem_genes)
    output$plot_bot <- renderPlotly(plot_empty_chem_genes)
    output$plt_pathways_per_chemical <- renderPlotly(plot_empty_chem_pathways)

    plot_empty_pathways <- plotly_empty(type="scatter", mode="markers") %>%
        config(displayModeBar=FALSE) %>%
        layout(
            title=list(
                text="Select a chemical and pathway from the lists<br>on the left to view gene expression statistics<br>for the given chemical and pathway.",
                yref="paper",
                y=0.5
            )
        )
    output$plt_pathways <- renderPlotly(plot_empty_pathways)

    plot_empty_pathway_genes <- plotly_empty(type="scatter", mode="markers") %>%
        config(displayModeBar=FALSE) %>%
        layout(
            title=list(
                text="Select a region in the bar graph to the left<br>to view gene expression fold change data<br>for the given chemical and pathway.",
                yref="paper",
                y=0.5
            )
        )
    output$plot_pathchem <- renderPlotly(plot_empty_pathway_genes)

    output$dt_top <- renderDT(data.frame(matrix()))
    output$dt_bot <- renderDT(data.frame(matrix()))
    output$dt_pathchem <- renderDT(data.frame(matrix()))

    output$chem_input <- renderDT(
        init,
        selection="single",
        rownames=FALSE,
        caption=paste0("Select a chemical from the list:"), class="row-border stripe compact", filter=list(position="top", clear=TRUE), options=list(scrollX=TRUE)
    )

    output$pathway_input <- renderDT(
        init_pathways,
        selection="single",
        rownames=FALSE,
        caption=paste0("Select a pathway from the list:"), class="row-border stripe compact", filter=list(position="top", clear=TRUE), options=list(scrollX=TRUE)
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

        # Load pathway up/down-regulated data
        pathway_up_query = paste0("
            select
                dsstox_substance_id,
                preferred_name,
                msigdb_pathway_name,
                percent_present,
                percent_upregulated_total
            from
                bravo_misgdbpathway_hcon_poszscopval_counts
            where
                preferred_name = $1
        ")
        pathways_up <- run_query(pathway_up_query, args=list(chem_name))

        # pathway_down_query = paste0("
        #     select
        #         dsstox_substance_id,
        #         preferred_name,
        #         msigdb_pathway_name,
        #         percent_present,
        #         percent_downregulated_total
        #     from
        #         bravo_misgdbpathway_hcon_negzscopval_counts
        #     where
        #         preferred_name = $1
        # ")
        # pathways_down <- run_query(pathway_down_query, args=list(chem_name))

        pathways <- pathways_up
        pathways <- pathways[order(pathways$msigdb_pathway_name), ]

        plt <- plot_ly(pathways, x=~msigdb_pathway_name, y=~percent_upregulated_total, name="percent_upregulated_total", type="bar", height=1500, width=1500)
        plt <- plt %>% add_trace(y=~percent_present, name="percent_present", type="bar")
        plt <- plt %>% layout(title=paste0(chem_name, " Pathways"), xaxis=list(title="Pathway"), yaxis=list(title="Percent"), barmode="stack", autosize=FALSE)

        output$plt_pathways_per_chemical <- renderPlotly(plt)

    }, ignoreNULL=TRUE, ignoreInit=TRUE)

    observeEvent(input$pathway_input_rows_selected, {
        sel <- init_pathways[input$pathway_input_rows_selected, ]

        sel_chem <- init[input$chem_input_rows_selected, ]
        chem_id <- sel_chem[sel_chem$preferred_name != "DMSO", "dsstox_substance_id"][1]

        pathway_name <- sel[, "msigdb_pathway_name"][1]


        pathway_subset_up <- PATHWAYS_POS[PATHWAYS_POS$msigdb_pathway_name == pathway_name & PATHWAYS_POS$dsstox_substance_id == chem_id, c("dsstox_substance_id", "preferred_name", "msigdb_pathway_name", "percent_present", "percent_upregulated_total")]
        pathways <- pathway_subset_up

        pathways_count <- reshape2::melt(pathways, value.name="count", id=c("dsstox_substance_id", "preferred_name", "msigdb_pathway_name"))

        plt <- plot_ly(pathways, x=~msigdb_pathway_name, y=~percent_upregulated_total, name="percent_upregulated_total", type="bar", height=400, width=600, source="plt_pathways")
        plt <- plt %>% add_trace(y=~percent_present, name="percent_present", type="bar")
        plt <- plt %>% layout(title=paste0("Pathway:<br>", pathway_name), xaxis=list(title="Pathway"), yaxis=list(title="Percent"), barmode="stack", autosize=FALSE)

        output$plt_pathways <- renderPlotly(plt)
    }, ignoreNULL=TRUE, ignoreInit=TRUE)


    observeEvent(event_data("plotly_click", source="plt_pathways"), {
        sel <- event_data("plotly_click", source="plt_pathways")
        selected_pathway <- sel[1, "x"]

        sel_chem <- init[input$chem_input_rows_selected, ]
        chem_id <- sel_chem[sel_chem$preferred_name != "DMSO", "dsstox_substance_id"][1]
        chem_name <- sel_chem[sel_chem$preferred_name != "DMSO", "preferred_name"][1]

        # Load genes for selected pathway
        pathway_up_query <- paste0("
            SELECT
                dsstox_substance_id,
                preferred_name,
                msigdb_pathway_name,
                gene_name
            FROM
                bravo_misgdbpathway_hcon_poszscopval_genes
            WHERE
                msigdb_pathway_name = $1
            AND dsstox_substance_id = $2
        ")
        pathways_up <- run_query(pathway_up_query, args=list(selected_pathway, chem_id))

        pathway_to_chems_query <- paste0("
            SELECT
                dsstox_substance_id,
                preferred_name,
                gene_name,
                concentration,
                adjusted_value
            FROM
                bravo_foldchanges_total
            WHERE
                dsstox_substance_id = $1
            AND gene_name IN (", paste0(lapply(seq_len(length(unique(pathways_up$gene_name))), function(x) paste0("$", x+1)), collapse=", "), ")
        ")
        pathway_to_chems <- run_query(pathway_to_chems_query, args=as.list(c(chem_id, unlist(pathways_up$gene_name))))

        # Plots for genes
        d <- split(pathway_to_chems, pathway_to_chems$concentration)
        fig <- plot_ly()
        for(x in d){
            fig <- fig %>% add_trace(x, x=x$gene_name, y=x$adjusted_value, color=as.character(x$concentration), type="box", quartilemethod="exclusive", name=x$concentration[1])
        }
        fig <- fig %>% layout(title=paste0("Gene expression fold change for pathway:<br>", selected_pathway, "<br>chemical: ", chem_name), boxmode="group")
        output$plot_pathchem <- renderPlotly(fig)
        output$dt_pathchem <- renderDT(
            pathway_to_chems,
            extensions="Buttons",
            selection="none",
            rownames=FALSE,
            class="row-border stripe compact",
            filter=list(position="top", clear=TRUE),
            caption=paste0("Gene expression fold change for pathway: ", selected_pathway, "; chemical: ", chem_name),
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
