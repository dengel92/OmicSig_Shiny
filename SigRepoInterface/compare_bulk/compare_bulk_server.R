
compare_sigs <- function(){
    return(DT::renderDataTable(sqlFindingQuery('signature_pair_view')))
}

output$compare_table <- compare_sigs()

output$fraction_heatmap <- renderPlotly({compare_heatmap()})

output$download_result <- downloadHandler(
    filename = paste("ree.tsv"),
    content = function(file) {
        write.table(sqlFindingQuery('signature_pair_view'), 
                    file, row.names = F, quote=F, col.names = T, sep="\t")
    } )