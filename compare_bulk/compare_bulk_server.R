
compare_sigs <- function(){
    return(DT::renderDataTable(sql_finding_query('signature_pair_view')))
}

output$compare_table <- compare_sigs()

output$download_result <- downloadHandler(
    filename = paste("ree.tsv"),
    content = function(file) {
        write.table(sql_finding_query('signature_pair_view'), 
                    file, row.names = F, quote=F, col.names = T, sep="\t")
    } )