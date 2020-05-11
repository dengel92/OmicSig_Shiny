## Server logic for search page

## Source file containing general functions for the search page
source("search/search_functions.R", local=TRUE)

## Make a dataframe storing all the relevant information about each input widget
## id = the input ID of the widget
## field = the name of the database field corresponding to the widget
## display = the text to display in the selected search terms section of the app
## type = the type of widget (i.e. dropdown)
##      type is used to determine how to update the widget
## dbTable = the default table in the database to query for this field
widgets <-
    rbind.data.frame(
        c("searchSignatureName",
            "signature_name",
            "signatures",
            "dropdownSig",
            "platform_signature_view"),
        c("searchSpecies",
            "species",
            "species",
            "dropdown",
            "platform_signature_view"), 
        c("searchExperimentType",
            "experiment_type",
            "experiment types",
            "dropdown",
            "platform_signature_view"),
        c("searchPlatformName",
            "platform_name",
            "platforms",
            "dropdown",
            "platform_signature_view"),
        c("searchSourceType",
            "source_type",
            "source types",
            "dropdown",
            "platform_signature_view"),
        c("searchPhenotype",
            "phenotype",
            "phenotypes",
            "dropdown",
            "platform_signature_view"),
        c("searchFeatureType",
            "feature_type",
            "feature types",
            "dropdownFeat",
            "feature_signature_view"),
        c("searchFeatureName",
            "feature_name",
            "feature names",
            "dropdownFeat",
            "feature_signature_view"),
        c("searchKeyword",
            "keyword",
            "keywords",
            "dropdownKey",
            "keyword_signature_view"),
        c("searchSubmitter",
            "submitter",
            "submitters",
            "dropdown",
            "platform_signature_view"),
        c("searchUploadDate",
            "upload_date",
            "upload date (start, end)",
            "date",
            "platform_signature_view"),
        stringsAsFactors=FALSE
    )
## Set column names of dataframe
colnames(widgets) <- c("id", "field", "display", "type", "dbTable")

## Extract the row numbers for each type of dropdown menu
dropdownRows <- which(widgets$type == "dropdown")
sigRows <- which(widgets$type == "dropdownSig")
featRows <- which(widgets$type == "dropdownFeat")
keyRows <- which(widgets$type == "dropdownKey")

## Clear selected terms after clicking clear button
observeEvent(input$clear, {
    isolate({
        ## Clear all dropdown menus
        apply(widgets[c(dropdownRows, sigRows, featRows, keyRows), ],
            1,
            function(x) {
                clearDropdown(x[[1]], x[[2]], x[[5]])
            })
        
        ## Reset date range to default
        updateDateRangeInput(session,
            "searchUploadDate",
            start="2020-01-01",
            end=Sys.Date())
    })
})

## Show which search terms have been selected so far
output$searchTerms <- renderText(
    c(
        ## Section header
        "<p><font size=3><b>",
        "Selected Search Terms:</b></font></p><p><font size=2>",
        
        ## Show selected terms for each dropdown menu
        apply(widgets, 1, function(x) {
            selectedHTML(x[1], x[3])
        }),
        
        "</p></font>"
    )
)

## Construct list of in clauses
ins <- reactive({
    ## Initialize list to store result
    insList <- list()
    
    ## Pull out relevant rows of widgets dataframe
    dropdowns <- widgets[c(sigRows, dropdownRows), ]
    
    ## Loop through rows of dropdowns dataframe
    for (row in 1:dim(dropdowns)[1]) {
        ## Extract field name and dropdown id
        field <- dropdowns[row, ]$field
        id <- dropdowns[row, ]$id
        
        ## Add an element whose name is the field and whose value is the
        ##   input from the dropdown
        insList[[field]] <- input[[id]]
    }
    
    return(insList)
})

## Construct list of between clauses
betweens <- reactive({
    list(
        "upload_date"=input[["searchUploadDate"]]
    )
})

## Construct list of feature types and names
features <- reactive({
    list(
        "feature_name"=input[["searchFeatureName"]],
        "feature_type"=input[["searchFeatureType"]]
    )
})

## Construct list of keywords
keywords <- reactive({
    list(
        "keyword"=input[["searchKeyword"]]
    )
})

observe({
    ## Disable widgets until they finish updating
    lapply(widgets$id, disable)
    
    ## Get list of in clauses from reactive function
    ins <- ins()
    
    ## Extract signature names in clause for updating features and keywords
    featureSignatures <- list("signature_name"=ins[["signature_name"]])
    keywordSignatures <- featureSignatures
    
    ## If any features are selected, update signature_name in list of in clauses
    ## Also get list of all signature names corresponding to selected feature
    ##   names and types for updating keyword dropdown menu
    if (length(compact(features())) > 0) {
        ## Get the list of signature names for the in clause
        featureSignatures <-
            getIntersection("signature_name",
                "feature_signature_view",
                features(),
                ins())
        
        ## Update the list of in clauses
        ins <- featureSignatures
        
        ## Extract signature names from updated list of in clauses
        featureSignatures <-
            list("signature_name"=featureSignatures[["signature_name"]])
    }
    
    ## If any keywords are selected, update signature_name in list of in clauses
    ## Also get list of all signature names corresponding to selected keywords
    ##   for updating feature name and type dropdown menus
    if (length(compact(keywords())) > 0) {
        ## Get the list of signature names for the in clause
        keywordSignatures <-
            getIntersection("signature_name",
                "keyword_signature_view",
                keywords(),
                ins())
        
        ## Update the list of in clauses
        ins <-
            getIntersection("signature_name",
                "keyword_signature_view",
                keywords(),
                ins)
        
        ## Extract signature names from updated list of in clauses
        keywordSignatures <-
            list("signature_name"=keywordSignatures[["signature_name"]])
    }
    
    ## Update signature name dropdown menu
    if (length(ins[["signature_name"]]) > 0) {
        if (length(compact(c(features(), keywords()))) > 0) {
            ## If the list of in clauses contains signature_name AND features
            ##   or keywords are selected, then the signature_name in clause
            ##   will contain the remaining options for signature_name
            updateSelectizeInput(
                session,
                "searchSignatureName",
                choices=c(ins[["signature_name"]],
                    input[["searchSignatureName"]]),
                selected=input[["searchSignatureName"]]
            )
        } else {
            ## If the list of in clauses contains signature_name but NO features
            ##   or keywords are selected, then the signature_name in clause
            ##   will contain the already selected options for signature_name,
            ##   and the signature_name dropdown can be updated like the other
            ##   dropdowns (besides the feature and keyword dropdowns)
            updateDropdown(
                "searchSignatureName",
                "signature_name",
                "platform_signature_view",
                ins,
                betweens()
            )
        }
    } else {
        ## If the list of in clauses does not contain signature_name, then no
        ##   signatures, features, or keywords are selected, and we need to get
        ##   the list of all signatures available (based on other inputs) for
        ##   updating the signature, feature, and keyword dropdown menus
        signatures <- getFieldValues("signature_name",
            "platform_signature_view", ins, betweens())
        
        ## Update the signature name dropdown menu
        updateSelectizeInput(
            session,
            "searchSignatureName",
            choices=c(signatures, input[["searchSignatureName"]]),
            selected=input[["searchSignatureName"]]
        )
        
        ## Also update list of signature names for updating feature and
        ##   keyword dropdowns
        featureSignatures <- list("signature_name"=signatures)
        keywordSignatures <- featureSignatures
    }
    
    ## Update features dropdown menus
    apply(widgets[featRows, ], 1, function(x) {
        updateDropdown(x[[1]],
            x[[2]],
            x[[5]],
            c(keywordSignatures, features()),
            NULL)
    })
    
    ## Update keyword dropdown menu
    apply(widgets[keyRows, ], 1, function(x) {
        updateDropdown(x[[1]],
            x[[2]],
            x[[5]],
            featureSignatures,
            NULL)
    })
    
    ## Update remaining dropdown menus
    apply(widgets[dropdownRows, ], 1, function(x) {
        updateDropdown(x[[1]],
            x[[2]],
            x[[5]],
            ins,
            betweens())
    })
    
    ## Re-enable widgets
    lapply(widgets$id, enable)
})

## Display output and download button after clicking search button
resultsTable <- eventReactive(input$search, {
    ## Make sure at least one search term is selected before querying database
    if (length(compact(c(ins(), betweens(), features()))) < 1) {
        shinyalert("Please select at least one search term!")
        return()
    }
    
    ## Get list of in clauses from reactive function
    ins <- ins()
    
    ## If any features are selected, update signature_name in list of in clauses
    if (length(compact(features())) > 0) {
        ins <- getIntersection("signature_name", "feature_signature_view",
            features(), ins)
    }
    
    ## If any keywords are selected, update signature_name in list of in clauses
    if (length(compact(keywords())) > 0) {
        ins <- getIntersection("signature_name", "keyword_signature_view",
            keywords(), ins)
    }
    
    ## Search database for matching signatures
    sqlObj <-
        sqlFindingQuery(
            fields=c(
                "signature_name",
                "species",
                "experiment_type",
                "platform_name",
                "source_type",
                "phenotype",
                "submitter",
                "upload_date"
            ),
            dbTable="platform_signature_view",
            ins=ins,
            betweens=betweens()
        )
    
    ## Display error message instead of table if query produces no results
    if (dim(sqlObj)[1] < 1) {
        shinyalert("No results found.", "Please modify your search.")
        return()
    }
    
    return(sqlObj)
})

## Display table of search results
output$searchResults <- renderDataTable({
    ## Make sure there are results to show
    if (!is.null(resultsTable())) {
        ## Get table contents from reactive expression
        resultsTable <- resultsTable()
        
        ## Make signature name a link to that signature's directory
        resultsTable$signature_name <-
            createLink(resultsTable$signature_name)
        
        return(resultsTable)
    }
}, escape = FALSE)

## Select/unselect all rows depending on checkbox input
observeEvent(input$selectAll, {
    ## Create an object to manipulate existing table
    dtProxy <- dataTableProxy("searchResults")
    
    ## Update selected rows
    if (input$selectAll) {
        ## Select all rows
        DT::selectRows(dtProxy, input$searchResults_rows_all)
    } else {
        ## Unselect all rows
        DT::selectRows(dtProxy, NULL)
    }
})

## Update displayed table with list of selected rows
output$selectedRows <-
    renderPrint(print(input$searchResults_rows_selected))

## Download button for full search results table
output$searchResultsDownload <- downloadHandler(
    filename="SigRepo_search_results_table.tsv",
    content=function(file) {
        write.table(
            resultsTable(),
            file,
            row.names=FALSE,
            quote=FALSE,
            col.names=TRUE,
            sep="\t"
        )
    }
)

## Download button for selected signatures from search results table
output$selectedSearchResultsDownload <-
    downloadHandler(
        filename="SigRepo_search_results_selected.zip",
        content=function(file) {
            ## Navigate to a temporary directory
            owd <- setwd(tempdir())
            
            ## Return to original directory when download ends
            on.exit(setwd(owd))
            
            ## Start a list of files to zip
            files <- c()
            
            ## Extract signature names from selected rows
            signatures <-
                resultsTable()[input$searchResults_rows_selected, ]$signature_name
            
            ## Loop through the signature names from selected rows
            for (signature in signatures){
                ## Write each signature to a json file and save the filename
                fileName <- paste0(signature, ".json")
                writeJson(retrieveOmicSigObj(signature), fileName)
                # or we can directly specify the file path here; 
                # the path is hard-coded in retrieveOmicSigObj() function
                files <- c(fileName, files)
            }
            
            ## Create the zip file
            zip(file, files)
        },
        contentType="application/zip"
    )

