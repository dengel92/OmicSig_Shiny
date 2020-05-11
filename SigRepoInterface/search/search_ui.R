## Search page structure
searchUI <- tabPanel("Search",
    ## Create a layout with a sidebar and a main panel
    sidebarLayout(
        ## Add the sidebar
        sidebarPanel(
            ## Add some space at the top
            dq_space(),
            
            ## Button for clearing selected terms
            actionButton("clear", "Clear Search Terms"),
            
            ## Show checkbox for displaying selected search terms
            checkboxInput("showSelected", "Show selected search terms"),
            
            ## Show selected search terms if checkbox is checked
            conditionalPanel(
                condition="input.showSelected == 1",
                htmlOutput(outputId="searchTerms")
            ),
            
            ## Dropdown menu for selecting signature name
            selectizeInput(
                inputId="searchSignatureName",
                label="Choose signature name(s)",
                choices=NULL,
                multiple=TRUE
            ),
            
            ## Dropdown menu for selecting species
            selectizeInput(
                "searchSpecies",
                label="Choose species",
                choices=NULL,
                multiple=TRUE
            ),
            
            ## Dropdown menu for selecting experiment type
            selectizeInput(
                inputId="searchExperimentType",
                label="Choose experiment type(s)",
                choices=NULL,
                multiple=TRUE
            ),
            
            ## Dropdown menu for selecting platform
            selectizeInput(
                inputId="searchPlatformName",
                label="Choose platform(s)",
                choices=NULL,
                multiple=TRUE
            ),
            
            ## Dropdown menu for selecting source type
            selectizeInput(
                inputId="searchSourceType",
                label="Choose source type(s)",
                choices=NULL,
                multiple=TRUE
            ),
            
            ## Dropdown menu for selecting phenotype
            selectizeInput(
                inputId="searchPhenotype",
                label="Choose phenotype(s)",
                choices=NULL,
                multiple=TRUE
            ),
            
            ## Checkbox to toggle additional search options
            checkboxInput("moreFilters", "Display additional filters"),
            
            ## Additional search options that appear only when "moreFilters"
            ##   is checked
            conditionalPanel(
                condition="input.moreFilters == 1",
                
                ## Dropdown menu for selecting feature type
                selectizeInput(
                    inputId="searchFeatureType",
                    label="Choose feature type(s)",
                    choices=NULL,
                    multiple=TRUE
                ),
                
                ## Dropdown menu for selecting feature names
                selectizeInput(
                    inputId="searchFeatureName",
                    label="Choose feature name(s)",
                    choices=NULL,
                    multiple=TRUE
                ),
                
                ## Dropdown menu for selecting keywords
                selectizeInput(
                    inputId="searchKeyword",
                    label="Choose keyword(s)",
                    choices=NULL,
                    multiple=TRUE
                ),
                
                ## Dropdown menu for selecting submitter name
                selectizeInput(
                    inputId="searchSubmitter",
                    label="Choose submitter name(s)",
                    choices=NULL,
                    multiple=TRUE
                ),
                
                ## Date range input for selecting upload date
                dateRangeInput("searchUploadDate",
                    label="Choose upload date range",
                    start="2020-01-01", end=Sys.Date()
                )
            ),
            
            ## Button to submit search terms
            actionButton("search", "Search Signatures")
        ),
        
        ## Add the main panel
        mainPanel(
            ## Add some space at the top
            dq_space(),
            
            ## Panel appears only when results table is displayed
            conditionalPanel(
                condition="output.searchResults",
                
                ## Show checkbox for selecting/unselecting all rows
                checkboxInput("selectAll", "Select/unselect all")
            ),
            
            ## Show table of signatures matching search terms
            div(DT::dataTableOutput("searchResults"),
                ## If the output table is too wide, add a scrollbar
                style="overflow-y: scroll"),
            
            ## Panel appears only when results table is displayed
            conditionalPanel(
                condition="output.searchResults",
                
                ## Show download button for full search results table
                downloadButton("searchResultsDownload", "Download Table"),
                
                ## Show download button for selected signatures
                downloadButton(
                    "selectedSearchResultsDownload",
                    "Download Selected Signatures"
                )
            )
        )
    ))