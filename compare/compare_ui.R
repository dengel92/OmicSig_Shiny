compare_ui <- tabPanel(
    "Compare",
    #If you guys know a cooler/different layout from sidebarLayout,
    #Please tell me, please
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                "compare_1",
                label = "Signature 1",
                choices = NULL,
                multiple = FALSE
            ),            
            selectizeInput(
                "compare_2",
                label = "Signature 2",
                choices = NULL,
                multiple = FALSE
            ),
        actionButton("compare_signatures", "Compare Signatures")),
        mainPanel(
            textOutput("compare_result")    
        )
    )
)
