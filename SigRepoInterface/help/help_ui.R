# Help page structure

# Set up accordion menu headers and contents
accordion_titles <-
    c("About",
        "How tall is this panel",
        "Uploading a new signature")

accordion_contents <- list(
    htmlOutput("about_text"),
    "This panel is short now. :)",
    "You have several options when uploading a new signature."
)

# Make accordion menu
accordion_menu <- dq_accordion(
    "accordion_menu",
    accordion_titles,
    accordion_contents,
    options = list(
        # Allow all panels to be closed at once
        collapsible = TRUE,
        # Close all panels by default
        active = FALSE,
        # Set the height of each panel to be the height of the content
        heightStyle = "content"),
    bg_color = "#f8f5f0"
)

# Help page structure
help_ui <- tabPanel("Help",
    fluidPage(title = "Help",
        dq_space(),
        accordion_menu))