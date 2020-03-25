# Help page structure

# Set up accordion menu headers and contents
accordion_titles <-
    c("Here is a frequently asked question",
        "And another one",
        "And a third")

accordion_contents <- list(
    "This is the answer to the first faq",
    "This is the answer to the second faq",
    "This is the answer to the third faq"
)

# Make accordion menu
accordion_menu <- dq_accordion("accordion_menu",
    accordion_titles,
    accordion_contents,
    # These options allow all of the panels to be collapsed at once
    options = list(active = FALSE, collapsible = TRUE),
    bg_color = "#f8f5f0")

# Help page structure
help_ui <- tabPanel("Help",
    fluidPage(title = "Help",
        accordion_menu)
    )