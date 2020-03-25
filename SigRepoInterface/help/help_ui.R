# Help page structure

upload_titles <-
    c("Here is a frequently asked question",
        "And another one",
        "And a third")
upload_contents <- list(
    "This is the answer to the first faq",
    "This is the answer to the second faq",
    "This is the answer to the third faq"
)
upload_help_tab <- tabPanel(
    "Upload help",
    htmlOutput(outputId = "upload_help"),
    dq_accordion("upload_faqs",
        upload_titles,
        upload_contents,
        options = list(active = FALSE, collapsible = TRUE),
        bg_color = "#f8f5f0")
)

search_titles <-
    c("Here is a frequently asked question",
        "And another one",
        "And a third")
search_contents <- list(
    "This is the answer to the first faq",
    "This is the answer to the second faq",
    "This is the answer to the third faq"
)
search_help_tab <- tabPanel(
    "Search help",
    htmlOutput(outputId = "search_help"),
    dq_accordion("search_faqs",
        search_titles,
        search_contents,
        options = list(active = FALSE, collapsible = TRUE),
        bg_color = "#f8f5f0")
)

help_ui <- tabPanel("Help",
    navbarPage(
        "",
        upload_help_tab,
        search_help_tab
    ))