# # Help page structure
# 
# upload_titles <-
#     c("Here is a frequently asked question",
#         "And another one",
#         "And a third")
# upload_contents <- list(
#     "This is the answer to the first faq",
#     "This is the answer to the second faq",
#     "This is the answer to the third faq"
# )
# 
# search_titles <-
#     c("Here is a frequently asked question",
#         "And another one",
#         "And a third")
# search_contents <- list(
#     "This is the answer to the first faq",
#     "This is the answer to the second faq",
#     "This is the answer to the third faq"
# )
# 
# help_ui <- tabPanel("Help",
#     sidebarLayout(sidebarPanel(
#         tabPanel(
#             "Upload help",
#             htmlOutput(outputId = "upload_help"),
#             dq_space(),
#             dq_accordion(
#                 "upload_faqs",
#                 upload_titles,
#                 upload_contents,
#                 options = list(active = FALSE, collapsible = TRUE),
#                 bg_color = "#f8f5f0"
#             )
#         ),
#         tabPanel(
#             "Search help",
#             htmlOutput(outputId = "search_help"),
#             dq_space(),
#             dq_accordion(
#                 "search_faqs",
#                 search_titles,
#                 search_contents,
#                 options = list(active = FALSE, collapsible = TRUE),
#                 bg_color = "#f8f5f0"
#             )
#         )
#     )))

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

search_titles <-
    c("Here is a frequently asked question",
        "And another one",
        "And a third")
search_contents <- list(
    "This is the answer to the first faq",
    "This is the answer to the second faq",
    "This is the answer to the third faq"
)

help_ui <- tabPanel("Help",
    navbarPage(
        "",
        tabPanel(
            "Upload help",
            htmlOutput(outputId = "upload_help"),
            dq_accordion("upload_faqs",
                upload_titles,
                upload_contents,
                options = list(active = FALSE, collapsible = TRUE),
                bg_color = "#f8f5f0")
        ),
        tabPanel(
            "Search help",
            htmlOutput(outputId = "search_help"),
            dq_accordion("search_faqs",
                search_titles,
                search_contents,
                options = list(active = FALSE, collapsible = TRUE),
                bg_color = "#f8f5f0")
        )
    ))