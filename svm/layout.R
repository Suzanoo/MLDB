header <- tagList(
  # img(src = "appsilon-logo.png", class = "logo"),
  div(Text(variant = "medium", "Support Vector Matchine Demo"), class = "title"),
  # CommandBar(
  #   items = list(
  #     CommandBarItem("New", "Add", subitems = list(
  #       CommandBarItem("Email message", "Mail", key = "emailMessage", href = "mailto:me@example.com"),
  #       CommandBarItem("Calendar event", "Calendar", key = "calendarEvent")
  #     )),
  #     CommandBarItem("Upload sales plan", "Upload"),
  #     CommandBarItem("Share analysis", "Share"),
  #     CommandBarItem("Download report", "Download")
  #   ),
  #   farItems = list(
  #     CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
  #     CommandBarItem("Info", "Info", iconOnly = TRUE)
  #   ),
  #   style = list(width = "100%"))
  )

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Data', url = '#!/data', key = 'data', icon = 'Table'),
      list(name = 'Analysis', url = '#!/other', key = 'analysis', icon = 'AnalyticsReport')
      # list(name = 'shiny.fluent', url = 'http://github.com/Appsilon/shiny.fluent', key = 'repo', icon = 'GitGraph'),
      # list(name = 'shiny.react', url = 'http://github.com/Appsilon/shiny.react', key = 'shinyreact', icon = 'GitGraph'),
      # list(name = 'Appsilon', url = 'http://appsilon.com', key = 'appsilon', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Appsilon", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at hello@appsilon.com"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)
  

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}