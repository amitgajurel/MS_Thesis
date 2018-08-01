pkg_gui = c("shiny","shinydashboard")
#install.packages(pkg_gui)
lapply(pkg_gui,library, character.only = TRUE)

shinyUI(
  dashboardPage(
    dashboardHeader(title = " This is the Header"),
    dashboardSidebar(
      menuItem("Menu Item 1"),
        menuSubItem("Menu Subitem 1"),
        menuSubItem("Menu Subitem 2"),
      menuItem("Menu Item 2"),
      menuItem("Menu Item 3")
    ),
    dashboardBody()
    
  )
)