pageWithSidebar(
  headerPanel('Exploratory Data Analysis'),
  sidebarPanel(
    selectInput("xcol", 'Choose dimension', dim)),
    mainPanel(plotOutput('plot1'))
)
