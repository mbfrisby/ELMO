shinyUI(fluidPage(
  titlePanel("ELMO Version 0.1.3"),
  h4("Welcome to the Endogenous Latent Moderator (ELMO) Variance Calculator BETA Version 0.1.3"),
  sidebarPanel(
    fileInput(inputId = 'file1', 'Please upload your Mplus .out file containing endogenous interactions', multiple = FALSE,
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.out')),
              p("We work hard to build and maintain this calculator, and to make it freely available to anyone wishing to use it. 
              You can support us by citing our work."),
              p(strong("Citation:"), "Frisby, M. B., Diemer, M., A. (",em("under review"),"). Everything in moderation: a proposed 
              improvement to variance calculation for visualizing latent endogenous moderation."),
              HTML("<p>For access to more information including R code to run this program locally, 
              please see <a href='https://github.com/mbfrisby/EndogenousLVModeration'>
              https://github.com/mbfrisby/EndogenousLVModeration</a>.</p>"),
              p(strong("Contact:"), "mbfrisby@umich.edu"),
              br(),
              p(strong("Note:"), "Variances for endogenous variables predicted by anteceding interactions are incompletely calculated. Please 
              see the above citation for details and corrections.")
  ),
  mainPanel(
    tableOutput('endovar'), tableOutput('endosd')
  )
))