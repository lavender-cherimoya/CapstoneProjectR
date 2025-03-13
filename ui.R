fluidPage(
      useShinyjs(), 
      titlePanel('Next word prediction Application'),

      # Side bar for the user-application interactions and place to to enter the input
      sidebarLayout(
            sidebarPanel(
                  textInput("sentence", 
                            "Enter the beginning of a sentence:", 
                            placeholder = "Type your sentence here..."),
                  actionButton("analyse", "Analyse Sentence"),
                  br(),
                  actionButton("show_doc", "Documentation"),
                  
            ),
            
            # Main panel which will show the output
            mainPanel(
                  h3("Predicted Next Word:"),
                  textOutput("best_suggestion"),
                  tableOutput("dataframe_output"),
                  div(id = "loading_message", style = "display:none;",  
                      h4("Analysing sentence... Please wait."))  # Loading message
            )
      )
)
