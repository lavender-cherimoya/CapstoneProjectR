function(input, output) {
      
      # Call the documentationModal() function, written in global.R, to display the
      # documentation text
      observeEvent(input$show_doc, {
            showModal(documentationModal())
      })
      
      # Text about the data, which is displayed when clicking on the Data Information
      # button, which is available in the Documentation page
      observeEvent(input$show_data_info, {
            showModal(modalDialog(
                  title = "Data & Algorithm Information",
                  p("The algorithm uses n-grams dictionaries to make predictions for the next word. It works as follows:"),
                  tags$ul(
                        tags$li("The entered words get preprocessed by removing punctuations and special characters,
                                the characters get lowered etc. They are also transformed into separated tokens."),
                        tags$li("The algorithm will first use the ttri-grams dictionary (n-grams composed of 3 tokens).
                                For this, it takes the last 2 entered tokens, then searches in the dictionary any
                                tri-grams that match those first 2 tokens."),
                        tags$li("If no match is found, the algorithm will repeat the same with the bi-grams dictionary
                                (n-grams composed of 2 tokens)."),
                        tags$li("If still no match is detected, the program returns the message No matching n-gram was found."),
                        tags$li("On the contrary, if one or more possible n-grams are found, they are saved in a dataframe,
                               ordered by frequency."),
                        tags$li("The algorithm gives back the last word of the n-gram with the highest frequency as best suggestion."),
                        tags$li("If there exists a tie between several n-grams, the algorithm takes the last word of each n-gram, and
                                looks for its frequency in the uni-grams dictionary (n-grams composed of 1 token). It choses the word
                                that has the highest frequency in the uni-grams dictionary as best suggestion."),
                        tags$li("The application displays the best suggestion as the predicted word. In addition, a table with up
                                to 10 other suggestions is shown below."),
                  ),
                  p("The datasets used in this application to create the n-grams dictionaries are coming from english corpora: `en_US.blogs.txt`, `en_US.news.txt` and `en_US.twitter.txt`. 
                  Each file represents a text corpus that originates either from blogs, news or twitter messages, which needs to be processed. The preprocessing
                    was performed as follows:"),
                  tags$ul(
                        tags$li("The three corpora were combined together."),
                        tags$li("The text data was cleaned. For example, profanity words can be present in the sample corpus, which is unwanted. 
                                To filter them out, a profanity list was created combining lexicons that are already implemented in R 
                                (`profanity_alvarez`, `profanity_banned`, `profanity_arr_bad`) as well as a list of bad words created by
                                `https://www.cs.cmu.edu`"),
                        tags$li("The unwanted non-ASCII characters, punctuations, numbers, separators, symbols and url links are removed."),
                        tags$li("After cleaning, the text corpora need to be put into an adapted format to be used as a n-gram dictionary. 
                                Therefore, they were first transformed into a list of tokens."), 
                        tags$li("The quanteda library is used to generate a list of n-grams. In this work, uni-grams (collection of 1 token),
                                bi-grams (collection of 2 tokens) and tri-grams (collection of 3 tokens) are used."),
                        tags$li("The n-grams data is put into a document-feature matrix (dfm) form, one matrix per n-gram type. Again, the quanteda
                                library is used for this."),
                        tags$li("Finally, making use of the dfm format, the data can be transformed into a dataframe with 3 columns. 
                                The first column contains the n-gram, the second column contains the frequency of the n-gram, and the third
                                column contains its probability."),
                        tags$li("The four obtained dataframes are saved as rds files. They are used as the dictionaries for the prediction application."),
                        

                  ),
                  
                  footer = tagList(
                        actionButton("back_to_doc", "Back"),
                        modalButton("Close")
                  )
            ))
      })
      # When the user clicks on the Back document, display again the text from
      # the documentation function
      observeEvent(input$back_to_doc, {
            removeModal()
            showModal(documentationModal())
      })
      
      observeEvent(input$analyse, {
            shinyjs::show(id = "loading_message")  # Show the loading message
      })
      
      # Place that will show the best suggestion (if the program finds one)
      output$best_suggestion <- renderText({
            input$analyse
            isolate({
                  sentence <- input$sentence
                  if (!is.null(sentence) && sentence != "") {
                        result <- predict_next_word(sentence, unigram_dt, bigram_dt, trigram_dt) #, tetragrams_df)
                        shinyjs::hide(id = "loading_message")
                        return(result$best_suggestion)
                  } else {
                        shinyjs::hide(id = "loading_message")
                        return("Please enter a sentence.")
                  }
            })
      })
      
      # Place that will show the n-gram table with the top frequencies (if the program finds them)
      output$dataframe_output <- renderTable({
            input$analyse
            isolate({
                  sentence <- input$sentence
                  if (!is.null(sentence) && sentence != "") {
                        result <- predict_next_word(sentence, unigram_dt, bigram_dt, trigram_dt) #, tetragrams_df)
                        shinyjs::hide(id = "loading_message")
                        return(result$dataframe)
                  } else {
                        shinyjs::hide(id = "loading_message")
                        return(data.frame(Message = "Please enter a sentence"))
                  }
            })
            })
      
      # Clear memory
      observeEvent(input$analyse, {
            gc()
      })

      
}
