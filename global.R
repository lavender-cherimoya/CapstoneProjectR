library(shinyjs)
library(dplyr)
library(quanteda) #package for corpus
library(data.table)

unigram_dt <- readRDS("./unigrams_dt.rds")
bigram_dt <- readRDS("./bigrams_dt.rds")
trigram_dt <- readRDS("./trigrams_dt.rds")
#tetragrams_df <- readRDS("./tetragrams_df.rds")

documentationModal <- function() {
      modalDialog(
            title = "Documentation",
            p("This application predicts the most probable next word following a series of words. To use the app:"),
            tags$ul(
                  tags$li("Enter the beginning of a sentence of your choice in the dedicated space."),
                  tags$li("Click on the Analyse sentence button. The processing of the analysis can take some time."),
                  tags$li("Once the analysis is finished, the applicatios displays the best suggestion for 
                          the next word. In addition, a table with up to 10 most probable words with their frequencies
                          is also displayed below. If a tie exists between several most possible words,
                          the application choses the best suggestion by looking in the unigram dictionary. "),
                  tags$li("A new sentence can be entered in the dedicated space at any moment after the first analysis is done.")
            ),
            
            actionButton("show_data_info", "Show Data Information"),
            
            footer = modalButton("Close")
      )
}

find_next_word <- function(ngram_dt, n, words) {
      # Get the last words of the input sentence
      last_words <- paste(tail(words, n-1), collapse = " ")
      
      # For debugging
      #print(last_words)
      
      # Filter the ngram_dt to find matching ngrams
      matching_ngrams <- ngram_dt[grepl(paste0("^", last_words, "\\b"), Ngram), ]
      
      # If no match is found, return NA
      if (nrow(matching_ngrams) == 0) {
            return(NA)
      }
      
      # Return a data table with the 10 most frequent ngrams
      matching_ngrams <- matching_ngrams[order(-Frequency)][1:10, ]
      
      return(matching_ngrams)
}

predict_next_word <- function(sentence, unigram_dt = unigram_dt,
                              bigram_dt = bigram_dt, trigram_dt = trigram_dt) {
      
      # Clean the input
      cleaned_tokens <- tokens(sentence,
                               what = "word",
                               remove_punct = TRUE,
                               remove_numbers = TRUE,
                               remove_separators = TRUE,
                               remove_symbols = TRUE,
                               remove_url = TRUE) %>% tokens_tolower()
      
      words <- as.character(cleaned_tokens)
      
      # Get the total number of words used as input
      num_tokens <- ntoken(cleaned_tokens)
      
      # For debugging
      #print(cleaned_tokens)
      #print(words)
      #print(num_tokens)
      
      answer <- NA
      
      # If there are more than 2 words used as input, use first the tri-grams dictionary
      if (num_tokens > 1) {
            answer <- find_next_word(trigram_dt, 3, words)
      }
      
      # Else, use the bi-grams dictionary
      if (any(is.na(answer)) && num_tokens > 0) {
            answer <- find_next_word(bigram_dt, 2, words)
      }
      
      # If no valid input, or no matching n-grams is found, return a warning message
      if (any(is.na(answer))) {
            return(list(dataframe = "No matching n-gram was found.", best_suggestion = "No matching n-gram was found."))
      }
      
      
      # Get only the top n-grams based on Frequency
      top_frequency <- max(answer$Frequency)
      top_ngrams <- answer[Frequency == top_frequency, ]
      
      # Check if we found at least one n-gram
      if (nrow(top_ngrams) > 0) {
            # Extract the last word of each n-gram
            last_words <- sapply(strsplit(top_ngrams$Ngram, " "), tail, 1)
            
            # Get the uni-gram frequency for each last word
            unigram_frequencies <- sapply(last_words, function(word) {
                  freq <- unigram_dt$Frequency[unigram_dt$Ngram == word]
                  
                  if (length(freq) > 0) {
                        return(freq)
                  } else {
                        return(0)  # Return 0 if the word is not found in unigram_dt
                  }
            })
            
            # Add the LastWord and UnigramFrequency columns to top_ngrams
            top_ngrams[, LastWord := last_words]
            top_ngrams[, UnigramFrequency := unigram_frequencies]
            
            # If there is a tie between several n-grams for the most frequent ones
            # Handle them using uni-grams frequency of the last word of those n-grams
            # Therefore, we sort by UnigramFrequency
            top_ngrams <- top_ngrams[order(-UnigramFrequency), ]

      }
      
      # The best suggestion is the last word of the very top n-gram
      best_suggestion <- top_ngrams$LastWord[1]
      
      # Return the table with the 10 most frequent n-grams, as well as the best suggestion
      return(list(dataframe = answer, best_suggestion = best_suggestion))
}