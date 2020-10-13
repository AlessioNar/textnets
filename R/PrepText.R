#' @title Prepare text for analysis 
#'
#' @description  The function creates a dataframe in summarized tidy text format (where each row of the dataset describes the prevalence of each word within the document). 
#'
#' @param dataframe Dataframe
#' @param textvar Name of the column containing the text
#' @param groupvar Name of the column containing the grouping variable
#' @param tokenizer Describes whether the input data are tweets rather than regular text. If tokenizer = "tweets" then hashtag and mentions will be tokenized correctly, as well as the url removed. 
#' @param strip_punct Boolean. If true, it will automatically remove punctuation
#' @param node_type Describes whether the user wants to create a network where the nodes are words within a document, or the nodes are the documents themselves.
#' @param pos If pos = "nouns" it will returns only nouns or proper nouns.
#' @param language 
#' @param remove_stop_words Boolean. If true, it will automatically remove stopwords
#' @param remove_numbers Boolean. If true, it will automatically remove all numbers
#' @param compound_nouns Boolean. If true, it will parse noun compounds
#' @param udmodel_lang It provides the possibility to pass a preloaded udpipe language model
#' 
#' 
#' @return Dataframe summarized in tidytext format
#'
#' @examples \dontrun{PrepText(textdata, groupvar, textvar)}
#'
#' @export PrepText
#' 

PrepText <- function(textdata, groupvar, textvar, node_type = c("groups","words"), 
                    tokenizer = c("words", "tweets"), pos = c("all", "nouns"),
                    language = "english", remove_stop_words = FALSE, 
                    remove_numbers = NULL, compound_nouns = FALSE,
                    udmodel_lang = NULL,
                    ...) {
  
  # remove non-UTF8 characters
  textdata[[textvar]] <- iconv(textdata[[textvar]],  to="UTF-8", sub='')
  
  # remove emojis, symbols, and meta characters from tweets
  if (tokenizer=="tweets") {
    textdata[[textvar]] <- gsub("&amp;|&lt;|&gt;|RT", "", textdata[[textvar]])
    if (!is.null(remove_numbers) && isTRUE(remove_numbers)) { # && evaluates arg two only if arg one is true
      textdata[[textvar]]<-gsub("\\b\\d+\\b", "",textdata[[textvar]])
    }
  }
  
  if(is.null(udmodel_lang)){
  # udpipe setup
  # download udpipe language model
  lang_mod <- udpipe_download_model(language = language)
  # set up udpipe language model for pos tagging
  udmodel_lang <- udpipe_load_model(file = lang_mod$file_model)
  }
  
  ## DEFAULT: ANNOTATE WORDS NOT COMPOUND NOUNS
  if (isFALSE(compound_nouns)){
    
    textdata_tokens <- as_tibble({{textdata}}) %>%
      select({{groupvar}}, {{textvar}}) %>%
      unnest_tokens(output = "word", input = {{textvar}}, token = {{tokenizer}}, ...)
    
    # get part of speech with udpipe
    # annotate for pos only w/ pre-tokenized data
    # following: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html#annotate_your_text
    textdata_pos <- as.data.frame(udpipe_annotate(udmodel_lang, x = textdata_tokens$word,
                                                tagger = "default", parser = "none",
                                                tokenizer = "vertical"))
    
    # combine part of speech and textdata
    textdata <- bind_cols(textdata_tokens, textdata_pos[, c("upos", "lemma")])
  }

  
  ## IF SPECIFIED: ANNOTATE WORDS AND COMPOUND NOUNS
  if (isTRUE(compound_nouns)){
    
    # we use tidytext to flexibly tokenize words or tweets
    textdata_tokens <- as_tibble({{textdata}}) %>%
      select({{groupvar}}, {{textvar}}) %>%
      unnest_tokens(output = "word", input = {{textvar}}, token = {{tokenizer}}, strip_punct = FALSE, ...)
    
    # then we prepare the tokenized documents for dependency parsing
    textdata_tokens <- textdata_tokens %>% 
      group_by_({{groupvar}}) %>% 
      summarise(documents = paste(word, collapse = "\n"))
    
    # parse dependencies with udpipe
    textdata_dep <- as.data.frame(udpipe_annotate(udmodel_lang, x = textdata_tokens$documents,
                                                  doc_id = textdata_tokens[[groupvar]],
                                                  tagger = "default", parser = "default"))
    
    # NOUN COMPOUNDS
    # retrieve noun compounds
    # row numbers of all compound elements
    noun_compound <- which(textdata_dep$dep_rel=="compound")
    # list of consecutive compound elements
    compound_elements <- split(noun_compound, cumsum(c(1, diff(noun_compound) != 1)))
    # vector of compound bases
    compound_bases <- mapply(`[[`, compound_elements, lengths(compound_elements))+1
    # add compound bases to compound list
    all_compound_elements <- mapply(c, compound_elements, compound_bases, SIMPLIFY = FALSE)
    # retrieve all text elements and collapse them to get compound nouns
    compound_nouns <- sapply(all_compound_elements, function(x) paste0(textdata_dep$lemma[x], collapse = " "))
    
    # assign compound nouns to compound bases 
    textdata_dep$lemma[compound_bases] <- compound_nouns
    
    # remove compound elements and punctuation from dataframe
    textdata_dep <- textdata_dep %>% 
      filter(dep_rel!="compound" & upos!="PUNCT")
    
    # rename df and groupvar to avoid redudant coding
    textdata <- textdata_dep
    names(textdata)[1] <- groupvar
  }
  
  # remove stopwords
  if (remove_stop_words) {
    textdata <- {{textdata}} %>% 
      anti_join(get_stopwords(language = language), by = c("lemma" = "word"))
  }
  
  # subset to nouns and proper nouns (if desired)
  if (length(pos)>1){
    warning(paste0("You did not specify `pos`. Function defaults to all parts of speech."))
    pos <- "all"
  }
  if (pos=="nouns"){
    textdata <- {{textdata}} %>% 
      filter(upos%in%c("NOUN", "PROPN"))
  }
  
  # count word occurences within grouping variable
  if (length(node_type)>1){
    warning(paste0("You did not specify a `node_type`. Returned nodes are ", groupvar, "."))
    node_type <- "groups"
  }

  if (node_type=="groups"){
    # count terms by group
    textdata <- {{textdata}} %>%
      group_by_({{groupvar}}) %>%
      count(lemma) %>%
      rename(count = n)
  }

  if (node_type=="words"){
    # count groups by term
    textdata <- {{textdata}} %>%
      group_by(lemma) %>%
      count_({{groupvar}}) %>%
      rename(count = n)
  }
  
  return({{textdata}})
}
