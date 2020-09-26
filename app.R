library(shiny)
library(tidytext)
library(tidyverse)
library(pdftools)
library(tidyr)
library(tm)
library(textdata)
library(reshape2)
library(wordcloud)
library(qdapDictionaries)
library(ggthemes)
library(shinyWidgets)
library(ggraph)
library(ngram)
library(textclean)

x <- {".pdf"}
y <- list.files(path = "scripts/.")

movies <- y[apply(sapply(x, function(q) {grepl(q, y)}), 1, function(x) {sum(as.numeric(x)) > 0})]

scripts <- data.frame()

for (i in 1:length(movies)){
    name <- paste("scripts/", movies[i], sep = "")
    film_name <- str_split(name, pattern = "\\.")[[1]][1]
    
    temp <- sapply(name, pdf_text) %>%
        str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
        tibble() %>%
        rename(text = ".") %>%
        mutate(linenumber = row_number(),
               film = film_name) %>%
        unnest_tokens(word, text)
    scripts <- rbind(scripts, temp)
}

scripts <- scripts %>% 
    mutate(film = str_replace_all(film, pattern = "scripts/", 
                                       replacement = ""))

ui <- fluidPage(

    setBackgroundColor(color = "#424242"),
    
    titlePanel("Syncopy Data",
               title = div(img(src = "syncopy.jpg",
                               height = "50%", width = "50%"), 
                               style = "text-align: center;")),

    tabsetPanel(tabPanel("Graph",fluid = TRUE,
    sidebarLayout(
        sidebarPanel(style = "background-color: #424242; color: #FFFFFF; 
                     border-color: #424242",
            checkboxGroupInput("films", "Films",
                               c("Batman Begins" = "Batman Begins",
                                 "The Dark Knight" = "The Dark Knight",
                                 "The Dark Knight Rises" = "The Dark Knight Rises",
                                 "Inception" = "Inception",
                                 "Interstellar" = "Interstellar",
                                 "The Prestige" = "The Prestige",
                                 "Dunkirk" = "Dunkirk",
                                 "Insomnia" = "Insomnia")),
            
            selectInput("graph", "Plot Type",
                        c("Sentiments" = "sent",
                          "Frequency" = "freq")),
            selectInput("pos", "Position",
                       c("Stacked" = "stack",
                         "Side-by-side" = "dodge"))
        ), mainPanel(plotOutput("data")))),
    
    tabPanel("Markov", fluid = TRUE,
             sidebarLayout(
             sidebarPanel(style = "background-color: #424242; color: #FFFFFF; 
                 border-color: #424242",
                 selectInput("film", "Film",
                                    c("Batman Begins" = "Batman Begins.pdf",
                                      "The Dark Knight" = "The Dark Knight.pdf",
                                      "The Dark Knight Rises" = "The Dark Knight Rises.pdf",
                                      "Inception" = "Inception.pdf",
                                      "Interstellar" = "Interstellar.pdf",
                                      "The Prestige" = "The Prestige.pdf",
                                      "Dunkirk" = "Dunkirk.pdf",
                                      "Insomnia" = "Insomnia.pdf")),
             numericInput("ngram", "Word Sequence Length (ngram)",
                          value = 3, min = 1, max = 5)
    ), mainPanel(plotOutput("markov"))
    ))
    )
)

server <- function(input, output) {
    
    output$markov <- renderPlot({
        selection <- sapply(paste("scripts/", input$film, sep = ""), pdf_text)
        
        text <- paste(selection, collapse = " ") %>%
            tolower() %>%
            str_replace_all(pattern = "\\(v.o.\\)", replacement = " ") %>%
            str_replace_all(pattern = "’", replacement = "\'") %>%
            replace_contraction(contraction.key = lexicon::key_contractions,
                                ignore.case = TRUE) %>%
            str_replace_all(pattern = "cont\'d", replacement = " ") %>%
            str_replace_all(pattern = "cut to:\n.*\\n", replacement = " ") %>%
            str_replace_all(pattern = "int.*\\n", replacement = " ") %>%
            str_replace_all(pattern = "ext.*\\n", replacement = " ") %>%
            str_replace_all(pattern = "-", replacement = " ") %>%
            str_replace_all(pattern = "cut to:", replacement = " ") %>%
            str_replace_all(pattern = "\\n", replacement = " ") %>%
            str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
            str_replace_all(pattern = "\\s+", replacement = " ") %>%
            str_replace_all(pattern = " s ", replacement = " ") %>%
            str_replace_all(pattern = " m ", replacement = " ") %>%
            ngram(n = input$ngram) %>%
            get.phrasetable() %>%
            separate(ngrams, into = c("word", "word2"), sep = " ") %>%
            arrange(desc(prop)) %>%
            head(50)
        
        ggraph(text, layout = "fr") +
            geom_edge_link(mapping = aes(edge_alpha = prop),
                           color = "lightcoral", 
                           end_cap = circle(0.05, 'inches'), 
                           edge_width = 1) +
            geom_node_text(aes(label = name, color = "#lightcoral", 
                               fontface = 2)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill = "grey26"),
                  plot.background = element_rect(fill = "grey26", color = NA),
                  legend.background = element_rect(fill = "grey26"),
                  legend.position = "none",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    })
    
    output$data <- renderPlot({
        filtered <- scripts %>% filter(film %in% c(paste(input$films)))
        len <- length(c(paste(input$films)))
        
        if (len == 0){
            ggplot() + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = "grey26"),
                      plot.background = element_rect(fill = "grey26", color = NA))
        }
        else if (input$graph == "sent"){
            filtered %>% 
                inner_join(get_sentiments("bing")) %>%
                group_by(film, linenumber) %>%
                count(sentiment) %>%
                pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
                mutate(sentiment = positive - negative) %>%
                ggplot(aes(linenumber, sentiment, fill = film)) +
                    geom_col(width = 1.5, position = input$pos) +
                    ylim(-20, 20) +
                    labs(x = "Page Number", y = "Mean Sentiment") +
                    theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill = "grey26"),
                        plot.background = element_rect(fill = "grey26", color = NA),
                        legend.background = element_rect(fill = "grey26"),
                        legend.position = "bottom",
                        legend.text = element_text(size = 15, color = "#ffffff"),
                        legend.title = element_text(size = 15, color = "#ffffff"),
                        axis.title = element_text(size = 15, color = "#ffffff"),
                        axis.text = element_text(size = 15, color = "#ffffff")) +
                geom_hline(yintercept = 0, color = "white", size = 1.5) +
                scale_fill_manual(name = "Film", values = c("Batman Begins" = "lightcoral", 
                                                            "The Dark Knight" = "#F8B195",
                                                            "The Dark Knight Rises" = "#C06C84", 
                                                            "Inception" = "#6C5B7B", 
                                                            "Interstellar" = "#355C7D", 
                                                            "The Prestige" = "snow2", 
                                                            "Dunkirk" = "#A8E6CE",
                                                            "Insomnia" = "#83AF9B"))
        }
        else if (input$graph == "freq"){
            if (len > 1){
                scripts %>% 
                    filter(film %in% c(paste(input$films))) %>%
                    group_by(film) %>%
                    count(word) %>%
                    ungroup() %>%
                    bind_tf_idf(word, film, n) %>%
                    arrange(desc(tf_idf)) %>%
                    group_by(film) %>% 
                    top_n(5) %>% 
                    arrange(desc(tf_idf)) %>%
                    ggplot(aes(reorder(word, tf_idf), tf_idf, fill = film)) +
                        geom_col(position = input$pos) +
                        coord_flip() +
                        labs(x = "Words", y = "Term Frequency–Inverse Document Frequency") +
                        theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_rect(fill = "grey26"),
                            plot.background = element_rect(fill = "grey26", color = NA),
                            legend.background = element_rect(fill = "grey26"),
                            legend.position = "bottom",
                            legend.text = element_text(size = 15, color = "#ffffff"),
                            legend.title = element_text(size = 15, color = "#ffffff"),
                            axis.title = element_text(size = 15, color = "#ffffff"),
                            axis.text = element_text(size = 15, color = "#ffffff")) +
                            scale_fill_manual(name = "Film", values = c("Batman Begins" = "lightcoral", 
                                                                        "The Dark Knight" = "#F8B195",
                                                                        "The Dark Knight Rises" = "#C06C84", 
                                                                        "Inception" = "#6C5B7B", 
                                                                        "Interstellar" = "#355C7D", 
                                                                        "The Prestige" = "snow2", 
                                                                        "Dunkirk" = "#A8E6CE",
                                                                        "Insomnia" = "#83AF9B"))
            }
            else{
                ggplot() + 
                    theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_rect(fill = "grey26"),
                          plot.background = element_rect(fill = "grey26", color = NA))
            }
        }
        else{
            ggplot() + 
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = "grey26"),
                      plot.background = element_rect(fill = "grey26", color = NA))
        }
        })
}

shinyApp(ui = ui, server = server)
