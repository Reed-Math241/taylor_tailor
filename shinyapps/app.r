library(shiny)
library(shinythemes)
library(gt)
library(DT)
library(tidyverse)
library(readr)
library(stringr)
library(ggrepel)
library(ggplot2)

#horse race... to see code, see horse_race.rmd

#tables
log_data <- read_csv("horse_race_results/log_data.csv")

log_table <- log_data %>% 
  gt() %>%
  tab_header(title = md("**Logistic Regression Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("darkseagreen2","Tomato"))



lda_data <- read_csv("horse_race_results/lda_data.csv")

lda_table <- lda_data %>% 
  gt() %>%
  tab_header(title = md("**Linear Discriminant Analysis Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("darkseagreen2","Tomato"))



qda_data <- read_csv("horse_race_results/qda_data.csv")

qda_table <- qda_data %>% 
  gt() %>%
  tab_header(title = md("**Quadratic Discriminant Analysis Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("Tomato","darkseagreen2"))

tree_data <- read_csv("horse_race_results/tree_data.csv")

tree_table <- tree_data %>% 
  gt() %>%
  tab_header(title = md("**Classification Tree Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("Tomato","darkseagreen2"))





rf_data <- read_csv("horse_race_results/rf_data.csv")

rf_table <- rf_data %>% 
  gt() %>%
  tab_header(title = md("**Random Forest Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("Tomato","darkseagreen2"))



svm_data <- read_csv("horse_race_results/svm_data.csv")

svm_table <- svm_data %>% 
  gt() %>%
  tab_header(title = md("**Support Vector Machine Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("Tomato","darkseagreen2"))


nn_data <- read_csv("horse_race_results/nn_data.csv")

nn_table <- nn_data %>% 
  gt() %>%
  tab_header(title = md("**Neural Network Results**")) %>%
  tab_spanner(label = "Reference", columns = 2:3)  %>%
  tab_stubhead(label = "Predicted") %>%
  data_color(columns = 2, 
             colors = c("Tomato", "darkseagreen2")) %>%
  data_color(columns = 3, 
             colors = c("darkseagreen2", "Tomato"))


total_results <- read_csv("horse_race_results/horserace_results.csv")
total_results_table <- total_results %>%
  rename("overall accuracy" = overall) %>%
  gt() %>%
  tab_header(title = md("**Horse Race Results**")) %>%
  data_color(
    columns = 2:4,
    colors = scales::col_numeric(
      palette =  as.character(paletteer::paletteer_d("ggsci::purple_material", n = 10)),
      domain = c(.5, 1))
  )


false_positives_table <- false_positives_df %>%
  gt() %>%
  tab_spanner(label = "Logistic Regression", columns = c(1,2)) %>%
  tab_spanner(label = "LDA", columns = c(3,4)) %>%
  tab_spanner(label = "QDA", columns = c(5,6)) %>%
  tab_spanner(label = "Classification Tree", columns = c(7,8)) %>%
  tab_spanner(label = "Random Forest", columns = c(9,10)) %>%
  tab_spanner(label = "Support Vector Machine", columns = c(11,12)) %>%
  tab_spanner(label = "Neural Network", columns = c(13,14)) %>%
  tab_header(title = "Songs misclassified as Taylor Swift Songs", subtitle = NULL)  %>%
  data_color(columns = 1:2, colors = "#FAC9DC", apply_to = "fill") %>%
  data_color(columns = 3:4, colors = "#FA7575", apply_to = "fill") %>%
  data_color(columns = 5:6, colors = "#FF985E", apply_to = "fill") %>%
  data_color(columns = 7:8, colors = "#F0D878", apply_to = "fill") %>%
  data_color(columns = 9:10, colors = "#CAE8A2", apply_to = "fill") %>%
  data_color(columns = 11:12, colors = "#B3C7EB", apply_to = "fill") %>%
  data_color(columns = 13:14, colors = "#BFA7DB", apply_to = "fill")


#clustering plots and tables
pca_rotations <- read.csv("clustering_results/pca_rotations.csv")
pc1graph <- ggplot(data = pca_rotations, mapping = aes(x = variables, y = PC1, 
                                                       fill = variables)) +
  geom_col() +
  coord_flip() +
  labs(title = "Production: Simple vs. Embellished") +
  theme(legend.position = "none")

pc2graph <- ggplot(data = pca_rotations, mapping = aes(x = variables, y = PC2, 
                                                       fill = variables)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Rhythm vs. Energy")

n_pca_kmeans <- read_csv("clustering_results/n_pca_kmeans.csv")
df <- n_pca_kmeans %>% filter(artist == "Taylor Swift")
kmeans_graph <- ggplot(n_pca_kmeans, aes(x = PC1, y = PC2,
                                         color = as.factor(cluster))) + 
  geom_text_repel(df, mapping = aes(label = album_name, 
                  color = as.factor(cluster))) +
  geom_point(alpha = .4) +
  labs(title = "Kmeans Clustering",
       x = "Production: Simple vs. Embellished",
       y = "Rhythm vs. Energy",
       color = "Cluster") +
  theme(legend.position = "bottom")

kmeans_df <- n_pca_kmeans %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "upbeat and complex production",
    cluster == 2 ~ "slightly embellished, pop-adjacent",
    cluster == 3 ~ "lyrical and moody",
    cluster == 4 ~ "stripped down and calm"
  )) %>%
  dplyr::select(4:6)

na <- data.frame(artist = "", album_name = "")
c3 <- kmeans_df %>%
  filter(cluster == "lyrical and moody")
c3 <- c3[,-1]

kmeans_df <- kmeans_df %>%
  arrange(artist)
c1 <- kmeans_df %>%
  filter(cluster == "upbeat and complex production")
c1 <- c1[,-1]
while (length(c1$artist) < length(c3$artist)) {
  c1 <- rbind(c1, na)
}
c2 <- kmeans_df %>%
  filter(cluster == "slightly embellished, pop-adjacent")
c2 <- c2[,-1]
while (length(c2$artist) < length(c3$artist)) {
  c2 <- rbind(c2, na)
}


c4 <- kmeans_df %>%
  filter(cluster == "stripped down and calm")
c4 <- c4[,-1]
while (length(c4$artist) < length(c3$artist)) {
  c4 <- rbind(c4, na)
}

kmeans_table_start <- cbind(c1, c2, c3, c4) 

colnames(kmeans_table_start) <- c("Album Name", "   Artist",
                                  "Album Name ", "  Artist",
                                  "Album Name  ", " Artist",
                                  "Album Name   ", "Artist")

kmeans_table <- kmeans_table_start %>%
  gt() %>%
  tab_spanner(label = "Cluster 1", columns = c(1,2)) %>%
  tab_spanner(label = "Cluster 2", columns = c(3,4)) %>%
  tab_spanner(label = "Cluster 3", columns = c(5,6)) %>%
  tab_spanner(label = "Cluster 4", columns = c(7,8)) %>%
  tab_footnote(footnote = "Upbeat and complex production",
               location = cells_column_spanners(spanners = "Cluster 1")) %>%
  tab_footnote(footnote = "Slightly embellished, pop-adjacent",
               location = cells_column_spanners(spanners = "Cluster 2")) %>%
  tab_footnote(footnote = "Lyrical and moody",
               location = cells_column_spanners(spanners = "Cluster 3")) %>%
  tab_footnote(footnote = "Stripped down and calm",
               location = cells_column_spanners(spanners = "Cluster 4")) %>%
  tab_header(title = "Kmeans Clusters", subtitle = NULL)  %>%
  data_color(columns = 1:2, colors = "#A1BEE6", apply_to = "fill") %>%
  data_color(columns = 3:4, colors = "#FCBF6B", apply_to = "fill") %>%
  data_color(columns = 5:6, colors = "#C0ADDB", apply_to = "fill") %>%
  data_color(columns = 7:8, colors = "#D6E8BD", apply_to = "fill")


pca_titles <- read_csv("clustering_results/pca_titles.csv")

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Taylor Tailor: A Spotify Project"),
    mainPanel(width = 10,
      tabsetPanel(
        tabPanel("Overview",
                 tags$p("Three-time Album of the Year winner Taylor Swift released her debut album in 2006. With a discography spanning 15 years and multiple genres, 
                        it can be hard to know where to begin when exploring her music. On the other side of the spectrum, Taylor Swift fans have a reputation of listening to only her music.
                         The Taylor Tailor project seeks to solve both of these problems."),
                 br(),
                 tags$p("Using Spotify data obtained from the API wrapper, ", tags$a(href = "https://www.rcharlie.com/spotifyr/", tags$em("spotifyr")), 
                        ", we grouped similar albums together to determine album recommendations. We pulled data for 14 artists in multiple genres including pop, rap, and folk. 
                        Some groupings are predictable, such as 1989 by Taylor Swift and Dua Lipa’s Future Nostalgia in Cluster 1, but others are more surprising, like Kanye’s My Beautiful Dark Twisted Fantasy 
                        in the same group as Taylor Swift’s country albums. See the Clustering tab to see more results."),
                 br(),
                 tags$p("To further explore the similarities between the music, we compare methods to predict whether a song was made by Taylor Swift or not. Most models had an accuracy rate of 90% or above, 
                        so songs that are misclassified as Taylor Swift songs are highly likely to sound like a Taylor Swift song. See the Horse Race tab to view results.")),
        tabPanel("Clustering", 
                 sidebarLayout(
                   sidebarPanel(
                     checkboxGroupInput("artist", label = h3("Artists For PCA Map"), 
                                        choices = list("Taylor Swift" = "Taylor Swift", "Bon Iver" = "Bon Iver", "Kanye" = "Kanye",
                                                       "Simon & Garfunkel" = "Simon & Garfunkel", "Drake" = "Drake", "Charli XCX" = "Charli XCX", 
                                                       "Ariana Grande" = "Ariana Grande", "Carly Rae Jepsen" = "Carly Rae Jepsen",
                                                       "Dua Lipa" = "Dua Lipa", "The Chicks" = "The Chicks", "Beach Bunny" = "Beach Bunny", 
                                                       "Hozier" = "Hozier", "The Beatles" = "The Beatles", "Lorde" = "Lorde"),
                                        selected = 1),
                     
                     
                     hr(),
                     fluidRow(column(3, verbatimTextOutput("value")))
                   ),
                   mainPanel(tags$p("We use Principal Component Analysis to simplify the given Spotify attributes into two principal components, and then use K-means clustering to group similar albums by the average attribute values of the songs in the album. Hierarchical clustering was also looked at, but all Taylor Swift albums were grouped into one cluster out of four, so it was not as helpful."),
                           br(),
                           tags$p("PCA simplifies the predictors of a data set and tries to explain the variability of the data, however, in doing so, some interpretability is lost."),
                           tags$p("The important attributes for PC1 are instrumentalness and acousticness on the negative side, and loudness and energy on the positive side. We designate this principal component to be a measure of the production of the album, with negative values corresponding to more simple production and positive values corresponding to more complex and loud production."),
                           plotOutput(outputId = "pc1graph"),
                           tags$p("PC2 splits the attributes into duration and danceability on the negative side and valence and liveness on the positive side. We define this component so that a negative value corresponds to rhythm while a positive value corresponds to energy. A third principle component was plotted, but the split of attributes was similar to PC2, so it was not included in our analysis."),
                           plotOutput(outputId = "pc2graph"),
                           tags$p("We then plot the albums on a principal component map. The checkboxes in the sidebar can be used to compare the albums of multiple artists."),
                           plotOutput(outputId = "pca_titles"),
                           tags$p("In order to recommend a Taylor Swift album, we cluster the albums using the K-means technique. As the name suggests, this technique groups the observations into k clusters, with each observation belonging to the nearest cluster center.
                                  There was no distinct elbow on the scree plot, so we choose K = 4. There is a lot of overlap between Cluster 2 and Cluster 3, so the third principal component must be important in separating these clusters. Cluster 4 does not contain any Taylor Swift albums, but my personal recommendation would be the folklore album if you like the albums in this cluster."),
                           plotOutput(outputId = "kmeans_graph"),
                           tags$p("After examining the albums in each cluster and their respective principal components, we assigned characteristics to each cluster. The descriptions of each cluster can be found in the footnotes of the following table."),
                           gt_output(outputId = "kmeans_table")))),
        tabPanel("Horse Race",
                 p("In this section, we investigate the predictive power of different classification techniques. We compare the accuracy rates, sensitivity (True Positive Rate: Classifying a Taylor Swift song as made by Taylor Swift), and specificity (True Negative Rate: Classifying a song not made by Taylor Swift as a non-Taylor Swift Song). Specifically, we compare logistic regression, linear discriminant analysis, quadratic discriminant analysis, the  classification tree, random forest, support vector machine, and neural network."),
                 br(),
                 p("Our results were obtained using a test and train split of the dataset. Of the 2304 songs pulled from spotify, 2000 were split into the training dataset on which we trained our models, and 304 were kept aside as a test dataset. Of the 302 Taylor Swift songs in the entire dataset, 275 were used in the training dataset, and our models attempted to identify the remaining 27 Taylor Swift songs out of a pool of 300 observations."),
                 gt_output(outputId = "total_results_table"), br(),  br(),
                 
                 #change <br> to just be new paragraphs
                 p("Some conclusions:"),
                 tags$li("All models, with the exception of QDA, had an accuracy rate of approximately 90% or above, with differences in specificity and sensitivity."),
                 tags$li("My personal choice is the random forest, which had the highest accuracy of 97.33% in identifying whether a song was made by Taylor Swift or not."),
                 tags$li("Some models had a decent overall accuracy rate, such as logistic regression, LDA, and the Neural Network, roughly 90%, but identified very little Taylor Swift songs (low sensitivity). Essentially these models achieved their high accuracy by rejecting Taylor Swift for each song. Since there were many non-Taylor Swift songs, model's rates didn't suffer too badly."),
                 tags$li("On the other hand, QDA has the lowest accuracy rate, but the highest sensitivity. This model predicted too many song's to be made by Taylor Swift but when it was Taylor Swift, this model predicted correctly."),
                 tags$li("I'm sticking with my personal favorite, Random Forest, but if you're in a scenario where you were given a song and you absolutely can't pass up a Taylor Swift song, I could see QDA being useful."),
                 tags$li("If you are a fan of Taylor Swift, you will probably enjoy songs that were misclassified as Taylor Swift songs, such as Hozier's 'From Eden' and The Chick's 'Everybody Knows'."),
                 br(),
                 gt_output(outputId = "false_positives_table"), br(),  br(),
                 p("See the confusion matrices below for the summarize predictions for each model. 'True Negatives' in the top left corner, 'False Negatives' in the top right, 'False Positives' in the bottom left, and 'True Positives' in the bottom right corner. As noted above, the Random Forest method had the least total errors in identifying songs, and QDA identified the most Taylor Swift songs, although this came with many false positives" ),
                 gt_output(outputId = "log_table"), br(),  br(),
                 gt_output(outputId = "lda_table"), br(), br(),
                 gt_output(outputId = "qda_table"), br(), br(),
                 gt_output(outputId = "tree_table"), br(), br(),
                 gt_output(outputId = "rf_table"), br(), br(),
                 gt_output(outputId = "svm_table"), br(), br(),
                 gt_output(outputId = "nn_table"),  br(),  br()
                 )
        )
      
    )
   )
  


server <- function(input, output, session){
  output$log_table <- render_gt(log_table)
  output$lda_table <- render_gt(lda_table)
  output$qda_table <- render_gt(qda_table)
  output$tree_table <- render_gt(tree_table)
  output$rf_table <- render_gt(rf_table)
  output$svm_table <- render_gt(svm_table)
  output$nn_table <- render_gt(nn_table)
  output$total_results_table <- render_gt(total_results_table)
  output$false_positives_table <- render_gt(false_positives_table)
  output$pc1graph <- renderPlot(pc1graph)
  output$pc2graph <- renderPlot(pc2graph)
  output$kmeans_graph <- renderPlot(kmeans_graph)
  dat_titles <- reactive({
    pca_titles %>%
      filter(artist %in% input$artist)
    })
  output$pca_titles <- renderPlot({
    ggplot(data = dat_titles(), mapping = aes(x = PC1, y = PC2, color = artist)) +
      geom_text_repel(aes(label = album_name)) + 
      labs(title = "Principle Component Map by Artist",
           x = "Production: Simple va. Embellished",
           y = "Rhythm vs. Energy") +
      theme(legend.position = "bottom")
  })
  output$kmeans_table <- render_gt(kmeans_table)
}

# Creates app
shinyApp(ui = ui, server = server)
