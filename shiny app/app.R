library(ggplot2)
library(viridis)
library(hrbrthemes)
library(shiny)
library(plotly)
library(dplyr)
library(ggExtra)
library(readr)

data <-read.csv("C:/Users/karol/OneDrive/Pulpit/semest 1/prezentacja i wizualizacja baz danych/projekt5/data/books_data.csv")
# Bezpośredni link do pliku CSV
#direct_link <- "https://drive.google.com/uc?export=download&id=137w93r_VPASNUj0DLvOxQfoI2Len43Tk"

# Wczytanie danych z linku bezpośredniego
#data <- read.csv(url(direct_link))

# Wyświetlenie wczytanych danych
head(data)  # Wyświetlenie pierwszych kilku wierszy danych
ui <- fluidPage(
  titlePanel("Filtrowanie danych"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tab_selector", "Wybierz zakładkę",
                  choices = c("Znajdź publikację", "Wydawnictwa", "Kategorie")),
      uiOutput("specific_sidebar")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Znajdź publikację",
                 fluidRow(
                   column(width = 6, plotlyOutput("plot_tab1"))
                 )
        ),
        
        tabPanel("Wydawnictwa",
                 fluidRow(
                   column(width = 6, plotOutput("plot_tab2")),
                 ),
                 fluidRow(
                   column(width = 6, plotOutput("area_plot_tab2"),  style = "margin-top: 250px;")
                 )
        ),
        
        tabPanel("Kategorie",
                 fluidRow(
                   column(width = 6, plotOutput("plot_tab3")),  
                   column(width = 6, plotOutput("plot_tab3_extra"))
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #filtrowanie  po tematyce
  filtered_data_1 <- reactive({
    filtered <- data  
    
    if (!is.null(input$Theme) && input$Theme != "All") {
      filtered <- filtered %>%
        filter(Theme == input$Theme)
    }
    
    if (!is.null(input$Pisarz) && input$Pisarz != "" && input$Pisarz != "Wprowadź imię i nazwisko") {
      filtered <- filtered %>%
        filter(grepl(input$Pisarz, Authors))
    }
    
    if (!is.null(input$Tytuł) && input$Tytuł != "" && input$Tytuł != "Wprowadź tytuł") {
      filtered <- filtered %>%
        filter(grepl(input$Tytuł, Title))
    }
    
    filtered
  })
  
  output$plot_tab1 <- renderPlotly({
    p <- filtered_data_1() %>%
      mutate(text = paste("Tytuł: ", Title, "\nAutor: ", Authors , "\nWydawnictwo: ", Publisher, sep="")) %>%
      ggplot(aes(x = Ratings_Count, y = Average_Rating, size = Text_Reviews_Count, color = Publisher, text = text)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(1.4, 19), name = "Population (M)") +
      scale_color_viridis(discrete = TRUE, guide = FALSE) +
      theme(legend.position = "none") +
      scale_y_continuous(breaks = seq(0, 5, by = 0.5), labels = scales::comma_format()) +
      scale_x_continuous(labels = scales::comma_format())
    gg <- ggplotly(p, tooltip = "text")
    gg %>% layout(height = 600, width = 600)
  })
  
  filtered_data <- reactive({
    # Zliczenie książek dla każdego wydawnictwa
    book_counts <- data %>%
      count(Publisher, name = "total_books")
    
    # Filtrowanie wydawnictw, które wydały więcej niż 50 książek
    sum_books_filtered <- book_counts %>%
      filter(total_books > 50)
    
  
    data %>%
      filter(Publisher %in% sum_books_filtered$Publisher)
    
  })
  
  filtered_data_2 <- reactive({
    filtered <- filtered_data()  
    
    # Filtracja danych po dacie publikacji
    filtered <- filtered %>%
      filter(Publication_Date >= input$Year[1] & Publication_Date <= input$Year[2])
    
    filtered  
  })
  
  
  output$plot_tab2 <- renderPlot({
    data_filtered <- filtered_data_2()  
    
    ggplot(data_filtered, aes(x = Publisher, fill = Theme)) +
      geom_bar() +
      scale_fill_viridis(discrete = TRUE) +
      ggtitle("Liczba wydanych książek według wydawnictwa i tematyki") +
      theme_minimal() +
      xlab("Wydawnictwo") +
      ylab("Liczba wydanych książek") +
      guides(fill = guide_legend(ncol = 1))
  }, height = 600, width = 1000)
  
  output$area_plot_tab2 <- renderPlot({
    data_filtered <- filtered_data_2()
    data_filtered <- data_filtered %>%
      group_by(Publication_Date, Theme) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    ggplot(data_filtered, aes(x = Publication_Date, y = Count, fill = Theme)) + 
      geom_area() +
      scale_fill_viridis(discrete = TRUE) +
      ggtitle("Liczba wydanych książek na przestrzeni czasu")+
      theme_minimal() +
      xlab("Data Publikacji") +
      ylab("Liczba wydanych książek")+
      guides(fill = guide_legend(ncol = 1))
  }, height = 600, width = 1000)
  
  
  filtred_data_3 <- reactive({
    filtered <- filtered_data()
    
    if (!is.null(input$Theme_2) && "All" %in% input$Theme_2) {
      # If "All" is selected, retain all data
      data_to_plot <- filtered
    } else if (!is.null(input$Theme_2)) {
      # Filter based on selected themes
      data_to_plot <- filtered %>%
        filter(Theme %in% input$Theme_2)
    }
  
  })
  
  output$plot_tab3 <- renderPlot({
    data_to_plot <- filtred_data_3()
    
    ggplot(data_to_plot, aes(x = Theme, y = Average_Rating, fill = Theme)) +
      geom_boxplot() +
      labs(x = "Theme", y = "Average_Rating", fill = "Theme") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis(discrete = TRUE, guide = FALSE) +
      ggtitle("Średnia ocena w podziale na tematykę książki")
  }, height = 500, width = 500)
  
  output$plot_tab3_extra <- renderPlot({
    data_to_plot <-  filtred_data_3()
    
    # Obliczanie średniej i odchylenia standardowego
    mean_num_pages <- mean(data$Num_Pages)
    sd_num_pages <- sd(data$Num_Pages)
    
    # granice
    lower_bound <- mean_num_pages - 3 * sd_num_pages
    upper_bound <- mean_num_pages + 3 * sd_num_pages
    
    # Filtrowanie danych poza granicami
    filtered_data_no_outliers <- data_to_plot %>%
      filter(Num_Pages >= lower_bound & Num_Pages <= upper_bound)
    
    ggplot(filtered_data_no_outliers, aes(x = Theme, y = Num_Pages, fill = Theme)) +
      geom_boxplot() +
      labs(x = "Theme", y = "Num_Pages", fill = "Theme") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis(discrete = TRUE, guide = FALSE) +
      ggtitle("Liczba stron w podziale na tematykę książki")
  }, height = 500, width = 500)

  
  # Dynamicznie generowane elementy interfejsu dla sidebarPanel
  output$specific_sidebar <- renderUI({tab_choice <- input$tab_selector
  
  if (tab_choice == "Znajdź publikację") {
    # Elementy interfejsu dla zakładki 1
    tagList(
      selectInput("Theme", label =  "Wybierz tematykę książki",
                  choices =  c("All","Young Adult and Children Fiction", "Fiction", "Arts & Culture", "Biography & Autobiography",
                               "Social Sciences & Philosophy", "Health, Personal Development & Relationships",
                               "Technology, Science & Animals", "Education, Business & Economics",
                               "Religious & Spiritual", "History & Historical Fiction", "Other",
                               "International Literature & Poetry", "Fantasy & Science Fiction", "Travel & Places",
                               "Mystery & Detective" ), selected = "All"),
      textInput("Pisarz", label = h3("Wpisz imię pisarza"), value = "Wprowadź imię i nazwisko"),
      textInput("Tytuł", label = h3("Wpisz Tytuł książki"), value = "Wprowadź tytuł")
    )
  }  else if (tab_choice == "Wydawnictwa") {
      # Elementy interfejsu dla zakładki 2
      sliderInput("Year", label = "Data publikacji", min= 1913, max = 2024, value = c(1913, 2024))
    
    } else if (tab_choice == "Kategorie") {
      # Elementy interfejsu dla zakładki 3
      checkboxGroupInput("Theme_2", 
                         h3("Wybierz Tematykę"), 
                         choices = list("All" = "All", 
                                        "Young Adult and Children Fiction" = "Young Adult and Children Fiction",
                                        "Fiction" = "Fiction",
                                        "Arts & Culture" = "Arts & Culture",
                                        "Biography & Autobiography" = "Biography & Autobiography",
                                        "Social Sciences & Philosophy" = "Social Sciences & Philosophy",
                                        "Health, Personal Development & Relationships" = "Health, Personal Development & Relationships",
                                        "Religious & Spiritual" = "Religious & Spiritual",
                                        "History & Historical Fiction" = "History & Historical Fiction",
                                        "Other" = "Other",
                                        "International Literature & Poetry" = "International Literature & Poetry",
                                        "Fantasy & Science Fiction" = "Fantasy & Science Fiction",
                                        "Travel & Places" = "Travel & Places",
                                        "Mystery & Detective" = "Mystery & Detective",
                                        "Education, Business & Economics" = "Education, Business & Economics",
                                        "Technology, Science & Animals" = "Technology, Science & Animals"
                         ), selected = "All")  
    }
  })
}
shinyApp(ui = ui, server = server)

