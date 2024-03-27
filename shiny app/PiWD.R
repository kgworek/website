

# library

library(ggplot2)
library(viridis)
library(hrbrthemes)
library(shiny)
library(plotly)
library(dplyr)
library(ggExtra)



#przygotowanie danych

data <-read.csv("C:/Users/karol/OneDrive/Pulpit/semest 1/prezentacja i wizualizacja baz danych/proba2/data/books_data.csv")

data_ <- read.csv("C:/Users/karol/OneDrive/Pulpit/semest 1/prezentacja i wizualizacja baz danych/books/data/books_.csv")

data_1 <- read.csv("C:/Users/karol/OneDrive/Pulpit/semest 1/prezentacja i wizualizacja baz danych/books/data/books.csv")

merged_data <- merge(data_, data_1, by = "isbn13")

#wybór kolumn
selected_columns <- c(
  "title.x", "authors.x", "average_rating.x", "ratings_count.x",
  "language_code", "num_pages.x", "publication_date", "publisher", 
  "categories", "text_reviews_count"
)

data <- merged_data[, selected_columns]

#nowe nazwy kolumn
names <- c(
  "Title", "Authors", "Average_Rating", "Ratings_Count",
  "Language_Code", "Num_Pages", "Publication_Date", "Publisher",
  "Categories", "Text_Reviews_Count"
)

#nowe nazwy kolumn do ramki danych
colnames(data) <- names


#tylko rok z daty

data$Publication_Date <- substr(data$Publication_Date, nchar(data$Publication_Date) - 3, nchar(data$Publication_Date))


#dodanie kolumny 
data$Theme <-  ifelse(data$Categories %in% c("Actors and actresses", "Antiques & Collectibles", "Apartment houses", "Architecture", "Art", "Art museum curators", "Artists", "Ballet", "Ballet dancers", "Ballets", "Bracelets", "Candy", "Cider house rules. (Motion picture)", "Comedy", "Crafts & Hobbies", "Design", "Drama", "Electronic books", "English drama", "Feature films [DVD]", "Film producers and directors", "Motion picture plays", "Music", "Performing Arts", "Photographers", "Photography", "Psycho (Motion picture : 1960)", "Rock groups", "Rock musicians", "Television"), "Arts & Culture",
                                      ifelse(data$Categories %in% c("Amis, Kingsley", "Authors", "Authors, American", "Authors, Cuban", "Authors, English", "Authors, German", "Authors, Italian", "Political leadership", "Vice-Presidents", "Belgians","American", "Biography & Autobiography", "BIOGRAPHY & AUTOBIOGRAPHY", "Blacks", "British", "Brothers", "Butlers", "Cellists", "Explorers", "Motion picture actors and actresses", "Novelists, English"), "Biography & Autobiography",
                                             ifelse(data$Categories %in% c("Adolescence", "Adult education", "Allegories", "Arithmetic", "Boarding school - fiction", "Boarding schools", "Books", "Civil law", "College attendance", "College teachers", "Education", "Englisch - Geschichte - Lyrik - Aufsatzsammlung", "English", "English language", "Foreign Language Study", "High schools", "Language Arts & Disciplines", "Large print books", "Law", "Motion pictures", "Reference", "Study Aids", "Apprentices", "Banks and banking", "British", "Brewing", "Building laws", "Business & Economics", "Business enterprises", "Businesswomen",  "Political science", "Political Science", "Capitalism", "Construction workers", "Consumer behavior", "Finance, Personal", "Music trade"), "Education, Business & Economics",
                                                                  ifelse(data$Categories %in% c("87th Precinct (Imaginary place)", "Adventure fiction", "Adventure stories", "Aeneas (Legendary character)", "Alanna (Fictitious character : Pierce)", "Alcestis (Greek mythology)", "Animals, Mythical", "Arthurian romances", "Assassins", "Baggins, Bilbo (Fictitious character)", "Baggins, Frodo (Fictitious character)", "Beowulf", "Beresford, Tommy (Fictitious character)", "Blake, Anita (Fictitious character)", "Bond, James (Fictitious character)", "Bumppo, Natty (Fictitious character)", "Cabrillo, Juan (Fictitious character)", "Castle Rock (Me. : Imaginary place)", "Continental Op (Fictitious character)", "Dallas, Eve (Fictitious character)", "Death (Fictitious character : Gaiman)", "Dent, Arthur (Fictitious character)", "Discworld (Imaginary place)", "Dracula, Count (Fictitious character)", "Dragons", "Drenai (Imaginary place)", "Dystopias", "Erinyes (Greek mythology)", "Adventure stories", "Fairy tales", "Fairy tales, English", "Fantasy", "Fantasy fiction", "American fiction", "Experimental fiction", "Fantasy fiction, American", "Fantasy fiction, English", "Fantasy.", "Ghost stories", "Heroes", "Holt, Max (Fictitious character)", "Horror stories.", "Horror tales", "Horror tales, American", "Horror tales, English", "Imaginary wars and battles", "Interplanetary voyages", "Life on other planets", "Magic", "Occult fiction", "Science fiction", "Science fiction, American", "Science fiction, English", "Science fiction, English.", "Star trek fiction"), "Fantasy & Science Fiction",  
                                                                         ifelse(data$Categories %in% c ("Indic fiction (English)", "Love poetry",  "Poetry",  "Poets, American", "American literature", "Polish poetry",  "American literature"  ),"International Literature & Poetry",
                                                                                ifelse(data$Categories %in% c("Bail bond agents", "Bombings", "Crime investigation", "Crime investigations", "Detective and mystery stories", "Detective and mystery stories, American", "Detective and mystery stories, English", "Espionage", "Insane, Criminal and dangerous", "Organized Crime", "True Crime"), "Mystery & Detective",
                                                                                              ifelse(data$Categories %in% c("Comic books, strips, etc", "Comics & Graphic Novels", "Contests", "Games", "Games & Activities",  "Humorous stories", "Amnesty", "Antisemitism"), "Other",
                                                                                                     ifelse(data$Categories %in% c("Albigenses", "Angels", "Atonement", "Bible", "Bibles", "Buddhism", "Catholic women", "Catholics", "Christian life", "Christian saints", "Christianity", "Christmas", "Christmas stories", "Church work with the poor", "Clergy", "Confucianism", "Demonology", "Exorcism", "God (Christianity)", "Good and evil", "Mythology, Classical", "Religion", "Theology, Doctrinal"), "Religious & Spiritual",
                                                                                                                   ifelse(data$Categories %in% c("Anger", "Body, Mind & Spirit", "Conduct of life", "Confianza en sí mismo", "Dreams", "Meditation", "Reducing diets", "Self-Help", "Spiritual life", "Sports & Recreation", "Accidents", "Alcoholics", "Alzheimer's disease", "Amnesia", "Autism", "Bereavement", "Blind", "Cancer", "Cookbooks", "Cooking", "Cooking, French", "Dangerously mentally ill", "Epidemics", "Health & Fitness", "Intellectual disability facilities", "Mental illness", "Minimal brain dysfunction in children", "Obesity", "People with social disabilities", "Families", "Family & Relationships", "Family life", "Female friendship", "Friendship", "Friendship in adolescence", "House & Home", "Man-woman relationships", "Married people", "Sex"), "Health, Personal Development & Relationships",
                                                                                                                          ifelse(data$Categories %in% c("Abused wives", "Adjustment (Psychology)", "Adultery", "African American families", "African American men", "African Americans", "Aged women", "Alienation (Social psychology)", "Americans", "Boys", "Bullying", "Children of the rich", "City and town life", "City girl", "Cocaine industry", "Country life", "Curiosities and wonders", "Dating (Social customs)", "Dead", "Death", "Divorce", "Divorced women", "Dysfunctional families", "Empiricism", "Essentialism (Philosophy)", "Existential psychotherapy", "Existentialism", "Gay men", "Girls", "Human behavior", "Identity (Psychology)", "Igbo (African people)", "Jews", "Men", "Misogyny", "Mothers and sons", "Philosophy", "Philosophy, Ancient", "Psychology", "Social Science"), "Social Sciences & Philosophy",
                                                                                                                                 ifelse(data$Categories %in% c("Aeronautics", "Astronomers", "C (Computer program language)", "Computer programmers", "Computers", "Cosmology", "Human cloning", "Inventions","Labrador retriever", "Mars (Planet)", "Mathematicians", "Mathematics", "Medical", "Pets", "Physicists", "Pigeons", "Science", "Zero (The number)", "Zoology", "Technology & Engineering", "Agriculture", "Animal sanctuaries", "Animal sounds", "Animals", "Badgers", "Caterpillars", "Gardening", "Gardens", "Heat", "Human - alien encounters", "Human - animal relationships", "Nature"), "Technology, Science & Animals",
                                                                                                                                        ifelse(data$Categories %in% c("Adirondack Mountains (N.Y.)", "Africa, East", "Air pilots", "Arctic regions", "Australia", "Azerbaijan", "Boats and boating", "Boston (Mass.)", "Botswana", "Cambridge (Mass.)", "Canada", "Canterbury (England)", "China", "Cities and towns", "Cornwall (England : County)", "Czech Republic", "Czechoslovakia", "Dublin (Ireland)", "Egypt", "England", "Eretz Israel", "Europe", "Everest, Mount (China and Nepal)", "France", "Germany", "Great Britain", "Hitchhiking", "Illinois", "Japan", "Latin America", "London (England)", "Los Angeles (Calif.)", "New York (State)", "Paris (France)", "Provence (France)", "Transportation", "Travel", "United States"), "Travel & Places",
                                                                                                                                               ifelse(data$Categories %in% c("Children's stories", "Children's stories, English", "Juvenile Fiction", "JUVENILE FICTION", "Juvenile Nonfiction", "Sweet Valley (Imaginary place)", "Teenagers", "Young Adult Fiction"), "Young Adult and Children Fiction", 
                                                                                                                                                      ifelse(data$Categories %in% c("Amour - Ouvrages avant 1800", "Antiheroes", "Chick lit", "Classical fiction", "Diary fiction", "Domestic fiction", "English essays", "English fiction", "Epic literature", "Essays", "Experimental fiction", "Fiction", "FICTION", "Graphic novels", "Humor", "Humorous fiction", "Humorous stories", "Humorous stories, English", "Lecter, Hannibal (Fictitious character)", "Literary Collections", "Literary Criticism", "LITERARY CRITICISM", "Literary Criticism & Collections", "Poirot, Hercule (Fictitious character)", "Popular literature", "Ryan, Jack (Fictitious chara)"), "Fiction",
                                                                                                                                                                    ifelse(data$Categories %in% c("African American plantation owners", "Auschwitz (Poland : Concentration camp)", "Book burning", "Bosnia and Hercegovina", "Bosnia and Herzegovina", "Children of Holocaust survivors", "Conspiracies", "Crusades", "Disasters", "Estados Unidos - Relaciones exteriores - 1945-1989", "Frontier and pioneer life", "Historical fiction", "History", "History, Modern", "Napoleonic Wars, 1800-1815", "Persian Gulf War, 1991", "Prisoners of war", "Slave insurrections", "Stone age", "Vietnam War, 1961-1975"), "History & Historical Fiction", "Other"
                                                                                                                                                                    )))))))))))))))





data$Ratings_Count <- as.numeric(data$Ratings_Count)
data$Average_Rating <- as.numeric(data$Average_Rating)
data$Text_Reviews_Count <- as.numeric(data$Text_Reviews_Count)
data$Num_Pages <- as.numeric(data$Num_Pages)
data$Publication_Date <- as.numeric(data$Publication_Date)
data$Theme <- factor(data$Theme)
data$Publisher <- factor(data$Publisher)


#usunięcie mniej popularnych wydań tych samych tytułów 
data <- data %>%
  arrange(desc(Ratings_Count)) %>%
  distinct(Title, .keep_all = TRUE)

books_data <- data %>%
  filter(Ratings_Count >= 10)



#zapisanie danych

write.csv(books_data, "books_data.csv", row.names = FALSE)

# Tworzenie wykresu słupkowego
barplot(table(data$Average_Rating), main="Wykres słupkowy zmiennej średnia ocen", xlab="Średnia Ocen", ylab="Liczebność",  col = "skyblue", border = "darkblue")
boxplot(table(data$Average_Rating), main="Wykres słupkowy zmiennej średnia ocen", xlab="Średnia Ocen", ylab="Liczebność",  col = "skyblue", border = "darkblue")



# Tworzenie wykresu słupkowego
barplot(table(data$Ratings_Count), main="Wykres słupkowy zmiennej średnia ocen", xlab="Średnia Ocen", ylab="Liczebność",  col = "skyblue", border = "skyblue")
barplot(table(data$Text_Reviews_Count), main="Wykres słupkowy zmiennej średnia ocen", xlab="Średnia Ocen", ylab="Liczebność",  col = "skyblue", border = "skyblue")



# Obliczenie szerokości słupków na potrzeby wyświetlenia dwóch zestawów słupków obok siebie
bar_width <- 0.35

# Obliczenie pozycji słupków dla dwóch zestawów danych
position <- barplot(table(data$Ratings_Count), 
                    main = "Porównanie Ratings_Count i Text_Reviews_Count", 
                    xlab = "Liczba ocen/recenzji na GoodReads", ylab = "Liczebność w bazie danych", 
                    col = "skyblue", border = "skyblue",
                    xlim = c(0, max(length(table(data$Ratings_Count)), length(table(data$Text_Reviews_Count))) + 1))

# Dodanie słupków dla drugiego zestawu danych
barplot(table(data$Text_Reviews_Count), 
        add = TRUE, 
        col = "lightgrey", border = "lightgrey", 
        width = bar_width, 
        at = position + bar_width)

# Tworzenie legendy
legend("topright", 
       legend = c("Ratings_Count", "Text_Reviews_Count"), 
       fill = c("skyblue", "lightgrey"))



publisher_counts <- table(data$Publisher)

# Wybranie 5 najczęściej występujących wydawnictw
top_publishers <- names(head(sort(publisher_counts, decreasing = TRUE), 10))

# Wyświetlenie 5 najczęściej występujących wydawnictw
print(top_publishers)

# Filtracja danych tylko dla top_publishers
filtered_data <- subset(data, Publisher %in% top_publishers)

# Tworzenie wykresu słupkowego dla top_publishers
ggplot(data = filtered_data, aes(x = Publisher)) +
  geom_bar() +
  labs(x = "Wydawnictwo", y = "Liczba książek", title = "Liczba książek według wydawnictwa", col="lightblue")

unique_count <- length(unique(data$Publisher))
unique_count



# Tworzenie wykresu punktowego
ggplot(data = data, aes(x = Ratings_Count, y = Average_Rating, size = Text_Reviews_Count)) +
  geom_point(alpha = 0.6) +  # Ustawienie przezroczystości punktów
  labs(x = "Liczba ocen", y = "Średnia ocena", title = "Zależność między liczbą ocen a średnią oceną") +
  scale_size_continuous(range = c(3, 10), name = "Liczba recenzji tekstu")

# Interaktywny
p <- data %>%
  mutate(text = paste("Tytuł: ", Title, "\nAutor: ", Authors, sep="")) %>%  # Adjust tooltip text as needed
  
  # Classic ggplot
  ggplot(aes(x = Ratings_Count, y = Average_Rating, size = Text_Reviews_Count, color = Publisher, text = text)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1.4, 19), name = "Population (M)") +
  scale_color_viridis(discrete = TRUE, guide = FALSE) +
  theme() + 
  theme(legend.position = "none") +  
  scale_y_continuous(breaks = seq(0, 5, by = 0.5), labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::comma_format())

  #  ggplot --> plotly
  pp <- ggplotly(p, tooltip = "text")
  pp

#wykres wydawnictw 

# Obliczenie sumy wydanych książek dla każdego wydawnictwa
book_counts <- data %>%
  count(Publisher, name = "total_books")  # Zliczenie książek dla każdego wydawnictwa

# Wyfiltruj wydawnictwa, które wydały więcej niż 50 książek
sum_books_filtered <- book_counts %>%
  filter(total_books > 50)

# Wybierz tylko dane dla wydawnictw spełniających kryterium
data_filtered <- data %>%
  filter(Publisher %in% sum_books_filtered$Publisher)

# Tworzenie wykresu dla wydawnictw, które wydały więcej niż 50 książek
ggplot(data_filtered, aes(x = Publisher, fill = Theme)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Liczba wydanych książek według wydawnictwa i tematyki") +
  theme_minimal() +
  xlab("Wydawnictwo") +
  ylab("Liczba wydanych książek") +
  guides(fill = guide_legend(ncol = 1))


# Histogram i  Scatterplot
hist(data$Average_Rating,  main="Średnia Ocen"  , breaks=30 , col=rgb(0.3,0.5,1,0.4) , xlab="Średnia Ocena" , ylab="Liczebność")
plot(data$Average_Rating ,data$Average_Rating ,  main="" , pch=20 , cex=0.4 , col=rgb(0.3,0.5,1,0.4)  , xlab="primadur" , ylab="Ixos" )

plot(data$Text_Reviews_Count, data$Retings_Count ,  main="" , pch=20 , cex=0.4 , col=rgb(0.3,0.5,1,0.4)  , xlab="Średnia Ocena" , ylab="Liczebność" )

#na przestrzeni lat
data_year <- data %>%
  group_by(Publication_Date, Theme) %>%
  summarise(Count = n()) %>%
  ungroup()

#tematyka a data publikacji
ggplot(data_year, aes(x = Publication_Date, y = Count, fill = Theme)) + 
  geom_area()+
  scale_x_continuous(limits = c(1910, 2020), breaks = seq(1910, 2020, by = 10))+
  guides(fill = guide_legend(ncol = 1))

min(data$Publication_Date)

#liczba recenzji, liczba ocen 
data_year <- data %>%
  group_by(Publication_Date, Theme) %>%
  summarise(Count = n()) %>%
  ungroup()


#tematyka a data publikacji
ggplot(data_year, aes(x = Publication_Date, y = Count, fill = Theme)) + 
  geom_area()+
  scale_x_continuous(limits = c(1970, 2020), breaks = seq(1910, 2020, by = 10))+
   scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  guides(fill = guide_legend(ncol = 1))


# classic plot :
p <- ggplot(data, aes(x = Ratings_Count, y = Text_Reviews_Count, colour= "darkblue") +
  geom_point() +
  theme(legend.position = "none"))
  

# marginal density
p2 <- ggMarginal(p, type="density")

print(p2)

# marginal boxplot
p3 <- ggMarginal(p, type="boxplot")

print(p3)


# Ratings_Count, Text_Reviews_Count
p <- ggplot(data, aes(x = Ratings_Count, y = Text_Reviews_Count)) +
  geom_point(color = "skyblue") +
  theme(legend.position = "none") +
  labs(x = "Liczba ocen", y = "Liczba recenzji") +
  xlim(0, 250000) +  # Zakres dla osi x
  ylim(0, 10000)     # Zakres dla osi y

p2 <- ggMarginal(p, type = "histogram", fill = "darkblue", xparams = list(bins = 10))

print(p2)

# Pobranie tabeli wystąpień wydawców
publisher_counts <- table(data$Publisher)

# Wybór pięciu najczęściej występujących wydawców
top_publishers <- names(head(sort(publisher_counts, decreasing = TRUE), 5))
# Filtracja danych dla pięciu najczęściej występujących wydawców
filtered_data <- subset(data, Publisher %in% top_publishers)
# Tworzenie wykresu pudełkowego dla pięciu najczęściej występujących wydawców

ggplot(filtered_data, aes(x = Publisher, y = Num_Pages, fill = Publisher)) +
  geom_boxplot() +
  labs(x = "Publisher", y = "Number of Pages", fill = "Publisher") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Comparison of Number of Pages by Top 5 Publishers")


# Pobranie tabeli wystąpień tematów
theme_counts <- table(data$Theme)
# Wybór pięciu najczęściej występujących tematów
top_themes <- names(head(sort(theme_counts, decreasing = TRUE), 10))
# Filtracja danych dla pięciu najczęściej występujących tematów
filtered_data <- subset(data, Theme %in% top_themes)


# Obliczanie średniej i odchylenia standardowego
mean_num_pages <- mean(data$Num_Pages)
sd_num_pages <- sd(data$Num_Pages)

# Definiowanie granic zakresu
lower_bound <- mean_num_pages - 3 * sd_num_pages
upper_bound <- mean_num_pages + 3 * sd_num_pages

# Filtrowanie danych poza granicami 1-sigma
filtered_data_no_outliers <- data %>%
  filter(Num_Pages >= lower_bound & Num_Pages <= upper_bound)

# Tworzenie wykresu pudełkowego bez wartości odstających
ggplot(filtered_data_no_outliers, aes(x = Theme, y = Num_Pages, fill = Theme)) +
  geom_boxplot() +
  labs(x = "Theme", y = "Number of Pages", fill = "Theme") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  ggtitle("Comparison of Number of Pages by Top 5 Themes (No Outliers)")


# Filtracja danych dla pięciu najczęściej występujących tematów
filtered_data <- subset(data, Theme %in% top_themes)

# Obliczanie granic zakresu tylko dla pięciu najczęściej występujących tematów
mean_num_pages <- mean(filtered_data$Num_Pages)
sd_num_pages <- sd(filtered_data$Num_Pages)

# Definiowanie granic zakresu
lower_bound <- mean_num_pages - 3 * sd_num_pages
upper_bound <- mean_num_pages + 3 * sd_num_pages

# Filtrowanie danych poza granicami 1-sigma
filtered_data_no_outliers <- filtered_data %>%
  filter(Num_Pages >= lower_bound & Num_Pages <= upper_bound)

# Tworzenie wykresu pudełkowego bez wartości odstających
ggplot(filtered_data_no_outliers, aes(x = Theme, y = Num_Pages, fill = Theme)) +
  geom_boxplot() +
  labs(x = "Theme", y = "Number of Pages", fill = "Theme") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  ggtitle("Porównanie liczebności stron 10 głównych kategorii") +
  ylim(0, mean_num_pages + 3 * sd_num_pages)


ggplot(data = data, aes(x = Publication_Date, y = Average_Rating)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Data publikacji", y = "Średnia ocena") +
  geom_point()

ggplot(data = data, aes(x = Num_Pages, y = Average_Rating)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Liczba stron", y = "Średnia ocena") +
  geom_point()

ggplot(data = data, aes(x = Ratings_Count , y = Num_Pages)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Ratings_Count", y = "Num_Pages") +
  geom_point()


ggplot(data = data, aes(x = Text_Reviews_Count , y = Num_Pages)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Text_Reviews_Count", y = "Num_Pages") +
  geom_point()

# ltrens liniowy strony a średnia ocena 
ggplot(data, aes(x = Num_Pages, y = Average_Rating)) +
  geom_point(alpha = 0.5, color = "#2c3e50") +  
  geom_smooth(method = "lm", fill = "#69b3a2", se = TRUE) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  theme_ipsum()


# Pobranie tabeli wystąpień wydawców
theme_counts <- table(data$Theme)

# Wybór pięciu najczęściej występujących wydawców
top_theme  <- names(head(sort(theme_counts, decreasing = TRUE), 5))

# Filtracja danych dla pięciu najczęściej występujących wydawców
filtered_data <- subset(data, Theme %in% top_theme)

# Tworzenie wykresu pudełkowego dla pięciu najczęściej występujących wydawców

ggplot(filtered_data, aes(x = Theme, y = Average_Rating, fill = Theme)) +
  geom_boxplot() +
  labs(x = "Theme", y = "Average_Rating", fill = "Theme") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  ggtitle("Średnia ocena w podzale na tematykę ksiązki")

uniqueT <- unique(data$Theme)



data_2 <- data %>%
  mutate(Average_Rating = round(Average_Rating * 2) / 2)

data_2 <- data_2 %>%
  filter(abs(Ratings_Count - mean(Ratings_Count)) < 200 * sd(Ratings_Count))


ggplot(data_2, aes(x = Average_Rating, y = Ratings_Count)) +
  geom_point() +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red")


ggplot(data, aes(x = Average_Rating)) +
  geom_density() +
  labs(x = "Average_Rating", y = "Rozkład") +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~Theme, scales = "free", ncol = 2) +
  scale_x_continuous(breaks = seq(0, 5, by = 1))


selected_categories <- c("Fantasy & Science Fiction", "Biography & Autobiography", "Arts & Culture", "Fiction" )

# Filtruj dane tylko dla wybranych kategorii
filtered_data <- data %>%
  filter(Theme %in% selected_categories)



ggplot(filtered_data, aes(x = Average_Rating, fill = Theme)) +
  geom_histogram(position = "identity", alpha = 1, bins = 50) +
  labs(x = "Średnia ocena", y = "Liczba występowań danej oceny") +
  facet_wrap(~ Theme, scales = "free") +
  theme_minimal()+
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  scale_x_continuous(limits = c(2.5, 5), breaks = seq(2.5, 5, by = 0.5))


selected_categories <- c("Biography & Autobiography", "Young Adult and Children Fiction", "Fiction", "Other", "International Literature & Poetry" )

# Filtruj dane tylko dla wybranych kategorii
filtered_data <- data %>%
  filter(Theme %in% selected_categories)
ggplot(filtered_data) +
  geom_point(aes(x = Ratings_Count, y = Average_Rating)) +
  geom_smooth(aes(x = Ratings_Count, y = Average_Rating, group = Theme), method = "lm", se = FALSE) +
  labs(y = "Ocena (rating)") +
  scale_x_continuous(breaks = seq(min(filtered_data$Ratings_Count), 1000, by = 5000)) +
  facet_wrap(~ Theme, nrow = 1) +
  xlab("Ratings_Count")


theme_1 <- unique(books_data$Theme)
print(theme_1)
