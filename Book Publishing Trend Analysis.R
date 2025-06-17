# Book Publishing Trend Analysis

library(janitor)
library(tidyverse)

books <- read_csv("books.csv")
books <- clean_names(books)

library(lubridate)

books$first_publish_date <- mdy(books$first_publish_date)

books$year <- year(books$first_publish_date)

books <- books %>%
  filter(year >= 1990, year <= 2020)

books <- books %>%
  select(-publish_date, -edition, -characters, -price, -genres, -setting, -isbn)

books <- books %>%
  filter(pages < 700)

books <- na.omit(books)

glimpse(books)

summary(books)

ggplot(books, aes(x = rating)) +
  geom_histogram(binwidth = 0.25, fill = "red") +
  labs(title = "Histogram of Book Ratings", x = "Rating", y = "Number of Books")

boxplot(books$pages,
        main = "Box Plot of Page Counts",
        xlab = "Pages",
        col = "red",
        horizontal = TRUE)

by_year <- books %>%
  group_by(year) %>%
  summarize(total_books = n())

ggplot(by_year, aes(x = year, y = total_books)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Number of Books Rated Per Year", x = "Year", y = "Total Books")

book_publisher <- books %>%
  group_by(publisher) %>%
  summarize(book_count = n())

book_publisher <- book_publisher %>%
  filter(book_count >= 125)

book_publisher <- book_publisher %>%
  arrange(desc(book_count))

book_publisher$cum_counts <- cumsum(book_publisher$book_count)

book_publisher$rel_freq <- book_publisher$book_count / sum(book_publisher$book_count)

book_publisher$cum_freq <- cumsum(book_publisher$rel_freq)

book_publisher$publisher <- factor(book_publisher$publisher, levels = book_publisher$publisher)

ggplot(book_publisher, aes(x = publisher, y = book_count)) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_point(aes(y = cum_counts), color = "black") +
  geom_line(aes(y = cum_counts, group = 1), color = "black") +
  labs(title = "Book Counts (1990 - 2020)", x = "Publisher", y = "Number of Books") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

books %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(x = year, y = avg_rating)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Book Rating Per Year", x = "Year", y = "Average Rating")

testthat::test_file("project3_tests.R")
