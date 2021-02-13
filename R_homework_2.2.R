library(tidyverse)

#	1.Take a sample of 100 words from this data, and save this sample 
#   as a new data.

set.seed(2016555019) 
new_data <- sample(words, 100)

new_data <- tibble(vocab = new_data)
view(new_data)




# 2. Find words which are starting with “a” and ending with “e”.

with_a = str_subset(new_data, "^a")

(a_and_e <- tibble(a_e = str_subset(with_a, "e$")))




#	3. Calculate the number of words which have more than 3 vowels.

num_more_three <- sum(str_count(new_data, "(a|e|i|o|u)") > 3)

logic_more_three <- tibble(
  words = new_data,
  logical = str_count(new_data, "(a|e|i|o|u)") > 3
)

more_three <- logic_more_three %>%
  group_by(words) %>%
  summarize(which(logical == TRUE))

all_words <- str_c(more_three[1] %>%
                     .[[1]], collapse = ", ")

#	4. List the five longest word in your data.

longest <- tibble(words = new_data,
                  lengths = (str_length(new_data)))

(longest_5 <- longest %>%
    arrange(desc(lengths)) %>%
    head(5))




#	5. Try to find word(s) which contain any of these words:
#    age, any, day, exp, her, pro, the.

(containing_words <- tibble(
  contains = str_subset(new_data, "(age|any|day|exp|her|pro|the)")
))











