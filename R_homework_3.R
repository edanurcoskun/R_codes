library(tidyverse)


# 1.	Write a function that finds a prime number(s) given a set of numbers.
#     For example you have a vector of numbers like (89, 107, 597, 931, 1083).
#     The function you will write should return a logical vector depending on
#     the given number is prime or not.


# 1st way...

set.seed(2016555019)

(set_of_num <- sample(c(2:100), size = 7))

for (i in seq_along(set_of_num)) {
  sorted_form <- set_of_num[i]
  test <- sum(sorted_form %% 2:(sorted_form - 1) == 0)
  if (test > 0) {
    set_of_num[i] = FALSE
  } else {
    set_of_num[i] = TRUE
  }
}

as.logical(set_of_num)



# 2nd way...

# Because of I was not sure whether 'as.logical() 'method was used in our
# classes, I wrote in another way without using this method.

set.seed(2016555019)

(set_of_num <- sample(c(2:100), size = 7))

is_prime <- function(x) {
  refill <- x
  for (i in seq_along(x)) {
    num <- x[i]
    test <- sum(num %% 2:(num - 1) == 0)
    if (test > 0) {
      refill[i] = 'FALSE'
    } else {
      refill[i] = 'TRUE'
    }
  }
  refill
}

is_prime(set_of_num)




# 2.	Write a function that finds the letter numbers of all words in a given
#     text and sorts the text according to those numbers from words with few
#     letters to words with many letters. Sort the words containing the same
#     number of letters alphabetically.



set.seed(2016555019)

(new_data <- (a = sample(sentences, 6)))

# for combine all sentences together and separate into words 
(separate_form <- (tibble
                   (str_c(new_data, collapse = " ")) %>%
                     str_split(boundary("word"))) %>%
    .[[1]])

# for calculating lengths of each word
(find_length <- tibble(words = separate_form,
                       lengths = (str_length(separate_form))))

# default variables to use in loop
temp_vector <- c()
count <- 1

# first find equal-length-words
# later sort alphabetically
# lastly add each found vector into the default vector to collect all in one 

while (count <= max(find_length$lengths)) {
  part_of_vector <- find_length %>%
    group_by(words) %>%
    summarize(which(lengths == count), .groups = 'drop')
  # I know '.groups' method was not used in class but when I do not write it, 
  # a warning occurs that does not affect to result of algorithm
  
  str_sort(part_of_vector$words, locale = "en")
  temp_vector <- rbind(temp_vector, part_of_vector)
  count <- count + 1
}

#for combine all words together
(sorted_form <- (str_c(temp_vector$words, collapse = " ")))

# for convert to lowercase each words
(lower_form <- lapply(sorted_form, tolower) %>%
    .[[1]])



