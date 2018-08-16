
# Classification of Web page content is vital to many tasks in Web information retrieval such as maintaining Web 
# directories and focused crawling. The uncontrolled nature of Web content presents additional challenges to Web page 
# classification as compared to traditional text classification, however the interconnected nature of hypertext 
# also provides features that can assist the process.
# 
# Here the task is to classify the web pages to the respective classes it belongs to, in a single label classification 
# setup (Each webpage can belong to only 1 class).
# 
# Basically given the complete html and url, predict the tag a web page belongs to out of 9 predefined tags as given below:
#   
# 1) People profile
# 2) Conferences/Congress
# 3) Forums
# 4) News article
# 5) Clinical trials
# 6) Publication
# 7) Thesis
# 8) Guidelines
# 9) Others 

# ----------------------------------------------------------------------------------------
### 1: Get libraries, Read data and Process
# ----------------------------------------------------------------------------------------

# Libraries Used
library(data.table)
library(ggplot2)
library(tm)
library(dplyr)
library(stringr)
library(tidytext)

data_path <- 'C:/Kamal/Work/19_AV/Innoplexus_Online_Hackathon/Data/'
clean = ''
list.files(data_path)

#dat_html_data <- read.csv(paste0(data_path, 'html_data',clean,'.csv'), stringsAsFactors = F)
dat_train <- read.csv(paste0(data_path, 'train',clean,'.csv'), stringsAsFactors = F)
table(dat_train$Tag)

dat_test <- read.csv(paste0(data_path, 'test',clean,'.csv'), stringsAsFactors = F)
#Split cells with commas in them
split2 <- strsplit(dat_test$Webpage_id.Domain.Url, split = "\\,")

# get the maximal length of the vectors in split2
ncol_new <- max(sapply(split2, length))
# set the length of all of those elements to the maximal lenght
# this also fills them with NA values
split3 <- lapply(split2, `length<-`, ncol_new)
# transform the list of vectors to a matrix
new_cols <- matrix(unlist(split3), byrow=TRUE, ncol=ncol_new)
# set new colnames
colnames(new_cols) <- paste0("newCol", 1:ncol_new)
# and add the matrix to the dataframe as new columns
df <- cbind(df, new_cols)
df <- as.data.frame(df)
dat_test <- df %>% select(newCol1,newCol2,newCol3) %>% rename(Webpage_id = newCol1,Domain = newCol2,Url = newCol3)
rm(df,split2,split3,new_cols)

dat_test$Tag <- NA
dat_test$Tag <- as.character(dat_test$Tag)

print(sum(duplicated(dat_train$Webpage_id)))

# Get the TRAIN dataset
dat_all <- dat_train
#rm(dat_train,dat_test)

# duplicate id values across categories, concat id to category and we're good
print(sum(duplicated(dat_all$Webpage_id)))

# ----------------------------------------------------------------------------------------
### 2: Declaring custom functions
# ----------------------------------------------------------------------------------------

# removes all html tags
remove_html_tags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# custom tokenizer
custom_tokenizer <- function(param_big_string) {
  #' lower-cases, removes punctuation, new line and return characters, 
  #' and removes unnecessary whitespace, then strsplits 
  split_content <- sapply(param_big_string, removePunctuation, preserve_intra_word_dashes=T)
  split_content <- sapply(split_content, function(y) gsub("[\r\n]", " ", y))
  split_content <- sapply(split_content, tolower)
  split_content <- sapply(split_content, function(y) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", y, perl=TRUE))
  return(split_content)
  #return(split_content <- (sapply(split_content, strsplit, " ")))
}


# ----------------------------------------------------------------------------------------
### 3: Clean and Process data
# ----------------------------------------------------------------------------------------

# quick clean on content column
dat_all$Url <- remove_html_tags(dat_all$Url)

# tokenize
dat_all$Domain <- custom_tokenizer(dat_all$Domain)
dat_all$Url <- custom_tokenizer(dat_all$Url)
dat_all$Tag <- custom_tokenizer(dat_all$Tag)
invisible(gc())

# ----------------------------------------------------------------------------------------
### 4: Text mining
# ----------------------------------------------------------------------------------------

original_books <- data_frame(line = 1:length(dat_all$Url), text = dat_all$Url, category = dat_all$Tag)

stack_words <- original_books %>%
  unnest_tokens(word, text) %>%
  count(category, word, sort = TRUE) %>%
  ungroup()

stack_words <- stack_words %>%
  bind_tf_idf(word, category, n) 

stack_words

plot_stack <- stack_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(category = factor(category, levels = c("clinicalTrials","conferences",
                                                "forum","guidelines","news",
                                                "others","profile","publication","thesis")))

ggplot(plot_stack[1:20,], aes(word, tf_idf, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

# TF-IDf on content / Url

plot_stack <- plot_stack %>% 
  group_by(category) %>% 
  top_n(15, tf_idf) %>% 
  mutate(word = reorder(word, tf_idf))

ggplot(plot_stack, aes(word, tf_idf, fill = category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~category, ncol = 2, scales = "free") +
  ggtitle("TF-IDF on content") + 
  coord_flip()

# We will analyze based on Tag/category and Domain

original_books <- data_frame(line = 1:length(dat_all$Url), text = dat_all$Url, category = dat_all$Tag)
original_books <- data_frame(line = 1:length(dat_all$content), text = dat_all$title, category = dat_all$category)

stack_words <- original_books %>%
  unnest_tokens(word, text) %>%
  count(category, word, sort = TRUE) %>%
  ungroup()

stack_words <- stack_words %>%
  bind_tf_idf(word, category, n) 

plot_stack <- stack_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(category = factor(category, levels = c("clinicalTrials","conferences",
                                                "forum","guidelines","news",
                                                "others","profile","publication","thesis")))

ggplot(plot_stack[1:20,], aes(word, tf_idf, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

