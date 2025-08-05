# ----
# Set up code

rm( list = ls() )
setwd( "/Users/jyoung20/GitHub/TAWR2" )
text_v <- scan( "data/text/melville.txt", what = "character", sep = "\n" )
start_v <- which( text_v == "CHAPTER 1. Loomings." )
novel_lines_v <- text_v[start_v : length( text_v )]
chap_positions_v <- grep( "^CHAPTER \\d", novel_lines_v )
last_positions_v <- length( novel_lines_v )
chap_positions_v <- c( chap_positions_v, last_positions_v )

# this is the chapters
chapter_raws_l <- list()

#
chapter_freqs_l <- list()
for( i in 1:length( chap_positions_v ) ){
  if( i != length( chap_positions_v ) ){
    chapter_title <- novel_lines_v[chap_positions_v[i]]
    start <- chap_positions_v[i] + 1
    end <- chap_positions_v[i + 1] - 1
    chapter_lines_v <- novel_lines_v[start:end]
    chapter_words_v <- tolower( paste( chapter_lines_v, collapse = " " ) )
    chapter_words_l <- strsplit( chapter_words_v, "\\W" )
    chapter_word_v <- unlist( chapter_words_l )
    chapter_word_v <- chapter_word_v[which( chapter_word_v != "" )]
    chapter_freqs_t <- table( chapter_word_v )
    chapter_raws_l[[chapter_title]] <- chapter_freqs_t
    chapter_freqs_t_rel <- 100 * ( chapter_freqs_t / sum( chapter_freqs_t ) )
    chapter_freqs_l[[chapter_title]] <- chapter_freqs_t_rel
  }
}


# ----
# Chapter 7
# create an object with the mean number of words per chapter

mean_word_use_m <- do.call( rbind,
                            lapply( chapter_raws_l, mean )
                            )

# chapters with higher means use less lexical variety
# compared to chapters with lower means

# subtract the overall mean so the values
# represent deviation from the grand mean
mean_word_use_m_s <- scale( mean_word_use_m )

# create the type-token ratio
ttr_l <- lapply(
  chapter_raws_l,
  function( x ){ length( x ) / sum( x ) * 100 }
)

ttr_m <- do.call( rbind, ttr_l )

# number of words in each chapter
chapter_lengths_m <- do.call( rbind,
                            lapply( chapter_raws_l, sum )
)


# ----
# Chapter 8

# number of singletons, or hapaxes
chapter_hapax_v <- sapply( chapter_raws_l, function( x ) sum( x == 1 ) )

# percentage
hapax_percentage <- chapter_hapax_v / chapter_lengths_m


# ----
# Chapter 9

# set the path
input_dir <- "data/text"

# use a regular expression to get all the files that are .txt files 
# in the directory; use the dir() function

files_v <- dir( input_dir, pattern = "\\.txt$", full.names = TRUE )

# write a function to better show the text data
show_files <- function( directory_path, pattern = "\\.txt$" ){
  file_name_v <- dir( directory_path, pattern = "\\.txt$", full.names = TRUE )
  for( i in seq_along( file_name_v ) ){
    cat( i, file_name_v[i], "\n", sep = " " )
  }
}

show_files( input_dir )

# function to return an ordered vector of words
make_token_v <- function( file_path, pattern = "\\W" ){
  text_v <- scan( file_path, what = "character", sep = "\n" )
  text_v <- paste( text_v, collapse = " " )
  text_lower_v <- tolower( text_v )
  text_words_v <- strsplit( text_lower_v, pattern )
  text_words_v <- unlist( text_words_v )
  text_words_v <- text_words_v[which( text_words_v != "" )]
  return( text_words_v )
}

austen_word_v <- make_token_v( "data/text/austen.txt" )

# find position of the word "anguish"
positions_v <- which( austen_word_v == "anguish" )

# define first instance of "anguish"
first_instance <- positions_v[1]

# find words before and after it
austen_word_v[ ( first_instance - 1) : ( first_instance + 1 )]

# print it out
cat( austen_word_v[ ( first_instance - 1) : ( first_instance + 1 )] )

# now, second instance
cat( austen_word_v[ ( positions_v[2] - 1) : ( positions_v[2] + 1 )] )

# look for word dog
# print out a sentence with five words in front and five words behind
which( austen_word_v == "dog" )

cat( austen_word_v[ ( which( austen_word_v == "dog" ) - 5) : ( which( austen_word_v == "dog" ) + 5 )] )


# ----
# Chapter 10

rm( list = ls() )

# call the functions you created in a separate script
source( "code/corpus_functions.R")

# set input directory
input_dir <- "data/text"

# set output directory
output_dir <- "results"

# using the readline function
myyear <- readline( "What year was Moby Dick published? \n" ) 

# try the function
doitKwic( input_dir )


# ----
# Chapter 11

# using stack function
chapter_lengths_df <- stack( lapply( chapter_raws_l, sum ) )

# number of words that occur once 
chap_haps_df_l <- lapply( chapter_raws_l, function( x ) sum( x == 1 ) )
chap_haps_df <- stack( chap_haps_df_l )

# build a data frame
a_data_frame <- data.frame( chapter_lengths_df, chap_haps_df )

# build a better data frame
hap_lens_df <- data.frame(
  chap_names = chapter_lengths_df$ind,
  chapter_lengths = chapter_lengths_df$values,
  num_hapax = chap_haps_df$values
)

# start using dplyr
library( dplyr )

# mutate a variable
new_df <- mutate(
  hap_lens_df,
  hapax_percentage = num_hapax / chapter_lengths
)

# clean up the headings
nice_df <- mutate(
  new_df,
  short_title = gsub( "\\..*$", "", chap_names )
)


# ----
# Chapter 12

# xml2 package
library( xml2 )


# use the read_xml function
xml_doc <- read_xml( "data/XML1/melville1.xml" )


# example using the xml markup
chapters_ns <- xml_find_all( 
  xml_doc, xpath = "//tei:div1[@type='chapter']",
  ns = c( tei = "http://www.tei-c.org/ns/1.0" )
  )

titles_ns <- xml_find_all( 
  xml_doc, xpath = "//tei:div1[@type='chapter']/tei:head",
  ns = c( tei = "http://www.tei-c.org/ns/1.0" )
)


# get the titles of each chapter as a vector
titles_v <- xml_text( titles_ns )


# function to build a list
get_node_text <- function( node, xpath, ns){
  paragraph_nodes <- xml_find_all( node, xpath, ns )
  paragraph_v <- xml_text( paragraph_nodes )
  paste( paragraph_v, collapse = " " )
}

text_l <- lapply(
  chapters_ns,
  get_node_text,
  xpath = ".//tei:p",
  ns = c( tei =  "http://www.tei-c.org/ns/1.0" )
)


# test tokenize function
source( "code/corpus_functions.R" )
tokenize( "This is a test." )

# apply the function
word_tokens_l <- lapply( text_l, tokenize )

# table of words
word_tables_l <- lapply( word_tokens_l, table )

# raw count of whale in each chapter
unlist( lapply( word_tables_l, '[', 'whale' ) )


# ----
# Chapter 13

rm( list = ls() )
setwd( "/Users/jyoung20/GitHub/TAWR2" )

library( xml2 )
library( dplyr )

xml_doc <- read_xml( "data/drama/hamlet.xml"  )

# speakers
speakers_ns <- xml_find_all( xml_doc, ".//SPEAKER" )
speaker_names_v <- xml_text( speakers_ns )
sort( table( speaker_names_v ), decreasing = TRUE )
sort( 
  table( speaker_names_v ) / length( speakers_ns ) , 
  decreasing = TRUE 
  )

# speeches
speeches_ns <- xml_find_all( xml_doc, ".//SPEECH" )

# create a function to get the speaker and the receiver together
get_pairing <- function( node ){
  speaker_v <- xml_text( xml_find_all( node, "SPEAKER" ) )
  receiver_v <- xml_text( xml_find_all( node, "RECEIVER" ) )
  paste( speaker_v, " -> ", receiver_v )
}

# run the function inside lapply
pairings_l <- lapply( speeches_ns, get_pairing )

# unlist to get a vector of characters
pairs_v <- unlist( pairings_l )

# put it together in a nice data frame
pairings_df <- data.frame( table( pairs_v ) )

# arrange the data by frequency of interaction
arrange( pairings_df, desc( Freq ) )
 

# make the function better by restructuring what data are pulled
# also add the collapse to the speaker and receiver to combine multiple people
get_pairing <- function( node ){

    speaker_v <-paste( 
    xml_text( xml_find_all( node, "SPEAKER" ) ),
    collapse = "/"
    )

    receiver_v <- paste(
    xml_text( xml_find_all( node, "RECEIVER" ) ),
    collapse = "/"
    )
    
  lines_v <- paste(
    xml_text( xml_find_all( node, "LINE" ) ),
    collapse = " "
  )
  
  c( speaker_v, receiver_v, lines_v )
}


# run over the speeches object
get_pairing( speeches_ns[[1]] )


# run from source
source( "code/corpus_functions.R" )

# run the function
speech_data_l <- lapply( speeches_ns, get_pairing )

# now build the data frame
speech_data_df <- data.frame(
  do.call( rbind, speech_data_l ),
  stringsAsFactors = FALSE
)

# set the column names
colnames( speech_data_df ) <- c( "Speaker", "Receiver", "Speech" )

# see the unique speakers
speakers_df <- select( speech_data_df, Speaker ) %>% 
  unique() %>% 
  arrange( Speaker )

# see the unique receivers
receivers_df <- select( speech_data_df, Receiver ) %>% 
  unique() %>% 
  arrange( Receiver )

# create pairings
pairings_df <- mutate(
  speech_data_df,
  pair = paste( Speaker, Receiver, sep = " -> " )
  ) %>% 
  select( pair ) %>% 
  unique() %>% 
  arrange( pair )


# run modified tokenize function to get counts
speech_data_counts_df <- rowwise( speech_data_df ) %>% 
  mutate( word_count = get_token_count( Speech ) )

# now sort it
sorted_speeches_df <- arrange(
  speech_data_counts_df,
  desc( word_count )
  ) %>% 
  select( Speaker, Receiver, word_count )


# who has the most speeches overall
group_by( sorted_speeches_df, Speaker ) %>% 
  summarize( Total = sum( word_count ) ) %>% 
  arrange( desc( Total ) )

# who is the most common speaker speaking to
group_by( sorted_speeches_df, Speaker, Receiver ) %>% 
  filter( Speaker == "HAMLET" ) %>% 
  summarize( Total = sum( word_count ) ) %>% 
  arrange( desc( Total ) )


# ----
# Chapter 14


rm( list = ls() )
setwd( "/Users/jyoung20/GitHub/TAWR2" )

library( syuzhet )


# get the raw text
moby_v  <- get_text_as_string( path_to_file = "data/text/melville.txt" )
sense_v <- get_text_as_string( path_to_file = "data/text/austen.txt" )


# ----
# Chapter 15

# setup
rm( list = ls() )
setwd( "/Users/jyoung20/GitHub/TAWR2" )
input_dir <- "data/XMLAuthorCorpus"

# use the dir function to make a vector of file names
files_v <- dir( path = input_dir, pattern = ".*xml" )

# load the xml2 library
library( xml2 )

# load the functions you need
source( "code/corpus_functions.R" )

# run the for loop to process each text
# create the object to add information to
book_freqs_l <- list()

for( i in seq_along( files_v ) ){
  
  # set the xml object to read
  xml_doc <- read_xml( file.path( input_dir, files_v[i] ) )
  
  # create a string of all words in the paragraphs in the document
  para_text <- get_node_text( xml_doc,
                              xpath = "/tei:TEI/tei:text/tei:body//tei:p",
                              ns = c( tei = "http://www.tei-c.org/ns/1.0" ) 
                              )
  
  # tokenize the paragraphs
  word_v <- tokenize( para_text )
  
  # relative frequency of words
  freq_table <- table( word_v ) / length( word_v )
  
  # assign it to the book_freqs_l object
  book_freqs_l[[files_v[i]]] <- as.data.frame(
    freq_table, stringsAsFactors = FALSE
  )
  
}





























































