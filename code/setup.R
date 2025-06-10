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







































