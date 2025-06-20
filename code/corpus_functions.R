
# write a function to better show the text data
show_files <- function( directory_path, pattern = "\\.txt$" ){
  file_name_v <- dir( directory_path, pattern = "\\.txt$", full.names = TRUE )
  for( i in seq_along( file_name_v ) ){
    cat( i, file_name_v[i], "\n", sep = " " )
  }
}


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


# write function to execute KWIC more efficiently
doitKwic <- function( directory_path ){
  file_id <- as.numeric( readline( show_files( directory_path ) ) )
  keyword <- readline( "Enter a Keyword: " )
  context <- as.numeric( readline( "How many words of context? " ) )
  word_v <- make_token_v(
    file.path( directory_path, dir( directory_path )[file_id] )
  )
  hits_v <- which( word_v == keyword )
  for( i in seq_along( hits_v ) ){
    start <- hits_v[i] - context
    end <- hits_v[i] + context
    before <- word_v[ start: ( start + context - 1 ) ]
    after <- word_v[ ( start + context + 1 ): end ]
    keyword <- word_v[ start + context ]
    cat( "---", i, "---", "\n" )
    cat( before, "[", keyword, "]", after, "\n" )
  }
}