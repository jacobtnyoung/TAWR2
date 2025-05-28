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
# 
