#??zellik 3:

# ??nceki fonksiyonu tan??mla
spotify_search_artist <- function(artist_name) {
  if (!is.character(artist_name)) stop("Artist name must be character type.")
  
  token <- spotify_token()
  search_url <- paste0(
    "https://api.spotify.com/v1/search?q=", URLencode(artist_name),
    "&type=artist&limit=", 5
  )
  
  response <- httr::GET(
    url = search_url,
    add_headers("Authorization" = token[[2]])
  )
  
  search_result <- httr::content(response, type = "application/json")
  status_code <- status_code(response)
  
  # ??lk num_results sanat????s??n?? al
  artists <- search_result$artists$items[seq_len(5)]
  
  search_results <- data.frame(
    artist = sapply(artists, function(x) x$name),
    id = sapply(artists, function(x) x$id)
  )
  
  result <- list(
    status_code = status_code,
    search_results = search_results
  )
  
  return(result)
}

# my_artists veri ??er??evesini olu??tur
my_artists <- data.frame(
  artist = c("Artist1", "Artist2", "Artist3", "Artist4", "Artist5"),
  id = character(5),
  stringsAsFactors = FALSE
)

# Her bir sanat???? i??in id'yi bul ve my_artists'e ekle
for (i in seq_len(nrow(my_artists))) {
  search_result <- spotify_search_artist(my_artists$artist[i])
  my_artists$id[i] <- search_result$search_results$id[1]
}

# E??le??meleri kontrol et
matches <- my_artists$id == my_artists$search_results$id

# Sonu??lar?? yazd??r
cat("E??le??me durumu:\n")
print(matches)

# my_artists ve search_results'?? yazd??r
cat("\nmy_artists veri ??er??evesi:\n")
print(my_artists)

cat("\nsearch_results veri ??er??evesi:\n")
print(search_result$search_results)