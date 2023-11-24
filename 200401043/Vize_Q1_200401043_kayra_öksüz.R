#Özellik 1:

library(httr)

spotify_token <- function() {
  # Spotify API token alma URL
  token_url <- "https://accounts.spotify.com/api/token"
  
  if (Sys.getenv("SPOTIFY_ID") == "" || Sys.getenv("SPOTIFY_SECRET") == "") {
    stop("SPOTIFY_ID ve SPOTIFY_SECRET environment variable'larını ayarlayın.")
  }
  
  # Token alma isteği için body
  body <- list(
    grant_type = "client_credentials",
    client_id = Sys.getenv("SPOTIFY_ID"),
    client_secret = Sys.getenv("SPOTIFY_SECRET")
  )
  
  # Token alma isteği gönderme
  response <- POST(
    url = token_url,
    body = body,
    encode = "form",
    add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )
  
  # HTTP status code'u alma
  status_code <- status_code(response)
  
  # Token değerini alma
  token <- content(response)$access_token
  
  # Bearer token stringini oluşturma
  bearer_token <- paste("Bearer", token)
  
  # Sonuçları liste olarak döndürme
  result <- list(
    status_code = status_code,
    token = bearer_token
  )
  
  return(result)
}
