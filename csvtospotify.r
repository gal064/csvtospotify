library(spotifyr)
library(httr)
library(tidyverse)
library(readr)


#set params
user = "user"
key = "key"
secret = "secret"
csv_path = "path"




#set spotify app on developers site. set redirect URI http://localhost:1410/
#authorization flow
spotify_endpoint <- oauth_endpoint(request = NULL,
                                  authorize = "https://accounts.spotify.com/authorize",
                                  access = "https://accounts.spotify.com/api/token")
spotify_app <- oauth_app("rcsv_spotify",
                        key,
                        secret)

access_token <- oauth2.0_token(spotify_endpoint,
                              spotify_app,
                              scope = c("playlist-read-private", "playlist-modify-private"))





# Sys.setenv(SPOTIFY_CLIENT_ID = '7ee099f7cb504a49b35a24150fd19f18')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = '0f9a6ce011db42638cd04d74d393dc2d')
# 
# access_token <- get_spotify_access_token()


#function to search track details
find_tracks <- function(name, artist, token = access_token) {
  hebrew_letters <- "[אבגדהוזחטיכלמנסעפקצרשת]"
  if (is.na(artist) | grepl(hebrew_letters, name)| grepl(hebrew_letters, artist)) {
    q <- name
  } else {
    q <- paste(name, " artist:", artist,
               sep = "")
  }
  
  res <- GET("https://api.spotify.com/v1/search",
             query = list(q = q, type = "track", limit = 50),
             config(token = token))
  
  content <- res %>% content()
  track_list <- content$tracks$items
  tracks <- matrix(NA, length(track_list), 4)
  
  
  if (length(track_list) == 0 | is.na(length(track_list))){
    tracks = matrix(c(name, artist, NA, NA), 1)
  } else {
    for(i in 1:length(track_list)){
      tracks[i,] = c(track_list[[i]]$name, track_list[[i]]$artists[[1]][["name"]], as.numeric(track_list[[i]]$popularity), track_list[[i]]$id)
    }
  }
  
  
  tracks <- data.frame(tracks)
  names(tracks) <- c("name", "artist", "popularity", "id")
  tracks <- tracks %>% mutate(popularity = as.numeric(as.character(popularity)))
  return(tracks)

  
}


#read songs
Sys.setlocale("LC_ALL", "Hebrew")
music <- read_csv(csv_path, locale = locale(date_names = "he", encoding = "UTF-8"))

#query for track id
for (i in 1:nrow(music)){
  print(i)
  name <- music$song[i]
  artist <- music$artist[i]
  track <- find_tracks(name, artist) %>%
    arrange(-popularity) %>% 
      filter(row_number() == 1)
  track <- cbind(name, artist, track)
  if (i == 1) {
    tracks <- track
  } else {
    tracks <- rbind(tracks, track)
  }
}

#add songs to playlist

#create playlist
playlist_res <- POST(sprintf("https://api.spotify.com/v1/users/%s/playlists", user),
                body = list(name = "mp3files", public = "false"),
                config(token = access_token),
                encode = "json")
if(playlist_res$status_code == 403)  print("error creating playlist")
playlist_con <- content(playlist_res)
playlist_id <- playlist_con$id

#add tracks to playlisy
for (i in 1:nrow(tracks)) {
  print(i)
  res <- POST(sprintf("https://api.spotify.com/v1/playlists/%s/tracks", playlist_id),
       query = list(uris = paste("spotify:track:", tracks$id[i], sep = "")),
       config(token = access_token))
}


 


