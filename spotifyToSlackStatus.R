#devtools::install_github("lynuhs/spotifyr")
library(spotifyr)
library(httr)

bearer <- "Bearer [ENTER_SLACK_OAUTH_TOKEN]"
Sys.setenv(SPOTIFY_CLIENT_ID = [ENTER_SPOTIFY_API_CLIENT_ID])
Sys.setenv(SPOTIFY_CLIENT_SECRET = [ENTER_SPOTIFY_API_CLIENT_SECRET])
user_id <- [ENTER_SLACK_MEMBER_ID]

access_token <- get_spotify_access_token()


tryCatch({
  current_track <- get_my_currently_playing()

  if(current_track$is_playing){
    if(current_track$currently_playing_type == "episode"){
      POST(
        url = "https://slack.com/api/users.profile.set",
        encode = "json",
        add_headers(
          .headers = c(
            "Authorization" = bearer,
            "Content-Type" = "application/json; charset=utf-8"
          )
        ),
        body = list(
          profile = list(
            status_emoji = ":headphones:",
            status_text = "Podcast"
          ),
          user = user_id
        )
      )
    } else {
      track <- current_track$item$name
      artist <- paste(current_track$item$artists$name, collapse = ", ")
      external_link <- current_track$item$external_urls$spotify
      text = paste0(track, " (", artist, ")")

      POST(
        url = "https://slack.com/api/users.profile.set",
        encode = "json",
        add_headers(
          .headers = c(
            "Authorization" = bearer,
            "Content-Type" = "application/json; charset=utf-8"
          )
        ),
        body = list(
          profile = list(
            status_emoji = ":headphones:",
            status_text = text,
            fields = list(
             [CUSTOM_FIELD_ID] = list(
                value = external_link,
                alt = text
              )
            )
          ),
          user = user_id
        )
      )
    }

  } else {
    POST(
      url = "https://slack.com/api/users.profile.set",
      encode = "json",
      add_headers(
        .headers = c(
          "Authorization" = bearer,
          "Content-Type" = "application/json; charset=utf-8"
        )
      ),
      body = list(
        profile = list(
          status_emoji = "",
          status_text = ""
        ),
        user = user_id
      )
    )
  }
}, error = function(e){
  POST(
      url = "https://slack.com/api/users.profile.set",
      encode = "json",
      add_headers(
        .headers = c(
          "Authorization" = bearer,
          "Content-Type" = "application/json; charset=utf-8"
        )
      ),
      body = list(
        profile = list(
          status_emoji = "",
          status_text = ""
        ),
        user = user_id
      )
    )
  }
})
