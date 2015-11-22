
library(httr)
library(jsonlite)
# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. To make your own application, register at at
#    https://github.com/settings/applications. Use any URL for the homepage URL
#    (http://github.com is fine) and  http://localhost:1410 as the callback url
#
#    Replace your key and secret below.
myapp <- oauth_app("github",
                   key = "8e00b2032bc41bd8804d",
                   secret = "3290f925f7555247c54312f567588d42b200973e")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API


# "https://api.github.com/users/jtleek/repos"
parseRepos <- function(aUrl) {
    gtoken <- config(token = github_token)
    req <- GET(aUrl, gtoken)
    stop_for_status(req)
    json1 <- content(req)    
    #json1
    json2 <- jsonlite::fromJSON(toJSON(json1))
    json2
}


# OR:
#req <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
#stop_for_status(req)
#content(req)