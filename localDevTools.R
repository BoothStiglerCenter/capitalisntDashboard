library(tidyverse)
library(httr)
library(rdrop2)
library(cyphr)
library(sodium)
library(rjson)
library(renv)


### Create an encrypted OAuth Token that can be safely uploaded to GitHub
.dstate <- new.env(parent = emptyenv())
env_keys_json <- fromJSON(file = "env_keys.json")
keyphrase <- env_keys_json$sodium_encryption_phrase
sodium_key <- hash(charToRaw(keyphrase))
sodium_key_for_cyphr <- key_sodium(sodium_key)


custom_drop_auth <- function(new_user = FALSE, key = "mmhfsybffdom42w", secret = "l8zeqqqgm1ne5z0", cache = TRUE, rdstoken = NA) {
    if (new_user == FALSE && !is.na(rdstoken)) {
        if (file.exists(rdstoken)) {
            .dstate$token <- readRDS(rdstoken)
        } else {
            stop("given token file not found")
        }
    } else {
        if (new_user && file.exists(".httr-oauth")) {
            message('New user set to "TRUE" so removing old credentials...')
            file.remove(".httr-oauth")
        }
        dropbox <- httr::oauth_endpoint(
            authorize = "https://www.dropbox.com/oauth2/authorize",
            access = "https://api.dropbox.com/oauth2/token"
        )
        dropbox_app <- httr::oauth_app("dropbox", key, secret)
        dropbox_token <- httr::oauth2.0_token(
            dropbox,
            dropbox_app,
            cache = cache,
            query_authorize_extra = list(token_access_type = "offline")
        )
        if (!inherits(dropbox_token, "Token2.0")) {
            stop("Something went wrong please try again")
        }
        .dstate$token <- dropbox_token
    }
}

drop_token <- custom_drop_auth()

encrypt(saveRDS(drop_token, "capitalisntDashboardApp/drop_token_rds.rds"), sodium_key_for_cyphr)
# saveRDS(drop_token, "drop_token_rds_decrypt.rds")

renv::snapshot('.')