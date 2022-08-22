library(rsconnect)
library(rdrop2)
# library(cyphr)
# library(sodium)

error_on_missing_name <- function(name) {
    var <- Sys.getenv(name, unset = NA)
    if (is.na(var)) {
        stop(paste0("CANNOT FIND", name, "!", sep = " "), call. = FALSE)
    }
    gsub("\"", "", var)
    print(var)
}

error_on_missing_name("SODIUM_ENCRYPTION_PHRASE")
error_on_missing_name("SHINY_ACC_NAME")
error_on_missing_name("TOKEN")
error_on_missing_name("SECRET")
error_on_missing_name("MASTERNAME")
error_on_missing_name("LOCALDEVNAME")

### Decrypt Dropbox authentication token to be pushed to ShinyApps
# keyphrase <- error_on_missing_name("SODIUM_ENCRYPTION_PHRASE")
# sodium_key <- hash(charToRaw(keyphrase))
# sodium_key_for_cyphr <- key_sodium(sodium_key)
# decrypted_drop_token <- decrypt(readRDS("drop_token_rds.rds"), sodium_key_for_cyphr)

# saveRDS("capitalisntDashboardApp/drop_token_rds.rds")

# ### Shiny Authentication section:
# setAccountInfo(
#     name = error_on_missing_name("SHINY_ACC_NAME"),
#     token = error_on_missing_name("TOKEN"),
#     secret = error_on_missing_name("SECRET")
# )

# ### Deploy Application to Shiny
# deployApp(
#     appFiles = c("ui.R", "global.R", "server.R", "drop_token_rds.rds"),
#     appName = error_on_missing_name("MASTERNAME"),
#     appTitle = "Capitalisn't Dashboard"
# )


# encrypt(saveRDS(decrypted_drop_token, "drop_token_rds.rds"), sodium_key_for_cyphr)

# if (file.exists("capitalisntDashboardApp/drop_token_rds.rds")) {
#     file.remove("capitalisntDashboardApp/drop_token_rds.rds")
# }
