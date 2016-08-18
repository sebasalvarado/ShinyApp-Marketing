#Facebook Ads: Testing fRads for a few calls
#First Attempt: Manage Facebook Ads
library(httr)
fb_app_id <-"176440219433348"
fb_app_secret <- "7eae251f5bc6d9e3bb1afd83214b0d33"

fb_app <- oauth_app('facebook',fb_app_id,fb_app_secret)
#Run localhost 8000
Sys.setenv('HTTR_SERVER_PORT' = '1410/')
token <- oauth2.0_token(oauth_endpoints('facebook'),fb_app,scope='ads_management',type  = 'localhost:8000', cache = FALSE)

token <- token$credentials$acces_token

#Second Attempt: Search Facebook
library(Rfacebook)
my_oauth <- fbOAuth(fb_app_id,fb_app_secret)

