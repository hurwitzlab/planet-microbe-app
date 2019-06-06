module Config exposing (apiBaseUrl, alertBannerText)


-- Backend REST API


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:3020"


alertBannerText : Maybe String
alertBannerText =
    Nothing
