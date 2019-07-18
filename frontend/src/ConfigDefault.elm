module Config exposing (..)



---- Backend REST API ----


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:3020"



---- Misc ----


alertBannerText : Maybe String
alertBannerText =
    Nothing


googleAnalyticsTrackingId : String
googleAnalyticsTrackingId =
    ""


dataCommonsUrl : String
dataCommonsUrl =
    "http://datacommons.cyverse.org/browse"


discoveryEnvironmentUrl : String
discoveryEnvironmentUrl =
    "https://de.cyverse.org/de/?type=data&folder="



---- Agave API ----


agaveBaseUrl : String
agaveBaseUrl =
    "https://agave.iplantc.org"