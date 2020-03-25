module Config exposing (..)



---- Backend REST API ----


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:3020"



---- Misc ----


alertBannerText : Maybe String
alertBannerText =
    Nothing


maintenanceMode : Bool
maintenanceMode =
    False


googleAnalyticsTrackingId : String
googleAnalyticsTrackingId =
    ""


dataCommonsUrl : String
dataCommonsUrl =
    "http://datacommons.cyverse.org/browse"


discoveryEnvironmentUrl : String
discoveryEnvironmentUrl =
    "https://de.cyverse.org/de/?type=data&folder="


sraUrl : String
sraUrl =
    "https://www.ncbi.nlm.nih.gov/sra/?term="



---- Agave API ----


agaveBaseUrl : String
agaveBaseUrl =
    "https://agave.iplantc.org"


-- OAuth2 Configuration
agaveOAuthClientId : String
agaveOAuthClientId =
    ""


agaveRedirectUrl : String
agaveRedirectUrl =
    ""



---- Plan B ----


-- Base URL
planbBaseUrl : String
planbBaseUrl =
    "https://www.imicrobe.us/plan-b"