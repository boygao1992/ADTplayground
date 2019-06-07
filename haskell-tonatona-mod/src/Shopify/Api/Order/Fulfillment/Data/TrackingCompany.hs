module Shopify.Api.Order.Fulfillment.Data.TrackingCompany where

import RIO
import Data.Aeson

data TrackingCompany
  = TrackingCompany_4PX
  | TrackingCompany_APC
  | TrackingCompany_Amazon_Logistics_UK
  | TrackingCompany_Amazon_Logistics_US
  | TrackingCompany_Australia_Post
  | TrackingCompany_Bluedart
  | TrackingCompany_Canada_Post
  | TrackingCompany_China_Post
  | TrackingCompany_Correios
  | TrackingCompany_DHL_Express
  | TrackingCompany_DHL_eCommerce
  | TrackingCompany_DHL_eCommerce_Asia
  | TrackingCompany_DPD
  | TrackingCompany_DPD_Local
  | TrackingCompany_DPD_UK
  | TrackingCompany_Delhivery
  | TrackingCompany_Eagle
  | TrackingCompany_FSC
  | TrackingCompany_FedEx
  | TrackingCompany_FedEx_UK
  | TrackingCompany_GLS
  | TrackingCompany_Globegistics
  | TrackingCompany_Japan_Post_EN
  | TrackingCompany_Japan_Post_JA
  | TrackingCompany_La_Poste
  | TrackingCompany_New_Zealand_Post
  | TrackingCompany_Newgistics
  | TrackingCompany_PostNL
  | TrackingCompany_PostNord
  | TrackingCompany_Purolator
  | TrackingCompany_Royal_Mail
  | TrackingCompany_SF_Express
  | TrackingCompany_Sagawa_EN
  | TrackingCompany_Sagawa_JA
  | TrackingCompany_Singapore_Post
  | TrackingCompany_TNT
  | TrackingCompany_UPS
  | TrackingCompany_USPS
  | TrackingCompany_Whistl
  | TrackingCompany_Yamato_EN
  | TrackingCompany_Yamato_JA
  deriving (Eq, Ord, Show, Enum)

instance ToJSON TrackingCompany where
  toJSON TrackingCompany_4PX                 = "4PX"
  toJSON TrackingCompany_APC                 = "APC"
  toJSON TrackingCompany_Amazon_Logistics_UK = "Amazon Logistics UK"
  toJSON TrackingCompany_Amazon_Logistics_US = "Amazon Logistics US"
  toJSON TrackingCompany_Australia_Post      = "Australia Post"
  toJSON TrackingCompany_Bluedart            = "Bluedart"
  toJSON TrackingCompany_Canada_Post         = "Canada Post"
  toJSON TrackingCompany_China_Post          = "China Post"
  toJSON TrackingCompany_Correios            = "Correios"
  toJSON TrackingCompany_DHL_Express         = "DHL Express"
  toJSON TrackingCompany_DHL_eCommerce       = "DHL eCommerce"
  toJSON TrackingCompany_DHL_eCommerce_Asia  = "DHL eCommerce Asia"
  toJSON TrackingCompany_DPD                 = "DPD"
  toJSON TrackingCompany_DPD_Local           = "DPD Local"
  toJSON TrackingCompany_DPD_UK              = "DPD UK"
  toJSON TrackingCompany_Delhivery           = "Delhivery"
  toJSON TrackingCompany_Eagle               = "Eagle"
  toJSON TrackingCompany_FSC                 = "FSC"
  toJSON TrackingCompany_FedEx               = "FedEx"
  toJSON TrackingCompany_FedEx_UK            = "FedEx UK"
  toJSON TrackingCompany_GLS                 = "GLS"
  toJSON TrackingCompany_Globegistics        = "Globegistics"
  toJSON TrackingCompany_Japan_Post_EN       = "Japan Post (EN)"
  toJSON TrackingCompany_Japan_Post_JA       = "Japan Post (JA)"
  toJSON TrackingCompany_La_Poste            = "La Poste"
  toJSON TrackingCompany_New_Zealand_Post    = "New Zealand Post"
  toJSON TrackingCompany_Newgistics          = "Newgistics"
  toJSON TrackingCompany_PostNL              = "PostNL"
  toJSON TrackingCompany_PostNord            = "PostNord"
  toJSON TrackingCompany_Purolator           = "Purolator"
  toJSON TrackingCompany_Royal_Mail          = "Royal Mail"
  toJSON TrackingCompany_SF_Express          = "SF Express"
  toJSON TrackingCompany_Sagawa_EN           = "Sagawa (EN)"
  toJSON TrackingCompany_Sagawa_JA           = "Sagawa (JA)"
  toJSON TrackingCompany_Singapore_Post      = "Singapore Post"
  toJSON TrackingCompany_TNT                 = "TNT"
  toJSON TrackingCompany_UPS                 = "UPS"
  toJSON TrackingCompany_USPS                = "USPS"
  toJSON TrackingCompany_Whistl              = "Whistl"
  toJSON TrackingCompany_Yamato_EN           = "Yamato (EN)"
  toJSON TrackingCompany_Yamato_JA           = "Yamato (JA)"


instance FromJSON TrackingCompany where
  parseJSON (String "4PX"                 ) = pure TrackingCompany_4PX
  parseJSON (String "APC"                 ) = pure TrackingCompany_APC
  parseJSON (String "Amazon Logistics UK" ) = pure TrackingCompany_Amazon_Logistics_UK
  parseJSON (String "Amazon Logistics US" ) = pure TrackingCompany_Amazon_Logistics_US
  parseJSON (String "Australia Post"      ) = pure TrackingCompany_Australia_Post
  parseJSON (String "Bluedart"            ) = pure TrackingCompany_Bluedart
  parseJSON (String "Canada Post"         ) = pure TrackingCompany_Canada_Post
  parseJSON (String "China Post"          ) = pure TrackingCompany_China_Post
  parseJSON (String "Correios"            ) = pure TrackingCompany_Correios
  parseJSON (String "DHL Express"         ) = pure TrackingCompany_DHL_Express
  parseJSON (String "DHL eCommerce"       ) = pure TrackingCompany_DHL_eCommerce
  parseJSON (String "DHL eCommerce Asia"  ) = pure TrackingCompany_DHL_eCommerce_Asia
  parseJSON (String "DPD"                 ) = pure TrackingCompany_DPD
  parseJSON (String "DPD Local"           ) = pure TrackingCompany_DPD_Local
  parseJSON (String "DPD UK"              ) = pure TrackingCompany_DPD_UK
  parseJSON (String "Delhivery"           ) = pure TrackingCompany_Delhivery
  parseJSON (String "Eagle"               ) = pure TrackingCompany_Eagle
  parseJSON (String "FSC"                 ) = pure TrackingCompany_FSC
  parseJSON (String "FedEx"               ) = pure TrackingCompany_FedEx
  parseJSON (String "FedEx UK"            ) = pure TrackingCompany_FedEx_UK
  parseJSON (String "GLS"                 ) = pure TrackingCompany_GLS
  parseJSON (String "Globegistics"        ) = pure TrackingCompany_Globegistics
  parseJSON (String "Japan Post (EN)"     ) = pure TrackingCompany_Japan_Post_EN
  parseJSON (String "Japan Post (JA)"     ) = pure TrackingCompany_Japan_Post_JA
  parseJSON (String "La Poste"            ) = pure TrackingCompany_La_Poste
  parseJSON (String "New Zealand Post"    ) = pure TrackingCompany_New_Zealand_Post
  parseJSON (String "Newgistics"          ) = pure TrackingCompany_Newgistics
  parseJSON (String "PostNL"              ) = pure TrackingCompany_PostNL
  parseJSON (String "PostNord"            ) = pure TrackingCompany_PostNord
  parseJSON (String "Purolator"           ) = pure TrackingCompany_Purolator
  parseJSON (String "Royal Mail"          ) = pure TrackingCompany_Royal_Mail
  parseJSON (String "SF Express"          ) = pure TrackingCompany_SF_Express
  parseJSON (String "Sagawa (EN)"         ) = pure TrackingCompany_Sagawa_EN
  parseJSON (String "Sagawa (JA)"         ) = pure TrackingCompany_Sagawa_JA
  parseJSON (String "Singapore Post"      ) = pure TrackingCompany_Singapore_Post
  parseJSON (String "TNT"                 ) = pure TrackingCompany_TNT
  parseJSON (String "UPS"                 ) = pure TrackingCompany_UPS
  parseJSON (String "USPS"                ) = pure TrackingCompany_USPS
  parseJSON (String "Whistl"              ) = pure TrackingCompany_Whistl
  parseJSON (String "Yamato (EN)"         ) = pure TrackingCompany_Yamato_EN
  parseJSON (String "Yamato (JA)"         ) = pure TrackingCompany_Yamato_JA
  parseJSON _ = fail "Unrecognized tracking company"

