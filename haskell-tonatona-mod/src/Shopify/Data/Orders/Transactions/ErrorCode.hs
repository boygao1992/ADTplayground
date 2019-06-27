module Shopify.Data.Orders.Transactions.ErrorCode where

import RIO
import Data.Aeson

data ErrorCode
  = Incorrect_Number
  | Invalid_Number
  | Invalid_Expiry_Date
  | Invalid_Cvc
  | Expired_Card
  | Incorrect_Cvc
  | Incorrect_Zip
  | Incorrect_Address
  | Card_Declined
  | Processing_Error
  | Call_Issuer
  | Pick_Up_Card
  deriving (Eq, Ord, Show, Enum)

instance ToJSON ErrorCode where
  toJSON Incorrect_Number    = "incorrect_number"
  toJSON Invalid_Number      = "invalid_number"
  toJSON Invalid_Expiry_Date = "invalid_expiry_date"
  toJSON Invalid_Cvc         = "invalid_cvc"
  toJSON Expired_Card        = "expired_card"
  toJSON Incorrect_Cvc       = "incorrect_cvc"
  toJSON Incorrect_Zip       = "incorrect_zip"
  toJSON Incorrect_Address   = "incorrect_address"
  toJSON Card_Declined       = "card_declined"
  toJSON Processing_Error    = "processing_error"
  toJSON Call_Issuer         = "call_issuer"
  toJSON Pick_Up_Card        = "pick_up_card"

instance FromJSON ErrorCode where
  parseJSON (String "incorrect_number"   ) = pure Incorrect_Number
  parseJSON (String "invalid_number"     ) = pure Invalid_Number
  parseJSON (String "invalid_expiry_date") = pure Invalid_Expiry_Date
  parseJSON (String "invalid_cvc"        ) = pure Invalid_Cvc
  parseJSON (String "expired_card"       ) = pure Expired_Card
  parseJSON (String "incorrect_cvc"      ) = pure Incorrect_Cvc
  parseJSON (String "incorrect_zip"      ) = pure Incorrect_Zip
  parseJSON (String "incorrect_address"  ) = pure Incorrect_Address
  parseJSON (String "card_declined"      ) = pure Card_Declined
  parseJSON (String "processing_error"   ) = pure Processing_Error
  parseJSON (String "call_issuer"        ) = pure Call_Issuer
  parseJSON (String "pick_up_card"       ) = pure Pick_Up_Card
  parseJSON _ = fail "Unrecognized transaction error code"

