module Shopify.Api.Admin.Data.Scopes where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import Servant
import Data.Bifunctor as Bifunctor (first)
import Data.ByteString.Char8 (split)
import Data.Aeson (FromJSON, ToJSON, Value(String), parseJSON, toJSON)

newtype Scopes = Scopes { unScopes :: [Scope] }
  deriving newtype (Eq, Ord)

instance Show Scopes where
  show = List.intercalate "," . fmap show . unScopes
instance ToHttpApiData Scopes where
  toQueryParam = Text.pack . show
instance ToJSON Scopes where
  toJSON = String . toQueryParam
instance FromHttpApiData Scopes where
  parseQueryParam = fmap Scopes . traverse (parseQueryParam <=< Bifunctor.first (Text.pack . show) . decodeUtf8') . split ',' . encodeUtf8
instance FromJSON Scopes where
  parseJSON (String scopes) = case parseQueryParam scopes of
    Left err -> fail (Text.unpack err)
    Right res -> pure res
  parseJSON _ = fail "Unrecognized OAuth scope"

data Scope
  = ReadContent
  | WriteContent
  | ReadThemes
  | WriteThemes
  | ReadProducts
  | WriteProducts
  | ReadProductListings
  | ReadCustomers
  | WriteCustomers
  | ReadOrders
  | WriteOrders
  | ReadAllOrders
  | ReadDraftOrders
  | WriteDraftOrders
  | ReadInventory
  | WriteInventory
  | ReadLocations
  | ReadScriptTags
  | WriteScriptTags
  | ReadFulfillments
  | WriteFulfillments
  | ReadShipping
  | WriteShipping
  | ReadAnalytics
  | ReadUsers
  | WriteUsers
  | ReadCheckouts
  | WriteCheckouts
  | ReadReports
  | WriteReports
  | ReadPriceRules
  | WritePriceRules
  | ReadMarketingEvents
  | WriteMarketingEvents
  | ReadResourceFeedbacks
  | WriteResourceFeedbacks
  | ReadShopifyPaymentsPayouts
  | ReadShopifyPaymentsDisputes
  | UnauthenticatedReadProductListings
  | UnauthenticatedReadProductTags
  | UnauthenticatedWriteCheckouts
  | UnauthenticatedReadCheckouts
  | UnauthenticatedWriteCustomers
  | UnauthenticatedReadCustomers
  | UnauthenticatedReadCustomerTags
  | UnauthenticatedReadContent
  deriving (Eq, Ord, Enum)

instance Show Scope where
  show ReadContent                         = "read_content"
  show WriteContent                        = "write_content"
  show ReadThemes                          = "read_themes"
  show WriteThemes                         = "write_themes"
  show ReadProducts                        = "read_products"
  show WriteProducts                       = "write_products"
  show ReadProductListings                 = "read_product_listings"
  show ReadCustomers                       = "read_customers"
  show WriteCustomers                      = "write_customers"
  show ReadOrders                          = "read_orders"
  show WriteOrders                         = "write_orders"
  show ReadAllOrders                       = "read_all_orders"
  show ReadDraftOrders                     = "read_draft_orders"
  show WriteDraftOrders                    = "write_draft_orders"
  show ReadInventory                       = "read_inventory"
  show WriteInventory                      = "write_inventory"
  show ReadLocations                       = "read_locations"
  show ReadScriptTags                      = "read_script_tags"
  show WriteScriptTags                     = "write_script_tags"
  show ReadFulfillments                    = "read_fulfillments"
  show WriteFulfillments                   = "write_fulfillments"
  show ReadShipping                        = "read_shipping"
  show WriteShipping                       = "write_shipping"
  show ReadAnalytics                       = "read_analytics"
  show ReadUsers                           = "read_users"
  show WriteUsers                          = "write_users"
  show ReadCheckouts                       = "read_checkouts"
  show WriteCheckouts                      = "write_checkouts"
  show ReadReports                         = "read_reports"
  show WriteReports                        = "write_reports"
  show ReadPriceRules                      = "read_price_rules"
  show WritePriceRules                     = "write_price_rules"
  show ReadMarketingEvents                 = "read_marketing_events"
  show WriteMarketingEvents                = "write_marketing_events"
  show ReadResourceFeedbacks               = "read_resource_feedbacks"
  show WriteResourceFeedbacks              = "write_resource_feedbacks"
  show ReadShopifyPaymentsPayouts          = "read_shopify_payments_payouts"
  show ReadShopifyPaymentsDisputes         = "read_shopify_payments_disputes"
  show UnauthenticatedReadProductListings  = "unauthenticated_read_product_listings"
  show UnauthenticatedReadProductTags      = "unauthenticated_read_product_tags"
  show UnauthenticatedWriteCheckouts       = "unauthenticated_write_checkouts"
  show UnauthenticatedReadCheckouts        = "unauthenticated_read_checkouts"
  show UnauthenticatedWriteCustomers       = "unauthenticated_write_customers"
  show UnauthenticatedReadCustomers        = "unauthenticated_read_customers"
  show UnauthenticatedReadCustomerTags     = "unauthenticated_read_customer_tags"
  show UnauthenticatedReadContent          = "unauthenticated_read_content"

instance ToHttpApiData Scope where
  toQueryParam = Text.pack . show

instance ToJSON Scope where
  toJSON = String . toQueryParam

instance FromHttpApiData Scope where
   parseQueryParam "read_content"                          = Right ReadContent
   parseQueryParam "write_content"                         = Right WriteContent
   parseQueryParam "read_themes"                           = Right ReadThemes
   parseQueryParam "write_themes"                          = Right WriteThemes
   parseQueryParam "read_products"                         = Right ReadProducts
   parseQueryParam "write_products"                        = Right WriteProducts
   parseQueryParam "read_product_listings"                 = Right ReadProductListings
   parseQueryParam "read_customers"                        = Right ReadCustomers
   parseQueryParam "write_customers"                       = Right WriteCustomers
   parseQueryParam "read_orders"                           = Right ReadOrders
   parseQueryParam "write_orders"                          = Right WriteOrders
   parseQueryParam "read_all_orders"                       = Right ReadAllOrders
   parseQueryParam "read_draft_orders"                     = Right ReadDraftOrders
   parseQueryParam "write_draft_orders"                    = Right WriteDraftOrders
   parseQueryParam "read_inventory"                        = Right ReadInventory
   parseQueryParam "write_inventory"                       = Right WriteInventory
   parseQueryParam "read_locations"                        = Right ReadLocations
   parseQueryParam "read_script_tags"                      = Right ReadScriptTags
   parseQueryParam "write_script_tags"                     = Right WriteScriptTags
   parseQueryParam "read_fulfillments"                     = Right ReadFulfillments
   parseQueryParam "write_fulfillments"                    = Right WriteFulfillments
   parseQueryParam "read_shipping"                         = Right ReadShipping
   parseQueryParam "write_shipping"                        = Right WriteShipping
   parseQueryParam "read_analytics"                        = Right ReadAnalytics
   parseQueryParam "read_users"                            = Right ReadUsers
   parseQueryParam "write_users"                           = Right WriteUsers
   parseQueryParam "read_checkouts"                        = Right ReadCheckouts
   parseQueryParam "write_checkouts"                       = Right WriteCheckouts
   parseQueryParam "read_reports"                          = Right ReadReports
   parseQueryParam "write_reports"                         = Right WriteReports
   parseQueryParam "read_price_rules"                      = Right ReadPriceRules
   parseQueryParam "write_price_rules"                     = Right WritePriceRules
   parseQueryParam "read_marketing_events"                 = Right ReadMarketingEvents
   parseQueryParam "write_marketing_events"                = Right WriteMarketingEvents
   parseQueryParam "read_resource_feedbacks"               = Right ReadResourceFeedbacks
   parseQueryParam "write_resource_feedbacks"              = Right WriteResourceFeedbacks
   parseQueryParam "read_shopify_payments_payouts"         = Right ReadShopifyPaymentsPayouts
   parseQueryParam "read_shopify_payments_disputes"        = Right ReadShopifyPaymentsDisputes
   parseQueryParam "unauthenticated_read_product_listings" = Right UnauthenticatedReadProductListings
   parseQueryParam "unauthenticated_read_product_tags"     = Right UnauthenticatedReadProductTags
   parseQueryParam "unauthenticated_write_checkouts"       = Right UnauthenticatedWriteCheckouts
   parseQueryParam "unauthenticated_read_checkouts"        = Right UnauthenticatedReadCheckouts
   parseQueryParam "unauthenticated_write_customers"       = Right UnauthenticatedWriteCustomers
   parseQueryParam "unauthenticated_read_customers"        = Right UnauthenticatedReadCustomers
   parseQueryParam "unauthenticated_read_customer_tags"    = Right UnauthenticatedReadCustomerTags
   parseQueryParam "unauthenticated_read_content"          = Right UnauthenticatedReadContent
   parseQueryParam _ = Left "Unrecognized OAuth scope"

instance FromJSON Scope where
  parseJSON (String scope) = case parseQueryParam scope of
    Left err -> fail (Text.unpack err)
    Right res -> pure res
  parseJSON _ = fail "Unrecognized OAuth scope"
