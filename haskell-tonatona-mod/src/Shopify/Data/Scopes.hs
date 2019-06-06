module Shopify.Data.Scopes where

import RIO
import Servant
import qualified RIO.List as List (intercalate)
import Data.ByteString.Char8 (split, unpack)
import Text.Read (readsPrec)

newtype Scopes = Scopes { unScopes :: [Scope] }
  deriving newtype (Eq, Ord)

-- instance Show Scopes where
--   show = List.intercalate "," . fmap show . unScopes
instance Read Scopes where
  readsPrec _ input = [ (Scopes . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]

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

instance ToHttpApiData Scope where
  toQueryParam ReadContent                         = "read_content"
  toQueryParam WriteContent                        = "write_content"
  toQueryParam ReadThemes                          = "read_themes"
  toQueryParam WriteThemes                         = "write_themes"
  toQueryParam ReadProducts                        = "read_products"
  toQueryParam WriteProducts                       = "write_products"
  toQueryParam ReadProductListings                 = "read_product_listings"
  toQueryParam ReadCustomers                       = "read_customers"
  toQueryParam WriteCustomers                      = "write_customers"
  toQueryParam ReadOrders                          = "read_orders"
  toQueryParam WriteOrders                         = "write_orders"
  toQueryParam ReadAllOrders                       = "read_all_orders"
  toQueryParam ReadDraftOrders                     = "read_draft_orders"
  toQueryParam WriteDraftOrders                    = "write_draft_orders"
  toQueryParam ReadInventory                       = "read_inventory"
  toQueryParam WriteInventory                      = "write_inventory"
  toQueryParam ReadLocations                       = "read_locations"
  toQueryParam ReadScriptTags                      = "read_script_tags"
  toQueryParam WriteScriptTags                     = "write_script_tags"
  toQueryParam ReadFulfillments                    = "read_fulfillments"
  toQueryParam WriteFulfillments                   = "write_fulfillments"
  toQueryParam ReadShipping                        = "read_shipping"
  toQueryParam WriteShipping                       = "write_shipping"
  toQueryParam ReadAnalytics                       = "read_analytics"
  toQueryParam ReadUsers                           = "read_users"
  toQueryParam WriteUsers                          = "write_users"
  toQueryParam ReadCheckouts                       = "read_checkouts"
  toQueryParam WriteCheckouts                      = "write_checkouts"
  toQueryParam ReadReports                         = "read_reports"
  toQueryParam WriteReports                        = "write_reports"
  toQueryParam ReadPriceRules                      = "read_price_rules"
  toQueryParam WritePriceRules                     = "write_price_rules"
  toQueryParam ReadMarketingEvents                 = "read_marketing_events"
  toQueryParam WriteMarketingEvents                = "write_marketing_events"
  toQueryParam ReadResourceFeedbacks               = "read_resource_feedbacks"
  toQueryParam WriteResourceFeedbacks              = "write_resource_feedbacks"
  toQueryParam ReadShopifyPaymentsPayouts          = "read_shopify_payments_payouts"
  toQueryParam ReadShopifyPaymentsDisputes         = "read_shopify_payments_disputes"
  toQueryParam UnauthenticatedReadProductListings  = "unauthenticated_read_product_listings"
  toQueryParam UnauthenticatedReadProductTags      = "unauthenticated_read_product_tags"
  toQueryParam UnauthenticatedWriteCheckouts       = "unauthenticated_write_checkouts"
  toQueryParam UnauthenticatedReadCheckouts        = "unauthenticated_read_checkouts"
  toQueryParam UnauthenticatedWriteCustomers       = "unauthenticated_write_customers"
  toQueryParam UnauthenticatedReadCustomers        = "unauthenticated_read_customers"
  toQueryParam UnauthenticatedReadCustomerTags     = "unauthenticated_read_customer_tags"
  toQueryParam UnauthenticatedReadContent          = "unauthenticated_read_content"

instance Read Scope where
   readsPrec _ "read_content"                          = [(ReadContent, "")]
   readsPrec _ "write_content"                         = [(WriteContent, "")]
   readsPrec _ "read_themes"                           = [(ReadThemes, "")]
   readsPrec _ "write_themes"                          = [(WriteThemes, "")]
   readsPrec _ "read_products"                         = [(ReadProducts, "")]
   readsPrec _ "write_products"                        = [(WriteProducts, "")]
   readsPrec _ "read_product_listings"                 = [(ReadProductListings, "")]
   readsPrec _ "read_customers"                        = [(ReadCustomers, "")]
   readsPrec _ "write_customers"                       = [(WriteCustomers, "")]
   readsPrec _ "read_orders"                           = [(ReadOrders, "")]
   readsPrec _ "write_orders"                          = [(WriteOrders, "")]
   readsPrec _ "read_all_orders"                       = [(ReadAllOrders, "")]
   readsPrec _ "read_draft_orders"                     = [(ReadDraftOrders, "")]
   readsPrec _ "write_draft_orders"                    = [(WriteDraftOrders, "")]
   readsPrec _ "read_inventory"                        = [(ReadInventory, "")]
   readsPrec _ "write_inventory"                       = [(WriteInventory, "")]
   readsPrec _ "read_locations"                        = [(ReadLocations, "")]
   readsPrec _ "read_script_tags"                      = [(ReadScriptTags, "")]
   readsPrec _ "write_script_tags"                     = [(WriteScriptTags, "")]
   readsPrec _ "read_fulfillments"                     = [(ReadFulfillments, "")]
   readsPrec _ "write_fulfillments"                    = [(WriteFulfillments, "")]
   readsPrec _ "read_shipping"                         = [(ReadShipping, "")]
   readsPrec _ "write_shipping"                        = [(WriteShipping, "")]
   readsPrec _ "read_analytics"                        = [(ReadAnalytics, "")]
   readsPrec _ "read_users"                            = [(ReadUsers, "")]
   readsPrec _ "write_users"                           = [(WriteUsers, "")]
   readsPrec _ "read_checkouts"                        = [(ReadCheckouts, "")]
   readsPrec _ "write_checkouts"                       = [(WriteCheckouts, "")]
   readsPrec _ "read_reports"                          = [(ReadReports, "")]
   readsPrec _ "write_reports"                         = [(WriteReports, "")]
   readsPrec _ "read_price_rules"                      = [(ReadPriceRules, "")]
   readsPrec _ "write_price_rules"                     = [(WritePriceRules, "")]
   readsPrec _ "read_marketing_events"                 = [(ReadMarketingEvents, "")]
   readsPrec _ "write_marketing_events"                = [(WriteMarketingEvents, "")]
   readsPrec _ "read_resource_feedbacks"               = [(ReadResourceFeedbacks, "")]
   readsPrec _ "write_resource_feedbacks"              = [(WriteResourceFeedbacks, "")]
   readsPrec _ "read_shopify_payments_payouts"         = [(ReadShopifyPaymentsPayouts, "")]
   readsPrec _ "read_shopify_payments_disputes"        = [(ReadShopifyPaymentsDisputes, "")]
   readsPrec _ "unauthenticated_read_product_listings" = [(UnauthenticatedReadProductListings, "")]
   readsPrec _ "unauthenticated_read_product_tags"     = [(UnauthenticatedReadProductTags, "")]
   readsPrec _ "unauthenticated_write_checkouts"       = [(UnauthenticatedWriteCheckouts, "")]
   readsPrec _ "unauthenticated_read_checkouts"        = [(UnauthenticatedReadCheckouts, "")]
   readsPrec _ "unauthenticated_write_customers"       = [(UnauthenticatedWriteCustomers, "")]
   readsPrec _ "unauthenticated_read_customers"        = [(UnauthenticatedReadCustomers, "")]
   readsPrec _ "unauthenticated_read_customer_tags"    = [(UnauthenticatedReadCustomerTags, "")]
   readsPrec _ "unauthenticated_read_content"          = [(UnauthenticatedReadContent, "")]
   readsPrec _ _ = []
