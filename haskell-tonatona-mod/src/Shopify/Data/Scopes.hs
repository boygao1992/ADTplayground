module Shopify.Data.Scopes where

import RIO
import qualified RIO.List as List (intercalate)
import Data.ByteString.Char8 (split, unpack)
import Text.Read (readsPrec)

newtype Scopes = Scopes { unScopes :: [Scope] }
  deriving newtype (Eq, Ord)

instance Read Scopes where
  readsPrec _ input = [ (Scopes . mapMaybe (readMaybe . unpack) . split ',' . fromString $ input, "")]
instance Show Scopes where
  show = List.intercalate "," . fmap show . unScopes

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
