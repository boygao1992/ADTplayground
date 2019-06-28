{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Orders.Order where

import RIO
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Customers.Customer (Customer)
import Shopify.Data.Customers.CustomerId (CustomerId)
import Shopify.Data.Customers.Addresses.Address (Address)
import Shopify.Data.Customers.Addresses.AddressId (AddressId)
import Shopify.Data.Orders.DiscountApplication (DiscountApplication)
import Shopify.Data.Orders.DiscountCode (DiscountCode)
import Shopify.Data.Orders.NoteAttribute (NoteAttribute)
import Shopify.Data.Orders.TaxLine (TaxLine)
import Shopify.Data.Orders.PriceSet (PriceSet)
import Shopify.Data.Orders.LineItem (LineItem)
import Shopify.Data.Orders.ShippingLine (ShippingLine)
import Shopify.Data.Orders.ClientDetails (ClientDetails)
import Shopify.Data.Orders.OrderId (OrderId)
import Shopify.Data.Orders.Refunds.Refund (Refund)
import Shopify.Data.Orders.Fulfillments.Fulfillment (Fulfillment)

data Order = Order
  { __id :: !(Maybe OrderId)
    -- "id": 450789469,
  , __email :: !(Maybe Text)
    -- "email": "bob.norman@hostmail.com",
  , __closed_at :: !(Maybe Text)
    -- "closed_at": null,
  , __created_at :: !(Maybe Text)
    -- "created_at": "2008-01-10T11:00:00-05:00",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2008-01-10T11:00:00-05:00",
  , __number :: !(Maybe Word64)
    -- "number": 1,
  , __note :: !(Maybe Text)
    -- "note": null,
  , __token :: !(Maybe Text)
    -- "token": "b1946ac92492d2347c6235b4d2611184",
  , __test :: !(Maybe Bool)
    -- "test": false,
  , __total_price :: !(Maybe Text)
    -- , __total_price: "598.94",
  , __subtotal_price :: !(Maybe Text)
    -- "subtotal_price": "597.00",
  , __total_weight :: !(Maybe Double)
    -- "total_weight": 0,
  , __total_tax :: !(Maybe Text)
    -- "total_tax": "11.94",
  , __taxes_included :: !(Maybe Bool)
    -- "taxes_included": false,
  , __currency :: !(Maybe Text)
    -- "currency": "USD",
  , __financial_status :: !(Maybe Text)
    -- "financial_status": "partially_refunded",
  , __confirmed :: !(Maybe Bool)
    -- "confirmed": true,
  , __total_discounts :: !(Maybe Text)
    -- "total_discounts": "10.00",
  , __total_line_items_price :: !(Maybe Text)
    -- "total_line_items_price": "597.00",
  , __cart_token :: !(Maybe Text)
    -- "cart_token": "68778783ad298f1c80c3bafcddeea02f",
  , __buyer_accepts_marketing :: !(Maybe Bool)
    -- "buyer_accepts_marketing": false,
  , __name :: !(Maybe Text)
    -- "name": "#1001",
  , __referring_site :: !(Maybe Text)
    -- "referring_site": "http://www.otherexample.com",
  , __landing_site :: !(Maybe Text)
    -- "landing_site": "http://www.example.com?source=abc",
  , __cancelled_at :: !(Maybe Text)
    -- "cancelled_at": null,
  , __cancel_reason :: !(Maybe Text)
    -- "cancel_reason": null,
  , __total_price_usd :: !(Maybe Text)
    -- "total_price_usd": "598.94",
  , __checkout_token :: !(Maybe Text)
    -- "checkout_token": "bd5a8aa1ecd019dd3520ff791ee3a24c",
  , __reference :: !(Maybe Text)
    -- "reference": "fhwdgads",
  , __user_id :: !(Maybe CustomerId)
    -- "user_id": null,
  , __location_id :: !(Maybe AddressId)
    -- "location_id": null,
  , __source_identifier :: !(Maybe Text)
    -- "source_identifier": "fhwdgads",
  , __source_url :: !(Maybe Text)
    -- "source_url": null,
  , __processed_at :: !(Maybe Text)
    -- "processed_at": "2008-01-10T11:00:00-05:00",
  , __device_id :: !(Maybe Word64)
    -- "device_id": null,
  , __phone :: !(Maybe Text)
    -- "phone": "+557734881234",
  , __customer_locale :: !(Maybe Text)
    -- "customer_locale": null,
  , __app_id :: !(Maybe Word64)
    -- "app_id": null,
  , __browser_ip :: !(Maybe Text)
    -- "browser_ip": "0.0.0.0",
  , __landing_site_ref :: !(Maybe Text)
    -- "landing_site_ref": "abc",
  , __order_number :: !(Maybe Word64)
    -- "order_number": 1001,
    -- NOTE The ID of the order used by the shop owner and customer.
    -- This is different from the id property, which is the ID of the order used by the API.
  , __discount_applications :: !(Maybe [DiscountApplication])

  , __discount_codes :: !(Maybe [DiscountCode])

  , __note_attributes :: !(Maybe [NoteAttribute])

  , __payment_gateway_names :: !(Maybe [Text])

  , __processing_method :: !(Maybe Text)
    -- "processing_method": "direct",
  , __checkout_id :: !(Maybe Word64)
    -- "checkout_id": 901414060,
  , __source_name :: !(Maybe Text)
    -- "source_name": "web",
  , __fulfillment_status :: !(Maybe Text)
    -- "fulfillment_status": null,
  , __tax_lines :: !(Maybe [TaxLine])

  , __tags :: !(Maybe Text)
    -- "tags": "",
  , __contact_email :: !(Maybe Text)
    -- "contact_email": "bob.norman@hostmail.com",
  , __order_status_url :: !(Maybe Text)
    -- "order_status_url": "https://checkout.local/690933842/orders/b1946ac92492d2347c6235b4d2611184/authenticate?key=ef277d5d70d5b10d129bd6f012cff754",
  , __presentment_currency :: !(Maybe Text)
    -- "presentment_currency": "USD",
  , __total_line_items_price_set :: !(Maybe PriceSet)

  , __total_discounts_set :: !(Maybe PriceSet)

  , __total_shipping_price_set :: !(Maybe PriceSet)

  , __subtotal_price_set :: !(Maybe PriceSet)

  , __total_price_set :: !(Maybe PriceSet)

  , __total_tax_set :: !(Maybe PriceSet)

  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Order/450789469",
  , __line_items :: !(Maybe [LineItem])

  , __shipping_lines :: !(Maybe [ShippingLine])

  , __billing_address :: !(Maybe Address)

  , __shipping_address :: !(Maybe Address)

  , __fulfillments :: !(Maybe [Fulfillment])

  , __client_details :: !(Maybe ClientDetails)
  , __refunds :: !(Maybe [Refund])
  , __customer :: !(Maybe Customer)
  } deriving (Eq, Show)
$(makeLensesDropOne ''Order)
$(deriveJSONDropTwo ''Order)


data SingleOrder = SingleOrder
  { __order :: !Order
  } deriving (Eq, Show)
$(deriveJSONDropTwo ''SingleOrder)

data Orders = Orders { orders :: ![Order] }
$(deriveJSONDropTwo ''Orders)


-- type Api = "orders" :> OrderApi

-- type OrderApi
--   = Get '[JSON] Orders
-- GET /admin/api/2019-04/orders.json
-- Retrieves a list of orders
-- GET /admin/api/2019-04/orders/#{order_id}.json
-- Retrieves a specific order
-- GET /admin/api/2019-04/orders/count.json
-- Retrieves an order count
-- POST /admin/api/2019-04/orders/#{order_id}/close.json
-- Closes an order
-- POST /admin/api/2019-04/orders/#{order_id}/open.json
-- Re-opens a closed order
-- POST /admin/api/2019-04/orders/#{order_id}/cancel.json
-- Cancels an order
-- POST /admin/api/2019-04/orders.json
-- Creates an order
-- PUT /admin/api/2019-04/orders/#{order_id}.json
-- Updates an order
-- DELETE /admin/api/2019-04/orders/#{order_id}.json
-- Deletes an order
