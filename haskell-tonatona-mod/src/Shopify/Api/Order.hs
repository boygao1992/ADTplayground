{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Order where

import RIO
import Data.Aeson.TH
import Shopify.Api.Customer.Data.Customer (Customer)
import Shopify.Api.Customer.Data.CustomerId (CustomerId)
import Shopify.Api.Customer.Address (Address)
import Shopify.Api.Customer.Address.Data.AddressId (AddressId)
import Shopify.Api.Order.Data.DiscountApplication (DiscountApplication)
import Shopify.Api.Order.Data.DiscountCode (DiscountCode)
import Shopify.Api.Order.Data.NoteAttribute (NoteAttribute)
import Shopify.Api.Order.Data.TaxLine (TaxLine)
import Shopify.Api.Order.Data.PriceSet (PriceSet)
import Shopify.Api.Order.Data.LineItem (LineItem)
import Shopify.Api.Order.Data.ShippingLine (ShippingLine)
import Shopify.Api.Order.Data.ClientDetails (ClientDetails)
import Shopify.Api.Order.Data.OrderId (OrderId)
import Shopify.Api.Order.Refund (Refund)
import Shopify.Api.Order.Fulfillment (Fulfillment)

data Order = Order
  { _id :: !(Maybe OrderId)
    -- "id": 450789469,
  , _email :: !(Maybe Text)
    -- "email": "bob.norman@hostmail.com",
  , _closed_at :: !(Maybe Text)
    -- "closed_at": null,
  , _created_at :: !(Maybe Text)
    -- "created_at": "2008-01-10T11:00:00-05:00",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2008-01-10T11:00:00-05:00",
  , _number :: !(Maybe Word64)
    -- "number": 1,
  , _note :: !(Maybe Text)
    -- "note": null,
  , _token :: !(Maybe Text)
    -- "token": "b1946ac92492d2347c6235b4d2611184",
  , _test :: !(Maybe Bool)
    -- "test": false,
  , _total_price :: !(Maybe Text)
    -- , _total_price: "598.94",
  , _subtotal_price :: !(Maybe Text)
    -- "subtotal_price": "597.00",
  , _total_weight :: !(Maybe Double)
    -- "total_weight": 0,
  , _total_tax :: !(Maybe Text)
    -- "total_tax": "11.94",
  , _taxes_included :: !(Maybe Bool)
    -- "taxes_included": false,
  , _currency :: !(Maybe Text)
    -- "currency": "USD",
  , _financial_status :: !(Maybe Text)
    -- "financial_status": "partially_refunded",
  , _confirmed :: !(Maybe Bool)
    -- "confirmed": true,
  , _total_discounts :: !(Maybe Text)
    -- "total_discounts": "10.00",
  , _total_line_items_price :: !(Maybe Text)
    -- "total_line_items_price": "597.00",
  , _cart_token :: !(Maybe Text)
    -- "cart_token": "68778783ad298f1c80c3bafcddeea02f",
  , _buyer_accepts_marketing :: !(Maybe Bool)
    -- "buyer_accepts_marketing": false,
  , _name :: !(Maybe Text)
    -- "name": "#1001",
  , _referring_site :: !(Maybe Text)
    -- "referring_site": "http://www.otherexample.com",
  , _landing_site :: !(Maybe Text)
    -- "landing_site": "http://www.example.com?source=abc",
  , _cancelled_at :: !(Maybe Text)
    -- "cancelled_at": null,
  , _cancel_reason :: !(Maybe Text)
    -- "cancel_reason": null,
  , _total_price_usd :: !(Maybe Text)
    -- "total_price_usd": "598.94",
  , _checkout_token :: !(Maybe Text)
    -- "checkout_token": "bd5a8aa1ecd019dd3520ff791ee3a24c",
  , _reference :: !(Maybe Text)
    -- "reference": "fhwdgads",
  , _user_id :: !(Maybe CustomerId)
    -- "user_id": null,
  , _location_id :: !(Maybe AddressId)
    -- "location_id": null,
  , _source_identifier :: !(Maybe Text)
    -- "source_identifier": "fhwdgads",
  , _source_url :: !(Maybe Text)
    -- "source_url": null,
  , _processed_at :: !(Maybe Text)
    -- "processed_at": "2008-01-10T11:00:00-05:00",
  , _device_id :: !(Maybe Word64)
    -- "device_id": null,
  , _phone :: !(Maybe Text)
    -- "phone": "+557734881234",
  , _customer_locale :: !(Maybe Text)
    -- "customer_locale": null,
  , _app_id :: !(Maybe Word64)
    -- "app_id": null,
  , _browser_ip :: !(Maybe Text)
    -- "browser_ip": "0.0.0.0",
  , _landing_site_ref :: !(Maybe Text)
    -- "landing_site_ref": "abc",
  , _order_number :: !(Maybe Word64)
    -- "order_number": 1001,
    -- NOTE The ID of the order used by the shop owner and customer.
    -- This is different from the id property, which is the ID of the order used by the API.
  , _discount_applications :: !(Maybe [DiscountApplication])

  , _discount_codes :: !(Maybe [DiscountCode])

  , _note_attributes :: !(Maybe [NoteAttribute])

  , _payment_gateway_names :: !(Maybe [Text])

  , _processing_method :: !(Maybe Text)
    -- "processing_method": "direct",
  , _checkout_id :: !(Maybe Word64)
    -- "checkout_id": 901414060,
  , _source_name :: !(Maybe Text)
    -- "source_name": "web",
  , _fulfillment_status :: !(Maybe Text)
    -- "fulfillment_status": null,
  , _tax_lines :: !(Maybe [TaxLine])

  , _tags :: !(Maybe Text)
    -- "tags": "",
  , _contact_email :: !(Maybe Text)
    -- "contact_email": "bob.norman@hostmail.com",
  , _order_status_url :: !(Maybe Text)
    -- "order_status_url": "https://checkout.local/690933842/orders/b1946ac92492d2347c6235b4d2611184/authenticate?key=ef277d5d70d5b10d129bd6f012cff754",
  , _presentment_currency :: !(Maybe Text)
    -- "presentment_currency": "USD",
  , _total_line_items_price_set :: !(Maybe PriceSet)

  , _total_discounts_set :: !(Maybe PriceSet)

  , _total_shipping_price_set :: !(Maybe PriceSet)

  , _subtotal_price_set :: !(Maybe PriceSet)

  , _total_price_set :: !(Maybe PriceSet)

  , _total_tax_set :: !(Maybe PriceSet)

  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Order/450789469",
  , _line_items :: !(Maybe [LineItem])

  , _shipping_lines :: !(Maybe [ShippingLine])

  , _billing_address :: !(Maybe Address)

  , _shipping_address :: !(Maybe Address)

  , _fulfillments :: !(Maybe [Fulfillment])

  , _client_details :: !(Maybe ClientDetails)
  , _refunds :: !(Maybe [Refund])
  , _customer :: !(Maybe Customer)
  } deriving (Eq, Show)

$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Order)


data SingleOrder = SingleOrder
  { _order :: !Order
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''SingleOrder)

data Orders = Orders { orders :: ![Order] }
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      }
    ''Orders)


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
