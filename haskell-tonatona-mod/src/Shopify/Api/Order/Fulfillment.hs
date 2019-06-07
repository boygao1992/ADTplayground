{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Api.Order.Fulfillment where

import RIO
import Data.Aeson (Value)
import Data.Aeson.TH
import Shopify.Api.Order.Data.OrderId (OrderId)
import Shopify.Api.Order.Fulfillment.Data.FulfillmentId (FulfillmentId)
import Shopify.Api.Order.Fulfillment.Data.Status (Status)
import Shopify.Api.Order.Fulfillment.Data.TrackingCompany (TrackingCompany)
import Shopify.Api.Order.Fulfillment.Data.ShipmentStatus (ShipmentStatus)
import Shopify.Api.Order.Data.LineItem as Order (LineItem)


data Fulfillment = Fulfillment
  { _id :: !(Maybe FulfillmentId)
    -- "id": 255858046,
  , _order_id :: !(Maybe OrderId)
    -- "order_id": 450789469,
  , _status :: !(Maybe Status)
    -- "status": "failure",
  , _created_at :: !(Maybe Text)
    -- "created_at": "2019-05-03T11:02:30-04:00",
  , _service :: !(Maybe Text)
    -- "service": "manual",
  , _updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-03T11:02:30-04:00",
  , _tracking_company :: !(Maybe TrackingCompany)
    -- "tracking_company": null,
  , _shipment_status :: !(Maybe ShipmentStatus)
    -- "shipment_status": null,
  , _location_id :: !(Maybe Word32)
    -- "location_id": 905684977,
  , _tracking_number :: !(Maybe Text)
    -- "tracking_number": "1Z2345",
  , _tracking_numbers :: !(Maybe [Text])
    -- "tracking_numbers": [
    --   "1Z2345"
    -- ],
  , _tracking_url :: !(Maybe Text)
    -- "tracking_url": "http://wwwapps.ups.com/etracking/tracking.cgi?InquiryNumber1=1Z2345&TypeOfInquiryNumber=T&AcceptUPSLicenseAgreement=yes&submit=Track",
  , _tracking_urls :: !(Maybe [Text])
    -- "tracking_urls": [
    --   "http://wwwapps.ups.com/etracking/tracking.cgi?InquiryNumber1=1Z2345&TypeOfInquiryNumber=T&AcceptUPSLicenseAgreement=yes&submit=Track"
    -- ],
  , _receipt :: !(Maybe Value)
    -- "receipt": {
    --   "testcase": true,
    --   "authorization": "123456"
    -- },
  , _name :: !(Maybe Text)
    -- "name": "#1001.0",
  , _admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Fulfillment/255858046",
  , _line_items :: !(Maybe [Order.LineItem])
    -- "line_items": [
  } deriving (Eq, Show)
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 1
      , omitNothingFields = True
      }
    ''Fulfillment)


-- GET /admin/api/2019-04/orders/#{order_id}/fulfillments.json
-- Retrieves fulfillments associated with an order
-- GET /admin/api/2019-04/orders/#{order_id}/fulfillments/count.json
-- Retrieves a count of fulfillments associated with a specific order
-- GET /admin/api/2019-04/orders/#{order_id}/fulfillments/#{fulfillment_id}.json
-- Receive a single Fulfillment
-- POST /admin/api/2019-04/orders/#{order_id}/fulfillments.json
-- Create a new Fulfillment
-- PUT /admin/api/2019-04/orders/#{order_id}/fulfillments/#{fulfillment_id}.json
-- Modify an existing Fulfillment
-- POST /admin/api/2019-04/orders/#{order_id}/fulfillments/#{fulfillment_id}/complete.json
-- Complete a fulfillment
-- POST /admin/api/2019-04/orders/#{order_id}/fulfillments/#{fulfillment_id}/open.json
-- Transition a fulfillment from pending to open.
-- POST /admin/api/2019-04/orders/#{order_id}/fulfillments/#{fulfillment_id}/cancel.json
-- Cancel a fulfillment
