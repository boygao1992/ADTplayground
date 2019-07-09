{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Shopify.Data.Orders.Fulfillments.Fulfillment where

import RIO
import Data.Aeson (Value)
import Data.Aeson.TH.Util (deriveJSONDropTwo)
import Lens.Micro.TH.Util (makeLensesDropOne)
import Shopify.Data.Orders.OrderId (OrderId)
import Shopify.Data.Orders.Fulfillments.FulfillmentId (FulfillmentId)
import Shopify.Data.Orders.Fulfillments.Status (Status)
import Shopify.Data.Orders.Fulfillments.TrackingCompany (TrackingCompany)
import Shopify.Data.Orders.Fulfillments.ShipmentStatus (ShipmentStatus)
import Shopify.Data.Orders.LineItem as Order (LineItem)


data Fulfillment = Fulfillment
  { __id :: !(Maybe FulfillmentId)
    -- "id": 255858046,
  , __order_id :: !(Maybe OrderId)
    -- "order_id": 450789469,
  , __status :: !(Maybe Status)
    -- "status": "failure",
  , __created_at :: !(Maybe Text)
    -- "created_at": "2019-05-03T11:02:30-04:00",
  , __service :: !(Maybe Text)
    -- "service": "manual",
  , __updated_at :: !(Maybe Text)
    -- "updated_at": "2019-05-03T11:02:30-04:00",
  , __tracking_company :: !(Maybe TrackingCompany)
    -- "tracking_company": null,
  , __shipment_status :: !(Maybe ShipmentStatus)
    -- "shipment_status": null,
  , __location_id :: !(Maybe Word64)
    -- "location_id": 905684977,
  , __tracking_number :: !(Maybe Text)
    -- "tracking_number": "1Z2345",
  , __tracking_numbers :: !(Maybe [Text])
    -- "tracking_numbers": [
    --   "1Z2345"
    -- ],
  , __tracking_url :: !(Maybe Text)
    -- "tracking_url": "http://wwwapps.ups.com/etracking/tracking.cgi?InquiryNumber1=1Z2345&TypeOfInquiryNumber=T&AcceptUPSLicenseAgreement=yes&submit=Track",
  , __tracking_urls :: !(Maybe [Text])
    -- "tracking_urls": [
    --   "http://wwwapps.ups.com/etracking/tracking.cgi?InquiryNumber1=1Z2345&TypeOfInquiryNumber=T&AcceptUPSLicenseAgreement=yes&submit=Track"
    -- ],
  , __receipt :: !(Maybe Value)
    -- "receipt": {
    --   "testcase": true,
    --   "authorization": "123456"
    -- },
  , __name :: !(Maybe Text)
    -- "name": "#1001.0",
  , __admin_graphql_api_id :: !(Maybe Text)
    -- "admin_graphql_api_id": "gid://shopify/Fulfillment/255858046",
  , __line_items :: !(Maybe [Order.LineItem])
    -- "line_items": [
  } deriving (Eq, Show)
$(makeLensesDropOne ''Fulfillment)
$(deriveJSONDropTwo ''Fulfillment)


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
