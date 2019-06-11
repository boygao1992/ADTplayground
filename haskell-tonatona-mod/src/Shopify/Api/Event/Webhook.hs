module Shopify.Api.Event.Webhook where

import RIO
import Servant

type Topic = Header "X-Shopify-Topic" Text
-- X-Shopify-Topic: orders/create
type HmacSha256 = Header "X-Shopify-Hmac-Sha256" Text
-- X-Shopify-Hmac-Sha256: XWmrwMey6OsLMeiZKwP4FppHH3cmAiiJJAweH5Jo4bM=
type ShopDomain = Header "X-Shopify-Shop-Domain" Text
-- X-Shopify-Shop-Domain: johns-apparel.myshopify.com
type APIVersion = Header "X-Shopify-API-Version" Text
-- X-Shopify-API-Version: 2019-04
