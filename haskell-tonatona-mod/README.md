# Shopify RESTful Api

- use `servant-server` to faithfully encode api specifications
- use `servant-client` to auto generate client request queries
  - `.json` suffix
    - Servant.Client.Core.Suffix
    - Shopify.Servant.DotJSON
  - OAuth header
    - Shopify.Servant.Client.Auth.OAuth

## Progress

Base Url
- `/admin/api/#{api_version}`

### Necessary

DONE (10/10) Customers > Customer
- `/customers.json`
  - GET
  - POST
- `/customers/count.json`
  - GET
- `/customers/search.json`
  - GET
- `/customers/#{customer_id}.json`
  - GET
  - PUT
  - DELETE
- `/customers/#{customer_id}/account_activation_url.json`
  - POST
- `/customers/#{customer_id}/orders.json`
  - GET
- `/customers/#{customer_id}/send_invite.json`
  - POST

DONE (7/7) Customers > Customer Address
- `/customers/#{customer_id}/addresses.json`
  - GET
  - POST
- `/customers/#{customer_id}/addresses/default.json`
  - PUT
- `/customers/#{customer_id}/addresses/set.json`
  - PUT
- `/customers/#{customer_id}/addresses/#{address_id}.json`
  - GET
  - PUT
  - DELETE

DONE (6/6) Products > Product
- `/products.json`
  - GET
  - POST
- `/products/count.json`
  - GET
- `/products/#{product_id}.json`
  - GET
  - PUT
  - DELETE

TODO (0/6) Products > Product Variant
- `/products/#{product_id}/variants.json`
  - GET
  - POST
- `/products/#{product_id}/variants/count.json`
  - GET
- `/products/#{product_id}/variants/#{variant_id}.json`
  - GET
  - PUT
  - DELETE

DONE (5/5) Online Store > Theme
- `/themes.json`
  - GET
  - POST
- `/themes/#{theme_id}.json`
  - GET
  - PUT
  - DELETE

DONE (4/4) Online Store > Asset
- `/themes/#{theme_id}/assets.json`
  - GET
  - PUT
- `/themes/#{theme_id}/assets.json?asset[key]=#{asset_key}`
  - GET
  - DELETE

DONE (6/6) Online Store > Script Tag
- `/script_tags.json`
  - GET
  - POST
- `/script_tags/count.json`
  - GET
- `/script_tags/#{script_tag_id}.json`
  - GET
  - PUT
  - DELETE

### Others

(7) Customers > Customer Saved Search
- `/customer_saved_searches.json`
  - GET
  - POST
- `/customer_saved_searches/count.json`
  - GET
- `/customer_saved_searches/#{customer_saved_search_id}.json`
  - GET
  - PUT
  - DELETE
- `/customer_saved_searches/#{customer_saved_search_id}/customers.json`
  - GET

(6) Products > Product Image
- `/products/#{product_id}/images.json`
  - GET
  - POST
- `/products/#{product_id}/images/count.json`
  - GET
- `/products/#{product_id}/images/#{image_id}.json`
  - GET
  - PUT
  - DELETE

(6) Products > Collect
- `/collects.json`
  - GET
  - POST
- `/collects/count.json`
  - GET
- `/collects/#{collect_id}.json`
  - GET
  - PUT
  - DELETE

(7) Products > Smart Collection
- `/smart_collections.json`
  - GET
  - POST
- `/smart_collections/count.json`
  - GET
- `/smart_collections/#{smart_collection_id}.json`
  - GET
  - PUT
  - DELETE
- `/smart_collections/#{smart_collection_id}/order.json`
  - GET

(6) Products > Custom Collection
- `/custom_collections.json`
  - GET
  - POST
- `/custom_collections/count.json`
  - GET
- `/custom_collections/#{custom_collection_id}.json`
  - GET
  - PUT
  - DELETE

(9) Orders > Order
- `/orders.json`
  - GET
  - POST
- `/orders/count.json`
  - GET
- `/orders/#{order_id}.json`
  - GET
  - PUT
  - DELETE
- `/orders/#{order_id}/cancel.json`
  - POST
- `/orders/#{order_id}/close.json`
  - POST
- `/orders/#{order_id}/open.json`
  - POST

(2) Orders > Abandoned Checkouts
- `/checkouts.json`
  - GET
- `/checkouts/count.json`
  - GET

(4) Orders > Refund
- `orders/#{order_id}/refunds.json`
  - GET
  - POST
- `orders/#{order_id}/refunds/calculate.json`
  - POST
- `orders/#{order_id}/refunds/#{refund_id}.json`
  - GET

(4) Orders > Transaction
- `/orders/#{order_id}/transactions.json`
  - GET
  - POST
- `/orders/#{order_id}/transactions/count.json`
  - GET
- `/orders/#{order_id}/transactions/#{transaction_id}.json`
  - GET

(8) Orders > Draft Order
- `draft_orders.json`
  - GET
  - POST
- `draft_orders/count.json`
  - GET
- `draft_orders/send_invoice.json`
  - POST
- `draft_orders/#{draft_order_id}.json`
  - GET
  - PUT
  - DELETE
- `draft_orders/#{draft_order_id}/complete.json`
  - PUT

(5) Orders > Order Risk
- `/orders/#{order_id}/risks.json`
  - GET
  - POST
- `/orders/#{order_id}/risks/#{risk_id}.json`
  - GET
  - PUT
  - DELETE
