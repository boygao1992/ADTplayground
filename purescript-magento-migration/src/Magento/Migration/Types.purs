module Magento.Migration.Types where

import Data.Nullable (Nullable)

type CustomerCore =
  { entity_id :: Int
  , website_id :: Int
  , email :: String
  }

type CustomerFull =
  CustomerUpdate
  ( updated_at :: String
  )

type CustomerUpdate r =
  CustomerInsert
  ( entity_id :: Int
  | r
  )

type CustomerInsert r =
  { website_id :: Int
  , email :: String
  , group_id :: Int
  , store_id :: Int
  , created_in :: Nullable String
  , prefix :: Nullable String
  , firstname :: Nullable String
  , middlename :: Nullable String
  , lastname :: Nullable String
  , suffix :: Nullable String
  , password_hash :: Nullable String
  , gender :: Nullable Int
  | r
  }

type CustomerMappingTarget r =
  { source_customer_id :: Int
  , target_customer_id :: Int
  , synced_at :: String
  , last_update :: String
  | r
  }

type AddressCore =
  { entity_id :: Int
  , uuid :: String
  }

type AddressFull =
  AddressUpdate
    ( parent_id :: Int
    , updated_at :: String
    )

type AddressUpdate r =
  { entity_id :: Int
  , city :: Nullable String
  , company :: Nullable String
  , country_id :: Nullable String
  , fax :: Nullable String
  , firstname :: Nullable String
  , lastname :: Nullable String
  , middlename :: Nullable String
  , postcode :: Nullable String
  , prefix :: Nullable String
  , region :: Nullable String
  , region_id :: Nullable Int
  , street :: Nullable String
  , suffix :: Nullable String
  , telephone :: Nullable String
  | r
  }

type AddressInsert r =
  { parent_id :: Int
  , uuid :: String
  , city :: Nullable String
  , company :: Nullable String
  , country_id :: Nullable String
  , fax :: Nullable String
  , firstname :: Nullable String
  , lastname :: Nullable String
  , middlename :: Nullable String
  , postcode :: Nullable String
  , prefix :: Nullable String
  , region :: Nullable String
  , region_id :: Nullable Int
  , street :: Nullable String
  , suffix :: Nullable String
  , telephone :: Nullable String
  | r
  }

type AddressMappingTargetInsert r =
  AddressMappingTargetUpdate
  ( uuid :: String
  | r
  )
type AddressMappingTargetUpdate r =
  { source_address_id :: Int
  , target_address_id :: Int
  , synced_at :: String
  , last_update :: String
  | r
  }

type SalesFlatCreditMemoGrid =
  { entity_id :: Int
  , store_id :: Nullable Int
  , store_to_order_rate :: Nullable Number
  , base_to_order_rate :: Nullable Number
  , grand_total :: Nullable Number
  , store_to_base_rate :: Nullable Number
  , base_to_global_rate :: Nullable Number
  , base_grand_total :: Nullable Number
  , order_id :: Nullable Int
  , creditmemo_status :: Nullable Int
  , state :: Nullable Int
  , invoice_id :: Nullable Int
  , store_currency_code :: Nullable String
  , order_currency_code :: Nullable String
  , base_currency_code :: Nullable String
  , global_currency_code :: Nullable String
  , increment_id :: Nullable String
  , order_increment_id :: Nullable String
  , created_at :: Nullable String
  , order_created_at :: Nullable String
  , billing_name :: Nullable String
  }

type SalesFlatInvoiceGrid =
  { entity_id :: Int
  , store_id :: Nullable Int
  , base_grand_total :: Nullable Number
  , grand_total :: Nullable Number
  , order_id :: Nullable Int
  , state :: Nullable Int
  , store_currency_code :: Nullable String
  , order_currency_code :: Nullable String
  , base_currency_code :: Nullable String
  , global_currency_code :: Nullable String
  , increment_id :: Nullable String
  , order_increment_id :: Nullable String
  , created_at :: Nullable String
  , order_created_at :: Nullable String
  , billing_name :: Nullable String
  }

type SalesFlatOrderGrid =
  { entity_id :: Int
  , status :: Nullable String
  , store_id :: Nullable Int
  , store_name :: Nullable String
  , customer_id :: Nullable Int
  , base_grand_total :: Nullable Number
  , base_total_paid :: Nullable Number
  , grand_total :: Nullable Number
  , total_paid :: Nullable Number
  , increment_id :: Nullable String
  , base_currency_code :: Nullable String
  , order_currency_code :: Nullable String
  , shipping_name :: Nullable String
  , billing_name :: Nullable String
  , created_at :: Nullable String
  , updated_at :: Nullable String
  }

type SalesFlatShipmentGrid =
  { entity_id :: Int
  , store_id :: Nullable Int
  , total_qty :: Nullable Number
  , order_id :: Nullable Int
  , shipment_status :: Nullable Int
  , increment_id :: Nullable String
  , order_increment_id :: Nullable String
  , created_at :: Nullable String
  , order_created_at :: Nullable String
  , shipping_name :: Nullable String
  }
