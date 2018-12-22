@org.hibernate.annotations.NamedQueries
( { @org.hibernate.annotations.NamedQuery
    ( name = "findItemsOrderByName",
    , query = "SELECT i FROM Item i ORDER BY i.name ASC"
    )
  , @org.hibernate.annotations.NamedQuery
    ( name = "findItemBuyNowPriceGreaterThan"
    , query = "SELECT i FROM Item i WHERE i.buyNowPrice > :price"
    , timeOut = 60 // seconds
    )
  }
)
