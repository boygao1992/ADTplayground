module Polaris.UI.Block.DataTable where

import Prelude

import DOM.HTML.Indexed (HTMLtable, HTMLtd, HTMLth, HTMLtr)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
import Ocelot.Block.Builder (blockBuilder)

_tableCollapsed = "Polaris-DataTable--collapsed" :: String
_tableHasFooter = "Polaris-DataTable--hasFooter" :: String

_table = "Polaris-DataTable__Table" :: String

_navigation = "Polaris-DataTable__Navigation" :: String

_row = "Polaris-DataTable__TableRow" :: String

_cell = "Polaris-DataTable__Cell" :: String
_cellHeader = "Polaris-DataTable__Cell--header" :: String
_cellNumeric = "Polaris-DataTable__Cell--numeric" :: String
_cellFixed = "Polaris-DataTable__Cell--fixed" :: String
_cellTruncated = "Polaris-DataTable__Cell--truncated" :: String
_cellSortable = "Polaris-DataTable__Cell--sortable" :: String
_cellSorted = "Polaris-DataTable__Cell--sorted" :: String
_cellTotal = "Polaris-DataTable__Cell--total" :: String
_cellFooter = "Polaris-DataTable__Cell--footer" :: String

tableClasses :: Array HH.ClassName
tableClasses = HH.ClassName <$>
  [ _table
  ]

table
  :: ∀ p i
  . Array (IProp HTMLtable i)
  -> Array (HTML p i)
  -> HTML p i
table = blockBuilder HH.table tableClasses

table_
  :: ∀ p i
  . Array (HTML p i)
  -> HTML p i
table_ = table []

rowClasses :: Array HH.ClassName
rowClasses = HH.ClassName <$>
  [ _row
  ]

row
  :: ∀ p i
  . Array (IProp HTMLtr i)
  -> Array (HTML p i)
  -> HTML p i
row = blockBuilder HH.tr rowClasses

row_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
row_ = row []


headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ _cell
  , _cellHeader
  ]

header
  :: ∀ p i
   . Array (IProp HTMLth i)
  -> Array (HTML p i)
  -> HTML p i
header = blockBuilder HH.th headerClasses

header_
  :: ∀ p i
   . Array (HTML p i)
  -> HTML p i
header_ = header []

cellClasses :: Array HH.ClassName
cellClasses = HH.ClassName <$>
  [ _cell
  ]

cell
  :: forall p i
  . Array (IProp HTMLtd i)
  -> Array (HTML p i)
  -> HTML p i
cell = blockBuilder HH.td cellClasses

cell_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
cell_ = cell []
