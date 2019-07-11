# Halogen Generic Table

Directly from `Array (Record row)` to an editable Table in Halogen using `purescript-heterogenous`.

Supported Types
- `Boolean`
- `Int`
- `Number`
- `Fixed precision`
- `String`
- Enum (`Generic a rep => IsEnum rep => a`, see `Data.Generic.Rep.IsEnum`)

Type-Component Mapping
- `Boolean` -> Checkbox
- `Int`, `Number`, `Fixed`, `String` -> Input
- Enum -> Input + Dropdown

Features
- auto complete
- auto-resized Input
- undo editing by `Escape` or un`Focus` (`Blur`)
- primitive validations

# Transformations

```purescript
tableInputAdapter
  :: forall row
  .  HFoldlWithIndex RowInputMapping
      (List (String /\ CellType))
      (Record row)
      (List (String /\ CellType))
  => Array (Record row)
  -> Array (Array (String /\ CellType))

tableOutputAdapter
  :: forall row
  .  RecordableWithRelation FromCellType 
       (Map String CellType) 
       row
  => Map Int (Map String CellType) -- NOTE from nested Halogen queries
  -> Maybe (Array (Record row))
```
