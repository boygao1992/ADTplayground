current API doesn't support definition of recursive type

Though `objectTypeRec` takes a thunk as a lazy step, PureScript type synonym (for Record in this case) doesn't support recursive reference so we need `newtype` definition (a lazy step in type inference) which is not supported by current API (both `objectType`, `ObjectTypeRec` take a Record of `ObjectTypeField`s and doesn't allow `newtype` of a Record).

should probably start implementing my own type wrapper
