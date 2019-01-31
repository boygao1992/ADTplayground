module GraphQL.Type.Data.Field where

-- | Field
data Field (name :: Symbol) (argType :: ArgType) typ (fieldType :: FieldType) target

-- | FieldType = ScalarField | RelationalField
foreign import kind FieldType
foreign import data ScalarField :: FieldType
foreign import data RelationalField :: FieldType
data FTProxy (fieldType :: FieldType) = FTProxy

-- | Args = NoArgs | WithArgs a
foreign import kind ArgType
foreign import data NoArg :: ArgType
foreign import data WithArgs :: Type -> ArgType
data AProxy (argType :: ArgType) = AProxy
