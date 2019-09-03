two layers of restrictions for data access
- (type-level / structural / path   / column / `Select`) restrict data fields to retrieve
- (term-level / value      / filter / row    / `Where`)  restrict data record

GraphQL as a query language for RDBMS
- explicit equality condition (`On`) for joins is not necessary
  - it's well defined in database schema through foreign key constraint, and explicit `On` in queries doesn't loosen the coupling between queries and specific schema any further
- missing built-in term-level filtering in its query language
  - specific filters can be baked in path definition in schema

