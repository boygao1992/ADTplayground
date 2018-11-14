module ResourceNames where

import Prelude
import RouteParams as RP

-- Utils
route :: String -> String
route = (":" <> _)

-- Resource Names
root :: String
root = "/"

logger :: String
logger = root <> "logger"

todos :: String
todos = root <> "todos"

todosCreate :: String
todosCreate = todos <> "/" <> "create"

todosUpdate :: String
todosUpdate = todos <> "/" <> "update" <> "/" <> route RP.id

todosDelete :: String
todosDelete = todos <> "/" <> "delete" <> "/" <> route RP.id

todosDone :: String
todosDone = todos <> "/" <> "done" <> "/" <> route RP.id

