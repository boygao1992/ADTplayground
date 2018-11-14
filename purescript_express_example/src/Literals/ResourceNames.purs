module ResourceNames where

import Prelude
import RouteParams as RP

-- Utils
route :: String -> String
route = (":" <> _)

-- Resource Names
root :: String
root = "/"

list :: String
list = root <> "list"

create :: String
create = root <> "create"

update :: String
update = root <> "update" <> "/" <> route RP.id

delete :: String
delete = root <> "delete" <> "/" <> route RP.id

done :: String
done = root <> "done" <> "/" <> route RP.id

logger :: String
logger = root <> "logger"
