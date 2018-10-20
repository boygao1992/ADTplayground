module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Select (buildComponent, empty, defaultConfig, Query(..))
import Web.DOM.ParentNode (QuerySelector(..))


-- data Query next
  -- | Init next
  -- | HandleKey KE.KeyboardEvent (H.SubscribeStatus -> next)

type InMsg = Unit

data OutMsg = Void

type IO = Aff

-- onKeyUp_ :: HTMLDocument -> (KE.KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
-- onKeyUp_ document fn = do
--   let target = HTMLDocument.toEventTarget document
--   listener <- WEET.eventListener (traverse_ fn <<< KE.fromEvent)
--   WEET.addEventListener KET.keyup listener false target
--   pure $ WEET.removeEventListener KET.keyup listener false target

-- onKeyDown_ :: HTMLDocument -> (KE.KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
-- onKeyDown_ document fn = do
--   let target = HTMLDocument.toEventTarget document
--   listener <- WEET.eventListener (traverse_ fn <<< KE.fromEvent)
--   WEET.addEventListener KET.keydown listener false target
--   pure $ WEET.removeEventListener KET.keydown listener false target

-- eval :: forall f item. Query ~> H.ComponentDSL (State f item) Query OutMsg IO
-- eval (Init next) = do
--   document <- H.liftEffect $ DOM.document =<< DOM.window
--   H.subscribe $ HQES.eventSource' (onKeyUp_ document) (Just <<< H.request <<< HandleKey)
--   H.subscribe $ HQES.eventSource' (onKeyDown_ document) (Just <<< H.request <<< HandleKey)
--   pure next
-- eval (HandleKey ev reply)
--   = do
--     H.liftEffect $ WEE.preventDefault $ KE.toEvent ev
--     H.liftEffect $ log $ KE.key ev
--     pure (reply H.Listening)

newtype Item = Item String
derive newtype instance eqItem :: Eq Item
derive newtype instance ordItem :: Ord Item
instance showItem :: Show Item where
  show (Item s) =  s


type Person =
    { name :: String
    , year :: Int
    , city :: String
    , state :: String
    }

person :: String -> Int -> String -> String -> Person
person name year city state = { name, year, city, state }

presidents :: Array Person
presidents =
    [ person "George Washington" 1732 "Westmoreland County" "Virginia"
    , person "John Adams" 1735 "Braintree" "Massachusetts"
    , person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
    , person "James Madison" 1751 "Port Conway" "Virginia"
    , person "James Monroe" 1758 "Monroe Hall" "Virginia"
    , person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
    , person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
    , person "William Henry Harrison" 1773 "Charles City County" "Virginia"
    , person "Martin Van Buren" 1782 "Kinderhook" "New York"
    , person "Zachary Taylor" 1784 "Barboursville" "Virginia"
    , person "John Tyler" 1790 "Charles City County" "Virginia"
    , person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
    , person "James K. Polk" 1795 "Pineville" "North Carolina"
    , person "Millard Fillmore" 1800 "Summerhill" "New York"
    , person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
    , person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
    , person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
    , person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
    , person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
    , person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
    , person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
    , person "Benjamin Harrison" 1833 "North Bend" "Ohio"
    , person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
    , person "William McKinley" 1843 "Niles" "Ohio"
    , person "Woodrow Wilson" 1856 "Staunton" "Virginia"
    , person "William Howard Taft" 1857 "Cincinnati" "Ohio"
    , person "Theodore Roosevelt" 1858 "New York City" "New York"
    , person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
    , person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
    , person "Herbert Hoover" 1874 "West Branch" "Iowa"
    , person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
    , person "Harry S. Truman" 1884 "Lamar" "Missouri"
    , person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
    , person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
    , person "Ronald Reagan" 1911 "Tampico" "Illinois"
    , person "Richard M. Nixon" 1913 "Yorba Linda" "California"
    , person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
    , person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
    , person "George H. W. Bush" 1924 "Milton" "Massachusetts"
    , person "Jimmy Carter" 1924 "Plains" "Georgia"
    , person "George W. Bush" 1946 "New Haven" "Connecticut"
    , person "Bill Clinton" 1946 "Hope" "Arkansas"
    , person "Barack Obama" 1961 "Honolulu" "Hawaii"
    ]

personToId :: Person -> String
personToId = _.name

personToKeyword :: Person -> String
personToKeyword = _.name

personToHTML :: forall q. Person -> H.ComponentHTML q
personToHTML { name, year } =
  HH.div_ [ HH.text name ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  app <- HA.selectElement (QuerySelector "#app")
  io <- runUI
    ( buildComponent
        { toHTML : personToHTML
        , toId : personToId
        , toKeyword : personToKeyword
        }
        { internal : empty
        , external : Just presidents
        , config : defaultConfig
        }
    )
    []
    (fromMaybe body app)

  _ <- H.liftAff $ forkAff do
    delay (Milliseconds 10000.0)
    H.liftEffect $ log "10 second"
    io.query $ H.action $ Sync presidents

  pure unit
