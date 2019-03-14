module CarQuery.Api.GetTrims where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), either)
import Data.String.CodeUnits (drop, dropRight) as String
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS

type Response =
  { "Trims" :: Array Trim
  }


{-
  "model_id": "78",
  "model_make_id": "abarth",
  "model_name": "Grande Punto",
  "model_trim": "",
  "model_year": "2007",
  "model_body": null,
  "model_engine_position": "Front",
  "model_engine_cc": "1368",
  "model_engine_cyl": "4",
  "model_engine_type": "in-line",
  "model_engine_valves_per_cyl": null,
  "model_engine_power_ps": "152",
  "model_engine_power_rpm": "5500",
  "model_engine_torque_nm": "312",
  "model_engine_torque_rpm": "3000",
  "model_engine_bore_mm": null,
  "model_engine_stroke_mm": null,
  "model_engine_compression": null,
  "model_engine_fuel": "Gasoline",
  "model_top_speed_kph": null,
  "model_0_to_100_kph": null,
  "model_drive": "Rear",
  "model_transmission_type": "Manual",
  "model_seats": null,
  "model_doors": "2",
  "model_weight_kg": null,
  "model_length_mm": null,
  "model_width_mm": null,
  "model_height_mm": null,
  "model_wheelbase_mm": null,
  "model_lkm_hwy": null,
  "model_lkm_mixed": null,
  "model_lkm_city": null,
  "model_fuel_cap_l": null,
  "model_sold_in_us": "0",
  "model_co2": null,
  "model_make_display": "Abarth",
  "make_display": "Abarth",
  "make_country": "Italy"
-}

{-
  "model_id": "75188",
  "model_make_id": "Lexus",
  "model_name": "CT 200h",
  "model_trim": "4dr Hatchback (1.8L 4cyl gas/electric hybrid CVT)",
  "model_year": "2019",
  "model_body": "Compact Cars",
  "model_engine_position": "Front",
  "model_engine_cc": "1800",
  "model_engine_cyl": "4",
  "model_engine_type": "Inline",
  "model_engine_valves_per_cyl": "4",
  "model_engine_power_ps": "134",
  "model_engine_power_rpm": null,
  "model_engine_torque_nm": "142",
  "model_engine_torque_rpm": null,
  "model_engine_bore_mm": null,
  "model_engine_stroke_mm": null,
  "model_engine_compression": "13",
  "model_engine_fuel": "Regular Unleaded",
  "model_top_speed_kph": null,
  "model_0_to_100_kph": null,
  "model_drive": "Front Wheel Driv",
  "model_transmission_type": "Automatic",
  "model_seats": null,
  "model_doors": "4",
  "model_weight_kg": "3130",
  "model_length_mm": null,
  "model_width_mm": null,
  "model_height_mm": null,
  "model_wheelbase_mm": null,
  "model_lkm_hwy": "40.0",
  "model_lkm_mixed": "42.0",
  "model_lkm_city": "43.0",
  "model_fuel_cap_l": "12",
  "model_sold_in_us": "1",
  "model_co2": null,
  "model_make_display": "Lexus",
  "make_display": "Lexus",
  "make_country": "Japan"
-}
type Trim =
  { model_id :: String
  , model_trim :: String
  , model_make_id :: String
  , model_make_display :: String
  , model_name :: String
  , model_year :: String
  , model_body :: String
  }

-- https://www.carqueryapi.com/api/0.3/?callback=?&cmd=getTrims&[params]
getTrims :: String -> Aff (Array Trim)
getTrims make = do
  res <- AX.get ResponseFormat.string url
  pure $ case res.body of
    Left error -> []
    Right body -> either (const []) identity do
      let jsonStr = String.drop 2 <<< String.dropRight 2 $ body
      json <- jsonParser jsonStr
      (response :: Response) <- decodeJson json
      pure $ response."Trims"
  where
    url = "https://www.carqueryapi.com/api/0.3/?callback=?&cmd=getTrims&make=" <>  make

writeTrims :: String -> Aff Unit
writeTrims make = do
  res <- AX.get ResponseFormat.string url
  for_ res.body \body -> do
    liftEffect $ FS.writeTextFile UTF8 filePath <<< String.drop 2 <<< String.dropRight 2 $ body
  where
    url = "https://www.carqueryapi.com/api/0.3/?callback=?&cmd=getTrims&make=" <>  make

    filePath :: String
    filePath = "./data/CarQuery/trims/" <> make <> ".json"
