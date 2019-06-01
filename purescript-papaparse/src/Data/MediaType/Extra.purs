module Data.MediaType.Extra
( module Export
, applicationXLSX
) where

import Data.MediaType (MediaType(..))

import Data.MediaType (MediaType(..)) as Export
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON, applicationJavascript, applicationOctetStream, applicationXML, imageGIF, imageJPEG, imagePNG, multipartFormData, textCSV, textHTML, textPlain, textXML) as Export

applicationXLSX :: MediaType
applicationXLSX = MediaType "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
