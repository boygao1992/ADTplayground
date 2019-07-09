module Polaris.UI.Block.Layout where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Halogen.HTML (HTML, IProp, ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Ocelot.Block.Builder (blockBuilder)
import Polaris.UI.Block.TextContainer as TextContainer
import Polaris.UI.Block.Heading as Heading

_layout = "Polaris-Layout" :: String

_layoutSection = "Polaris-Layout__Section" :: String
_layoutSection_secondary = "Polaris-Layout__Section--secondary" :: String
_layoutSection_fullWidth = "Polaris-Layout__Section--fullWidth" :: String
_layoutSection_oneHalf = "Polaris-Layout__Section--oneHalf" :: String
_layoutSection_oneThird = "Polaris-Layout__Section--oneThird" :: String

_layoutAnnotation = "Polaris-Layout__Annotation" :: String
_layoutAnnotationWrapper = "Polaris-Layout__AnnotationWrapper" :: String
_layoutAnnotatedSection = "Polaris-Layout__AnnotatedSection" :: String
_layoutAnnotationDescription = "Polaris-Layout__AnnotationDescription" :: String
_layoutAnnotationContent = "Polaris-Layout__AnnotationContent" :: String

data SectionType
  = Basic
  | Secondary
  | FullWidth
  | OneHalf
  | OneThird
derive instance eqSectionType :: Eq SectionType
derive instance ordSectionType :: Ord SectionType

type AnnotationSectionOptions =
  { title :: String
  , description :: String
  }

annotationSectionDefaultOptions :: AnnotationSectionOptions
annotationSectionDefaultOptions =
  { title: ""
  , description: ""
  }


layout
  :: forall p i
  . Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
layout = blockBuilder HH.div [ ClassName _layout ]

layout_
  :: forall p i
  . Array (HTML p i)
  -> HTML p i
layout_ = layout []

section
  :: forall p i
  . SectionType
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
section sectionType = blockBuilder HH.div $ ClassName <$>
  [ _layoutSection
  , case sectionType of
      Basic -> mempty
      Secondary -> _layoutSection_secondary
      FullWidth -> _layoutSection_fullWidth
      OneHalf -> _layoutSection_oneHalf
      OneThird -> _layoutSection_oneThird
  ]

annotationSection
  :: forall p i
  . AnnotationSectionOptions
  -> Array (IProp HTMLdiv i)
  -> Array (HTML p i)
  -> HTML p i
annotationSection { title, description } iprops inner =
  HH.div [ HP.class_ $ ClassName _layoutAnnotatedSection ]
  [ HH.div [ HP.class_ $ ClassName _layoutAnnotationWrapper ]
    [ HH.div [ HP.class_ $ ClassName _layoutAnnotation ]
      [ TextContainer.textContainer_ TextContainer.defaultOptions
        [ Heading.heading_
          [ HH.text title
          ]
        , HH.div [ HP.class_ $ ClassName _layoutAnnotationDescription ]
          [ HH.p_
            [ HH.text description
            ]
          ]
        ]
      ]
    , blockBuilder HH.div [ ClassName _layoutAnnotationContent ] iprops inner
    ]
  ]
