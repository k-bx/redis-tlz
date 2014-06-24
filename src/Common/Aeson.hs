module Common.Aeson where

import Data.Aeson.Types (Options)
import           Data.Aeson.TH          (constructorTagModifier, defaultOptions,
                                         fieldLabelModifier)
import           Data.String.Additional (camelToUnderscore)

jsonOptions :: Int -> Options
jsonOptions n = defaultOptions { fieldLabelModifier = camelToUnderscore . drop n
                               , constructorTagModifier = camelToUnderscore }
