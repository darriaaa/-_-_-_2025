{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_display_schedule (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "display_schedule"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Console application to manage a display class schedule"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
