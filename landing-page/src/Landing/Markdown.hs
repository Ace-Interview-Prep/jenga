module Landing.Markdown where

import Landing.Impl
import Landing.MyShell
import Lamarckian.Template
import Common.Route
import Obelisk.Route
import Reflex.Dom.Core
import Language.Haskell.TH
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

markdownRoute
  :: R MainLandingRoute
  -> T.Text
  -> Q Exp
markdownRoute r md = do
  getPlainTextPageCompiled r $ markdownBuilder md

markdownBuilder :: T.Text -> IO BS.ByteString
markdownBuilder md = do
  let mappy = Map.fromList [(slotKey, T.unpack md)]
  str <- renderStaticTemplate mappy $ blogShell $ elClass "div" "containerBlog" $ templateSlot $ T.pack slotKey
  pure . T.encodeUtf8 . T.pack $ str
