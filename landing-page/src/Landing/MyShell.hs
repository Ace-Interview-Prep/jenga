module Landing.MyShell where

import Lamarckian.Types
import Landing.Static -- Obelisk.Generated.Static

import Reflex.Dom.Core
import Data.Text (Text)

type Href = Text

preloadStyleLink :: DomBuilder t m => Href -> m ()
preloadStyleLink href = elAttr "link"
  ( "rel" =: "preload"
    <> "as" =: "style"
    <> "href" =: href
    <> "onload" =: "this.onload=null;this.rel='stylesheet';"
  ) blank

styleLink :: DomBuilder t m => Href -> m ()
styleLink href = elAttr "link"
  ( "rel" =: "stylesheet"
    <> "type" =: "text/css"
    <> "href" =: href
  ) blank


title :: DomBuilder t m => Text -> m ()
title t = el "title" $ text t

jsScript :: DomBuilder t m => Text -> m ()
jsScript src = elAttr "script" ("type" =: "application/javascript" <> "defer" =: "" <> "src" =: src) blank

icon :: DomBuilder t m => Text -> m ()
icon src = elAttr "link" ("rel" =: "icon" <> "href" =: src) blank

charSetUTF8 :: DomBuilder t m => m ()
charSetUTF8 = elAttr "meta" ("charset" =: "UTF-8") blank

description :: DomBuilder t m => Text -> m ()
description d = elAttr "meta" ("name" =: "description" <> "content" =: d) blank

frontendHead :: DomBuilder t m => m ()
frontendHead = do
  -- Config
  charSetUTF8 -- This should always be first

  title "Ace Talent"
  icon "empty"
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no") blank
  -- todo: get from config, so that non-techies can update this easily
  description "Discover exceptional tech talent with Ace Talent. We connect businesses to upskilled professionals from top organizations, ready for remote or on-site roles. Our personalized approach ensures candidates align with your company's vision and culture. Experience efficient hiring and seamless integration with a partner who truly cares about your success. For developers, Ace Talent Community is the best way to learn how to code and get hired as a software engineer"
  -- JS libraries
  jsScript $(static "js/lib.js")

  -- CSS
  styleLink $(static "css/styles.css")
  -- TODO: Only in development
  elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank

  -- FONTS
  preloadStyleLink "https://fonts.googleapis.com/css2?family=Karla:ital,wght@0,200;0,300;0,400;0,500;0,600;0,700;0,800;1,200;1,300;1,400;1,500;1,600;1,700;1,800&display=swap"
  preloadStyleLink "https://fonts.googleapis.com/icon?family=Material+Icons"
  preloadStyleLink "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
  preloadStyleLink "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"
  preloadStyleLink "https://fonts.googleapis.com/css2?family=Sarabun&display=swap"
  preloadStyleLink "https://fonts.gstatic.com"

mainShell :: StaticWidget' r x () -> StaticWidget' r x ()
mainShell body = do
  elAttr "html" ("lang" =: "en") $ do
    el "head" $ do
      frontendHead
      --style
    el "body" $ elClass "div" "fixed top-0 left-0 w-full h-full overflow-auto z-10 scroll-smooth" $ do
      body

blogShell :: DomBuilder t m => m () -> m () --StaticWidget' r x () -> StaticWidget' r x ()
blogShell body = do
  elAttr "html" ("lang" =: "en") $ do
    el "head" $ do
      --style
      el "style" $ text "a { color: #00B9DA; text-decoration: none; }"
    el "body" $ elClass "div" "fixed top-0 left-0 w-full h-full overflow-auto z-10 scroll-smooth" $ do
      body


-- renderInMainShell' :: StaticWidget' r x () -> Q (StaticWidget' r x ())
-- renderInMainShell' = pure . mainShell

-- renderInMainShell :: StaticWidget' r x () -> Q BS.ByteString
-- renderInMainShell = runIO . runStaticWidget . mainShell

-- renderInMainShellIO :: StaticWidget' r x () -> IO BS.ByteString
-- renderInMainShellIO = runStaticWidget . mainShell
