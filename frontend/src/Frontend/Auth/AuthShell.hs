{-# LANGUAGE OverloadedStrings #-}

module Frontend.Auth.AuthShell where

import Classh
import Classh.Reflex

import Templates.Types
import Reflex.Dom.Core

import Data.Text as T

authFormTemplate
  :: Template t m
  => m ()
  -> m a
  -> m a
authFormTemplate _leftSide form = do
  centerSimple $ do
    elClass "div" "p-4 lg:p-12 rounded-3xl w-full max-w-xl" $ do
      form

authFormLabel :: DomBuilder t m => T.Text -> m ()
authFormLabel labelText = do
  elClass "div" $(classh' [mb .~~ TWSize 2]) $ do
    textS $(classh' [text_size .~~ XL, text_font .~~ Font_Custom "Sarabun", text_color .|~ [White, White, Gray C700]])
      labelText

-- | TODO: different funcs based on type
authFormTextInput
  :: DomBuilder t m
  => T.Text -- type eg email, password
  -> T.Text -- placeholder
  -> m (InputEl t m)
authFormTextInput typeHtml placeholder  = do
  inputElement $ def
    & initialAttributes .~ ("type" =: typeHtml <> "class" =: (inputClass <&> inputTextClass) <> "placeholder" =: placeholder)
  where
    inputClass = $(classh' [ w .~~ TWSize_Full
                           , px .~~ TWSize 4
                           , py .~~ TWSize 2
                           , bw .~~ B1
                           , bc .~ [("def", Gray C300), ("focus", hex "00B9DA")]
                           , br .~~ R_Lg
                           ])
    inputTextClass = $(classh' [text_font .~~ Font_Custom "Sarabun", text_size .~~ LG])

authFormTitle :: DomBuilder t m => T.Text -> m ()
authFormTitle title = do
  elClass "h1" $(classh' [mb .~~ TWSize 4, custom .~ "text-center"]) $ do
    textS $(classh' [text_size .~~ XL4, text_font .~~ Font_Custom "Sarabun", text_color .|~ [White, White, Gray C700]])
      title

authFormRow :: DomBuilder t m => m a -> m a
authFormRow = elClass "div" $(classh' [custom .~ "flex flex-col", mt .~~ TWSize 4])

niceInput :: DomBuilder t m => T.Text -> m (InputElement EventResult (DomBuilderSpace m) t)
niceInput label = do
  authFormRow $ do
    authFormLabel label
    authFormTextInput "text" $ "Enter " <> label

hectorRecommendation :: DomBuilder t m => m ()
hectorRecommendation = do
  elClass "div" "text-left flex flex-col justify-center px-12" $ do
    elClass "h2" "text-white font-[Sarabun] text-xl md:text-6xl" $ do
      text "Empower your job seekers."
    elClass "p" "text-white font-[Sarabun] text-md md:text-2xl mt-4" $ do
      text "Ready your candidates with personalized feedback, exercises, and unlimited mock interviews."
  elClass "div" "absolute bottom-20 left-1/2 transform -translate-x-1/2 -mb-8 bg-white bg-opacity-20 md:w-10/12 h-60 lg:h-56 rounded-2xl shadow-xl flex flex-col items-center justify-center p-6" $ do
    elClass "div" "flex items-center space-x-6 pb-2" $ do
      elClass "div" "flex-col" $ do
        elClass "p" "text-white font-[Sarabun] text-s md:text-md lg:text-xl md:mb-2 overflow-hidden text-ellipsis px-2 py-2 italic" $ text "''I was blown away by the user experience and personalized feedback provided by this mock-interview platform. Highly recommend for anyone looking to nail their next job interview!''"
        elClass "p" "text-white font-[Sarabun] font-bold md:text-xl" $ text "Hector Munachi"
        elClass "p" "text-white font-[Sarabun] font-bold md:text-lg" $ text "Frontend Engineer"
