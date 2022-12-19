{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.String
import Lucid.Svg


foreign import javascript unsafe "((html) => { return setInner(html); })"
  setInnerHtml :: CString -> IO ()


svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [width_ "300", height_ "200"]


image :: Svg ()
image = rect_ [width_ "100%", height_ "100%", fill_ "red"]


main :: IO ()
main = do
  setInnerHtml =<< newCString (show $ svg image)
  print $ svg image
