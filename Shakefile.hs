{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Development.Shake
import Development.Shake.FilePath

import Debug.Trace

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_shake"} $ do
    want ["_site/index.html"]

    "css/tachyons.min.css" %> \out -> do
        files <- getDirectoryFiles "src" ["//*.css"]
        need $ fmap ("src" </>) files
        command_
            []
            "nix-shell"
            [ "-p nodejs_24"
            , "--run"
            , "npm exec --package=tachyons-cli@1.3.3 -- tachyons src/tachyons.css --minify > css/tachyons.min.css"
            ]

    "_site/index.html" %> \_ -> do
        need ["css/tachyons.min.css", "site.hs"]
        command_ [] "cabal" ["run", "site", "--", "build"]

    phony "watch" $ do
        need ["css/tachyons.min.css", "site.hs"]
        command_  [] "cabal" ["run", "site", "--", "watch"]

    phony "clean" $ do
        removeFilesAfter "_shake" ["//*"]

    phony "distclean" $ do
        removeFilesAfter "_shake" ["//*"]
        removeFilesAfter "css" ["tachyons.min.css"]
        removeFilesAfter "_site" ["//*"]
