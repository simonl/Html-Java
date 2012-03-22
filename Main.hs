module Main where

import XML as XML
import System.IO as IO

main = do
	text <- getContents
	let res = XML.entirety text
	case res of
		Nothing -> IO.hPutStrLn IO.stderr "Invalid program!"
		Just prog -> print prog



