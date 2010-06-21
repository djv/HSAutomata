-- System-dependent utilities. Currently just a function, meant to be 
-- invoked from GHCi, to open a PDF of a graphviz string.

module Automata.System (previewDot) where

import Control.Concurrent
import Control.Monad
     
import System.Directory
import System.Exit
import System.IO
import System.Process

-- Input string should be valid Graphviz text. 
-- Opens a PDF with a rendering of the Graphviz.
-- Relies on the presence of @dot@.
-- Mac OS X only: Relies on presence of @open@.
-- Linux/GNOME users may be able to alias @open@ to @gnome-open@.
previewDot :: String -> IO ()
previewDot g = do tempDir <- getTemporaryDirectory
                  (pdfPath, pdfHandle) <- openTempFile tempDir "graph.pdf"
                  (Just dot_stdin, _stdout, _stderr, p) <- createProcess (dotProcess pdfHandle)
                  hPutStr dot_stdin g
                  exitCode <- waitForProcess p
                  hClose pdfHandle
                  openPDFIfSucceeded exitCode pdfPath
    where openPDFIfSucceeded ExitSuccess pdfPath
            = do forkIO $ system ("gnome-open " ++ pdfPath) >> return ()
                 return ()
          openPDFIfSucceeded (ExitFailure errorCode) pdfPath
            = do putStrLn $ "Conversion to PDF failed (Code " ++ show errorCode ++ ")"
                 putStrLn $ "Destination was " ++ show pdfPath
                 putStrLn "when trying to convert: \n"
                 putStrLn g
                  
          dotProcess h
            = CreateProcess { cmdspec = ShellCommand "dot -Tpdf"
                            , cwd = Nothing
                            , env = Nothing
                            , std_in = CreatePipe
                            , std_out = UseHandle h
                            , std_err = Inherit
                            , close_fds = False
                            }
