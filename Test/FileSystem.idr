module Test.FileSystem

import Control.Monad.Free
import Control.Monad.AsyncJS_IO
import Node.Free.FileSystem
import Node.Sync.FileSystem
import Node.Async.FileSystem


exclaimHello : FileSystem Unit
exclaimHello = do
  writeFile "foo.txt" "hello, world!"
  contents <- readFile "foo.txt"
  writeFile "foo.txt" (contents <+> "!")

copyHello : FileSystem Unit
copyHello = do
  contents <- readFile "foo.txt"
  writeFile "bar.txt" contents

consoleLog : String -> AsyncJS_IO ()
consoleLog s = liftJS_IO (putStrLn' s)


namespace Main
  export
  main : JS_IO ()
  main = do
    foldFree interpret exclaimHello
    foldFree interpret copyHello

    {-
    runAsync $ do
      c <- foldFree interpret (readFile "bar.txt")
      consoleLog c
    -}
