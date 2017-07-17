module Node.Async.FileSystem

import Node.Free.FileSystem
import Control.Monad.AsyncJS_IO
import Data.JSError

%lib Node "fs"
%access public export

toPtr : (JSError -> JS_IO ()) -> (Ptr -> JS_IO ())
toPtr errorCb = (\e => errorCb $ MkJSError e)


writeFile' : String -> String -> (JSError -> JS_IO ()) -> (() -> JS_IO ()) -> JS_IO ()
writeFile' path contents error success =
  foreign FFI_JS "fs.writeFile(%0, %1, function(err, data) { %3(data) })"
    (String -> String -> JsFn (Ptr -> JS_IO ()) -> JsFn (() -> JS_IO ()) -> JS_IO ()) path contents (MkJsFn $ toPtr error) (MkJsFn success)

writeFileAsync : String -> String -> AsyncJS_IO ()
writeFileAsync path contents = MkAsync $ \e => \f => writeFile' path contents (\x => e x) (\x => f x)


readFile' : String -> (JSError -> JS_IO ()) -> (String -> JS_IO ()) -> JS_IO ()
readFile' path error success = foreign FFI_JS "fs.readFile(%0, 'utf8', function(err, data) { err ? %1(err) : %2(data) })"
  (String -> JsFn (Ptr -> JS_IO ()) -> JsFn (String -> JS_IO ()) -> JS_IO ()) path (MkJsFn $ toPtr error) (MkJsFn success)

readFileAsync : String -> AsyncJS_IO String
readFileAsync path = MkAsync $ \e => \f => readFile' path (\x => e x) (\s => f s)


interpret : FileSystemF a -> AsyncJS_IO a
interpret (WriteFile filePath contents next) = writeFileAsync filePath contents *> pure next
interpret (ReadFile filePath nextFn) = do
  contents <- readFileAsync filePath
  pure (nextFn contents)
