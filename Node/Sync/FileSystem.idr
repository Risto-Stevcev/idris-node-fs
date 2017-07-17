module Node.Sync.FileSystem

import Node.Free.FileSystem

%lib Node "fs"
%access public export

writeFileSync : String -> String -> JS_IO ()
writeFileSync path contents =
  foreign FFI_JS "fs.writeFileSync(%0, %1)" (String -> String -> JS_IO ()) path contents

readFileSync : String -> JS_IO String
readFileSync path = foreign FFI_JS "fs.readFileSync(%0, 'utf8')" (String -> JS_IO String) path


interpret : FileSystemF a -> JS_IO a
interpret (WriteFile filePath contents next) = writeFileSync filePath contents *> pure next
interpret (ReadFile filePath nextFn) = do
  contents <- readFileSync filePath
  pure (nextFn contents)
