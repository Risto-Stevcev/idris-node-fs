module Node.Free.FileSystem

import Control.Monad.Free

%access public export

FilePath : Type
FilePath = String

Contents : Type
Contents = String

public export
data FileSystemF next
  = WriteFile FilePath Contents next
  | ReadFile FilePath (Contents -> next)

FileSystem : Type -> Type
FileSystem = Free FileSystemF

Functor FileSystemF where
  map f (WriteFile filePath contents next) = WriteFile filePath contents (f next)
  map f (ReadFile filePath nextFn) = ReadFile filePath (f . nextFn)

writeFile : FilePath -> Contents -> FileSystem Unit
writeFile filePath contents = liftFree $ WriteFile filePath contents ()

readFile : FilePath -> FileSystem String
readFile filePath = liftFree $ ReadFile filePath id
