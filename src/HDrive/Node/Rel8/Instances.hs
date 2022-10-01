module HDrive.Node.Rel8.Instances where

import HDrive.Node.Types.DirNode
import HDrive.Node.Types.FileNode
import HDrive.Node.Types.Store
import Rel8

instance DBType DirId
instance DBEq DirId
instance DBType FileType
instance DBType FileId
instance DBType S3FilePath
instance DBType StoreName
instance DBEq StoreName
