{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Android.MyGame
  ( mkMyGameClass
  ) where

import Data.Binary.Put
import Data.Bits
import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy.Encoding
import Data.Word

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

import Development.Spaceport.Android.Manifest

import qualified Data.Android.PackageName as PackageName

data Constant
  = InvalidConstant
  | ClassName
  | ClassReference
  | CodeAttribute
  | ConstructorName
  | ConstructorNameAndType
  | ConstructorType
  | EmptyString
  | EmptyStringReference
  | GetAnalyticsKeyName
  | GetAnalyticsSecretName
  | GetEntryUrlName
  | OnPrepareOptionsMenuName
  | OnPrepareOptionsMenuType
  | StringGetterType
  | SuperclassConstructorReference
  | SuperclassName
  | SuperclassReference
  deriving (Bounded, Enum, Eq, Ord)

data Implementation = Implementation
  { implName :: Constant
  , implType :: Constant
  , implMaxStack :: Int
  , implInstance :: Bool
  , implParams :: Int
  , implLocals :: Int
  , implCode :: ByteString
  }

-- | The default empty method implementation. Minimal
-- complete definition: @implName@, @implType@, @implCode@,
-- and @implMaxStack@. Methods are otherwise assumed to be
-- instance methods taking no parameters and performing no
-- operations.
defaultImplementation :: Implementation
defaultImplementation = Implementation
  { implName = InvalidConstant
  , implType = InvalidConstant
  , implMaxStack = 0
  , implInstance = True
  , implParams = 0
  , implLocals = 0
  , implCode = mempty
  }

implMaxLocals :: Implementation -> Int
implMaxLocals Implementation {..} = sum
  [ if implInstance then 1 else 0
  , implParams
  , implLocals
  ]

constantIndex :: (Num a) => Constant -> a
constantIndex x = fromIntegral $ fromEnum x

-- | Generates a Java class file for a class called
-- @MyGame@, suitable for use as a Spaceport entry point on
-- Android. This generation is done manually to avoid a
-- dependence on @javac@ and, by extension, the Java
-- Development Kit. The Java class file format is written
-- per Oracle's documentation:
--
-- http://docs.oracle.com/javase/specs/jvms/se5.0/html/ClassFile.doc.html
mkMyGameClass :: AndroidManifestXml -> ByteString
mkMyGameClass AndroidManifestXml {..} = runPut $ do
  header

  -- Take note: the constant pool contains one fewer entry
  -- than the constant pool count. Constant pool indices are
  -- 1-based; 0 is reserved as an invalid or null constant
  -- pool reference, denoted by 'InvalidConstant'.
  u2 . fromIntegral $ length constants + 1
  sequence_ constants

  accessFlags
  c2 ClassReference
  c2 SuperclassReference

  -- Interfaces.
  u2 0

  -- Fields.
  u2 0

  -- Methods.
  u2 . fromIntegral $ length methods
  sequence_ methods

  -- Class attributes.
  u2 0

  where
  packagePath = Text.intercalate "/"
    $ PackageName.toPieces manifestPackageName
  constants = map constantValue [succ minBound .. maxBound]

  -- | The constant table is keyed by an enumeration,
  -- allowing constants to refer to one another. Each entry
  -- in the constant table consists of a one-byte tag
  -- followed by some data. There are no constraints on the
  -- ordering of constants, so they are simply sorted
  -- alphabetically.
  constantValue constant = case constant of
    InvalidConstant -> error
      $ "Development.Spaceport.Android.MyGame.mkMyGameClass.constantValue:\
      \ Invalid constant."
    ClassName -> utf8 $ packagePath <> "/MyGame"
    ClassReference -> classRef ClassName
    CodeAttribute -> utf8 "Code"
    ConstructorName -> utf8 "<init>"
    ConstructorNameAndType -> nameAndTypeRef ConstructorName ConstructorType
    ConstructorType -> utf8 "()V"
    EmptyString -> utf8 ""
    EmptyStringReference -> stringRef EmptyString
    GetAnalyticsKeyName -> utf8 "getAnalyticsKey"
    GetAnalyticsSecretName -> utf8 "getAnalyticsSecret"
    GetEntryUrlName -> utf8 "getEntryUrl"
    OnPrepareOptionsMenuName -> utf8 "onPrepareOptionsMenu"
    OnPrepareOptionsMenuType -> utf8 "(Landroid/view/Menu;)Z"
    StringGetterType -> utf8 "()Ljava/lang/String;"
    SuperclassConstructorReference
      -> methodRef SuperclassReference ConstructorNameAndType
    SuperclassName -> utf8 "sibblingz/spaceport/SpaceportApp"
    SuperclassReference -> classRef SuperclassName

  methods = map makeImplementation $
    [ defaultImplementation
      { implName = ConstructorName
      , implType = ConstructorType
      , implCode = runPut $ do  -- { super(this); return this; }
        jaload_0
        jinvokespecial SuperclassConstructorReference
        jreturn
      , implMaxStack = 1
      }
    , defaultImplementation
      { implName = GetEntryUrlName
      , implType = StringGetterType
      , implCode = returnEmptyString
      , implMaxStack = 1
      }
    , defaultImplementation
      { implName = GetAnalyticsKeyName
      , implType = StringGetterType
      , implCode = returnEmptyString
      , implMaxStack = 1
      }
    , defaultImplementation
      { implName = GetAnalyticsSecretName
      , implType = StringGetterType
      , implCode = returnEmptyString
      , implMaxStack = 1
      }
    ]
    ++ [onPrepareOptionsMenu | not manifestMenu]

  onPrepareOptionsMenu = defaultImplementation
    { implName = OnPrepareOptionsMenuName
    , implType = OnPrepareOptionsMenuType
    , implParams = 1
    , implCode = runPut $ do  -- { return true; }
      jiconst_1
      jireturn
    , implMaxStack = 1
    }

  -- { return ""; }
  returnEmptyString = runPut $ do
    jldc EmptyStringReference
    jareturn

  -- JVM bytecode instructions, prefixed with @j@ to avoid
  -- naming conflicts.
  jaload_0 = u1 0x2a
  jareturn = u1 0xb0
  jiconst_1 = u1 0x04
  jinvokespecial method = do
    u1 0xb7
    c2 method
  jireturn = u1 0xac
  jldc constant = do
    u1 0x12
    c1 constant
  jreturn = u1 0xb1

header :: Put
header = do
  u4 0xcafebabe  -- Magic number.
  u2 0x0000      -- Minor version.
  u2 0x0031      -- Major version.

accessFlags :: Put
accessFlags = u2
  $   0x0001  -- public
  .|. 0x0020  -- super

-- | Writes the implementation of a method. In addition to
-- name, signature, and body, an implementation needs two
-- bits of metadata:
--
-- 1. The number of local variable slots to allocate,
-- including one for @this@ and one for each parameter.
--
-- 2. The maximum stack depth produced by bytecode
-- instructions in the method. This is ordinarily tedious to
-- calculate by hand, but all of the methods we generate
-- happen to be trivial.

-- The code itself is written in the @Code@ attribute of the
-- method descriptor. Other attributes are possible, e.g.,
-- for local variable names or line number information, but
-- only @Code@ is required.
makeImplementation :: Implementation -> Put
makeImplementation impl@Implementation {..} = do
  u2 $ 0x0001  -- Access flags: public.
  c2 implName
  c2 implType
  u2 1  -- Attribute count.
  c2 CodeAttribute
  u4 $ 12 + fromIntegral (BS.length implCode)
  u2 . fromIntegral $ implMaxStack
  u2 . fromIntegral $ implMaxLocals impl
  u4 . fromIntegral $ BS.length implCode
  putLazyByteString implCode
  u2 0  -- Exception table length.
  u2 0  -- Attribute count.

stringRef :: Constant -> Put
stringRef string = do
  u1 0x08  -- String reference tag.
  c2 string

methodRef :: Constant -> Constant -> Put
methodRef class_ nameAndType = do
  u1 0x0a  -- Method reference tag.
  c2 class_
  c2 nameAndType

classRef :: Constant -> Put
classRef name = do
  u1 0x07  -- Class reference tag.
  c2 name

utf8 :: Text -> Put
utf8 text = do
  u1 0x01  -- UTF-8 constant tag.
  u2 . fromIntegral $ BS.length text'
  putLazyByteString text'
  where text' = encodeUtf8 $ LazyText.fromStrict text

nameAndTypeRef :: Constant -> Constant -> Put
nameAndTypeRef name signature = do
  u1 0x0c  -- Name and type tag.
  c2 name
  c2 signature

c1, c2 :: Constant -> Put
c1 = u1 . constantIndex
c2 = u2 . constantIndex

u1 :: Word8 -> Put
u1 = putWord8

u2 :: Word16 -> Put
u2 = putWord16be

u4 :: Word32 -> Put
u4 = putWord32be
