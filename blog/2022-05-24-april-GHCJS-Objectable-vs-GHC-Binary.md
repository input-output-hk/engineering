# Objectable vs GHC Binary

As part of the integration of GHCJS into GHC as a cross-compilation backend, we've converted the binary serialisation that GHCJS previously used, which was via its `Objectable` typeclass, into GHC's internal `Binary` typeclass representation.

This allows us to make use of certain data types internal to GHC, i.e. `Name` and `FastString`, within GHCJS's ASTs, as well as maintaining consistency with GHC's `.hi` interface file serialisation.

## How GHC Binary Works

In GHC's `.hi` interface files, `Name` and `FastString` are serialised in a different way to other data structures, in that they a written in the main data structure payload as indicies of a table, and these tables contain the actual string-like data of these types. So, an interface file might resemble:

* Header
  * Magic number for recognising interface files
  * Pointer to `Name` symbol table
  * Pointer to `FastString` dictionary
* Main data structure payload
* `Name` symbol table
* `FastString` dictionary

Importantly, the `FastString` dictionary must be written _after_ the `Name` symbol table, because `Name`s contain `FastString`s, so writing the symbol table will expand the dictionary. Additionally, because we only have one buffer, and we don't know the size of the payload until it's written, the tables cannot be written in the header, and instead serialisation code must reserve space for the table pointers and jump back to write the pointers once the table locations are known.

During serialisation, GHC uses mutable data structures to store both the serialised binary buffer, as well as these tables:

```haskell
data BinHandle
  = BinMem {                     -- binary data stored in an unboxed array
     bh_usr :: UserData,         -- sigh, need parameterized modules :-)
     _off_r :: !FastMutInt,      -- the current offset
     _sz_r  :: !FastMutInt,      -- size of the array (cached)
     _arr_r :: !(IORef BinArray) -- the array (bounds: (0,size-1))
    }

data UserData =
   UserData {
        -- for *deserialising* only:
        ud_get_name :: BinHandle -> IO Name,
        ud_get_fs   :: BinHandle -> IO FastString,

        -- for *serialising* only:
        ud_put_nonbinding_name :: BinHandle -> Name -> IO (),
        -- ^ serialize a non-binding 'Name' (e.g. a reference to another
        -- binding).
        ud_put_binding_name :: BinHandle -> Name -> IO (),
        -- ^ serialize a binding 'Name' (e.g. the name of an IfaceDecl)
        ud_put_fs   :: BinHandle -> FastString -> IO ()
   }
```

Here, we see that various functions are stored in the handle structure, to be later referenced by their respective types in their `GHC.Utils.Binary.Binary` typeclass instances:

```haskell
class Binary a where
    put_   :: BinHandle -> a -> IO ()
    put    :: BinHandle -> a -> IO (Bin a)
    get    :: BinHandle -> IO a

instance Binary FastString where
  put_ bh f =
    case getUserData bh of
        UserData { ud_put_fs = put_fs } -> put_fs bh f

  get bh =
    case getUserData bh of
        UserData { ud_get_fs = get_fs } -> get_fs bh

instance Binary Name where
   put_ bh name =
      case getUserData bh of
        UserData{ ud_put_nonbinding_name = put_name } -> put_name bh name

   get bh =
      case getUserData bh of
        UserData { ud_get_name = get_name } -> get_name bh
```

Later, in `GHC.Iface.Binary`, `getWithUserData` and `putWithUserData` structure the header in the way previously described, and initialise the `UserData` functions to write to/read from mutable tables:

```haskell
data BinDictionary = BinDictionary {
        bin_dict_next :: !FastMutInt, -- The next index to use
        bin_dict_map  :: !(IORef (UniqFM FastString (Int,FastString)))
                                -- indexed by FastString
  }

putFastString :: BinDictionary -> BinHandle -> FastString -> IO ()
putFastString dict bh fs = allocateFastString dict fs >>= put_ bh

allocateFastString :: BinDictionary -> FastString -> IO Word32
allocateFastString BinDictionary { bin_dict_next = j_r,
                                   bin_dict_map  = out_r} f = do
    out <- readIORef out_r
    let !uniq = getUnique f
    case lookupUFM_Directly out uniq of
        Just (j, _)  -> return (fromIntegral j :: Word32)
        Nothing -> do
           j <- readFastMutInt j_r
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM_Directly out uniq (j, f)
           return (fromIntegral j :: Word32)
```

## How Objectable Works

In comparison, GHCJS previously involved using instances of the `Objectable` typeclass to serialise its interface files:

```haskell
import qualified Data.Binary as DB

data SymbolTable
  = SymbolTable !Int !(Map ShortText Int)
  deriving (Show)

type PutSM = St.StateT SymbolTable DB.PutM -- FIXME: StateT isn't strict enough apparently
type PutS  = PutSM ()
type GetS  = ReaderT ObjEnv DB.Get

class Objectable a where
  put :: a -> PutS
  get :: GetS a
  putList :: [a] -> PutS
  putList = putListOf put
  getList :: GetS [a]
  getList = getListOf get
```

Here we see that GHCJS has opted for a different approach that avoids the mutable buffer by instead using `Data.Binary` instances that work via concatenating lazy `ByteString`s. Additionally, the mutable tables are replaced with a `State` monad that holds the symbol table as a `Map` structure.

Because `Data.Binary` forms lazy `ByteString`s, it's trivial to serialise the individual parts of the interface file and later concatenate these using `ByteString`'s monoid instance:

```haskell
object'
  :: ModuleName                 -- ^ module
  -> SymbolTable                -- ^ final symbol table
  -> Deps                       -- ^ dependencies
  -> [([ShortText],ByteString)] -- ^ serialized units and their exported symbols, the first unit is module-global
  -> ByteString
object' mod_name st0 deps0 os = hdr <> symbs <> deps1 <> idx <> mconcat (map snd os)
  where
    hdr          = putHeader (Header (moduleNameTag mod_name) (bl symbs) (bl deps1) (bl idx))
    bl           = fromIntegral . B.length
    deps1        = putDepsSection deps0
    (sti, idx)   = putIndex st0 os
    symbs        = putSymbolTable sti
```

## Comparison

### Advantages of the GHC Binary Approach

* Interface file generation appears to be very fast: speculatively looking at the generated core, this could be because the optimiser has a much better time with the style of IO code that is used - rather than a limitation of more abstacted approaches
* A significant amount of infrastructure already exists within GHC for interface files, so we can remove a lot of code used for `Objectable` and its maintenance burden along with it.

### Advantages of the Objectable Approach

* Avoids a lot of the weakly-typed binary buffer fiddling by using standard abstactions of `ByteString`s and `State` monads
* Concatenation of `ByteString`s allows a more declarative implementation of the serialised file's structure
* Concatenation of `ByteString`s allows a more flexible ordering of the individual parts of the file
* Don't need to carry around read/write functions (which are `undefined` during each-other's stage).

## Conclusion

While making use of GHC's existing infrastructure lets the GHCJS backend to make use of the `FastString` and `Name` data types, as well as allowing for the removal of a significant amount of now-redundant code, the comparison provided the GHCJS's old approach makes it clear that GHC's `Binary` implementation, while very useful, could be improved in both readability and extensiblity.
