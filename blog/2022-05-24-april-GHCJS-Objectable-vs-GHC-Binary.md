# Objectable vs GHC Binary

As part of the integration of GHCJS into GHC as a cross-compilation backend, we've converted the binary serialisation that GHCJS previously used, which was via its `Objectable` typeclass, into GHC's internal `Binary` typeclass representation. In doing this, we gain access to instances for serialising many of GHC's internal data types, and, importantly, we can reuse GHC's mechanism for serialising its `Name` and `FastString` types, which are written to lookup tables in order to maintain identity, as well as allowing for space savings on disk.

In this post, we will explain how the GHC `Binary` and GHCJS `Objectable` approaches work, and compare their tradeoffs.

## How GHC Binary Works

Internally, GHC uses the `Name` data type to track the uniqueness of objects during compilation. Amongst information relating to the definition of a `Name` within the Haskell source, a `Name` also contains a `Unique` integer (the value of which is provided by the complation environment monad). Using this `Unique` integer, which is unpacked in `Name`'s definition, we can make O(1) equality comparisons without following further memory references - allowing for this operation to be very quick, which will have a large effect on compilation performance given how often it is used.

`FastString` is used within GHC to store short, string-like data, and, similarly to `Name`, `FastString` uses a unique integer to allow for very fast equality comparisons. Primarily, `FastString` is used to represent variables and other definitions, and is used both in `Name` as the string-representation of a name with extra information attached, as well as directly, representing names that don't require this extra information, such as local variables.

In GHC's `.hi` interface files, `Name` and `FastString` are serialised differently compared to other data structures. They are written in the main data structure payload as indicies of a table, and these tables contain the actual string-like data of these types. So, an interface file might resemble:

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

Here, we see that various functions are stored in the handle structure, to be later referenced by their respective types in their `GHC.Utils.Binary.Binary` typeclass instances. Notice that the instance of `Binary Name` references `ud_put_nonbinding_name` and `ud_get_name`. Similarly, the `Binary FastString` instance uses `ud_put_fs` and `ud_get_fs`.

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

In `GHC.Iface.Binary`, helper types and functions are defined to store the `Name` symbol table and `FastString` dictionary in a mutable data structure. Here, `putFastString` is intended to be partially applied - passing it an appropriately initialised `BinDictionary` so that the resulting function can be stored in the `us_put_fs` field of the `UserData`. `allocateFastString` does the low-level work here, incrementing the index and modifying the mutable map (stored as a `UniqFM`, which is map keyed on types that contain `Unique`s - recalling that these are used for fast equality comparisons):

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

Later, in `GHC.Iface.Binary`, `getWithUserData` and `putWithUserData` will structure the header, and initialise the `UserData` functions to write to/read from mutable tables. Notice that we must first reserve header space for pointers to the lookup tables, as well as initialise the mutable tables, write these initialised structures to the `UserData` (for example, we see the previous `putFastString` partially applied here), then write the main payload, then write the lookup tables (`Name` symbol table first, because writing this can add to the `FastString` dictionary), and finally jump back to fill in the pointers to these tables:

```haskell
putWithUserData :: Binary a => TraceBinIFace -> BinHandle -> a -> IO ()
putWithUserData traceBinIface bh payload = do
    -- Remember where the dictionary pointer will go
    dict_p_p <- tellBin bh
    -- Placeholder for ptr to dictionary
    put_ bh dict_p_p

    -- Remember where the symbol table pointer will go
    symtab_p_p <- tellBin bh
    put_ bh symtab_p_p
    -- Make some initial state
    symtab_next <- newFastMutInt 0
    symtab_map <- newIORef emptyUFM
    let bin_symtab = BinSymbolTable {
                         bin_symtab_next = symtab_next,
                         bin_symtab_map  = symtab_map }
    dict_next_ref <- newFastMutInt 0
    dict_map_ref <- newIORef emptyUFM
    let bin_dict = BinDictionary {
                       bin_dict_next = dict_next_ref,
                       bin_dict_map  = dict_map_ref }

    -- Put the main thing,
    bh <- return $ setUserData bh $ newWriteState (putName bin_dict bin_symtab)
                                                  (putName bin_dict bin_symtab)
                                                  (putFastString bin_dict)
    put_ bh payload

    -- Write the symtab pointer at the front of the file
    symtab_p <- tellBin bh        -- This is where the symtab will start
    putAt bh symtab_p_p symtab_p  -- Fill in the placeholder
    seekBin bh symtab_p           -- Seek back to the end of the file

    -- Write the symbol table itself
    symtab_next <- readFastMutInt symtab_next
    symtab_map  <- readIORef symtab_map
    putSymbolTable bh symtab_next symtab_map
    case traceBinIface of
      QuietBinIFace         -> return ()
      TraceBinIFace printer ->
         printer (text "writeBinIface:" <+> int symtab_next
                                        <+> text "Names")

    -- NB. write the dictionary after the symbol table, because
    -- writing the symbol table may create more dictionary entries.

    -- Write the dictionary pointer at the front of the file
    dict_p <- tellBin bh          -- This is where the dictionary will start
    putAt bh dict_p_p dict_p      -- Fill in the placeholder
    seekBin bh dict_p             -- Seek back to the end of the file

    -- Write the dictionary itself
    dict_next <- readFastMutInt dict_next_ref
    dict_map  <- readIORef dict_map_ref
    putDictionary bh dict_next dict_map
    case traceBinIface of
      QuietBinIFace         -> return ()
      TraceBinIFace printer ->
         printer (text "writeBinIface:" <+> int dict_next
                                        <+> text "dict entries")
```

In summary, we see a number of structural characteristics of code using GHC's Binary implementation:
* Use of a single buffer means that the lookup tables can't be written in the header, so we have to reserve space for table pointers in the header, and jump back once we know where they will be located in order to write the pointers to the buffer. Essentially, an ordering of file sections is enforced by the data dependencies of the payload containing `Name`s and `FastString`s, and `Name`s containing `FastString`s - which means these must be written in this order, but reading must be done in the reverse order, causing the need for pointers in the header.
* Jumping around in binary buffers results in weakly enforced types and fiddly, code that Haskell's type system isn't able to help us debug
* Must carry about read/write functions for the lookup table types (`Name` and `FastString`), which are `undefined` during the opposite serialisation stage, and are hard-coded into the handle, reducing extensibility.

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

Because `Data.Binary` forms lazy `ByteString`s, it's trivial to serialise the individual parts of the interface file and later concatenate these using `ByteString`'s monoid instance - allowing for all of the sections of the file to be defined declaratively at the top-level of the function in order of their appearance within the file.

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

In summary, the use of multiple `ByteString` sections that are later concatenated offer several different structural characteristics compared to the use of a single mutable buffer:
* The final ordering of the sections is flexible, because they are serialsied separately, so any data dependencies don't introduce ordering in the file - which we see in the `where` clause of `object'`
* Types are more strongly enforced because imperative `seekBin` instructions aren't required. However, each section is still _deserialised_ by taking a substring of the file to be read as that section type. Of course, all serialisation eventually results in raw binary, so the simplification of concatenating the sections into the final file without jumping around limits the places that bugs can hide
* Visually, the ordering of the sections within the final file is very clear - we see in `object'` that every section is simply listed _in order_ on one line, concatenated together.

## Conclusion

Making use of GHC's existing infrastructure lets the GHCJS backend to make use of the `FastString` and `Name` data types, as well as allowing for the removal of a significant amount of now-redundant code.

Additionally, interface file generation using GHC's `Binary` appears to be very fast - for example, attempts to hide the handle behind a reader monad significantly reduce the compiler's performance as measured by CI. Speculatively, looking at the generated core, this could be because the optimiser has a much better time with the style of IO code that is used - rather than being a limitation of more abstacted approaches.

The comparison provided the GHCJS's old approach makes it clear that GHC's `Binary` implementation, while very useful, has potential to be improved in both readability and extensiblity. However, because CI has shown that serialisation performance has a significant effect on overall compilation performance, this tradeoff must be considered when making any changes. Potentially, these readability shortfalls in GHC's implementation might just be the result of legacy code, and so benchmarks of other approaches, such as `Data.Binary`, should be used to guide future work in improving the readability and flexibility of GHC's serialisation without sacrificing performance.
