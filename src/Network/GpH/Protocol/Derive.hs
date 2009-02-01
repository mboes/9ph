module Network.GpH.Protocol.Derive (derive) where

import Language.Haskell.TH
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Generics
import Control.Monad
import Foreign.Storable
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as B


requestFromIndex :: Int -> Word8
requestFromIndex x = fromIntegral (100 + (x * 2))

indexFromRequest :: Word8 -> Int
indexFromRequest x = fromIntegral ((x - 100) `div` 2)

derive :: Data a => a -> Q [Dec]
derive x = liftM (:[]) protocolInst
    where protocolInst = do
            methods <- sequence [sizeD, decodeD, encodeD]
            return $ InstanceD []
                       (AppT (ConT (mkName "Protocol"))
                                 (ConT (mkName (typeName x)))) methods

          -- define 'size'
          sizeD = do
            clauses <- mapM sizeClause constructors
            return $ decl "size" clauses

          -- define 'decode'
          decodeD =
              liftM head
              [d| decode =
                    let reqs = listArray (0, 14)
                               $(liftM ListE $ mapM getBuilder constructors)
                    in runGet $ do get :: Get Word32;
                                   req <- get :: Get Word8;
                                   get :: Get Word16;
                                   reqs ! fromIntegral ((req - 100) `div` 2)
               |]

          -- define 'encode'
          encodeD =
              liftM head
               [d| encode x = runPut (m x) where
                       m x = $(liftM (CaseE (VarE 'x)) $ mapM putCase $ zip [0..] constructors)
               |]

          typeName x = tyConString $ fst $ splitTyConApp $ typeOf x
          constructors = map gen $ dataTypeConstrs $ dataTypeOf x
          gen con = ( mkName (showConstr con)
                    , gmapQ (\arg -> do x <- newName "x"
                                        return (x, typeName arg))
                                 $ fromConstr con `asTypeOf` x
                    )

          -- 'size' functions
          sizeClause (con, argsm) = do
            args <- sequence argsm
            sizes <- liftM (LitE (IntegerL 7) :) $ mapM gsize args
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            return $ Clause [pat] (NormalB (AppE (var "sum") (ListE sizes))) []
              where gsize (arg, ty) =
                        case ty of
                          "Data.ByteString.Lazy.Internal.ByteString" ->
                              [| 2 + fromIntegral (B.length $(varE arg)) :: Word32 |]
                          "[]" ->
                              [| 2 + fromIntegral
                                 (sum (map (\x -> 2 + B.length x) $(varE arg))) :: Word32 |]
                          _ -> [| $(litE (integerL (sizeOf ty))) |]
                    sizeOf "Word8" = 1
                    sizeOf "Word16" = 2
                    sizeOf "Word32" = 4
                    sizeOf "Word64" = 8
                    sizeOf x = error $ "Unrecognized type: " ++ x

          -- 'encode' functions
          putClause (n, (con, argsm)) = do
            args <- sequence argsm
            puts <- mapM gputS args
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            header <- sequence [putsizeOfS c, putreqCodeS n, putTagS]
            return $ Clause [pat] (NormalB (DoE (header ++ puts))) []
          putsizeOfS c = return $ NoBindS $ putE $ AppE (var "size") (VarE c)
          putreqCodeS n =
              liftM NoBindS
              [| put ($(litE (integerL (fromIntegral (requestFromIndex n)))) :: Word8) |]
          putTagS = liftM NoBindS [| put (0 :: Word16) |]
          gputS (arg, ty) =
              case ty of
                "Data.ByteString.Lazy.Internal.ByteString" ->
                    return NoBindS `ap` putByteStringS (varE arg)
                "[]" ->
                    return NoBindS `ap` putByteStringListS (varE arg)
                _ -> return $ NoBindS (putE (VarE arg))
          putByteStringS x =
              [| do put (fromIntegral (B.length $x) :: Word16)
                    putLazyByteString $x |]
          putByteStringListS x =
              [| do let len = fromIntegral $ length $x :: Word16
                        f xs = do
                          put (fromIntegral (B.length xs) :: Word16)
                          putLazyByteString xs
                    put len
                    mapM_ f $x |]

          -- 'decode' functions
          getBuilder (con, argsm) = do
            args <- sequence argsm
            binds <- mapM ggetS args
            let ret = NoBindS $ AppE (var "return")
                      $ foldl AppE (ConE con) (map (VarE . fst) args)
            return $ DoE (binds ++ [ret])
          ggetS (arg, ty) =
              case ty of
                "Data.ByteString.Lazy.Internal.ByteString" ->
                    return (BindS (VarP arg)) `ap` getByteStringQ
                "[]" ->
                    return (BindS (VarP arg)) `ap` getByteStringListQ
                _ -> return (BindS (VarP arg) getE)
          getByteStringQ =
              [| do size <- get :: Get Word16
                    getLazyByteString (fromIntegral size) |]
          getByteStringListQ =
              [| do size <- get :: Get Word16
                    replicateM (fromIntegral size) $getByteStringQ |]

          -- Utilities
          var = VarE . mkName
          decl x clauses = FunD (mkName x) clauses
          putE = AppE (VarE (mkName "put"))
          getE = VarE (mkName "get")
