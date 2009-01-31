module Network.GpH.Protocol.Derive (derive) where

import Language.Haskell.TH
import Data.Binary
import Data.Binary.Get
import Data.Generics
import Control.Monad
import Foreign.Storable
import Data.Array


-- size :: (Data d, Num n) => d -> n
-- size = gmapQl (+) 0 dispatch where
--     dispatch :: (Data d, Num n) => d -> n
--     dispatch = error "Impossible"
--                `extQ` word16 `extQ` word32 `extQ` word64
--                `extQ` arbitrary `extQ` list arbitrary
--     word16 x = fromIntegral $ sizeOf (x :: Word16)
--     word32 x = fromIntegral $ sizeOf (x :: Word32)
--     word64 x = fromIntegral $ sizeOf (x :: Word64)
--     arbitrary x = fromIntegral $ B.length (x :: B.ByteString)
--     list f = sum . map f

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
          sizeD = do
            clauses <- mapM sizeClause constructors
            return $ decl "size" clauses
          decodeD = do
            body <-
                [| let reqs = listArray (0, 14)
                              $(liftM ListE $ mapM getBuilder constructors)
                   in runGet $ do get :: Get Word32;
                                  req <- get :: Get Word8;
                                  get :: Get Word16;
                                  reqs ! fromIntegral ((req - 100) `div` 2) |]
            return $ decl "decode" [Clause [] (NormalB body) []]
          encodeD = do
            x <- newName "x"
            m <- newName "m"
            clauses <- mapM putClause constructors
            return $ FunD (mkName "encode") [Clause [VarP x] (NormalB (AppE (var "runPut") (AppE (VarE m) (VarE x)))) [FunD m clauses]]
          typeName x = tyConString $ fst $ splitTyConApp $ typeOf x
          constructors = zip [(0 :: Int)..] $ map gen $ dataTypeConstrs $ dataTypeOf x
          gen con = ( mkName (showConstr con)
                    , gmapQ (\x -> do n <- newName "x"
                                      return (n, typeName x))
                                 $ fromConstr con `asTypeOf` x
                    )
          sizeClause (_, (con, argsm)) = do
            args <- sequence argsm
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            return $ Clause [pat] (NormalB (AppE (var "sum") (ListE (map gsize args)))) []
              where gsize (arg, ty) =
                        case ty of
                          "Data.ByteString.Lazy.Internal.ByteString" ->
                              coerce (AppE (var "B.length") (VarE arg)) word32
                          "[]" ->
                              coerce (AppE (var "length") (VarE arg)) word32
                          _ -> LitE (IntegerL (fromIntegral (sizeOf ty)))
                    sizeOf "Word8" = 1
                    sizeOf "Word16" = 2
                    sizeOf "Word32" = 4
                    sizeOf "Word64" = 8
                    sizeOf x = error $ "Unrecognized type: " ++ x
          putClause (n, (con, argsm)) = do
            args <- sequence argsm
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            return $ Clause [pat] (NormalB (DoE (putsizeOfS c : putreqCodeS n : map gputS args))) []
          putsizeOfS c = NoBindS $ putE $ AppE (var "size") (VarE c)
          putreqCodeS n = NoBindS $ putE (SigE (LitE (IntegerL (fromIntegral (requestFromIndex n)))) word8)
          gputS (arg, ty) =
              NoBindS $ case ty of
                          "Data.ByteString.Lazy.Internal.ByteString" ->
                              putByteStringS arg
                          "Prelude.[]" -> putByteStringListS arg
                          _ -> putE (VarE arg)
          putByteStringS x =
              DoE [ NoBindS $ putE $ coerce (AppE (var "B.length") (VarE x)) word16
                  , NoBindS $ AppE (var "putLazyByteString") (VarE x) ]
          putByteStringListS = undefined -- xxx
          getBuilder (n, (con, argsm)) = do
            args <- sequence argsm
            binds <- mapM ggetS args
            let ret = NoBindS $ AppE (var "return")
                      $ foldl AppE (ConE con) (map (VarE . fst) args)
            return $ DoE (binds ++ [ret])
          ggetS (arg, ty) =
              case ty of
                "Data.ByteString.Lazy.Internal.ByteString" ->
                    return (BindS (VarP arg)) `ap` getByteStringQ
                "Prelude.[]" ->
                    return (BindS (VarP arg)) `ap` getByteStringListQ
                _ -> return (BindS (VarP arg) getE)
          getByteStringQ = do
             size <- newName "size"
             return $ DoE [ BindS (SigP (VarP size) word16) getE
                          , NoBindS $
                            AppE (VarE (mkName "getLazyByteString"))
                                     (AppE (VarE (mkName "fromIntegral")) (VarE size)) ]
          getByteStringListQ = do
             size <- newName "size"
             getByteStringE <- getByteStringQ
             return $ DoE [ BindS (SigP (VarP size) word8) getE
                          , NoBindS $
                            AppE (AppE (VarE (mkName "replicateM"))
                                  (AppE (VarE (mkName "fromIntegral")) (VarE size))) getByteStringE ]
          var = VarE . mkName
          decl x clauses = FunD (mkName x) clauses
          putE = AppE (VarE (mkName "put"))
          getE = VarE (mkName "get")
          coerce exp ty = SigE (AppE (var "fromIntegral") exp) ty
          word8 = ConT (mkName "Word8")
          word16 = ConT (mkName "Word16")
          word32 = ConT (mkName "Word32")
