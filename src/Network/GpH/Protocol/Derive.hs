module Network.GpH.Protocol.Derive (derive) where

import Language.Haskell.TH
import Data.Binary
import Foreign.Storable
import Data.Generics
import Control.Monad


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
derive x = sequence [storableInst, decodeD, encodeD]
    where storableInst = do
            methods <- sequence [sizeOfD, alignmentD]
            return $ InstanceD []
                       (AppT (ConT (mkName "Storable"))
                                 (ConT (mkName (typeName x)))) methods
          alignmentD = liftM head [d| alignment = undefined |]
          sizeOfD = do
            clauses <- mapM sizeClause constructors
            return $ decl "sizeOf" clauses
          decodeD = liftM head [d| decode = undefined |]
          encodeD = do
            clauses <- mapM putClause constructors
            return $ decl "encode" clauses
          typeName x = tyConString $ fst $ splitTyConApp $ typeOf x
          constructors = zip [(0 :: Int)..] $ map gen $ dataTypeConstrs $ dataTypeOf x
          gen con = ( mkName (showConstr con)
                    , gmapQ (\x -> do n <- newName "x"
                                      return (n, typeName x))
                                 $ fromConstr con `asTypeOf` x
                    )
          sizeClause (n, (con, argsm)) = do
            args <- sequence argsm
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            return $ Clause [pat] (NormalB (AppE (var "sum") (ListE (map sizeOf args)))) []
              where sizeOf (arg, _) = AppE (var "sizeOf") (VarE arg)
          putClause (n, (con, argsm)) = do
            args <- sequence argsm
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            return $ Clause [pat] (NormalB (DoE (putsizeOfS c : putreqCodeS n : map gputS args))) []
          putsizeOfS c = NoBindS $ putE $ AppE (var "sizeOf") (VarE c)
          putreqCodeS n = NoBindS $ putE (SigE (LitE (IntegerL (fromIntegral (requestFromIndex n)))) (ConT (mkName "Word8")))
          gputS (arg, ty) =
              NoBindS $ case ty of
                          "Data.ByteString.Internal.ByteString" -> putByteStringS arg
                          "Prelude.[]" -> putByteStringListS arg
                          _ -> putE (VarE arg)
          putByteStringS x =
              DoE [ NoBindS $ putE $ AppE (var "sizeOf") (VarE x)
                  , NoBindS $ putE $ (VarE x) ]
          putByteStringListS = undefined
          var = VarE . mkName
          decl x clauses = FunD (mkName x) clauses
          putE = AppE (VarE (mkName "put"))
