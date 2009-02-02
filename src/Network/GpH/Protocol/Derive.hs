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


derive :: Data a => a -> Q [Dec]
derive x = liftM (:[]) binaryInst
    where binaryInst = do
            methods <- sequence [getD, putD]
            return $ InstanceD []
                       (AppT (ConT (mkName "Binary"))
                                 (ConT (mkName (typeName x)))) (concat methods)

          -- define 'get'
          getD = do
            -- We have to jump through a hoop here to avoid Template Haskell
            -- being too smart on us. Template Haskell doesn't know 'get' is a
            -- method, so when we use 'get' at another type inside the
            -- definition of 'get', Template Haskell will throw a type error.
            --
            -- Solution: use Oxford brackets only for the body of 'get', then
            -- build the definition manually.
            body <- [| let reqs = listArray (0, 14)
                                  $(liftM ListE $ mapM getBuilder constructors)
                       in do get :: Get Word32
                             req <- get :: Get Word8
                             get :: Get Word16
                             reqs ! fromIntegral ((req - 100) `div` 2)
                     |]
            return [FunD (mkName "get") [Clause [] (NormalB body) []]]

          -- define 'put'
          putD =
               [d| put x =
                     -- Construct the encoding, then compute the length and
                     -- prepend the encoding with it, of course not forgetting
                     -- to count the size component.
                     let result = runPut (m x)
                         size = runPut
                                $ put (4 + fromIntegral (B.length result) :: Word32)
                     in putLazyByteString $ B.append size result where
                         m x = $( liftM (CaseE (VarE 'x))
                                  $ mapM putCase $ zip [0..] constructors )
               |]

          typeName x = tyConString $ fst $ splitTyConApp $ typeOf x
          constructors = map gen $ dataTypeConstrs $ dataTypeOf x
          gen con = ( mkName (showConstr con)
                    , gmapQ (\arg -> do x <- newName "x"
                                        return (x, typeName arg))
                                 $ fromConstr con `asTypeOf` x
                    )

          -- 'encode' functions
          putCase (n, (con, argsm)) = do
            args <- sequence argsm
            puts <- mapM (liftM NoBindS . gput) args
            c <- newName "c"
            let pat = AsP c (ConP con (map (VarP . fst) args))
            header <- mapM (liftM NoBindS)
                      [putreqCode n, putTag]
            return $ Match pat (NormalB (DoE (header ++ puts))) []
          putreqCode n =
              [| put ($(litE (integerL (100 + (n * 2)))) :: Word8) |]
          putTag = [| put (0 :: Word16) |]
          gput (arg, ty) =
              -- Binary provides an instance for lists but it doesn't do what
              -- we want, so call our own put method here, overriding
              -- Haskell's instance selection.
              case ty of
                "[]" -> [| do put (fromIntegral $ length $(varE arg) :: Word16)
                              mapM_ put $(varE arg) |]
                _    -> [| put $(varE arg) |]

          -- 'decode' functions
          getBuilder (con, argsm) = do
            args <- sequence argsm
            binds <- mapM ggetS args
            let ret = NoBindS $ AppE (VarE (mkName "return"))
                      $ foldl AppE (ConE con) (map (VarE . fst) args)
            return $ DoE (binds ++ [ret])
          ggetS (arg, ty) =
              case ty of
                "[]" -> return (BindS (VarP arg)) `ap` getList
                _    -> return (BindS (VarP arg) (VarE (mkName "get")))
          getList =
              [| do size <- get :: Get Word16
                    replicateM (fromIntegral size) get |]
