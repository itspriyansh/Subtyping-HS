{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Class.SubtypeLens where

import Control.Lens
import Types.Global
import Control.Monad.Trans.State (State)

class SubtypeLens a b | a -> b, b -> a where
  printSubtype :: a -> b -> IO String

  projectAux :: a -> b

  injectAux :: a -> b -> a

  subtypeLens :: Lens' a b
  subtypeLens f s = injectAux s <$> f (projectAux s)

  project :: a -> State Global ()

  inject :: a -> State Global a

  {-# MINIMAL printSubtype, projectAux, injectAux, project, inject #-}
