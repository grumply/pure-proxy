{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DeriveGeneric, ViewPatterns, PatternSynonyms, FlexibleContexts #-}
module Pure.Proxy where

import Pure hiding (child)
import Pure.Data.Prop

import Control.Arrow ((&&&))
import Data.Foldable (traverse_)
import GHC.Generics

data Proxy = Proxy_
    { child :: View
    , innerRef :: Node -> IO ()
    , onMount :: IO ()
    , onMounted :: IO ()
    , onUnmounted :: IO ()
    } deriving (Generic)

instance Default Proxy

pattern Proxy :: Proxy -> Proxy
pattern Proxy a = a

instance Pure Proxy where
    view = LibraryComponentIO $ \self ->
        let
            withRef (getHost -> h) = do
                f <- innerRef <$> ask self
                traverse_ f h
        in
            def
                { construct = return ()
                , mount     = \_ ->   ask  self >>= onMount
                , mounted   =         ask  self >>= onMounted >>
                                      look self >>= withRef
                , unmounted =         ask  self >>= onUnmounted
                , updated   = \_ _ -> look self >>= withRef
                , render    = \r _ -> child r
                }

data InnerRef = InnerRef_
pattern InnerRef :: HasProp InnerRef a => Prop InnerRef a -> a -> a
pattern InnerRef p a <- (getProp InnerRef_ &&& id -> (p,a)) where
    InnerRef p a = setProp InnerRef_ p a

data OnMount = OnMount_
pattern OnMount :: HasProp OnMount a => Prop OnMount a -> a -> a
pattern OnMount p a <- (getProp OnMount_ &&& id -> (p,a)) where
    OnMount p a = setProp OnMount_ p a

data OnMounted = OnMounted_
pattern OnMounted :: HasProp OnMounted a => Prop OnMounted a -> a -> a
pattern OnMounted p a <- (getProp OnMounted_ &&& id -> (p,a)) where
    OnMounted p a = setProp OnMounted_ p a

data OnUnmounted = OnUnmounted_
pattern OnUnmounted :: HasProp OnUnmounted a => Prop OnUnmounted a -> a -> a
pattern OnUnmounted p a <- (getProp OnUnmounted_ &&& id -> (p,a)) where
    OnUnmounted p a = setProp OnUnmounted_ p a

only :: [View] -> View
only [] = Null
only (a:_) = a

instance HasChildren Proxy where
    getChildren v = [ child v ]
    setChildren cs v = v { child = only cs }

instance HasProp InnerRef Proxy where
    type Prop InnerRef Proxy = Node -> IO ()
    getProp _ = innerRef
    setProp _ ir p = p { innerRef = ir }

instance HasProp OnMount Proxy where
    type Prop OnMount Proxy = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnMounted Proxy where
    type Prop OnMounted Proxy = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMounted = om }

instance HasProp OnUnmounted Proxy where
    type Prop OnUnmounted Proxy = IO ()
    getProp _ = onUnmounted
    setProp _ om p = p { onUnmounted = om }
