{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Data.GMachine.State
  ( GMachineState
  , buildInitialState
  , checkTerminalState

  , fetchNextInstruction
  , putInstruction
  , putInstructions
  , assertLastCode

  , garbageCollection

  , allocNodeOnNodeHeap
  , updateNodeOnNodeHeap
  , findNodeOnNodeHeap

  , allocAddressOnGlobal
  , updateAddressOnGlobal
  , findAddressOnGlobal

  , pushAddrToAddressStack
  , pushAddrsToAddressStack
  , popAddrFromAddressStack
  , popAddrsFromAddressStack
  , popAllAddrsFromAddressStack
  , peekAddrOnAddressStack
  , peekNthAddrOnAddressStack
  , checkSizeOfAddressStack

  , pushValueToValueStack
  , popValueFromValueStack

  , saveStateToDump
  , loadStateFromDump
  ) where

import Prelude hiding ( fail )

import Control.Lens.Each ( each )
import Control.Lens.Getter ( to )
import Control.Lens.Iso ( iso )
import Control.Lens.Operators
import Control.Lens.Operators.Minicute
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.Type
import Control.Lens.Unsound ( lensProduct )
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad ( forM )
import Control.Monad.Fail
import Control.Monad.State
  ( MonadState
  , StateT
  , evalStateT
  , execState
  , runState
  , runStateT
  )
import Data.Data
import Data.Text.Prettyprint.Doc ( Pretty(..) )
import Data.Tuple.Minicute
import GHC.Generics ( Generic )
import Minicute.Data.Common
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Data.Map as Map
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Minicute.Data.GMachine.AddressStack as AddressStack
import qualified Minicute.Data.GMachine.Code as Code
import qualified Minicute.Data.GMachine.Dump as Dump
import qualified Minicute.Data.GMachine.Global as Global
import qualified Minicute.Data.GMachine.NodeHeap as NodeHeap
import qualified Minicute.Data.GMachine.ValueStack as ValueStack

data GMachineState
  = GMachineState
    { code :: Code.Code
    , addressStack :: AddressStack.AddressStack
    , valueStack :: ValueStack.ValueStack
    , dump :: Dump.Dump
    , nodeHeap :: NodeHeap.NodeHeap
    , global :: Global.Global
    }
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

makeLensesFor
  [ ("code", "_code")
  , ("addressStack", "_addressStack")
  , ("valueStack", "_valueStack")
  , ("dump", "_dump")
  , ("nodeHeap", "_nodeHeap")
  , ("global", "_global")
  ]
  ''GMachineState

_di :: Lens' GMachineState Dump.DumpItem
_di = lensProduct _code (lensProduct _addressStack _valueStack) . iso tupleUnzip2 tupleZip2
{-# INLINE _di #-}


instance Pretty GMachineState where
  pretty s
    = PP.align
      . PP.braces
      . PP.enclose PP.hardline PP.hardline
      . PP.indent 2
      . PP.vsep
      $ [ prettyGMS s Normal (code s)
        , prettyGMS s Normal (addressStack s)
        , prettyGMS s Normal (valueStack s)
        , prettyGMS s Normal (dump s)
        , prettyGMS s Normal (nodeHeap s)
        , prettyGMS s Normal (global s)
        ]


buildInitialState :: Code.GMachineProgram -> GMachineState
buildInitialState program
  = GMachineState
    { code = Code.initialCode
    , addressStack = AddressStack.empty
    , valueStack = ValueStack.empty
    , dump = Dump.empty
    , nodeHeap = initialNodeHeap
    , global = initialGlobal
    }
  where
    (globalEntries, initialNodeHeap)
      = runState buildGlobalEntriesAndHeap NodeHeap.empty
    initialGlobal
      = execState buildGlobal Global.empty

    buildGlobalEntriesAndHeap
      = forM program
        $ \(ident, arity, c) ->
            (,) ident <$> NodeHeap.allocNode (NGlobal (toInteger arity) c)
    buildGlobal
      = forM globalEntries
        $ uncurry Global.allocAddress

    {-# INLINE initialGlobal #-}
    {-# INLINABLE buildGlobalEntriesAndHeap #-}
    {-# INLINABLE buildGlobal #-}

checkTerminalState :: GMachineState -> Bool
checkTerminalState state
  = state ^. _code == Code.empty
    && evalStateT (AddressStack.checkSize 1) (state ^. _addressStack) == Just True
    && state ^. _valueStack == ValueStack.empty
{-# INLINABLE checkTerminalState #-}


fetchNextInstruction :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Code.Instruction
fetchNextInstruction = applySubstructuralAction _code Code.fetchNextInstruction
{-# INLINABLE fetchNextInstruction #-}

putInstruction :: (MonadState s m, s ~ GMachineState) => Code.Instruction -> m ()
putInstruction = applySubstructuralAction _code . Code.putInstruction
{-# INLINABLE putInstruction #-}

putInstructions :: (MonadState s m, s ~ GMachineState) => [Code.Instruction] -> m ()
putInstructions = applySubstructuralAction _code . Code.putInstructions
{-# INLINABLE putInstructions #-}

assertLastCode :: (MonadState s m, s ~ GMachineState, MonadFail m) => m ()
assertLastCode = applySubstructuralAction _code Code.assertLastCode
{-# INLINABLE assertLastCode #-}


garbageCollection :: (MonadState s m, s ~ GMachineState, MonadFail m) => m ()
garbageCollection = do
  rootAddrs <- findGarbageCollectionRoots
  applySubstructuralAction _nodeHeap $ do
    NodeHeap.mark rootAddrs
    NodeHeap.sweep
  where
    findGarbageCollectionRoots :: (MonadState s m, s ~ GMachineState, MonadFail m) => m [Address]
    findGarbageCollectionRoots = do
      addrStkAddrs <- applySubstructuralAction _addressStack AddressStack.peekAllAddrs
      dumpAddrs <- applySubstructuralAction _dump Dump.extractAllAddresses
      globalAddrs <- applySubstructuralAction _global Global.findAllAddresses
      pure $ addrStkAddrs <> dumpAddrs <> globalAddrs
    {-# INLINABLE findGarbageCollectionRoots #-}
{-# INLINABLE garbageCollection #-}


allocNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState) => Node -> m Address
allocNodeOnNodeHeap = applySubstructuralAction _nodeHeap . NodeHeap.allocNode
{-# INLINABLE allocNodeOnNodeHeap #-}

updateNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Address -> Node -> m ()
updateNodeOnNodeHeap = (applySubstructuralAction _nodeHeap .) . NodeHeap.updateNode
{-# INLINABLE updateNodeOnNodeHeap #-}

findNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Address -> m Node
findNodeOnNodeHeap = applySubstructuralAction _nodeHeap . NodeHeap.findNode
{-# INLINABLE findNodeOnNodeHeap #-}


allocAddressOnGlobal :: (MonadState s m, s ~ GMachineState) => Identifier -> Address -> m ()
allocAddressOnGlobal = (applySubstructuralAction _global .) . Global.allocAddress
{-# INLINABLE allocAddressOnGlobal #-}

updateAddressOnGlobal :: (MonadState s m, s ~ GMachineState, MonadFail m) => Identifier -> Address -> m ()
updateAddressOnGlobal = (applySubstructuralAction _global .) . Global.updateAddress
{-# INLINABLE updateAddressOnGlobal #-}

findAddressOnGlobal :: (MonadState s m, s ~ GMachineState, MonadFail m) => Identifier -> m Address
findAddressOnGlobal = applySubstructuralAction _global . Global.findAddress
{-# INLINABLE findAddressOnGlobal #-}


pushAddrToAddressStack :: (MonadState s m, s ~ GMachineState) => Address -> m ()
pushAddrToAddressStack = applySubstructuralAction _addressStack . AddressStack.pushAddr
{-# INLINABLE pushAddrToAddressStack #-}

pushAddrsToAddressStack :: (MonadState s m, s ~ GMachineState) => [Address] -> m ()
pushAddrsToAddressStack = applySubstructuralAction _addressStack . AddressStack.pushAddrs
{-# INLINABLE pushAddrsToAddressStack #-}

popAddrFromAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Address
popAddrFromAddressStack = applySubstructuralAction _addressStack AddressStack.popAddr
{-# INLINABLE popAddrFromAddressStack #-}

popAddrsFromAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => Int -> m [Address]
popAddrsFromAddressStack = applySubstructuralAction _addressStack . AddressStack.popAddrs
{-# INLINABLE popAddrsFromAddressStack #-}

popAllAddrsFromAddressStack :: (MonadState s m, s ~ GMachineState) => m [Address]
popAllAddrsFromAddressStack = applySubstructuralAction _addressStack AddressStack.popAllAddrs
{-# INLINABLE popAllAddrsFromAddressStack #-}

peekAddrOnAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Address
peekAddrOnAddressStack = applySubstructuralAction _addressStack AddressStack.peekAddr
{-# INLINABLE peekAddrOnAddressStack #-}

peekNthAddrOnAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => Int -> m Address
peekNthAddrOnAddressStack = applySubstructuralAction _addressStack . AddressStack.peekNthAddr
{-# INLINABLE peekNthAddrOnAddressStack #-}

checkSizeOfAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => Int -> m Bool
checkSizeOfAddressStack = applySubstructuralAction _addressStack . AddressStack.checkSize
{-# INLINABLE checkSizeOfAddressStack #-}


pushValueToValueStack :: (MonadState s m, s ~ GMachineState) => Integer -> m ()
pushValueToValueStack = applySubstructuralAction _valueStack . ValueStack.pushValue
{-# INLINABLE pushValueToValueStack #-}

popValueFromValueStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Integer
popValueFromValueStack = applySubstructuralAction _valueStack ValueStack.popValue
{-# INLINABLE popValueFromValueStack #-}


saveStateToDump :: (MonadState s m, s ~ GMachineState) => m ()
saveStateToDump = _di <<.= Dump.emptyDumpItem >>= applySubstructuralAction _dump . Dump.saveState
{-# INLINABLE saveStateToDump #-}

loadStateFromDump :: (MonadState s m, s ~ GMachineState, MonadFail m) => m ()
loadStateFromDump = _di <~ applySubstructuralAction _dump Dump.loadState
{-# INLINABLE loadStateFromDump #-}


applySubstructuralAction :: (MonadState s m, s ~ GMachineState) => Lens' s a -> StateT a m r -> m r
applySubstructuralAction _l action = _l %%~= runStateT action
{-# INLINE applySubstructuralAction #-}


data PrettyGMSVerbosity
  = Simple
  | Normal
  deriving ( Eq
           )

-- |
-- 'PrettyGMS' (which stands for Pretty GMachine State) is a type class
-- for pretty printing
class PrettyGMS a where
  prettyGMS :: GMachineState -> PrettyGMSVerbosity -> a -> PP.Doc ann

instance PrettyGMS AddressStack.AddressStack where
  prettyGMS st v addrStk
    = "address" PP.<+> "stack"
      PP.<+>
      prettyBracedItems prettyAddresses addrs
    where
      addrs = addrStk ^. _Wrapped

      prettyAddresses = PP.vsep . fmap prettyAddress
      prettyAddress addr
        = pretty addr PP.<> PP.colon
          PP.<+>
          case v of
            Normal -> prettyNodeOfAddr st addr
            Simple -> "..."

      {-# INLINE addrs #-}
      {-# INLINE prettyAddresses #-}
      {-# INLINABLE prettyAddress #-}

instance PrettyGMS Code.Code where
  prettyGMS _ v c
    = "code"
      PP.<+>
      case v of
        Normal -> PP.unsafeViaShow (c ^. _Wrapped)
        Simple -> PP.brackets "..."

instance PrettyGMS Dump.Dump where
  prettyGMS st _ d
    = "dump"
      PP.<+>
      prettyBracedItems prettyIndexedDumpItems indexedDis
    where
      indexedDis = reverse . zip [1..] . reverse $ dis :: [(Integer, Dump.DumpItem)]
      dis = d ^. _Wrapped

      prettyIndexedDumpItems = PP.vsep . fmap prettyIndexedDumpItem
      prettyIndexedDumpItem (ind, di)
        = "dump" PP.<+> "item"
          PP.<+> PP.angles (pretty ind) <> PP.colon
          PP.<+> prettyDumpItem di
      prettyDumpItem (c, as, vs)
        = PP.braces . PP.enclose PP.hardline PP.hardline . PP.indent 2 . PP.vsep
          $ [ prettyGMS st Simple c
            , prettyGMS st Simple as
            , prettyGMS st Simple vs
            ]

      {-# INLINE indexedDis #-}
      {-# INLINE dis #-}
      {-# INLINE prettyIndexedDumpItems #-}
      {-# INLINABLE prettyIndexedDumpItem #-}
      {-# INLINABLE prettyDumpItem #-}

instance PrettyGMS Global.Global where
  prettyGMS st v gl
    = "global"
      PP.<+>
      prettyBracedItems prettyGlobalItems globalItems
    where
      glMap = gl ^. _Wrapped
      globalMaxIdLen
        = globalItems
          ^.. each . _1 . _Wrapped . to length
          & maximum
      globalItems = Map.toAscList glMap

      prettyGlobalItems = PP.vsep . fmap prettyGlobalItem
      prettyGlobalItem (ident, addr)
        = PP.fill globalMaxIdLen (pretty ident)
          PP.<+> "->" PP.<+> pretty addr
          PP.<>
          case v of
            Normal -> PP.colon PP.<+> prettyNodeOfAddr st addr
            Simple -> PP.emptyDoc

      {-# INLINE glMap #-}
      {-# INLINE globalMaxIdLen #-}
      {-# INLINE prettyGlobalItems #-}
      {-# INLINABLE prettyGlobalItem #-}

instance PrettyGMS NodeHeap.NodeHeap where
  prettyGMS st v nh
    = "node" PP.<+> "heap" PP.<+> PP.angles (pretty lastAddr)
      PP.<+>
      prettyBracedItems prettyNodeHeapItems nhItems
    where
      nhItems = Map.toAscList nhMap
      (lastAddr, nhMap) = nh ^. _Wrapped

      prettyNodeHeapItems = PP.vsep . fmap prettyNodeHeapItem
      prettyNodeHeapItem (addr, (_, node))
        = pretty addr PP.<> PP.colon PP.<+> prettyGMS st v node

      {-# INLINE nhItems #-}
      {-# INLINE lastAddr #-}
      {-# INLINE nhMap #-}
      {-# INLINE prettyNodeHeapItems #-}
      {-# INLINE prettyNodeHeapItem #-}

instance PrettyGMS ValueStack.ValueStack where
  prettyGMS _ _ valStk
    = "value" PP.<+> "stack" PP.<+> PP.unsafeViaShow (valStk ^. _Wrapped)

instance PrettyGMS Node where
  prettyGMS _ _ NEmpty = "empty"
  prettyGMS _ _ (NInteger n) = PP.pretty n
  prettyGMS _ _ (NStructure tag addr)
    = "$C" PP.<> PP.braces (PP.pretty tag PP.<> PP.semi PP.<> pretty addr)
  prettyGMS _ _ (NStructureFields _ addrs)
    = "$F" PP.<+> PP.list (fmap pretty addrs)
  prettyGMS _ _ (NApplication fAddr argAddr)
    = pretty fAddr PP.<+> "$" PP.<+> pretty argAddr
  prettyGMS st v (NIndirect addr)
    = "~>" PP.<+> pretty addr PP.<> PP.colon
      PP.<+>
      case v of
        Normal -> prettyNodeOfAddr st addr
        Simple -> "..."
  prettyGMS _ v (NGlobal arity insts)
    = "global" PP.<> PP.angles (PP.pretty arity)
      PP.<+>
      case v of
        Normal -> PP.unsafeViaShow insts
        Simple -> PP.brackets "..."

prettyBracedItems :: ([a] -> PP.Doc ann) -> [a] -> PP.Doc ann
prettyBracedItems _ []
  = PP.braces PP.hardline
prettyBracedItems f xs
  = PP.braces
    . PP.enclose PP.hardline PP.hardline
    . PP.indent 2
    $ f xs
{-# INLINABLE prettyBracedItems #-}

prettyNodeOfAddr :: GMachineState -> Address -> PP.Doc ann
prettyNodeOfAddr st addr
  = case st ^. _nodeHeap & evalStateT (NodeHeap.findNode addr) of
      Just node ->
        prettyGMS st Simple node
      Nothing ->
        "--invalid address--"
{-# INLINABLE prettyNodeOfAddr #-}
