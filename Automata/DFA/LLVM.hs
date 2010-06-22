-- | Compile DFAs over Word8 to fast machine code.
module Automata.DFA.LLVM (compile, buildMatches) where

import Control.Monad

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word

import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Optimize    
    
import Foreign.C.String    
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

import Automata.DFA.Datatype

--------------------------------
-- TEST / EXAMPLE USAGE CODE --
-- Imports below this are just for the main test/example function
import Automata.DFA.Minimize (minimize)
import Automata.Conversions (mapNFAToMapDFA, regularExpressionToNFA)
import Data.ByteString.Internal (c2w)
import Automata.RegularExpression.Parser (eParse)
import Automata.RegularExpression.Functor


-- parse the regular expression, throwing an exception if there's an error in parsing.
-- convert it from a regex over Haskell Chars to a regex over bytes (Word8), then 
-- convert to an NFA, then a DFA, then minimize the DFA
dfa =  minimize $ mapNFAToMapDFA $ regularExpressionToNFA $ fmap c2w $ eParse "(aran)*"

main = do input <- getContents
          matches <- compile dfa
          forM_ (lines input) $ \line -> print $ matches line

-- END OF TEST / EXAMPLE USAGE CODE --
--------------------------------------

-- | Compile a DFA over bytes into a fast function.
compile :: Ord stateLabel => MapDFA stateLabel Word8 -> IO (String -> Bool)
compile dfa
    = do initializeNativeTarget
         m <- newNamedModule "Matcher"
         fns <- defineModule m (buildMatches dfa)
         optimizeModule 3 m
         
         matches <- runEngineAccess $ do
                         addModule m
                         generateFunction fns
         let pureMatches = unsafePurify $ withString matches
         let boolMatches line = toBool (pureMatches line)
         return boolMatches
    where withString fn str 
            = withCString str (\ptr_to_cchar -> fn $ castPtr ptr_to_cchar)

-- | Low-level LLVM codegen for a DFA over bytes.
buildMatches :: Ord stateLabel =>
                MapDFA stateLabel Word8 -> 
                CodeGenModule (Function (Ptr Word8 -> IO Word8))
buildMatches dfa = createNamedFunction ExternalLinkage "matches" (matchesBody dfa)

successBlock = do success <- createBasicBlock
                  ret (1 :: Word8)
                  return success
failureBlock = do failure <- createBasicBlock
                  ret (0::Word8)
                  return failure


-------------------------------------------------------------------------
-- OVERVIEW
-- LLVM IR is a static-single assignment (SSA) intermediate representation 
-- with excellent support for compiling and executing straight to memory.
-- LLVM code is divided into Modules, which are divided into Functions, which
-- are divided in Basic Blocks. 
-- For more, see the LLVM documentation or a reference on optimizing compilers.
-- 
-- Our compilation strategy for a boolean-result DFA is as follows:
-- Each DFA state maps to a basic block.
-- We keep track of a single integer representing the current offset into the 
-- null-terminated input string.
-- Each DFA state basic block increments the integer.
-- Each DFA state basic blcok has a Phi instruction for the value of the integer, 
-- with possibilities coming from each inbound DFA transition.
-- Each DFA state examines the value of the string at the offset and jumps to 
-- the appropriate next state with a "switch" instruction.
-- (The switch instruction can be implemented /very/ efficiently. (At least on x86).)
-- If there is no appropriate next state for the "switch", we jump to a failure state
-- that returns false.
-- Each accepting DFA state also has a transition that recognizes the terminating
-- null character and jumps to a success state that returns true.
--

data Acceptance = Accepting | NonAccepting deriving (Show, Eq)
-- Helper function for constructing DFA state basic blocks. 
-- Return SSA variables curoffset and nextoffset representing 
-- the input string offset before and after the block executes.
dfaBlock bb str is_accepting success failure outs 
    = do defineBasicBlock bb
         curoffset <- phi []
         ptr <- getElementPtr str (curoffset, ())
         val <- load ptr
         nextoffset <- add (1::Word32) curoffset
         switch val failure (outJumps is_accepting success outs)
         return (curoffset, nextoffset)
    where outJumps Accepting success outs
            = (constOf (0::Word8), success) : outs
          outJumps NonAccepting _ outs
            = outs

matchesBody :: Ord stateLabel => 
               MapDFA stateLabel Word8 -> 
               Value (Ptr Word8) -> 
               CodeGenFunction Word8 ()
matchesBody dfa str
    = do
       -- Declare the function entry block.
       entry <- newNamedBasicBlock "Entry"
       br entry

       -- Declare and define success and failure blocks.
       success <- successBlock
       failure <- failureBlock
       
       -- Declare one basic block per DFA state. 
       let stateLabels = Set.elems (states dfa)
       blocks <- replicateM (length stateLabels) newBasicBlock
       let statesToBlocks = Map.fromList $ zip stateLabels blocks

       -- The entry block initializes our string offset to 0 for the start of string.
       -- Then it jumps to the basic block representing the DFA start state.
       defineBasicBlock entry
       let nextoffset_entry = valueOf (0 :: Word32)
       br (statesToBlocks Map.! startKey dfa)
       
       -- Define each block, holding on to the string offset SSA variables.
       -- offsets :: [(curoffset, nextoffset), ...]
       offsets <- forM stateLabels $ \state ->
                     let block = statesToBlocks Map.! state
                         acceptance = (if Set.member state (acceptKeys dfa) then Accepting else NonAccepting)
                     in  dfaBlock block str acceptance success failure (outs dfa state statesToBlocks)
       
       let statesToOffsetPairs = Map.fromList $ zip stateLabels offsets
       -- Generate Phi instructions to keep track of the offset counter.
       forM_ stateLabels $ \state -> 
                let curoffset = fst $ statesToOffsetPairs Map.! state
--                                         :: [(nextoffset, state basic block), ...]
                in  addPhiInputs curoffset (inbound dfa state statesToBlocks statesToOffsetPairs (nextoffset_entry, entry))


  where 
      -- Helper function: Map DFA outbound transitions to block SSA variables suitable for the switch instruction. 
      outs dfa state statesToBlocks 
        = let isFromState = Map.filterWithKey (isFrom state) (transitionMap dfa)
          in  map (\((from, transition), to) -> (constOf transition, statesToBlocks Map.! to)) (Map.toList isFromState)
      -- Helper function: Map DFA inbound transitions to offset SSA variables suitable for the phi instruction.
      inbound dfa state statesToBlocks statesToOffsetPairs startOffsetPair  
        = let isToState = Map.filterWithKey (isTo state) (transitionMap dfa)
              inbounds = map (\((from, transition), to) -> (snd $ statesToOffsetPairs Map.! from, statesToBlocks Map.! from)) (Map.toList isToState)
              result True = startOffsetPair : inbounds
              result False = inbounds
          in  result (state == startKey dfa)