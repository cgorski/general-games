-- The Loop-While Monad Transformer.
-- Copyright (c) 2008--2010, Neil Brown.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--  * Neither the name of the University of Kent nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | A module containing a monad transformer for performing while loops.  There
-- is nothing here that can't be built using if-then-else, but it can allow you
-- to express control more succinctly.
--
-- For example, here is a loop that executes until a certain time is reached:
--
-- > loop $ do lift performAction
-- >           t <- lift getTime
-- >           while (t < endTime)
--
-- This would commonly be called a do-while loop in other languages.  But the while
-- statement does not have to be at the end of the loop:
--
-- > loop $ do lift performAction
-- >           t <- lift getTime
-- >           while (t < endTime)
-- >           lift $ putStrLn ("Cur Time: " ++ show t)
--
-- This is sometimes known as do-while-do.  Note that like other monad
-- transformers, you'll either need to explicitly lift the actions from the
-- transformed monad, or use an mtl-style type-class to do so.
module Control.Monad.LoopWhile (LoopWhileT, loop, while) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, liftM, when)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Maybe (isJust)


-- | A monad transformer for easier looping.  See 'loop' and 'while'.
newtype LoopWhileT m a = LWT { getLoop :: m (Maybe a) }

instance Monad m => Monad (LoopWhileT m) where
  m >>= f = LWT $ getLoop m >>= maybe (return Nothing) (getLoop . f)
  return = LWT . return . Just

instance Monad m => Functor (LoopWhileT m) where
  fmap = liftM

instance Monad m => Applicative (LoopWhileT m) where
  pure = return
  (<*>) = ap

instance MonadTrans LoopWhileT where
  lift = LWT . liftM Just

instance MonadIO m => MonadIO (LoopWhileT m) where
  liftIO = lift . liftIO

-- | Runs the given action in a loop, executing it repeatedly until a 'while'
-- statement inside it has a False condition.  If you use 'loop' without 'while',
-- the effect is the same as 'forever'.
loop :: Monad m => LoopWhileT m a -> m ()
loop l = body
  where
    body = do x <- getLoop l
              when (isJust x) body

-- | Continues executing the loop if the given value is True.  If the value
-- is False, the loop is broken immediately, and control returns to the
-- caller of the 'loop' statement.  Thus you can build pre-condition,
-- post-condition, and \"mid-condition\" loops, placing the condition wherever
-- you like.
while :: Monad m => Bool -> LoopWhileT m ()
while b = LWT $ return $ if b then Just () else Nothing
