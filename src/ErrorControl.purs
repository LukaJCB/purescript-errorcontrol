module Error.Control where
  

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except.Trans (lift)
import Control.Monad.State (StateT(..), mapStateT, runStateT)
import Data.Either (Either(..), either)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

-- | A type class for principled Error handling.
-- | ErrorControl allows to represent already handled errors in the type system
-- | by switching to an unexceptional type constructor `g`.
-- | Find more here: https://lukajcb.github.io/blog/functional/2018/04/15/rethinking-monaderror.html
-- |
-- | Should respect the following laws:
-- | Catch: `controlError (throwError e) f === f e`
-- | Pure: `controlError (pure a) f === pure a`
-- | No Errors in g: `handleBlunder (accept ga) f === ga`
class (MonadThrow e f, Monad g) <= ErrorControl f g e | f -> e, f -> g where
  controlError :: forall a. f a -> (e -> g a) -> g a
  accept :: g ~> f

intercept :: forall f g e a. ErrorControl f g e => f a -> (e -> a) -> g a
intercept fa f = controlError fa (f >>> pure) 


trial :: forall f g e a. ErrorControl f g e => Monad f => f a -> g (Either e a)
trial fa =
  intercept (map Right fa) Left


absolve :: forall f g e a. ErrorControl f g e => Monad f => g (Either e a) -> f a
absolve gea = accept gea >>= (either throwError pure)


assure :: forall f g e a. ErrorControl f g e => Monad f => g a -> (a -> e) -> (a -> Boolean) -> f a
assure ga error predicate =
  accept ga >>= (\a -> 
    if predicate a then pure a else throwError (error a))


instance errorControlEither :: ErrorControl (Either e) Identity e where
  controlError fa f = case fa of
    Left e -> f e
    Right a -> Identity a

  accept = unwrap >>> Right

instance errorControlExceptT :: Monad f => ErrorControl (ExceptT e f) f e where
  controlError efa f =
    runExceptT efa >>= (\eea -> case eea of
      Left e -> f e
      Right a -> pure a
    )

  accept = lift

instance errorControlStateT :: (ErrorControl f g e) => ErrorControl (StateT s f) (StateT s g) e where
  controlError sfa f =
    StateT (\s -> runStateT sfa s `controlError` (f >>> (\state -> runStateT state s)))

  accept = mapStateT accept