import Control.Monad
import System.IO
import System.Console.Haskeline

import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State

import Ctx
import Interpret
import Syntax


printLine :: (Member (Final (InputT IO)) r) => String -> Sem r ()
printLine a = embedFinal $ outputStrLn @IO $ a

runCommand :: Members '[State Ctx, Error String, Fail, Final (InputT IO)] r => Command -> Sem r ()
runCommand (Run stmt) = do
  interpretStmt stmt >>= \case
    Just (RPoly (WrappedPolynomial p)) -> printLine $ show p
    Just (RList l) -> applyList (\l -> printLine $ show l) l
    Nothing -> return ()
  repl
runCommand (Do Quit)     = return ()
runCommand (Do ShowHelp) = do
  printLine "\t:order Lex|RevLex|DegLex|DegRevLex\tchange monomial order"
  printLine "\t:help\tshow this message"
  printLine "\t:quit\tquit"
  repl
runCommand (Do (SwitchOrder ord)) = modify (\(Ctx vars _) -> Ctx vars (WrappedOrder ord)) >> repl

repl :: Members '[State Ctx, Error String, Fail, Final (InputT IO)] r => Sem r ()
repl = do
  (Just line) <- embedFinal $ getInputLine @IO "> "
  let res = parseCommand line
  case res of
    Right cmd -> runCommand cmd `catch` \s -> printLine s >> repl
    Left err  -> printLine err >> repl

main :: IO ()
main =
  runInputT defaultSettings
  . runFinal @(InputT IO)
  . void
  . (>>= \case
      Left e -> printLine e
      Right _ -> return ()
    )
  . runError @String
  . failToError id
  . runState defaultCtx
  $ repl

