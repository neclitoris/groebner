import Control.Monad
import Data.Proxy
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
runCommand (Run f stmt) = do
  interpretStmt f stmt >>= maybe (return ()) printLine
  repl
runCommand (Do Quit)     = return ()
runCommand (Do ShowHelp) = do
  printLine "\t:order Lex|RevLex|DegLex|DegRevLex\tchange monomial order"
  printLine "\t:field Double|Rational|GF <prime>\tchange coefficient field"
  printLine "\t:help\tshow this message"
  printLine "\t:quit\tquit"
  repl
runCommand (Do (SwitchOrder ord)) = modify (switchOrder ord) >> repl
runCommand (Do (SwitchField p)) = modify (switchField p) >> repl

repl :: Members '[State Ctx, Error String, Fail, Final (InputT IO)] r => Sem r ()
repl = do
  (Just line) <- embedFinal $ getInputLine @IO "> "
  Ctx f _ _ <- get
  let res = parseCommand f line
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

