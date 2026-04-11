import Control.Applicative
import Parsers
import Data.Set
import Data.List 
import Utils

data CompileError = SyntaxError
data Var = Var { name :: String, type_ :: Sexp }
newtype Env = Env { variables :: Set Var }

var :: Sexp -> Either (Maybe CompileError) Var
var (Atom _) = Left Nothing
var (List sexps) = 
    if (sexps !? 0) /= (Justt $ Atom "var") then Left Nothing else
    if (length sexps)
    if (sexps !? 1) /= (Just $ Atom "var") then Nothing else
    Nothing

globalEnv :: [Sexp] -> Env
globalEnv = undefined

funSig :: Sexp -> Maybe Sexp
funSig = undefined

varSig :: Sexp -> Maybe Sexp
varSig = undefined

main :: IO ()
main = undefined