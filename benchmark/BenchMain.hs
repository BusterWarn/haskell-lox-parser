import Criterion.Main

-- Adjust the import based on your module structure
import Scanner (scanTokens)

generateStringOfSize :: Int -> String
generateStringOfSize size = go size ""
 where
  go 0 generatedString = generatedString
  go n str =
    let pattern = "var _var = 32.9089; //good\n\"my multiline\nstring\"\n" -- String have size 50
        newStr = str ++ pattern
     in go (n - 1) newStr

main :: IO ()
main =
  defaultMain
    [ bench "Code of length 500    " $ whnf scanTokens (generateStringOfSize 10)
    , bench "Code of length 5000   " $ whnf scanTokens (generateStringOfSize 100)
    , bench "Code of length 50000  " $ whnf scanTokens (generateStringOfSize 1000)
    , bench "Code of length 500000 " $ whnf scanTokens (generateStringOfSize 10000)
    , bench "Code of length 5000000" $ whnf scanTokens (generateStringOfSize 100000)
    ]
