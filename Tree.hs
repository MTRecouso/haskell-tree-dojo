module Tree where
import Text.Read (readMaybe)

data Tree a = Node a (Tree a) (Tree a) | Leaf a | Null deriving Show

main :: IO ()
main = do
    let while tree = do
        putStrLn "Digite um numero"
        userInput <- getLine
        if(userInput /= "") then do
            case readMaybe userInput :: Maybe Int of
                Nothing -> do
                    putStrLn "Por favor digite um numero inteiro\n"
                    while tree
                (Just number) -> do
                    newTree <- return $ insertIntoTree number tree
                    putStrLn $ "Arvore: \n" ++ show newTree ++ "\n"
                    while newTree
        else
            putStrLn "Fim" 
    while Null

insertIntoTree :: Int -> Tree Int -> Tree Int
insertIntoTree number (Node value left right)
    | number < value = Node value (insertIntoTree number left) right
    | number >= value = Node value left (insertIntoTree number right)
insertIntoTree number (Leaf value)
    | number < value = Node value (Leaf number) Null
    | number >= value = Node value Null (Leaf number)
insertIntoTree number Null = Leaf number