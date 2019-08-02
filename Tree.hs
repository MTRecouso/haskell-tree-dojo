module Tree where
import Text.Read (readMaybe)

data Tree a = Node a (Tree a) (Tree a) | Leaf a | Null deriving Show

data Action = Inserir | Deletar | Percorrer | Sair

main :: IO ()
main = do
    let while tree = do
        putStrLn "Digite 1 para inserir, 2 para deletar e 3 para percorrer e 4 para sair"
        actionInput <- getLine
        case toAction actionInput of
            (Just Sair) -> do
                putStrLn "Fim"
            (Just Percorrer) -> do
                putStrLn $ showTreeInOrder tree
                while tree
            (Just action) -> do
                putStrLn "Digite um numero"
                userInput <- getLine
                case readMaybe userInput :: Maybe Int of
                    Nothing -> do
                        putStrLn "Por favor digite um numero inteiro\n"
                        while tree
                    (Just number) -> do
                        newTree <- return $ doAction action number tree
                        putStrLn $ "Arvore: \n" ++ show newTree ++ "\n"
                        while newTree
            Nothing -> do
                while tree
    while Null


toAction :: String -> Maybe Action
toAction "1" = Just Inserir
toAction "2" = Just Deletar
toAction "3" = Just Percorrer
toAction "4" = Just Sair
toAction _ = Nothing

showTreeInOrder :: Tree Int -> String
showTreeInOrder (Node value left right) = showTreeInOrder left ++ show value ++ ", " ++ showTreeInOrder right
showTreeInOrder (Leaf value) = show value ++ ", "
showTreeInOrder Null = ""

doAction :: Action -> Int -> Tree Int -> Tree Int
doAction Inserir = insertIntoTree
doAction Deletar = deleteFromTree

insertIntoTree :: Int -> Tree Int -> Tree Int
insertIntoTree number (Node value left right)
    | number < value = Node value (insertIntoTree number left) right
    | number >= value = Node value left (insertIntoTree number right)
insertIntoTree number (Leaf value)
    | number < value = Node value (Leaf number) Null
    | number >= value = Node value Null (Leaf number)
insertIntoTree number Null = Leaf number

deleteFromTree :: Int -> Tree Int -> Tree Int
deleteFromTree number (Node value (Leaf leftValue) Null)
    | number == value = Leaf leftValue
    | number == leftValue = Leaf value
    | otherwise = (Node value (Leaf leftValue) Null)
deleteFromTree number (Node value Null (Leaf rightValue)) 
    | number == value = Leaf rightValue
    | number == rightValue = Leaf value
    | otherwise = (Node value Null (Leaf rightValue)) 
deleteFromTree number (Node value left right)
    | number < value = Node value (deleteFromTree number left) right
    | number > value = Node value left (deleteFromTree number right)
    | number == value = rebuildSubTree left right
deleteFromTree number (Leaf value)
    | number == value = Null
    | otherwise = Leaf value
deleteFromTree number Null = Null

rebuildSubTree :: Tree Int -> Tree Int -> Tree Int
rebuildSubTree left right = Node minLeaf left (deleteFromTree minLeaf right) where
    minLeaf = searchMinLeaf right

searchMinLeaf :: Tree Int -> Int
searchMinLeaf (Node value Null _) = value
searchMinLeaf (Node value left _) = searchMinLeaf left
searchMinLeaf (Leaf value) = value
