module Main where

import PureProlog
import System.IO
import System.IO.Error
import Data.Maybe
import Data.List

getTill c h = do
                car <- hGetChar h
                if car == c 
                  then return [c]
                  else fmap (car:) $ getTill c h

getGoal :: IO Goal
getGoal = fmap read (getTill '.' stdin)

getRule :: Handle -> IO Rule
getRule h = fmap read (getTill '.' h)
            

haltTerm = TFun (Funtor "halt") []
haltGoal = Goal [haltTerm]

eofTerm = TFun (Funtor "eof") []
eofRule = Rule eofTerm (Goal [])

main = prolog []

prolog kb = do
              goal <- getGoal
              if goal == haltGoal
                then return []
                else work kb goal >>= prolog
         
work kb (Goal [TFun (Funtor "load") [TFun (Funtor filename) []]]) = cargarKnowledgeBase kb (filename++".pl")
work kb g = do
                -- Si no se quiere que haga un getline entre opciones, cambiar por esta linea mas simple.
                --mapM_ (imprimir $ goalVars g) $ solve kb g
                sequence_ . concat . 
                    zipWith (\x y -> [x,y]) (repeat (do{getLine;return()})) . 
                        map (imprimir $ goalVars g) $ solve kb g
                putStrLn "false."
                return kb

imprimir vars sus = do
                        if null vars then putStr "true"
                         else mapM_ putStr . intersperse (" , ") $ 
                            [show v ++ " = " ++  (show . sus . TVar $ v) | v <- vars]
                        putStrLn ";"

cargarKnowledgeBase kb filename = catchIOError 
                                 (do
                                    putStrLn $ "Cargando " ++ filename  ++ " ..."
                                    handle <- openFile filename ReadMode
                                    let cargar = catchIOError
                                                 (do
                                                    g <- getRule handle
                                                    if g == eofRule
                                                      then return []
                                                      else fmap (g:) cargar
                                                 )
                                         $ const (do
                                                    putStrLn $ "No se encontro 'eof.' en " ++ filename
                                                    return kb
                                                 )
                                    nkb <- cargar
                                    hClose handle
                                    putStrLn $ "Se cargo correctamente el archivo " ++ filename
                                    return nkb
                                 )
                         $ const (do
                                    putStrLn $ "No se pudo abrir el archivo " ++ filename
                                    return kb
                                 )
                                
