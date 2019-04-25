module PureProlog where

import Data.List
import Data.Maybe
import Data.Char

newtype Funtor = Funtor {fun2str :: String} deriving Eq
newtype Var = Var {var2str :: String} deriving Eq
data Termino = TFun Funtor [Termino] | TVar Var deriving Eq

-- Caracteres reservados.

reservado = flip elem ":-,()."
charId x = not (reservado x || isSpace x)

-- Parseo feliz XD

instance Read Funtor where 
    readsPrec _ ss = let s = dropWhile isSpace ss in
                      if null s || (not . isLower . head $ s)
                        then []
                        else [(Funtor fun,sobra) | let (fun,sobra) = span charId s]

instance Read Var where
    readsPrec _ ss = let s = dropWhile isSpace ss in
                    if null s || (not . isUpper . head $ s)
                        then []
                        else [(Var fun,sobra) | let (fun,sobra) = span charId s]

instance Show Funtor where
    show = fun2str

instance Show Var where
    show = var2str

instance Show Termino where
    show (TVar x) = show x
    show (TFun f par) | null par  = show f
                      | otherwise = show f ++ "(" ++ concat params  ++ ")"
                            where params = intersperse "," (map show par)

instance Read Termino where
    readsPrec _ s = [(TVar x,resto) | (x,resto) <- reads s] ++ 
                    [(TFun f l,sobrante) | (f,resto) <- reads s, (l,sobrante) <- ([],resto):readListParen resto]
                        -- Copy paste del preludio de la funcion readList, cambiando los corchetes por parentesis :D
                        where readListParen = readParen False (\r -> [pr | ("(",s)  <- lex r,
                                                     pr       <- readl s])
                              readl  s = [([],t)   | (")",t)  <- lex s] ++
                                         [(x:xs,u) | (x,t)    <- reads s,
                                                     (xs,u)   <- readl' t]
                              readl' s = [([],t)   | (")",t)  <- lex s] ++
                                         [(x:xs,v) | (",",t)  <- lex s,
                                                     (x,u)    <- reads t,
                                                     (xs,v)   <- readl' u]

-- UNIFICANDO SOY CONTENTO :D


type Sustitucion = Termino -> Termino
type EqUnif = [(Termino,Termino)]

vars :: Termino -> [Var]
vars (TVar x) = [x]
vars (TFun _ par) = nub . concat . map vars $ par

occurs :: Var -> Termino -> Bool
occurs x (TVar y) = x==y
occurs x (TFun _ par) = any (occurs x) par

satom :: Var -> Termino -> Sustitucion
satom v t (TVar x) | x == v    = t
                   | otherwise = TVar x
satom v t (TFun f par) = TFun f $ map (satom v t) par

-- ALGORITMO DE UNIFICACION DE MARTELLI MONTANARI, CON OCCURS CHECK

mgu :: EqUnif -> Maybe Sustitucion
mgu [] = Just id
mgu ((a,b):xs) = do
                    (nuevas,sus) <- pasoUnificacion a b
                    resto <- mgu $ nuevas ++ map (\(a,b) -> (sus a, sus b)) xs
                    Just $ resto . sus

pasoUnificacion :: Termino->Termino->Maybe (EqUnif,Sustitucion)

pasoUnificacion (TVar x) (TVar y) | x == y    = Just ([],id)                     -- eliminacion del par trivial
                                  | otherwise = Just ([],satom y (TVar x))       -- unificacion de variables
                                  
pasoUnificacion ter@(TFun _ _) var@(TVar _) = pasoUnificacion var ter            -- regla del swap

pasoUnificacion (TVar x) ter@(TFun f par) | occurs x ter = Nothing               -- occurs check
                                          | otherwise    = Just ([],satom x ter) -- sustitucion variable -> termino
                                          
pasoUnificacion (TFun f1 par1) (TFun f2 par2) | f1 /= f2                   = Nothing -- fail, funtores distintos
                                              | length par1 /= length par2 = Nothing -- fail, distinta aridad
                                              | otherwise = Just (zip par1 par2,id)  -- funtores compatibles, descomposicion.

-- PROLOG!

-- clausulas de horn + goal
data Rule = Rule Termino Goal deriving Eq
newtype Goal = Goal [Termino] deriving Eq
type KnowledgeBase = [Rule]

instance Read Rule where
    readsPrec _ s = [ (Rule ruleHead goal, resto) |
                       (ruleHead,s1) <- reads s,
                       (":-",s2) <- lex s1,
                       (goal,resto) <- reads s2
                   ] ++ [(Rule ruleHead (Goal []), resto) |
                       (ruleHead,s1) <- reads s,
                       (".",resto) <- lex s1]

instance Show Rule where
    show (Rule t g@(Goal goals)) = show t ++ 
                         if null goals 
                            then ""
                            else " :- " ++ show g
                        ++ "."

instance Read Goal where
    readsPrec _ s = [(Goal l,resto) | (l,s1) <- readsCommaSeparated s, (".",resto) <- lex s1]
                    where readsCommaSeparated = readParen False readl
                          readl  s = [([],s)] ++
                                     [(x:xs,u) | (x,t)    <- reads s,
                                                 (xs,u)   <- readl' t]
                          readl' s = [([],s)] ++
                                     [(x:xs,v) | (",",t)  <- lex s,
                                                 (x,u)    <- reads t,
                                                 (xs,v)   <- readl' u]

instance Show Goal where
    show (Goal goals) = concat . intersperse " , " . map show $ goals

-- renombrar variables en el primer termino de forma que no contenga ninguna de las variables indicadas

variables = [Var $ 'X':show i | i <- [0..]]


renombrar :: [Var] -> Termino -> Termino
renombrar vs t = susti t
                where varsT = vars t
                      varTodas = vs ++ varsT
                      inter = intersect vs varsT
                      frescas = filter (not . flip elem varTodas) variables
                      sustituciones = zipWith (\x y -> satom x (TVar y)) inter frescas
                      susti = foldr (.) id sustituciones
                      
                      

-- Devuelve una lista de las sustituciones que resuelven. Capo. :D
solve :: KnowledgeBase -> Goal -> [Sustitucion]
solve _ (Goal []) = [id]
solve kb g@(Goal (t:ts)) = concat [map (.sus) . solve kb . Goal . map sus $ ruleGoals ++ ts | 
                                    Rule originalRuleHead (Goal originalRuleGoals) <- kb,
                                    let ruleHead = renombrar (goalVars g) originalRuleHead,
                                    let ruleGoals = map (renombrar (goalVars g)) originalRuleGoals,
                                    sus <- maybeToList $ mgu [(ruleHead,t)]
                                ]
                            
goalVars :: Goal -> [Var]
goalVars (Goal l) = nub . concat . map vars $ l
