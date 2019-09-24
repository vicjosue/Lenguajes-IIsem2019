import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map)
import Data.Char
import Data.List (sort,map,nub)
import System.IO


run = sort [5,2,8,3,4]

type Estado = HashMap [Char] [Int]

main :: IO ()
main = do 
       mainloop (fromList[])

mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "leer" -> do
               putStrLn ">>> Nombre archivo entrada: "
               nombreArchivo <- getLine
               inh <- openFile nombreArchivo ReadMode
               let estado2 = initEstado estado "N" [0,0,0,0]--only use first
               let estado3 = initEstado estado2 "P" [0,0,0,0]--[positive,neutral,negative,no used]
               nuevoestado <- cargar inh estado3
               hClose inh
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
               let llaves =keys nuevoestado
               let nuevoestado2 = delete_value nuevoestado llaves
               mainloop nuevoestado2
               -- putStrLn $ "Comando leer desactivado"
               -- mainloop estado
     "guardar" -> do
               putStrLn ">>> Nombre archivo salida: "
               nombreArchivo <- getLine
               outh <- openFile nombreArchivo WriteMode
               descargar outh (sort (toList estado))
               hClose outh
               -- putStrLn $ "Comando guardar desactivado"
               mainloop estado     
     "clin" -> do
                let (nuevoestado, salida)= contar_linea (tail tokens) estado
                putStrLn salida
                mainloop nuevoestado
     
     "borrar" -> do
                   let (nuevoestado, salida)= cmd_borrar (tail tokens) estado
                   putStrLn salida
                   mainloop nuevoestado
                   -- putStrLn $ "Comando borrar desactivado"
                   -- mainloop estado
     "imp" -> do
                 let (nuevoestado, salida) = cmd_imp estado
                 putStrLn salida
                 mainloop nuevoestado
     "fin" -> do
                 putStrLn "Saliendo..."
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado
                 
-- función que implementa el comando def
-- cmd_def::[String] -> Estado -> (Estado, String)
-- cmd_def tokens estado = (nuevoestado, mensaje)
  -- where nuevoestado = insert (tokens!!0) (tokens!!1) estado
        -- mensaje = tokens!!0 ++ " fue definida"

-- función que implementa el comando borrar
cmd_borrar::[String] -> Estado -> (Estado, String)
cmd_borrar [] estado = (estado, "No se especificó qué borrar")
cmd_borrar (v:_) estado = if member v estado 
                               then (delete v estado, v ++ " borrado")
                               else (estado, v ++ " no aparece")

-- función que maneja un comando desconocido
cmd_desconocido ::
      String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False,estado,mensaje)
  where mensaje = "Comando desconocido ("++ cmd ++"): '" 
                                         ++ comando ++ "'"

-- función que implementa el comando imp
cmd_imp :: Estado -> (Estado, String)
cmd_imp estado = (estado, show estado)

cargar :: Handle -> Estado -> IO Estado
cargar inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       --sum1 to class p
                       --let replace0 = map (\c -> if (isMember c ['a'..'z']) then c; else ' ')
                       --putStrLn (replace1 inpStr)
                       let palabras = (words (map toLower(replace1 inpStr)))
                       
                       let p = head palabras
                       let estado1 = sumP estado "P" p
                       --sum1 al resto de palaras
                       let nuevoestado = foldl contar_token estado1 (nub (tail palabras))
                       let nuevoestado2 = selectSumEstadoP nuevoestado (tail palabras) p
                       let estado2 = sumEstado nuevoestado2 "N" "1"
                       cargar inh estado2

-- función que implementa el comando contar_linea
contar_linea :: [String] -> Estado -> (Estado, String) 
contar_linea tokens estado = (foldl contar_token estado tokens, "contar_linea" )

contar_token :: Estado -> String-> Estado
--recibe un hashmap y una 
contar_token estado tok = case lookup tok estado of
                                --lookup ( key hashmap)
                               --Nothing -> insert tok (selectSumP [1,0,0,0] p) estado
                               --Just valor -> insert tok (selectSumP (sum1to1 valor) p) estado
                               Nothing -> insert tok [1,0,0,0] estado
                               Just valor -> insert tok (sum1to1 valor) estado
                               --insert( key value hashmap)                          
--mycompare:: [Int]->Bool
mycompare x = case x of
    Just a -> if (a!!0<3) then True
                else False

delete_Ni3 ::Estado -> String -> Estado
delete_Ni3 estado x = if (mycompare(lookup x estado))
    --((fromIntegral(lookup x estado)!!0 )< 3) 
    then delete x estado
    else estado

delete_value:: Estado -> [String] ->Estado
delete_value estado [] = estado
delete_value estado (x:xs) = do let nuevoestado = (delete_Ni3 estado x)
                                delete_value nuevoestado xs 

initEstado :: Estado -> String -> [Int] -> Estado
--recibe un hashmap y una palabra
initEstado estado tok array = case lookup tok estado of
                                --lookup ( key hashmap)
                               Nothing -> insert tok array estado
                               Just valor -> insert tok (array) estado
                               --insert( key value hashmap)
-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs

--myFunctions
sum1to1::[Int]->[Int]
sum1to1 (x:xs) = [x+1]++xs

sum1to2::[Int]->[Int]
sum1to2 (x:xs:xl) = [x]++[xs+1]++xl

sum1to3::[Int]->[Int]
sum1to3 (x:xs:xl:xxl) = [x]++[xs]++[xl+1]++xxl

sum1to4::[Int]->[Int]
sum1to4 (x:xs:xl:xxl:xxxl) = [x]++[xs]++[xl]++[xxl+1]++xxxl

sumEstado :: Estado -> String ->String -> Estado
sumEstado estado tok indice = case lookup tok estado of
        --lookup ( key hashmap)
        Just valor -> insert tok (selectSum valor indice) estado
        --Nothing -> insert tok array estado --JAMASS!

selectSum::[Int]->String->[Int]
selectSum array b = case b of
    "1" -> do
        sum1to1 array
    "2" -> do
        sum1to2 array
    "3" -> do
        sum1to3 array
    "4" -> do
        sum1to4 array

sumP :: Estado -> String ->String -> Estado
sumP estado tok indice = case lookup tok estado of
        --lookup ( key hashmap)
        Just valor -> insert tok (selectSumP valor indice) estado
        --Nothing -> insert tok array estado --JAMASS!


insertP::Estado -> String ->String -> Estado
insertP estado tok indice = case lookup tok estado of
            --lookup ( key hashmap)
            Just valor -> insert tok (selectSumP valor indice) estado
    
selectSumEstadoP::Estado->[String]->String->Estado
selectSumEstadoP estado palabras p = 
    if palabras==[] then estado
    else
        selectSumEstadoP (insertP estado (head palabras) p) (tail palabras) p

selectSumP::[Int]->String->[Int]
selectSumP array b = case b of
    "0" -> do
        sum1to2 array
    "1" -> do
        sum1to2 array
    "2" -> do
        sum1to3 array
    "3" -> do
        sum1to4 array
    "4" -> do
        sum1to4 array

isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs

    
replace0::[String]->[String]
replace0 [] = []
replace0 (x:xs) = [replace1 x] ++ replace0 xs
    
replace1::String->String
replace1 [] = []
replace1 (x:xs) = [if ((isMember x ['a'..'z'] )||( isMember x ['0'..'4'])) then x; else ' ']++replace1 xs
