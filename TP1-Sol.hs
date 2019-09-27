import Prelude hiding (null, lookup, map, filter)
import Data.HashMap.Lazy hiding (sort,map)
import Data.Char
import Data.List (sort,map,nub)
import System.IO


run = sort [5,2,8,3,4]

type Estado = HashMap [Char] ([Int],Double)

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
               let estado1 = fromList[]
               let estado2 = initEstado estado1 "N" [0,0,0,0]--only use first
               let estado3 = initEstado estado2 "P" [3,0,0,0]--[randomValue,positive,neutral,negative]
               nuevoestado <- cargar inh estado3
               hClose inh
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
               let llaves =keys nuevoestado
               let nuevoestado2 = delete_value nuevoestado llaves
               let h_value = h nuevoestado2
               let llaves2 = keys nuevoestado2
               let estadofinal = h_ki llaves2 nuevoestado2
               putStrLn (show h_value)
               --putStrLn (show (getN nuevoestado2))
               --putStrLn (show (to0(getP nuevoestado2)))
               mainloop estadofinal
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
                       let palabras = nub (words (map toLower(replace1 inpStr)))
                       
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
                               Nothing -> insert tok ([1,0,0,0],0) estado
                               Just valor -> insert tok ((sum1to1 (fst (valor))),0) estado
                               --insert( key value hashmap)                          
--mycompare:: [Int]->Bool
mycompare x = case x of
    Just a -> if ((fst a)!!0<3) then True
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
                               Nothing -> insert tok (array,0) estado
                               Just valor -> insert tok (array,0) estado
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
        Just valor -> insert tok ((selectSum valor indice),0) estado
        --Nothing -> insert tok array estado --JAMASS!

selectSum::([Int],Double)->String->[Int]
selectSum array b = case b of
    "1" -> do
        sum1to1 (fst array)
    "2" -> do
        sum1to2 (fst array)
    "3" -> do
        sum1to3 (fst array)
    "4" -> do
        sum1to4 (fst array)

sumP :: Estado -> String ->String -> Estado
sumP estado tok indice = case lookup tok estado of
        --lookup ( key hashmap)
        Just valor -> insert tok ((selectSumP valor indice),0) estado
        --Nothing -> insert tok array estado --JAMASS!

insertP::Estado -> String ->String -> Estado
insertP estado tok indice = case lookup tok estado of
            --lookup ( key hashmap)
            Just valor -> insert tok ((selectSumP valor indice),0) estado

selectSumEstadoP::Estado->[String]->String->Estado
selectSumEstadoP estado palabras p = 
    if palabras==[] then estado
    else
        selectSumEstadoP (insertP estado (head palabras) p) (tail palabras) p

selectSumP::([Int],Double)->String->[Int]
selectSumP array b = case b of
    "0" -> do
        sum1to2 (fst array)
    "1" -> do
        sum1to2 (fst array)
    "2" -> do
        sum1to3 (fst array)
    "3" -> do
        sum1to4 (fst array)
    "4" -> do
        sum1to4 (fst array)

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

-- Ejemplo de como calcular
--H = suma ( (np / N) * log2(np / N) )
--         i=1..#clases 

log2::Double->Double
log2 x = if x == 0 then 0
                   else log(x)/log(2)

f::Double->Double
f x = x * log2(x)

-- map primero divide cada entrada entre n
-- y luego obtiene x log2 x
--h n listax = - sum (map (f . (\y->y/n)) listax)
mylog::Double->Double->Double
mylog y n = if (y==0) then 0
        else  logBase 2 (y/  n ) 

h::Estado->Double
h estado = - sum ( map (\y->( (fromIntegral y)/ (fromIntegral (getN estado)) * (mylog (fromIntegral y) (fromIntegral(getN estado))))) (to0(getP estado)) )

getN::Estado->Int
getN estado = case (lookup "N" estado) of
    Just valor -> ((fst (valor))!!0)

getP::Estado->[Int]
getP estado = case (lookup "P" estado) of
    Just valor -> (fst valor)
    Nothing -> [] --JAMASS!

to0::[Int]->[Int]
to0 []=[]
to0 (x:xs) = xs

get1::String->Estado->Int
get1 x estado = case (lookup x estado) of
    Just valor -> ((fst (valor))!!0)

get2::String->Estado->Int
get2 x estado = case (lookup x estado) of
    Just valor -> ((fst (valor))!!1)

get3::String->Estado->Int
get3 x estado = case (lookup x estado) of
    Just valor -> ((fst (valor))!!2)

get4::String->Estado->Int
get4 x estado = case (lookup x estado) of
    Just valor -> ((fst (valor))!!3)

h_ki::[String]->Estado->Estado
h_ki [] estado = estado
h_ki (x:xs) estado = do
    let nuevoestado = get_Hki x estado
    h_ki xs nuevoestado

get_Hki::String->Estado->Estado
get_Hki llave estado = case lookup llave estado of
    Just value -> insert llave ((fst value),((h estado) - (formula llave estado)) ) estado

formula::String->Estado->Double
formula llave estado = do
    let n_i = (get1 llave estado)
    let n = (getN estado)
    ( (f (fromIntegral n_i)) +  (f (fromIntegral n - fromIntegral n_i)) + ((formula2 estado llave)))/fromIntegral n
    --(n_i * (logBase 2 n_i )) + (n-n_i)*(logBase 2  (n-n_i)) - (formula2 estado llave)/n

formula2::Estado->String->Double
formula2 estado llave = do
    let n_ip1 = get2 llave estado
    let n_ip2 = get3 llave estado
    let n_ip3 = get4 llave estado

    let primersuma = - ( f(fromIntegral n_ip1) + f(fromIntegral n_ip2) + f(fromIntegral n_ip3) )
        -- n_ip1 * (logBase 2  (n_ip1)) + n_ip2 * (logBase 2  (n_ip2)) + n_ip3 * (logBase 2  (n_ip3))
        
    let np_array = (to0(getP estado))
    let x1 =f (fromIntegral(np_array!!0) - (fromIntegral n_ip1)) -- * (logBase 2 (np_array!!0 - n_ip1))
    let x2 =f (fromIntegral(np_array!!1) - (fromIntegral n_ip2)) -- * (logBase 2 (np_array!!1 - n_ip2))
    let x3 =f (fromIntegral(np_array!!2) - (fromIntegral n_ip3) )--(logBase 2 (fromIntegral((np_array!!2) - (n_ip3))))
    let segundasuma = x1 + x2 + x3
    primersuma - segundasuma
