module Lib where
import Text.Show.Functions

laVerdad = True

data Elemento = UnElemento { 
tipo :: String,
ataque :: (Personaje-> Personaje),
defensa :: (Personaje-> Personaje) 
} deriving (Show)

data Personaje = UnPersonaje { 
nombre :: String,
salud :: Float,
elementos :: [Elemento],
anioPresente :: Int 
} deriving (Show)

--PUNTO 1--

{-
Lo esperado es poder usar el efecto de ataque de un elemento sobre el rival y el de defensa sobre el personaje que lo tiene. 
En caso de que no se indique cuál es el efecto defensivo o el ofensivo, significa que no se altera de ninguna forma al personaje recibido.

Empecemos por algunas transformaciones básicas:
mandarAlAnio: lleva al personaje al año indicado.
meditar: le agrega la mitad del valor que tiene a la salud del personaje.
causarDanio: le baja a un personaje una cantidad de salud dada.
Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede quedar menor a 0.
-}

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje {salud = alterarSalud (/2) (salud personaje)}

causarDanio :: Float -> Personaje -> Personaje
causarDanio valorDeSalud personaje = personaje {salud = alterarSalud (subtract valorDeSalud) (salud personaje)}

alterarSalud :: (Float -> Float) -> Float -> Float
alterarSalud operacion valor = max (operacion valor) 0


--PUNTO 2--

{-
Queremos poder obtener algo de información extra sobre los personajes. Definir las siguientes funciones:
esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje en cuestión es de tipo “Maldad”.
danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre la salud inicial del personaje 
y la salud del personaje luego de usar el ataque del elemento sobre él.
enemigosMortales que dado un personaje y una lista de enemigos, devuelve la lista de los enemigos que pueden 
llegar a matarlo con un solo elemento. Esto sucede si luego de aplicar el efecto de ataque del elemento, 
el personaje queda con salud igual a 0.
-}



