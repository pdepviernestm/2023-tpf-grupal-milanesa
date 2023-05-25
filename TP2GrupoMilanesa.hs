--Declaro el tipo cambio 
type Cambio = Carpeta -> Carpeta

--Declaración de estructuras

data Archivo = UnArchivo {
    nombrearchivo:: String,
    contenido:: String
} deriving (Show, Eq)

data Carpeta = UnaCarpeta {
nombrecarpeta:: String,
archivos::[Archivo]
} deriving (Show, Eq)

data Origen = Origen Branch | Raiz
data Camino = Derecha | Izquierda | Ninguno

data Commit = UnCommit {
  nombre :: String,
  descripcion :: String,
  cambios :: [Cambio]
}

data Branch = UnaBranch {
  listacommits :: [Commit],
  ramaant:: Origen,
  bifurcaciones :: [Branch],
  ramasig :: Camino
}
--Funciones auxiliares a los cambios principales

contieneArchivo :: String -> Carpeta -> Bool
contieneArchivo nombre1 carpeta = nombre1 `elem` map nombrearchivo (archivos carpeta)

noCoincide :: String -> Archivo -> Bool
noCoincide nombre2 archivo = nombrearchivo archivo /= nombre2

agregarContenido :: String -> String -> Archivo -> Archivo
agregarContenido nombreArchivo contenidoNuevo archivo
  | nombrearchivo archivo == nombreArchivo = archivo { contenido = contenidoNuevo ++ contenido archivo }
  | otherwise = archivo

sacarContenido :: String->Int->Int->Archivo->Archivo
sacarContenido nombreArchivo inicio fin archivo
 | nombrearchivo archivo == nombreArchivo = archivo{contenido = sacarCaracteres (contenido archivo) inicio fin}
 | otherwise = archivo

sacarCaracteres :: String -> Int -> Int -> String
sacarCaracteres texto inicio fin = take (inicio-1) texto ++ drop fin texto

--Funciones de los cambios principales

creararchivo :: String -> Carpeta -> Carpeta
creararchivo nombredearchivo carpeta 
  |contieneArchivo nombredearchivo carpeta = carpeta
  | otherwise = carpeta { archivos = archivos carpeta ++ [UnArchivo nombredearchivo ""] }

eliminararchivo :: String -> Carpeta -> Carpeta
eliminararchivo nombredearchivo carpeta = carpeta { archivos = filter (noCoincide nombredearchivo) (archivos carpeta) }

vaciarcarpeta :: Carpeta -> Carpeta
vaciarcarpeta carpeta = carpeta { archivos = []}

agregartexto :: String -> String -> Carpeta -> Carpeta
agregartexto contenidoNuevo nombredearchivo carpeta = carpeta { archivos = map (agregarContenido nombredearchivo contenidoNuevo) (archivos carpeta) }

sacartexto :: String -> Int -> Int -> Carpeta -> Carpeta
sacartexto nombredearchivo inicio fin carpeta = carpeta { archivos = map (sacarContenido nombredearchivo inicio fin) (archivos carpeta) }

--Ejemplo de archivo
archivoUno :: Archivo
archivoUno = UnArchivo{
  nombrearchivo = "papa2",
  contenido = "dospapas"
}

-- Ejemplos de carpetas
carpetaUno :: Carpeta
carpetaUno = UnaCarpeta {
nombrecarpeta = "carpetaUno",
archivos=[UnArchivo "papa.hs" "papita"]
}
carpetaArchInf :: Carpeta
carpetaArchInf = UnaCarpeta {
nombrecarpeta = "carpetaUno",
archivos=[archivoInfinito]
}

--Ejemplos de commit
commitUno :: Commit
commitUno = UnCommit {
nombre = "Basico",
descripcion = "Paparela",
cambios = [creararchivo "BuenArchivo", agregartexto "Superpapa" "BuenArchivo", sacartexto "BuenArchivo" 1 5, creararchivo "MalArchivo"]
}

commitDos :: Commit
commitDos = UnCommit {
nombre = "Basicopapa",
descripcion = "Papas",
cambios = [creararchivo "PDEP.pdf", agregartexto "Milanesa.hs" "PDEP.pdf", eliminararchivo "MalArchivo"]
}

commitTres :: Commit
commitTres = UnCommit {
nombre = "superbasicopapa",
descripcion = "Paparela",
cambios = [creararchivo "Milanesa", agregartexto "Napolitana" "Milanesa", sacartexto "BuenArchivo" 1 6, eliminararchivo "archivoInfinito"]
}

commitCuatro :: Commit
commitCuatro = UnCommit{
nombre= "pancho",
descripcion = "pancho",
cambios = [creararchivo "pancho", agregartexto "pancho" "pancho" ]
}

commitCinco :: Commit
commitCinco = UnCommit{
  nombre = "empanada",
  descripcion = "empanada",
  cambios = [agregartexto " y empanada" "pancho", vaciarcarpeta]

}

--Funciones para realizar cambios de un commit y analizar su utilidad
realizarCambio :: Carpeta -> Cambio -> Carpeta
realizarCambio carpeta cambio = cambio carpeta

realizarCommit :: Commit -> Carpeta -> Carpeta
realizarCommit commit carpeta = foldl realizarCambio carpeta (cambios commit)

esCommitInutil :: Commit -> Carpeta -> Bool
esCommitInutil commit carpeta = realizarCommit commit carpeta == carpeta

esCommitInutilInverso :: Commit -> Carpeta -> Bool
esCommitInutilInverso commit carpeta = realizarCommit (commitInverso commit) carpeta == carpeta

commitInverso :: Commit -> Commit
commitInverso commit = commit { cambios = reverse (cambios commit) }

--Ejemplos de ramas
inicioizq :: Branch
inicioizq = UnaBranch{
 listacommits = [commitUno],
 ramaant = Raiz,
 bifurcaciones = [rama1,rama7],
 ramasig = Izquierda
}

inicioder :: Branch
inicioder = inicioizq{ramasig = Derecha}

rama1 :: Branch
rama1 = UnaBranch{
  listacommits = [commitDos,commitTres],
  ramaant = Origen inicioizq,
  bifurcaciones = [rama3,rama4],
  ramasig = Izquierda
}

rama2 :: Branch
rama2 = rama1{ramasig = Derecha}

rama3 :: Branch
rama3= UnaBranch{
  listacommits = [commitTres],
  ramaant = Origen rama1,
  bifurcaciones = [rama5,rama6],
  ramasig = Izquierda
}

rama4 :: Branch
rama4 = rama3{ramasig = Derecha}

rama5 :: Branch
rama5 = UnaBranch{
  listacommits = [commitCinco, commitUno],
  ramaant = Origen rama3,
  bifurcaciones = [],
  ramasig = Ninguno
}

rama6 :: Branch
rama6 = UnaBranch {
  listacommits = [commitCuatro, commitCinco],
  ramaant = Origen rama3,
  bifurcaciones = [],
  ramasig = Ninguno
}

rama7 :: Branch
rama7 = UnaBranch{
  listacommits = [commitCinco],
  ramaant = Origen inicioder,
  bifurcaciones = [],
  ramasig = Ninguno
}

--Funciones para realizar los commits de una sola rama de izquierda a derecha y viceversa
realizarBranchl :: [Commit] -> Carpeta -> Carpeta
realizarBranchl commitejemplo carpeta = foldl (flip realizarCommit) carpeta commitejemplo

realizarBranchr :: [Commit] -> Carpeta -> Carpeta
realizarBranchr commits carpeta = foldr realizarCommit carpeta commits

--Funcion que recibe una rama, una carpeta y aplica todos los cambios de un camino sobre esa carpeta
checkout :: Branch -> Carpeta -> Carpeta
checkout (UnaBranch commits origen [] camino) carpeta =  realizarBranchl commits carpeta
checkout (UnaBranch commits origen [i,d] Izquierda) carpeta = checkout i (realizarBranchl commits carpeta)
checkout (UnaBranch commits origen [i,d] Derecha) carpeta  = checkout d  (realizarBranchl commits carpeta)
checkout rama carpeta  = error "Mal construido el commit"

--Funcion auxiliar de LogMain
obtenerNombreCommit :: Commit -> String
obtenerNombreCommit = nombre

--Funcion que recibe una rama y nombra todos los commits de su camino
logMain :: Branch -> [String]
logMain (UnaBranch commits origen [] camino) =map obtenerNombreCommit commits
logMain (UnaBranch commits origen [i,d] Izquierda) = map obtenerNombreCommit commits ++ logMain i
logMain (UnaBranch commits origen [i,d] Derecha) = map obtenerNombreCommit commits  ++ logMain d
logMain rama = error "Mal construido el commit"


--Funcion que recibe una rama, una carpeta y aplica todos los cambios desde un commit hasta la raiz
checkoutinverso :: Branch -> Carpeta -> Carpeta
checkoutinverso (UnaBranch commits1 (Origen ramaanterior) caminos camino) carpeta = checkoutinverso ramaanterior (realizarBranchr commits1 carpeta)
checkoutinverso (UnaBranch commits1 Raiz caminos camino) carpeta = realizarBranchr commits1 carpeta

--Funciones auxiliares para los ejemplos infinitos
textoInfinito :: String -> String
textoInfinito texto = texto ++ textoInfinito texto

archivosInfinitos :: Archivo -> [Archivo]
archivosInfinitos archivoejemplo = archivoejemplo: archivosInfinitos archivoejemplo

--Ejemplos infinitos

archivoInfinito :: Archivo
archivoInfinito = UnArchivo{
  nombrearchivo = "papa",
  contenido = textoInfinito "papa"
}

-- Con el archivo con el contenido infinito se pueden hacer todos los cambios menos agregar texto, esto debido a que en el archivoInfinito
-- se pueden ver los primeros caracteres pero no tiene ultimos ya que es infinito, por lo que nada puede venir despues.
carpetaInfinita :: Carpeta
carpetaInfinita = UnaCarpeta{
  nombrecarpeta = "carpeta infinita",
  archivos = archivosInfinitos archivoUno
}

-- Con la carpeta con infinitos archivos se pueden hacer todos los cambios menos crear una nuevo archivo, esto debido a que debe revisar en los
--infinitos archivos si ya esta el nombre de archivo que se va a crear, por lo que no es verdadero ni falso ya que no lo sabe y la funcion no termina de ejecutarse
-- hasta que de stack overflow (por la gran cantidad de archivos en la que busco nombres)

