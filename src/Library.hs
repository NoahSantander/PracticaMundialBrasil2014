module Library where
import PdePreludat

-- Defino mis alias
type Nombre = String
type Grupo = Char
type Edad = Number
type PromedioDeGol = Number
type Habilidad = Number
type Cansancio = Number
type Jugadores = [Jugador]
type Equipo = (Nombre, Grupo, Jugadores)
type Figuras = [Jugador]
type Faranduleros = [Jugador]
type JugadoresDificiles = [Jugador]
type Equipos = [Equipo]

-- Defino mis tipos
data Jugador = CJugador {
    nombre :: Nombre,
    edad :: Edad,
    promedioDeGol :: PromedioDeGol,
    habilidad :: Habilidad,
    cansancio :: Cansancio
} deriving Show

-- Inicializo algunos jugadores
martin = CJugador "Martin" 26 0.0 50 35.0
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean = CJugador "Hacha" 23 0.01 50 35.0
brian = CJugador "Panadero" 21 5 80 15.0

garcia = CJugador "Sargento" 30 1 80 13.0
messi = CJugador "Pulga" 26 10 99 43.0
aguero = CJugador "Aguero" 24 5 90 5.0

-- Inicializo algunos equipos 
equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

-- Defino una función de ayuda
quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

-- Defino quienes son las figuras de un equipo
esFigura :: Jugador -> Bool
esFigura jugador = (> 75) (habilidad jugador) && (> 0) (promedioDeGol jugador)

figurasDelEquipo :: Equipo -> Figuras
figurasDelEquipo (_, _, jugadores) = filter (esFigura) jugadores

-- Defino la función
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

esFarandulero :: Jugador -> Bool
esFarandulero = (flip elem jugadoresFaranduleros).nombre

tieneFarandulero :: Equipo -> Faranduleros
tieneFarandulero (_, _, jugadores) = filter (esFarandulero) jugadores

-- Defino si un jugador es figurita dificil
esJoven :: Jugador -> Bool
esJoven = (< 27).edad

esDificil :: Jugador -> Bool
esDificil jugador = esJoven jugador && esFigura jugador && (not.esFarandulero) jugador

perteneceAlGrupo :: Grupo -> Equipo ->  Bool
perteneceAlGrupo grupo (_, grupoEquipo, _) = (==grupo) grupoEquipo

figuritasDificiles :: Equipo -> JugadoresDificiles
figuritasDificiles (_, _, jugadores) = filter (esDificil) jugadores

jugadoresDificiles :: Grupo -> Equipos -> JugadoresDificiles
jugadoresDificiles grupo equipos = concat(map (figuritasDificiles) (filter (perteneceAlGrupo grupo) equipos))

-- Defino la función
modificarCansancio :: Jugador -> Jugador
modificarCansancio jugador
    | esDificil jugador = jugador {cansancio = 50}
    | esJoven jugador = jugador {cansancio = (*1.1) (cansancio jugador)}
    | (not.esJoven) jugador && esFigura jugador = jugador {cansancio = (+20) (cansancio jugador)}
    | otherwise = jugador {cansancio = (*2) (cansancio jugador)}

jugarPartido :: Equipo -> Equipo
jugarPartido (nombre, grupo, jugadores) = (nombre, grupo, map (modificarCansancio) jugadores)

-- Defino el ganador de un partido dados dos equipos
menosCansado :: Jugador -> Jugador -> Bool
menosCansado jugador1 jugador2 = cansancio jugador1 < cansancio jugador2

promedioDeGolEquipo :: Equipo -> PromedioDeGol
promedioDeGolEquipo (_, _, jugadores) = sum(map (promedioDeGol) (take 11 (quickSort menosCansado jugadores)))

ganadorDelPartido :: Equipo -> Equipo -> Equipo
ganadorDelPartido equipo1 equipo2 
    | promedioDeGolEquipo equipo1 > promedioDeGolEquipo equipo2 = jugarPartido equipo1
    | otherwise = jugarPartido equipo2

-- Defino el ganador de un torneo
{--ganadorDelTorneo :: Equipos -> Equipo
ganadorDelTorneo equipos = foldl1 (ganadorDelPartido) equipos --}

ganadorDelTorneo :: Equipos -> Equipo
ganadorDelTorneo [equipo] = equipo
ganadorDelTorneo (equipo1:equipo2:equipos) = ganadorDelTorneo ((ganadorDelPartido equipo1 equipo2):equipos)

-- Defino el premio "EL GROSO"
elGroso :: Equipos -> Jugador
elGroso equipos = (head.figurasDelEquipo) (ganadorDelTorneo equipos)

-- Teórico
-- Use funciones de orden superior como quickSort o filter, que me permitieron delegar la responsabilidad de una función en otra, sin que la conozca siquiera, al tomarla como argumento.
-- No pasaría nada, ya que las funciones convergerían porque se evaluan los argumentos a medida que se los necesita, independientemente de diverger. Lazy Evaluation