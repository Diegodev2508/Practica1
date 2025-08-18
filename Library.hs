import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.List (find)
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (isNothing)

-- Tipo de dato para un libro prestado
data Libro = Libro {
    libroId :: String,  -- Evitamos 'id' para no chocar con Prelude.id
    prestamo :: UTCTime,
    devolucion :: Maybe UTCTime  -- Nothing si no devuelto
} deriving (Show, Read)

-- Registrar préstamo (Check Out)
registrarPrestamo :: String -> UTCTime -> [Libro] -> [Libro]
registrarPrestamo idLibro tiempo libros =
    Libro idLibro tiempo Nothing : libros

-- Registrar devolución (Check In)
registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro]
registrarDevolucion idLibro tiempo libros =
    map (\lib -> if idLibro == libroId lib && isNothing (devolucion lib)
                then lib { devolucion = Just tiempo }
                else lib) libros

-- Buscar libro por ID (solo si en préstamo)
buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro idLibro libros =
    find (\lib -> idLibro == libroId lib && isNothing (devolucion lib)) libros

-- Calcular duración del préstamo
duracionPrestamo :: Libro -> UTCTime -> NominalDiffTime
duracionPrestamo libro tiempoActual =
    case devolucion libro of
        Just tiempoDev -> diffUTCTime tiempoDev (prestamo libro)
        Nothing        -> diffUTCTime tiempoActual (prestamo libro)

-- Guardar libros en archivo con reintentos
guardarLibros :: [Libro] -> IO ()
guardarLibros libros = do
    resultado <- reintentar 5 (writeFile "Library.txt" (unlines (map show libros)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando: " ++ show ex
        Right _ -> putStrLn "Datos guardados en Library.txt."

-- Función para reintentar IO
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left _ -> do
            threadDelay 1000000  -- 1 segundo
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Cargar libros desde archivo
cargarLibros :: IO [Libro]
cargarLibros = do
    resultado <- try (readFile "Library.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando: " ++ show ex
            return []
        Right contenido -> return $ map read (lines contenido)

-- Menú principal
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal libros = do
    putStrLn "\n--- Menú de Gestión de Biblioteca ---"
    putStrLn "1. Registrar Préstamo (Check Out)"
    putStrLn "2. Registrar Devolución (Check In)"
    putStrLn "3. Buscar por ID"
    putStrLn "4. Listar Libros Prestados"
    putStrLn "5. Calcular Duración de Préstamo (por ID)"
    putStrLn "6. Salir y Guardar"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese ID del libro:"
            idLibro <- getLine
            tiempoActual <- getCurrentTime
            let librosActualizados = registrarPrestamo idLibro tiempoActual libros
            putStrLn $ "Préstamo registrado para ID: " ++ idLibro
            guardarLibros librosActualizados
            cicloPrincipal librosActualizados
        "2" -> do
            putStrLn "Ingrese ID del libro:"
            idLibro <- getLine
            tiempoActual <- getCurrentTime
            let librosActualizados = registrarDevolucion idLibro tiempoActual libros
            if any (\lib -> libroId lib == idLibro && isNothing (devolucion lib)) libros
                then putStrLn $ "Devolución registrada para ID: " ++ idLibro
                else putStrLn "Libro no encontrado o ya devuelto."
            guardarLibros librosActualizados
            cicloPrincipal librosActualizados
        "3" -> do
            putStrLn "Ingrese ID del libro:"
            idLibro <- getLine
            case buscarLibro idLibro libros of
                Just lib -> putStrLn $ "Libro encontrado: " ++ show lib
                Nothing  -> putStrLn "Libro no encontrado o no en préstamo."
            cicloPrincipal libros
        "4" -> do
            putStrLn "Libros prestados actualmente:"
            let prestados = filter (isNothing . devolucion) libros
            if null prestados
                then putStrLn "No hay libros prestados."
                else mapM_ print prestados
            cicloPrincipal libros
        "5" -> do
            putStrLn "Ingrese ID del libro:"
            idLibro <- getLine
            case find (\lib -> libroId lib == idLibro) libros of
                Just lib -> do
                    tiempoActual <- getCurrentTime
                    let dur = duracionPrestamo lib tiempoActual
                    putStrLn $ "Duración: " ++ show dur ++ " segundos."
                Nothing  -> putStrLn "Libro no encontrado."
            cicloPrincipal libros
        "6" -> guardarLibros libros >> putStrLn "Datos guardados. Saliendo..."
        _   -> putStrLn "Opción inválida." >> cicloPrincipal libros

-- Main
main :: IO ()
main = do
    libros <- cargarLibros
    putStrLn "¡Bienvenido al Sistema de Gestión de Biblioteca!"
    cicloPrincipal libros