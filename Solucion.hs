module Solucion where

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios
{- Ejercicio 1
    La función nombreDeUsuarios recibe un tipo de dato RedSocial y devuelve una lista de strings con los nombres de los usuarios.
    En la implementación desestructurizo la tupla para obtener una lista de usuarios de la red social y llamo a proyectarNombres para obtener una lista con los nombres de los usuarios.
    La función usuarios se utiliza para separar la información de los usuarios de la estructura total de la red social. dejando que la función nombresDeUsuario trabaje unicamente con la lista de los usuarios.
-}
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red)

{-
    La función proyectarNombres toma una lista de usuarios y devuelve una lista de strings con los nombres de los usuarios.
    Uso recursión parar recorrer la lista y obtener los nombres agregandolos a una lista vacía.
-}

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (u:us) = nombreDeUsuario u : proyectarNombres us

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe x y = estanRelacionados (relaciones x) y

--La funcion estanRelacionados toma una lista de relaciones y un usuario. Devuelve una lista de usuarios que esten relacionados con el usuario ingresado.
estanRelacionados :: [Relacion] -> Usuario -> [Usuarios]
estanRelacionados [] u = []
estanRelacionados ((r1, r2):rs) u | r1 == u = r2 : estanRelacionados (rs)
                                  | r2 == u = r1 : estanRelacionados (rs)
                                  | otherwise = estanRelacionados (rs)

-- cuenta lo de arriba hermano
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos x y = longitud amigosDe x y

longitud :: [a] -> Int
longitud [] = 0
longitud x:xs = 1 + longitud xs

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos x u = f (usuarios x) u

usuarioPopular :: [Usuarios] -> Usuario
usuarioPopular [] u = u
usuarioPopular (x:xs) u | cAm x >= cAm u = usuarioPopular xs x
                        | otherwise = usuarioPopular xs u
                        where cAm = cantidadDeAmigos

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
