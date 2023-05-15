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

-- Ejercicios:

{- Ejercicio 1:
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
estanRelacionados ((r1, r2):rs) u | r1 == u = r2 : estanRelacionados (rs u)
                                  | r2 == u = r1 : estanRelacionados (rs u)
                                  | otherwise = estanRelacionados (rs u)

-- cuenta lo de arriba hermano
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos x y = longitud amigosDe x y

longitud :: [a] -> Int
longitud [] = 0
longitud x:xs = 1 + longitud xs

{- Ejercicio 2:
    amigosDe recibe un tipo de dato RedSocial, un tipo Usuario, y nos devuelve una lista de Usuarios.
    La funcion la modularizamos, primero obteniendo del tipo RedSocial su lista de Relacion, en la que se uso la funcion relaciones.
    Una vez obtenida se llama a la funcion estanRelacionados que recibe una lista de Relacion y un Usuario, donde la lista que nos devuelve son todos usuarios que comparten un tipo Relacion en el que pertenecen ambos.
-}

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe x y = estanRelacionados (relaciones x) y

{- 
    Esta funcion plantea un recursion sobre una listaa de tipo Relacion=(Usuario,Usuario). 
    Se fija si el primer o el segundo elemento de la cada dupla es igual al usurio ingreasado.
    Si lo es, agrega el Usuario a una lista que se genera haciendo el llamado recurisvo. Si no lo es simplemente lo omite y sigue recorriendo la lista.
    Una vez que llego al final se encuentra con la lista vacia que es nuestro caso base y la recursion termina y nos devuelve la lista de Usuarios que se relacionan. 
-}

estanRelacionados :: [Relacion] -> Usuario -> [Usuario]
estanRelacionados [] u = []
estanRelacionados ((u1,u2):rs) u | u1 == u = u2 : estanRelacionados rs u
                       | u2 == u = u1 : estanRelacionados rs u
                       | otherwise = estanRelacionados rs u

{- Ejercicio 3
    La funcion cantidadDeAmigos recibe un tipo RedSocial y un tipo Usuario, y nos devuelve un Integer.
    Hicimos uso de la funcion amigosDe para poder obtener una lista de Usuarios donde los usuarios que aparecieran sean todos los amigos del Usuario.
    Luego llamamos a la funcion longitud que nos devuelve la cantidad de elementos que hay en la lista de usuarios que le pasamos, que corresponde a la cantidad de amigos del usuario. 
-}
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = longitud (amigosDe red u) 

{-
    longitud suma 1 por cada elemento de la lista. Una vez que la lista esta vacia suma 0 y finaliza.
-}
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos x u = usuarioPopular (usuarios x) u

usuarioPopular :: [Usuarios] -> Usuario
usuarioPopular [] u = u
usuarioPopular (x:xs) u | cAm x >= cAm u = usuarioPopular xs x
                        | otherwise = usuarioPopular xs u
                        where cAm = cantidadDeAmigos

{- Ejercicio 5 
    estaRobertoCarlos recibe un tipo RedSocial y devuelve un booleano
    Usando la funcion cantidadDeAmigos, esta funcion chequea cuantos amigos tiene cada usuario dentro de la lista de Usuarios.
    Luego se fija si esa cantidad es mayor a 10 (1 millon) si hay alguno devuelve True, sino False.
    Para lograrlo la funcion plantea un recursion sobre los usuarios de la lista obteniendo la cantidad de amigos de cada uno y comparando la desigualdad. 
-}
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos ((u:us),(r:rs),_) | cantidadDeAmigos (r:rs) u > 10 = True
                                    | otherwise = estaRobertoCarlos (us,(r:rs,_))

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
