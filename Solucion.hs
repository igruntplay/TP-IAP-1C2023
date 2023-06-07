{- Grupo: Haskelindodia
    Dante Llosa Fernandez - dantellosafernandez@gmail.com - 947/22
    Agustín Russo - agus.drum12@gmail.com - 39/23
    Gaspar Onesto De Luca - gluca@dc.uba.ar - 711/22
    Ruslan Sobol Sanmartin - rus1147@gmail.com - 275/14
-}
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
    La función "nombresDeUsuarios" recibe como parámetro una RedSocial
    y devuelve una lista de nombres (Strings) de los usuarios de esa red.
    En nuestra implementación, llamamos a la auxiliar "proyectarNombres"
    a la que le pasamos como parámetro la lista de usuarios de la red usando "usuarios red".
    Por último, usamos la función "quitarRepetidos" para filtrar nombres repetidos en la lista final.
-}

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = quitarRepetidos (proyectarNombres (usuarios red))

{-
    La función "proyectarNombres" toma esta lista de usuarios
    y devuelve una lista de strings con los nombres de los usuarios.
    Usamos recursión para recorrer la lista, obtener los nombres de los usuarios (usando "nombreDeUsuario")
    y finalmente los agregamos a una lista vacía.
-}

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (u:us) = nombreDeUsuario u : proyectarNombres us

quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs) | pertenece x xs = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs

{- Ejercicio 2:
    "amigosDe" recibe una RedSocial, un Usuario, y nos devuelve una lista de Usuarios
    que contiene a todos los amigos de ese usuario.
    Para esto, implementamos la funcion auxiliar "estanRelacionados"
    a la que le pasamos la lista de relaciones de la red ("relaciones red") y el usuario.
-}

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = estanRelacionados (relaciones red) u

{- 
    "estanRelacionados" plantea una recursión sobre una lista de relaciones, que son tuplas (Usuario1, Usuario2).
    Como el orden dentro de las relaciones es irrelevante, la funcion evalúa las relaciones
    fijándose si el usuario en cuestión es uno de los 2 elementos de la tupla, en cuyo caso,
    devuelve el otro elemento (el amigo) y lo agrega al siguiente paso recursivo.
    Si el usuario no forma parte de la tupla, no hace nada y sigue evaluando el siguiente elemento.
    Cuando termina de procesar todas las relaciones, devuelve una lista vacía a la que recursivamente se agregan todos los usuarios reelevantes.
-}

estanRelacionados :: [Relacion] -> Usuario -> [Usuario]
estanRelacionados [] u = []
estanRelacionados ((u1,u2):rs) u | u1 == u = u2 : estanRelacionados rs u
                                 | u2 == u = u1 : estanRelacionados rs u
                                 | otherwise = estanRelacionados rs u

{- Ejercicio 3
    La funcion cantidadDeAmigos recibe un tipo RedSocial y un tipo Usuario, y nos devuelve un Integer.
    Hicimos uso de la funcion amigosDe para poder obtener una lista de Usuarios donde los usuarios que aparecieran sean todos los amigos del Usuario.
    Luego llamamos a la funcion longitud que nos devuelve la cantidad de elementos que hay en la lista de usuarios que le pasamos,
    que corresponde a la cantidad de amigos del usuario. 
-}

cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = longitud (amigosDe red u) 

{-
    Longitud suma 1 por cada elemento de la lista. Una vez que la lista esta vacia devuelve 0 y finaliza.
-}

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{- Ejercicio 4 
    usuarioConMasAmigos recibe un tipo RedSocial y devuelve un Usuario.
    Se compone con la funcion usuarioPopular que recibe la RedSocial y y la lista de Usuarios de esa misma red.
    Nos devolvera el Usuario con mas amigos en esa red.
-}

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioPopular red (usuarios red)

{-
    usuarioPopular plante una recursion para encontrar el Usuario con mas amigos.
    Si la lista de Usuarios que recibe solo tiene un Usuario devuelve es usuario.
    Si tiene mas se fija si el primer usuario es tiene mas amigos que el segundo, y si es asi llama a la recursion con el primer usuario y el resto de la lista.
    Caso contrario hace lo mismo pero con el segundo usuario ya este tiene mas amigos.
    Para facilitar la lectura currificamos cantidadDeAmigos red = amigos .
-}

usuarioPopular :: RedSocial -> [Usuario] -> Usuario
usuarioPopular red [u] = u
usuarioPopular red (u1:u2:us) | amigos u1 >= amigos u2 = usuarioPopular red (u1:us)
                              | otherwise = usuarioPopular red (u2:us)
                              where amigos = cantidadDeAmigos red

{- Ejercicio 5
    estaRobertoCarlos recibe un tipo RedSocial y devuelve un booleano.
    la función utiliza los auxiliares, longitud, amigosDe y usuarioConMasAmigos, para encontrar al usuario de la red con la mayor cantidad de amigos. luego con longitud cuenta el número de amigos de ese usuario.
    Si la red social está vacía o si el usuario con más amigos tiene 10 o menos amigos, la función devuelve False.
    Si el usuario con más amigos tiene más de 10 amigos, la función devuelve True.
-}


estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos red = 10 < longitud (amigosDe red (usuarioConMasAmigos red)) 

{- Ejercicio 6
    publicacionesDe recibe un tipo RedSocial, un Usuario y devuelve una lista de publicaciones del usuario especificado.
    La función utiliza una auxiliar publicadasPor que primero, extrae la lista de todas las publicaciones de la red social usando la función publicaciones y luego pasa esta lista junto con el usuario a publicadasPor que itera de manera recursiva sobre la lista de publicaciones.
    En el caso base la lista de publicaciones está vacía, la función devuelve una lista vacía.
    Y en el paso recursivo si la lista de publicaciones no está vacía, la función extrae el usuario de la primera publicación utilizando usuarioDePublicacion y lo compara con el usuario ingresado. 
        - Si son iguales, agrega la publicación a la lista de publicaciones del usuario y llama recursivamente a publicadasPor con el resto de la lista de publicaciones.
        - Si no son iguales, la función omite la publicación y llama recursivamente a publicadasPor con el resto de la lista de publicaciones.
    Al final, publicacionesDe devuelve una lista de todas las publicaciones realizadas por el usuario ingresado en la red social dada.
-}

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicadasPor (publicaciones red) u

publicadasPor :: [Publicacion] -> Usuario -> [Publicacion]
publicadasPor [] u = []
publicadasPor (p:ps) u | u == autor = p : publicadasPor ps u
                       | otherwise = publicadasPor ps u
                       where autor = usuarioDePublicacion p

{- Ejercicio 7
    publicacionesQueLeGustanA recibe un tipo RedSocial, un usuario. Esta devuelve una lista de publicaciones que le gustan a ese usuario.
    Se utiliza la función auxiliar likeadasPor que primero extrae la lista de todas las publicaciones de la red social con la función publicaciones que pasa esta lista junto con el usuario a likeadasPor. Que itera de manera recursiva sobre la lista de publicaciones.
    En el caso base si la lista de publicaciones está vacía, la función devuelve una lista vacía.
    En el paso recursivo, la lista de publicaciones no está vacía, utiliza likesDePublicacion para obtener la lista de usuarios a los que les gusta la primera publicación y verifica si el usuario ingresado está en esa lista.
        - Si está, agrega la publicación a la lista de publicaciones que le gustan al usuario y llama recursivamente a likeadasPor con el resto de la lista de publicaciones.
        - Si no está, omite la publicación y llama recursivamente a likeadasPor con el resto de la lista de publicaciones.
    Finalmente, publicacionesQueLeGustanA devuelve una lista de todas las publicaciones que le gustan al usuario ingresado en la red social dada.
-}

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = likeadasPor (publicaciones red) u

likeadasPor :: [Publicacion] -> Usuario -> [Publicacion]
likeadasPor [] u = []
likeadasPor (p:ps) u | pertenece u (likesDePublicacion p) = p : likeadasPor ps u
                     | otherwise = likeadasPor ps u

{-
    pertenece la tipamos definiendo sobre 'a' la clase de tipo Eq, asi podemos reutilizarla mas adelante con distintos tipos de datos.
    Se fija si un elemente (de tipo a) pertenece a una lista (de elementos tipo a).
    Si la lista es vacia (caso base) devuelve False.
    En caso de que no es vacia, si el elemento que le dimos es igual a el primero de la lista, devuelve True.
    O, si no es igual, llama a la recursion y compara el resto de los elementos.
-}

pertenece :: Eq a => a -> [a] -> Bool
pertenece y [] = False
pertenece y (x:xs) = x == y || pertenece y xs

{- Ejercicio 8
    lesGustanLasMismasPUblicaciones recibe un tipo RedSocial y 2 Usuario, para devolvernos un booleano.
    Para esta funcion usamos la funcion anterior publicacionesQueLeGustanA a todos para obtener la lista de publicaciones que le gustan al usuario1 y al usuario2.
    Luego la funcion sonLaMismaLista compara si esas dos lista son iguales, si lo son devuelve True, caso contrario False.
    Para mejorar la lectura usamos currificacion en la funcion publicacionesQueLeGustanA por likes.
-}

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool 
lesGustanLasMismasPublicaciones red u1 u2 = sonLaMismaLista (likes u1) (likes u2)
                                          where likes = publicacionesQueLeGustanA red

sonLaMismaLista :: Eq a => [a] ->[a]->Bool
sonLaMismaLista [] [] = True
sonLaMismaLista [] _ = False
sonLaMismaLista _ [] = False
sonLaMismaLista (x:xs) (y:ys) = x == y && sonLaMismaLista xs ys

{- Ejercicio 9
    tieneUnSeguidorFiel recibe un tipo RedSocial, un Usuario y devuelve un booleano.
    De la RedSocial nos quedamos con la lista de Usuarios y tambien la lista de publicaciones que realizo el usuario que le dimos.
    Esas dos listas se la pasamos a la funcion aAlguienLeGustanTodas.
-}

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = aAlguienLeGustanTodas (usuarios red) (publicacionesDe red u) u

{-
    aAlguienLeGustanTodas nos devuelve un booleano chequeando si es verdadera la funcion leGustanTodasLasPublicaciones, 
    que en el caso de False llama a la recursion para fijarse si al siguiente de la lista de usuarios le gustan todas las publicaciones.
-}

aAlguienLeGustanTodas :: [Usuario] -> [Publicacion] -> Usuario -> Bool
aAlguienLeGustanTodas _ [] u1 = False -- decidimos, que si no tiene publicaciones, no puede tener seguidor fiel.
aAlguienLeGustanTodas [] ps u1 = False
aAlguienLeGustanTodas (u:us) ps u1 = u1 /= u && leGustanTodasLasPublicaciones u ps || aAlguienLeGustanTodas us ps u1

{-
    leGustanTodasLasPublicaciones recibe un Usuario y una lista de tipo Publicacion y nos devuelve un booleano.
    El caso base sucede cuando la lista de publicaciones, que sucede cuando la recursion recorrio toda la lista sin caer en el False.
    Cuando la lista no es vacia, ingresa al elemento correspondiente a los "Likes" de la publicacion (que es una lista de Usuarios),
    y se fija si el usuario que le dimos pertenece a esa lista y por la recursion, que suceda para todas las publicaciones.
    En el caso de que sea False, la funcion anterior le entrega el usuario siguiente para repetir el proceso.
-}

leGustanTodasLasPublicaciones :: Usuario -> [Publicacion] -> Bool
leGustanTodasLasPublicaciones u [] = True
leGustanTodasLasPublicaciones u (p:ps) = pertenece u (likesDePublicacion p) && leGustanTodasLasPublicaciones u ps

{- Ejercicio 10
    La funcion existeSecuenciaDeAmigos toma una RedSocial y dos usuarios. Devuelve True si existe una secuencia de relaciones entre usuarios que conecte al usuario 1
    con el usuario 2 y devuelve False si no existe una secuencia de relaciones que los conecten o si estan directamente relacionados. 
    Este problema era bastante complicado, por lo que encontramos un problema equivalente que nos solucionara la misma situacion.
    Lo que pensamos fue el concepto de "Red Amistosa" que seria una lista de usuarios que tiene a todos los usuarios que pueden
    relacionarse entre si con alguna cadena de amistades. Seria como separar el conjuntos de usuarios de una red en "Islas"
    y lo que hace la funcion redAmistosa seria mapear una de esas islas.
-}

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | u1 == u2 = 1 < longitud (redAmistosa red [u1] (amigosDe red u1))
                                  | otherwise = pertenece u2 (redAmistosa red [u1] (amigosDe red u1))

{-
    redAmistosa toma como parametros la Red Social a analizar y 2 listas. Una de usuarios visitados y otra de usuarios por visitar.
    redAmistosa va iterando por todos los usuarios por visitar. En el paso recursivo se agrega el "usuario 1" a usuarios visitados
    y se arma la nueva lista de usuarios por explorar agregando UNICAMENTE los usuarios relacionados con el "usuario 1" que no hayan sido visitados anteriormente.
    Cuando no quedan usuarios por visitar, se devuelve la lista de visitados.
    Si "usuario 2" esta en esta lista, significa que puede conectarse con "usuario 1" de ALGUNA manera.
-}

redAmistosa :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
redAmistosa red visitado [] = visitado
redAmistosa red visitado (u:us) = redAmistosa red (u:visitado) (porExplorar (amigosDe red u) us visitado) 

porExplorar :: [Usuario] -> [Usuario] -> [Usuario] -> [Usuario]
porExplorar [] us visitado = us
porExplorar (x:xs) us visitado | pertenece x visitado || pertenece x us = porExplorar xs us visitado
                               | otherwise = x : porExplorar xs us visitado