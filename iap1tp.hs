{- Grupo: Haskelindodia
    Dante Llosa Fernandez - dantellosafernandez@gmail.com - 947/22
    Agustín Russo - agus.drum12@gmail.com - 39/23
    Gaspar Onesto De Luca - gluca@gmail.com - 711/22
    Ruslan Sobol Sanmartin - rus1147@gmail.com - 275/14
-}

module Iap1tp where

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
    En la implementación desestructurizamos la tupla para obtener una lista de usuarios de la red social y llamo a proyectarNombres
    para obtener una lista con los nombres de los usuarios.
    La función usuarios se utiliza para separar la información de los usuarios de la estructura total de la red social
    dejando que la función nombresDeUsuario trabaje unicamente con la lista de los usuarios.
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


{- Ejercicio 2:
    amigosDe recibe un tipo de dato RedSocial, un tipo Usuario, y nos devuelve una lista de Usuarios.
    La funcion la modularizamos, primero obteniendo del tipo RedSocial su lista de Relacion, en la que se uso la funcion relaciones.
    Una vez obtenida se llama a la funcion estanRelacionados que recibe una lista de Relacion y un Usuario,
    donde la lista que nos devuelve son todos usuarios que comparten un tipo Relacion en el que pertenecen ambos.
-}

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = estanRelacionados (relaciones red) u

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
    Luego llamamos a la funcion longitud que nos devuelve la cantidad de elementos que hay en la lista de usuarios que le pasamos,
    que corresponde a la cantidad de amigos del usuario. 
-}

cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red u = longitud (amigosDe red u) 

{-
    Longitud suma 1 por cada elemento de la lista. Una vez que la lista esta vacia suma 0 y finaliza.
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
usuarioPopular red [] = undefined
usuarioPopular red [u] = u
usuarioPopular red (u1:u2:us) | amigos u1 >= amigos u2 = usuarioPopular red (u1:us)
                              | otherwise = usuarioPopular red (u2:us)
                              where amigos = cantidadDeAmigos red

{- Ejercicio 5 
    estaRobertoCarlos recibe un tipo RedSocial y devuelve un booleano.
    Usando la funcion cantidadDeAmigos, esta funcion chequea cuantos amigos tiene cada usuario dentro de la lista de Usuarios.
    Luego se fija si esa cantidad es mayor a 10 (1 millon) si hay alguno devuelve True, sino False.
    Para lograrlo la funcion plantea un recursion sobre los usuarios de la lista obteniendo la cantidad de amigos de cada uno y comparando la desigualdad. 
-}

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos ((u:us),rel,_) | cantidadDeAmigos ([],rel,[]) u > 10 = True
                                 | otherwise = estaRobertoCarlos (us,rel,[])

{- Ejercicio 6
    publicacionesDe recibe un tipo RedSocial y un Usuario y nos devuelve una lista de publicaciones.
    Nuestro caso base sucede cuando dentro del tipo RedSocial que le pasamos, la lista de publicaciones esta vacia.
    En el caso de que no lo este, se llama a la funcion basica usuarioDePublicacion a la que le damos la primera publicacion de la lista y nos devuelve el usuario que la realizo.
    Ese usuario se compara con el usuario que ingresamos y si son iguales agrega la publicacion a una lista y llama a la recursion con el resto de la lista de publicaciones y el mismo usuario.
    En el caso de que no sean iguales los usuarios simplemente saltea la publicacion y sigue con la recursion.
    Cuando se recorre toda la lista nos devuelve la lista de pulicaciones que hizo el ususario .
-}

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_,_,[]) u = []
publicacionesDe (_,_,(p:ps)) u | usuarioDePublicacion p == u = p : publicacionesDe ([],[],ps) u
                               | otherwise = publicacionesDe ([],[],ps) u

{- Ejercicio 7
    publicacionesQueLeGustanA recibe un tipo RedSocial y un Usuario, y nos devuelve una lista de publicaciones.
    Nuestro caso base sucede cuando la lista de publicaciones esta vacia.
    Usando la funcion basica likesDePublicacion que nos devuelve la lista de todos los usuarios que le dieron like a una publicación, 
    le damos la primera publicacion y chequeamos si el usuario que ingresamos pertenece a la lista que nos devolvio.
    Si es verdad, agrega la publiacion a una lista y llama a la recursion con el resto de la lista de publicaciones y el mismo usuario.
    En el caso de que no pertenezca omite la publicacion y sigue con la recursion.
    Una vez que recorrio todas la publicaciones llega a la lista vacia y nos devuelve la lista de publicaciones que le gustan al usuario.
-}

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_,_,[]) u = []
publicacionesQueLeGustanA (_,_,(p:ps)) u | pertenece u (likesDePublicacion p) = p : publicacionesQueLeGustanA ([],[],ps) u
                                         | otherwise = publicacionesQueLeGustanA ([],[],ps) u

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
tieneUnSeguidorFiel red u = aAlguienLeGustanTodas (usuarios red) (publicacionesDe red u)

{-
    aAlguienLeGustanTodas nos devuelve un booleano chequeando si es verdadera la funcion leGustanTodasLasPublicaciones, 
    que en el caso de False llama a la recursion para fijarse si al siguiente de la lista de usuarios le gustan todas las publicaciones.
-}

aAlguienLeGustanTodas :: [Usuario] -> [Publicacion] -> Bool
aAlguienLeGustanTodas _ [] = False -- decidimos, que si no tiene publicaciones, no puede tener seguidor fiel.
aAlguienLeGustanTodas [] ps = False
aAlguienLeGustanTodas (u:us) ps = leGustanTodasLasPublicaciones u ps || aAlguienLeGustanTodas us ps

{-
    leGustanTodasLasPublicaciones recibe un Usuario y una lista de tipo Publicacion y nos devuelve un booleano.
    El caso base sucede cuando la lista de publicaciones, que sucede cuando la recursion recorrio toda la lista sin caer en el False.
    Cuando la lista no es vacia, ingresa al elemento correspondiente a los "Likes" de la publicacion (que es una lista de Usuarios),
    y se fija si el usuario que le dimos pertenece a esa lista y por la recursion, que suceda para todas las publicaciones.
    En el caso de que sea False, la funcion anterior le entrega el usuario siguiente para repetir el proceso.
-}

leGustanTodasLasPublicaciones :: Usuario -> [Publicacion] -> Bool
leGustanTodasLasPublicaciones u [] = True
leGustanTodasLasPublicaciones u ((_,_,likes):ps) = pertenece u likes && leGustanTodasLasPublicaciones u ps

{- Ejercicio 10
    La funcion existeSecuenciaDeAmigos toma una RedSocial y dos usuarios. Devuelve True si existe una secuencia de relaciones entre usuarios que conecte al usuario 1
    con el usuario 2 y devuelve False si no existe una secuencia de relaciones que los conecten o si estan directamente relacionados. 
    Este problema era bastante complicado, por lo que encontramos un problema equivalente que nos solucionara la misma situacion.
    Lo que pensamos fue el concepto de "Red Amistosa" que seria una lista de usuarios que tiene a todos los usuarios que pueden
    relacionarse entre si con alguna cadena de amistades. Seria como separar el conjuntos de usuarios de una red en "Islas"
    y lo que hace la funcion redAmistosa seria mapear una de esas islas.
-}

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | pertenece u2 (amigosDe red u1) = False
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