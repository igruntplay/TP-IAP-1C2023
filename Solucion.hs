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

-- describir qué hace la función: ..... Ejercicio 4
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos r = usuarioPopular r (usuarios r)

usuarioPopular :: RedSocial -> [Usuario] -> Usuario
usuarioPopular r [] = undefined
usuarioPopular r [x] = x
usuarioPopular r (x:xs:xss) | cAm r x >= cAm r xs = usuarioPopular r (x:xss)
                            | otherwise = usuarioPopular r (xs:xss)
                            where cAm = cantidadDeAmigos


{- Ejercicio 5 
    estaRobertoCarlos recibe un tipo RedSocial y devuelve un booleano
    Usando la funcion cantidadDeAmigos, esta funcion chequea cuantos amigos tiene cada usuario dentro de la lista de Usuarios.
    Luego se fija si esa cantidad es mayor a 10 (1 millon) si hay alguno devuelve True, sino False.
    Para lograrlo la funcion plantea un recursion sobre los usuarios de la lista obteniendo la cantidad de amigos de cada uno y comparando la desigualdad. 
-}
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos ((u:us),(r:rs),_) | cantidadDeAmigos ([],(r:rs),[]) u > 10 = True
                                    | otherwise = estaRobertoCarlos (us,(r:rs),[])

{- Ejercicio 6

-}
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_,_,[]) u = []
publicacionesDe (_,_,(x:xs)) u | usuarioDePublicacion x == u = x : publicacionesDe ([],[],xs) u
                               | otherwise = publicacionesDe ([],[],xs) u

{- Ejercicio 7

-}
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_,_,[]) u = []
publicacionesQueLeGustanA (_,_,(x:xs)) u | pertenece (likesDePublicacion x) u = x : publicacionesQueLeGustanA ([],[],xs) u
                                         | otherwise = publicacionesQueLeGustanA ([],[],xs) u

pertenece :: Eq a => [a] -> a -> Bool
pertenece [] y = False
pertenece (x:xs) y = x == y || pertenece xs y

{- Ejercicio 8

-}
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool 
lesGustanLasMismasPublicaciones r u1 u2 = sonLaMismaLista (publicacionesQueLeGustanA r u1) (publicacionesQueLeGustanA r u2) 


sonLaMismaLista :: Eq a => [a] ->[a]->Bool
sonLaMismaLista [] [] = True
sonLaMismaLista [] _ = False
sonLaMismaLista _ [] = False
sonLaMismaLista (x:xs) (y:ys) = x==y && sonLaMismaLista xs ys

{- Ejercicio 9
    tieneUnSeguidorFiel recibe un tipo RedSocial, un Usuario y devuelve un booleano.
    De la RedSocial nos quedamos con la lista de Usuarios y tambien la lista de publicaciones que realizo el usuario que le dimos
    Esas dos listas se la pasamos a la funcion aAlguienLeGustanTodas
-}
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u = aAlguienLeGustanTodas (usuarios r) (publicacionesDe r u)

{-
    aAlguienLeGustanTodas nos devuelve un booleano chequeando si es verdadera la funcion leGustanTodasLasPublicaciones, 
    que en el caso de False llama a la recursion para fijarse si al siguiente de la lista de usuarios le gustan todas las publicaciones
-}

aAlguienLeGustanTodas :: [Usuario] -> [Publicacion] -> Bool
aAlguienLeGustanTodas (u:us) ps = leGustanTodasLasPublicaciones u ps || aAlguienLeGustanTodas us ps

{-
    leGustanTodasLasPublicaciones recibe un Usuario y una lista de tipo Publicacion y nos devuelve un booleano
    el caso base sucede cuando la lista de publicaciones, que sucede cuando la recursion recorrio toda la lista sin caer en el False
    cuando la lista no es vacia ingresa al elemento correspondiente a los "Likes" de la publicacion (que es una lista de Usuarios) y se fija si el usuario que le dimos pertenece a esa lista y por la recursion, que suceda para todas las publicaciones
    en el caso de que sea False la funcion anterior le entrega el usuario siguiente para repetir el proceso.

-}

leGustanTodasLasPublicaciones :: Usuario -> [Publicacion] -> Bool
leGustanTodasLasPublicaciones u [] = True
leGustanTodasLasPublicaciones u ((_,_,l):ps) = pertenece l u && leGustanTodasLasPublicaciones u ps

 
    

{- Ejercicio 10
    La funcion existeSecuenciaDeAmigos toma una RedSocial y dos usuarios. Devuelve True si existe una secuencia de relaciones entre usuarios que conecte al usuario 1 con el usuario 2 y devuelve False si no existe una secuencia de relaciones que los conecten o si estan directamente relacionados. 
    Este problema era bastante complicado, por lo que encontramos un problema equivalente que nos solucionara la misma situacion. Lo que pensamos fue el concepto de "Red Amistosa" que seria una lista de usuarios que tiene a todos los usuarios que pueden relacionarse entre si con alguna cadena de amistades. Seria como separar el conjuntos de usuarios de una red en "Islas" y lo que hace la funcion redAmistosa seria mapear una de esas islas.
    redAmistosa toma como parametros la Red Social a analizar y 2 listas. Una de usuarios visitados y otra de usuarios por visitar.
    redAmistosa va iterando por todos los usuarios por visitar. En el paso recursivo se agrega el "usuario 1" a usuarios visitados y se arma la nueva lista de usuarios por explorar agregando UNICAMENTE los usuarios relacionados con el "usuario 1" que no hayan sido visitados anteriormente. Cuando no quedan usuarios por visitar, se devuelve la lista de visitados.
    Si "usuario 2" esta en esta lista, significa que puede conectarse con "usuario 1" de ALGUNA manera.
-}

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 | pertenece (amigosDe r u1) u2 = False
                                | otherwise = pertenece (redAmistosa r [u1] (amigosDe r u1)) u2

redAmistosa :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
redAmistosa red visitado [] = visitado
redAmistosa red visitado (u:us) = redAmistosa red (u:visitado) (porExplorar (amigosDe red u) us visitado) 
                                                             --(quitar (union (amigosDe red u) us) (u:visitado))

porExplorar :: [Usuario] -> [Usuario] -> [Usuario] -> [Usuario]
porExplorar [] us visitado = us
porExplorar (x:xs) us visitado | pertenece visitado x || pertenece us x = porExplorar xs us visitado
                               | otherwise = x : porExplorar xs us visitado

{-union :: Eq a => [a] -> [a] -> [a]
union [] y = y
union (x:xs) y | pertenece y x = union xs y
               | otherwise = x : union xs y

quitar :: Eq a => [a] -> [a] -> [a]
quitar [] y = []
quitar (x:xs) y | pertenece y x = quitar xs y
                | otherwise = x : quitar xs y-}



