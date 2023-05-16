import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    
    " amigosDe 2" ~: (amigosDe redB usuario2) ~?= [usuario1, usuario3],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " cantidadDeAmigos 2" ~: (cantidadDeAmigos redB usuario2) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " usuarioConMasAmigos 2" ~: expectAny (usuarioConMasAmigos redB) [usuario2],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " estaRobertoCarlos 2" ~: (estaRobertoCarlos redC) ~?= True,


    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

   " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

   " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True

 {-   " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True -}
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Roberto Carlos") -- Casos Propios
usuario7 = (7, "Facundo") -- Casos Propios
usuario8 = (8, "Jose") -- Casos Propios
usuario9 = (9, "Gaspar") -- Casos Propios
usuario10 = (10, "Ruslan") -- Casos Propios
usuario11 = (11, "Dante") -- Casos Propios
usuario12 = (12, "Agustin") -- Casos Propios
usuario13 = (13, "Santiago") -- Casos Propios

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion1_5 = (usuario1, usuario5) -- Casos Propios
relacion1_6 = (usuario1, usuario6) -- Casos Propios
relacion1_7 = (usuario1, usuario7) -- Casos Propios
relacion1_8 = (usuario1, usuario8) -- Casos Propios
relacion1_9 = (usuario1, usuario9) --Casos Propios 
relacion1_10 = (usuario1, usuario10) --Casos Propios
relacion1_11 = (usuario1,usuario11) --Casos Propios
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)
relacion6_2 = (usuario6, usuario2) -- Casos Propios
relacion6_3 = (usuario6, usuario3) -- Casos Propios
relacion6_4 = (usuario6, usuario4) -- Casos Propios
relacion6_5 = (usuario6, usuario5) -- Casos Propios
relacion6_7 = (usuario6, usuario7) -- Casos Propios
relacion6_8 = (usuario6, usuario8) -- Casos Propios
relacion6_9 = (usuario6, usuario9) --Casos Propios 
relacion6_10 = (usuario6, usuario10) --Casos Propios
relacion6_11 = (usuario6,usuario11) --Casos Propios
relacion6_12 = (usuario6, usuario12)
relacion6_13 = (usuario6, usuario13)
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion7_1 = (usuario7, "SOMOS CAMPEONES DEL MUNDO", [usuario2, usuario4]) -- Casos Propios
publicacion7_2 = (usuario7, "Hoy me echaron del laburo, alguno tiene una changa?", [usuario4]) -- Casos Propios
publicacion7_3 = (usuario7, "Vendo Charizard holografico, solo existen 2, 1 palo", [usuario2, usuario5]) -- Casos Propios
publicacion7_4 = (usuario7, "No estes triste, siempre adelante", []) -- Casos Propios
publicacion7_5 = (usuario7, "queres gamar dinero fasil? hablame al dm", [usuario5]) -- Casos Propios
publicacion8_1 = (usuario8, "queres ser tu propio jefe, la vida es una, vota a Milei", [usuario5]) -- Casos Propios
publicacion9_1 = (usuario9, "Alguno tiene nota de armas, cualquier data al dm", [usuario5]) -- Casos Propios

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

usuariosC = [usuario1, usuario2, usuario3, usuario5, usuario6]
relacionesC = [relacion1_2, relacion2_3, relacion1_6, relacion6_2, relacion6_3, relacion6_4, relacion6_5, relacion6_7, relacion6_8, relacion6_9, relacion6_10, relacion6_11, relacion6_12, relacion6_13]
publicacionesC = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)
