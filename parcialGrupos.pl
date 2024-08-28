integrante(sophieTrio, sophie, violin).
integrante(sophieTrio, santi, guitarra).
integrante(vientosDelEste, lisa, saxo).
integrante(vientosDelEste, santi, voz).
integrante(vientosDelEste, santi, guitarra).
integrante(jazzmin, santi, bateria).

nivelQueTiene(sophie, violin, 5).
nivelQueTiene(santi, guitarra, 2).
nivelQueTiene(santi, voz, 3).
nivelQueTiene(santi, bateria, 4).
nivelQueTiene(lisa, saxo, 4).
nivelQueTiene(lore, violin, 4).
nivelQueTiene(luis, trompeta, 1).
nivelQueTiene(luis, contrabajo, 4).

instrumento(violin, melodico(cuerdas)).
instrumento(guitarra, armonico).
instrumento(bateria, ritmico).
instrumento(saxo, melodico(viento)).
instrumento(trompeta, melodico(viento)).
instrumento(contrabajo, armonico).
instrumento(bajo, armonico).
instrumento(piano, armonico).
instrumento(pandereta, ritmico).
instrumento(voz, melodico(vocal)).

% 1] Saber si un grupo tiene buena base
tieneBuenaBase(Grupo):-
        integrante(Grupo, Musico1, Instrumento1),
        integrante(Grupo, Musico2, Instrumento2),
        Musico1 \= Musico2,
        instrumento(Instrumento1, ritmico),
        instrumento(Instrumento2, armonico).

% 2] Saber si una persona se destaca en un grupo
seDestaca(Persona, Grupo):-
        integrante(Grupo, Persona, Instrumento),
        nivelQueTiene(Persona, Instrumento, NivelDestacado),
        forall((integrante(Grupo, OtraPersona, OtroInstrumento), 
                nivelQueTiene(OtraPersona, OtroInstrumento, OtroNivel), 
                Persona \= OtraPersona),
                nivelSeDestaca(NivelDestacado, OtroNivel)).

nivelSeDestaca(NivelDestacado, OtroNivel):-
        NivelDestacadoMinimo is OtroNivel + 2,
        NivelDestacado >= NivelDestacadoMinimo.

% 3] Incorporamos tipos de grupo en base de conocimientos
grupo(vientosDelEste, bigBand).
grupo(sophieTrio, formacion([contrabajo, guitarra, violin])).
grupo(jazzmin, formacion([bateria, bajo, trompeta, piano, guitarra])).

% 4] Saber si hay cupo para un instrumento en un grupo
hayCupo(Instrumento, Grupo):-
        grupo(Grupo, bigBand),
        instrumento(Instrumento, melodico(viento)).

hayCupo(Instrumento, Grupo):-
        instrumento(Instrumento, _),
        grupo(Grupo, TipoGrupo),
        not( integrante(Grupo, _, Instrumento)),
        sirvePara(TipoGrupo, Instrumento).

sirvePara(formacion(Instrumentos), Instrumento):-
        member(Instrumento, Instrumentos).

sirvePara(bigBand, bateria).
sirvePara(bigBand, bajo).
sirvePara(bigBand, piano).

% 5] Saber si una persona puede incorporarse a un grupo y con que instrumento
puedeIncorporarse(Persona, Instrumento, Grupo):-
        nivelQueTiene(Persona, Instrumento, Nivel),
        grupo(Grupo, TipoGrupo),
        not(integrante(Grupo, Persona, _)),
        hayCupo(Instrumento, Grupo),
        nivelEsperado(TipoGrupo, NivelEsperado),
        Nivel >= NivelEsperado.

nivelEsperado(bigBand, 1).
nivelEsperado(formacion(Instrumentos), NivelEsperado):-
        length(Instrumentos, CantidadInstrumentos),
        NivelEsperado is 7 - CantidadInstrumentos.

% 6] Saber si una persona se quedo en banda (no puede incorporarse a ningun grupo y no esta en un grupo actualmente)
seQuedoEnBanda(Persona):-
        nivelQueTiene(Persona, _, _),
        not(integrante(_, Persona, _)),
        not(puedeIncorporarse(Persona, _, _)).