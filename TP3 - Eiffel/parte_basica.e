note
	description: "Summary description for {PARTE_BASICA}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	PARTE_BASICA

inherit
	CHUNCHE

create
	make

feature {NONE} -- Initialization

	make (x:STRING;y:STRING;z:DOUBLE)
		do
			codUnico := x
			nomUnico := y
			complejidad := z
		end
feature
	getString: STRING
		do
			Result:= nomUnico
		end
	expandir (cant:INTEGER;resultado: HASH_TABLE[INTEGER, STRING];noExpandir:HASH_TABLE[STRING, STRING];
    		listaChunches: HASH_TABLE[CHUNCHE, STRING]): HASH_TABLE[INTEGER, STRING]
    local
    	cantidadAnterior: detachable INTEGER
    do
    	if(resultado.has_key (codUnico)) then
    		cantidadAnterior:=resultado.at (codUnico)
    		if attached cantidadAnterior then
    			resultado.replace (cant+cantidadAnterior, codUnico)
    		end
    	else
    		resultado.extend (cant,codUnico)
    	end
    	result:=resultado
    end

    

invariant
	invariant_clause: True -- Your invariant here

end
