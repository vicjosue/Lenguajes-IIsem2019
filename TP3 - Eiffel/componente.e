note
	description: "Summary description for {COMPONENTE}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	COMPONENTE

inherit
	CHUNCHE
	--	redefine
	--		expandir
	--	end

create
	make

feature {NONE} -- Initialization

	make (x:STRING;y:STRING;z:LIST[STRING])
		local
			cont:INTEGER
		do
			create subcomponentes.make (0)
			codUnico := x
			nomUnico := y
			complejidad:=0
			from
				cont:=4
			until
				cont>=z.count
			loop
				subComponentes.extend (z.at (cont+1).to_integer,z.at (cont))
				cont:=cont+2

			end

		end

feature

	expandir (cant:INTEGER;resultado: HASH_TABLE[INTEGER, STRING];noExpandir:HASH_TABLE[STRING, STRING]
			listaChunches: HASH_TABLE[CHUNCHE, STRING]): HASH_TABLE[INTEGER, STRING]
	local
		--valor:detachable INTEGER
		cantidadAnterior: detachable INTEGER
		cont:INTEGER
		chunche:detachable CHUNCHE
		keys:ARRAY[STRING]
		resultadoNuevo: HASH_TABLE[INTEGER, STRING]
		bandera: BOOLEAN
    do
    	create resultadoNuevo.make (0)
    	keys:=resultado.current_keys
    	bandera:=FALSE

    	from
    		cont:=1
    	until
    		cont>resultado.count
    	loop
    		cantidadAnterior:=resultado.at (keys.at (cont))
    		if attached cantidadAnterior then
    			resultadoNuevo.extend (cantidadAnterior, keys.at (cont))
    		end
    		cont:=cont+1
    	end
    	keys:=subComponentes.current_keys

    	from
    		cont:=1
    	until
    		cont>subComponentes.count
	    	loop
	    	if not(bandera) then
	    		if noExpandir.has (codUnico) then
	    			if resultadoNuevo.has (codUnico) then
	    				cantidadAnterior:=resultadoNuevo.at (codUnico)
	    				if attached cantidadAnterior then
	    					resultadoNuevo.replace (cantidadAnterior+cant, codUnico)
	    				end
	    			else
	    				resultadoNuevo.extend (cant,codUnico)
	    			end
	    			bandera:=true
	    		else
		    		cantidadAnterior:=subComponentes.at (keys.at (cont))
		    		if attached cantidadAnterior then
		    			chunche:=listaChunches.at (keys.at (cont))
			    		if attached chunche then
			    			resultadoNuevo:=chunche.expandir(cant*cantidadAnterior,resultadoNuevo,noExpandir,listaChunches)
			    		end
			    	end
	    		end
	    	end
    		cont:=cont+1
    	end
    	result:=resultadoNuevo
    end


	getString: STRING
		do
			Result:= nomUnico
		end
	setComplejidad (x:DOUBLE)
		do
			complejidad:=x
		end

feature {NONE}
	--subcomponentes:ARRAYED_LIST[TUPLE[codigo:STRING;cantidad:INTEGER]]
	subComponentes:HASH_TABLE[INTEGER, STRING]
invariant
	invariant_clause: True -- Your invariant here

end
