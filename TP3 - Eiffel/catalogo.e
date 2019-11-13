note
	description: "Summary description for {CATALOGO}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	CATALOGO

inherit
	DOUBLE_MATH

create
	make

feature -- Access

feature {NONE} -- Initialization
	make(num:INTEGER)
	do
		create listaChunches.make (num)
		--create inventario.make (num)
	end

feature
	mostrar
		local
			keys:ARRAY[STRING]
			cont:INTEGER
			chunche:detachable CHUNCHE
		do
			keys:=listaChunches.current_keys
			print("codigo  nombre  complejidad %N")
			from
				cont:=1
			until
				cont>keys.count
			loop
				chunche:=listaChunches.at (keys.at (cont))
				if attached chunche then
					print(chunche.codunico+"   "+
					chunche.nomunico+"    "+
					chunche.complejidad.out+"%N")
				end
				cont:=cont+1
			end
		end

	insertarp (datos:LIST[STRING])--(y:STRING;x:INTEGER;z:DOUBLE)
		local
			nuevo:PARTE_BASICA
			cont:INTEGER
		do
			from
				cont:=2
			until
				cont>=datos.count
			loop
				create nuevo.make(datos.at (cont),datos.at (cont+1),datos.at (cont+2).to_real)
				--listaChunches.put (insertar)
				listaChunches.put (nuevo, datos.at (cont))
				cont:=cont+3
			end
		end

	insertarc (datos:LIST[STRING])
	--deberia pasarle el catalogo a componente para que el mismo se setee la complejidad
		local
			nuevo:COMPONENTE
			cont:INTEGER
			sum:DOUBLE
			chunche:detachable CHUNCHE
		do
			create nuevo.make(datos.at (2),datos.at (3),datos)
			sum:=0
			from
				cont:=4
			until
				cont>datos.count
			loop
				chunche:=listaChunches.at (datos.at (cont))
				if attached chunche then
					sum:=sum + (log(1 + datos.at (cont+1).to_integer)/log(2))*chunche.complejidad
				end
				cont:=cont+2
			end
			nuevo.setcomplejidad (sum)
			listaChunches.extend (nuevo, datos.at (2))
		end

	expandir  (datos:LIST[STRING])
		local
			cont:INTEGER
			chunche:detachable CHUNCHE
			listaNoExpandir: HASH_TABLE[STRING, STRING]
			banderaExpandir: BOOLEAN

			resultado: HASH_TABLE[INTEGER, STRING]
			cantidadAnterior: INTEGER

		do
			create listaNoExpandir.make (0)
			create resultado.make (0)
			banderaExpandir:=false

			--NO EXPANDIR--
			from
				cont:=2
			until
				cont>datos.count
			loop
				if datos.at (cont).is_equal ("-") then
					banderaExpandir:=true
					cont:=cont+1
				end
				if banderaExpandir then
					listaNoExpandir.extend (datos.at (cont),datos.at (cont))
				end
				cont:=cont+1
			end
			banderaExpandir:=false
			--ALGORITMO EXPANDIR--
			from
				cont:=2
			until
				cont>datos.count
			loop
				if datos.at (cont).is_equal ("-") then
					banderaExpandir:=true
					cont:=cont+1
				end
				if not(banderaExpandir) then
					chunche:=listaChunches.at (datos.at (cont))
					if attached chunche then
						if listaNoExpandir.has (datos.at (cont)) then
							if resultado.has (datos.at (cont)) then
								cantidadAnterior:=resultado.at (datos.at (cont))
								if attached cantidadAnterior then
									resultado.replace (cantidadAnterior+datos.at (cont+1).to_integer,chunche.codunico)
								end
							else
								resultado.extend (datos.at (cont+1).to_integer, chunche.codunico)
							end
						else
							resultado:=chunche.expandir(datos.at (cont+1).to_integer,resultado,listaNoExpandir,listaChunches)
						end
					end
				end
				cont:=cont+2
			end
			mostrarExpandir(resultado)
		end


    mostrarExpandir(resultado: HASH_TABLE[INTEGER, STRING])
    local
		keys:ARRAY[STRING]
    	cont:INTEGER
    	cantidad:detachable INTEGER
    do
    	keys:=resultado.current_keys
    	from
    		cont:=1
    	until
    		cont>keys.count
    	loop
    		cantidad:=resultado.at (keys.at (cont))
    		if attached cantidad then
    			print(cantidad.out+"	"+keys.at (cont)+"%N")
    		end
    		cont:=cont+1
    	end
    end

	getCatalogo: HASH_TABLE[CHUNCHE, STRING] --usada en inventario.componenteMax
		do
			Result:=listaChunches
		end

feature {NONE}
	--listaChunches2: ARRAYED_LIST[CHUNCHE]
	listaChunches: HASH_TABLE[CHUNCHE, STRING]

invariant
	invariant_clause: True -- Your invariant here

end
