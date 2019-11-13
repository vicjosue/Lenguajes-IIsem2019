note
	description: "Summary description for {CATALOGO}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	INVENTARIO
	--inherit
	--SORTED_TWO_WAY_LIST[DATO_ORDENABLE]

create
	make

feature -- Access

feature {NONE} -- Initialization
	make(num:INTEGER)
	do
		create inventario.make (num)
	end

feature
	faltante(datos:LIST[STRING];catalogo:HASH_TABLE[CHUNCHE, STRING])
		local
			cont:INTEGER
			chunche:detachable CHUNCHE
			bandera: BOOLEAN
			--resultado: HASH_TABLE[INTEGER, STRING]
			listaVacia:HASH_TABLE [INTEGER_32, STRING_8]
			noexpandir:HASH_TABLE [STRING_8, STRING_8]
			temp:HASH_TABLE [INTEGER_32, STRING_8]

		do
			create listaVacia.make (0)
			create noexpandir.make (0)
			create temp.make (0)
			bandera:=false

			-- CREAR INVENTARIO --
			from
				cont:=2
			until
				cont>datos.count
			loop
				if datos.at (cont).is_equal ("-") then
					bandera:=true
					cont:=cont+1
				end
				chunche:=catalogo.at (datos.at (cont))
				if attached chunche then
					if bandera then
						temp:=chunche.expandir (datos.at (cont+1).to_integer, temp, noexpandir, catalogo)
						--inventario.extend (datos.at (cont+1).to_integer,datos.at (cont))
					end
				end
				cont:=cont+1
			end
			insertar(temp)
			-- ALGORITMO --
			bandera:=false
			from
				cont:=2
			until
				cont>datos.count
			loop
				if datos.at (cont).is_equal ("-") then
					bandera:=true
					cont:=datos.count+1 --break
				end
				if not(bandera) then
					chunche:=catalogo.at (datos.at (cont))
					if attached chunche then
						--print(chunche.codunico+" "+cont.out+"%N")
						restarInventario(chunche.expandir (datos.at(cont+1).to_integer, listaVacia, noexpandir,catalogo))
					end
				end
				cont:=cont+2
			end
			mostrarFaltante(inventario)
			create inventario.make (0)
		end
		
	mostrarFaltante(resultado: HASH_TABLE[INTEGER, STRING])
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
    		if attached cantidad and cantidad<0 then
    			print(cantidad.abs.out+"	"+keys.at (cont)+"%N")
    		end
    		cont:=cont+1
    	end
    end

	restarInventario(pieza:HASH_TABLE[INTEGER, STRING])
	local
		keys:ARRAY[STRING]
		cont:INTEGER
		resultado:BOOLEAN
		restantes: detachable INTEGER
		cantidad: detachable INTEGER
	do
		resultado:=false
		keys:=pieza.current_keys
		from
			cont:=1
		until
			cont>keys.count
		loop
			if inventario.has (keys.at (cont)) then
				restantes:=inventario.at (keys.at (cont))
				if attached restantes then
					cantidad:=pieza.at (keys.at (cont))
					if attached cantidad then
						inventario.replace (restantes-cantidad, keys.at (cont))
					end
				end
			else
				inventario.extend (-cantidad, keys.at (cont))
			end
			cont:=cont+1
		end
	end

	insertar(pieza:HASH_TABLE[INTEGER, STRING])
	local
		keys:ARRAY[STRING]
		cont:INTEGER
		cantidad: detachable INTEGER
		anterior: detachable INTEGER
	do
		keys:=pieza.current_keys
		from
			cont:=1
		until
			cont>keys.count
		loop
			cantidad:=pieza.at (keys.at (cont))
			if attached cantidad then
				if inventario.has (keys.at (cont)) then
					anterior:=inventario.at (keys.at (cont))
					inventario.replace (anterior+cantidad,  keys.at (cont))
				else
					inventario.extend (cantidad, keys.at (cont))
				end
			end
			cont:=cont+1
		end
	end

	componenteMax (datos:LIST[STRING];catalogo:HASH_TABLE[CHUNCHE, STRING])
	--datos = ["cod", "cantidad","cod", "cantidad",...]
		local
			miLista : SORTED_TWO_WAY_LIST[CHUNCHE]
			cont:INTEGER
			chunche:detachable CHUNCHE
			keys:ARRAY[STRING]
			temp:HASH_TABLE [INTEGER_32, STRING_8]

			listaVacia:HASH_TABLE [INTEGER_32, STRING_8]
			noexpandir:HASH_TABLE [STRING_8, STRING_8]
		do
			create miLista.make
			create listaVacia.make (0)
			create noexpandir.make (0)
			create temp.make (0)
			----INVENTARIO----
			from
				cont:=2
			until
				cont>datos.count
			loop
				chunche:=catalogo.at (datos.at (cont))
				if attached chunche then
					temp:=chunche.expandir (datos.at (cont+1).to_integer, temp, noexpandir, catalogo)
				end
				--inventario.extend (datos.at (cont+1).to_integer,datos.at (cont)) --crea el inventario
				cont:=cont+2
			end
			insertar(temp)
			----CATALOGO ORDENADO----
			keys:=catalogo.current_keys
			from
				cont:=1
			until
				cont>catalogo.count
			loop
				chunche:=catalogo.at (keys.at (cont)) --crear una nueva lista con las partes para ordenarlos
				if attached chunche then
					miLista.extend (chunche)
				end
				cont:=cont+1
			end
			miLista.sort
			componenteMaxAlgoritmo(miLista,catalogo)
		end

	componenteMaxAlgoritmo (miLista : SORTED_TWO_WAY_LIST[CHUNCHE];catalogo:HASH_TABLE[CHUNCHE, STRING])
		--miLista contiene los objetos a construir ordenados
		local
			chunche:detachable CHUNCHE
			restantes: detachable INTEGER --inventario

			cont:INTEGER
			val: detachable INTEGER
			resultado:HASH_TABLE[INTEGER, STRING]--piezas creadas a retornar
			listaVacia:HASH_TABLE[INTEGER, STRING]
			noexpandir:HASH_TABLE[STRING, STRING]
		do
			create resultado.make (0)
			create listaVacia.make (0) --lista vacia
			create noexpandir.make (0)

			from
				cont:=1
			until
				cont>miLista.count
			loop --este loop solo aumenta cuando ya no se pueden crear mas de este tipo, construye cada parte
				chunche:=miLista.at (cont)
				if attached chunche then

					if inventario.has (chunche.codunico) then
						restantes:=inventario.at (chunche.codunico)
						if attached restantes and restantes>0 then
							inventario.replace (restantes-1, chunche.codunico) --si tengo 80 cubos, se resta 1 80 veces

							if resultado.has (chunche.codunico) then
								val:=resultado.at (chunche.codunico)
								if attached val then
									resultado.replace (val+1,chunche.codunico) -- numero de piezas construidas de chunche.codUnico
								end
							else
								resultado.extend (1,chunche.codunico) --aun no habia piezas construidas de este tipo
							end

						else
							cont:=cont+1 --si no se puede construir mas de este tipo, se intenta construir el siguiente
						end

					else
						if posibleExpandir(chunche.expandir (1, listaVacia, noexpandir, catalogo)) then
							descontarInventario(chunche.expandir (1, listaVacia, noexpandir, catalogo))
							if resultado.has (chunche.codunico) then
								val:=resultado.at (chunche.codunico)
								if attached val then
									resultado.replace (val+1,chunche.codunico) -- numero de piezas construidas de chunche.codUnico
								end
							else
								resultado.extend (1,chunche.codunico) --aun no habia piezas construidas de este tipo
							end
						else
							cont:=cont+1 --si no se puede construir, se intenta construir el siguiente
						end
					end
				end
			end
			mostrar(resultado)
			create inventario.make (0)
		end

	mostrar(resultado: HASH_TABLE[INTEGER, STRING])
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

	descontarInventario(pieza:HASH_TABLE[INTEGER, STRING])
	local
		keys:ARRAY[STRING]
		cont:INTEGER
		resultado:BOOLEAN
		restantes: detachable INTEGER
		cantidad: detachable INTEGER
	do
		resultado:=false
		keys:=pieza.current_keys
		from
			cont:=1
		until
			cont>keys.count
		loop
			restantes:=inventario.at (keys.at (cont))
			if attached restantes then
				cantidad:=pieza.at (keys.at (cont))
				if attached cantidad then
					inventario.replace (restantes-cantidad, keys.at (cont))
				end
			end
			cont:=cont+1
		end
	end

	posibleExpandir(pieza:HASH_TABLE[INTEGER, STRING]):BOOLEAN
	local
		keys:ARRAY[STRING]
		cont:INTEGER
		resultado:BOOLEAN
		restantes: detachable INTEGER
		cantidad: detachable INTEGER
	do
		resultado:=false
		keys:=pieza.current_keys
		from
			cont:=1
		until
			cont>keys.count
		loop
			restantes:=inventario.at (keys.at (cont))
			if attached restantes then
				cantidad:=pieza.at (keys.at (cont))
				if attached cantidad and (restantes-cantidad)<0 then
					resultado:=false
					cont:=keys.count+1
				else
					resultado:=true
				end
			end
			cont:=cont+1
		end
		result:=resultado
	end

feature {NONE}
	inventario: HASH_TABLE[INTEGER,STRING]

invariant
	invariant_clause: True -- Your invariant here

end
