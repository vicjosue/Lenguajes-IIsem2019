note
	description: "Partes_Componentes application root class"
	date: "$Date$"
	revision: "$Revision$"

class
	APPLICATION

inherit
	ARGUMENTS_32


create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			--inventario: HASH_TABLE[CHUNCHE, INTEGER]
			--catalogo : ARRAYED_LIST[CHUNCHE]
			catalogo : CATALOGO
			--ARRAYED_LIST[TUPLE[codigo:STRING;cantidad:INTEGER]]
			i:INTEGER
			inventario:INVENTARIO
			datos:LIST[STRING]
		do
			create catalogo.make (0)
			create inventario.make(0)
			from i := 0 until i = 1
			loop
				print(">>")
				Io.read_line
				datos:=Io.last_string.split (' ')
				if(datos.at (1).is_equal ("defp") )
					then catalogo.insertarp (datos)
				elseif (datos.at (1).is_equal ("defc") )
					then catalogo.insertarc (datos)
				elseif (datos.at (1).is_equal ("expandir") )
					then catalogo.expandir (datos)
				elseif (datos.at (1).is_equal ("componenteMax") )
					then inventario.componenteMax (datos, catalogo.getCatalogo)
				elseif (datos.at (1).is_equal ("faltante") )
					then inventario.faltante (datos,catalogo.getcatalogo)
				elseif (datos.at (1).is_equal ("mostrar") )
					then catalogo.mostrar
				elseif (datos.at (1).is_equal ("fin") )
					then i:=i+1
				else print("not recognized %N")

				end
			end
		end

end
