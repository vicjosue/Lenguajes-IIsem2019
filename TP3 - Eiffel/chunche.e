note
	description: "Summary description for {CHUNCHE}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

deferred class
	CHUNCHE

INHERIT
	COMPARABLE

feature --access

feature --measurement

    codUnico: STRING

    nomUnico: STRING

    complejidad: DOUBLE

feature
    expandir (cant:INTEGER;resultado: HASH_TABLE[INTEGER, STRING];noExpandir:HASH_TABLE[STRING, STRING];
    		listaChunches: HASH_TABLE[CHUNCHE, STRING]): HASH_TABLE[INTEGER, STRING]
    	deferred
    	end


feature {NONE} -- Implementation

feature
	is_less alias "<" (other: like Current): BOOLEAN
		do
			Result:= complejidad > other.complejidad
		end

invariant
	invariant_clause: True -- Your invariant here

end
