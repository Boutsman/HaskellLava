-- Taak Lava
import Lava

halfAdder::(Bit,Bit) -> (Bit,Bit)
halfAdder (a,b) = (sum, carry)
  where sum = xor2(a,b)
        carry = and2(a,b)
		
data StateMachine state inp = StateMachine
	{ states		:: [state]
	, inputs		:: [inp]
	, initial		:: [state]
	, transition	:: state -> inp -> [state]
	}
	
theStateMachine = StateMachine
	{ states	= ["A","B","C"]
	, inputs	= ['a','b','c']
	, initial	= ["A"]
	, transition = \state inp ->
		[ next | (state', inp', next) <-
			[ ("A", 'a', "C")
			, ("A","b","B")
			, ("B","a","A")
			, ("B","b","C")
			, ("C","a","B")
			, ("C","b","A")
			]
		, state == state'
		, inp == inp'
		]
	}