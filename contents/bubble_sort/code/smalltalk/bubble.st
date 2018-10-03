bubbleSort
	"Bubble sort for an array."

	| len swapper thisElem nextElem nextIndex |
	len := self size.
	1 to: len - 1 do:  [ :iteration |
			1 to: len - 1 do: [ :thisIndex |
				thisElem := self at: thisIndex.
				nextIndex := thisIndex + 1.
				nextElem := self at: nextIndex.
				(thisElem > nextElem) ifTrue: [ 
						swapper := nextElem.
						self at: nextIndex put: thisElem.
						self at: thisIndex put: swapper.
					]
				]
		 ]
