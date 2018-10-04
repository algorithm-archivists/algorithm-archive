"Add this method to the SequenceableCollection class in the browser:"
bubbleSort
    "Bubble sort for a collection."
    | len swapper thisElem nextElem nextIndex |
    len  := self size.
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

"Then run this anywhere in your code: "
#(4 3 2 1 6 5) bubbleSort "outputs:  #(1 2 3 4 5 6)"
